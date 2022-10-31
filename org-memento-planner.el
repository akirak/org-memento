;;; org-memento-planner.el ---  -*- lexical-binding: t -*-

(require 'org-memento-policy)
(require 'org-memento-timeline)

;;;; Constants

(defconst org-memento-planner-buffer "*Org-Memento Planner*")

;;;; Custom variables

(defcustom org-memento-planner-effort-threshold "0:10"
  ""
  :type 'string)

;;;; Faces

(defface org-memento-planner-archived-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "gray72")
    (((class color) (min-colors 88) (background light))
     :foreground "gray35"))
  "")

;;;; Variables

(defvar org-memento-planner-span nil)
(defvar org-memento-planner-date-range nil)

;;;; Major mode

(define-derived-mode org-memento-planner-mode magit-section-mode
  "MmtPlanner"
  (setq-local revert-buffer-function #'org-memento-planner-revert))

;;;; Commands

;;;###autoload
(cl-defun org-memento-planner ()
  (interactive)
  (setq org-memento-planner-span 'week)
  (setq org-memento-planner-date-range (org-memento-timeline--week-range 1))
  (with-current-buffer (get-buffer-create org-memento-planner-buffer)
    (org-memento-planner-mode)
    (org-memento-planner-revert)
    (pop-to-buffer (current-buffer))))

(defun org-memento-planner-revert (&rest _args)
  (unless (derived-mode-p 'org-memento-planner-mode)
    (user-error "Not in org-memento-planner-mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-memento-policy-maybe-load)
    (magit-insert-section (magit-section)
      (org-memento-planner-overview-section)
      (org-memento-planner-agenda-section)
      (org-memento-planner-budgets-section)
      (org-memento-planner-policies-section)
      (org-memento-planner-activities-section))))

(defun org-memento-planner-overview-section ()
  (magit-insert-section (overview)
    (magit-insert-heading
      "Overview")
    (cl-labels
        ((midnight-from-string (string)
           (org-memento--set-time-of-day (parse-time-string string)
                                         0 0 0)))
      (let* ((date (midnight-from-string (car org-memento-planner-date-range)))
             (final-date (midnight-from-string (cadr org-memento-planner-date-range)))
             (sum 0))
        (insert (format "| %-10s | %-5s | %-5s |%-7s| %-6s |\n"
                        "Date" "Start" "End" "Saving" "Mean D"))
        (while (not (org-memento-date--le final-date date))
          (when-let (plist (org-memento--normal-workhour date))
            (let* ((checkin (plist-get plist :normal-checkin))
                   (checkin-time (time-add (encode-time date)
                                           (* 60 (org-duration-to-minutes checkin))))
                   (duration (plist-get plist :normal-duration))
                   (checkout-time (time-add checkin-time
                                            (* 60 (org-duration-to-minutes duration))))
                   (saving (plist-get plist :normal-saving)))
              (insert (format "| %-10s | %5s | %5s | %5s | %6s |\n"
                              (format-time-string "%F" (encode-time date))
                              checkin
                              (org-memento--format-army-time checkout-time
                                                             (float-time (encode-time date)))
                              saving
                              (org-duration-from-minutes
                               (- (org-duration-to-minutes duration)
                                  (org-duration-to-minutes saving)))))
              (cl-incf sum (org-duration-to-minutes duration))))
          (setq date (decoded-time-add date (make-decoded-time :day 1))))
        (insert (format "| %-10s | %-5s | %-5s |%-7s| %-5s |\n"
                        "Sum" "" "" ""
                        (org-memento--format-duration sum)))))
    (insert ?\n)))

(defun org-memento-planner-agenda-section ()
  (let ((end (thread-first
               (cadr org-memento-planner-date-range)
               (parse-time-string)
               (org-memento--set-time-of-day (or org-extend-today-until 0)
                                             0 0)
               (decoded-time-add (make-decoded-time :hour 23 :minute 59))
               (encode-time))))
    (cl-labels
        ((file (item)
           (thread-last
             (org-element-property :hd-marker (nth 2 item))
             (marker-buffer)
             (buffer-file-name)))
         (date (item)
           (format-time-string "%F %a" (car item)))
         (first-date (taxy)
           (thread-last
             (taxy-taxys taxy)
             (mapcar #'taxy-name)
             (seq-sort #'string<)
             (car)))
         (taker (item taxy)
           (taxy-take-keyed
             (list #'file
                   #'date)
             item taxy)))
      (magit-insert-section (planning)
        (magit-insert-heading "Agenda")
        (dolist (date-group (thread-last
                              (make-taxy :take #'taker)
                              (taxy-emptied)
                              (taxy-fill (org-memento-planner--scan-agenda-files end))
                              (taxy-taxys)
                              (seq-group-by #'first-date)
                              (seq-sort-by #'car #'string<)))
          (magit-insert-section (date)
            (magit-insert-heading
              (make-string 2 ?\s)
              (car date-group))
            (dolist (file-taxy (cdr date-group))
              (magit-insert-section (file (taxy-name file-taxy))
                (magit-insert-heading
                  (make-string 4 ?\s)
                  (file-name-nondirectory (taxy-name file-taxy)))
                (dolist (events-taxy (seq-sort-by #'taxy-name #'string<
                                                  (taxy-taxys file-taxy)))
                  (magit-insert-section (date)
                    (magit-insert-heading
                      (make-string 6 ?\s)
                      (taxy-name events-taxy))
                    (dolist (item (taxy-items events-taxy))
                      (insert (make-string 8 ?\s)
                              (org-element-property :raw-value (nth 2 item))
                              "\n")))))))))
      (insert ?\n))))

(defun org-memento-planner--scan-agenda-files (end)
  (let (result)
    (dolist (file (org-agenda-files))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-planning-line-re nil t)
           (unless (equal (match-string 1) "CLOSED:")
             (while (re-search-forward org-ts-regexp (pos-eol) t)
               (when (time-less-p (encode-time
                                   (org-memento--fill-decoded-time
                                    (parse-time-string (match-string 1))))
                                  end)
                 (save-excursion
                   (org-back-to-heading)
                   (let* ((element (org-element-headline-parser (org-entry-end-position)))
                          (effort (org-element-property :EFFORT element)))
                     (unless (or (org-element-property :closed element)
                                 (org-element-property :archivedp element)
                                 (and effort
                                      (<= (org-duration-to-minutes effort)
                                          (org-duration-to-minutes
                                           org-memento-planner-effort-threshold))))
                       (setq element (org-element-put-property element
                                                               :hd-marker (point-marker)))
                       (when-let (scheduled (org-element-property :scheduled element))
                         (push (list (float-time (org-timestamp-to-time scheduled))
                                     'scheduled
                                     element)
                               result))
                       (when-let (deadline (org-element-property :deadline element))
                         (push (list (float-time (org-timestamp-to-time deadline))
                                     'deadline
                                     element)
                               result)))))
                 (goto-char (org-entry-end-position)))))))))
    result))

(defun org-memento-planner-budgets-section ()
  (let* ((rules (org-memento-planner--rules))
         (budget-taxy (org-memento-policy-group-taxy
                       (seq-filter #'org-memento-policy-budget-rule-p rules)))
         (columns '((day minimum)
                    (day goal)
                    (day limit)
                    (week minimum)
                    (week goal)
                    (week limit))))
    (cl-labels
        ((budgetp (span type x)
           (and (eq type (slot-value x 'level))
                (eq span (slot-value x 'span))))
         (makep (span type)
           (apply-partially #'budgetp span type))
         (taxy-budget (taxy span-and-type)
           (when-let (rule (seq-find (apply #'apply-partially #'budgetp span-and-type)
                                     (taxy-items taxy)))
             (slot-value rule 'duration-minutes)))
         (insert-taxy (level taxy)
           (magit-insert-section (group (taxy-name taxy))
             (let ((rules (taxy-items taxy)))
               (magit-insert-heading
                 (apply #'format "  | %-18s | %5s | %5s | %5s | %6s | %6s | %6s |"
                        (concat (make-string (* 2 level) ?\s)
                                (funcall (plist-get (nth level org-memento-group-taxonomy)
                                                    :format)
                                         (nth level (taxy-name taxy))))
                        (thread-last
                          columns
                          (mapcar (apply-partially #'taxy-budget taxy))
                          (mapcar (lambda (duration)
                                    (if duration
                                        (org-memento--format-duration duration)
                                      "")))))))
             (insert-taxys (1+ level) (taxy-taxys taxy))))
         (insert-taxys (level taxys)
           (dolist (taxy taxys)
             (insert-taxy level taxy))
           (when (> (length taxys) 1)
             (insert (apply #'format "  | %-18s | %5s | %5s | %5s | %6s | %6s | %6s |\n"
                            (if (= level 0) "Total" "Subtotal")
                            (thread-last
                              columns
                              (mapcar (lambda (column)
                                        (cl-reduce #'+ (mapcar (lambda (taxy)
                                                                 (or (taxy-budget taxy column)
                                                                     0))
                                                               taxys)
                                                   :initial-value 0)))
                              (mapcar (lambda (duration)
                                        (if (> duration 0)
                                            (org-memento--format-duration duration)
                                          "")))))))))
      (magit-insert-section (budgets)
        (magit-insert-heading
          "Budgets")
        (magit-insert-section (table)
          (magit-insert-heading
            (format "  | %-18s | %-21s | %-24s |\n"
                    "Group" "Daily min/goal/limit" "Weekly min/goal/limit"))
          (insert-taxys 0 (taxy-taxys budget-taxy))))
      (insert ?\n))))

(defun org-memento-planner--rules ()
  (org-memento-policy-rules
   :span org-memento-planner-span
   :start-date (car org-memento-planner-date-range)
   :end-date (cadr org-memento-planner-date-range)))

(defun org-memento-planner-policies-section ()
  (cl-labels
      ((insert-group (level taxy)
         (magit-insert-section (group (taxy-name taxy))
           (magit-insert-heading
             (make-string (* 2 (1+ level)) ?\s)
             (propertize (funcall (plist-get (nth level org-memento-group-taxonomy) :format)
                                  (nth level (taxy-name taxy)))
                         'face
                         'magit-section-heading))
           (dolist (subtaxy (taxy-taxys taxy))
             (insert-group (1+ level) subtaxy)))))
    (magit-insert-section (policies)
      (magit-insert-heading
        "Groups")
      (dolist (group (taxy-taxys (org-memento-policy-group-taxy
                                  (org-memento-policy-contexts))))
        (insert-group 0 group)))
    (insert ?\n)))

(defun org-memento-planner-activities-section ()
  (unless org-memento-group-cache
    (org-memento--cache-groups))
  (magit-insert-section (groups)
    (magit-insert-heading
      "Group activities")
    (cl-labels
        ((transform (group olp)
           (list (car olp)
                 group
                 olp))
         (match-group (group x)
           (org-memento-policy-match-group x group))
         (find-group-context (group)
           (org-memento-policy-find-context
            (apply-partially #'match-group group)))
         (insert-group-subsection (x)
           (pcase-exhaustive x
             (`(,date ,group ,olp)
              (let* ((context-taxy (find-group-context group))
                     (archived (and context-taxy
                                    (slot-value (taxy-name context-taxy) 'archived))))
                (magit-insert-section (group group)
                  (magit-insert-heading
                    (make-string 2 ?\s)
                    date
                    " "
                    (propertize (org-memento--format-group group)
                                'face
                                (if archived
                                    'org-memento-planner-archived-face
                                  'magit-section-heading)))))))))
      (thread-last
        org-memento-group-cache
        (map-apply #'transform)
        (seq-sort-by #'car #'string>)
        (mapc #'insert-group-subsection)))))

(provide 'org-memento-planner)
;;; org-memento-planner.el ends here
