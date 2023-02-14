;;; org-memento-zone.el --- Zone list -*- lexical-binding: t -*-

(require 'org-memento-timeline)

(declare-function org-clocking-p "org-clock")

(defconst org-memento-zone-list-buffer "*Org-Memento Zones*")

(defvar org-memento-zone-current nil
  "Path to the last visited zone.")

(defvar org-memento-zone-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'org-memento-timeline-add)
    ;; (define-key map "e" #'org-memento-timeline-edit-dwim)
    ;; (define-key map "r" #'org-memento-timeline-rename)
    (define-key map "o" #'org-memento-timeline-open-entry)
    ;; (define-key map "D" #'org-memento-timeline-delete-entry)
    (define-key map (kbd "C-c C-n") #'org-memento-zone-find-next)
    (define-key map (kbd "SPC") #'org-memento-timeline-show-entry)
    map))

(define-derived-mode org-memento-zone-mode magit-section-mode
  "Major mode for presenting zones.")

;;;###autoload
(defun org-memento-zone-list ()
  "Display zones in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create org-memento-zone-list-buffer)
    (org-memento-zone-mode)
    (org-memento-zone--revert)
    (setq-local revert-buffer-function #'org-memento-zone--revert)
    (pop-to-buffer (current-buffer))))

(defun org-memento-zone--revert (&rest _)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((today (org-memento--today-string (decode-time))))
      (setq-local org-memento-timeline-span 'day
                  org-memento-timeline-date-range (list today today)))
    (org-memento-zone--insert-content)
    (goto-char (point-min))))

(defun org-memento-zone--insert-content ()
  (let* ((now (float-time (org-memento--current-time)))
         (today (org-memento--today-string))
         (taxy (org-memento-activity-taxy today today))
         (all-items (append (thread-last
                              (org-memento-yield-for-span taxy 'day
                                :start-date today
                                :end-date today
                                :require-budget t)
                              (mapcan #'cdr)
                              (mapcan #'identity))
                            (org-memento--blocks)
                            (org-memento--planning-items)))
         (planned-items-map (thread-last
                              (org-memento--blocks)
                              (seq-filter #'org-memento-block-not-closed-p)
                              (mapcar (lambda (block)
                                        (cons (org-memento-title block)
                                              (org-memento-get-planning-items block))))))
         (planned-items (thread-last
                          (map-values planned-items-map)
                          (apply #'append)))
         (clocked-item-id (when (org-clocking-p)
                            (org-with-point-at org-clock-marker
                              (org-id-get)))))
    (cl-labels
        ((make-indent (level)
           (let ((n (* 2 (1- level))))
             (if (> n 0)
                 (make-string n ?\s)
               "")))
         (planned (item)
           (and (org-memento-planning-item-p item)
                (assoc (org-memento-planning-item-id item) planned-items)))
         (donep (item)
           (cl-typecase item
             (org-memento-block
              (org-memento-ended-time item))
             (org-memento-planning-item
              (org-with-point-at (org-memento-planning-item-hd-marker item)
                (org-entry-is-done-p)))))
         (highlight-previous-line ()
           (put-text-property (pos-bol 0) (1- (pos-bol))
                              'face 'org-memento-timeline-active-face))
         (insert-subheading (level text)
           (magit-insert-heading
             (make-indent (1+ level))
             (propertize text 'face 'org-memento-timeline-subheading-face)))
         (insert-order (level order)
           (magit-insert-section (generated-task order)
             (magit-insert-heading
               (make-indent (+ 2 level))
               (propertize (org-memento-order-title order)
                           'face 'org-memento-timeline-order-face)
               (when-let (duration (org-memento-order-duration order))
                 (concat " " (org-memento--format-duration duration)))
               (format " (%s)" (org-memento--format-group
                                (org-memento-group-path order)))
               (when-let (latest
                          (caar (org-memento-order-previous-activities order)))
                 (concat " "
                         (propertize (org-memento--format-diff-days (- now latest))
                                     'face 'font-lock-comment-face))))))
         (insert-planning-item (level item)
           (let ((filename (thread-last
                             (org-memento-planning-item-hd-marker item)
                             (marker-buffer)
                             (org-base-buffer)
                             (buffer-file-name)))
                 (todo (org-with-point-at (org-memento-planning-item-hd-marker item)
                         (org-get-todo-state)))
                 (effort (org-memento-planning-item-effort item)))
             (magit-insert-section (planning item)
               (magit-insert-heading
                 (make-indent (+ 2 level))
                 ;; (if (donep item)
                 ;;     (concat org-memento-timeline-done-format " ")
                 ;;   "  ")
                 (propertize (format "%s: " (file-name-base filename))
                             'help-echo (abbreviate-file-name filename))
                 (if todo
                     (concat todo " ")
                   "")
                 (propertize (org-memento-planning-item-heading item)
                             'face 'org-memento-timeline-agenda-item-face)
                 (if effort
                     (concat " " effort)
                   "")))))
         (current-item-or-block-p (item)
           (cl-typecase item
             (org-memento-block
              (and org-memento-current-block
                   (equal org-memento-current-block (org-memento-title item))))
             (org-memento-planning-item
              (and clocked-item-id
                   (equal (org-memento-planning-item-id item)
                          clocked-item-id)))))
         (insert-block (level item)
           (magit-insert-section (block item)
             (magit-insert-heading
               (make-indent level)
               (pcase (org-element-property :todo-keyword (org-memento-headline-element item))
                 (`nil
                  "  ")
                 ((and kwd (guard (equal org-memento-todo-keyword-for-success kwd)))
                  (concat org-memento-timeline-done-format " "))
                 (kwd
                  (concat kwd " ")))
               (propertize (org-memento-title item)
                           'face 'org-memento-timeline-group-path-face)
               (when-let* ((start (org-memento-started-time item))
                           (end (org-memento-ended-time item)))
                 (format " (%s)" (org-memento--format-duration (/ (- end start) 60)))))
             (when (current-item-or-block-p item)
               (highlight-previous-line))
             (when-let (items (map-elt planned-items-map (org-memento-title item)))
               (magit-insert-section (blocks)
                 (magit-insert-heading
                   (make-indent (1+ level))
                   "TODO")
                 (pcase-dolist (`(,id . ,title) items)
                   (magit-insert-section (item id)
                     (magit-insert-heading
                       (make-indent (+ 2 level))
                       (propertize title 'face 'org-memento-timeline-agenda-item-face))))))))
         (insert-items (level items &key hide-suggestions)
           (pcase-let*
               ((`(,done-items-and-blocks ,undone-items-and-blocks)
                 (thread-last
                   items
                   (cl-remove-if #'org-memento-order-p)
                   (cl-remove-if #'planned)
                   (-separate #'donep)))
                (`(,current-items-and-blocks ,unstarted-items-and-blocks)
                 (thread-last
                   undone-items-and-blocks
                   (-separate #'current-item-or-block-p)))
                (`(,items-and-blocks-with-time ,items-and-blocks-without-time)
                 (thread-last
                   unstarted-items-and-blocks
                   (-separate #'org-memento-starting-time)))
                (`(,items-without-time ,blocks-without-time)
                 (-separate #'org-memento-planning-item-p items-and-blocks-without-time)))
             (dolist (item done-items-and-blocks)
               (cl-etypecase item
                 (org-memento-block
                  (insert-block level item))
                 (org-memento-planning-item
                  (org-memento-timeline-with-overlay
                   ((keymap . org-memento-timeline-planning-map))
                   (insert-planning-item level item)))))
             (when current-items-and-blocks
               (magit-insert-section (items)
                 (insert-subheading level "In progress")
                 (dolist (item current-items-and-blocks)
                   (cl-etypecase item
                     (org-memento-block
                      (insert-block level item))
                     (org-memento-planning-item
                      (unless org-memento-current-block
                        (org-memento-timeline-with-overlay
                         ((keymap . org-memento-timeline-planning-map))
                         (insert-planning-item level item))))))))
             (when items-without-time
               (magit-insert-section (items)
                 (insert-subheading level "Not allocated")
                 (org-memento-timeline-with-overlay
                  ((keymap . org-memento-timeline-planning-map))
                  (dolist (item items-without-time)
                    (insert-planning-item level item)))
                 (insert ?\n)))
             (when items-and-blocks-with-time
               (magit-insert-section (items)
                 (insert-subheading level "Scheduled with timing")
                 (dolist (item (seq-sort-by #'org-memento-starting-time #'<
                                            items-and-blocks-with-time))
                   (cl-etypecase item
                     (org-memento-block
                      (insert-block (1+ level) item))
                     (org-memento-planning-item
                      (org-memento-timeline-with-overlay
                       ((keymap . org-memento-timeline-planning-map))
                       (insert-planning-item (1+ level) item)))))))
             (when blocks-without-time
               (magit-insert-section (items)
                 (insert-subheading level "Unscheduled blocks")
                 (dolist (item blocks-without-time)
                   (insert-block level item))))
             (when-let (suggestions (seq-filter #'org-memento-order-p items))
               (magit-insert-section (suggestions nil hide-suggestions)
                 (insert-subheading level "Suggestions")
                 (dolist (suggestion suggestions)
                   (insert-order level suggestion))))))
         (done-duration (item)
           (cl-typecase item
             (org-memento-block
              (when-let* ((started (org-memento-started-time item))
                          (ended (org-memento-ended-time item)))
                (/ (- ended started)
                   60)))))
         (planned-duration (item)
           (cl-typecase item
             (org-memento-block
              (unless (and (org-memento-started-time item)
                           (org-memento-ended-time item))
                (org-memento--block-duration item)))))
         (sum-duration (durations)
           (cl-reduce #'+ (delq nil durations) :initial-value 0))
         (format-zone-status (zone-taxy)
           (cond
            ((plist-get (cdr (taxy-name zone-taxy)) :complete)
             (concat org-memento-timeline-done-format " "))
            (t
             "  ")))
         (format-meta (&key spent planned goal)
           (format-spec (propertize " (%d spent%p%g)"
                                    'face 'default)
                        `((?d . ,(org-memento--format-duration spent))
                          (?p . ,(if (and planned (> planned 0))
                                     (format " / %s planned"
                                             (org-memento--format-duration
                                              (+ planned spent)))
                                   ""))
                          (?g . ,(if (and goal (> goal 0))
                                     (format " / %s goal"
                                             (org-memento--format-duration goal))
                                   "")))))
         (zone-goal-in-minutes (zone-taxy)
           (if-let (goal-string (thread-first
                                  (cdr (taxy-name zone-taxy))
                                  (plist-get :duration)))
               (org-duration-to-minutes goal-string)
             0))
         (insert-zone (parent-zone-path zone-taxy)
           (let* ((label (car (taxy-name zone-taxy)))
                  (zone-path (append parent-zone-path
                                     (when label
                                       (list label))))
                  (level (length zone-path))
                  (plist (cdr (taxy-name zone-taxy)))
                  (spent (thread-last
                           (taxy-flatten zone-taxy)
                           (mapcar #'done-duration)
                           (sum-duration)))
                  (planned (thread-last
                             (taxy-flatten zone-taxy)
                             (mapcar #'planned-duration)
                             (sum-duration)))
                  (goal (zone-goal-in-minutes zone-taxy))
                  (goal (if (> goal 0)
                            goal
                          (cl-reduce #'+
                                     (mapcar #'zone-goal-in-minutes
                                             (taxy-taxys zone-taxy))
                                     :initial-value 0)))
                  (hide-suggestions (and planned goal (>= planned goal))))
             (magit-insert-section (zone (cons zone-path
                                               (list :spent spent
                                                     :planned planned
                                                     :goal goal))
                                         (and (= goal 0)
                                              (= planned 0)
                                              (= spent 0)))
               (when-let (name (car (taxy-name zone-taxy)))
                 (magit-insert-heading
                   (make-indent (if parent-zone-path
                                    (1- (length zone-path))
                                  (length zone-path)))
                   (when parent-zone-path
                     (format-zone-status zone-taxy))
                   (propertize name
                               'face
                               (if (equal (seq-take org-memento-zone-current
                                                    (1+ (length parent-zone-path)))
                                          (append parent-zone-path (list name)))
                                   'org-memento-timeline-active-zone-title-face
                                 'org-memento-timeline-zone-title-face))
                   (format-meta :spent spent :planned planned :goal goal))
                 (when-let (description (taxy-description zone-taxy))
                   (insert (make-indent (1+ level))
                           (propertize description 'face 'org-memento-timeline-zone-desc-face)
                           "\n")))
               (when-let (groups (plist-get plist :groups))
                 (magit-insert-section (associated-groups
                                        nil
                                        (or hide-suggestions
                                            (or (taxy-taxys zone-taxy)
                                                (taxy-items zone-taxy))))
                   (insert-subheading level "Primary groups")
                   (dolist (group groups)
                     (magit-insert-section (group group)
                       (magit-insert-heading
                         (make-indent (+ 2 level))
                         (propertize (org-memento--format-group group)
                                     'face 'org-memento-timeline-group-path-face))))))
               (when (car (taxy-name zone-taxy))
                 (insert ?\n))
               (if (taxy-taxys zone-taxy)
                   (progn
                     (dolist (subtaxy (taxy-taxys zone-taxy))
                       (insert-zone zone-path subtaxy))
                     (when (taxy-items zone-taxy)
                       (let ((level1 (if (car (taxy-name zone-taxy))
                                         (1+ level)
                                       0)))
                         (magit-insert-section (zone (cons (taxy-name zone-taxy)
                                                           nil))
                           (magit-insert-heading
                             (when (car (taxy-name zone-taxy))
                               (make-indent level1))
                             (propertize "Others" 'face 'org-memento-timeline-zone-title-face)
                             (format-meta :spent (thread-last
                                                   (taxy-items zone-taxy)
                                                   (mapcar #'done-duration)
                                                   (sum-duration))))
                           (insert-items (1+ level1) (taxy-items zone-taxy))))))
                 (insert-items level (taxy-items zone-taxy)
                               :hide-suggestions hide-suggestions))
               (insert ?\n)))))
      (if org-memento-zone-taxy
          (insert-zone nil (thread-last
                             (copy-taxy org-memento-zone-taxy)
                             (taxy-emptied)
                             (taxy-fill all-items)))
        (magit-insert-section (zones)
          (magit-insert-heading "Tasks")
          (insert-items 0 all-items))))))

(defun org-memento-zone-find-next (&optional arg)
  "Move point to the next actionable item in the timeline.

With two universal prefixes, the function moves the point to the
last visited zone with the same command."
  (interactive "P")
  (if (equal arg '(16))
      (org-memento-zone--goto org-memento-zone-current)
    (let ((start (point)))
      (if-let (section (catch 'section
                         (while-let ((pos (and (< (point) (point-max))
                                               (next-single-property-change (point)
                                                                            'magit-section))))
                           (goto-char pos)
                           (when-let (section (magit-current-section))
                             (cond
                              ((org-memento-zone--actionable-p section)
                               (throw 'section section))
                              ((eq 'zone (oref section type))
                               (setq org-memento-zone-current
                                     (car (oref section value)))))))))
          (save-excursion
            (while (and (magit-section-invisible-p section)
                        (not (org-memento-zone--complete-p section)))
              (magit-section-up)
              (magit-section-show-headings (magit-current-section))))
        (goto-char start)))))

(defun org-memento-zone--goto (path)
  (let ((pos (point)))
    (goto-char (point-min))
    (catch 'found
      (while (text-property-search-forward 'magit-section)
        (when-let (section (magit-current-section))
          (when (and (eq 'zone (oref section type))
                     (equal path (car (oref section value))))
            (throw 'found (point)))))
      (goto-char pos)
      (message "Path %s is not found" path))))

(defun org-memento-zone--complete-p (section)
  (and (eq (oref section type) 'zone)
       (when-let* ((plist (cdr (oref section value)))
                   (spent (plist-get plist :spent))
                   (goal (plist-get plist :goal)))
         (and (> goal 0)
              (> spent goal)))))

(defun org-memento-zone--actionable-p (section)
  (let ((type (oref section type)))
    (or (memq type '(generated-task
                     planning))
        (and (eq type 'block)
             (org-memento-block-not-closed-p (oref section value))))))

(provide 'org-memento-zone)
;;; org-memento-zone.el ends here
