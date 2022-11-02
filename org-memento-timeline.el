;;; org-memento-timeline.el --- Visualize an activity timeline -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `org-memento-timeline'.

;;; Code:

(require 'org-memento)
(require 'org-memento-policy)
(require 'org-memento-yield)
(require 'taxy)
(require 'dash)
(require 'magit-section)

(declare-function org-clocking-p "org-clock")

(defgroup org-memento-timeline nil
  "Activity timeline."
  :prefix "org-memento-"
  :group 'org-memento)

(defconst org-memento-timeline-ms-buffer "*Org-Memento Timeline*")

;;;; Custom variables

(defcustom org-memento-timeline-hook
  '(org-memento-timeline-planning-sections
    org-memento-timeline-section)
  "Hook run every time the buffer is refreshed.

The hook is run inside the timeline buffer.

Each function in the hook takes the taxy representing the
timeline as an argument."
  :type 'hook)

(defcustom org-memento-timeline-planning-hook
  '(org-memento-timeline-progress-section
    org-memento-timeline-agenda-section
    org-memento-timeline-late-blocks-section
    org-memento-timeline-next-event-section
    org-memento-timeline-feasibility-section
    org-memento-timeline-unscheduled-blocks-section)
  "Hook run inside `org-memento-timeline-planning-sections'."
  :type 'hook)

(defcustom org-memento-timeline-hide-planning t
  "Whether to hide the planning section when in a block."
  :type 'boolean)

(defcustom org-memento-timeline-refresh-interval 180
  "Interval in seconds to refresh the timeline."
  :type '(choice (const nil) number))

;;;; Faces

(defface org-memento-timeline-time-face
  '((t (:inherit default :slant italic)))
  "Face for time ranges.")

(defface org-memento-timeline-active-face
  '((((class color) (min-colors 88) (background dark))
     :background "HotPink3")
    (((class color) (min-colors 88) (background light))
     :background "LightPink")
    (t (:inherit default)))
  "Face for an item at the current time.")

(defface org-memento-timeline-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face for warning items.")

;;;; Variables

(defvar org-memento-timeline-refresh-timer nil)

(defvar org-memento-timeline-slots nil
  "Empty time slots in the current span.")

;;;; Macros

(defmacro org-memento-timeline-with-overlay (props &rest progn)
  `(let ((start (point)))
     (prog1 (progn ,@progn)
       (let ((ov (make-overlay start (point))))
         (pcase-dolist (`(,prop . ,value) ',props)
           (overlay-put ov prop (eval value)))))))

(defmacro org-memento-timeline-with-marker-point (&rest progn)
  `(when-let* ((section (magit-current-section))
               (value (oref section value))
               (marker (org-memento-timeline--marker value)))
     (save-current-buffer
       (org-with-point-at (org-memento-planning-item-hd-marker value)
         ,@progn))))

;;;; Display the timeline

(defvar org-memento-timeline-span nil)

(defvar org-memento-timeline-date-range nil)

;;;###autoload
(cl-defun org-memento-timeline (start-day end-day &key span)
  (interactive (if (equal current-prefix-arg '(4))
                   (list (org-read-date)
                         (org-read-date))
                 (let ((today (org-memento--today-string (decode-time))))
                   (list today today :span 'day))))
  (when (string-lessp end-day start-day)
    (user-error "The end day must be no earlier than the start day"))
  (with-current-buffer (get-buffer-create org-memento-timeline-ms-buffer)
    (org-memento-timeline-mode)
    (setq-local org-memento-timeline-date-range (list start-day end-day)
                org-memento-timeline-span span
                revert-buffer-function #'org-memento-timeline-revert)
    (org-memento-timeline-revert)
    (pop-to-buffer (current-buffer))
    (dolist (hook '(org-clock-in-hook
                    org-clock-out-hook
                    org-memento-block-start-hook
                    org-memento-block-exit-hook))
      (add-hook hook 'org-memento-timeline-refresh)))
  (when org-memento-timeline-refresh-timer
    (cancel-timer org-memento-timeline-refresh-timer))
  (when org-memento-timeline-refresh-interval
    (setq org-memento-timeline-refresh-timer
          (run-with-timer org-memento-timeline-refresh-interval t
                          #'org-memento-timeline-refresh-1))))

;;;###autoload
(defun org-memento-timeline-for-week (&optional arg)
  (interactive "P")
  (pcase-let
      ((`(,start ,end) (pcase-exhaustive arg
                         (`nil
                          (org-memento-week-date-range 0))
                         ((pred numberp)
                          (org-memento-week-date-range arg)))))
    (org-memento-timeline start end :span 'week)))

(defun org-memento-timeline-revert (&rest _args)
  (interactive)
  (let ((taxy (apply #'org-memento-activity-taxy
                     (append org-memento-timeline-date-range
                             (list :groups t)))))
    (when (org-memento-timeline--within-range-p taxy)
      (org-memento--status))
    (let ((inhibit-read-only t))
      (delete-all-overlays)
      (erase-buffer)
      (run-hook-with-args 'org-memento-timeline-hook taxy))))

(defun org-memento-timeline-refresh ()
  (when-let (buffer (get-buffer org-memento-timeline-ms-buffer))
    (with-current-buffer buffer
      (org-memento-timeline-revert))))

(defun org-memento-timeline-refresh-1 ()
  (when-let (buffer (get-buffer org-memento-timeline-ms-buffer))
    (unless (or org-memento-block-idle-logging
                (org-clocking-p))
      (with-current-buffer buffer
        (org-memento-timeline-revert)))))

(defun org-memento-timeline-section (root-taxy)
  "Insert the timeline section."
  ;; TODO: Maybe set magit-section-set-visibility-hook
  (let ((now (float-time (org-memento--current-time)))
        last-block-end)
    (cl-labels
        ((get-record (item)
           (if (taxy-p item)
               (taxy-name item)
             item))
         (start-time (item)
           (car (get-record item)))
         (end-time (item)
           (cadr (get-record item)))
         (title (item)
           (caddr (get-record item)))
         (marker (item)
           (cadddr (get-record item)))
         (sort-trees (items)
           (cl-sort items #'< :key #'start-time))
         (format-time-range (start end)
           (concat (if start
                       (format-time-string "%R" start)
                     "")
                   "–"
                   (if end
                       (format-time-string "%R" end)
                     "")
                   (if (and end start)
                       (format " (%s)"
                               (org-memento--format-duration
                                (/ (- end start) 60)))
                     "")))
         (unfinished-clock-p (item)
           (eq (nth 4 item) 'clock-unfinished))
         (highlight-previous-line ()
           (put-text-property (pos-bol 0) (1- (pos-bol))
                              'face 'org-memento-timeline-active-face))
         (insert-items (items end-time-of-block)
           (when items
             (let ((indent1 (make-string 6 ?\s))
                   (indent2 (make-string 15 ?\s)))
               (dolist (group (-partition-by #'caddr items))
                 (magit-insert-section (group (car group)
                                              'hide)
                   (let ((title (caddr (car group))))
                     (magit-insert-heading
                       indent1
                       (format-time-string "%R" (car (car group)))
                       (make-string 2 ?\s)
                       (if title
                           (propertize (org-link-display-format title)
                                       'face 'magit-section-heading)
                         (when (cadr (car group))
                           (propertize "Gap" 'face 'font-lock-comment-face)))
                       (format " (%s)\n"
                               (org-memento--format-duration
                                (/ (- (cadr (car group))
                                      (car (car group)))
                                   60))))
                     (when (unfinished-clock-p (car (last group)))
                       (highlight-previous-line))
                     (when (or title (> (length group) 1))
                       (dolist (clock group)
                         (insert indent2
                                 (format-time-range (start-time clock)
                                                    (end-time clock))
                                 (if (unfinished-clock-p clock)
                                     " (continuing)"
                                   "")
                                 "\n")
                         (when (unfinished-clock-p clock)
                           (highlight-previous-line)))))))
               (let ((last-entry (car (last items))))
                 (unless (eq (nth 4 last-entry)
                             'clock-unfinished)
                   (when-let (last-end (cadr last-entry))
                     (magit-insert-section (clock-out)
                       (magit-insert-heading
                         indent1
                         (format-time-string "%R" last-end)
                         (make-string 2 ?\s)
                         (if (< last-end now)
                             (concat (propertize "Clocked out" 'face 'font-lock-comment-face)
                                     (if end-time-of-block
                                         (format " (until %s)"
                                                 (format-time-string "%R" end-time-of-block))
                                       ""))
                           (propertize "Ending" 'face 'font-lock-comment-face))
                         "\n"))))))
             (insert ?\n)))
         (insert-block (taxy)
           (magit-insert-section (block (taxy-name taxy) 'hide)
             (let ((indent1 (make-string 4 ?\s))
                   (start (start-time taxy))
                   (end (end-time taxy))
                   (nowp (eq 'now (nth 4 (taxy-name taxy)))))
               (if nowp
                   (magit-insert-heading
                     indent1
                     (make-string 6 ?-)
                     (propertize " Now " 'face 'font-lock-comment-face)
                     (make-string (- (window-width)
                                     (+ 4 6 5
                                        ;; margin
                                        5))
                                  ?-))
                 (magit-insert-heading
                   indent1
                   (if start
                       (if (and (> start now)
                                (< start last-block-end))
                           (propertize (format-time-string "%R- " start)
                                       'face 'font-lock-warning-face
                                       'org-memento-warning-type 'overlap)
                         (format-time-string "%R- " start))
                     (make-string 7 ?\s))
                   (if-let (title (title taxy))
                       (propertize title
                                   ;; TODO: Apply a different face if the block
                                   ;; type is away
                                   'face (if (eq (cadddr (cdr (get-record taxy))) 'idle)
                                             'font-lock-comment-face
                                           'magit-section-heading))
                     (if (taxy-items taxy)
                         (let ((marker (cadddr (car (taxy-items taxy)))))
                           (propertize (file-name-base (buffer-name (marker-buffer marker)))
                                       'face 'font-lock-comment-face))
                       (propertize (cond
                                    ((< (float-time) start)
                                     "Empty slot")
                                    ((< end (float-time))
                                     "Gap")
                                    (t
                                     (if (or org-memento-current-block
                                             (org-clocking-p))
                                         "Remaining"
                                       "Empty slot")))
                                   'face 'font-lock-comment-face)))
                   (if (and start end)
                       (format " (%s)"
                               (org-memento--format-duration
                                (/ (- (end-time taxy)
                                      (start-time taxy))
                                   60)))
                     "")))
               (when (or (and (taxy-items taxy)
                              (unfinished-clock-p (car (last (taxy-items taxy)))))
                         (and org-memento-current-block
                              (equal (title taxy) org-memento-current-block)))
                 (highlight-previous-line))
               ;; TODO: Add sum of clocked minutes and the utilization
               (insert-items (taxy-items taxy)
                             ;; Don't show the end of the block if the block is
                             ;; anonymous
                             (when (title taxy)
                               end))
               (setq last-block-end end))))
         (insert-date (taxy)
           (magit-insert-section (date (taxy-name taxy))
             (let ((title (title taxy))
                   (indent (make-string 2 ?\s)))
               (magit-insert-heading
                 (if title
                     (propertize (thread-last
                                   (parse-time-string title)
                                   (org-memento--fill-decoded-time)
                                   (encode-time)
                                   (format-time-string "%F (%a)"))
                                 'face 'magit-section-heading)
                   (propertize (format "(%s between the days)"
                                       (org-memento--format-duration
                                        (/ (- (end-time taxy)
                                              (start-time taxy))
                                           60)))
                               'face 'font-lock-comment-face)))
               (insert indent
                       (if-let (time (start-time taxy))
                           (format-time-string "%R" time)
                         "??:??")
                       (if title
                           " Checked in"
                         "")
                       "\n")
               (dolist (block (taxy-taxys taxy))
                 (insert-block block))
               (when-let (end-time (end-time taxy))
                 (insert indent
                         (format-time-string "%R" end-time)
                         (if title
                             (format (if (< end-time now)
                                         " Checked out (%s)\n"
                                       " Checking out (%s)\n")
                                     (org-memento--format-duration
                                      (/ (- (end-time taxy)
                                            (start-time taxy))
                                         60)))
                           "\n")))))
           (insert ?\n)))
      (magit-insert-section (magit-section)
        (dolist (taxy (taxy-taxys root-taxy))
          (insert-date taxy)))
      (goto-char (point-min)))))

;;;; Mode

(defvar org-memento-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'org-memento-timeline-edit-dwim)
    (define-key map "o" #'org-memento-timeline-open-entry)
    (define-key map "D" #'org-memento-timeline-delete-entry)
    (define-key map (kbd "SPC") #'org-memento-timeline-show-entry)
    map))

;;;###autoload
(define-derived-mode org-memento-timeline-mode magit-section-mode
  "MementoTl"
  "Major mode that displays a timeline of Org Memento.")

;;;;; Commands available in the mode

(defconst org-memento-timeline-indirect-buffer
  "*Org-Memento Indirect*")

(defun org-memento-timeline-open-entry ()
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              (marker (org-memento-timeline--marker value)))
    (org-memento-timeline--display-entry marker #'pop-to-buffer)))

(defun org-memento-timeline-show-entry ()
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value))
              (marker (org-memento-timeline--marker value)))
    (org-memento-timeline--display-entry marker #'display-buffer)))

(defun org-memento-timeline--display-entry (marker fn)
  (interactive)
  (when-let (buffer (get-buffer org-memento-timeline-indirect-buffer))
    (kill-buffer buffer))
  (with-current-buffer (make-indirect-buffer
                        (org-base-buffer (marker-buffer marker))
                        org-memento-timeline-indirect-buffer
                        'clone)
    (goto-char marker)
    (org-narrow-to-subtree)
    (org-show-context 'agenda)
    (funcall fn (current-buffer))))

(defun org-memento-timeline-schedule (&optional arg)
  (interactive "P")
  (org-memento-timeline-with-marker-point
   (org-schedule arg)))

(defun org-memento-timeline-todo ()
  (interactive)
  (org-memento-timeline-with-marker-point
   (org-todo)))

(defun org-memento-timeline-delete-entry ()
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value)))
    (when (pcase value
            ((and `(,start ,_end ,_ ,marker . ,_)
                  (guard marker))
             (cond
              ((and start (> start (float-time (org-memento--current-time))))
               (save-current-buffer
                 (org-with-point-at marker
                   (org-end-of-meta-data t)
                   (if (looking-at (concat org-ts-regexp "\n"))
                       (when (yes-or-no-p "Remove the timestamp of the entry?")
                         (delete-region (point) (1+ (pos-eol)))
                         (message "Removed an active timestamp")
                         t)
                     (error "No timestamp is found")))))
              ((not start)
               (when (yes-or-no-p "Remove the entry?")
                 (save-current-buffer
                   (org-with-point-at marker
                     (org-cut-subtree)))))
              (t
               (user-error "Nothing to do"))))
            (_
             (user-error "Nothing to do")))
      (org-memento-timeline-revert))))

(defun org-memento-timeline-edit-dwim (&optional arg)
  "Adjust the time slice(s) at point.

If ARG is non-nil, create an away event."
  (interactive "P")
  (cl-labels
      ((of-type-p (type record)
         (eq type (nth 4 record)))
       (block-p (record)
         (of-type-p 'block record))
       (time-range (records)
         (list (apply #'min (delq nil (mapcar #'car records)))
               (apply #'max (delq nil (mapcar #'cadr records)))))
       (format-inactive (time)
         (format-time-string (org-time-stamp-format t t) time))
       (set-new-past-range (record start end)
         (save-current-buffer
           (org-with-point-at (nth 3 record)
             (let (updated)
               (when start
                 (org-entry-put nil "MEMENTO_CHECKIN_TIME" (format-inactive start))
                 (setq updated t))
               (when end
                 ;; Throw an error if there is no closed time string
                 (re-search-forward (rx word-start (literal org-closed-string)
                                        (* " ") (group (+ nonl)))
                                    (org-entry-end-position))
                 (replace-match (format-inactive end) nil nil nil 1)
                 (setq updated (or updated t)))
               updated))))
       (adjust-ts (marker)
         (save-current-buffer
           (org-with-point-at marker
             (org-memento-adjust-time))))
       (schedule-new-block (start end-bound)
         (pcase-exhaustive (org-memento--read-time-span
                            (org-memento--format-active-range
                             (if (and start (< start (float-time)))
                                 (+ (float-time) (* 5 60))
                               start)
                             end-bound)
                            start)
           (`(,modified-start ,end)
            (org-memento-schedule-block (float-time modified-start)
                                        (if end
                                            (float-time end)
                                          end-bound)
                                        :confirmed-time t))))
       (add-event (start end &optional moderate-time away)
         (pcase-exhaustive (if moderate-time
                               (org-memento--read-time-span
                                (org-memento--format-active-range start end)
                                start)
                             (list start end))
           (`(,start ,end)
            (org-memento-add-event :start start :end end
                                   :interactive t :away away))))
       (log-away-event (start-bound end-bound marker)
         (pcase-exhaustive (thread-last
                             (org-memento--read-time-span
                              (org-memento--format-active-range start-bound end-bound)
                              start-bound)
                             (mapcar #'float-time))
           (`(,start ,end)
            (org-memento--remove-clock marker start-bound end-bound start end)
            (org-memento-add-event :start start :end end :interactive t :away t)))))
    (let ((now (float-time)))
      (when (catch 'updated
              ;; Has multiple selections
              (if-let (values (magit-region-values))
                  (pcase-let ((`(,start ,end) (time-range values)))
                    (pcase (seq-count #'block-p values)
                      (1
                       (if (< end now)
                           (let ((the-block (seq-find #'block-p values)))
                             (when (yes-or-no-p (format "Expand the time range of\
 the block \"%s\"?"
                                                        (nth 2 the-block)))
                               (throw 'updated (set-new-past-range the-block start end))))
                         (error "Merging is not supported for future slices")))
                      (0
                       (if (and (< start now)
                                (< end now))
                           (add-event start end t)
                         (add-event start end t arg)))
                      (_
                       (error "There are multiple time blocks"))))
                ;; No selection
                (pcase (oref (magit-current-section) value)
                  (`nil
                   (user-error "No section at point"))
                  (`(,start ,end ,_title ,marker ,type . ,_)
                   (cl-case type
                     (dismissed
                      (adjust-ts marker)
                      t)
                     (block
                      ;; Only allow adjusting time of future events.
                      (when (and (> start now))
                        (adjust-ts marker)
                        t))
                     (away
                      (adjust-ts marker)
                      t)
                     (gap
                      (add-event start end t arg))
                     (anonymous
                      (if (> end now)
                          (add-event start end t arg)
                        (add-event start end nil nil)))
                     (idle
                      (log-away-event start end marker))
                     (otherwise
                      (error "Unexpected event type %s" type))))
                  ;; Gap between blocks
                  (`(,start ,end . ,_)
                   (if (> end now)
                       (if arg
                           (add-event (max start (float-time)) end t t)
                         (schedule-new-block (max start (float-time)) end))
                     (add-event start end t arg)))
                  (_
                   (user-error "Don't know what to do for the section")))))
        (org-memento-timeline-revert)))))

;;;; Utility functions

(defun org-memento-timeline--within-range-p (taxy)
  (let ((now (org-memento--current-time)))
    (and (time-less-p (car (taxy-name taxy))
                      now)
         (time-less-p now
                      (cadr (taxy-name taxy))))))

(defun org-memento-timeline--marker (value)
  (pcase value
    ((pred org-memento-planning-item-p)
     (org-memento-planning-item-hd-marker value))
    ((pred org-memento-block-p)
     (org-memento-block-hd-marker value))
    ((and `(,x . ,_)
          (guard (org-memento-block-p x)))
     (org-memento-block-hd-marker x))
    (`(,_ ,_ ,_ ,marker . ,_)
     marker)))

(defun org-memento-timeline-range ()
  "Return the range as a list of internal time representations."
  (list (encode-time
         (org-memento--set-time-of-day
          (parse-time-string (car org-memento-timeline-date-range))
          (or org-extend-today-until 0) 0 0))
        (encode-time
         (decoded-time-add
          (org-memento--set-time-of-day
           (parse-time-string (cadr org-memento-timeline-date-range))
           (or org-extend-today-until 0) 0 0)
          (make-decoded-time :hour 23 :minute 59)))))

;;;; Extra hooks

(defvar org-memento-timeline-progress-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-memento-timeline-progress-return)
    map))

(defun org-memento-timeline-progress-section (taxy)
  (org-memento-policy-maybe-load)
  (let* ((rules (org-memento-policy-rules
                 :span org-memento-timeline-span
                 :start-date (car org-memento-timeline-date-range)
                 :end-date (cadr org-memento-timeline-date-range)))
         (budgets (seq-filter #'org-memento-policy-budget-rule-p rules))
         (sums-for-span (org-memento-group-sums taxy))
         (sums-by-spans (cons (cons org-memento-timeline-span sums-for-span)
                              (when (< (cl-position org-memento-timeline-span
                                                    org-memento-policy-span-types)
                                       (cl-position 'week
                                                    org-memento-policy-span-types))
                                `((week . ,(org-memento--merge-group-sums-1
                                            (list sums-for-span
                                                  org-memento-weekly-group-sums)))))))
         (yields (seq-filter #'org-memento-yield-instance-p rules)))
    (setq org-memento-timeline-slots (org-memento--empty-slots taxy))
    (cl-labels
        ((budget-span (rule)
           (oref rule span))
         (rule-group-path (rule)
           (slot-value (slot-value rule 'context) 'group-path))
         (match-group (group-path group)
           (equal group-path (seq-take group (length group-path))))
         (rule-match-group (group-path rule)
           (match-group group-path (rule-group-path rule)))
         (budget-type-is (level rule)
           (eq level (slot-value rule 'level)))
         (insert-group-status (span group-path group-budgets &optional sum)
           (let ((sum (or sum
                          (cl-reduce
                           #'+
                           (mapcar #'cdr (cl-remove-if-not
                                          (apply-partially #'match-group group-path)
                                          (alist-get span sums-by-spans)
                                          :key #'car))
                           :initial-value 0)))
                 (main-budget (or (seq-find (-partial #'budget-type-is 'goal) group-budgets)
                                  (seq-find (-partial #'budget-type-is 'minimum) group-budgets)
                                  (seq-find (-partial #'budget-type-is 'limit) group-budgets))))
             (magit-insert-section (group-budgets (list span group-path) t)
               (magit-insert-heading
                 (make-string 4 ?\s)
                 (format "| %-12s | %5s%1s%5s %-6s |"
                         (string-pad (org-memento--format-group-last-node group-path)
                                     12)
                         (org-memento--format-duration sum)
                         (if group-budgets "/" "")
                         (if main-budget
                             (org-memento--format-duration
                              (slot-value main-budget 'duration-minutes))
                           "")
                         (if main-budget
                             (cl-ecase (slot-value main-budget 'level)
                               (minimum "(min.)")
                               (goal "(goal)")
                               (`limit "(lim.)"))
                           "")))
               (dolist (yield-rule (seq-filter (apply-partially #'rule-match-group group-path)
                                               yields))
                 (magit-insert-section (yield-rule yield-rule)
                   (dolist (task (car (org-memento-yield-some
                                       yield-rule
                                       (org-memento-yield--activities-1 yield-rule taxy))))
                     (magit-insert-section (task-and-slots task)
                       (magit-insert-heading
                         (make-string 6 ?\s)
                         "+ "
                         (propertize (org-memento-order-title task)
                                     'face 'magit-section-heading)
                         (when-let (duration (org-memento-order-duration task))
                           (concat " " (org-memento--format-duration duration)))))))))))
         (in-some-group (group-paths group)
           (seq-find (-partial (-flip #'match-group) group)
                     group-paths)))
      (magit-insert-section (magit-section)
        (magit-insert-heading
          "Progress")
        (org-memento-timeline-with-overlay
         ((keymap . org-memento-timeline-progress-map))
         (pcase-dolist (`(,span . ,budget-rules-for-span)
                        (thread-last
                          budgets
                          (seq-group-by #'budget-span)
                          (seq-sort-by #'car #'org-memento-policy--compare-span-types)))
           (when (<= (cl-position org-memento-timeline-span
                                  org-memento-policy-span-types)
                     (cl-position span
                                  org-memento-policy-span-types))
             (magit-insert-section (span span)
               (magit-insert-heading
                 (make-string 2 ?\s)
                 (cl-ecase span
                   (day "Daily")
                   (week "Weekly")
                   (month "Monthly"))
                 "\n"
                 (make-string 4 ?\s)
                 (format "| %-12s | %11s %-6s |\n"
                         "Group" "" ""))
               (let ((groups-with-budgets
                      (thread-last
                        budget-rules-for-span
                        (seq-group-by #'rule-group-path))))
                 (pcase-dolist (`(,group-path . ,group-budget-rules)
                                groups-with-budgets)
                   (insert-group-status span group-path group-budget-rules))
                 (pcase-dolist (`(,group . ,sum)
                                (thread-last
                                  (cl-remove-if (apply-partially
                                                 #'in-some-group
                                                 (mapcar #'car groups-with-budgets))
                                                (alist-get span sums-by-spans)
                                                :key #'car)
                                  (seq-group-by #'caar)
                                  (mapcar (pcase-lambda (`(,node . ,groups-and-sums))
                                            (cons (list node)
                                                  (cl-reduce #'+
                                                             (mapcar #'cdr groups-and-sums)
                                                             :initial-value 0))))))
                   (insert-group-status span group nil sum))))))))
      (insert ?\n))))

(defun org-memento-timeline-progress-return ()
  (interactive)
  (if-let* ((section (magit-current-section))
            (value (oref section value))
            (type (oref section type)))
      (cl-etypecase value
        (org-memento-order
         (pcase-let*
             ((slots org-memento-timeline-slots)
              (title (org-memento-read-title nil :default (org-memento-order-title value)))
              (duration (org-memento-order-duration value))
              (slot (when slots
                      (org-memento-select-slot (format "Choose a slot for \"%s\": "
                                                       title)
                                               (if duration
                                                   (seq-filter `(lambda (slot)
                                                                  (<= (- (cadr slot)
                                                                         (car slot))
                                                                      (* ,duration)))
                                                               slots)
                                                 slots))))
              (`(,start ,end) (org-memento--read-time-span
                               (when slot
                                 (org-memento--format-timestamp
                                  (car slot)
                                  (if duration
                                      (+ (car slot) (* 60 duration))
                                    (cadr slot))))
                               (float-time (org-memento--current-time)))))
           (org-memento-add-event :title title
                                  :start start
                                  :end (or end
                                           (when (and start duration)
                                             (+ start (* 60 duration))))
                                  :interactive t
                                  :group (org-memento--default-group
                                          (org-memento-order-group value))
                                  :copy-from (org-memento-order-sample-marker value)))))
    (user-error "No value")))

(defun org-memento-timeline-planning-sections (taxy)
  (unless (and org-memento-timeline-hide-planning
               org-memento-current-block)
    (run-hook-with-args 'org-memento-timeline-planning-hook taxy)))

(defvar org-memento-timeline-planning-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'org-memento-timeline-edit-agenda-item)
    (define-key map (kbd "C-c C-s") #'org-memento-timeline-schedule)
    (define-key map (kbd "C-c C-t") #'org-memento-timeline-todo)
    ;; (define-key map (kbd "C-c C-d") #'org-memento-timeline-deadline)
    map))

(defun org-memento-timeline-agenda-section (taxy)
  (when (org-memento-timeline--within-range-p taxy)
    (let ((planned-items (thread-last
                           (org-memento--blocks)
                           (seq-filter #'org-memento-block-not-closed-p)
                           (mapcar #'org-memento-get-planning-items)
                           (apply #'append))))
      (cl-labels
          ((file (item)
             (buffer-name (marker-buffer (org-memento-planning-item-hd-marker item))))
           (take-file (item taxy)
             (taxy-take-keyed
               (list #'file)
               item taxy))
           (planned (item)
             (assoc (org-memento-planning-item-id item) planned-items)))
        (when-let (planning-items
                   (cl-remove-if #'planned (org-memento--planning-items)))
          (magit-insert-section (magit-section)
            (magit-insert-heading "Planning Items")
            (org-memento-timeline-with-overlay
             ((keymap . org-memento-timeline-planning-map))
             (dolist (taxy (taxy-taxys (thread-last
                                         (make-taxy :take #'take-file)
                                         (taxy-emptied)
                                         (taxy-fill planning-items))))
               (magit-insert-section (planning-group)
                 (magit-insert-heading
                   (make-string 2 ?\s)
                   (taxy-name taxy))
                 (dolist (item (taxy-items taxy))
                   (magit-insert-section (planning item)
                     (magit-insert-heading
                       (make-string 4 ?\s)
                       (org-memento-planning-item-heading item)))))))))
        (insert ?\n)))))

(defun org-memento-timeline-edit-agenda-item ()
  (interactive)
  (if-let (values (magit-region-values))
      (when-let (values (seq-filter #'org-memento-planning-item-p values))
        (org-memento-schedule-planning-items values))
    (let ((value (oref (magit-current-section) value)))
      (when (org-memento-planning-item-p value)
        (org-memento-schedule-planning-items (list value))))))

(defun org-memento-timeline-late-blocks-section (taxy)
  (when (org-memento-timeline--within-range-p taxy)
    (let ((now (float-time (org-memento--current-time))))
      (cl-flet
          ((block-due-p (block)
             (and (org-memento-block-not-closed-p block)
                  (not (org-memento-started-time block))
                  (when-let (starting (org-memento-starting-time block))
                    (< starting now)))))
        (when-let (blocks (thread-last
                            (org-memento--blocks)
                            (seq-filter #'block-due-p)))
          (magit-insert-section (magit-section)
            (magit-insert-heading "Late blocks")
            (insert (make-string 2 ?\s)
                    "There are overdue events.\n")
            (dolist (block blocks)
              (org-memento-timeline--insert-block 1 block :type 'dismissed)))
          (insert ?\n))))))

(defun org-memento-timeline-next-event-section (taxy)
  "Insert information on the next event(s) on the day.

You should update the status before you call this function."
  (when (org-memento-timeline--within-range-p taxy)
    (let ((now (float-time (org-memento--current-time))))
      (cl-flet
          ((block-scheduled-future-p (block)
             (and (org-memento-block-not-closed-p block)
                  (org-memento-starting-time block)
                  (> (org-memento-starting-time block) now))))
        (magit-insert-section (magit-section)
          (magit-insert-heading "Next Event")
          (let* ((next-block (thread-last
                               (org-memento--blocks)
                               (seq-filter #'block-scheduled-future-p)
                               (seq-sort-by #'org-memento-starting-time #'<)
                               (car)))
                 (event (org-memento--next-agenda-event
                         nil
                         (when next-block
                           (org-memento-starting-time next-block)))))
            (if (or event next-block)
                (org-memento-timeline--insert-block 1 (or event next-block))
              (insert (make-string 2 ?\s)
                      "No next event.\n"))))
        (insert ?\n)))))

(defun org-memento-timeline-unscheduled-blocks-section (taxy)
  (when (org-memento-timeline--within-range-p taxy)
    (cl-flet
        ((block-unscheduled-p (block)
           (and (org-memento-block-not-closed-p block)
                (not (org-memento-starting-time block))
                (not (org-memento-started-time block)))))
      (when-let (blocks (thread-last
                          (org-memento--blocks)
                          (seq-filter #'block-unscheduled-p)))
        (magit-insert-section (magit-section)
          (magit-insert-heading "Blocks without time")
          (dolist (block blocks)
            (org-memento-timeline--insert-block 1 block :omit-time t)))
        (insert ?\n)))))

(cl-defun org-memento-timeline--insert-block (level block &key omit-time type)
  (let ((start (org-memento-starting-time block))
        (end (org-memento-ending-time block))
        (title (org-memento-title block))
        (marker (cl-typecase block
                  (org-memento-block
                   (org-memento-block-hd-marker block))
                  (org-memento-org-event
                   (org-memento-org-event-marker block))))
        (type (or type
                  (cl-typecase block
                    (org-memento-block 'block)
                    (org-memento-org-event 'org-event)))))
    (magit-insert-section (block (list start end title marker type))
      (magit-insert-heading
        (make-string (* 2 level) ?\s)
        (unless omit-time
          (concat (propertize (format-time-string "%R-" start)
                              'face 'font-lock-warning-face)
                  (if end
                      (propertize (format-time-string "%R" end)
                                  'face 'font-lock-warning-face)
                    (make-string 5 ?\s))
                  " "))
        (propertize title 'face 'magit-section-heading)))))

(defvar org-memento-timeline-feasibility-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'org-memento-timeline-edit-feasibility)
    map))

(defun org-memento-timeline-feasibility-section (taxy)
  (when (org-memento-timeline--within-range-p taxy)
    (let ((planning-items (org-memento--planning-items))
          (now (float-time (org-memento--current-time))))
      (cl-labels
          ((block-not-started-p (block)
             (not (org-memento-started-time block)))
           (duration-from-time (block)
             (when-let* ((starting (org-memento-starting-time block))
                         (ending (org-memento-ending-time block)))
               (/ (- ending starting) 60)))
           (find-planning-item (cell)
             (or (cl-find (car cell) planning-items
                          :key #'org-memento-planning-item-id
                          :test #'equal)
                 (error "Cannot find an item \"%s\" by its ID" (cdr cell))))
           (compare-maybe-number (x y)
             (if (and x y)
                 (< x y)
               x))
           (insert-block (block)
             (let* ((duration (or (org-memento-duration block)
                                  (duration-from-time block)))
                    (items (thread-last
                             (org-memento-get-planning-items
                              (org-memento-block-hd-marker block))
                             (mapcar #'find-planning-item)))
                    (effort-values (when (seq-every-p #'org-memento-planning-item-effort
                                                      items)
                                     ;; Some of the effort property values can be in an
                                     ;; invalid form. In that case, the resulting value
                                     ;; will be nil.
                                     (ignore-errors
                                       (mapcar #'org-memento-duration items))))
                    (effort-sum (when effort-values
                                  (-sum effort-values))))
               (magit-insert-section (block-feasibility (cons block effort-sum) nil)
                 (magit-insert-heading
                   (make-string 2 ?\s)
                   (format "%4s / %4s "
                           (propertize (if effort-sum
                                           (org-duration-from-minutes effort-sum)
                                         "??")
                                       'face
                                       (if (and effort-sum
                                                duration
                                                (<= effort-sum duration))
                                           'font-lock-constant-face
                                         'font-lock-warning-face))
                           (propertize (if duration
                                           (org-duration-from-minutes duration)
                                         "??")
                                       'face
                                       (if duration
                                           'default
                                         'font-lock-warning-face)))
                   (propertize (org-memento-title block)
                               'face 'magit-section-heading))
                 (dolist (item items)
                   (magit-insert-section (planning-item item)
                     (magit-insert-heading
                       (make-string 2 ?\s)
                       (format "%4s"
                               (if-let (duration (org-memento-planning-item-effort item))
                                   duration
                                 "??"))
                       (make-string 8 ?\s)
                       (propertize (org-memento-planning-item-heading item)
                                   'face 'default))))
                 (insert ?\n)))))
        (when-let (blocks (thread-last
                            (org-memento--blocks)
                            (seq-filter #'block-not-started-p)))
          (magit-insert-section (magit-section)
            (magit-insert-heading
              "Feasibility")
            (let (overlaps)
              (dolist (date-taxy (taxy-taxys taxy))
                (let (last-end)
                  (when (< (car (taxy-name date-taxy)) now)
                    (dolist (block-taxy (taxy-taxys date-taxy))
                      (let* ((record (taxy-name block-taxy))
                             (start (car record))
                             (end (cadr record)))
                        (when (> end now)
                          (when (and last-end
                                     (< start last-end))
                            (push (cons record last-end) overlaps))
                          (setq last-end end)))))))
              (when overlaps
                (pcase-dolist (`(,record . ,last-end) overlaps)
                  (magit-insert-section (block record t)
                    (magit-insert-heading
                      (make-string 2 ?\s)
                      (propertize (nth 2 record) 'face 'magit-section-heading)
                      (format-spec
                       " starts at %s before its previous event ends at %e (overlap)."
                       `((?s . ,(format-time-string "%R" (car record)))
                         (?e . ,(format-time-string "%R" last-end)))))))
                (insert ?\n)))
            (org-memento-timeline-with-overlay
             ((keymap . org-memento-timeline-feasibility-map))
             (dolist (block (seq-sort-by #'org-memento-starting-time
                                         #'compare-maybe-number
                                         blocks))
               (insert-block block))))
          (insert ?\n))))))

(defun org-memento-timeline-edit-feasibility ()
  (interactive)
  (let ((value (oref (magit-current-section) value)))
    (when (cl-ecase (oref (magit-current-section) type)
            (block-feasibility
             (let ((block (car value)))
               (save-current-buffer
                 (org-with-point-at (org-memento-block-hd-marker block)
                   (if (org-memento-ending-time block)
                       (org-memento-adjust-time)
                     (org-set-effort))))))
            (planning-item
             (save-current-buffer
               (org-with-point-at (org-memento-planning-item-hd-marker value)
                 (org-set-effort)
                 t))))
      (org-memento-timeline-revert))))

;;;;; Overview

;; This needs rewrite.
(defun org-memento-timeline-insert-insights (taxy)
  (magit-insert-section (overview (taxy-name taxy) nil)
    (magit-insert-heading "Insights")
    (cl-labels
        ((get-record (x)
           (if (taxy-p x)
               (taxy-name x)
             x))
         (get-marker (x)
           (cadddr (get-record x)))
         (start-time (x)
           (car (get-record x)))
         (end-time (x)
           (cadr (get-record x)))
         (title (x)
           (caddr (get-record x))))
      (let ((indent1 (make-string 2 ?\s))
            (indent2 (make-string 4 ?\s))
            (ndays (thread-last
                     (taxy-taxys taxy)
                     (seq-filter #'title)
                     (length))))
        (magit-insert-section (magit-section)
          (magit-insert-heading indent1 "Time on categories")
          (let (block-activities)
            (dolist (date-taxy (taxy-taxys taxy))
              (dolist (block-taxy (taxy-taxys date-taxy))
                (when-let* ((block-marker (get-marker block-taxy))
                            (start (start-time block-taxy))
                            (end (end-time block-taxy)))
                  (save-current-buffer
                    (org-with-point-at block-marker
                      (when-let (category (org-entry-get nil "memento_category"))
                        (push (cons category
                                    (round (/ (- end start) 60)))
                              block-activities)))))))
            (if block-activities
                (let* ((statistics (thread-last
                                     (seq-group-by #'car block-activities)
                                     (mapcar (pcase-lambda (`(,category . ,alist))
                                               (let ((sum (cl-reduce #'+ (mapcar #'cdr alist)
                                                                     :initial-value 0)))
                                                 (list category
                                                       (org-memento--format-duration sum)
                                                       (org-memento--format-duration
                                                        (/ sum ndays))))))))
                       (heading1 "Category")
                       (width1 (thread-last
                                 (mapcar #'car statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading1))))
                       (heading2 "Actual")
                       (width2 (thread-last
                                 (mapcar #'cadr statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading2))))
                       (heading3 "Average")
                       (width3 (thread-last
                                 (mapcar #'caddr statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading3)))))
                  (insert indent1 "| " (string-pad heading1 width1)
                          " | " (string-pad heading2 width2)
                          " | " (string-pad heading3 width3)
                          " |\n")
                  (pcase-dolist (`(,category ,actual ,average)
                                 statistics)
                    (insert indent1 "| "
                            (string-pad category width1)
                            " | " (string-pad actual width2)
                            " | " (string-pad average width3)
                            " |\n")))
              (insert indent2 "No activities yet.\n")))))))
  (insert ?\n))

(provide 'org-memento-timeline)
;;; org-memento-timeline.el ends here
