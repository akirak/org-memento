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

(defconst org-memento-timeline-buffer "*Org-Memento Timeline*")

;;;; Custom variables

(defcustom org-memento-timeline-hook
  '(org-memento-timeline-overview-section
    org-memento-timeline-planning-sections
    org-memento-timeline-section)
  "Hook run every time the buffer is refreshed.

The hook is run inside the timeline buffer.

Each function in the hook takes the taxy representing the
timeline as an argument."
  :type 'hook)

(defcustom org-memento-timeline-planning-hook
  '(org-memento-timeline-progress-section
    org-memento-timeline-suggestions-section
    org-memento-timeline-agenda-section
    org-memento-timeline-late-blocks-section
    org-memento-timeline-next-event-section
    org-memento-timeline-feasibility-section
    org-memento-timeline-unscheduled-blocks-section)
  "Hook run inside `org-memento-timeline-planning-sections'."
  :type 'hook)

(defcustom org-memento-timeline-initial-position 'now
  "Position after the timeline is loaded."
  :type '(choice (const :tag "Now, if available and on a single day" now)
                 (const :tag "Beginning of the buffer" nil)))

(defcustom org-memento-timeline-hide-planning t
  "Whether to hide the planning section when in a block."
  :type 'boolean)

(defcustom org-memento-timeline-refresh-interval 180
  "Interval in seconds to refresh the timeline."
  :type '(choice (const nil) number))

(defcustom org-memento-timeline-hidden-sections nil
  "Alist of sections to hide by default."
  :type '(alist :key-type (symbol :tag "Type of the magit section")
                :value-type (boolean :tag "Whether to hide the section")))

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

(defface org-memento-timeline-estimated-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for items that consider future activities in calculation.")

(defface org-memento-timeline-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face for warning items.")

(defface org-memento-timeline-insufficient-face
  '((t (:inherit org-memento-timeline-warning-face)))
  "Face for insufficient amounts.")

(defface org-memento-timeline-above-limit-face
  '((t (:inherit org-memento-timeline-warning-face)))
  "Face for insufficient amounts.")

(defface org-memento-timeline-complete-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "LawnGreen")
    (((class color) (min-colors 88) (background light))
     :foreground "green4")
    (t (:inherit default :foreground "green3")))
  "Face for complete amounts.")

(defface org-memento-timeline-planned-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "LightGoldenrod3")
    (((class color) (min-colors 88) (background light))
     :foreground "DarkGoldenrod4")
    (t (:inherit font-lock-comment-face)))
  "Face for planned time values.")

;;;; Variables

(defvar org-memento-timeline-refresh-timer nil)

(defvar org-memento-timeline-slots nil
  "Empty time slots in the current span.")

(defvar org-memento-timeline-dismissed-items nil)

;;;; Macros

(defmacro org-memento-timeline--section-1 (type &rest body)
  "Insert a magit section without a value."
  (declare (indent 1))
  `(magit-insert-section (,type nil (alist-get ',type org-memento-timeline-hidden-sections))
     ,@body))

(defmacro org-memento-timeline-with-overlay (props &rest progn)
  `(let ((start (point)))
     (prog1 (progn ,@progn)
       (let ((ov (make-overlay start (point))))
         (pcase-dolist (`(,prop . ,value) ',props)
           (overlay-put ov prop (eval value)))))))

(defmacro org-memento-timeline-with-props (props &rest body)
  `(let ((start (point)))
     (prog1 (progn ,@body)
       (add-text-properties start (point) ,props))))

(defmacro org-memento-timeline-with-marker-point (&rest progn)
  `(when-let* ((section (magit-current-section))
               (marker (org-memento-timeline--org-marker section)))
     (save-current-buffer
       (org-with-point-at (org-memento-planning-item-hd-marker (oref section value))
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
  (with-current-buffer (get-buffer-create org-memento-timeline-buffer)
    (org-memento-timeline-mode)
    (setq-local org-memento-timeline-date-range (list start-day end-day)
                org-memento-timeline-span span
                revert-buffer-function #'org-memento-timeline-revert)
    (org-memento-timeline-revert)
    (pop-to-buffer (current-buffer))
    (add-hook 'org-memento-update-hook 'org-memento-timeline-refresh)
    (if (and (eq 'now org-memento-timeline-initial-position)
             (eq 'day org-memento-timeline-span))
        (org-memento-timeline-goto-now)
      (goto-char (point-min))))
  (when org-memento-timeline-refresh-timer
    (cancel-timer org-memento-timeline-refresh-timer))
  (when org-memento-timeline-refresh-interval
    (setq org-memento-timeline-refresh-timer
          (run-with-timer org-memento-timeline-refresh-interval t
                          #'org-memento-timeline-refresh-1))))

;;;###autoload
(defun org-memento-timeline-goto-now ()
  (org-memento-timeline--search-section
   (lambda (section)
     (pcase (oref section value)
       (`(,_ ,_ ,_ ,_ now)
        t)))))

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
      (org-memento--status)
      (setq org-memento-timeline-slots (org-memento--empty-slots taxy))
      (org-memento-policy-maybe-load))
    (let* ((section (magit-current-section))
           (type (when section (oref section type)))
           (value (when section (oref section value)))
           (head (if-let (values (magit-region-values))
                     (when (seq-every-p #'listp values)
                       (car (seq-sort-by #'car #'< values)))
                   value))
           (time (and (listp head)
                      (numberp (car head))
                      (car head)))
           (time2 (and (listp head)
                       (numberp (cadr head))
                       (cadr head)))
           (on-now (and (listp head)
                        (eq 'now (nth 4 head))))
           (toplevel (org-memento-timeline--toplevel-section))
           (toplevel-type (when toplevel (oref toplevel type)))
           (inhibit-read-only t))
      (delete-all-overlays)
      (erase-buffer)
      (run-hook-with-args 'org-memento-timeline-hook taxy)
      (or (when on-now
            (org-memento-timeline--search-section
             `(lambda (section)
                (and (listp (oref section value))
                     (eq 'now (nth 4 (oref section value)))))))
          (org-memento-timeline--search-section
           `(lambda (section)
              (and (and ',type
                        (eq ',type (oref section type)))
                   (or (equal ',value (oref section value))
                       (and ,time
                            (listp (oref section value))
                            (ignore-errors
                              (=< ,time (car (oref section value)))))
                       (and ,time2
                            (listp (oref section value))
                            (ignore-errors
                              (= ,time2 (cadr (oref section value)))))))))
          ;; As a fallback, go to the top-level section.
          (when toplevel-type
            (org-memento-timeline--search-section
             `(lambda (section)
                (eq (oref section type) ',toplevel-type))))
          (goto-char (point-min))))))

(defun org-memento-timeline--toplevel-section ()
  "Return the toplevel section."
  (when-let (section (magit-current-section))
    (while-let ((parent (oref section parent)))
      (setq section parent))
    section))

(defun org-memento-timeline-refresh ()
  (when-let (buffer (get-buffer org-memento-timeline-buffer))
    (when (get-buffer-window buffer 'all-frames)
      (with-current-buffer buffer
        (when (time-less-p (org-memento--current-time)
                           (org-memento-timeline--range-end))
          (org-memento-timeline-revert))))))

(defun org-memento-timeline-refresh-1 ()
  (when-let (buffer (get-buffer org-memento-timeline-buffer))
    (unless (or org-memento-block-idle-logging
                (org-clocking-p))
      (when (get-buffer-window buffer 'all-frames)
        (with-current-buffer buffer
          (when (time-less-p (org-memento--current-time)
                             (org-memento-timeline--range-end))
            (org-memento-timeline-revert)))))))

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
                             (concat (propertize "Clocked out"
                                                 'face 'font-lock-comment-face)
                                     (if end-time-of-block
                                         (format " (until %s)"
                                                 (format-time-string
                                                  "%R" end-time-of-block))
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
         (sum-durations (pred records)
           (cl-reduce #'+
                      (thread-last
                        records
                        (seq-filter pred)
                        (mapcar (lambda (record)
                                  (/ (- (cadr record)
                                        (car record))
                                     60))))
                      :initial-value 0))
         (block-record-p (record)
           (eq (nth 4 record) 'block))
         (insert-date (taxy)
           (magit-insert-section (date (taxy-name taxy))
             (let ((title (title taxy))
                   (indent (make-string 2 ?\s))
                   (day-unfinished (and (cadr (taxy-name taxy))
                                        (< now (cadr (taxy-name taxy))))))
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
                               'face 'font-lock-comment-face))
                 (when title
                   (thread-first
                     (format-spec (if day-unfinished
                                      " (%d focused, %f unfocused, %u untracked, \
%s scheduled, %a available)"
                                    " (%d focused, %f unfocused, %u untracked)")
                                  `((?d . ,(thread-last
                                             (taxy-taxys taxy)
                                             (mapcar #'taxy-name)
                                             (sum-durations
                                              (lambda (record)
                                                (and (block-record-p record)
                                                     (cadr record)
                                                     (< (cadr record) now))))
                                             (org-memento--format-duration)))
                                    (?f . ,(thread-last
                                             (taxy-taxys taxy)
                                             (mapcar #'taxy-name)
                                             (sum-durations
                                              (lambda (record)
                                                (eq (nth 4 record) 'anonymous)))
                                             (org-memento--format-duration)))
                                    (?u . ,(thread-last
                                             (taxy-taxys taxy)
                                             (mapcar #'taxy-name)
                                             (sum-durations
                                              (lambda (record)
                                                (and (null (nth 4 record))
                                                     (cadr record)
                                                     (< (cadr record) now))))
                                             (org-memento--format-duration)))
                                    (?s . ,(when day-unfinished
                                             (thread-last
                                               (taxy-taxys taxy)
                                               (mapcar #'taxy-name)
                                               (sum-durations
                                                (lambda (record)
                                                  (and (block-record-p record)
                                                       (car record)
                                                       (> (cadr record) now))))
                                               (org-memento--format-duration))))
                                    (?a . ,(when day-unfinished
                                             (thread-last
                                               (taxy-taxys taxy)
                                               (mapcar #'taxy-name)
                                               (sum-durations
                                                (lambda (record)
                                                  (and (null (nth 4 record))
                                                       (cadr record)
                                                       (> (cadr record) now))))
                                               (org-memento--format-duration))))))
                     (propertize
                      'face 'default))))
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

      (org-memento-timeline--section-1 timeline
        (dolist (taxy (taxy-taxys root-taxy))
          (insert-date taxy)))
      (goto-char (point-min)))))

;;;; Mode

(defvar org-memento-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'org-memento-timeline-add-from-suggestion)
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
  (when-let (section (magit-current-section))
    (if-let (marker (org-memento-timeline--org-marker section))
        (org-memento-timeline--display-entry marker #'pop-to-buffer)
      (when-let (marker (org-memento-timeline--group-marker section))
        (with-current-buffer (marker-buffer marker)
          (pop-to-buffer (current-buffer))
          (goto-char marker))))))

(defun org-memento-timeline-show-entry ()
  (interactive)
  (when-let (section (magit-current-section))
    (if-let (marker (org-memento-timeline--org-marker section))
        (org-memento-timeline--display-entry marker #'display-buffer)
      (when-let (marker (org-memento-timeline--group-marker section))
        (with-current-buffer (marker-buffer marker)
          (display-buffer (current-buffer))
          (goto-char marker))))))

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
            ((cl-type org-memento-order)
             (let ((title (org-memento-order-title value)))
               (when (yes-or-no-p (format "Dismiss \"%s\"?" title))
                 (push title org-memento-timeline-dismissed-items)))
             (org-memento-timeline-revert))
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
       (adjust-ts (marker &optional away)
         (save-current-buffer
           (org-with-point-at marker
             (org-memento-adjust-time :allow-edit-clock away))))
       (update-ts (marker new-start)
         (save-current-buffer
           (org-with-point-at marker
             (org-memento-adjust-time
              :new-start (+ new-start (* 60 org-memento-margin-minutes))))))
       (schedule-new-block (start end-bound)
         (pcase-exhaustive (org-memento--read-time-span
                            (org-memento--format-active-range
                             (if (and start (< start (float-time)))
                                 (+ (float-time) (* 60 org-memento-margin-minutes))
                               start)
                             end-bound)
                            start)
           (`(,modified-start ,end)
            (org-memento-schedule-block (float-time modified-start)
                                        (if end
                                            (float-time end)
                                          end-bound)
                                        :confirmed-time t
                                        :suggestions
                                        (org-memento-timeline-suggestions)))))
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
                (when (eq 'group-budgets (oref (magit-current-section) type))
                  (user-error "Not supported on this type of element"))
                (pcase (oref (magit-current-section) value)
                  (`nil
                   (user-error "No section at point"))
                  (`(,start ,end ,title ,marker ,type . ,_)
                   (cl-case type
                     (dismissed
                      ;; TODO:
                      (pcase-exhaustive (org-memento-timeline--find-slot
                                         title (when end
                                                 (/ (- end start) 60)))
                        (`(,slot-start ,_slot-end . ,_)
                         (update-ts marker slot-start)
                         t)))
                     (block
                      ;; Only allow adjusting time of future events.
                      (when (or (not start)
                                (> start now))
                        (adjust-ts marker)
                        t))
                     (away
                      (adjust-ts marker t)
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

(defun org-memento-timeline--org-marker (section)
  "Return an Org marker associated with a section."
  (unless (eq 'group-budgets (oref section type))
    (when-let (value (oref section value))
      (pcase value
        ((pred org-memento-planning-item-p)
         (org-memento-planning-item-hd-marker value))
        ((pred org-memento-block-p)
         (org-memento-block-hd-marker value))
        ((and `(,x . ,_)
              (guard (org-memento-block-p x)))
         (org-memento-block-hd-marker x))
        (`(,_ ,_ ,_ ,marker . ,_)
         marker)))))

(defun org-memento-timeline-group-at-point ()
  "Return the group of the section at point, if any."
  (org-memento-timeline--section-group (magit-current-section)))

(defun org-memento-timeline--section-group (section)
  (cl-flet
      ((section-type (section)
         (slot-value section 'type))
       (section-value (section)
         (slot-value section 'value)))
    (pcase section
      ((and (app section-type 'group-budgets)
            (app section-value `(,_ ,path . ,_)))
       path))))

(defun org-memento-timeline--group-marker (section)
  (when-let (path (org-memento-timeline--section-group section))
    (org-memento-policy-find-definition path)))

(defun org-memento-timeline-range ()
  "Return the range as a list of internal time representations."
  (list (org-memento-timeline--range-start)
        (org-memento-timeline--range-end)))

(defun org-memento-timeline--range-start ()
  (thread-first
    (parse-time-string (car org-memento-timeline-date-range))
    (org-memento--set-time-of-day (or org-extend-today-until 0) 0 0)
    (encode-time)))

(defun org-memento-timeline--range-end ()
  (thread-first
    (parse-time-string (cadr org-memento-timeline-date-range))
    (org-memento--set-time-of-day (or org-extend-today-until 0) 0 0)
    (decoded-time-add (make-decoded-time :hour 23 :minute 59))
    (encode-time)))

(defun org-memento-timeline--search-section (pred)
  "Move the point to a section that matches PRED.

If there is a match, the function returns the position of the
section."
  (let ((pos (point)))
    (when-let (new-pos (catch 'section
                         (save-excursion
                           (while (setq pos (next-single-property-change
                                             pos 'magit-section))
                             (goto-char pos)
                             (when-let (section (magit-current-section))
                               (when (funcall pred section)
                                 (throw 'section pos)))))))
      (goto-char new-pos))))

;;;; Extra hooks

(defun org-memento-timeline-overview-section (taxy)
  (unless (eq org-memento-timeline-span 'day)
    (magit-insert-section (overview)
      (magit-insert-heading
        "Overview")
      (cl-labels
          ((midnight-from-string (string)
             (org-memento--set-time-of-day (parse-time-string string)
                                           0 0 0))
           (taxy-match-date (string taxy)
             (and (eq 'date (nth 4 (taxy-name taxy)))
                  (equal string (nth 2 (taxy-name taxy)))))
           (find-date-taxy (string)
             (seq-find (apply-partially #'taxy-match-date string)
                       (taxy-taxys taxy)))
           (format-seconds (time future)
             (propertize (org-memento--format-duration (/ time 60))
                         'face
                         (if future
                             'org-memento-timeline-planned-face
                           'default)))
           (sum-durations (pred records)
             (cl-reduce #'+
                        (thread-last
                          records
                          (seq-filter pred)
                          (mapcar (lambda (record)
                                    (/ (- (cadr record)
                                          (car record))
                                       60))))
                        :initial-value 0)))
        (let* ((date (midnight-from-string (car org-memento-timeline-date-range)))
               (final-date (midnight-from-string (cadr org-memento-timeline-date-range)))
               (now (float-time (org-memento--current-time))))
          (insert (format "| %-14s | ChkIn | ChkOut| Total |Focusd| Idle |Untrkd|\n"
                          "Date"))
          (while (not (org-memento-date--le final-date date))
            (let* ((date-string (format-time-string "%F" (encode-time date)))
                   (date-taxy (find-date-taxy date-string))
                   (date-record (when date-taxy (taxy-name date-taxy)))
                   (plist (org-memento--normal-workhour date)))
              (when (or date-taxy plist)
                (let* ((midnight (float-time (encode-time date)))
                       (checkin-time (or (car date-record)
                                         (when-let (string (plist-get plist :normal-checkin))
                                           (+ midnight (* 60 (org-duration-to-minutes string))))))
                       (checkout-time (or (cadr date-record)
                                          (when-let (duration (plist-get plist :normal-duration))
                                            (+ checkin-time
                                               (* 60 (org-duration-to-minutes duration))))))
                       (duration-seconds (when (and checkin-time checkout-time)
                                           (- checkout-time checkin-time)))
                       (focused (when date-taxy
                                  (thread-last
                                    (taxy-taxys date-taxy)
                                    (mapcar #'taxy-name)
                                    (sum-durations
                                     (lambda (record)
                                       (and (eq (nth 4 record) 'block)
                                            (cadr record)
                                            (< (cadr record) now)))))))
                       (idle (when date-taxy
                               (thread-last
                                 (taxy-taxys date-taxy)
                                 (mapcar #'taxy-name)
                                 (sum-durations
                                  (lambda (record)
                                    (and (memq (nth 4 record) '(idle away))
                                         (cadr record)
                                         (< (cadr record) now)))))))
                       (untracked (when date-taxy
                                    (thread-last
                                      (taxy-taxys date-taxy)
                                      (mapcar #'taxy-name)
                                      (sum-durations
                                       (lambda (record)
                                         (and (null (nth 4 record))
                                              (cadr record)
                                              (< (cadr record) now))))))))
                  (insert (format "| %-14s | %5s | %5s | %5s | %4s | %4s | %4s |\n"
                                  (format-time-string "%F %a" (encode-time date))
                                  (if checkin-time
                                      (format-seconds (- checkin-time midnight)
                                                      (> checkin-time now))
                                    "")
                                  (if checkout-time
                                      (format-seconds (- checkout-time midnight)
                                                      (> checkout-time now))
                                    "")
                                  (if duration-seconds
                                      (format-seconds duration-seconds
                                                      (> checkout-time now))
                                    "")
                                  (if focused
                                      (org-memento--format-duration focused)
                                    "")
                                  (if idle
                                      (org-memento--format-duration idle)
                                    "")
                                  (if untracked
                                      (org-memento--format-duration untracked)
                                    ""))))))
            (setq date (decoded-time-add date (make-decoded-time :day 1))))))
      (insert ?\n))))

(defun org-memento-timeline-progress-section (taxy)
  (let ((rules (org-memento-policy-rules
                :span org-memento-timeline-span
                :start-date (car org-memento-timeline-date-range)
                :end-date (cadr org-memento-timeline-date-range)))
        (group-sums-for-span (org-memento-group-sums-1 taxy)))
    (cl-labels
        ((spentp (x)
           (and (org-memento-group-data-p x)
                (eq 'activity-sum (org-memento-group-data-tag x))))
         (plannedp (x)
           (and (org-memento-group-data-p x)
                (eq 'planned-sum (org-memento-group-data-tag x))))
         (budget-for-span-p (span x)
           (and (org-memento-policy-budget-rule-p x)
                (eq (slot-value x 'span) span)))
         (test-budget (span level x)
           (and (org-memento-policy-budget-rule-p x)
                (eq (slot-value x 'span) span)
                (eq (slot-value x 'level) level)))
         (budget (span level rules)
           (when-let (rule (seq-find (apply-partially #'test-budget span level) rules))
             (slot-value rule 'duration-minutes)))
         (format-budget (span level rules spent)
           (if-let (budget (budget span level rules))
               (propertize (org-memento--format-duration budget)
                           'face
                           (if (>= spent budget)
                               (if (eq level 'limit)
                                   'org-memento-timeline-above-limit-face
                                 'org-memento-timeline-complete-face)
                             'default))
             ""))
         (group-value (x)
           (org-memento-group-data-value x))
         (sum (nums)
           (cl-reduce #'+ nums :initial-value 0))
         (insert-group (span depth group-taxy)
           (let* ((spent (thread-last
                           (taxy-flatten group-taxy)
                           (seq-filter #'spentp)
                           (mapcar #'group-value)
                           (sum)))
                  (planned (thread-last
                             (taxy-flatten group-taxy)
                             (seq-filter #'plannedp)
                             (mapcar #'group-value)
                             (sum)))
                  (goal (budget span 'goal (taxy-items group-taxy)))
                  (demand (when goal
                            (- goal (+ spent planned)))))
             (magit-insert-section (group-budgets (list span
                                                        (taxy-name group-taxy)
                                                        demand))
               (magit-insert-heading
                 (make-string 2 ?\s)
                 (pcase span
                   (`day
                    (format "| %-16s | %4s%s%4s | %4s | %4s %-7s | %4s |"
                            (truncate-string-to-width
                             (concat (make-string depth ?\s)
                                     (propertize
                                      (org-memento--format-group-last-node
                                       (taxy-name group-taxy))
                                      'face 'magit-section-heading))
                             16)
                            (if (> spent 0)
                                (org-memento--format-duration spent)
                              "")
                            (if (> planned 0)
                                "+"
                              " ")
                            (if (> planned 0)
                                (org-memento--format-duration planned)
                              "")
                            (format-budget 'day 'minimum (taxy-items group-taxy) spent)
                            (format-budget 'day 'goal (taxy-items group-taxy) spent)
                            (cond
                             ((and demand (> demand 0))
                              (propertize (format "(-%s)" (org-memento--format-duration demand))
                                          'face 'org-memento-timeline-insufficient-face))
                             ((and spent goal (>= spent goal))
                              (propertize (format "(%.f%%)" (* 100 (/ spent goal)))
                                          'face 'org-memento-timeline-complete-face))
                             (t
                              ""))
                            (format-budget 'day 'limit (taxy-items group-taxy) spent)))))
               (dolist (subtaxy (taxy-taxys group-taxy))
                 (insert-group span (1+ depth) subtaxy))))))
      (org-memento-timeline--section-1 progress
        (magit-insert-heading "Progress")
        (when (eq 'day org-memento-timeline-span)
          (org-memento-timeline--section-1 daily-progress
            (magit-insert-heading
              (make-string 2 ?\s)
              "Daily")
            (insert (propertize
                     (concat
                      (make-string 2 ?\s)
                      (format "| %-16s | %-9s | %-26s |\n"
                              "Group" "Today" "Daily budget")
                      (make-string 2 ?\s)
                      (format "| %-16s | %-9s | %-4s | %-12s | %-4s |\n"
                              "" "Current" "Min" "Goal" "Lim"))
                     'face 'magit-section-heading))
            (dolist (group-taxy
                     (thread-last
                       rules
                       (seq-filter (apply-partially #'budget-for-span-p 'day))
                       (org-memento-policy-group-taxy)
                       (taxy-fill group-sums-for-span)
                       (taxy-fill (org-memento-group-planned-sums-1))
                       (taxy-taxys)))
              (insert-group 'day 0 group-taxy)))
          (insert ?\n)
          (org-memento-timeline--weekly-progress
           rules
           (append group-sums-for-span org-memento-weekly-group-sums))))
      (insert ?\n))))

(defun org-memento-timeline--weekly-progress (rules group-sums)
  (let* ((threshold (/ (org-memento--percentage-on-week) 100))
         (gauge-width (- (window-width) 45 1))
         (rule-pos (floor (* gauge-width threshold)))
         ;; This can be a constant, but it is tedious.
         (row-format "| %-16s |%6s /%6s | %s%4.f%%")
         (total-actual 0)
         (total-goal 0))
    (cl-labels
        ((test-budget (span level x)
           (and (org-memento-policy-budget-rule-p x)
                (eq (slot-value x 'span) span)
                (eq (slot-value x 'level) level)))
         (sum (nums)
           (cl-reduce #'+ nums :initial-value 0))
         (duration-from-taxy (taxy)
           (if-let (budget (car (taxy-items taxy)))
               (slot-value budget 'duration-minutes)
             (sum (mapcar #'duration-from-taxy (taxy-taxys taxy)))))
         (sort-taxys (taxys)
           (seq-sort-by #'duration-from-taxy #'> taxys))
         (insert-group (depth group-taxy)
           (magit-insert-section (group-budgets (list 'week (taxy-name group-taxy) nil))
             (when-let (budget (car (taxy-items group-taxy)))
               (let* ((context (slot-value budget 'context))
                      (title (slot-value context 'title))
                      (group-path (taxy-name group-taxy))
                      (goal (slot-value budget 'duration-minutes))
                      (sum (sum (mapcar #'org-memento-group-data-value
                                        (org-memento-filter-by-group-path
                                         group-path group-sums))))
                      (rate (/ sum goal))
                      (w1 (min (round (* gauge-width rate))
                               gauge-width)))
                 (org-memento-timeline-with-props
                  (list 'help-echo
                        (concat (if title
                                    (format "\"%s\" " title)
                                  "")
                                (org-memento--format-group group-path)))
                  (magit-insert-heading
                    (make-string 2 ?\s)
                    (format row-format
                            (propertize (truncate-string-to-width
                                         (concat (make-string depth ?\s)
                                                 (or title
                                                     (org-memento--format-group-last-node
                                                      group-path)))
                                         16)
                                        'face 'magit-section-heading)
                            (propertize (org-memento--format-duration sum)
                                        'face
                                        (cond
                                         ((< rate threshold)
                                          'org-memento-timeline-insufficient-face)
                                         ((>= rate 1)
                                          'org-memento-timeline-complete-face)
                                         (t
                                          'default)))
                            (org-memento--format-duration goal)
                            (with-temp-buffer
                              (insert (make-string w1 ?x)
                                      (make-string (max 0 (- gauge-width w1)) ?\s))
                              (goto-char rule-pos)
                              (delete-char 1)
                              (insert "|")
                              (buffer-string))
                            (* 100 rate)))
                  (when (= depth 0)
                    (cl-incf total-actual (min sum goal))
                    (cl-incf total-goal goal)))))
             (dolist (subtaxy (sort-taxys (taxy-taxys group-taxy)))
               (insert-group (1+ depth) subtaxy)))))
      (org-memento-timeline--section-1 weekly-progress
        (magit-insert-heading
          (make-string 2 ?\s)
          "Weekly goals")
        (dolist (group-taxy (thread-last
                              rules
                              (seq-filter (apply-partially #'test-budget 'week 'goal))
                              (org-memento-policy-group-taxy)
                              (org-memento-taxy-trees-with-item)
                              (sort-taxys)))
          (insert-group 0 group-taxy))
        (let* ((rate (/ total-actual total-goal))
               (w1 (round (* rate gauge-width))))
          (insert (make-string 2 ?\s)
                  (propertize (format row-format
                                      (propertize "Total" 'face 'magit-section-heading)
                                      (propertize (org-memento--format-duration total-actual)
                                                  'face
                                                  (if (> rate threshold)
                                                      'org-memento-timeline-complete-face
                                                    'org-memento-timeline-insufficient-face))
                                      (org-memento--format-duration total-goal)
                                      (with-temp-buffer
                                        (insert (make-string w1 ?o)
                                                (make-string (max 0 (- gauge-width w1)) ?\s))
                                        (goto-char rule-pos)
                                        (delete-char 1)
                                        (insert "|")
                                        (buffer-string))
                                      (* 100 rate))
                              'face '(:overline t))
                  "\n"))))))

(defun org-memento-timeline-suggestions-section (taxy)
  (cl-labels
      ((rule-group-path (rule)
         (thread-first
           (slot-value rule 'context)
           (slot-value 'group-path))))
    (org-memento-timeline--section-1 suggestions
      (magit-insert-heading "Suggestions")
      (pcase-dolist
          (`(,rule . ,plans)
           (thread-last
             (org-memento-yield-for-span taxy org-memento-timeline-span
               :start-date (car org-memento-timeline-date-range)
               :end-date (cadr org-memento-timeline-date-range))
             (seq-sort-by (-compose #'org-memento--format-group #'rule-group-path #'car)
                          #'string<)))
        (when plans
          (let ((group-path (rule-group-path rule))
                (now (float-time (org-memento--current-time))))
            (let ((tasks (car plans)))
              (dolist (task tasks)
                (unless (member (org-memento-order-title task)
                                org-memento-timeline-dismissed-items)
                  (magit-insert-section (generated-task task)
                    (magit-insert-heading
                      (make-string 2 ?\s)
                      (propertize (org-memento-order-title task)
                                  'face 'magit-section-heading)
                      (when-let (duration (org-memento-order-duration task))
                        (concat " " (org-memento--format-duration duration)))
                      (format " (%s)" (org-memento--format-group group-path))
                      (when-let (latest
                                 (caar (org-memento-order-previous-activities task)))
                        (concat " "
                                (propertize (org-memento--format-diff-days (- now latest))
                                            'face 'font-lock-comment-face))))))))))))
    (insert ?\n)))

(defun org-memento-timeline-suggestions ()
  "Return the suggestions held in the suggestions section."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (org-memento-timeline--search-section
              (lambda (section)
                (org-memento-order-p (oref section value))))
        (push (oref (magit-current-section) value)
              result))
      (nreverse result))))

(defun org-memento-timeline-add-from-suggestion ()
  (interactive)
  (cl-flet*
      ((get-time-range (title &optional duration)
         (when-let (slot (org-memento-timeline--find-slot title duration))
           (pcase-exhaustive (org-memento--read-time-span
                              (when slot
                                (org-memento--format-timestamp
                                 (+ (car slot) (* 60 org-memento-margin-minutes))
                                 (if duration
                                     (+ (car slot)
                                        (* 60 duration)
                                        (* 60 org-memento-margin-minutes))
                                   (cadr slot))))
                              (float-time (org-memento--current-time)))
             (`(,start ,end)
              (list (float-time start)
                    (when end
                      (float-time end)))))))
       (add-item (value &optional interactive)
         (cl-etypecase value
           (org-memento-order
            (pcase-let*
                ((title (or (org-memento-order-title value)
                            (org-memento-read-title nil
                              :default (org-memento-order-title value))))
                 (duration (org-memento-order-duration value))
                 (`(,start ,end) (when interactive
                                   (get-time-range title duration))))
              (org-memento-add-event :title title
                                     :start start
                                     :end (or end
                                              (when (and start duration)
                                                (+ start (* 60 duration))))
                                     :duration duration
                                     :interactive t
                                     :group (org-memento--default-group
                                             (org-memento-order-group value))
                                     :copy-from (org-memento-order-sample-marker value))))))
       (match-group (group-path x)
         (equal group-path
                (seq-take (org-memento-group-path x)
                          (length group-path))))
       (fallback (&optional group-path duration)
         (pcase-let*
             ((group (org-memento-read-group
                         "Select a group: " :group-path group-path))
              (context-taxy (org-memento-policy-find-context-by-group group))
              (title (when context-taxy
                       (slot-value (taxy-name context-taxy) 'title)))
              (`(,start ,end) (get-time-range (or title
                                                  (org-memento--format-group group-path))
                                              duration)))
           (org-memento-add-event :group group
                                  :title title
                                  :interactive t
                                  :duration duration
                                  :start start
                                  :end end)))
       (select-suggestion (&optional group-path duration)
         (if-let (suggestions (if group-path
                                  (seq-filter (apply-partially #'match-group group-path)
                                              (org-memento-timeline-suggestions))
                                (org-memento-timeline-suggestions)))
             (add-item (or (org-memento-select-order "Select a task to add: "
                                                     suggestions)
                           (user-error "Not selected")))
           (fallback group-path duration)))
       (section-type (section)
         (slot-value section 'type))
       (section-value (section)
         (slot-value section 'value)))
    (if-let (values (magit-region-values))
        (dolist (value values)
          (when (org-memento-order-p value)
            (add-item value)))
      (pcase (magit-current-section)
        ((and (app section-type `group-budgets)
              (app section-value `(,_span ,group-path ,remaining . ,_)))
         (select-suggestion group-path remaining))
        ((and (app section-value (cl-type org-memento-order))
              (app section-value order))
         (add-item order t))
        ((app section-value `nil)
         (fallback))
        (obj
         (if-let (group-path (ignore-errors
                               (org-memento-group-path obj)))
             (select-suggestion group-path)
           (fallback)))))))

(defun org-memento-timeline--find-slot (title &optional duration)
  (when-let (slots (if duration
                       (seq-filter `(lambda (slot)
                                      (>= (- (cadr slot)
                                             (car slot))
                                          ,(* 60
                                              (+ duration
                                                 (* 2 org-memento-margin-minutes)))))
                                   org-memento-timeline-slots)
                     org-memento-timeline-slots))
    (org-memento-select-slot
     (format "Choose a slot for \"%s\": " title)
     slots)))

(defun org-memento-timeline-planning-sections (taxy)
  (when (and (or (not org-memento-timeline-hide-planning)
                 (not org-memento-current-block))
             (eq org-memento-timeline-span 'day))
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
          (org-memento-timeline--section-1 planning-items
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
          (org-memento-timeline--section-1 behind-schedule
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
        (let* ((next-block (thread-last
                             (org-memento--blocks)
                             (seq-filter #'block-scheduled-future-p)
                             (seq-sort-by #'org-memento-starting-time #'<)
                             (car)))
               (event (org-memento--next-agenda-event
                       nil
                       (when next-block
                         (org-memento-starting-time next-block))))
               (the-event (or event next-block)))
          (org-memento-timeline--section-1 next-event
            (magit-insert-heading "Next Event")
            (if the-event
                (org-memento-timeline--insert-block 1
                  (or event next-block)
                  :suffix (format " (in %s)"
                                  (org-memento--format-duration
                                   (/ (- (org-memento-starting-time the-event)
                                         now)
                                      60))))
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
        (org-memento-timeline--section-1 unscheduled-blocks
          (magit-insert-heading "Blocks without time")
          (dolist (block blocks)
            (org-memento-timeline--insert-block 1 block :omit-time t)))
        (insert ?\n)))))

(cl-defun org-memento-timeline--insert-block (level block
                                                    &key omit-time type suffix)
  (declare (indent 1))
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
        (propertize title 'face 'magit-section-heading)
        suffix))))

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
               (magit-insert-section (block-feasibility (cons block effort-sum) 'hide)
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
          (org-memento-timeline--section-1 feasibility
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

(provide 'org-memento-timeline)
;;; org-memento-timeline.el ends here
