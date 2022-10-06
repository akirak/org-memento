;;; org-memento.el --- Time blocking with Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/org-memento

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

;; Org-Memento is an Org-based framework for time management.

;; It is intended be an alternative to timeclock.el and supports time blocking.

;;; Code:

(require 'org)

(defgroup org-memento nil
  "Time blocking with Org mode."
  :group 'org)

;;;; Custom variables

(defcustom org-memento-file (locate-user-emacs-file "memento.org")
  "File that keeps the journal."
  :type 'file)

(defcustom org-memento-idle-time 30
  "Duration in minutes until idle tasks are performed.

Also see `org-clock-idle-time'. If `org-clock-idle-time' is
non-nil, the value of `org-memento-idle-time' should be larger
than `org-clock-idle-time'."
  :type '(choice (number :tag "Duration in minutes")
                 (const nil)))

(defcustom org-memento-idle-heading "Idle"
  ""
  :type 'string)

(define-widget 'org-memento-category-spec-type 'lazy
  ""
  :tag "Category spec"
  :type '(plist :options
                (((const :tag "Template" :template)
                  (choice (string :tag "Literal string")
                          (list (const file)
                                (string :tag "File name"))
                          (function :tag "Function that takes no argument")))
                 ((const :tag "Default duration" :duration)
                  (string :tag "H:MM"))
                 ((const :tag "Days of week" :dows)
                  (set (const :tag "Sunday" 0)
                       (const :tag "Monday" 1)
                       (const :tag "Tuesday" 2)
                       (const :tag "Wednesday" 3)
                       (const :tag "Thursday" 4)
                       (const :tag "Friday" 5)
                       (const :tag "Saturday" 6)))
                 ((const :tag "Time of day" :time)
                  (choice (list :tag "Relative time from check in"
                                (const relative)
                                (string :tag "H:MM"))
                          (list :tag "Absolute"
                                (const absolute)
                                (string :tag "H:MM(-H:MM)"))))
                 ((const :tag "User-defined properties" :x)
                  (choice plist sexp)))))

(defcustom org-memento-category-alist
  nil
  ""
  :type '(alist :key-type (string :tag "Category name")
                :value-type org-memento-category-spec-type))

(defcustom org-memento-template-directory nil
  ""
  :type 'directory)

(defcustom org-memento-block-start-hook nil
  "Hook run after starting a block."
  :type 'hook)

(defcustom org-memento-post-block-exit-hook nil
  "Hook run after finishing or stopping a block."
  :type 'hook)

(defcustom org-memento-mode-line-format
  '(" [" org-memento-current-block "]")
  "Mode line format for `global-mode-string'."
  :type 'sexp)

;;;; Variables

(defvar org-memento-current-block nil
  "Headline of the current block.")

(defvar org-memento-current-category nil)

(defvar org-memento-current-time nil
  "When non-nil, use this as the current time for testing.")

(defvar org-memento-status-data nil
  "Store the daily entry and blocks.

Intended for internal use.")

(defvar org-memento-block-timer nil)

(defvar org-memento-daily-timer nil)

(defvar org-memento-idle-timer nil)

;;;; Structs

(cl-defstruct org-memento-block
  title closed checkin duration active-ts category)

(cl-defstruct org-memento-org-event
  marker start-time start-time-with-margin margin-secs)

;;;; Substs

(defsubst org-memento--current-time ()
  (or org-memento-current-time (current-time)))

;;;; Macros

(defmacro org-memento-with-today-entry (&rest progn)
  `(with-current-buffer (org-memento--buffer)
     (org-with-wide-buffer
      (org-memento--find-today)
      ,@progn)))

(defmacro org-memento-with-block-title (title &rest progn)
  (declare (indent 1))
  `(org-memento-with-today-entry
    (org-narrow-to-subtree)
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   ,title)
                           nil t)
        (progn
          ,@progn)
      (error "Failed to find a heading %s" ,title))))

(defmacro org-memento-with-current-block (&rest progn)
  (declare (indent 0))
  `(org-memento-with-block-title org-memento-current-block
     ,@progn))

;;;; Predicates on blocks

(defsubst org-memento-block-not-closed-p (block)
  (not (org-memento-block-closed block)))

;;;; Global mode

(define-minor-mode org-memento-mode
  "Global mode that handle idles."
  :lighter " OrgMemento"
  :global t
  (when org-memento-idle-timer
    (cancel-timer org-memento-idle-timer)
    (setq org-memento-idle-timer nil))
  (cond
   ((bound-and-true-p org-memento-mode)
    (add-to-list 'global-mode-string
                 '(org-memento-current-block
                   org-memento-mode-line-format)
                 t)
    (when org-memento-idle-time
      (setq org-memento-idle-timer
            (run-with-idle-timer (* 60 org-memento-idle-time)
                                 nil
                                 #'org-memento-idle)))
    (message "Org-Memento mode started."))
   (t
    (delete '(org-memento-current-block
              org-memento-mode-line-format)
            global-mode-string))))

(defun org-memento-idle ()
  (let ((time-user-left (time-subtract (org-memento--current-time)
                                       (* 60 org-memento-idle-time))))
    (org-memento-with-today-entry
     (org-narrow-to-subtree)
     (unless (re-search-forward (format org-complex-heading-regexp-format
                                        org-memento-idle-heading)
                                nil t)
       (goto-char (point-max))
       (insert "\n** " org-memento-idle-heading "\n"))
     (org-clock-in nil time-user-left)
     (add-hook 'pre-command-hook #'org-memento-unidle))))

(defun org-memento-unidle ()
  (remove-hook 'pre-command-hook #'org-memento-unidle)
  (org-clock-out))

;;;; Commands

;;;###autoload
(defun org-memento ()
  "Display the current block status or select the next block."
  (interactive)
  (org-memento-status)
  (if org-memento-current-block
      (progn
        (pcase (read-char-choice (concat (org-memento--format-block-status)
                                         "\n[f]inish, [s]top, or co[n]tinue? ")
                                 (string-to-list "fsn"))
          (?f
           (org-memento-finish-block)
           (org-memento))
          (?s
           (org-memento-stop-block)
           (org-memento)))
        (message nil))
    (call-interactively #'org-memento-start-block)))

;;;###autoload
(defun org-memento-start-block (title)
  "Start working on a time block you have planned."
  (interactive (list (progn
                       (org-memento-status 'check-in)
                       (completing-read "Start a block: "
                                        (org-memento-block-completion)
                                        nil t
                                        nil nil
                                        (when-let (block (org-memento-next-scheduled-block))
                                          (org-memento-block-title block))))))
  (org-memento-with-block-title title
    (org-memento--maybe-check-in))
  (setq org-memento-current-block title)
  (let* ((plist (org-memento-current-block-status))
         (remaining-secs (plist-get plist :remaining-secs)))
    (setq org-memento-current-category
          (or (org-memento-block-category (plist-get plist :block))
              (car (assoc title org-memento-category-alist))))
    (when (and remaining-secs (< remaining-secs 0))
      (error "Already timeout"))
    (org-memento--cancel-block-timer)
    (setq org-memento-block-timer
          (run-with-timer remaining-secs nil
                          #'org-memento-block-timeout))
    (org-memento-setup-daily-timer)
    (run-hooks 'org-memento-block-start-hook)))

(defun org-memento-finish-block ()
  "Mark the current block as done."
  (interactive)
  (when org-memento-current-block
    (org-memento-with-current-block
      (org-todo 'done))
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-post-block-exit-hook)))

(defun org-memento-stop-block ()
  "Stop the current block without marking it as done."
  (interactive)
  (when org-memento-current-block
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-post-block-exit-hook)))

;;;###autoload
(defun org-memento-open-today ()
  "Open the subtree of today.

This function opens the buffer of `org-memento-file', narrow the
buffer to the daily subtree, and checks in to the journal. If the
initial point is not inside the daily subtree, it also moves the
point to the heading.
"
  (interactive)
  (with-current-buffer (org-memento--buffer)
    (let ((initial-pos (point)))
      (widen)
      (org-memento--find-today)
      (org-memento--maybe-check-in)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (org-memento--maybe-generate-day)
      (unless (save-excursion
                (re-search-forward (rx bol "*" (+ blank)) initial-pos t))
        (goto-char initial-pos)))
    (pop-to-buffer (current-buffer))))

;;;; Timers and notifications

(defun org-memento-block-timeout ()
  (org-memento))

(defun org-memento--cancel-block-timer ()
  (when org-memento-block-timer
    (cancel-timer org-memento-block-timer)
    (setq org-memento-block-timer nil)))

(defun org-memento-setup-daily-timer ()
  (unless org-memento-daily-timer
    (let ((time (org-memento--end-of-day)))
      (setq org-memento-daily-timer
            (run-with-timer (- (float-time time)
                               (float-time (org-memento--current-time)))
                            nil #'org-memento-refresh-day)))))

(defun org-memento-refresh-day ()
  (when org-memento-current-block
    (org-memento-stop-block))
  (when org-memento-daily-timer
    (cancel-timer org-memento-daily-timer))
  (setq org-memento-daily-timer nil)
  ;; If the function is called at 23:59, the end of the day is not on the next
  ;; day, so wait for one minute until setting up the next timer.
  (run-with-timer 90 nil #'org-memento-setup-daily-timer))

;;;; Functions for working with the structure

(defun org-memento--buffer ()
  (or (find-buffer-visiting org-memento-file)
      (let ((auto-insert-mode nil))
        (find-file-noselect org-memento-file))))

(defun org-memento--today-string ()
  "Return the today's date in ISO-8601 format."
  (require 'org)
  (let ((decoded (decode-time (org-memento--current-time))))
    (format-time-string "%F"
                        (encode-time
                         (if (< (nth 2 decoded) org-extend-today-until)
                             (decoded-time-add decoded (make-decoded-time :day -1))
                           decoded)))))

(defun org-memento--find-today ()
  "Move the point to the today's entry or insert the entry."
  (let ((today (org-memento--today-string)))
    (or (re-search-backward (format org-complex-heading-regexp-format
                                    (regexp-quote today))
                            nil t)
        (catch 'found-today
          (goto-char (point-min))
          (while (re-search-forward org-complex-heading-regexp nil t)
            (let ((heading (match-string 4)))
              (cond
               ((equal today heading)
                (throw 'found-today t))
               ;; Past date
               ((time-less-p (encode-time
                              (org-memento--fill-decoded-time
                               (parse-time-string heading)))
                             (org-memento--current-time))
                (end-of-line 0)
                (insert "\n* " today "\n")
                (throw 'found-today nil)))))
          (insert "\n* " today "\n")))))

(defun org-memento--checkin-time ()
  "Return the check-in time of the entry as an internal time."
  (when-let (string (org-entry-get nil "memento_checkin_time"))
    (org-timestamp-to-time (org-timestamp-from-string string))))

;;;;; Updating properties

(defun org-memento-set-duration (duration)
  "Set the duration of the block at point."
  (interactive "sDuration (H:MM): ")
  (org-entry-put nil "memento_duration" duration))

(defun org-memento-set-category (category)
  "Set the category of the block at point."
  (interactive (list (completing-read "Category: "
                                      org-memento-category-alist
                                      nil nil nil nil
                                      (org-entry-get nil "memento_category"))))
  (org-entry-put nil "memento_category" category))

(defun org-memento--maybe-check-in ()
  (unless (org-entry-get nil "memento_checkin_time")
    (org-entry-put nil "memento_checkin_time" (org-memento--inactive-ts-string
                                               (org-memento--current-time)))))

(defun org-memento--set-closing-time ()
  (let* ((value (org-entry-get nil "memento_closing_time"))
         (new-value (with-temp-buffer
                      (when value
                        (insert value))
                      (goto-char (point-min))
                      (org-time-stamp t)
                      (buffer-string))))
    (org-entry-put nil "memento_closing_time" new-value)))

;;;;; Scanning

(defun org-memento-status (&optional check-in)
  "Update the status. Interactively, print the status."
  (interactive)
  (setq org-memento-status-data (org-memento--block-data
                                 (or check-in
                                     (called-interactively-p))))
  (when (called-interactively-p)
    (if org-memento-current-block
        (message (org-memento--format-block-status))
      (if-let (event (org-memento--next-agenda-event))
          (message (org-memento--format-org-event-status event))
        (message "No scheduled event remaining on today")))))

(defun org-memento--block-data (&optional check-in)
  ;; The first item will always be the day itself.
  (org-memento-with-today-entry
   (when check-in
     (org-memento--maybe-check-in))
   (org-narrow-to-subtree)
   (org-map-entries #'org-memento-block-entry
                    nil nil
                    (lambda ()
                      (when (and (looking-at org-complex-heading-regexp)
                                 (or (< 2 (length (match-string 1)))
                                     (equal (match-string 4)
                                            org-memento-idle-heading)))
                        (re-search-forward (rx bol "** ")))))))

(defun org-memento-block-entry ()
  "Return information on the block at point."
  (org-back-to-heading)
  (let* ((headline (org-element-headline-parser))
         (active-ts (when (re-search-forward org-ts-regexp
                                             (org-entry-end-position)
                                             t)
                      (org-timestamp-from-string (match-string 0)))))
    (make-org-memento-block
     :title (org-element-property :raw-value headline)
     :closed (org-element-property :closed headline)
     :checkin (when-let (str (org-element-property :MEMENTO_CHECKIN_TIME headline))
                (org-timestamp-from-string str))
     :duration (org-element-property :MEMENTO_DURATION headline)
     :active-ts active-ts
     :category (org-element-property :MEMENTO_CATEGORY headline))))

;;;;; Selecting blocks or finding a block

;; To use these functions, first you have to call `org-memento-status' to update
;; the data.

(defun org-memento--blocks ()
  "Return blocks on today."
  (cdr org-memento-status-data))

(defun org-memento-next-scheduled-block ()
  "Return the blocked scheduled at an earliest time."
  (unless org-memento-current-block
    (thread-last
      (org-memento--blocks)
      (seq-filter #'org-memento-block-not-closed-p)
      (org-memento--sort-blocks-by-ts)
      (car))))

(defun org-memento--open-blocks ()
  (thread-last
    (org-memento--blocks)
    (seq-filter #'org-memento-block-checkin)
    (seq-filter #'org-memento-block-not-closed-p)))

(defun org-memento--current-block ()
  (when org-memento-current-block
    (thread-last
      (org-memento--blocks)
      (seq-find (lambda (block)
                  (equal (org-memento-block-title block)
                         org-memento-current-block))))))

(defun org-memento--free-blocks ()
  (thread-last
    (org-memento--blocks)
    (seq-filter (lambda (x)
                  (and (org-memento-block-not-closed-p x)
                       (or (org-memento-block-checkin x)
                           (not (org-memento-block-active-ts x))))))))

(defun org-memento--sort-blocks-by-ts (block-plists)
  "Sort blocks by active timestamps."
  (cl-sort block-plists #'time-less-p
           :key (lambda (block)
                  (when-let (ts (org-memento-block-active-ts block))
                    (org-timestamp-to-time ts)))))

;;;; Generating blocks

(defun org-memento--maybe-generate-day ()
  "If the daily entry has no blocks, generate blocks.

This function insert blocks into the current entry according to
definitions in `org-memento-category-alist'.

The buffer must be narrowed to the day, and the point must be on
the daily entry."
  (catch 'existing
    (save-excursion
      (while (re-search-forward org-complex-heading-regexp nil t)
        (when (and (equal (match-string 1) "**")
                   (not (equal (match-string 4) org-memento-idle-heading)))
          (throw 'existing t))))
    (if-let (checkin-time (org-memento--checkin-time))
        (org-memento--insert-blocks
         (org-memento--generated-categories checkin-time))
      (user-error "First check in to the entry."))))

(defun org-memento--insert-blocks (plists)
  "Insert blocks into the current entry.

Use `org-memento--generated-categories' to generate PLISTS.

The buffer must be narrowed to the day, and the point must be on
the daily entry."
  (save-excursion
    (goto-char (org-entry-end-position))
    (dolist (plist plists)
      (unless (bolp)
        (insert "\n"))
      (insert "** " (plist-get plist :name) "\n")
      (beginning-of-line 0)
      (when-let (duration (plist-get plist :duration))
        (org-entry-put nil "memento_duration" duration))
      (org-end-of-meta-data t)
      (let ((start-time (plist-get plist :start-time))
            (end-time (plist-get plist :end-time)))
        (when start-time
          (insert (format (org-format-time-string "<%Y-%m-%d %a %%s%%s>" start-time)
                          (org-format-time-string "%H:%M" start-time)
                          (if end-time
                              (org-format-time-string "-%H:%M" end-time)
                            ""))
                  "\n")))
      (when-let (template (plist-get plist :template))
        (pcase template
          (`(file ,filename)
           (insert-file-contents (expand-file-name filename org-memento-template-directory)))
          ((pred functionp)
           (insert (funcall template)))
          ((pred stringp)
           (insert template)))
        (unless (eolp)
          (insert "\n"))))))

(defun org-memento--generated-categories (time)
  "Return plists of categories for the day starting at TIME."
  (let* ((decoded-time (decode-time time))
         (day-start-decoded (org-memento--start-of-day decoded-time))
         (this-dow (nth 6 day-start-decoded)))
    (thread-last
      org-memento-category-alist
      (seq-filter `(lambda (cell)
                     (memq ,this-dow
                           (plist-get (cdr cell) :dows))))
      (mapcar `(lambda (cell)
                 (let ((plist (cdr cell)))
                   (append (pcase (plist-get plist :time)
                             (`(relative ,string)
                              (let ((start-time (time-add ',time
                                                          (* 60 (org-duration-to-minutes
                                                                 string)))))
                                (list (float-time start-time)
                                      :start-time start-time)))
                             (`(absolute ,string)
                              (let* ((range (org-memento--parse-time-range string))
                                     (start-time-minutes (car range))
                                     (start-time (encode-time
                                                  (org-memento--set-time-of-day
                                                   ',day-start-decoded
                                                   (floor (/ start-time-minutes 60))
                                                   (mod start-time-minutes 60)
                                                   0)))
                                     (end-time-minutes (cdr range))
                                     (end-time (when end-time-minutes
                                                 (encode-time
                                                  (org-memento--set-time-of-day
                                                   ',day-start-decoded
                                                   (floor (/ end-time-minutes 60))
                                                   (mod end-time-minutes 60)
                                                   0)))))
                                (list (float-time start-time)
                                      :start-time start-time
                                      :end-time end-time)))
                             (_
                              (list nil)))
                           (list :name (car cell))
                           plist))))
      (org-memento--sort-by-car)
      (mapcar #'cdr))))

(defun org-memento--sort-by-car (alist)
  (cl-sort alist (lambda (a b)
                   (if (and a b)
                       (< a b)
                     a))
           :key #'car))

;;;; Formatting status

(defun org-memento--format-block-status ()
  (let ((status-plist (org-memento-current-block-status)))
    (format "You are currently working on \"%s\".\n%s%s"
            org-memento-current-block
            (cond
             ((plist-get status-plist :remaining-secs)
              (format "%d minutes remaining. "
                      (round (/ (plist-get status-plist :remaining-secs) 60))))
             ((plist-get status-plist :must-quit)
              (let ((next-event (plist-get status-plist :next-event)))
                (format "You must quit right now. "
                        (save-current-buffer
                          (org-with-point-at (org-memento-org-event-marker next-event)
                            (org-get-heading nil nil nil nil)))
                        (format-time-string "%R" (org-memento-org-event-start-time next-event)))))
             ((plist-get status-plist :timeout-secs)
              (format "Time out by %d minutes. "
                      (round (/ (plist-get status-plist :timeout) 60)))))
            (if-let (next-event (plist-get status-plist :next-event))
                (org-memento--format-org-event-status next-event)
              ""))))

(defun org-memento--format-org-event-status (event)
  (format "\"%s\" starts at %s."
          (save-current-buffer
            (org-with-point-at (org-memento-org-event-marker event)
              (org-get-heading nil nil nil nil)))
          (format-time-string "%R" (org-memento-org-event-start-time event))))

;;;; Completion

(defvar org-memento-block-cache nil)

(defun org-memento-block-completion ()
  (let ((items (thread-last
                 (org-memento--blocks)
                 (seq-filter #'org-memento-block-not-closed-p))))
    (if org-memento-block-cache
        (clrhash org-memento-block-cache)
      (setq org-memento-block-cache
            (make-hash-table :test #'equal :size (length items))))
    (dolist (block items)
      (puthash (org-memento-block-title block) block org-memento-block-cache))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . org-memento-block)
                         (annotation-function . org-memento-block-annotator)))
         (complete-with-action action ',(mapcar #'org-memento-block-title items)
                               string pred)))))

(defun org-memento-block-annotator (title)
  (if-let (block (gethash title org-memento-block-cache))
      (concat (when-let* ((ts (org-memento-block-active-ts block))
                          (time (org-timestamp-to-time ts)))
                (propertize (format " %s, in %d minutes"
                                    (format-time-string "%R" time)
                                    (floor (/ (float-time
                                               (time-subtract
                                                time (org-memento--current-time)))
                                              60)))
                            'face 'font-lock-warning-face))
              (when-let (duration (org-memento-block-duration block))
                (propertize (format " (%s)" duration)
                            'face 'font-lock-doc-face))
              (when-let (checkin (org-memento-block-checkin block))
                (propertize (format-time-string " already checked in at %R"
                                                (org-timestamp-to-time checkin))
                            'face 'font-lock-comment-face)))
    ""))

;;;; Retrieving timing information

(defun org-memento-current-block-status ()
  "Return information on the current block."
  (when org-memento-current-block
    (let* ((block (org-memento-with-current-block
                    (org-memento-block-entry)))
           (end-time-1 (org-memento--expected-end-time block))
           (next-event (org-memento--next-agenda-event end-time-1))
           (next-event-time (when next-event
                              (org-memento-org-event-start-time-with-margin next-event)))
           (end-time (floor (float-time end-time-1)))
           (now (floor (float-time (org-memento--current-time))))
           (timeout (when (> now end-time) (- now end-time)))
           (must-quit (when next-event (< next-event-time now))))
      (list :timeout-secs timeout
            :must-quit must-quit
            :remaining-secs (unless (or must-quit timeout)
                              (- (org-memento--time-min next-event-time end-time)
                                 now))
            :next-event next-event
            :block block))))

(defun org-memento--calculated-end-time (block)
  "Return an end time calculated from the duration."
  (when-let ((checkin (org-memento-block-checkin block))
             (duration (org-memento-block-duration block)))
    (time-add (org-timestamp-to-time checkin)
              (* 60 (org-duration-to-minutes duration)))))

(defun org-memento--designated-end-time (block)
  "Return the end time specified in the active timestamp, if any."
  (when-let* ((ts (org-memento-block-active-ts block))
              (end-ts (org-timestamp-split-range ts 'end)))
    (org-timestamp-to-time end-ts)))

(defun org-memento--expected-end-time (block)
  "Return the end time of BLOCK."
  (or (org-memento--designated-end-time block)
      (org-memento--calculated-end-time block)))

(defun org-memento--next-agenda-event (&optional bound-time)
  "Return an Org entry that has the earliest time stamp.

If BOUND-TIME is an internal time, time stamps later than the
time are skipped.

This returns an internal time of the time stamp minus a margin, a
marker to the time stamp, and the margin in seconds."
  (let* ((now (org-memento--current-time))
         (ts-regexp (org-memento--make-ts-regexp
                     now
                     (or bound-time
                         (thread-first
                           (decode-time (org-memento--current-time))
                           (org-memento--start-of-day)
                           (decoded-time-add (make-decoded-time :day 1 :minute -1))
                           (encode-time)))))
         (min-time (when bound-time
                     (float-time bound-time)))
         result)
    (dolist (file (org-agenda-files))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward ts-regexp nil t)
           (when-let* ((ts (org-timestamp-from-string (match-string 0)))
                       (time (when (org-element-property :hour-start ts)
                               (float-time (org-timestamp-to-time ts))))
                       ;; The default margin is 10 minutes. It would be better
                       ;; if we had a different margin depending on the
                       ;; task/event type.
                       (margin 600)
                       (mtime (- time margin)))
             (when (or (not min-time)
                       (< mtime min-time))
               (setq min-time mtime)
               (setq result (make-org-memento-org-event
                             :marker (point-marker)
                             :start-time time
                             :start-time-with-margin mtime
                             :margin-secs margin))))))))
    result))

;;;; Utility functions for time representations and Org timestamps

(defsubst org-memento--set-time-of-day (decoded-time hour minute sec)
  "Set the time of day of a decoded time.

Return a copy of the list."
  (append (list sec minute hour)
          (seq-drop decoded-time 3)))

(defun org-memento--fill-decoded-time (decoded-time)
  "Fill time fields of DECODED-TIME."
  (dolist (i (number-sequence 0 2))
    (unless (nth i decoded-time)
      (setf (nth i decoded-time) 0)))
  decoded-time)

(defun org-memento--inactive-ts-string (time)
  "Return a string for an inactive timestamp at TIME."
  (org-format-time-string (org-time-stamp-format t t) time))

(defun org-memento--parse-time-range (string)
  "Return a cons cell of minutes from a string time spec."
  (when (string-match (rx bol (group (+ digit) ":" (+ digit))
                          (?  "-" (group (group (+ digit) ":" (+ digit))))
                          eol)
                      string)
    (cons (floor (org-duration-to-minutes (match-string 1 string)))
          (when (> (length (match-data)) 4)
            (floor (org-duration-to-minutes (match-string 2 string)))))))

(defun org-memento--start-of-day (decoded-time)
  "Return the start of the day given as DECODED-TIME.

This respects `org-extend-today-until'."
  (org-memento--set-time-of-day (if (and org-extend-today-until
                                         (< (nth 2 decoded-time) org-extend-today-until))
                                    (decoded-time-add decoded-time (make-decoded-time :day -1))
                                  decoded-time)
                                (or org-extend-today-until 0)
                                0
                                0))

(defun org-memento--make-ts-regexp (from to)
  "Return a regexp that matches active timestamps in a range.

FROM and TO must be internal time representations. The regexp
matches long active timestamps. It is intended for fast timestamp
scanning, and it can produce false positives. You should perform
further checks against your desired time range."
  (let ((date-strs (thread-last
                     (number-sequence (float-time from) (float-time to) (* 3600 24))
                     (mapcar (lambda (float)
                               (format-time-string "%F" float))))))
    (format "<%s>" (rx-to-string `(and (or ,@date-strs) blank (+? anything))))))

(defun org-memento--time-min (time1 time2)
  "Return an earlier time of the two.

Both TIME1 and TIME2 must be an internal time representation or
nil. If one of them is nil, the other one is returned."
  (if (and time1 time2)
      (if (time-less-p time1 time2)
          time1
        time2)
    (or time1 time2)))

(provide 'org-memento)
;;; org-memento.el ends here
