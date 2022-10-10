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
(require 'org-element)

(declare-function org-element-headline-parser "org-element")
(declare-function org-day-of-week "org-clock")
(defvar org-capture-entry)

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

(define-widget 'org-memento-days-of-week-type 'lazy
  ""
  :tag "Days of week"
  :type '(set (const :tag "Sunday" 0)
              (const :tag "Monday" 1)
              (const :tag "Tuesday" 2)
              (const :tag "Wednesday" 3)
              (const :tag "Thursday" 4)
              (const :tag "Friday" 5)
              (const :tag "Saturday" 6)))

(defcustom org-memento-template-sources
  '((file+olp org-memento-file "Notes" "Templates"))
  "Location where you define your templates for time blocks.

For simplicity, only a single file is allowed. If you want to
define categories in multiple files, you should implement it by
temporarily setting this variable."
  :type '(repeat (list (const file+olp)
                       (choice (file :tag "File name")
                               (symbol :tag "Variable to a file name"))
                       (repeat :tag "Outline path" :inline t string))))

(defcustom org-memento-block-start-hook nil
  "Hook run after starting a block."
  :type 'hook)

(defcustom org-memento-block-exit-hook nil
  "Hook run after finishing or stopping a block."
  :type 'hook)

(defcustom org-memento-checkin-hook nil
  "Hook run after checking in to a daily entry.

If you run `org-memento-open-today' or one of the other commands
that perform daily check-in, this hook is called after
scaffolding is done before the user gets interactivity. The hook
is first called at the body of the entry, so you can use it to
insert some contents into the daily entry.

Note that this hook is not called on blocks inside a daily entry."
  :type 'hook)

(defcustom org-memento-checkout-hook nil
  "Hook run when `org-memento-checkout-from-day' command is run."
  :type 'hook)

(defcustom org-memento-workhour-alist
  '(((1 2 3 4 5)
     :normal-checkin "9:30"
     :normal-duration "8:00"))
  ""
  :type '(alist :key-type org-memento-days-of-week-type
                :value-type (plist :options
                                   (((const :normal-checkin)
                                     string)
                                    ((const :normal-duration)
                                     string)))))

(defcustom org-memento-schedule-away-alist nil
  ""
  :type '(alist :key-type string
                :value-type plist))

(defcustom org-memento-frame-title-format
  '((t
     org-memento-current-block)
    (org-memento-current-category
     (" (" org-memento-current-category ")"))
    (t
     org-memento-title-string)
    " >")
  ""
  :type 'sexp)

;;;; Variables

(defvar org-memento-status-data nil)

(defvar org-memento-current-block nil
  "Headline of the current block.")

(defvar org-memento-current-category nil)

(defvar org-memento-current-time nil
  "When non-nil, use this as the current time for testing.")

(defvar org-memento-next-event nil)

(defvar org-memento-block-timer nil)

(defvar org-memento-daily-timer nil)

(defvar org-memento-idle-timer nil)

(defvar org-memento-block-idle-logging nil
  "Prevent from idle logging till next check-in.")

(defvar org-memento-title-string nil)

(defvar org-memento-template-cache nil
  "Hash table that stores available templates.")

;;;; Substs

(defsubst org-memento--current-time ()
  (or org-memento-current-time (current-time)))

(defsubst org-memento-minutes-from-now (float-time)
  (round (* (- float-time (float-time (org-memento--current-time)))
            60)))

(defsubst org-memento--set-time-of-day (decoded-time hour minute sec)
  "Set the time of day of a decoded time.

Return a copy of the list."
  (append (list sec minute hour)
          (cdddr decoded-time)))

;;;; Generics and structs

(cl-defgeneric org-memento-headline-element (x)
  "Return the headline element of X.")

(cl-defgeneric org-memento-active-ts (x)
  "Return an active timestamp of X, if any.")

(cl-defgeneric org-memento-title (x)
  "Return the headline text of X."
  (org-element-property :raw-value (org-memento-headline-element x)))

(cl-defgeneric org-memento-started-time (x)
  "Return the actual start time of X in float.")

(cl-defgeneric org-memento-starting-time (x)
  "Return the expected start time of X in float.")

(cl-defgeneric org-memento-ended-time (x)
  "Return the actual start time of X in float."
  (when-let (ts (org-element-property :closed (org-memento-headline-element x)))
    (float-time (org-timestamp-to-time ts))))

(cl-defgeneric org-memento-ending-time (x)
  "Return the expected end time of X in float.")

(cl-defgeneric org-memento-ending-time-default (x)
  "Return the expected end time of X in float."
  (or (org-memento-ending-time x)
      (+ (org-memento-starting-time x) (* 30 60))))

(cl-defgeneric org-memento-duration (x)
  "Return the expected duration in minutes of X."
  (when-let (effort (org-element-property :EFFORT (org-memento-headline-element x)))
    (org-duration-to-minutes effort)))

;;;;; org-memento-block

(cl-defstruct org-memento-block
  "Object representing a day or a block in a journal file."
  headline active-ts hd-marker)

(cl-defmethod org-memento-headline-element ((x org-memento-block))
  (org-memento-block-headline x))

(cl-defmethod org-memento-active-ts ((x org-memento-block))
  (org-memento-block-active-ts x))

(defun org-memento-block-category (x)
  (org-element-property :MEMENTO_CATEGORY (org-memento-headline-element x)))

(cl-defmethod org-memento-started-time ((x org-memento-block))
  (when-let (str (org-element-property :MEMENTO_CHECKIN_TIME (org-memento-headline-element x)))
    (float-time (org-timestamp-to-time (org-timestamp-from-string str)))))

(cl-defmethod org-memento-starting-time ((x org-memento-block))
  (when-let (ts (org-memento-active-ts x))
    (float-time (org-timestamp-to-time ts))))

(cl-defmethod org-memento-ending-time ((x org-memento-block))
  (if-let (ts (org-memento-block-active-ts x))
      (let ((end-time (org-timestamp-to-time ts 'end)))
        (unless (time-equal-p (org-timestamp-to-time ts) end-time)
          (float-time end-time)))
    (when-let* ((duration (org-memento-duration x))
                (start (or (org-memento-started-time x)
                           (org-memento-starting-time x))))
      (+ start (* 60 duration)))))

;;;;; org-memento-org-event

(cl-defstruct org-memento-org-event
  "Object representing an Org entry with an active timestamp."
  marker active-ts ending-time got-ending-time margin-secs)

(cl-defmethod org-memento-active-ts ((x org-memento-org-event))
  (org-memento-org-event-active-ts x))

(cl-defmethod org-memento-starting-time ((x org-memento-org-event))
  (let ((time (float-time (org-timestamp-to-time (org-memento-active-ts x)))))
    (if-let (margin (org-memento-org-event-margin-secs x))
        (- time margin)
      time)))

(cl-defmethod org-memento-duration ((x org-memento-org-event))
  (when-let (effort (save-current-buffer
                      (org-with-point-at (org-memento-org-event-marker x)
                        (org-entry-get nil "EFFORT"))))
    (org-duration-to-minutes effort)))

(cl-defmethod org-memento-ending-time ((x org-memento-org-event))
  (if (org-memento-org-event-got-ending-time x)
      (org-memento-org-event-ending-time x)
    (let* ((ts (org-memento-active-ts x))
           (starting-time (float-time (org-timestamp-to-time ts)))
           (end-ts (org-timestamp-split-range ts 'end)))
      (prog1 (setf (org-memento-org-event-ending-time x)
                   (if (not (eq (org-element-property :hour-start ts)
                                (org-element-property :hour-start end-ts)))
                       (float-time (org-timestamp-to-time end-ts))
                     (when-let (duration (org-memento-duration x))
                       (+ starting-time (* 60 duration)))))
        (setf (org-memento-org-event-got-ending-time x) t)))))

(cl-defmethod org-memento-ended-time ((x org-memento-org-event))
  (when-let (ts (save-current-buffer
                  (org-with-point-at (org-memento-org-event-marker x)
                    (org-back-to-heading)
                    (save-match-data
                      (when (re-search-forward org-closed-time-regexp
                                               (org-entry-end-position)
                                               t)
                        (org-timestamp-from-string (match-string 1)))))))
    (float-time (org-timestamp-to-time ts))))

;;;;; org-memento-template

(cl-defstruct org-memento-template
  file olp title category tags normal-hour normal-dows duration leaf-p
  relative-olp)

(cl-defmethod org-memento-title ((x org-memento-template))
  (org-memento-template-title x))

(cl-defmethod org-memento-duration ((x org-memento-template))
  (org-memento-template-duration x))

(cl-defstruct org-memento-twc
  "Template with scheduling context.

The template should be a `org-memento-template'.

The context can be `org-memento-block' or another type that
implements methods such as `org-memento-started-time'."
  template context
  ;; These fields hold computed values.
  starting-time ending-time)

(cl-defmethod org-memento-title ((x org-memento-twc))
  (org-memento-title (org-memento-twc-template x)))

(cl-defmethod org-memento-duration ((x org-memento-twc))
  (org-memento-duration (org-memento-twc-template x)))

(cl-defmethod org-memento-starting-time ((x org-memento-twc))
  (org-memento-twc-starting-time x))

(cl-defmethod org-memento-ending-time ((x org-memento-twc))
  (org-memento-twc-ending-time x))

(defun org-memento--init-twc (context template)
  (let* ((checkin-time (org-memento-started-time context))
         (decoded-checkin-time (decode-time checkin-time))
         starting-time
         ending-time)
    (pcase (org-memento-template-normal-hour template)
      (`(relative ,start ,end)
       (setq starting-time (+ checkin-time (* start 60))
             ending-time (when end
                           (+ checkin-time (* end 60)))))
      (`(absolute ,start ,end)
       (let ((midnight (thread-first
                         (org-memento--start-of-day decoded-checkin-time)
                         (org-memento--set-time-of-day 0 0 0)
                         (encode-time)
                         (float-time))))
         (setq starting-time (+ midnight (* start 60))
               ending-time (when end
                             (+ midnight (* end 60)))))))
    (when (and starting-time
               (not ending-time)
               (org-memento-duration template))
      (setq ending-time (+ starting-time
                           (* (org-memento-duration template) 60))))
    (make-org-memento-twc
     :template template
     :context context
     :starting-time starting-time
     :ending-time ending-time)))

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
  (not (org-memento-ended-time block)))

;;;; Global mode

;;;###autoload
(define-minor-mode org-memento-mode
  "Global mode that handle idles."
  :lighter " OrgMemento"
  :global t
  (when org-memento-idle-timer
    (cancel-timer org-memento-idle-timer)
    (setq org-memento-idle-timer nil))
  (when (bound-and-true-p org-memento-mode)
    (when org-memento-idle-time
      (setq org-memento-idle-timer
            (run-with-idle-timer (* 60 org-memento-idle-time)
                                 nil
                                 #'org-memento-idle)))
    (message "Org-Memento mode started.")))

(defun org-memento-idle ()
  (unless org-memento-block-idle-logging
    (let ((time-user-left (time-subtract (org-memento--current-time)
                                         (* 60 org-memento-idle-time))))
      (org-memento-with-today-entry
       (org-memento--find-or-create-idle-heading)
       (org-clock-in nil time-user-left)
       (add-hook 'pre-command-hook #'org-memento-unidle)))))

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
                                          (org-memento-title block))))))
  (org-memento-with-block-title title
    (when (org-memento--maybe-check-in)
      (org-memento--save-buffer)))
  (setq org-memento-current-block title)
  ;; This should be moved to `org-memento-status'.
  (let* ((block (org-memento-with-current-block
                  (org-memento-block-entry)))
         (ending-time (org-memento-ending-time block))
         (next-event (org-memento--next-agenda-event
                      (org-memento-block-hd-marker block)))
         (ending-time-2 (cond
                         ((and next-event ending-time)
                          (min ending-time (org-memento-ending-time-default next-event)))
                         (next-event
                          (org-memento-ending-time-default next-event))
                         (t
                          ending-time))))
    (setq org-memento-current-category
          (org-memento-block-category block)
          org-memento-next-event next-event)
    (setq org-memento-title-string (when ending-time-2
                                     (format-time-string " (until %R)" ending-time-2)))
    (org-memento--cancel-block-timer)
    (setq org-memento-block-timer
          (when ending-time-2
            (run-with-timer (- ending-time-2 (float-time))
                            nil
                            #'org-memento-block-timeout)))
    (org-memento-setup-daily-timer)
    (run-hooks 'org-memento-block-start-hook)))

(defun org-memento-finish-block ()
  "Mark the current block as done."
  (interactive)
  (when org-memento-current-block
    (org-memento-with-current-block
      (org-todo 'done)
      (org-memento--save-buffer))
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-block-exit-hook)))

(defun org-memento-stop-block ()
  "Stop the current block without marking it as done."
  (interactive)
  (when org-memento-current-block
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-block-exit-hook)))

;;;###autoload
(defun org-memento-log (start end)
  "Log a past time block to the today's entry."
  (interactive (org-memento--read-past-blank-hours))
  (let* ((title (read-string "Title: "))
         (category (org-memento-read-category))
         (donep (and end (time-less-p (current-time) end)))
         (checkin (format-time-string (org-time-stamp-format t t)
                                      start))
         (closed (if donep
                     (concat org-closed-string " "
                             (format-time-string (org-time-stamp-format t t)
                                                 end)
                             "\n")
                   ""))
         (active (if donep
                     ""
                   (concat (org-memento--format-active-range start end)
                           "\n")))
         (org-capture-entry `("" ""
                              entry #'org-memento-goto-today
                              ,(concat "* " (if donep "DONE " "") title "\n"
                                       closed
                                       ":PROPERTIES:\n"
                                       ":memento_checkin_time: " checkin "\n"
                                       ":memento_category: " category
                                       ":END:\n"
                                       active
                                       "%?"))))
    (org-capture)))

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
      (org-memento--maybe-checkin-to-day)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (unless (save-excursion
                (re-search-forward (rx bol "*" (+ blank)) initial-pos t))
        (goto-char initial-pos)))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun org-memento-checkout-from-day ()
  "Run this command when you finish all your work on the day."
  (interactive)
  (org-memento-with-today-entry
   (org-todo 'done)
   (org-memento--save-buffer)
   (setq org-memento-block-idle-logging t)
   (run-hooks 'org-memento-checkout-hook)))

;;;###autoload
(defun org-memento-display-empty-slots (from-date to-date &optional duration-minutes)
  "Display a list of empty slots during a period."
  (interactive (append (org-memento--read-date-range)
                       (when (equal current-prefix-arg '(4))
                         (list (org-duration-to-minutes
                                (read-string "Duration [H:MM]: "))))))
  (let ((duration-secs (when duration-minutes (* 60 duration-minutes))))
    (with-current-buffer (get-buffer-create "*Memento Slots*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase-dolist (`(,start . ,end)
                       (org-memento--search-empty-slots from-date to-date))
          (when (or (null duration-secs)
                    (> (- end start) duration-secs))
            (insert (org-memento--format-active-range start end)
                    "\n")))
        (org-mode))
      (read-only-mode t)
      (display-buffer (current-buffer)))))

(defun org-memento-schedule-away-time (start end)
  (let* ((title (completing-read "Title: " org-memento-schedule-away-alist))
         (org-capture-entry `("" ""
                              entry (function org-memento-goto-idle)
                              ,(concat "* " title "\n"
                                       (org-format-time-string
                                        (org-memento--format-active-range
                                         start end))
                                       "\n%?"))))
    (org-capture)))

;;;; Timers and notifications

(defun org-memento-block-timeout ()
  (org-memento))

(defun org-memento--cancel-block-timer ()
  (when org-memento-block-timer
    (cancel-timer org-memento-block-timer)
    (setq org-memento-block-timer nil)))

(defun org-memento-setup-daily-timer ()
  (unless org-memento-daily-timer
    (let ((time (encode-time
                 (decoded-time-add (org-memento--start-of-day
                                    (decode-time
                                     (org-memento--current-time)))
                                   (make-decoded-time :hour 23 :minute 59)))))
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

(defun org-memento-schedule-block (start end)
  (let ((result (org-memento-read-block-or-template "Schedule a new block: "
                                                    :start start
                                                    :end end)))
    (cl-typecase result
      (org-memento-block
       (save-current-buffer
         (org-with-point-at (org-memento-block-hd-marker result)
           (org-end-of-meta-data t)
           (let ((found (looking-at (concat org-ts-regexp (rx (* blank) "\n")))))
             (when found (replace-match ""))
             (insert (org-memento--format-active-range start end) "\n")
             (org-memento--save-buffer)
             (if found
                 (message "Replaced the existing timestamp")
               (message "Inserted a new timestamp"))))))
      (org-memento-template
       (org-memento-with-today-entry
        (org-memento--expand-templates (list (make-org-memento-twc
                                              :template result
                                              :context nil
                                              :starting-time (float-time start)
                                              :ending-time (float-time end))))
        (org-memento--save-buffer)
        (message "Added a new block from the selected template")))
      (string
       (let ((org-capture-entry `("" ""
                                  entry (function org-memento-goto-today)
                                  ,(concat "* " result "\n"
                                           (org-memento--format-active-range start end)
                                           "\n%?"))))
         (org-capture))))))

;;;; Functions for working with the structure

(defun org-memento--buffer ()
  (or (find-buffer-visiting org-memento-file)
      (let ((auto-insert-mode nil))
        (find-file-noselect org-memento-file))))

(defun org-memento--save-buffer ()
  (let ((make-backup-files nil)
        (version-control nil))
    (save-buffer)))

(defun org-memento-goto-today ()
  "Switch the buffer to the file and go to the today's entry.

This function is primarily intended for use in
`org-capture-templates'."
  (find-file org-memento-file)
  (org-memento--find-today))

(defun org-memento--find-today ()
  "Move the point to the today's entry or insert the entry."
  (let ((today (org-memento--today-string (decode-time (org-memento--current-time)))))
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

(defun org-memento-goto-idle ()
  "Go to the today's idle entry."
  (org-goto-marker-or-bmk (org-memento-with-today-entry
                           (org-memento--find-or-create-idle-heading)
                           (point-marker))))

(defun org-memento--find-or-create-idle-heading ()
  "Move the point to the next idle heading in the buffer.

To use this function properly, you have to move the point to the
daily entry."
  (org-narrow-to-subtree)
  (unless (re-search-forward (format org-complex-heading-regexp-format
                                     org-memento-idle-heading)
                             nil t)
    (goto-char (point-max))
    (insert (if (not (bolp))
                "\n"
              "")
            "** " org-memento-idle-heading "\n")
    (end-of-line 0)))

(defun org-memento-map-past-days (func)
  (with-current-buffer (org-memento--buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((regexp (rx-to-string
                    `(and bol "*" (+ blank)
                          (?  (regexp ,org-todo-regexp) (+ blank))
                          (regexp ,(org-memento--make-past-date-regexp
                                    (decode-time (org-memento--current-time)))))))
           result)
       (while (re-search-forward regexp nil t)
         (save-excursion
           (beginning-of-line 1)
           (org-narrow-to-subtree)
           (push (funcall func) result)
           (widen)))
       (nreverse result)))))

;;;;; Updating properties

(defun org-memento-set-duration (duration)
  "Set the duration of the block at point."
  (interactive "sDuration (H:MM): ")
  (org-entry-put nil "Effort" duration))

(defun org-memento-set-category (category)
  "Set the category of the block at point."
  (interactive (list (completing-read "Category: "
                                      (org-memento--all-categories)
                                      nil nil nil nil
                                      (org-entry-get nil "memento_category"))))
  (org-entry-put nil "memento_category" category))

(defun org-memento--maybe-check-in ()
  "If the entry has no check-in time, record the current time.

This function can be called both on a daily entry (at level 1)
and on a time block entry (at level 2).

The function returns non-nil if the check-in is done."
  (unless (org-entry-get nil "memento_checkin_time")
    (org-entry-put nil "memento_checkin_time" (org-memento--inactive-ts-string
                                               (org-memento--current-time)))
    t))

(defun org-memento--maybe-checkin-to-day ()
  "Check in to the daily entry, if it is not done yet."
  (setq org-memento-block-idle-logging nil)
  (when (org-memento--maybe-check-in)
    ;; The point should be moved to the heading to call scaffolding
    (org-back-to-heading)
    (org-memento--insert-checking-out-time)
    (save-excursion
      (atomic-change-group
        (org-memento--scaffold-day)))
    (org-memento--save-buffer)
    (org-end-of-meta-data t)
    (run-hooks 'org-memento-checkin-hook)
    t))

(defun org-memento--insert-checking-out-time ()
  "Insert the expected checkout time of the day after the metadata.

The point must be at the heading."
  (save-excursion
    (if (looking-at org-complex-heading-regexp)
        (let ((day (parse-time-string (match-string 4)))
              (now (org-memento--current-time)))
          (org-end-of-meta-data t)
          ;; If there is an existing active timestamp, don't insert it.
          (unless (re-search-forward org-ts-regexp (org-entry-end-position) t)
            (when-let (duration-string (plist-get (org-memento--standard-workhour day)
                                                  :normal-duration))
              (let ((end-time (time-add now (* 60 (org-duration-to-minutes
                                                   duration-string)))))
                (insert (org-memento--format-active-range now end-time)
                        "\n")))))
      (error "The heading does not match org-complex-heading-regexp"))))

;;;;; Scanning

(defun org-memento-status (&optional check-in)
  "Update the status."
  (interactive)
  (setq org-memento-status-data (org-memento--block-data
                                 (or check-in
                                     (called-interactively-p t))))
  (when-let (event (org-memento--next-agenda-event
                    (car org-memento-status-data)))
    (setq org-memento-next-event event)))

(defun org-memento--block-data (&optional check-in)
  ;; The first item will always be the day itself.
  (org-memento-with-today-entry
   (when check-in
     (org-memento--maybe-checkin-to-day))
   (org-narrow-to-subtree)
   (org-map-entries #'org-memento-block-entry
                    nil nil
                    (lambda ()
                      (when (and (looking-at org-complex-heading-regexp)
                                 (or (< 2 (length (match-string 1)))
                                     (let ((headline (match-string 4)))
                                       (or (equal org-memento-idle-heading headline)
                                           (string-prefix-p "COMMENT" headline)))))
                        (re-search-forward (rx bol "*" (? "*") blank)
                                           nil t))))))

(defun org-memento-block-entry ()
  "Return information on the block at point."
  (org-back-to-heading)
  (make-org-memento-block
   :hd-marker (point-marker)
   :headline (org-element-headline-parser)
   :active-ts (when (re-search-forward org-ts-regexp
                                       (org-entry-end-position)
                                       t)
                (org-timestamp-from-string (match-string 0)))))

;;;;; Selecting blocks or finding a block from the status

;; To use these functions, first you have to call `org-memento-status' to update
;; the data.

(defun org-memento-today-as-block ()
  "Return information on the current date as a block."
  (car org-memento-status-data))

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
    (seq-filter #'org-memento-started-time)
    (seq-filter #'org-memento-block-not-closed-p)))

(defun org-memento--current-block ()
  (when org-memento-current-block
    (thread-last
      (org-memento--blocks)
      (seq-find (lambda (block)
                  (equal (org-memento-title block)
                         org-memento-current-block))))))

(defun org-memento--free-blocks ()
  (thread-last
    (org-memento--blocks)
    (seq-filter (lambda (x)
                  (and (org-memento-block-not-closed-p x)
                       (or (org-memento-started-time x)
                           (not (org-memento-block-active-ts x))))))))

(defun org-memento--sort-blocks-by-ts (block-plists)
  "Sort blocks by active timestamps."
  (cl-sort block-plists #'time-less-p
           :key (lambda (block)
                  (when-let (ts (org-memento-block-active-ts block))
                    (org-timestamp-to-time ts)))))

;;;; Inserting block entries from templates

(defun org-memento-add-blocks ()
  (org-memento-with-today-entry
   (atomic-change-group
     (org-memento--scaffold-day))))

(defun org-memento--scaffold-day ()
  "If the daily entry has no blocks, insert blocks from templates.

The buffer must be narrowed to the day, and the point must be on
the daily entry."
  (unless (save-excursion
            (goto-char (org-entry-end-position))
            (looking-at (rx (* space) "**" blank)))
    (let ((block (org-memento-block-entry)))
      (if (org-memento-started-time block)
          (org-memento--expand-templates
           (org-memento--make-default-plan block))
        (user-error "First please check in to the entry.")))))

(defun org-memento--expand-templates (templates-with-context)
  "Expand templates with the context.

The buffer must be narrowed to the day, and the point must be on
the daily entry."
  (save-excursion
    (goto-char (org-entry-end-position))
    (dolist (item templates-with-context)
      (let* ((template (org-memento-twc-template item))
             (extra-data (save-current-buffer
                           (org-with-point-at
                               (org-find-olp
                                (cons (org-memento-template-file template)
                                      (org-memento-template-olp template)))
                             (org-memento--parse-template-extra)))))
        (insert (if (and (eolp) (not (bolp)))
                    "\n"
                  "")
                "** " (org-memento-title item)
                ;; FIXME: `org-set-tags' seems to be currently buggy, so
                ;; insert the string literally.
                (if-let (tags (org-memento-template-tags
                               (org-memento-twc-template item)))
                    (concat " :" (string-join tags ":") ":")
                  "")
                "\n")
        ;; Copy the properties
        (pcase-dolist (`(,key . ,value) (plist-get extra-data :props))
          (org-entry-put nil key value))
        (when-let (duration (org-memento-duration item))
          (org-entry-put nil "Effort" (org-duration-from-minutes duration)))
        (when (looking-at org-heading-regexp)
          (end-of-line 0))
        ;; Set the time
        (let ((starting-time (org-memento-starting-time item))
              (ending-time (org-memento-ending-time item)))
          (when starting-time
            (insert (if (bolp)
                        ""
                      "\n")
                    (org-memento--format-active-range
                     starting-time
                     (pcase (org-memento-template-normal-hour
                             (org-memento-twc-template item))
                       ((and `(,_ ,_ ,end)
                             (guard end))
                        ending-time))))))

        ;; Insert the body
        (when-let (body (plist-get extra-data :body))
          (insert (if (bolp)
                      ""
                    "\n")
                  body))))))

(defun org-memento--parse-template-extra ()
  "Parse extra template data at point."
  (let ((end (org-entry-end-position))
        (case-fold-search t))
    (save-excursion
      (list :props
            (when (re-search-forward org-property-start-re end t)
              (let ((drawer-end (save-excursion
                                  (re-search-forward org-property-end-re)))
                    props)
                (while (re-search-forward org-property-re drawer-end t)
                  (unless (string-match-p (rx bol (or "memento_normal_"
                                                      (and "memento_checkin_time" eol)
                                                      (and "END" eol)))
                                          (match-string 2))
                    (push (cons (match-string 2)
                                (match-string 3))
                          props)))
                (nreverse props)))
            :body
            (progn
              (org-end-of-meta-data)
              (let ((string (string-trim (buffer-substring (point) end))))
                (unless (string-empty-p string)
                  string)))))))

;;;; Reporting

;;;;; Check-in time

(defun org-memento-average-checkin-time ()
  "Return the average check-in time of past days."
  (let* ((entries (thread-last
                    (org-memento-map-past-days #'org-memento--checkin-time)
                    (delq nil)
                    (mapcar #'org-memento--seconds-since-midnight)))
         (seconds (/ (cl-reduce #'+ entries :initial-value 0)
                     (length entries))))
    (org-duration-from-minutes (/ seconds 60))))

;;;; Formatting status

(defun org-memento--format-block-status ()
  (let* ((block (org-memento-with-current-block
                  (org-memento-block-entry)))
         (ending-time (org-memento-ending-time block)))
    (concat "Current: " org-memento-current-block " "
            (if ending-time
                (let ((minutes (org-memento-minutes-from-now ending-time)))
                  (format " (ending at %s, %s)"
                          (format-time-string "%R" ending-time)
                          (if (> minutes 0)
                              (format "in %d minutes" minutes)
                            "overdue")))
              "")
            "\nNext event: "
            (if org-memento-next-event
                (format "%s (starting at %s)"
                        (save-current-buffer
                          (org-with-point-at (org-memento-org-event-marker
                                              org-memento-next-event)
                            (org-get-heading t t t t)))
                        (org-memento-starting-time org-memento-next-event))
              "None"))))

(defun org-memento--format-org-event-status (event)
  (format "\"%s\" starts at %s."
          (save-current-buffer
            (org-with-point-at (org-memento-org-event-marker event)
              (org-get-heading nil nil nil nil)))
          (format-time-string "%R" (org-memento-starting-time event))))

;;;; Manage templates

(defun org-memento-goto-template-parent ()
  "Return the marker to one of the template roots or its descendant."
  (let* ((alist (nreverse (org-memento--map-template-entries
                           (lambda (_source)
                             (cons (org-format-outline-path
                                    (org-get-outline-path t t)
                                    nil (buffer-name) "/")
                                   (point-marker)))
                           t)))
         (completions-sort nil)
         (choice (completing-read "Choose a parent: " alist
                                  nil t)))
    (org-goto-marker-or-bmk (cdr (assoc choice alist)))))

(defun org-memento-templates ()
  "Return a list of template entries loaded from the source files."
  (let (templates)
    (org-memento--map-template-entries
     (lambda (source)
       (push (org-memento--parse-template-spec source)
             templates)))
    (nreverse templates)))

(defun org-memento--parse-template-spec (source)
  (let* ((level (org-outline-level))
         (olp (org-get-outline-path t t))
         (relative-olp (pcase source
                         (`(file+olp ,_ . ,root-olp)
                          (seq-drop olp (length root-olp)))
                         (_
                          olp))))
    (make-org-memento-template
     :file (buffer-file-name)
     :olp olp
     :relative-olp relative-olp
     :leaf-p (save-excursion
               (goto-char (org-entry-end-position))
               (not (looking-at (concat "[[:space:]]*"
                                        (regexp-quote (make-string (1+ level) ?\*))
                                        "\\*?[[:blank:]]"))))
     :title (org-get-heading t t t t)
     :category (or (org-entry-get nil "memento_category" t)
                   (car relative-olp))
     :tags (org-get-tags)
     :normal-hour (when-let (string (org-entry-get nil "memento_normal_hour" t))
                    (org-memento--parse-normal-hour string))
     :normal-dows (when-let (string (org-entry-get nil "memento_normal_dows" t))
                    (org-memento--parse-normal-dows string))
     :duration (when-let (string (org-entry-get nil "Effort" t))
                 (floor (org-duration-to-minutes string))))))

(defun org-memento--parse-normal-hour (string)
  (if (string-match (rx (group (or "absolute" "relative"))
                        (+ blank)
                        (group (and (+ digit) ":" (+ digit)))
                        (?  "-" (group (and (+ digit) ":" (+ digit)))))
                    string)
      (list (intern (match-string 1 string))
            (floor (org-duration-to-minutes (match-string 2 string)))
            (when-let (s (match-string 3 string))
              (floor (org-duration-to-minutes s))))
    (error "Failed to match an hour spec against %s" string)))

(defun org-memento--parse-normal-dows (string)
  (mapcar #'string-to-number (split-string string)))

(defun org-memento--map-template-entries (func &optional include-roots)
  "Map a function on each entry under the template roots."
  (let (result)
    (dolist (source org-memento-template-sources)
      (setq result
            (append result
                    (pcase source
                      (`(file+olp ,file . ,olp)
                       (if-let (root (org-find-olp (cons (cl-etypecase file
                                                           (symbol (symbol-value file))
                                                           (string file))
                                                         olp)))
                           (save-current-buffer
                             (org-with-point-at root
                               (if include-roots
                                   (org-narrow-to-subtree)
                                 (narrow-to-region (org-entry-end-position)
                                                   (org-end-of-subtree)))
                               (org-map-entries
                                `(lambda ()
                                   (funcall ',func ',source))
                                nil nil :archive t)))
                         (error "Failed to find %s" source)))))))
    result))

(defun org-memento--make-default-plan (context)
  "Return templates with contexts for the day."
  (let* ((start-time (org-memento-started-time context))
         (decoded-time (decode-time start-time))
         (day-start-decoded (org-memento--start-of-day decoded-time))
         (this-dow (nth 6 day-start-decoded)))
    (thread-last
      (org-memento-templates)
      (seq-filter `(lambda (template)
                     (and (org-memento-template-leaf-p template)
                          (memq ,this-dow (org-memento-template-normal-dows template)))))
      (mapcar (apply-partially #'org-memento--init-twc context))
      (org-memento--sort-by #'org-memento-starting-time
                            (lambda (a b)
                              (if (and a b)
                                  (< a b)
                                a))))))

(defun org-memento--sort-by (key pred items)
  (cl-sort items pred :key key))

;;;###autoload
(defun org-memento-add-template ()
  "Add a template for time block."
  (interactive)
  (let ((org-capture-entry '("" ""
                             entry (function org-memento-goto-template-parent)
                             "* %?")))
    (org-capture)))

;;;; Completion

(defun org-memento-read-category (&optional prompt)
  "Prompt for a category name."
  (completing-read (or prompt "Category: ")
                   (org-memento--all-categories)))

(defun org-memento--all-categories ()
  (cl-remove-duplicates
   (thread-last
     (org-memento-templates)
     (mapcar #'org-memento-template-category)
     (delq nil)
     (append (with-current-buffer (org-memento--buffer)
               (org-property-get-allowed-values nil "memento_category"))))
   :test #'equal))

(cl-defun org-memento-read-block-or-template (prompt &key start end)
  (let* ((todayp (when start
                   (time-equal-p (thread-last
                                   (decode-time start)
                                   (org-memento--start-of-day))
                                 (thread-last
                                   (decode-time (org-memento--current-time))
                                   (org-memento--start-of-day)))))
         (duration (when (and start end)
                     (/ (- (float-time end) (float-time start)) 60)))
         (blocks (when todayp
                   ;; Use already generated blocks only for today
                   (org-memento-status)
                   (seq-filter `(lambda (block)
                                  ;; The following two types only matter:
                                  ;;
                                  (cond
                                   ;; * unfinished and unscheduled blocks
                                   ((and (not (org-memento-starting-time block))
                                         (not (org-memento-ended-time block)))
                                    (or (not ,duration)
                                        (not (org-memento-duration block))
                                        (<= (org-memento-duration block)
                                            ,duration)))
                                   ;; * checked-in but unfinished blocks
                                   ((and (org-memento-started-time block)
                                         (not (org-memento-ended-time block)))
                                    t)))
                               (org-memento--blocks))))
         ;; templates
         (templates (if duration
                        (thread-last
                          (org-memento-templates)
                          (seq-filter `(lambda (template)
                                         (or (not (org-memento-duration template))
                                             (<= (org-memento-duration template)
                                                 ,duration)))))
                      (org-memento-templates)))
         (cache (make-hash-table :test #'equal :size (+ (length blocks)
                                                        (length templates))))
         items)
    (unwind-protect
        (cl-labels
            ((annotation (candidate)
               (cl-typecase (gethash candidate cache)
                 (org-memento-block
                  " (block)")
                 (org-memento-template
                  " (template)")
                 (otherwise
                  "")))
             (table (string pred action)
               (if (eq action 'metadata)
                   (cons 'metadata
                         (list (cons 'category 'org-memento-eventable)
                               (cons 'annotation-function #'annotation)))
                 (complete-with-action action items string pred))))
          (dolist (block blocks)
            (let ((title (org-memento-title block)))
              (push title items)
              (puthash title block cache)))
          (dolist (template templates)
            (let ((title (org-format-outline-path
                          (org-memento-template-relative-olp template))))
              (remove-text-properties 0 (length title) '(face) title)
              (push title items)
              (puthash title template cache)))
          (let ((input (completing-read prompt #'table)))
            (gethash input cache input)))
      (clrhash cache))))

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
      (puthash (org-memento-title block) block org-memento-block-cache))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . org-memento-block)
                         (annotation-function . org-memento-block-annotator)))
         (complete-with-action action ',(mapcar #'org-memento-title items)
                               string pred)))))

(defun org-memento-block-annotator (title)
  (if-let (block (gethash title org-memento-block-cache))
      (concat (when-let (time (org-memento-starting-time block))
                (propertize (format " %s, in %d minutes"
                                    (format-time-string "%R" time)
                                    (org-memento-minutes-from-now time))
                            'face 'font-lock-warning-face))
              (when-let (duration (org-memento-duration block))
                (propertize (format " (%s)" (org-duration-from-minutes duration))
                            'face 'font-lock-doc-face))
              (when-let (time (org-memento-started-time block))
                (propertize (format-time-string " already checked in at %R" time)
                            'face 'font-lock-comment-face)))
    ""))

;;;; Retrieving timing information

(defun org-memento--read-past-blank-hours ()
  (let* ((today (car org-memento-status-data))
         (idle-hours (org-memento--idle-hours))
         (completions-sort nil)
         (start-string (thread-last
                         (cdr org-memento-status-data)
                         (mapcar #'org-memento-ended-time)
                         (delq nil)
                         (append (thread-last
                                   idle-hours
                                   (mapcar #'cdr)
                                   (delq nil)
                                   (mapcar #'encode-time)))
                         (mapcar (lambda (time) (format-time-string "%F %R" time)))
                         (cons (org-memento-started-time today))
                         (append '("other"))
                         ;; (seq-sort #'string-lessp)
                         (completing-read "Start: ")))
         (start (encode-time
                 (if (equal start-string "other")
                     (with-temp-buffer
                       (org-time-stamp nil)
                       (goto-char (point-min))
                       (looking-at org-ts-regexp)
                       (parse-time-string (match-string 1)))
                   (parse-time-string start-string))))
         (end-string (thread-last
                       (cdr org-memento-status-data)
                       (mapcar #'org-memento-started-time)
                       (delq nil)
                       (append (thread-last
                                 idle-hours
                                 (mapcar #'car)
                                 (delq nil)
                                 (mapcar #'encode-time)))
                       (seq-filter `(lambda (time)
                                      (time-less-p ',start time)))
                       (cons (current-time))
                       (mapcar (lambda (time) (format-time-string "%F %R" time)))
                       (append '("other"))
                       ;; (seq-sort #'string-less-p)
                       (completing-read "End: ")))
         (end (encode-time
               (if (equal end-string "other")
                   (with-temp-buffer
                     (org-time-stamp nil)
                     (goto-char (point-min))
                     (looking-at org-ts-regexp)
                     (parse-time-string (match-string 1)))
                 (parse-time-string end-string)))))
    (list start end)))

(defun org-memento--idle-hours ()
  "Return idle hours on today."
  (when org-memento-idle-heading
    (org-memento-with-today-entry
     (org-narrow-to-subtree)
     (when (re-search-forward (format org-complex-heading-regexp-format
                                      org-memento-idle-heading)
                              nil t)
       (org-memento--clock-ranges)))))

(defun org-memento--clock-ranges ()
  "Return a list of pairs of decoded times of clocked entries."
  (let ((bound (org-entry-end-position))
        result)
    (while (re-search-forward org-clock-line-re bound t)
      (let ((start (when (re-search-forward org-ts-regexp-inactive (pos-eol) t)
                     (parse-time-string (match-string 1))))
            (end (when (and (looking-at "--")
                            (re-search-forward org-ts-regexp-inactive (pos-eol) t))
                   (parse-time-string (match-string 1)))))
        (push (cons start end) result)))
    result))

(defun org-memento--calculated-end-time (block)
  "Return an end time calculated from the duration."
  (when-let ((checkin (org-memento-started-time block))
             (duration (org-memento-duration block)))
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

(defun org-memento--next-agenda-event (&optional hd-marker bound-time)
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
             (when (and (or (not min-time)
                            (< mtime min-time))
                        (not (org-entry-is-done-p)))
               (let ((marker (save-excursion
                               (org-back-to-heading)
                               (point-marker))))
                 (unless (and hd-marker
                              (equal marker hd-marker))
                   (setq min-time mtime)
                   (setq result (make-org-memento-org-event
                                 :marker marker
                                 :active-ts ts
                                 :margin-secs margin))))))))))
    result))

(defun org-memento--agenda-events (from-date to-date)
  "Scan all entries with an active time stamp between a range."
  (let ((ts-regexp (org-memento--make-ts-regexp
                    (encode-time (org-memento--fill-decoded-time from-date))
                    (encode-time (org-memento--fill-decoded-time to-date))))
        result)
    (dolist (file (org-agenda-files))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward ts-regexp nil t)
           (let ((obj (make-org-memento-org-event
                       :marker (point-marker)
                       :active-ts (org-timestamp-from-string (match-string 0)))))
             (push (cons (org-memento-starting-time obj)
                         obj)
                   result)
             (goto-char (org-entry-end-position)))))))
    (thread-last
      result
      ;; (org-memento--sort-by-car result)
      (mapcar #'cdr))))

(defun org-memento--standard-workhour (decoded-time)
  "Return a plist which specifies the work hour for the day."
  (let ((dow (apply #'org-day-of-week
                    (thread-first
                      decoded-time
                      (seq-drop 3)
                      (seq-take 3)))))
    (seq-some `(lambda (cell)
                 (when (memq ,dow (car cell))
                   (cdr cell)))
              org-memento-workhour-alist)))

(defun org-memento--search-empty-slots (from-date to-date)
  "Return a list of available time ranges during a period.

FROM-DATE and TO-DATE must be given as decoded times.

The returned value will be a list of (START . END) where START
and END are float times."
  (let* ((events (org-memento--agenda-events from-date to-date))
         result)
    (dolist (date (org-memento--date-list from-date to-date))
      (catch 'day-end
        (when-let* ((workhour (org-memento--standard-workhour date))
                    (checkin (plist-get workhour :normal-checkin))
                    (duration (plist-get workhour :normal-duration)))
          (let* ((checkin-minutes (floor (org-duration-to-minutes checkin)))
                 (duration-minutes (org-duration-to-minutes duration))
                 (time (float-time (encode-time
                                    (org-memento--set-time-of-day date
                                                                  (floor (/ checkin-minutes 60))
                                                                  (mod checkin-minutes 60)
                                                                  0))))
                 (day-end (+ time (* 60 duration-minutes))))
            (pcase (org-memento--search-empty-slots-1 time day-end events)
              (`(,part . ,remaining-events)
               (setq events remaining-events)
               (setq result (append result part))))))))
    result))

(defun org-memento--search-empty-slots-1 (initial-time day-end events)
  (let ((time initial-time)
        (event (pop events))
        result)
    (catch 'day-end
      (while (< time day-end)
        (cond
         ;; No event remaining, so the entire day will be available
         ((null event)
          (push (cons time day-end)
                result)
          (throw 'day-end t))
         ((< day-end (org-memento-starting-time event))
          (push (cons time day-end)
                result)
          (throw 'day-end t))
         ((< time (org-memento-starting-time event))
          (push (cons time (org-memento-starting-time event))
                result)
          (if (< (org-memento-ending-time-default event) day-end)
              (progn
                (setq time (org-memento-ending-time-default event))
                (setq event (pop events)))
            (throw 'day-end t)))
         ((> time (org-memento-starting-time event))
          (cond
           ((> time (org-memento-ending-time-default event))
            (setq event (pop events)))
           ((< (org-memento-ending-time-default event) day-end)
            (setq time (org-memento-ending-time-default event))
            (setq event (pop events)))
           (t
            (throw 'day-end t)))))))
    (cons (nreverse result)
          (if event
              (cons event events)
            events))))

(defun org-memento-workhour-end ()
  "Return time at which today's work ends."
  (when-let* ((time (org-memento--current-time))
              (decoded-time (decode-time time))
              (plist (org-memento--standard-workhour
                      (org-memento--start-of-day decoded-time)))
              (duration (plist-get plist :normal-duration))
              (checkin-time (org-memento-with-today-entry
                             (org-memento--checkin-time))))
    (time-add checkin-time
              (* 60 (org-duration-to-minutes duration)))))

;;;; Utility functions for time representations and Org timestamps

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

(defun org-memento--today-string (decoded-time)
  (format-time-string "%F"
                      (encode-time
                       (org-memento--maybe-decrement-date decoded-time))))

(defun org-memento--start-of-day (decoded-time)
  "Return the start of the day given as DECODED-TIME.

This respects `org-extend-today-until'."
  (org-memento--set-time-of-day (org-memento--maybe-decrement-date decoded-time)
                                (or org-extend-today-until 0)
                                0
                                0))

(defun org-memento--maybe-decrement-date (decoded-time)
  (if (and org-extend-today-until
           (< (nth 2 decoded-time) org-extend-today-until))
      (decoded-time-add decoded-time (make-decoded-time :day -1))
    decoded-time))

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

(defun org-memento--make-past-date-regexp (today)
  (cl-flet
      ((year-string (d)
         (format "%04d" d))
       (month-string (d)
         (format "%02d" d))
       (day-string (d)
         (format "%02d" d)))
    (let* ((this-year (nth 5 today))
           (this-month (nth 4 today))
           (this-day (nth 3 today))
           (previous-year-strings (thread-last
                                    (number-sequence 2000 (1- this-year))
                                    (mapcar #'year-string)))
           (previous-month-strings (thread-last
                                     (number-sequence 1 (1- this-month))
                                     (mapcar #'month-string)))
           (previous-day-strings (thread-last
                                   (number-sequence 1 (1- this-day))
                                   (mapcar #'day-string)))
           (all-month-strings (thread-last
                                (number-sequence 1 12)
                                (mapcar #'month-string)))
           (all-day-strings (thread-last
                              (number-sequence 1 31)
                              (mapcar #'day-string))))
      (rx-to-string `(or (and (or ,@previous-year-strings)
                              "-" (or ,@all-month-strings)
                              "-" (or ,@all-day-strings))
                         (and ,(number-to-string this-year)
                              "-"
                              (or (and (or ,@previous-month-strings)
                                       "-" (or ,@all-day-strings))
                                  (and ,(number-to-string this-month)
                                       "-" (or ,@previous-day-strings)))))))))

(defun org-memento--seconds-since-midnight (time)
  (- (float-time time)
     (float-time (encode-time (org-memento--set-time-of-day (decode-time time) 0 0 0)))))

(defun org-memento--time-min (time1 time2)
  "Return an earlier time of the two.

Both TIME1 and TIME2 must be an internal time representation or
nil. If one of them is nil, the other one is returned."
  (if (and time1 time2)
      (if (time-less-p time1 time2)
          time1
        time2)
    (or time1 time2)))

(defun org-memento--date-list (from-date to-date)
  "Return a date range in a list of decoded times."
  (let ((day (make-decoded-time :day 1))
        (date (org-memento--set-time-of-day from-date 0 0 0))
        (end-time (encode-time (org-memento--set-time-of-day to-date 23 59 0)))
        result)
    (while (time-less-p (encode-time date) end-time)
      (push date result)
      (setq date (decoded-time-add date day)))
    (nreverse result)))

(defun org-memento--read-date-range ()
  "Return a date range in a list of decoded times."
  (let ((org-extend-today-until 0)
        result)
    (with-temp-buffer
      (dotimes (_ 2)
        (org-time-stamp nil)
        (goto-char (point-min))
        (looking-at org-ts-regexp)
        (push (parse-time-string (match-string 1)) result)))
    (nreverse result)))

(defun org-memento--format-active-range (start-time end-time)
  (format (org-format-time-string "<%Y-%m-%d %a %%s%%s>" start-time)
          (org-format-time-string "%H:%M" start-time)
          (if end-time
              (org-format-time-string "-%H:%M" end-time)
            "")))

(defun org-memento--format-army-time-range (start end)
  (let ((midnight (thread-first
                    (org-memento--start-of-day (decode-time start))
                    (org-memento--set-time-of-day 0 0 0)
                    (encode-time)
                    (float-time))))
    (concat (org-memento--format-army-time start midnight)
            (unless (time-equal-p start end)
              (format "-%s" (org-memento--format-army-time end midnight))))))

(defun org-memento--format-army-time (time midnight)
  (let ((minutes (/ (- (thread-last
                         (float-time time)
                         (floor))
                       midnight)
                    60)))
    (format "%02d:%02d"
            (floor (/ minutes 60))
            (mod minutes 60))))

(cl-defun org-memento-read-time-of-day (&key start-time
                                             ;; decoded-date
                                             ;; past
                                             ;; future
                                             initial-value)
  "Prompt a time of day or a time range.

It returns the number of minutes since the midnight.

Optionally, it accepts h:MM-h:MM format, in which case the
function returns a list of durations."
  (let* ((prompt (format "%s (%sh:MM): "
                         (if start-time
                             "End time"
                           "Start time")
                         (if start-time
                             (format-time-string "%R-" start-time)
                           "")))
         (input (completing-read prompt nil nil nil
                                 (when initial-value
                                   ;; org-duration-from-minutes returns a 1d
                                   ;; h:mm string if the input is longer than
                                   ;; 24 hours, which is undesirable in this case.
                                   (format "%d:%02d"
                                           (/ (floor initial-value) 60)
                                           (mod (floor initial-value) 60))))))
    (cond
     ((string-empty-p input)
      nil)
     ((string-match (rx bol
                        (group (+ digit) ":" (+ digit))
                        "-"
                        (group (+ digit) ":" (+ digit))
                        eol)
                    input)
      (list (org-duration-to-minutes (match-string 1 input))
            (org-duration-to-minutes (match-string 2 input))))
     (t
      (org-duration-to-minutes input)))))

;;;; Integrations with third-party packages

;;;;; org-ql

;;;###autoload
(defun org-memento-make-agenda-block ()
  "Return an `org-agenda' block for time blocks on today.

Note that this functionality uses `org-ql-block', so you have to
install org-ql package to use it.

A recommended way to use this function is to define an
interactive command that wraps `org-agenda' function. Otherwise,
you would have to update the value of
`org-agenda-custom-commands' every day before you get to work."
  (require 'org-ql-search)
  `(org-ql-block '(and (level 2)
                       (parent (heading ,(org-memento--today-string
                                          (decode-time))))
                       (not (heading-regexp
                             ,(rx-to-string `(or (and bol "COMMENT")
                                                 ,org-memento-idle-heading)))))
                 ((org-ql-block-header "Time blocks")
                  (org-agenda-files '(,org-memento-file))
                  (org-super-agenda-properties-inherit nil)
                  (org-super-agenda-groups
                   '((:name "Closed" :todo "DONE")
                     (:name "Working on" :property "memento_checkin_time")
                     (:auto-map org-memento--super-agenda-ts-map)
                     (:name "Unscheduled" :anything t))))))

(defun org-memento--super-agenda-ts-map (item)
  (when-let* ((marker (or (get-text-property 0 'org-marker item)
                          (get-text-property 0 'org-hd-marker item)))
              (ts (save-current-buffer
                    (org-with-point-at marker
                      (when (re-search-forward (concat "<" org-ts-regexp1 "[^>\n]\\{5,16\\}>")
                                               (org-entry-end-position) t)
                        (org-timestamp-from-string (match-string 0)))))))
    (let ((start (org-timestamp-to-time ts))
          (end (org-timestamp-to-time ts t)))
      (concat (org-memento--format-army-time-range start end)
              (unless (time-equal-p start end)
                (format " (%.1fh)"
                        (/ (- (float-time end) (float-time start))
                           3600)))))))

(provide 'org-memento)
;;; org-memento.el ends here
