;;; org-memento.el --- Time blocking with Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (taxy "0.10"))
;; Keywords: calendar
;; URL: https://git.sr.ht/~akirak/org-memento

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
(declare-function taxy-emptied "ext:taxy" t t)
(declare-function taxy-fill "ext:taxy" t t)
(declare-function taxy-sort-items "ext:taxy" t t)
(declare-function taxy-taxys "ext:taxy" t t)
(declare-function taxy-items "ext:taxy" t t)
(declare-function taxy-name "ext:taxy" t t)
(declare-function make-taxy "ext:taxy" t t)
(declare-function org-capture "org-capture")
(declare-function org-clocking-p "org-clock")
(defvar org-capture-entry)

(defgroup org-memento nil
  "Time blocking with Org mode."
  :group 'org)

;;;; Constants

(defconst org-memento-date-regexp
  (rx (repeat 4 digit) "-"
      (repeat 2 digit) "-"
      (repeat 2 digit))
  "Regexp that matches date headings.")

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

(defcustom org-memento-block-start-hook nil
  "Hook run after starting a block."
  :type 'hook)

(defcustom org-memento-block-before-exit-hook nil
  "Hook run just before finishing or stopping a block.

This hook is called in the initial buffer before performing any
modification on the current entry."
  :type 'hook)

(defcustom org-memento-block-exit-hook nil
  "Hook run after finishing or stopping a block."
  :type 'hook)

(defcustom org-memento-checkin-hook nil
  "Hook run after checking in to a daily entry.

If you run `org-memento-open-today' or one of the other commands
that perform daily check-in, this hook is called after
scaffolding is done before the user gets interactivity. The hook
is first called at the next line of an active timestamp in the
body of the entry, so you can use it to insert some contents into
the daily entry.

Note that this hook is not called on blocks inside a daily entry."
  :type 'hook)

(defcustom org-memento-checkout-hook nil
  "Hook run when `org-memento-checkout-from-day' command is run."
  :type 'hook)

(defcustom org-memento-status-hook nil
  "Hook run when the status is updated with `org-memento-status'."
  :type 'hook)

(defcustom org-memento-agenda-files nil
  "Function used to determine agenda files for each block.

If the variable is a function, the function is called at the
heading of the block and should return a list of Org files.

When `org-memento-timeline' evaluates efficiency of each block
activity, only clock activities on the designated set of Org
files are considered proper utilization; other activities are
distractions."
  :type '(choice (const nil)
                 (function :tag "Function without an argument")))

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

(defcustom org-memento-group-function
  (lambda (element)
    (list (org-element-property :MEMENTO_CATEGORY element)))
  "Function that determines the group of the entry.

It is called at the heading of the entry without arguments.

It should return a list, and the group equality is compared using
`equal' on the entire list."
  :type 'function)

(defcustom org-memento-group-formatters
  '(identity)
  "List of functions used to format each level of the group."
  :type '(repeat function))

(defcustom org-memento-unique-properties
  '("CATEGORY"
    "MEMENTO_CHECKIN_TIME"
    "ID")
  "List of Org entry properties that should not be copied.

Note that all property names should be upper-cased."
  :type '(repeat string))

;;;; Variables

(defvar org-memento-status-data nil)

(defvar org-memento-current-block nil
  "Headline of the current block.")

(defvar org-memento-current-category nil)

(defvar org-memento-current-time nil
  "When non-nil, use this as the current time for testing.")

(defvar org-memento-block-timer nil)

(defvar org-memento-daily-timer nil)

(defvar org-memento-idle-timer nil)

(defvar org-memento-block-idle-logging nil
  "Prevent from idle logging till next check-in.")

(defvar org-memento-title-string nil)

;;;; Substs

(defsubst org-memento--current-time ()
  (or org-memento-current-time (current-time)))

(defsubst org-memento-minutes-from-now (float-time)
  (round (/ (- float-time (float-time (org-memento--current-time)))
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
  (if-let (element (org-memento-headline-element x))
      (when-let (str (org-element-property :MEMENTO_CHECKIN_TIME element))
        (float-time (org-timestamp-to-time (org-timestamp-from-string str))))
    (when-let* ((marker (org-memento-block-hd-marker x))
                (str (save-current-buffer
                       (org-with-point-at marker
                         (org-entry-get nil "memento_checkin_time")))))
      (float-time (org-timestamp-to-time (org-timestamp-from-string str))))))

(cl-defmethod org-memento-starting-time ((x org-memento-block))
  (when-let (ts (org-memento-active-ts x))
    (float-time (org-timestamp-to-time ts))))

(cl-defmethod org-memento-ending-time ((x org-memento-block))
  (or (when-let (ts (org-memento-block-active-ts x))
        (let ((end-time (org-timestamp-to-time ts 'end)))
          (unless (time-equal-p (org-timestamp-to-time ts) end-time)
            (float-time end-time))))
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
           (end-time (org-timestamp-to-time ts 'end)))
      (prog1 (setf (org-memento-org-event-ending-time x)
                   (if (not (time-equal-p (org-timestamp-to-time ts)
                                          end-time))
                       (float-time end-time)
                     (when-let (duration (org-memento-duration x))
                       (+ (float-time (org-timestamp-to-time ts))
                          (* 60 duration)))))
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

;;;; Macros

(defmacro org-memento-with-today-entry (&rest progn)
  `(with-current-buffer (org-memento--buffer)
     (org-with-wide-buffer
      (org-memento--find-today)
      ,@progn)))

(defmacro org-memento-maybe-with-date-entry (date &rest progn)
  (declare (indent 1))
  `(with-current-buffer (org-memento--buffer)
     (org-with-wide-buffer
      (goto-char (point-min))
      (when (re-search-forward (format org-complex-heading-regexp-format
                                       (regexp-quote (cl-etypecase ,date
                                                       (list (format-time-string "%F" ',date))
                                                       (number (format-time-string "%F" ,date))
                                                       (string ,date))))
                               nil t)
        ,@progn))))

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
    (message "Idle is detected by org-memento [%s]"
             (format-time-string "%F %R"))
    (let ((time-user-left (time-subtract (org-memento--current-time)
                                         (* 60 org-memento-idle-time))))
      (org-memento-with-today-entry
       (org-memento--find-or-create-idle-heading)
       (org-clock-in nil time-user-left)
       (add-hook 'pre-command-hook #'org-memento-unidle)))))

(defun org-memento-unidle ()
  (remove-hook 'pre-command-hook #'org-memento-unidle)
  (message "Idle finished [%s]" (format-time-string "%F %R"))
  (org-clock-out))

;;;; Commands

;;;###autoload
(defun org-memento-next-action ()
  "Pick an action depending on the current status."
  (interactive)
  (when (org-clocking-p)
    (user-error "Please finish your clock first."))
  (org-memento-status)
  (if-let (block (org-memento--current-block))
      (when (yes-or-no-p "Finish the current block?")
        (org-memento-finish-block))
    ;; It is hard to decide on the next action. Below is only an example.
    (let* ((upnext-event (org-memento--next-agenda-event))
           (time (when upnext-event (org-memento-starting-time upnext-event))))
      (cond
       ;; If there is an upcoming event that should be started within 10
       ;; minutes, display it.
       ((and time
             (< (- time (float-time (org-memento--current-time)))
                (* 10 60)))
        (org-goto-marker-or-bmk (org-memento-org-event-marker upnext-event)))
       ;; Start working on one of the remaining blocks.
       ((seq-find #'org-memento-block-not-closed-p (org-memento--blocks))
        (call-interactively #'org-memento-start-block))
       (t
        ;; Otherwise, I have no idea what would be the best thing to do in
        ;; general. Just display the status.
        (org-agenda))))))

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
    (when (org-memento--maybe-check-in :adjust t)
      (org-memento--save-buffer)))
  (setq org-memento-current-block title)
  ;; (org-memento-setup-daily-timer)
  (org-memento-status)
  (run-hooks 'org-memento-block-start-hook)
  (org-memento--setup-block-end-timer))

(defun org-memento-finish-block ()
  "Mark the current block as done."
  (interactive)
  (when org-memento-current-block
    (run-hooks 'org-memento-block-before-exit-hook)
    (org-memento-with-current-block
      (org-todo 'done)
      (org-memento--save-buffer))
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-block-exit-hook)))

(defun org-memento-stop-block ()
  "Change the state of the current block to a done keyword."
  (interactive)
  (when org-memento-current-block
    (run-hooks 'org-memento-block-before-exit-hook)
    (org-memento-with-current-block
      (org-todo (completing-read "Change the state: " org-done-keywords))
      (org-memento--save-buffer))
    (setq org-memento-current-block nil)
    (org-memento--cancel-block-timer)
    (run-hooks 'org-memento-block-exit-hook)))

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
    (widen)
    (push-mark)
    (let ((pos (point))
          (existing (org-memento--find-today)))
      (org-memento--maybe-checkin-to-day)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (when (and existing
                 (> pos (point-min))
                 (< pos (point-max)))
        (pop-mark))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun org-memento-checkout-from-day ()
  "Run this command when you finish all your work on the day."
  (interactive)
  (org-memento-with-today-entry
   (org-todo 'done)
   (org-memento--save-buffer)
   (setq org-memento-block-idle-logging t)
   (run-hooks 'org-memento-checkout-hook)))

(defun org-memento-adjust-time ()
  "Adjust the active timestamp of the entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-end-of-meta-data)
    (when (looking-at org-logbook-drawer-re)
      (goto-char (match-end 0)))
    (let ((has-ts (looking-at org-ts-regexp)))
      (when (org-time-stamp nil)
        (unless has-ts (insert "\n"))))))

;;;; Timers and notifications

(defun org-memento-block-timeout ()
  ;; TODO: Use alert.el
  (message "The event has timed out"))

(defun org-memento--setup-block-end-timer ()
  "Start a timer that finishes the current block."
  (org-memento--cancel-block-timer)
  (let ((time (org-memento-ending-time (org-memento--current-block)))
        (now (float-time (org-memento--current-time))))
    (when (and time (> time now))
      (setq org-memento-block-timer
            (run-with-timer (- time now)
                            nil #'org-memento-block-timeout)))))

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
  "Move the point to the today's entry or insert the entry.

The function returns non-nil if the heading is existing. It
returns nil if it creates a new heading.

After the function is called, the point should be at the
beginning of the entry."
  (let ((today (org-memento--today-string (decode-time (org-memento--current-time)))))
    (org-memento--goto-date today)))

(defun org-memento--goto-date (date)
  "Move the point to the entry of a given date or insert a new one.

DATE must be a string in ISO-8601 format.

The function returns non-nil if the heading is existing. It
returns nil if it creates a new heading.

After the function is called, the point should be at the
beginning of the entry."
  (or (re-search-backward (format org-complex-heading-regexp-format
                                  (regexp-quote date))
                          nil t)
      (catch 'found-heading
        (goto-char (point-min))
        (while (re-search-forward org-complex-heading-regexp nil t)
          (let ((heading (match-string 4)))
            (cond
             ((equal date heading)
              (beginning-of-line 1)
              (throw 'found-heading t))
             ;; Past date
             ((and (string-match-p org-memento-date-regexp heading)
                   (string-lessp heading date))
              (beginning-of-line)
              (insert "* " date "\n")
              (beginning-of-line 0)
              (throw 'found-heading nil)))))
        (insert (if (bolp) "" "\n")
                "* " date "\n")
        (beginning-of-line 0)
        ;; Explicitly return nil
        nil)))

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

(cl-defun org-memento--maybe-check-in (&key adjust)
  "If the entry has no check-in time, record the current time.

This function can be called both on a daily entry (at level 1)
and on a time block entry (at level 2).

The function returns non-nil if the check-in is done."
  (unless (org-entry-get nil "memento_checkin_time")
    (let ((now (org-memento--current-time)))
      (org-entry-put nil "memento_checkin_time" (org-memento--inactive-ts-string now))
      (when adjust
        (org-memento--move-active-ts now)))
    t))

(defun org-memento--move-active-ts (start-time)
  "Adjust the end time of an active ts according to START-TIME."
  (org-back-to-heading)
  (org-end-of-meta-data)
  (when (looking-at org-logbook-drawer-re)
    (goto-char (match-end 0)))
  (when-let (duration (when (looking-at org-ts-regexp)
                        (save-match-data
                          (org-memento--duration-secs-ts-at-point))))
    (replace-match (org-memento--format-active-range
                    start-time (time-add start-time duration)))))

(defun org-memento--maybe-checkin-to-day ()
  "Check in to the daily entry, if it is not done yet."
  (setq org-memento-block-idle-logging nil)
  (when (org-memento--maybe-check-in)
    ;; The point should be moved to the heading to call scaffolding
    (org-back-to-heading)
    (org-memento--insert-checking-out-time)
    ;; (save-excursion
    ;;   (atomic-change-group
    ;;     (org-memento--scaffold-day)))
    (org-memento--save-buffer)
    (org-end-of-meta-data t)
    (when (looking-at org-ts-regexp)
      (beginning-of-line 2))
    (save-excursion
      (run-hooks 'org-memento-checkin-hook))
    (org-memento-status)
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
          (unless (looking-at org-ts-regexp)
            (when-let (duration-string (plist-get (org-memento--normal-workhour day)
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
  (unless (and org-memento-current-block
               (seq-find (lambda (block)
                           (and (equal (org-memento-title block)
                                       org-memento-current-block)
                                (org-memento-started-time block)
                                (not (org-memento-ended-time block))))
                         (org-memento--blocks)))
    (setq org-memento-current-block
          (seq-some (lambda (block)
                      (when (and (org-memento-started-time block)
                                 (not (org-memento-ended-time block)))
                        (org-memento-title block)))
                    (org-memento--blocks))))
  (let* ((block (org-memento--current-block))
         (category (when block (org-memento-block-category block)))
         (ending-time (when block (org-memento-ending-time block))))
    (setq org-memento-current-category category))
  (run-hooks 'org-memento-status-hook))

(defun org-memento--status ()
  "Only update `org-memento-status-data'."
  (interactive)
  (setq org-memento-status-data (org-memento--block-data)))

(defun org-memento--block-data (&optional check-in)
  ;; The first item will always be the day itself.
  (org-memento-with-today-entry
   (when check-in
     (org-memento--maybe-checkin-to-day))
   (org-narrow-to-subtree)
   (org-map-entries #'org-memento-block-entry
                    nil nil
                    (lambda ()
                      (and (looking-at org-complex-heading-regexp)
                           (not (= 1 (length (match-string 1))))
                           (or (> (length (match-string 1))
                                  2)
                               (let ((headline (match-string 4)))
                                 (or (equal org-memento-idle-heading headline)
                                     (string-prefix-p "COMMENT" headline))))
                           (org-end-of-subtree))))))

(defun org-memento-block-entry ()
  "Return information on the block at point."
  (org-back-to-heading)
  (make-org-memento-block
   :hd-marker (point-marker)
   :headline (org-element-headline-parser (org-entry-end-position))
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

;;;; Agenda files

;;;###autoload
(defun org-memento-agenda-files ()
  "Return a list of Org agenda files for the current block."
  (org-memento-with-block-title (or org-memento-current-block
                                    (error "Currently no block"))
    (pcase org-memento-agenda-files
      ((pred functionp) (funcall org-memento-agenda-files)))))

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

;;;; Completion

(defun org-memento-read-category (&optional prompt)
  "Prompt for a category name."
  (completing-read (or prompt "Category: ")
                   (org-memento--all-categories)))

(defun org-memento--all-categories ()
  (with-current-buffer (org-memento--buffer)
    (delq nil (org-property-values "memento_category"))))

(cl-defun org-memento-read-title (&optional prompt &key category default)
  (completing-read (or prompt "Title: ")
                   (when category
                     (org-memento--titles-in-category category))
                   nil nil nil nil default 'inherit-input-method))

(defun org-memento--titles-in-category (category)
  (let (result)
    (with-current-buffer (org-memento--buffer)
      (let ((regexp (org-re-property "memento_category" nil nil category)))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward regexp nil t)
           (let ((heading (org-get-heading t t t t)))
             (remove-text-properties 0 (length heading) '(face) heading)
             (push heading result))))))
    (cl-remove-duplicates result :test #'equal)))

(defun org-memento--read-block-to-start ()
  (org-memento-status 'check-in)
  (let ((cache (make-hash-table :test #'equal :size 20))
        candidates)
    (cl-labels
        ((annotator (title)
           (if-let (block (gethash title cache))
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
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-block)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))

      (dolist (block (thread-last
                       (org-memento--blocks)
                       (seq-filter #'org-memento-block-not-closed-p)))
        (let ((title (org-memento-title block)))
          (puthash title block cache)
          (push title candidates)))
      (completing-read "Start a block: " #'completions))))

(defun org-memento-read-future-event (start end-bound)
  (org-memento--status)
  (let* ((now (float-time (org-memento--current-time)))
         (cache (make-hash-table :test #'equal :size 100))
         (duration-limit (/ (- end-bound start) 60))
         (prompt (format "Thing to do in %s-%s (%d min): "
                         (format-time-string "%R" start)
                         (format-time-string "%R" end-bound)
                         duration-limit))
         candidates)
    (cl-labels
        ((annotator (title)
           (pcase (gethash title cache)
             ((and (pred org-memento-block-p)
                   block)
              (let* ((time (org-memento-starting-time block))
                     (ending-time (org-memento-ending-time block))
                     (duration (or (org-memento-duration block)
                                   (when (and ending-time time)
                                     (/ (- ending-time time) 60))))
                     (category (org-memento-block-category block)))
                (concat (when time
                          (propertize (format " scheduled %s-%s"
                                              (format-time-string "%R" time)
                                              (format-time-string "%R" ending-time))
                                      'face 'font-lock-doc-face))
                        (when (and time (< time now))
                          (propertize (format " (should have started %.f minutes ago)"
                                              (/ (- now time) 60))
                                      'face 'font-lock-warning-face))
                        (when category
                          (propertize (concat " " category)
                                      'face 'font-lock-doc-face))
                        (when duration
                          (propertize (concat " " (org-duration-from-minutes duration))
                                      'face 'font-lock-doc-face)))))
             (`(group . ,group-with-entries)
              (org-memento--group-annotator group-with-entries))
             (_
              "")))
         (group (candidate transform)
           (if transform
               candidate
             (pcase (gethash candidate cache)
               ((pred org-memento-block-p)
                "Blocks to (re)allocate")
               (`(group . ,_)
                "Groups"))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-block)
                           (cons 'annotation-function #'annotator)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred))))
      (progn
        (let ((blocks (thread-last
                        (org-memento--blocks)
                        (seq-filter #'org-memento-block-not-closed-p)
                        (seq-filter `(lambda (block)
                                       (if-let (duration (org-memento-duration block))
                                           (< duration ,duration-limit)
                                         (if-let* ((starting (org-memento-starting-time block))
                                                   (ending (org-memento-ending-time block)))
                                             (< (/ (- ending starting) 60)
                                                ,duration-limit)
                                           t)))))))
          (dolist (block (append (seq-filter `(lambda (block)
                                                (and (org-memento-starting-time block)
                                                     (< (org-memento-starting-time block) ',now)))
                                             blocks)
                                 (seq-filter `(lambda (block)
                                                (not (org-memento-starting-time block)))
                                             blocks)))
            (puthash (org-memento-title block) block cache)
            (push (org-memento-title block) candidates)))
        (dolist (group-with-entries (org-memento--group-alist-1))
          (let ((group-title (org-memento--format-group (car group-with-entries))))
            (puthash group-title (cons 'group group-with-entries) cache)
            (push group-title candidates)))
        (setq candidates (nreverse candidates))
        (unwind-protect
            (let* ((completions-sort nil)
                   (input (completing-read prompt #'completions))
                   (entry (gethash input cache input)))
              (pcase entry
                (`(group . ,group-with-entries)
                 (org-memento--read-group-entry (cdr group-with-entries) start end-bound))
                (_
                 entry)))
          (clrhash cache))))))

(defun org-memento--read-group-entry (entries start end-bound)
  (let ((cache (make-hash-table :test #'equal :size (length entries)))
        candidates)
    (cl-labels
        ((annotator (candidate)
           (pcase (gethash candidate cache)
             (`(,start ,end ,_date ,_title . ,plist)
              (format " %s %s"
                      (plist-get plist :todo-keyword)
                      (org-duration-from-minutes (/ (- end start) 60))))))
         (sort-candidates (candidates)
           (cl-sort candidates #'string>))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'annotation-function #'annotator)
                           (cons 'display-sort-function #'sort-candidates)))
             (complete-with-action action candidates string pred))))
      (progn
        (dolist (entry entries)
          (pcase-exhaustive entry
            (`(,start ,end ,date ,heading . ,plist)
             (let ((title (string-join (list date heading)
                                       "/")))
               (puthash title entry cache)
               (push title candidates)))))
        (let ((input (completing-read (format "Thing to do in %s-%s (%.f min): "
                                              (format-time-string "%R" start)
                                              (format-time-string "%R" end-bound)
                                              (/ (- end-bound start) 60))
                                      #'completions nil t)))
          (pcase-exhaustive (gethash input cache)
            (`(,start1 ,end1 ,date ,headline . ,_)
             (list 'copy-entry
                   (org-find-olp (list org-memento-file date headline))
                   :title
                   (read-from-minibuffer "Title: " headline nil nil nil nil 'input-method)
                   :time
                   (org-memento--read-time-span
                    (org-memento--format-active-range
                     start (+ start (- end1 start1))))))))))))

(defun org-memento--group-alist-1 ()
  (thread-last
    (org-memento--collect-groups-1)
    (seq-group-by #'car)
    (cl-remove-if (lambda (group-with-entries)
                    (seq-every-p #'null (car group-with-entries))))
    (mapcar (lambda (group)
              (cons (car group)
                    (nreverse (mapcar #'cdr (cdr group))))))))

(defun org-memento--group-annotator (group-with-entries)
  (let* ((days (ceiling (/ (- (float-time (org-memento--current-time))
                              (caadr group-with-entries))
                           (* 3600 24))))
         (days (if (and (= days 1)
                        (> (caadr group-with-entries)
                           (thread-first
                             (org-memento--current-time)
                             (decode-time)
                             (org-memento--start-of-day)
                             (encode-time)
                             (float-time))))
                   0
                 days))
         (days-formatted (pcase days
                           (0 "today")
                           (1 "yesterday")
                           ((pred numberp) (format "%d days ago" days))
                           (_ (error "days: %s" days))))
         (durations (org-memento--group-durations group-with-entries)))
    (format-spec " %d (%D)"
                 `((?d . ,days-formatted)
                   (?D . ,(mapconcat #'org-duration-from-minutes durations " "))))))

(defun org-memento--group-durations (group-with-entries)
  (thread-last
    (cdr group-with-entries)
    (mapcar (lambda (list)
              (/ (- (nth 1 list)
                    (nth 0 list))
                 60)))
    (mapcar (lambda (minutes)
              (when-let (round (cond
                                ((< minutes 30)
                                 5)
                                ((< minutes 60)
                                 10)
                                ((< minutes 90)
                                 15)
                                ((< minutes 180)
                                 30)
                                ((<= minutes 300)
                                 60)
                                (t
                                 nil)))
                (* (ceiling (/ minutes round)) round))))
    (delq nil)
    (seq-sort #'<)
    (seq-uniq)))

(defun org-memento--format-group (group)
  (let ((fns (copy-sequence org-memento-group-formatters))
        (values (copy-sequence group))
        fn
        strings)
    (while (setq fn (pop fns))
      (when-let (string (funcall fn (pop values)))
        (push string
              strings)))
    (string-join (nreverse strings) " > ")))

;;;; Retrieving timing information

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
           (unless (and (equal file (expand-file-name org-memento-file))
                        (= 1 (org-outline-level)))
             (when-let* ((ts (org-timestamp-from-string (match-string 0)))
                         (time (when (org-timestamp-has-time-p ts)
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
                                   :margin-secs margin)))))))))))
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

(defun org-memento--normal-workhour (decoded-time)
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

;;;; Collect data for analytic purposes

;;;###autoload
(defun org-memento-activity-taxy (start-day end-day)
  (require 'taxy)
  (cl-labels
      ((date-string-to-time (string)
         (thread-last
           string
           parse-time-string
           org-memento--fill-decoded-time
           encode-time))
       (format-inactive-ts (time)
         (format-time-string (org-time-stamp-format t t) time))
       (format-range (start end)
         (if start
             (format "%s--%s"
                     (format-inactive-ts start)
                     (if end
                         (format-inactive-ts end)
                       ""))
           ""))
       (make-gap-date (start end)
         (cons (list start end nil)
               (list (list start end nil nil 'gap))))
       (make-block (start end)
         (list start end nil nil 'gap))
       (compare-dates (a b)
         (or (> (car a) (car b))
             (and (= (car a) (car b))
                  (> (cadr a) (cadr b)))))
       (thresp (seconds)
         (> seconds 60.0))
       (fill-voids (start-bound end-bound key make-record records)
         (if records
             (let* (result
                    (sorted-records (cl-sort records #'compare-dates
                                             :key key))
                    (next-start (or end-bound
                                    (cadr (funcall key (car sorted-records))))))
               (dolist (item sorted-records)
                 (let* ((start (car (funcall key item)))
                        (end (cadr (funcall key item))))
                   (when (and end
                              (< end next-start)
                              (thresp (- next-start end)))
                     (let ((new-item (funcall make-record end next-start)))
                       (unless (member new-item sorted-records)
                         (push new-item result))))
                   (setq next-start start)
                   (push item result)))
               (when (and start-bound
                          (< start-bound next-start)
                          (thresp (- next-start start-bound)))
                 (push (funcall make-record start-bound next-start)
                       result))
               result)
           ;; Return nil if there is no record during the span.
           nil))
       (make-block-taxy (block-record)
         (pcase block-record
           (`(,start ,end . ,_)
            (make-taxy
             :name block-record
             :predicate `(lambda (record)
                           (and ,start
                                ,end
                                (>= (car record) ,start)
                                (< (cadr record) ,end)))
             :then #'identity))))
       (prepend-unless-empty (list1 list2)
         (when list2
           (append list1 list2)))
       (make-date-taxy (date-record)
         (pcase date-record
           (`((,start ,end . ,_) . ,blocks)
            (let ((computed-start (or start
                                      (thread-first
                                        (decode-time start)
                                        (org-memento--start-of-day)
                                        (encode-time)
                                        (float-time))))
                  (computed-end (or end
                                    (thread-first
                                      (decode-time end)
                                      (org-memento--start-of-day)
                                      (decoded-time-add (make-decoded-time
                                                         :hour 23
                                                         :minute 59))
                                      (encode-time)
                                      (float-time)))))
              (make-taxy
               :name (car date-record)
               :predicate `(lambda (record)
                             (and (>= (car record) ,computed-start)
                                  (< (cadr record) ,computed-end)))
               :then #'identity
               :taxys (let ((now (float-time (org-memento--current-time))))
                        (or (thread-last
                              blocks
                              (fill-voids start end #'identity #'make-block)
                              (mapcar #'make-block-taxy))
                            (when end
                              (if (< end now)
                                  (list (make-block-taxy (list start end nil)))
                                (list (make-block-taxy (list start now nil))
                                      (make-block-taxy (list now end nil))))))))))))
       (make-gap-block (start end)
         (list start end nil))
       (make-gap-block-taxy (start end)
         (make-taxy
          :name (make-gap-block start end)))
       (make-block-taxy-for-item (item-record)
         (if (caddr item-record)
             (make-taxy
              :name (append (seq-take item-record 2)
                            (list nil
                                  nil
                                  'anonymous))
              :items (list item-record))
           (make-taxy
            :name item-record
            :items nil)))
       (fill-voids-in-block-taxy (block-taxy)
         (fill-voids (car (taxy-name block-taxy))
                     (cadr (taxy-name block-taxy))
                     #'identity #'make-gap-block
                     (taxy-items block-taxy)))
       (split-block-taxy (block-taxy)
         (thread-last
           (fill-voids-in-block-taxy block-taxy)
           (mapcar #'make-block-taxy-for-item)))
       (block-taxy-reducer (block-taxy acc)
         ;; If the block has a third element ,then it is a named block.
         (if (caddr (taxy-name block-taxy))
             (cons (make-taxy :name (taxy-name block-taxy)
                              :items (fill-voids (car (taxy-name block-taxy))
                                                 (cadr (taxy-name block-taxy))
                                                 #'identity #'make-gap-block
                                                 (taxy-items block-taxy)))
                   acc)
           (append (split-block-taxy block-taxy) acc)))
       (postprocess-block-taxys (block-taxys)
         (cl-reduce #'block-taxy-reducer block-taxys
                    :initial-value nil
                    :from-end t))
       ;; When compiled, setf inside cl-labels seems to cause an error.
       ;; For now, I will define this function to set the particular field.
       (set-taxy-taxys (taxy new-value)
         (aset taxy (cl-struct-slot-offset 'taxy 'taxys) new-value))
       (postprocess-root-taxy (taxy)
         (dolist (date-taxy (taxy-taxys taxy))
           (set-taxy-taxys date-taxy
                           (thread-last
                             (taxy-taxys date-taxy)
                             (postprocess-block-taxys)
                             (fill-voids (car (taxy-name date-taxy))
                                         (cadr (taxy-name date-taxy))
                                         #'taxy-name #'make-gap-block-taxy))))
         taxy)
       ;; Use `make-block-taxy' here.
       (make-empty-date-taxy (start end)
         (make-taxy
          :name (list start end nil)
          :taxys (list (make-taxy
                        :name (list start end nil)
                        :items nil))))
       (date-filler (date-taxy acc)
         (if-let* ((start (and (and (caddr (taxy-name date-taxy))
                                    (and acc (caddr (taxy-name (car acc)))))
                               (cadr (taxy-name date-taxy))))
                   (end (car (taxy-name (car acc)))))
             (cons date-taxy (cons (make-empty-date-taxy start end)
                                   ;; TODO: We can use this instead of the
                                   ;; expression above
                                   ;; (make-block-taxy (list start end nil))
                                   acc))
           (cons date-taxy acc)))
       (fill-date-gaps (taxy)
         (set-taxy-taxys taxy (cl-reduce #'date-filler (taxy-taxys taxy)
                                         :initial-value nil
                                         :from-end t))
         taxy))
    (let* ((now (float-time (org-memento--current-time)))
           (start-time (or (org-memento-maybe-with-date-entry start-day
                             (when-let (string (org-entry-get nil "memento_checkin_time"))
                               (thread-last
                                 (org-timestamp-from-string string)
                                 (org-timestamp-to-time))))
                           (thread-first
                             (parse-time-string start-day)
                             (org-memento--set-time-of-day (or org-extend-today-until) 0 0)
                             (encode-time))))
           (end-time (or (org-memento-maybe-with-date-entry end-day
                           (let ((block (save-excursion
                                          (org-memento-block-entry))))
                             (if-let (ended (org-memento-ended-time block))
                                 (time-convert ended 'list)
                               (let ((ending (org-memento-ending-time block)))
                                 (when (and ending (> ending now))
                                   (time-convert ending 'list))))))
                         (thread-first
                           (parse-time-string end-day)
                           (org-memento--set-time-of-day
                            (or org-extend-today-until) 0 0)
                           (decoded-time-add (make-decoded-time :hour 23 :minute 59))
                           (encode-time)))))
      (thread-last
        (make-taxy
         :name (list start-time end-time)
         :taxys (thread-last
                  (org-memento--block-activities start-day end-day)
                  (fill-voids (float-time start-time) (float-time end-time) #'car #'make-gap-date)
                  (mapcar #'make-date-taxy)))
        (taxy-emptied)
        (taxy-fill (when (and (not org-memento-current-block)
                              (not (org-clocking-p))
                              (> now (float-time start-time))
                              (< now (float-time end-time)))
                     (list (list now now nil nil 'now))))
        (taxy-fill (org-memento--agenda-activities
                    start-time
                    end-time
                    (cl-remove (expand-file-name org-memento-file)
                               (org-agenda-files)
                               :test #'equal)))
        (taxy-sort-items #'< #'car)
        (postprocess-root-taxy)
        (fill-date-gaps)))))

(cl-defun org-memento--agenda-activities (start-bound end-bound &optional files)
  "Gather activities during a certain date period from files.

Both START-BOUND and END-BOUND should be Emacs internal time
representations, which specifies the target time span.

FILES is a list of Org files from which activities are searched.
It should be usually `org-agenda-files'.

This function returns a list of lists. Each entry takes the form
(START END TITLE MARKER TYPE . ARGS) where START and END are
floats, TITLE is the heading of the Org entry the activity
occured at, MARKER is a marker to the headline, TYPE is a symbol
denoting the type of the activity. ARGS is an optional list."
  (let* ((files (or files (org-agenda-files)))
         (now (org-memento--current-time))
         (contains-future (time-less-p now end-bound))
         (regexp (org-memento--make-ts-regexp
                  start-bound end-bound
                  :active contains-future :inactive t))
         (start-bound-float (float-time start-bound))
         (end-bound-float (float-time end-bound))
         (now-float (when contains-future
                      (float-time now)))
         result)
    (cl-flet*
        ((parse-time (string)
           (thread-last
             (parse-time-string string)
             (encode-time)
             (float-time)
             (floor)))
         (scan ()
           (let ((hd-marker (point-marker))
                 (heading (when (looking-at org-complex-heading-regexp)
                            (match-string-no-properties 4)))
                 (bound (org-entry-end-position)))
             (when (re-search-forward org-logbook-drawer-re bound t)
               (goto-char (match-beginning 0))
               (let ((drawer-end (match-end 0)))
                 (while (re-search-forward (rx bol (* space) "CLOCK:" (* blank))
                                           drawer-end t)
                   (when (looking-at (rx (regexp org-ts-regexp-inactive)
                                         (?  "--" (regexp org-ts-regexp-inactive))))
                     (let ((start (parse-time (match-string 1)))
                           (end (when-let (str (match-string 2))
                                  (parse-time str))))
                       (when (and start
                                  (> start start-bound-float)
                                  (if end
                                      (< end end-bound-float)
                                    t))
                         (push (list start
                                     (or end (float-time (org-memento--current-time)))
                                     heading hd-marker
                                     (if end
                                         'clock
                                       'clock-unfinished))
                               result)))))))
             (when (and contains-future
                        (not (org-entry-is-done-p)))
               (catch 'active-ts
                 (goto-char hd-marker)
                 (while (re-search-forward org-ts-regexp bound t)
                   ;; Skip SCHEDULED or DEADLINE line
                   (unless (save-match-data
                             (save-excursion
                               (goto-char (pos-bol))
                               (looking-at org-planning-line-re)))
                     (let ((ts (org-timestamp-from-string (match-string 0))))
                       (when (org-timestamp-has-time-p ts)
                         (let ((event (make-org-memento-org-event :marker hd-marker
                                                                  :active-ts ts)))
                           (when-let* ((starting-time (org-memento-starting-time event))
                                       (ending-time (org-memento-ending-time event)))
                             (when (and (> starting-time now-float)
                                        (> ending-time now-float))
                               (push (list starting-time
                                           ending-time
                                           heading
                                           hd-marker
                                           'active-ts)
                                     result)
                               (throw 'active-ts t))))))))))))
         (skip ()
           (let ((bound (org-entry-end-position)))
             (unless (save-excursion (re-search-forward regexp bound t))
               bound))))
      (org-map-entries #'scan nil files #'skip))
    (nreverse result)))

(defun org-memento--block-activities (start-date-string &optional end-date-string)
  (cl-flet*
      ((parse-date (string)
         (encode-time (org-memento--set-time-of-day
                       (parse-time-string string)
                       0 0 0)))
       (parse-date-at-point ()
         (when (looking-at org-memento-date-regexp)
           (parse-date (match-string 0))))
       (parse-time (string)
         (thread-last
           (parse-time-string string)
           (encode-time)
           (float-time)
           (floor)))
       (only-future (float)
         (when (and float
                    (> float (float-time (org-memento--current-time))))
           float))
       (parse-entry (include-future &optional away)
         (if (or include-future away)
             (let* ((block (org-memento-block-entry))
                    (start (or (org-memento-started-time block)
                               (if away
                                   (org-memento-starting-time block)
                                 (only-future (org-memento-starting-time block)))))
                    (end (or (org-memento-ended-time block)
                             (if away
                                 (org-memento-ending-time block)
                               (only-future (org-memento-ending-time block)))
                             (when (equal (org-memento-title block)
                                          org-memento-current-block)
                               (float-time (org-memento--current-time))))))
               (when start
                 (list start end)))
           ;; Faster version for past activities.
           (let* ((entry-end (org-entry-end-position))
                  (end (when (re-search-forward org-closed-time-regexp entry-end t)
                         (parse-time (match-string 1))))
                  (start (when-let (string (org-entry-get nil "memento_checkin_time"))
                           (when (string-match org-ts-regexp-inactive string)
                             (parse-time (match-string 1 string))))))
             (when start
               (list start end)))))
       (parse-idle-clocks ()
         (let ((hd-marker (point-marker)))
           (when (re-search-forward org-logbook-drawer-re (org-entry-end-position)
                                    t)
             (goto-char (match-beginning 0))
             (let ((drawer-end (match-end 0))
                   clocks)
               (while (re-search-forward (rx bol (* space) "CLOCK:" (* blank))
                                         drawer-end t)
                 (when (looking-at (concat org-ts-regexp-inactive
                                           "--"
                                           org-ts-regexp-inactive))
                   (push (list (parse-time (match-string 1))
                               (parse-time (match-string 2))
                               org-memento-idle-heading
                               hd-marker
                               'idle)
                         clocks)))
               clocks))))
       (parse-idle-children (include-future)
         (let ((subtree-end (save-excursion (org-end-of-subtree)))
               blocks)
           ;; This is like in `org-memento--agenda-activities', but without future
           ;; activities.
           (while (re-search-forward org-complex-heading-regexp subtree-end t)
             (let ((heading (match-string-no-properties 4))
                   (hd-marker (point-marker))
                   (bound (org-entry-end-position)))
               (pcase (save-excursion (parse-entry nil 'away))
                 (`(,start ,end)
                  (push (list start
                              end
                              heading
                              hd-marker
                              'away)
                        blocks)))
               (when (re-search-forward org-logbook-drawer-re bound t)
                 (let ((drawer-end (match-end 0)))
                   (goto-char (match-beginning 0))
                   (while (re-search-forward (rx bol (* space) "CLOCK:" (* blank))
                                             drawer-end t)
                     (when (looking-at (rx (regexp org-ts-regexp-inactive)
                                           (?  "--" (regexp org-ts-regexp-inactive))))
                       (let ((start (parse-time (match-string 1)))
                             (end (parse-time (match-string 2))))
                         (push (list start
                                     end
                                     heading
                                     hd-marker
                                     'away)
                               blocks))))))))
           blocks)))
    (with-current-buffer (org-memento--buffer)
      (org-save-outline-visibility t
        (widen)
        (goto-char (point-min))
        (outline-show-all)
        (let (dates)
          (while (re-search-forward (rx bol "*" blank) nil t)
            (when-let (date-string (and (re-search-forward org-memento-date-regexp
                                                           (pos-eol)
                                                           t)
                                        (match-string-no-properties 0)))
              (when (or (member date-string (list start-date-string
                                                  end-date-string))
                        (and (or (not end-date-string)
                                 (string-lessp date-string
                                               end-date-string))
                             (string-lessp start-date-string
                                           date-string)))
                (let ((marker (progn
                                (beginning-of-line)
                                (point-marker)))
                      (include-future (not (string-lessp date-string
                                                         (org-memento--today-string
                                                          (decode-time
                                                           (org-memento--current-time)))))))
                  (pcase (parse-entry include-future)
                    (`(,start ,end)
                     (let ((day (list start end date-string marker 'date))
                           (subtree-end (save-excursion (org-end-of-subtree)))
                           blocks)
                       (while (re-search-forward org-complex-heading-regexp subtree-end t)
                         (beginning-of-line)
                         (let ((level (length (match-string 1)))
                               (heading (match-string-no-properties 4))
                               (hd-marker (point-marker)))
                           (if (equal heading org-memento-idle-heading)
                               (setq blocks (append blocks
                                                    (parse-idle-clocks)
                                                    (parse-idle-children include-future)))
                             (pcase (parse-entry include-future)
                               (`(,start ,end)
                                (push (list start
                                            end
                                            heading
                                            hd-marker
                                            (if (= level 2)
                                                'block
                                              'away))
                                      blocks)))))
                         (end-of-line 1))
                       (push (cons day blocks) dates))))))))
          dates)))))

;;;; Grouping

(cl-defun org-memento--collect-groups-1 (&optional start-date-string end-date-string)
  (with-current-buffer (org-memento--buffer)
    (org-with-wide-buffer
     (org-memento--find-today)
     (let (result)
       (catch 'finish-scan
         (while (re-search-forward (format org-complex-heading-regexp-format
                                           org-memento-date-regexp)
                                   nil t)
           (if (and end-date-string (string-lessp end-date-string (match-string 4)))
               (org-end-of-subtree)
             (let ((date (match-string-no-properties 4))
                   (bound (save-excursion
                            (org-end-of-subtree))))
               (when (and start-date-string
                          (string-lessp date start-date-string))
                 (throw 'finish-scan t))
               (while (re-search-forward org-complex-heading-regexp bound t)
                 (when (and (equal (match-string 1) "**")
                            (not (or (equal (match-string 4) org-memento-idle-heading)
                                     (string-prefix-p org-comment-string (match-string 4))))
                            (org-entry-is-done-p))
                   ;; Drop invalid entries using when-let
                   (when-let ((block (org-memento-block-entry))
                              (started (org-memento-started-time block))
                              (ended (org-memento-ended-time block)))
                     (push (list (save-excursion
                                   (funcall org-memento-group-function
                                            (org-memento-block-headline block)))
                                 started
                                 ended
                                 date
                                 (org-memento-title block)
                                 :todo-keyword
                                 (org-element-property :todo-keyword
                                                       (org-memento-headline-element block)))
                           result)))
                 (org-end-of-subtree))))))
       result))))

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
  (save-match-data
    (when (string-match (rx bol (group (+ digit) ":" (+ digit))
                            (?  "-" (group (group (+ digit) ":" (+ digit))))
                            eol)
                        string)
      (cons (save-match-data (floor (org-duration-to-minutes (match-string 1 string))))
            (when (> (length (match-data)) 4)
              (floor (org-duration-to-minutes (match-string 2 string))))))))

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

(cl-defun org-memento--make-ts-regexp (from to &key (active t) inactive)
  "Return a regexp that matches timestamps in a range.

FROM and TO must be internal time representations. The regexp
matches long active timestamps. It is intended for fast timestamp
scanning, and it can produce false positives. You should perform
further checks against your desired time range.

ACTIVE and INACTIVE specify types of timestamp to match against."
  (let ((date-strs (thread-last
                     (number-sequence (float-time from) (float-time to) (* 3600 24))
                     (mapcar (lambda (float)
                               (format-time-string "%F" float))))))
    (rx-to-string `(and (any ,(thread-last
                                (list (when active
                                        ?\<)
                                      (when inactive
                                        ?\[))
                                (delq nil)
                                (mapconcat #'char-to-string)))
                        (and (or ,@date-strs)
                             blank (+? anything))
                        (any ,(thread-last
                                (list (when active
                                        ?\>)
                                      (when inactive
                                        ?\]))
                                (delq nil)
                                (mapconcat #'char-to-string)))))))

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

(defun org-memento--time-max (time1 time2)
  "Return an later time of the two.

Both TIME1 and TIME2 must be an internal time representation or
nil. If one of them is nil, the other one is returned."
  (if (and time1 time2)
      (if (time-less-p time2 time1)
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

(defun org-memento--read-time-span (&optional default)
  "Prompt for a time span.

DEFAULT is an optional timestamp string which contains the
default range.

This function returns (START END) where START and END are time
representations. END can be nil if the user doesn't enter a time
range."
  (let* ((ts (with-temp-buffer
               (when default
                 (insert default)
                 (goto-char (point-min)))
               (org-time-stamp nil)
               (goto-char (point-min))
               (org-element-timestamp-parser)))
         (start-time (org-memento--timestamp-to-time ts))
         (end-time (org-memento--timestamp-to-time ts 'end)))
    (list start-time
          (unless (time-equal-p start-time end-time)
            end-time))))

(defun org-memento--timestamp-to-time (ts &optional end)
  (encode-time (make-decoded-time
                :year (org-element-property (if end :year-end :year-start) ts)
                :month (org-element-property (if end :month-end :month-start) ts)
                :day (org-element-property (if end :day-end :day-start) ts)
                :hour (org-element-property (if end :hour-end :hour-start) ts)
                :minute (org-element-property (if end :minute-end :minute-start) ts)
                :second 0)))

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

(defun org-memento--format-duration (minutes)
  "An alternative function for formatting a duration."
  (format "%d:%02d"
          (floor (/ minutes 60))
          (mod minutes 60)))

(defun org-memento--duration-secs-ts-at-point ()
  "Return the duration in seconds from a timestamp at point."
  (let ((ts (org-element-timestamp-parser)))
    (when (and (org-timestamp-has-time-p ts)
               (not (time-equal-p (org-timestamp-to-time ts)
                                  (org-timestamp-to-time ts 'end))))
      (let ((start (org-timestamp-to-time ts))
            (end (org-timestamp-to-time ts 'end)))
        (- (float-time end)
           (float-time start))))))

(defun org-memento--remove-clock (entry-marker orig-start orig-end
                                               &optional start end)
  (cl-flet*
      ((format-inactive-ts (time)
         (format-time-string (org-time-stamp-format t t) time))
       (make-clock-line (list)
         (pcase-exhaustive list
           (`(,start ,end)
            (concat org-clock-string " "
                    (format-inactive-ts start)
                    "--"
                    (format-inactive-ts end)
                    " =>  "
                    (org-duration-from-minutes (/ (- end start) 60))
                    "\n")))))
    (save-current-buffer
      (org-with-point-at entry-marker
        (org-with-wide-buffer
         (org-back-to-heading)
         (let (entries)
           (when (> (- start orig-start) 60)
             (push (list orig-start start) entries))
           (when (> (- orig-end end) 60)
             (push (list end orig-end) entries))
           (re-search-forward (rx-to-string `(and bol (* blank) ,org-clock-string
                                                  (* blank)
                                                  ,(format-inactive-ts orig-start)
                                                  "--"
                                                  ,(format-inactive-ts orig-end)
                                                  (+ nonl)
                                                  "\n"))
                              (org-entry-end-position))
           (replace-match (mapconcat #'make-clock-line entries))))))))

;;;; Capture

;;;###autoload
(cl-defun org-memento-add-event (&key title category start end copy-from
                                      interactive away)
  "Insert an block/event entry into the journal"
  (interactive (let* ((span (org-memento--read-time-span))
                      (title (org-memento-read-title))
                      (away (equal current-prefix-arg '(4)))
                      (category (unless away (org-memento-read-category nil))))
                 (list :start (car span)
                       :end (cadr span)
                       :title title
                       :category category
                       :interactive t
                       :away away)))
  (let* ((date (format-time-string "%F" (thread-last
                                          (decode-time start)
                                          (org-memento--maybe-decrement-date)
                                          (encode-time))))
         (jump-fn `(lambda ()
                     (org-memento--goto-date ,date)
                     (when ,(and away t)
                       (org-memento--find-or-create-idle-heading))))
         (title (or title (org-memento-read-title)))
         (category (unless (or away copy-from)
                     (or category
                         (org-memento-read-category nil))))
         (template (if away
                       (org-memento--away-event-template
                        :start start :end end :title title
                        :interactive interactive)
                     (apply #'org-memento--event-template
                            :start start :end end :title title :category category
                            :interactive interactive
                            (when copy-from
                              (save-current-buffer
                                (org-with-point-at copy-from
                                  (list :tags (org-get-tags nil t)
                                        :properties
                                        (cl-remove-if
                                         (lambda (key)
                                           (member key org-memento-unique-properties))
                                         (org-entry-properties nil 'standard)
                                         :key #'car))))))))
         (plist (unless interactive
                  '(:immediate-finish t)))
         (org-capture-entry `("" ""
                              entry (file+function ,org-memento-file ,jump-fn)
                              ,template ,@plist)))
    (org-capture)))

(defun org-memento-schedule-block (start end-bound)
  "Schedule a block."
  (let ((event (org-memento-read-future-event start end-bound)))
    (pcase-exhaustive event
      ((pred org-memento-block-p)
       (save-current-buffer
         (org-memento-with-block-title (org-memento-title event)
           (org-end-of-meta-data)
           (when (looking-at org-logbook-drawer-re)
             (goto-char (match-end 0)))
           (atomic-change-group
             (if (looking-at org-ts-regexp)
                 (replace-match "")
               (insert "\n")
               (beginning-of-line 0))
             (let ((had-duration (org-memento-duration event))
                   (starting (org-memento-starting-time event))
                   (ending (org-memento-ending-time event)))
               (insert (org-memento--format-active-range
                        start
                        (when (and (not had-duration)
                                   ending)
                          (+ start (- ending starting)))))
               (beginning-of-line 1)
               (unless had-duration
                 (org-time-stamp nil)))))))
      (`(copy-entry ,marker . ,plist)
       (org-memento-add-event :title (plist-get plist :title)
                              :start (nth 0 (plist-get plist :time))
                              :end (nth 1 (plist-get plist :time))
                              :interactive t
                              :copy-from marker))
      ((pred stringp)
       (pcase-exhaustive (org-memento--read-time-span
                          (org-memento--format-active-range
                           start end-bound))
         (`(,start ,end)
          (org-memento-add-event :title event
                                 :start start
                                 :end end
                                 :interactive t)))))))

(cl-defun org-memento--event-template (&key title category start end interactive
                                            tags properties)
  (let ((started-past (time-less-p start (org-memento--current-time)))
        (ended-past (and end (time-less-p end (org-memento--current-time)))))
    (pcase-dolist (`(,key . ,value)
                   `(("memento_category"
                      . ,(unless (and category (string-empty-p category))
                           category))
                     ("memento_checkin_time"
                      . ,(when started-past
                           (format-time-string (org-time-stamp-format t t) start)))))
      (when value
        (if-let (cell (assoc key properties))
            (setcdr cell value)
          (setq properties (cons (cons key value) properties)))))
    (concat "* " (if ended-past "DONE " "")
            (or title (user-error "Title is missing"))
            (if tags
                (concat " " (org-make-tag-string tags))
              "")
            "\n"
            (if ended-past
                (concat "CLOSED: "
                        (format-time-string (org-time-stamp-format t t) end)
                        "\n")
              "")
            (if properties
                (concat ":PROPERTIES:\n"
                        (mapconcat (lambda (cell)
                                     (format ":%s: %s" (car cell) (cdr cell)))
                                   properties
                                   "\n")
                        "\n:END:\n"))
            (if (and start (not (and started-past ended-past)))
                (concat (org-memento--format-active-range start end) "\n")
              "")
            (if interactive
                "%?"
              ""))))

(cl-defun org-memento--away-event-template (&key title start end interactive)
  (let ((past (time-less-p start (org-memento--current-time))))
    (concat "* " (or title (user-error "Title is missing")) "\n"
            (if past
                (format ":LOGBOOK:\nCLOCK: %s--%s =>  %s\n:END:\n"
                        (format-time-string (org-time-stamp-format t t) start)
                        (format-time-string (org-time-stamp-format t t) end)
                        (org-duration-from-minutes (/ (- (float-time end)
                                                         (float-time start))
                                                      60)))
              "")
            (if past
                ""
              (concat (org-memento--format-active-range start end) "\n"))
            (if interactive
                "%?"
              ""))))

;;;; Exporting

;;;###autoload
(defun org-memento-export-groups-to-csv (file &optional append
                                              start-date end-date)
  "Export the data to CSV."
  (let ((groups (mapcar (lambda (section)
                          (oref section value))
                        (magit-region-sections)))
        (data (org-memento--collect-groups-1 start-date end-date)))
    (when groups
      (cl-delete-if `(lambda (x) (member (caar x) ',groups))
                    data))
    (cl-flet*
        ((escape-quote (text)
           (replace-regexp-in-string "\"" "\\\"" text))
         (escape-backslash (text)
           (replace-regexp-in-string "\\\\" "\\\\" text))
         (wrap-quote (text)
           (if (string-match-p "," text)
               (format "\"%s\"" text)
             text))
         (escape-cell (text)
           (thread-last
             text
             escape-backslash
             escape-quote
             wrap-quote))
         (write-record (cells)
           (insert (mapconcat #'escape-cell cells ",") "\n"))
         (format-groups (groups)
           (let ((i 0)
                 result)
             (dolist (group groups)
               (push (funcall (nth i org-memento-group-formatters) group)
                     result)
               (cl-incf i))
             (nreverse result))))
      (with-temp-buffer
        (unless append
          (write-record (append (mapcar (lambda (i)
                                          (format "Group %d" i))
                                        (number-sequence 1 (length org-memento-group-formatters)))
                                (list "Date")
                                (list "Title")
                                (list "Start")
                                (list "End"))))
        (dolist (record data)
          (write-record (append (format-groups (car record))
                                (list (nth 2 (cdr record))
                                      (nth 3 (cdr record))
                                      (format-time-string "%FT%R" (nth 0 (cdr record)))
                                      (format-time-string "%FT%R" (nth 1 (cdr record)))))))
        (write-region (point-min) (point-max) file append)))))

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
