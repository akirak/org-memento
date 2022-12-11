;;; org-memento.el --- Time blocking with Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.6") (taxy "0.10") (magit-section "3.3") (dash "2.19"))
;; Keywords: calendar
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
(require 'eieio)

(eval-when-compile
  ;; For pcase
  (require 'map))

(declare-function org-memento-policy-group-leaves "org-memento-policy")
(declare-function org-memento-policy-contexts "org-memento-policy")
(declare-function org-memento-policy-group-archived-p "org-memento-policy")

(declare-function org-element-headline-parser "org-element")
(declare-function org-day-of-week "org-clock")
(declare-function taxy-emptied "ext:taxy" t t)
(declare-function taxy-fill "ext:taxy" t t)
(declare-function taxy-sort-items "ext:taxy" t t)
(declare-function taxy-taxys "ext:taxy" t t)
(declare-function taxy-items "ext:taxy" t t)
(declare-function taxy-name "ext:taxy" t t)
(declare-function taxy-p "ext:taxy" t t)
(declare-function make-taxy "ext:taxy" t t)
(declare-function org-capture "org-capture")
(declare-function org-clocking-p "org-clock")
(declare-function thing-at-point-looking-at "thingatpt")
(declare-function org-notify "org-clock")
(declare-function org-memento-timeline "org-memento-timeline")
(declare-function org-memento-timeline-refresh "org-memento-timeline")
(declare-function org-memento-date--le "org-memento-date")
(declare-function org-link-store-props "ol")
(declare-function org-ql-search "ext:org-ql-search")
(declare-function org-memento-policy-maybe-load "org-memento-policy")
(declare-function org-clock-clock-in "org-clock")
(declare-function org-fold-show-subtree "org-fold")
(declare-function org-fold-show-all "org-fold")
(defvar org-super-agenda-properties-inherit)
(defvar org-capture-entry)
(defvar org-agenda-start-on-weekday)
(defvar org-archive-tag)
(defvar org-memento-timeline-date-range)
(defvar org-memento-timeline-span)
(defvar org-memento-timeline-dismissed-items)

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

(defcustom org-memento-todo-keyword-for-success "DONE"
  "Org todo keyword that indicates a successful performance."
  :type 'string)

(defcustom org-memento-state-trigger-alist nil
  "Alist of actions triggered by state change.

This can be used, for example, to create a block for retry of
a failed attempt."
  :type '(alist :key-type (string :tag "Todo keyword")
                :value-type
                (plist :tag "Create a new entry"
                       :options
                       (((const :todo-keyword)
                         (string :tag "Todo keyword"))))))

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

(defcustom org-memento-open-journal-hook
  '(org-fold-show-subtree)
  "Hook run after `org-memento-open-journal' visits an Org entry."
  :type 'hook)

(defcustom org-memento-update-hook nil
  "Hook run after a timeline event is added/changed/removed."
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

(defcustom org-memento-next-action-fallback #'org-memento-timeline
  "Command run if there is no suggested next action.

This is called in `org-memento-next-action'. The function will be
called interactively."
  :type 'function)

(define-widget 'org-memento-duration-type 'lazy
  "String representing a duration."
  :tag "Duration (h:mm)"
  :type 'string)

(defcustom org-memento-workhour-alist
  '(((1 2 3 4 5)
     :normal-checkin "9:30"
     :normal-duration "8:00"
     :normal-saving "1:00"))
  ""
  :type '(alist :key-type org-memento-days-of-week-type
                :value-type (plist :options
                                   (((const :normal-checkin)
                                     string)
                                    ((const :normal-duration)
                                     org-memento-duration-type)
                                    ((const :normal-saving)
                                     org-memento-duration-type)))))

(defcustom org-memento-margin-minutes 5
  "Default amount of gaps between blocks."
  :type 'number)

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

(defcustom org-memento-agenda-excluded-tags (list org-archive-tag)
  "List of tags used to determine entries to be excluded.

Entries with these tags will not be shown in some views in
`org-memento-timeline' and `org-memento-planner', even if they
are contained in `org-agenda-files' and have planning timestamps."
  :type '(repeat (string :tag "Org tag")))

(defcustom org-memento-group-taxonomy
  '((:read
     (lambda (element)
       (list (org-element-property :MEMENTO_CATEGORY element)))
     :format
     identity
     :template
     (lambda (category)
       (when category
         (list :properties `(("MEMENTO_CATEGORY" . ,category)))))))
  "List of group definitions.

Each entry in the list corresponds to a level in a group.

An entry is a plist that contain the following properties:

 * :read is a function that determines the group of an entry. It
is called at the heading of the entry with an org-element of the
entry as an argument. The returned value can be any sexp, and
values are compared using `equal'.

 * :format is a function that formats the value. It takes the
   value returned by the :read function as an argument and must
   return a string or nil. If it returns nil, the level is
   usually omitted in presentation.

 * :template is a function used to build an entry from a group
  value. It takes the group value as an argument and must return
  a plist. The plist can contain :properties which is an alist of
  strings which will become the properties of the Org entry and
  :tags which is a list of strings.

The results of :template functions are merged to build an input
to an entry template."
  :type '(repeat (plist :options
                        (((const :init)
                          function)
                         ((const :format)
                          function)
                         ((const :expand)
                          function)))))

(defcustom org-memento-unique-properties
  '("CATEGORY"
    "MEMENTO_CHECKIN_TIME"
    "ID")
  "List of Org entry properties that should not be copied.

Note that all property names should be upper-cased."
  :type '(repeat string))

(defcustom org-memento-timer-generator #'org-memento-default-timer-generator
  "Function that determines timings of countdown.

When a block starts, the function is called with an argument,
the number of minutes until the end time of the block.

The function should return a list of entries specifying when a
timer will be run.

Each spec can be one of the following types:

 * The number of minutes from the current time at which a timer
will be run.

 * (MINUTE . MESSAGE) where MINUTE is a number and MESSAGE is a
string to be used as the format of a notification message. The
message format can contain %s which will be replaced with the
headline of the block.

 * t, which is the exact ending time of the block.

If this variable is nil, no timer will be run when you start a
block."
  :type '(choice function (const nil)))

(defcustom org-memento-extend-on-end 'ask
  "Whether to ask if the user wants to extend the time.

If this variable is set to \\='ask, it will ask if the user wants
to extend the time for the current block when the time runs out."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Default (send a notification)" nil)))

(defcustom org-memento-display-timeline '(after-exit)
  "When to display the timeline."
  :type '(set (const :tag "After exiting a block" after-exit)))

;;;; Variables

(defvar org-memento-init-done nil)

(defvar org-memento-status-data nil)

(defvar org-memento-current-block nil
  "Headline of the current block.")

(defvar org-memento-current-category nil)

(defvar org-memento-zone-taxy nil
  "When set, use this as the zone list for the day.

It should be a `taxy' where all of its descendants satisfies the
following conditions:

- `name' is a cons cell of a string name and a plist.

- `predicate' is a function that take `org-memento-block' or
`org-memento-planning-item', or `org-memento-order' as an
argument.

To define such a taxy, one could use `org-memento-def-zone'
macro, though the user doesn't necessarily have to.")

(defvar org-memento-current-time nil
  "When non-nil, use this as the current time for testing.")

(defvar org-memento-block-timers nil)

(defvar org-memento-next-event-timer nil)

(defvar org-memento-daily-timer nil)

(defvar org-memento-idle-timer nil)

(defvar org-memento-block-idle-logging nil
  "Prevent from idle logging till next check-in.")

(defvar org-memento-title-string nil)

(defvar org-memento-group-cache nil)

(defvar org-memento-weekly-group-sums nil)

(defvar org-memento-requesting-timeline nil
  "Non-nil if the timeline is requested on update.")

;;;; Substs and small utility functions

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

(defun org-memento--zip (keys values)
  (let ((keys (copy-sequence keys))
        (values (copy-sequence values))
        items)
    (while (and keys values)
      (push (list (pop keys) (pop values))
            items))
    (nreverse items)))

(defmacro org-memento--plist-get (key)
  `(lambda (plist)
     (plist-get plist ,key)))

;;;; Generics and structs

(cl-defgeneric org-memento-headline-element (x)
  "Return the headline element of X.")

(cl-defgeneric org-memento-marker (x)
  "Return the headline marker of X.")

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

(cl-defgeneric org-memento-group-path (x)
  "Return the group path of X.")

(cl-defmethod org-memento-group-path ((x list))
  x)

;;;;; org-memento-block

(cl-defstruct org-memento-block
  "Object representing a day or a block in a journal file."
  headline active-ts hd-marker)

(cl-defmethod org-memento-headline-element ((x org-memento-block))
  (org-memento-block-headline x))

(cl-defmethod org-memento-marker ((x org-memento-block))
  (org-memento-block-hd-marker x))

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
                         (org-entry-get nil "MEMENTO_CHECKIN_TIME")))))
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

(cl-defmethod org-memento-group-path ((x org-memento-block))
  (with-current-buffer (org-memento--buffer)
    (org-memento--get-group (org-memento-block-headline x))))

;;;;; org-memento-org-event

(cl-defstruct org-memento-org-event
  "Object representing an Org entry with an active timestamp."
  marker active-ts ending-time got-ending-time margin-secs title)

(cl-defmethod org-memento-title ((x org-memento-org-event))
  (or (org-memento-org-event-title x)
      (save-match-data
        (save-current-buffer
          (org-with-point-at (org-memento-org-event-marker x)
            (org-back-to-heading)
            (when (looking-at org-complex-heading-regexp)
              (setf (org-memento-org-event-title x)
                    (match-string-no-properties 4))))))))

(cl-defmethod org-memento-marker ((x org-memento-org-event))
  (org-memento-org-event-marker x))

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

;;;;; org-memento-planning-item

(cl-defstruct org-memento-planning-item hd-marker id heading effort
              time-of-day-ts)

(cl-defmethod org-memento-duration ((x org-memento-planning-item))
  (when-let (effort (org-memento-planning-item-effort x))
    (org-duration-to-minutes effort)))

(cl-defmethod org-memento-starting-time ((x org-memento-planning-item))
  (when-let (ts (org-memento-planning-item-time-of-day-ts x))
    (float-time (org-timestamp-to-time ts))))

;;;;; org-memento-order

(cl-defstruct org-memento-order
  group title template sample-marker duration
  no-earlier-than no-later-than previous-activities)

(cl-defmethod org-memento-title ((x org-memento-order))
  (org-memento-order-title x))

(cl-defmethod org-memento-duration ((x org-memento-order))
  (org-memento-order-duration x))

(cl-defmethod org-memento-group-path ((x org-memento-order))
  (org-memento-order-group x))

(cl-defmethod org-memento-starting-time ((_ org-memento-order))
  nil)

;;;;; org-memento-group-data

(cl-defstruct org-memento-group-data group-path value tag)
(cl-defmethod org-memento-group-path ((x org-memento-group-data))
  (org-memento-group-data-group-path x))

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
                                                       (list (format-time-string "%F"
                                                                                 ',date))
                                                       (number (format-time-string "%F"
                                                                                   ,date))
                                                       (string ,date))))
                               nil t)
        (org-back-to-heading)
        ,@progn))))

(defmacro org-memento-with-block-title (title &rest progn)
  (declare (indent 1))
  `(org-memento-with-today-entry
    (org-narrow-to-subtree)
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   ,title)
                           nil t)
        (progn
          (org-back-to-heading)
          ,@progn)
      (error "Failed to find a heading %s" ,title))))

(defmacro org-memento-with-current-block (&rest progn)
  (declare (indent 0))
  `(org-memento-with-today-entry
    (org-narrow-to-subtree)
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   org-memento-current-block)
                           nil t)
        (progn
          (org-back-to-heading)
          ,@progn)
      (setq org-memento-current-block nil)
      (error "Failed to find a heading for the current block"))))

;; This is not a macro but serves a similar purpose, so it's put here.
(defun org-memento-map-past-blocks-1 (fn &optional
                                         start-date-string
                                         end-date-string)
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
               (while (and (< (point) bound)
                           (re-search-forward org-complex-heading-regexp bound t))
                 (when (and (equal (match-string 1) "**")
                            (not (or (equal (match-string 4) org-memento-idle-heading)
                                     (string-prefix-p org-comment-string
                                                      (match-string 4))))
                            (org-entry-is-done-p))
                   (goto-char (match-beginning 0))
                   (when-let (x (funcall fn date))
                     (push x result)))
                 (org-end-of-subtree))))))
       result))))

(defun org-memento-map-past-blocks (fn &optional
                                       start-date-string
                                       end-date-string)
  "Call a function on every block entry performed in the past.

The function takes two arguments: the date string and an
`org-memento-block' struct."
  (org-memento-map-past-blocks-1
   `(lambda (date)
      (when-let* ((block (org-memento-block-entry))
                  (started (org-memento-started-time block))
                  (ended (org-memento-ended-time block)))
        (funcall ',fn date block)))
   start-date-string
   end-date-string))

(defun org-memento-map-away-events (fn)
  (with-current-buffer (org-memento--buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (result
           (regexp (format org-complex-heading-regexp-format
                           org-memento-idle-heading)))
       (while (re-search-forward regexp nil t)
         (let ((bound (save-excursion
                        (org-end-of-subtree))))
           (while (and (< (point) bound)
                       (re-search-forward org-complex-heading-regexp bound t))
             (beginning-of-line)
             (push (save-excursion (funcall fn)) result)
             (org-end-of-subtree))))
       result))))

(defmacro org-memento-with-group-entry (group &rest body)
  "Evaluate the body with point at some group entry."
  (declare (indent 1))
  `(if-let (marker (with-current-buffer (org-memento--buffer)
                     (org-with-wide-buffer
                      (or (seq-some (lambda (block)
                                      (goto-char (org-memento-block-hd-marker block))
                                      (when (equal (save-excursion
                                                     (org-memento--get-group
                                                      (org-memento-block-headline block)))
                                                   ,group)
                                        (point-marker)))
                                    (org-memento--blocks))
                          (when-let (olp (gethash ,group org-memento-group-cache))
                            (org-find-olp olp t))))))
       (save-current-buffer
         (org-with-point-at marker
           ,@body))
     (with-temp-buffer
       (let ((org-inhibit-startup t)
             ;; Don't load modules.
             (org-modules-loaded t))
         (delay-mode-hooks (org-mode)))
       (insert (apply #'org-memento--event-template :title ""
                      (org-memento--template-group ,group)))
       (goto-char (point-min))
       ,@body)))

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
    (message "Org-Memento mode started."))
  (if (bound-and-true-p org-memento-mode)
      (progn
        (add-hook 'org-clock-in-hook #'org-memento-log-update)
        (add-hook 'org-clock-out-hook #'org-memento-log-update))
    (remove-hook 'org-clock-in-hook #'org-memento-log-update)
    (remove-hook 'org-clock-out-hook #'org-memento-log-update)))

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

;;;; Minor mode for the journal file

(defvar org-memento-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-clock-in] #'org-memento-start-at-point)
    map))

(define-minor-mode org-memento-file-mode
  "Minor mode for the journal file of Org Memento.

This mode is primarily intended for remapping commands that
should not be run inside the journal file."
  :lighter " MmtFile")

(defun org-memento--ensure-file-mode ()
  (unless org-memento-file-mode
    (org-memento-file-mode t)))

(defun org-memento-start-at-point ()
  "Start the block at point."
  (interactive)
  (unless (file-equal-p (buffer-file-name (org-base-buffer (current-buffer)))
                        org-memento-file)
    (user-error "You cannot run this command outside of the journal file"))
  (when org-memento-current-block
    (user-error "Already started"))
  (pcase (org-get-outline-path t)
    (`(,date ,heading)
     (unless (equal date (org-memento--today-string
                          (decode-time (org-memento--current-time))))
       (user-error "Not on an entry for today"))
     (when (equal heading org-memento-idle-heading)
       (user-error "You cannot run this on an idle entry"))
     (when (org-entry-is-done-p)
       (user-error "The entry is already closed"))
     (org-memento-start-block heading))
    (_
     (user-error "Not on a block entry"))))

;;;; Commands

;;;###autoload
(defun org-memento-next-action (&optional arg)
  "Pick an action depending on the current status."
  (interactive "P")
  (org-memento-status)
  (if-let (block (org-memento--current-block))
      (progn
        (when (org-clocking-p)
          (user-error "Before you finish the block, please finish your clock."))
        (when (yes-or-no-p "Finish the current block?")
          (org-memento-finish-block arg)))
    (if (org-clocking-p)
        (org-memento-start-block
         (org-memento-read-block "There is a running clock. Choose a block: "))
      ;; It is hard to decide on the next action. `org-memento-timeline' is
      ;; supposed to properly address the issue. I am not sure if it is possible
      ;; to make the decision deterministically.
      (let* ((now (float-time (org-memento--current-time)))
             (unclosed-blocks (thread-last
                                (org-memento--blocks)
                                (seq-filter #'org-memento-block-not-closed-p)))
             (late-block (seq-find `(lambda (block)
                                      (and (org-memento-starting-time block)
                                           (< (org-memento-starting-time block)
                                              ,now)))
                                   unclosed-blocks))
             (upnext-event (org-memento--next-event now))
             (next-event-time (when upnext-event (org-memento-starting-time upnext-event)))
             (checkout-time (org-memento-ending-time (org-memento-today-as-block))))
        (if (and checkout-time
                 (> now checkout-time))
            (pcase-exhaustive (read-multiple-choice
                               (format-time-string "You are supposed to have already\
 checked out at %R."
                                                   checkout-time)
                               '((?x "Extend the time" nil org-memento-set-checkout-time)
                                 (?m "Moderate the events" nil org-memento-timeline)
                                 (?o "Check out now" nil org-memento-checkout-from-day)))
              (`(,_ ,_ ,_ ,command)
               (call-interactively command)))
          (cond
           ;; If there is an upcoming event that should be started within 10
           ;; minutes, display it.
           ((and next-event-time
                 (> next-event-time now)
                 (< (- next-event-time now) (* 10 60)))
            (if (org-memento-org-event-p upnext-event)
                (org-goto-marker-or-bmk (org-memento-marker upnext-event))
              (if (yes-or-no-p (format "Start \"%s\" right now? "
                                       (org-memento-title upnext-event)))
                  (org-memento-start-block (org-memento-title upnext-event))
                (with-current-buffer (org-memento--buffer)
                  (goto-char (org-memento-marker upnext-event))
                  (org-narrow-to-subtree)
                  (run-hooks 'org-memento-open-journal-hook)))))
           (late-block
            (if (and (or (org-memento-duration late-block)
                         (org-memento-ending-time late-block))
                     (< (+ now (if-let (duration (org-memento-duration late-block))
                                   (* 60 duration)
                                 (- (org-memento-ending-time late-block)
                                    (org-memento-starting-time late-block))))
                        (or next-event-time
                            (org-memento-ending-time (org-memento-today-as-block))
                            (error "No checkout time is set"))))
                (if (yes-or-no-p (format "Start \"%s\" right now? "
                                         (org-memento-title late-block)))
                    (org-memento-start-block (org-memento-title late-block))
                  (org-memento-moderate))
              (org-memento-moderate "There is a block you should have started, \
and starting it will affect execution of other events. Please manually moderate it.")))
           ;; If there is a block that have no starting time, start working on
           ;; it.
           ((seq-filter (lambda (block)
                          (not (org-memento-starting-time block)))
                        unclosed-blocks)
            (call-interactively #'org-memento-start-block))
           ((and upnext-event
                 (not (org-memento-org-event-p upnext-event))
                 (yes-or-no-p (format "Forward to the next event \"%s\"? "
                                      (org-memento-title upnext-event))))
            (org-memento-start-block (org-memento-title upnext-event)))
           (t
            (let ((blank-time (/ (- (or (when next-event-time
                                          (- next-event-time (* 60 org-memento-margin-minutes)))
                                        checkout-time)
                                    now)
                                 60)))
              (unless (org-memento-pick-next-action blank-time)
                (message "Nothing to do. Running the fallback action")
                (call-interactively org-memento-next-action-fallback))))))))))

(defun org-memento-pick-next-action (max-duration)
  "Pick an action the user can do within a certain duration."
  (interactive "sDuration: ")
  (let ((max-duration (cl-etypecase max-duration
                        (string (org-duration-to-minutes max-duration))
                        (number max-duration))))
    (cl-flet
        ((duration-ok-p (block)
           (<= (or (org-memento-duration block)
                   (/ (- (org-memento-ending-time block)
                         (org-memento-starting-time block))
                      60))
               max-duration)))
      (when-let* ((blocks (thread-last
                            (org-memento--blocks)
                            (seq-filter #'org-memento-block-not-closed-p)
                            (seq-filter #'duration-ok-p)))
                  (block-or-event (org-memento-read-block
                                   (format "Pick an action you can finish within %s: "
                                           (org-memento--format-duration max-duration))
                                   blocks
                                   :return-struct t
                                   :extra-actions '(("Do nothing or something else" . nil)))))
        (pcase block-or-event
          (`nil
           nil)
          ((cl-type org-memento-block)
           (org-memento-start-block (org-memento-title block-or-event))
           t)
          ((cl-type org-memento-org-event)
           (org-clock-clock-in (list (org-memento-marker block-or-event)))
           t))))))

;;;###autoload
(defun org-memento-moderate (&optional message-text)
  "Moderate the schedule.

At present, it runs `org-memento-timeline'."
  (interactive)
  (when message-text
    (message message-text))
  (call-interactively #'org-memento-timeline))

;;;###autoload
(defun org-memento-start-block (title)
  "Start working on a time block you have planned."
  (interactive (list (org-memento-read-block "Start a block: ")))
  (when org-memento-current-block
    (error "There is a running block: %s" org-memento-current-block))
  (org-memento-with-today-entry
   (org-narrow-to-subtree)
   (unless (re-search-forward (format org-complex-heading-regexp-format title)
                              nil t)
     (save-excursion
       (org-memento--add-immediate-block title))
     (re-search-forward (format org-complex-heading-regexp-format title)
                        nil t))
   (when (org-memento--maybe-check-in :adjust t)
     (org-memento--save-buffer)))
  (setq org-memento-current-block title)
  (org-memento-setup-daily-timer)
  (org-memento-status)
  (run-hooks 'org-memento-block-start-hook)
  (org-memento--setup-block-timers)
  (org-memento-log-update))

(defun org-memento-finish-block (&optional arg)
  "Finish the current block."
  (interactive "P")
  (unless org-memento-current-block
    (user-error "No current block"))
  (pcase-exhaustive (or arg org-memento-todo-keyword-for-success)
    ('(4)
     (org-memento-finish-block (completing-read "Finish the block: "
                                                (with-current-buffer (org-memento--buffer)
                                                  org-done-keywords)
                                                nil t)))
    ((and (pred stringp)
          keyword)
     (run-hooks 'org-memento-block-before-exit-hook)
     (org-memento-with-current-block
       (org-todo keyword)
       (org-memento-after-state-change keyword)
       (org-memento--save-buffer))
     (setq org-memento-current-block nil)
     (org-memento--status)
     (org-memento--cancel-block-timers)
     (org-memento--add-next-event-timer)
     (run-hooks 'org-memento-block-exit-hook)
     (when (memq 'after-exit org-memento-display-timeline)
       (setq org-memento-requesting-timeline t)
       (add-hook 'org-memento-update-hook #'org-memento--oneshot-timeline))
     (org-memento-log-update))))

;;;###autoload
(defun org-memento-open-journal (&optional arg)
  "Open the current block, the next block, or the daily entry."
  (interactive "P")
  (cl-flet
      ((show-block (title &optional narrow)
         (with-current-buffer (org-memento--buffer)
           (org-memento--ensure-file-mode)
           (widen)
           (org-memento--find-today)
           (org-narrow-to-subtree)
           (pop-to-buffer (current-buffer))
           (org-fold-show-entry)
           (with-demoted-errors "The heading for the current block does not exist. \
Possibly renamed? %s"
             (re-search-forward (format org-complex-heading-regexp-format title)))
           (org-back-to-heading)
           (when narrow
             (org-narrow-to-subtree))
           (run-hooks 'org-memento-open-journal-hook))))
    (cond
     (org-memento-current-block
      (condition-case _
          (show-block org-memento-current-block t)
        (error (progn
                 (setq org-memento-current-block nil)
                 (org-memento-open-journal arg)))))
     (t
      (org-memento--status)
      (if-let (block (thread-last
                       (org-memento--blocks)
                       (seq-filter #'org-memento-block-not-closed-p)
                       (seq-filter #'org-memento-starting-time)
                       (seq-sort-by #'org-memento-starting-time #'<)
                       (car)))
          (show-block (org-memento-title block))
        (org-memento-open-today)
        (run-hooks 'org-memento-open-journal-hook))))))

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
    (org-memento--ensure-file-mode)
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
(defun org-memento-checkout-from-day (&optional arg)
  "Run this command when you finish all your work on the day.

With a universal argument, you can specify the time of check out."
  (interactive "P")
  (when org-memento-current-block
    (call-interactively #'org-memento-finish-block))
  (org-memento-with-today-entry
   ;; If org-read-date is aborted, the entire checkout command will be aborted.
   (let ((time (when arg (org-read-date t t))))
     (org-todo 'done)
     (when arg
       (org-add-planning-info 'closed time)))
   (org-memento--save-buffer)
   (setq org-memento-block-idle-logging t)
   (run-hooks 'org-memento-checkout-hook)))

(defun org-memento-set-checkout-time ()
  "Extend the checkout time of the day."
  (interactive)
  (org-memento-with-today-entry
   (org-end-of-meta-data t)
   (let* ((org-read-date-prefer-future nil)
          (orig-ts (org-element-timestamp-parser))
          (string (org-read-date t nil nil "Checkout time"
                                 (org-memento-ending-time (org-memento-today-as-block)))))
     (delete-region (org-element-property :begin orig-ts)
                    (org-element-property :end orig-ts))
     (insert (org-memento--format-timestamp
              (org-timestamp-to-time orig-ts)
              (encode-time (parse-time-string string)))))))

(cl-defun org-memento-adjust-time (&key allow-edit-clock new-start)
  "Adjust the active timestamp of the entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-end-of-meta-data)
    (let ((has-drawer (looking-at org-logbook-drawer-re)))
      (if (and has-drawer
               allow-edit-clock
               (save-match-data
                 (re-search-forward org-clock-line-re (match-end 0) t)))
          (org-memento--edit-clock)
        (when has-drawer
          (goto-char (match-end 0)))
        (org-memento--edit-timestamp :new-start new-start)))))

(defun org-memento--edit-clock ()
  "Edit the clock entry at point.

The point must be after a \"CLOCK:\" string."
  (let* ((orig-clock (org-element-clock-parser (pos-eol)))
         (orig-ts (org-element-property :value orig-clock)))
    (pcase-exhaustive (org-memento--read-time-span
                       (org-memento--format-timestamp
                        (org-timestamp-to-time orig-ts)
                        (org-timestamp-to-time orig-ts t))
                       (org-memento--start-of-day
                        (decode-time
                         (org-timestamp-to-time orig-ts))))
      (`(,start ,end)
       (goto-char (org-element-property :begin orig-ts))
       (delete-region (org-element-property :begin orig-ts)
                      (org-element-property :end orig-ts))
       (insert (org-memento--format-timestamp start nil t)
               "--"
               (org-memento--format-timestamp end nil t))
       (org-clock-update-time-maybe)))))

(cl-defun org-memento--edit-timestamp (&key new-start)
  "Edit the timestamp at point or insert a new one."
  (let* ((had-ts (looking-at org-ts-regexp))
         (orig-ts (when had-ts
                    (org-timestamp-from-string (match-string 0))))
         (duration-secs (when (and orig-ts
                                   (eq 'active-range (org-element-property :type orig-ts)))
                          (- (float-time (org-timestamp-to-time orig-ts t))
                             (float-time (org-timestamp-to-time orig-ts))))))
    (pcase-exhaustive (if new-start
                          (org-memento--read-time-span
                           (org-memento--format-timestamp
                            new-start
                            (when duration-secs
                              (+ new-start duration-secs)))
                           (encode-time
                            (org-memento--start-of-day
                             (decode-time new-start))))
                        (org-memento--read-time-span
                         (when orig-ts
                           (org-memento--format-timestamp
                            (org-timestamp-to-time orig-ts)
                            (org-timestamp-to-time orig-ts t)))
                         (if orig-ts
                             (encode-time
                              (org-memento--start-of-day
                               (decode-time
                                (org-timestamp-to-time orig-ts))))
                           (org-memento--current-time))))
      (`(,start ,end)
       (when (looking-at org-ts-regexp)
         (replace-match ""))
       (insert (org-memento--format-timestamp
                start
                (or end
                    (when duration-secs
                      (time-add start duration-secs)))))
       (unless had-ts (insert "\n"))))))

;;;; Timers and notifications

(defun org-memento--setup-block-timers ()
  "Start timers for running notifications."
  (org-memento--cancel-block-timers)
  (when org-memento-timer-generator
    (let* ((time (org-memento-ending-time (org-memento--current-block)))
           (now (float-time (org-memento--current-time)))
           (minutes (when time
                      (/ (- time now) 60))))
      (when minutes
        (dolist (spec (funcall org-memento-timer-generator minutes))
          (push (pcase spec
                  ((and (pred numberp)
                        (guard (> spec 0)))
                   (run-with-timer (* spec 60) nil
                                   `(lambda (_)
                                      (org-memento--notify-end
                                       ,(unless (< (- minutes spec) 1)
                                          (- minutes spec))))))
                  (`t
                   (run-with-timer (* minutes 60) nil #'org-memento--notify-end))
                  (`(,diff . ,fmt)
                   (run-with-timer (* diff 60) nil
                                   `(lambda (_)
                                      (org-notify
                                       (format ,fmt org-memento-current-block)))))
                  (_
                   (run-with-timer (* spec 60) nil #'org-memento--notify-end)))
                org-memento-block-timers))))))

(defun org-memento--notify-end (&optional minutes)
  (if (and (not minutes)
           org-memento-extend-on-end)
      (if (yes-or-no-p (format "The time for %s has reached an end. Extend it?"
                               org-memento-current-block))
          (org-memento-extend-current-block)
        (org-memento-finish-block '(4)))
    (org-notify (if minutes
                    (format "%s is about to end in %d minutes."
                            org-memento-current-block
                            minutes)
                  (format "%s must end now." org-memento-current-block)))))

(defun org-memento-extend-current-block ()
  "Extend the end time of the current block."
  (interactive)
  (unless org-memento-current-block
    (user-error "No current block"))
  (let* ((now (float-time (org-memento--current-time)))
         (input (org-memento--read-duration
                 "How long do you want to extend from now? "
                 :check-next-event t
                 :default 30
                 :now now))
         (new-end-time (+ now (* 60 (org-duration-to-minutes input)))))
    (org-memento-with-current-block
      (org-back-to-heading)
      (org-end-of-meta-data t)
      (let* ((had-ts (looking-at org-ts-regexp))
             (start (save-match-data
                      (thread-last
                        (org-entry-get nil "MEMENTO_CHECKIN_TIME")
                        (org-timestamp-from-string)
                        (org-timestamp-to-time)))))
        (when had-ts
          (replace-match ""))
        (unless (bolp) (insert "\n"))
        (insert (org-memento--format-timestamp start new-end-time))
        (unless had-ts (insert "\n"))))
    (org-memento--cancel-next-event-timer)
    (org-memento--status)
    (org-memento--setup-block-timers)
    (org-memento-log-update)))

(defun org-memento--cancel-block-timers ()
  (mapc #'cancel-timer org-memento-block-timers)
  (setq org-memento-block-timers nil))

(defun org-memento-default-timer-generator (duration)
  (if (>= duration 15)
      (list (- duration 5) t)
    (list t)))

(defun org-memento--cancel-next-event-timer ()
  (when (and org-memento-next-event-timer
             (timerp org-memento-next-event-timer))
    (cancel-timer org-memento-next-event-timer)
    (setq org-memento-next-event-timer nil)))

(defun org-memento--update-next-event-timer (secs func)
  (declare (indent 1))
  (when (and org-memento-next-event-timer
             (timerp org-memento-next-event-timer))
    (cancel-timer org-memento-next-event-timer))
  (setq org-memento-next-event-timer
        (run-with-timer secs nil func)))

(defun org-memento--add-next-event-timer (&optional now)
  "Add a timer to notify the start of the next event."
  ;; Don't add a timer is there is a running block. Add it after finishing it.
  ;; The timers should be updated as any timeline event happens.
  (unless org-memento-current-block
    (if-let* ((now (or now (float-time (org-memento--current-time))))
              (event (org-memento--next-event now)))
        (let* ((start (org-memento-starting-time event))
               (title (org-memento-title event))
               (margin org-memento-margin-minutes)
               (margined-time (- start (* margin 60))))
          (if (org-memento-org-event-p event)
              (if (> margined-time now)
                  (org-memento--update-next-event-timer (- margined-time now)
                    `(lambda ()
                       (org-notify (format "Starting in %d minutes: %s"
                                           ,margin ,title))
                       (org-memento--add-next-event-timer ,start)))
                (org-memento--update-next-event-timer (- start now)
                  `(lambda ()
                     (org-notify (format "Starting: %s" ,title))
                     (org-memento--add-next-event-timer ,start))))
            (org-memento--update-next-event-timer (- start now)
              `(lambda ()
                 (if (yes-or-no-p (message "Start %s?" ,title))
                     (org-memento-start-block ,title)
                   (org-memento--add-next-event-timer ,start))))))
      ;; Cancel an existing timer, if any.
      (org-memento--cancel-next-event-timer))))

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
    (org-memento-finish-block '(4)))
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
        (with-current-buffer (find-file-noselect org-memento-file)
          (org-memento-file-mode t)
          (current-buffer)))))

(defun org-memento--save-buffer ()
  (let ((make-backup-files nil)
        (version-control nil))
    (save-buffer)))

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
  (when-let (string (org-entry-get nil "MEMENTO_CHECKIN_TIME"))
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

(defun org-memento--update-cache-1 (&optional force)
  "Cache information on the past activities."
  (when (or force
            (not org-memento-init-done))
    (save-excursion
      (org-save-outline-visibility t
        (org-fold-show-all)
        ;; `org-element-cache-reset' is available in the latest version of Org,
        ;; but not in its older versions.
        (when (fboundp 'org-element-cache-reset)
          (org-element-cache-reset))
        (org-memento--cache-groups)
        (org-memento--update-weekly-group-sums)
        (setq org-memento-init-done t)))))

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
                                      (org-entry-get nil "MEMENTO_CATEGORY"))))
  (org-entry-put nil "MEMENTO_CATEGORY" category))

(cl-defun org-memento--maybe-check-in (&key adjust)
  "If the entry has no check-in time, record the current time.

This function can be called both on a daily entry (at level 1)
and on a time block entry (at level 2).

The function returns non-nil if the check-in is done."
  (unless (org-entry-get nil "MEMENTO_CHECKIN_TIME")
    (let ((now (org-memento--current-time)))
      (org-entry-put nil "MEMENTO_CHECKIN_TIME" (org-memento--format-timestamp
                                                 now nil 'inactive))
      (when adjust
        (org-memento--move-active-ts now)))
    t))

(defun org-memento--move-active-ts (start-time)
  "Adjust the end time of an active ts according to START-TIME."
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (when-let (duration (when (looking-at org-ts-regexp)
                        (save-match-data
                          (org-memento--duration-secs-ts-at-point))))
    (replace-match (org-memento--format-active-range
                    start-time (time-add start-time duration)))))

(defun org-memento--maybe-checkin-to-day ()
  "Check in to the daily entry, if it is not done yet."
  (when org-memento-idle-timer
    (cancel-timer org-memento-idle-timer))
  (setq org-memento-block-idle-logging nil)
  (when (org-memento--maybe-check-in)
    (org-back-to-heading)
    (org-memento--insert-checking-out-time)
    (org-memento--save-buffer)
    (org-end-of-meta-data t)
    (when (looking-at org-ts-regexp)
      (beginning-of-line 2)
      (insert-char ?\n)
      (beginning-of-line 0))
    (org-memento--update-cache-1 t)
    (when (boundp 'org-memento-timeline-dismissed-items)
      (setq org-memento-timeline-dismissed-items nil))
    (org-memento--ensure-file-mode)
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

;;;###autoload
(defun org-memento-status (&optional check-in interactive)
  "Update the status."
  (interactive (list nil t))
  (org-memento--update-cache-1)
  (setq org-memento-status-data (org-memento--block-data
                                 (or check-in
                                     (called-interactively-p t))))
  (unless (catch 'cancel
            (unless (and org-memento-current-block
                         (seq-find (lambda (block)
                                     (and (equal (org-memento-title block)
                                                 org-memento-current-block)
                                          (org-memento-started-time block)
                                          (not (org-memento-ended-time block))))
                                   (org-memento--blocks)))
              (if-let (right-block (seq-some (lambda (block)
                                               (when (and (org-memento-started-time block)
                                                          (not (org-memento-ended-time
                                                                block)))
                                                 (org-memento-title block)))
                                             (org-memento--blocks)))
                  (progn
                    ;; Call `org-memento-start-block' to run necessary hooks.
                    (org-memento-start-block right-block)
                    ;; `org-memento-start-block' updates the status, so avoid
                    ;; rerun of the status hook.
                    (throw 'cancel t))
                (setq org-memento-current-block nil))))
    (unless org-memento-weekly-group-sums
      (org-memento--update-weekly-group-sums))
    (let* ((block (org-memento--current-block))
           (category (when block (org-memento-block-category block))))
      (setq org-memento-current-category category))
    (run-hooks 'org-memento-status-hook)
    (when interactive
      (org-memento--print-status))))

(defun org-memento--print-status ()
  "Briefly print the current status."
  (if-let (block (org-memento--current-block))
      (let* ((now (float-time))
             (started (org-memento-started-time block))
             (ending (org-memento-ending-time block))
             (duration (or (org-memento-duration block)
                           (and ending
                                (/ (- ending started) 60))))
             (group (save-current-buffer
                      (org-with-point-at (org-memento-block-hd-marker block)
                        (org-memento--get-group (org-memento-block-headline block))))))
        (message (concat (propertize (org-memento-title block)
                                     'face 'font-lock-string-face)
                         (if group
                             (propertize (format " (%s)" (org-memento--format-group group))
                                         'face 'font-lock-comment-face)
                           "")
                         (propertize ":" 'face 'font-lock-comment-face)
                         (if started
                             (format-time-string " %R-" started)
                           "")
                         (if ending
                             (format-time-string "%R" ending)
                           "")
                         (if (and duration started)
                             (format-spec " %e/%a"
                                          `((?e . ,(org-duration-from-minutes
                                                    (/ (- now started) 60)))
                                            (?a . ,(org-duration-from-minutes
                                                    duration))))
                           ""))))
    (if-let (block (org-memento-today-as-block))
        (let* ((now (float-time))
               (started (org-memento-started-time block))
               (ending (org-memento-ending-time block))
               (event (org-memento--next-event now)))
          (message (concat (format-time-string "%R-" started)
                           (if ending
                               (format-spec "%e, remaining %r"
                                            `((?e . ,(format-time-string "%R" ending))
                                              (?r . ,(org-duration-from-minutes
                                                      (/ (- ending now) 60)))))
                             "")
                           (if event
                               (format-spec ", next event in %d (%t)"
                                            `((?d . ,(org-duration-from-minutes
                                                      (/ (- (org-memento-starting-time event)
                                                            now)
                                                         60)))
                                              (?t . ,(org-memento-title event))))))))
      (message "Not checking in"))))

(defun org-memento--status ()
  "Only update `org-memento-status-data'."
  (interactive)
  (org-memento--update-cache-1)
  (setq org-memento-status-data (org-memento--block-data)))

(defun org-memento-log-update ()
  "Notify updates of the status."
  (add-hook 'post-command-hook #'org-memento--update))

(defun org-memento--update ()
  (remove-hook 'post-command-hook #'org-memento--update)
  (run-hooks 'org-memento-update-hook))

(defun org-memento--oneshot-timeline ()
  (remove-hook 'org-memento-update-hook #'org-memento--oneshot-timeline)
  (setq org-memento-requesting-timeline nil)
  (let ((today (org-memento--today-string (decode-time))))
    (org-memento-timeline today today :span 'day
                          :no-update-status t)))

(defun org-memento--block-data (&optional check-in)
  ;; The first item will always be the day itself.
  (org-memento-with-today-entry
   (when check-in
     (org-memento--maybe-checkin-to-day))
   (org-narrow-to-subtree)
   (org-save-outline-visibility t
     (org-fold-show-subtree)
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
                             (org-end-of-subtree)))))))

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

(defun org-memento--next-away-event (&optional bound-time now)
  "Return idle events on the current day."
  (org-memento-with-today-entry
   (when (re-search-forward (format org-complex-heading-regexp-format
                                    org-memento-idle-heading)
                            (save-excursion
                              (org-end-of-subtree))
                            t)
     (let ((now (or now
                    (float-time (org-memento--current-time))))
           (bound (save-excursion
                    (org-end-of-subtree)))
           event)
       (while (re-search-forward org-ts-regexp bound t)
         (let* ((ts (org-timestamp-from-string (match-string 0)))
                (time (when (org-timestamp-has-time-p ts)
                        (float-time (org-timestamp-to-time ts)))))
           (when (and time
                      (< now time)
                      (or (not bound-time)
                          (< time bound-time)))
             (setq bound-time time)
             (setq event (save-excursion
                           (org-back-to-heading)
                           (make-org-memento-org-event
                            :marker (point-marker)
                            :active-ts ts
                            :title (when (looking-at org-complex-heading-regexp)
                                     (match-string-no-properties 4))))))))
       event))))

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

;;;; Workflow

(defun org-memento-after-state-change (keyword)
  "Run post-process action on state change in the journal.

This function creates a follow-up task according to the value of
`org-memento-state-trigger-alist'."
  (pcase-exhaustive (assoc keyword org-memento-state-trigger-alist)
    (`nil)
    ((and `(,_ . ,plist)
          (guard (plistp plist)))
     (let* ((heading (and (looking-at org-complex-heading-regexp)
                          (match-string-no-properties 4)))
            (heading (read-from-minibuffer "Title of the duplicate block: "
                                           heading nil nil nil nil t))
            (tags (org-get-tags nil 'local))
            (todo (plist-get plist :todo-keyword))
            (body (org-memento--duplicate-body))
            (date (org-read-date nil nil nil
                                 (format "Date on which you work on \"%s\""
                                         heading)))
            (block (org-memento--current-block))
            (now (float-time (org-memento--current-time)))
            (ending-time (org-memento-ending-time block))
            (remaining-duration (when ending-time
                                  (max (/ (- ending-time now) 60)
                                       0)))
            (duration (org-memento--read-duration "Duration: "
                                                  :default remaining-duration))
            (properties (cons (cons "Effort" duration)
                              (cl-remove-if
                               (lambda (key)
                                 (member key (cons "EFFORT"
                                                   org-memento-unique-properties)))
                               (org-entry-properties nil 'standard)
                               :key #'car))))
       (with-current-buffer (org-memento--buffer)
         (org-with-wide-buffer
          (org-memento--goto-date date)
          (org-end-of-subtree)
          (unless (bolp)
            (newline))
          (insert "** " (if todo (concat todo " ") "") heading
                  (if tags
                      (concat " " (org-make-tag-string tags))
                    "")
                  "\n"
                  (if properties
                      (concat ":PROPERTIES:\n"
                              (mapconcat (lambda (cell)
                                           (format ":%s: %s" (car cell) (cdr cell)))
                                         properties
                                         "\n")
                              "\n:END:\n")
                    "")
                  body)
          (unless (bolp)
            (newline))))))))

(defun org-memento--duplicate-body ()
  "Return the body of a copy of the current entry."
  (save-excursion
    (let (texts)
      (when (looking-at-p org-heading-regexp)
        (forward-line))
      (while (or (looking-at-p org-planning-line-re)
                 (looking-at org-drawer-regexp))
        (if (and (looking-at-p (rx (* blank) ":"))
                 (not (member (match-string 1)
                              ;; Properties should be scanned separately
                              '("LOGBOOK" "PROPERTIES"))))
            (let ((pos (point)))
              (org-end-of-meta-data)
              (push (buffer-substring-no-properties pos (point))
                    texts))
          (org-end-of-meta-data)))
      (when (looking-at org-ts-regexp)
        (goto-char (match-end 0))
        (when (and (eolp) (not (bolp)))
          (beginning-of-line 2)))
      (string-join (nreverse (cons (if (looking-at-p org-heading-regexp)
                                       ""
                                     (string-chop-newline
                                      (buffer-substring-no-properties
                                       (point) (org-entry-end-position))))
                                   texts))))))

;;;; Agenda files

;;;###autoload
(defun org-memento-agenda-files ()
  "Return a list of Org agenda files for the current block."
  (org-memento-with-block-title (or org-memento-current-block
                                    (error "Currently no block"))
    (pcase org-memento-agenda-files
      ((pred functionp) (funcall org-memento-agenda-files)))))

;;;; Completion

(defun org-memento-read-category (&optional prompt)
  "Prompt for a category name."
  (completing-read (or prompt "Category: ")
                   (org-memento--all-categories)))

(defun org-memento--all-categories ()
  (with-current-buffer (org-memento--buffer)
    (delq nil (org-property-values "MEMENTO_CATEGORY"))))

(cl-defun org-memento-read-title (&optional prompt
                                            &key group category default date
                                            select-existing-heading)
  (declare (indent 1))
  (let* ((date (or date (thread-first
                          (org-memento--current-time)
                          (decode-time)
                          (org-memento--today-string))))
         (prompt (or prompt
                     (cond
                      (default
                       (format "Title [\"%s\"]: " default))
                      (group
                       (format "Title for group %s: " (org-memento--format-group group)))
                      (t
                       "Title: "))))
         existing-titles
         input)
    (org-memento-maybe-with-date-entry date
      (let ((bound (save-excursion
                     (org-end-of-subtree))))
        (while (and (< (point) bound)
                    (re-search-forward org-complex-heading-regexp bound t))
          (when (= 2 (length (match-string 1)))
            (push (match-string-no-properties 4)
                  existing-titles)))))
    (catch 'input
      (while (setq input (string-trim
                          (completing-read prompt
                                           (if select-existing-heading
                                               existing-titles
                                             (when category
                                               (org-memento--titles-in-category category)))
                                           nil
                                           select-existing-heading
                                           nil nil default 'inherit-input-method)))
        (if (and (not select-existing-heading)
                 (member input existing-titles))
            (setq prompt (format "Title (\"%s\" is a duplicate): " input))
          (throw 'input input))))))

(defun org-memento-select-slot (prompt slots)
  (let* ((alist (mapcar (lambda (slot)
                          (cons (org-memento--format-army-time-range
                                 (car slot) (cadr slot))
                                slot))
                        slots))
         (input (completing-read prompt (mapcar #'car alist))))
    (or (cdr (assoc input alist))
        (when (string-match (rx bol (group (+ digit)) ":" (group (+ digit)) eol)
                            input)
          (thread-first
            (decode-time)
            (org-memento--maybe-decrement-date)
            (org-memento--set-time-of-day
             (string-to-number (match-string 1 input))
             (string-to-number (match-string 2 input))
             0)
            (encode-time)
            (float-time)
            (list))))))

(defun org-memento-select-order (prompt orders)
  (let ((alist (mapcar (lambda (x)
                         (cons (org-memento-order-title x) x))
                       orders)))
    (cl-labels
        ((annotator (candidate)
           (let ((order (cdr (assoc candidate alist))))
             (concat (if-let (duration (org-memento-duration order))
                         (propertize (format " (%s)" (org-duration-from-minutes duration))
                                     'face 'font-lock-doc-face)
                       "")
                     " "
                     (propertize (org-memento--format-group (org-memento-order-group order))
                                 'face 'font-lock-comment-face))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-order)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (cdr (assoc (completing-read prompt #'completions nil t)
                  alist)))))

(cl-defun org-memento-read-group (&optional prompt &key title default group-path
                                            (from-group-cache t) (from-policies t))
  (declare (indent 1))
  (when (and from-group-cache
             (not org-memento-group-cache))
    (org-memento--cache-groups))
  (require 'org-memento-policy)
  (org-memento-policy-maybe-load)
  (let* ((default-formatted (when default
                              (org-memento--format-group default)))
         (cache (make-hash-table :test #'equal :size 100))
         (prompt (or prompt
                     (if title
                         (format-prompt "Select a group for \"%s\" (or empty to nil)"
                                        default-formatted
                                        title)
                       (format-prompt "Select a group (or empty to nil)"
                                      default-formatted))))
         candidates)
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (if (= (length (gethash candidate cache))
                    (length org-memento-group-taxonomy))
                 "Complete groups"
               "Incomplete group paths from policies")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-group)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred)))
         (check-group (group)
           (or (not group-path)
               (equal (seq-take group (length group-path))
                      group-path))))
      (progn
        (when from-group-cache
          (dolist (group (map-keys org-memento-group-cache))
            (when (check-group group)
              (unless (org-memento-policy-group-archived-p group)
                (let ((title (org-memento--format-group group)))
                  (puthash title group cache)
                  (push title candidates))))))
        (when (and from-policies
                   (bound-and-true-p org-memento-policy-data)
                   (taxy-p org-memento-policy-data))
          (dolist (group-path (cl-remove-duplicates
                               (mapcar #'org-memento-group-path
                                       (org-memento-policy-contexts))
                               :test #'equal))
            (when (check-group group-path)
              (let ((title (org-memento--format-group group-path)))
                (unless (gethash title cache)
                  (puthash title group-path cache))
                (push title candidates)))))
        (setq candidates (nreverse candidates))
        (unwind-protect
            (let ((input (completing-read prompt #'completions nil nil
                                          nil nil default-formatted)))
              (unless (string-empty-p input)
                (gethash input cache input)))
          (clrhash cache))))))

(defun org-memento--default-group (group-path)
  "Return a complete group by filling the full length with nils.."
  (when group-path
    (if (= (length group-path)
           (length org-memento-group-taxonomy))
        group-path
      (append group-path
              (make-list (- (length org-memento-group-taxonomy)
                            (length group-path))
                         nil)))))

(defun org-memento--titles-in-category (category)
  (let (result)
    (with-current-buffer (org-memento--buffer)
      (let ((regexp (org-re-property "MEMENTO_CATEGORY" nil nil category)))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward regexp nil t)
           (let ((heading (org-get-heading t t t t)))
             (remove-text-properties 0 (length heading) '(face) heading)
             (push heading result))))))
    (cl-remove-duplicates result :test #'equal)))

(cl-defun org-memento--read-duration (prompt &key default
                                             now check-next-event)
  (let* ((now (or now (float-time (org-memento--current-time))))
         (upnext-event (when check-next-event
                         (org-memento--next-agenda-event
                          nil nil
                          :now now
                          :include-memento-file t)))
         (limit (when upnext-event
                  (- (/ (- (org-memento-starting-time upnext-event)
                           now)
                        60)
                     org-memento-margin-minutes)))
         (default-minutes (cl-typecase default
                            (string (org-duration-to-minutes default))
                            (number default))))
    (completing-read (concat
                      (if upnext-event
                          (format "The next event \"%s\" starts at %s. "
                                  (org-memento-title upnext-event)
                                  (format-time-string
                                   "%R" (org-memento-starting-time upnext-event)))
                        "")
                      prompt)
                     (thread-last
                       (cons default-minutes (when limit (list limit)))
                       (seq-filter `(lambda (x)
                                      (or (not ,limit)
                                          (>= x ,limit))))
                       (seq-sort #'<)
                       (mapcar #'org-memento--format-duration)))))

(defun org-memento-read-away-title (&optional prompt)
  (completing-read (or prompt "Title: ")
                   (seq-uniq (org-memento-map-away-events
                              (lambda ()
                                (when (looking-at org-complex-heading-regexp)
                                  (match-string-no-properties 4)))))))

(cl-defun org-memento-read-block (prompt &optional blocks-and-events
                                         &key return-struct extra-actions)
  "Interactively select a title.

By default, it lets the user select an unfinished block and
returns its title in a string.

Optionally, you can pass BLOCKS-AND-EVENTS as an argument. It can
be a list where each item is either `org-memento-block' or
`org-memento-org-event'.

If RETURN-STRUCT is non-nil, it returns `org-memento-block' or
`org-memento-agenda-event'. This option implies the user cannot
enter a title that is not included in the candidates.

Optionally, you can give EXTRA-ACTIONS as either a list of
strings or an alist of strings and any objects, which will be fed
into the candidates as well."
  (org-memento-status 'check-in)
  (let ((cache (make-hash-table :test #'equal :size 20))
        candidates)
    (cl-labels
        ((annotator (title)
           (if-let (block-or-event (gethash title cache))
               (cl-etypecase block-or-event
                 (org-memento-block
                  (concat (when-let (time (org-memento-starting-time block-or-event))
                            (propertize (format " %s, in %d minutes"
                                                (format-time-string "%R" time)
                                                (org-memento-minutes-from-now time))
                                        'face 'font-lock-warning-face))
                          (when-let (duration (org-memento-duration block-or-event))
                            (propertize (format " (%s)" (org-duration-from-minutes duration))
                                        'face 'font-lock-doc-face))
                          (when-let (time (org-memento-started-time block-or-event))
                            (propertize (format-time-string " already checked in at %R" time)
                                        'face 'font-lock-comment-face))))
                 (org-memento-org-event
                  (if-let (duration (org-memento-duration block-or-event))
                      (format " Duration %s" (org-memento--format-duration duration))
                    "")))
             ""))
         (group (candidate transform)
           (if transform
               candidate
             (cl-typecase (gethash candidate cache)
               (org-memento-block "Block")
               (org-memento-org-event "Event from org-agenda-files")
               (otherwise "Other actions"))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-block)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))

      (dolist (block-or-event (or blocks-and-events
                                  (thread-last
                                    (org-memento--blocks)
                                    (seq-filter #'org-memento-block-not-closed-p))))
        (let ((title (org-memento-title block-or-event)))
          (puthash title block-or-event cache)
          (push title candidates)))
      (dolist (extra-action extra-actions)
        (pcase extra-action
          ((pred stringp)
           (puthash extra-action extra-action cache)
           (push extra-action candidates))
          (`(,title . ,obj)
           (puthash title obj cache)
           (push title candidates))))
      (let ((title (completing-read prompt #'completions nil return-struct)))
        (if return-struct
            (gethash title cache)
          title)))))

(cl-defun org-memento-read-future-event (start &optional end-bound
                                               &key (reschedule t)
                                               suggestions)
  (org-memento--status)
  (unless org-memento-group-cache
    (org-memento--cache-groups))
  (let* ((now (float-time (org-memento--current-time)))
         (cache (make-hash-table :test #'equal :size 100))
         (duration-limit (when end-bound
                           (/ (- end-bound start) 60)))
         (prompt (concat "Select or enter a thing todo"
                         (if end-bound
                             (concat " in " (org-memento--format-army-time-range
                                             start end-bound))
                           (concat " at " (format-time-string "%R" start)))
                         ": "))
         candidates)
    (cl-labels
        ((annotator (title)
           (pcase-exhaustive (gethash title cache)
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
             ((and (pred org-memento-order-p)
                   order)
              (let ((group (org-memento-group-path order))
                    (duration (org-memento-duration order)))
                (concat (when group
                          (propertize (concat " " (org-memento--format-group group))
                                      'face 'font-lock-doc-face))
                        (when duration
                          (propertize (concat " " (org-duration-from-minutes duration))
                                      'face 'font-lock-doc-face)))))
             (`(context . ,_)
              "")
             (`(copy-entry (,date ,_) :group ,group)
              (format " %s (%s)"
                      (org-memento--format-group group)
                      (org-memento--format-diff-days
                       (- now
                          (thread-first
                            (parse-time-string date)
                            (org-memento--set-time-of-day 0 0 0)
                            (encode-time)
                            (float-time))))))))
         (group (candidate transform)
           (if transform
               candidate
             (pcase (gethash candidate cache)
               ((pred org-memento-block-p)
                "Blocks to (re)schedule")
               ((pred org-memento-order-p)
                "Suggestions from policies")
               (`(context . ,_)
                "Group contexts defined in the policies")
               (`(copy-entry . ,_)
                "Copy the latest entry of a group"))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento-block)
                           (cons 'annotation-function #'annotator)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred)))
         (within-duration-limit-p (block)
           (if-let (duration (org-memento-duration block))
               (< duration duration-limit)
             (if-let* ((starting (org-memento-starting-time block))
                       (ending (org-memento-ending-time block)))
                 (< (/ (- ending starting) 60)
                    duration-limit)
               t)))
         (order-within-duration-limit-p (order)
           (if-let (duration (org-memento-duration order))
               (<= duration duration-limit)
             t))
         (scheduled-past-p (block)
           (and (org-memento-starting-time block)
                (< (org-memento-starting-time block) now)))
         (not-scheduled-p (block)
           (not (org-memento-starting-time block))))
      (progn
        (when reschedule
          (let ((blocks (thread-last
                          (org-memento--blocks)
                          (seq-filter #'org-memento-block-not-closed-p)
                          (seq-filter (if duration-limit
                                          #'within-duration-limit-p
                                        (lambda (_) t)))
                          (seq-filter (lambda (block)
                                        (not (equal (org-memento-title block)
                                                    org-memento-current-block)))))))
            (dolist (block (append (seq-filter #'scheduled-past-p blocks)
                                   (seq-filter #'not-scheduled-p blocks)))
              (puthash (org-memento-title block) block cache)
              (push (org-memento-title block) candidates))))
        (dolist (order (seq-filter #'order-within-duration-limit-p suggestions))
          (let ((title (org-memento-order-title order)))
            (puthash title order cache)
            (push title candidates)))
        (dolist (group (map-keys org-memento-group-cache))
          (unless (org-memento-policy-group-archived-p group)
            (let* ((olp (gethash group org-memento-group-cache))
                   (title (nth 1 olp)))
              (puthash title (list 'copy-entry olp :group group) cache)
              (push title candidates))))
        (when (and (bound-and-true-p org-memento-policy-data)
                   (taxy-p org-memento-policy-data))
          (require 'org-memento-policy)
          (dolist (group-path (org-memento-policy-group-leaves))
            (let ((title (string-join (org-memento--format-group-entries group-path)
                                      " > ")))
              (unless (gethash title cache)
                (puthash title (cons 'context group-path) cache))
              (push title candidates))))
        (setq candidates (nreverse candidates))
        (unwind-protect
            (let* ((completions-sort nil)
                   (input (completing-read prompt #'completions))
                   (entry (gethash input cache input)))
              (pcase entry
                (`(copy-entry ,olp :group ,group)
                 (let ((marker (org-find-olp (cons org-memento-file olp))))
                   (make-org-memento-order
                    :title (nth 1 olp)
                    :group group
                    :sample-marker marker
                    :duration
                    (when-let* ((block (save-current-buffer
                                         (org-with-point-at marker
                                           (org-memento-block-entry))))
                                (start (org-memento-started-time block))
                                (end (org-memento-ended-time block)))
                      (/ (- end start) 60)))))
                (`(context . ,group-path)
                 (make-org-memento-order
                  :group group-path))
                ((pred stringp)
                 (make-org-memento-order
                  :title entry))
                (_
                 entry)))
          (clrhash cache))))))

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
  (string-join (org-memento--format-group-entries group) " > "))

(defun org-memento--format-group-entries (group)
  (let (strings)
    (pcase-dolist (`(,fn ,value)
                   (org-memento--zip (mapcar (org-memento--plist-get :format)
                                             org-memento-group-taxonomy)
                                     group))
      (when-let (string (funcall fn value))
        (push string strings)))
    (nreverse strings)))

(defun org-memento--format-group-last-node (group-path)
  (let ((i (1- (length group-path))))
    (when-let (fn (plist-get (nth i org-memento-group-taxonomy) :format))
      (funcall fn (nth i group-path)))))

;;;; Retrieving timing information

(defun org-memento--next-event (&optional now)
  (let ((now (or now (float-time (org-memento--current-time)))))
    (cl-flet
        ((earlier-away-event (event)
           (org-memento--next-away-event
            (when event
              (org-memento-starting-time event))
            now))
         (earlier-agenda-event (event)
           (org-memento--next-agenda-event
            nil (when event
                  (org-memento-starting-time event))
            :now now))
         (reducer (event fn)
           (or (funcall fn event)
               event)))
      (cl-reduce #'reducer (list #'earlier-away-event #'earlier-agenda-event)
                 :initial-value
                 (thread-last
                   (org-memento--blocks)
                   (seq-filter #'org-memento-block-not-closed-p)
                   (seq-filter `(lambda (block)
                                  (and (org-memento-starting-time block)
                                       (> (org-memento-starting-time block)
                                          ,now))))
                   (seq-sort-by #'org-memento-starting-time #'<)
                   (car))))))

(defun org-memento--empty-slots (taxy)
  (let ((now (float-time (org-memento--current-time)))
        result)
    (dolist (date-taxy (taxy-taxys taxy))
      (when (and (cadr (taxy-name date-taxy))
                 (> (cadr (taxy-name date-taxy)) now))
        (dolist (block-taxy (taxy-taxys date-taxy))
          (when (and (not (taxy-taxys block-taxy))
                     (cadr (taxy-name block-taxy))
                     (> (cadr (taxy-name block-taxy)) now)
                     (not (nth 4 (taxy-name block-taxy))))
            (push (taxy-name block-taxy) result)))))
    result))

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
    (while (and (< (point) bound)
                (re-search-forward org-clock-line-re bound t))
      (let ((start (when (re-search-forward org-ts-regexp-inactive (pos-eol) t)
                     (parse-time-string (match-string 1))))
            (end (when (and (looking-at "--")
                            (re-search-forward org-ts-regexp-inactive (pos-eol) t))
                   (parse-time-string (match-string 1)))))
        (push (cons start end) result)))
    result))

(defun org-memento--clock-start-ts ()
  "Return the timestamp of the starting time of the clock."
  (org-with-point-at org-clock-marker
    (if (thing-at-point-looking-at org-ts-regexp-inactive)
        (org-timestamp-from-string (match-string 0))
      (error "Didn't match a timestamp"))))

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

(cl-defun org-memento--next-agenda-event (&optional hd-marker bound-time
                                                    &key include-memento-file now)
  "Return an Org entry that has the earliest time stamp.

If BOUND-TIME is an internal time, time stamps later than the
time are skipped.

NOW should be a float time which specifies the end of time.

This returns an internal time of the time stamp minus a margin, a
marker to the time stamp, and the margin in seconds."
  (let* ((now (or now (org-memento--current-time)))
         (ts-regexp (org-memento--make-ts-regexp
                     now
                     (or bound-time
                         (thread-first
                           (decode-time now)
                           (org-memento--start-of-day)
                           (decoded-time-add (make-decoded-time :day 1 :minute -1))
                           (encode-time)))))
         (min-time (when bound-time
                     (float-time bound-time)))
         (memento-file (expand-file-name org-memento-file))
         result)
    (dolist (file (if include-memento-file
                      (seq-uniq (cons memento-file (org-agenda-files)))
                    (cl-delete memento-file (org-agenda-files)
                               :test #'equal)))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward ts-regexp nil t)
           (unless (and (equal file memento-file)
                        (= 1 (save-match-data
                               (org-outline-level))))
             (when-let* ((ts (org-timestamp-from-string (match-string 0)))
                         (time (when (org-timestamp-has-time-p ts)
                                 (float-time (org-timestamp-to-time ts)))))
               (when (and (or (not min-time)
                              (< time min-time))
                          (not (org-entry-is-done-p))
                          (not (org-memento--maybe-skip-by-tag)))
                 (let ((marker (save-excursion
                                 (org-back-to-heading)
                                 (point-marker))))
                   (unless (and hd-marker
                                (equal marker hd-marker))
                     (let ((event (make-org-memento-org-event
                                   :marker marker
                                   :active-ts ts)))
                       (unless (and (equal (buffer-file-name)
                                           memento-file)
                                    org-memento-current-block
                                    (equal (org-memento-title event)
                                           org-memento-current-block))
                         (setq min-time time)
                         (setq result event))))))))))))
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
           (unless (org-memento--maybe-skip-by-tag t)
             (let ((obj (make-org-memento-org-event
                         :marker (point-marker)
                         :active-ts (org-timestamp-from-string (match-string 0)))))
               (push (cons (org-memento-starting-time obj)
                           obj)
                     result)
               (goto-char (org-entry-end-position))))))))
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

;;;; Planning

(defconst org-memento-planning-drawer "planning")

(defun org-memento-get-planning-items (block-or-marker)
  (save-current-buffer
    (org-with-point-at (cl-etypecase block-or-marker
                         (org-memento-block
                          (org-memento-block-hd-marker block-or-marker))
                         (marker
                          block-or-marker))
      (let ((bound (org-entry-end-position)))
        (catch 'planning
          (while (and (< (point) bound)
                      (re-search-forward org-drawer-regexp bound t))
            (when (string-equal-ignore-case (match-string 1)
                                            org-memento-planning-drawer)
              (let ((end (save-excursion
                           (re-search-forward org-drawer-regexp bound)))
                    links)
                (while (re-search-forward org-link-bracket-re end t)
                  (let ((href (match-string 1)))
                    (when-let (id (save-match-data
                                    (when (string-match (rx bol "id:") href)
                                      (substring-no-properties href (match-end 0)))))
                      (push (cons id (match-string 2))
                            links))))
                (throw 'planning links)))))))))

(defun org-memento-add-planning-items (block items)
  (save-current-buffer
    (org-with-point-at (org-memento-block-hd-marker block)
      (let ((bound (org-entry-end-position)))
        (catch 'planning
          (while (and (< (point) bound)
                      (re-search-forward org-drawer-regexp bound t))
            (when (string-equal-ignore-case (match-string 1)
                                            org-memento-planning-drawer)
              (insert "\n")
              (throw 'planning t)))
          (org-end-of-meta-data t)
          (when (looking-at org-ts-regexp)
            (forward-line))
          (org-insert-drawer nil "planning"))
        (insert (mapconcat (lambda (item)
                             (org-link-make-string
                              (concat "id:" (org-memento-planning-item-id item))
                              (org-memento-planning-item-heading item)))
                           items
                           "\n"))))))

(defun org-memento-schedule-planning-items (items)
  (let* ((file (thread-last
                 (car items)
                 (org-memento-planning-item-hd-marker)
                 (marker-buffer)
                 (buffer-file-name)))
         (table (make-hash-table :test #'equal))
         candidates)
    (cl-labels
        ((capable-p (block)
           (save-current-buffer
             (org-with-point-at (org-memento-block-hd-marker block)
               (and (member file
                            (mapcar #'expand-file-name
                                    (funcall org-memento-agenda-files)))
                    t))))
         (annotator (_candidate)
           "")
         (completion-group (candidate transform)
           (if transform
               candidate
             (pcase-exhaustive (gethash candidate table)
               ((and (cl-type org-memento-block)
                     block)
                (if (capable-p block)
                    "Blocks related to the file"
                  "Blocks unrelated from the file")))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-memento)
                           (cons 'group-function #'completion-group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred)))
         (t-first (x _)
           x))
      (dolist (block (thread-last
                       (org-memento--blocks)
                       (seq-filter #'org-memento-block-not-closed-p)
                       (seq-group-by #'capable-p)
                       (seq-sort-by #'car #'t-first)
                       (mapcar #'cdr)
                       (apply #'append)))
        (let ((title (org-memento-title block)))
          (puthash title block table)
          (push title candidates)))
      (setq candidates (reverse candidates))
      (let* ((completions-sort nil)
             (title (completing-read "Select a block: " #'completions)))
        (pcase-exhaustive (gethash title table)
          ((and (cl-type org-memento-block)
                block)
           (org-memento-add-planning-items block items)))
        t))))

(defun org-memento--planning-items ()
  "Collect planning items from the agenda files."
  (let (result
        (last-midnight (org-memento--midnight (org-memento--current-time))))
    (cl-labels
        ;; Faster version of `org-in-archived-heading-p'.
        ((archivedp ()
           (save-match-data
             (save-excursion
               (catch 'archived
                 (while (looking-at org-complex-heading-regexp)
                   (when (and (nth 10 (match-data))
                              (string-match-p (regexp-quote
                                               (concat ":" org-archive-tag ":"))
                                              (match-string 5)))
                     (throw 'archived t))
                   (when (= 1 (length (match-string 1)))
                     (throw 'archived nil))
                   (org-up-heading-all 1))))))
         (has-future-time ()
           (save-match-data
             (save-excursion
               (org-end-of-meta-data t)
               (when (re-search-forward org-stamp-time-of-day-regexp
                                        (org-entry-end-position)
                                        t)
                 (time-less-p (org-memento--current-time)
                              (thread-last
                                (match-string 0)
                                (org-timestamp-from-string)
                                (org-timestamp-to-time))))))))
      (dolist (file (org-agenda-files))
        ;; This function is currently used exclusively from org-memento-timeline.el,
        ;;and `org-memento-file' should be excluded in that case.
        (unless (file-equal-p file org-memento-file)
          (with-current-buffer (or (find-buffer-visiting file)
                                   (find-file-noselect file))
            (org-save-outline-visibility t
              (org-with-wide-buffer
               (org-fold-show-all)
               (goto-char (point-min))
               (while (re-search-forward org-planning-line-re nil t)
                 (when (catch 'today
                         (while (re-search-forward org-ts-regexp (pos-eol) t)
                           (let ((time (org-timestamp-to-time
                                        (org-timestamp-from-string (match-string 0)))))
                             (when (and (time-less-p time (org-memento--current-time))
                                        (not (time-less-p time last-midnight)))
                               (throw 'today t)))))
                   (save-excursion
                     (org-back-to-heading)
                     (looking-at org-complex-heading-regexp)
                     (unless (or (org-memento--maybe-skip-by-tag t)
                                 ;; (member (match-string 2) org-done-keywords)
                                 ;; (has-future-time)
                                 )
                       (push (make-org-memento-planning-item
                              :hd-marker (point-marker)
                              :heading (match-string-no-properties 4)
                              :effort (org-entry-get nil "Effort")
                              :id (org-id-get-create)
                              :time-of-day-ts
                              (save-excursion
                                (when (re-search-forward org-stamp-time-of-day-regexp
                                                         (org-entry-end-position)
                                                         t)
                                  (org-timestamp-from-string (match-string 0)))))
                             result)))
                   (re-search-forward org-heading-regexp nil t)))))))))
    result))

(defun org-memento--maybe-skip-by-tag (&optional goto-end)
  "Return non-nil if an Org entry at point has a skip tag.

This function returns non-nil if the entry has one of
`org-memento-agenda-excluded-tags'. Also, if GOTO-END is non-nil,
it moves the point to the end of the subtree if the result is
non-nil.

This function must be called at the beginning of the entry."
  (let ((regexp (rx-to-string `(and ":" (or ,@org-memento-agenda-excluded-tags) ":"))))
    (when-let (skipped (save-match-data
                         (save-excursion
                           (catch 'skipped
                             (while (looking-at org-complex-heading-regexp)
                               (when (and (nth 10 (match-data))
                                          (string-match-p regexp (match-string 5)))
                                 (throw 'skipped t))
                               (when (= 1 (length (match-string 1)))
                                 (throw 'skipped nil))
                               (org-up-heading-all 1))))))
      (when goto-end
        (org-end-of-subtree))
      skipped)))

;;;; Collect data for analytic purposes

;;;###autoload
(cl-defun org-memento-activity-taxy (start-day end-day &key groups todos)
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
                                (<= (cadr record) ,end)))
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
                                  (<= (cadr record) ,computed-end)))
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
                             (when-let (string (org-entry-get nil "MEMENTO_CHECKIN_TIME"))
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
                  (org-memento--block-activities start-day end-day
                                                 :annotate-todos todos
                                                 :annotate-groups groups)
                  (fill-voids (float-time start-time)
                              (float-time end-time)
                              #'car #'make-gap-date)
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
             (when (and (< (point) bound)
                        (re-search-forward org-logbook-drawer-re bound t))
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
                        (not (org-entry-is-done-p))
                        (not (org-memento--maybe-skip-by-tag)))
               (catch 'active-ts
                 (goto-char hd-marker)
                 (while (and (< (point) bound)
                             (re-search-forward org-ts-regexp bound t))
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

(cl-defun org-memento--block-activities (start-date-string &optional end-date-string
                                                           &key annotate-groups
                                                           annotate-todos)
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
       (parse-entry (include-future &optional away return-element)
         (if (or include-future away return-element)
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
                 (list start end
                       (when return-element
                         (list (org-memento-headline-element block))))))
           ;; Faster version for past activities.
           (let* ((entry-end (org-entry-end-position))
                  (end (when (re-search-forward org-closed-time-regexp entry-end t)
                         (parse-time (match-string 1))))
                  (start (when-let (string (org-entry-get nil "MEMENTO_CHECKIN_TIME"))
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
       (parse-idle-children ()
         (let ((subtree-end (save-excursion (org-end-of-subtree)))
               blocks)
           ;; This is like in `org-memento--agenda-activities', but without future
           ;; activities.
           (while (re-search-forward org-complex-heading-regexp subtree-end t)
             (let ((heading (match-string-no-properties 4))
                   (hd-marker (point-marker))
                   (bound (org-entry-end-position)))
               (pcase (save-excursion (parse-entry nil 'away))
                 (`(,start ,end . ,_)
                  (push (list start
                              end
                              heading
                              hd-marker
                              'away)
                        blocks)))
               (when (and (< (point) bound)
                          (re-search-forward org-logbook-drawer-re bound t))
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
           blocks))
       (end-of-day-float-time (float-time)
         (thread-first
           (decode-time float-time)
           (org-memento--start-of-day)
           (decoded-time-add (make-decoded-time :hour 23 :minute 59))
           (encode-time)
           (float-time))))
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
                    (`(,start ,end . ,_)
                     (let ((day (list start
                                      ;; If the end time is missing, e.g. it has
                                      ;; already passed the expected checkout
                                      ;; time, set it to the time right before
                                      ;; the start of the next day.
                                      (or end (end-of-day-float-time start))
                                      date-string marker 'date))
                           (subtree-end (save-excursion (org-end-of-subtree)))
                           blocks)
                       (while (re-search-forward org-complex-heading-regexp subtree-end t)
                         (beginning-of-line)
                         (let* ((level (length (match-string 1)))
                                (is-block (= level 2))
                                (heading (match-string-no-properties 4))
                                (hd-marker (point-marker)))
                           (if (equal heading org-memento-idle-heading)
                               (setq blocks (append blocks
                                                    (parse-idle-clocks)
                                                    (parse-idle-children)))
                             (pcase (parse-entry include-future nil annotate-groups)
                               (`(,start ,end . ,rest)
                                (let* ((record (list start
                                                     end
                                                     heading
                                                     hd-marker
                                                     (if is-block
                                                         'block
                                                       'away)))
                                       (element (caar rest)))
                                  (when is-block
                                    (nconc record
                                           (when-let (keyword (and annotate-todos
                                                                   (org-element-property
                                                                    :todo-keyword
                                                                    element)))
                                             `(:todo-keyword ,keyword))
                                           (when annotate-groups
                                             `(:group
                                               ,(save-excursion
                                                  (goto-char hd-marker)
                                                  (org-memento--get-group element))))))
                                  (push record blocks))))))
                         (end-of-line 1))
                       (push (cons day blocks) dates))))))))
          dates)))))

;;;; Grouping

(defun org-memento--cache-groups ()
  (let* ((alist1 (org-memento-map-past-blocks-1
                  (lambda (date)
                    (when (equal (org-get-todo-state)
                                 org-memento-todo-keyword-for-success)
                      (let ((props (thread-last
                                     (cl-remove-if
                                      (lambda (key)
                                        (member key org-memento-unique-properties))
                                      (org-entry-properties nil 'standard)
                                      :key #'car)
                                     (seq-sort-by #'car #'string<)))
                            (tags (org-get-tags)))
                        (when (or props tags)
                          (list (cons props tags)
                                date
                                (point-marker))))))))
         (alist2 (cl-remove-duplicates alist1
                                       :key #'car
                                       :test #'equal)))
    (if org-memento-group-cache
        (clrhash org-memento-group-cache)
      (setq org-memento-group-cache (make-hash-table :test #'equal)))
    (with-current-buffer (org-memento--buffer)
      (org-with-wide-buffer
       (pcase-dolist (`(,_ ,date ,marker) alist2)
         (goto-char marker)
         (let ((element (org-element-headline-parser (org-entry-end-position))))
           (puthash (org-memento--get-group element)
                    (list date (org-element-property :raw-value element))
                    org-memento-group-cache)))))))

(defun org-memento--collect-groups-1 (&optional start-date-string end-date-string)
  (org-memento-map-past-blocks
   (lambda (date block)
     (list (org-memento--get-group (org-memento-block-headline block))
           (org-memento-started-time block)
           (org-memento-ended-time block)
           date
           (org-memento-title block)
           :todo-keyword
           (org-element-property :todo-keyword
                                 (org-memento-headline-element block))))
   start-date-string end-date-string))

(defun org-memento--get-group (&optional element)
  (save-excursion
    (when element
      (goto-char (org-element-property :begin element)))
    (let (result
          ;; Explicitly giving ELEMENT can slightly improve performance.
          (element (or element
                       (org-element-headline-parser (org-entry-end-position)))))
      (dolist (plist org-memento-group-taxonomy)
        (push (funcall (plist-get plist :read)
                       element)
              result))
      (nreverse result))))

(defun org-memento--merge-group-sums-1 (lists)
  (thread-last
    (seq-group-by #'car (apply #'append lists))
    (mapcar (pcase-lambda (`(,group . ,groups-and-sums))
              (cons group
                    (cl-reduce #'+ (mapcar #'cdr
                                           groups-and-sums)
                               :initial-value 0))))))

(defun org-memento--update-weekly-group-sums ()
  "Update the value of `org-memento-weekly-group-sums'.

This is useful for `org-memento-timeline'."
  (pcase-exhaustive (org-memento-week-date-range 0)
    (`(,start-date ,_end-date)
     (let ((decoded-time (decode-time (org-memento--current-time))))
       (setq org-memento-weekly-group-sums
             (unless (equal start-date (org-memento--today-string decoded-time))
               (org-memento-group-sums-1
                (org-memento-activity-taxy
                 start-date (format-time-string
                             "%F"
                             (thread-first
                               decoded-time
                               (org-memento--start-of-day)
                               (decoded-time-add (make-decoded-time :day -1))
                               (encode-time)))
                 :groups t))))))))

(defun org-memento-group-sums-1 (taxy)
  "Return the sums of time spent on blocks.

TAXY must be a result of `org-memento-activity-taxy' with :blocks
argument set to non-nil."
  (let ((now (float-time (org-memento--current-time))))
    (thread-last
      (org-memento--map-taxy-blocks taxy
        `(lambda (record)
           (pcase record
             ((and `(,start ,end ,_ ,_ block . ,(map :group))
                   (guard start)
                   (guard end)
                   (guard group)
                   (guard (< end ,now)))
              (cons group
                    (/ (- end start) 60))))))
      (seq-group-by #'car)
      (mapcar (pcase-lambda (`(,group . ,records))
                (make-org-memento-group-data
                 :group-path group
                 :value (cl-reduce #'+ (mapcar #'cdr records)
                                   :initial-value 0)
                 :tag 'activity-sum))))))

(defun org-memento-group-planned-sums-1 ()
  (save-current-buffer
    (thread-last
      (org-memento--blocks)
      (cl-remove-if #'org-memento-started-time)
      (mapcar (lambda (block)
                (org-with-point-at (org-memento-block-hd-marker block)
                  (cons (org-memento--get-group
                         (org-memento-block-headline block))
                        (or (org-memento-duration block)
                            (and (org-memento-starting-time block)
                                 (org-memento-ending-time block)
                                 (/ (- (org-memento-ending-time block)
                                       (org-memento-starting-time block))
                                    60)))))))
      (cl-remove-if-not #'cdr)
      (seq-group-by #'car)
      (mapcar (lambda (group-and-entries)
                (make-org-memento-group-data
                 :group-path (car group-and-entries)
                 :value (cl-reduce #'+
                                   (mapcar #'cdr (cdr group-and-entries))
                                   :initial-value 0)
                 :tag 'planned-sum))))))

(cl-defun org-memento--map-taxy-blocks (taxy fn)
  "Run a function on each block record.

TAXY must be a result of `org-memento-activity-taxy'."
  (declare (indent 1))
  (let (result)
    (dolist (date-taxy (taxy-taxys taxy))
      (dolist (taxy (taxy-taxys date-taxy))
        (when-let (r (funcall fn (taxy-name taxy)))
          (push r result))))
    result))

(cl-defun org-memento-find-by-group-path (group-path items &key key)
  (cl-assert (not (null group-path)))
  (cl-labels
      ((path (x)
         (org-memento-group-path (if key
                                     (funcall key x)
                                   x)))
       (pred (x)
         (equal group-path
                (seq-take (path x)
                          (length group-path))))
       (path-length (x)
         (length (path x))))
    (thread-last
      (seq-filter #'pred items)
      (seq-sort-by #'path-length #'>)
      (car))))

(cl-defun org-memento-filter-by-group-path (group-path items &key key)
  (cl-assert (not (null group-path)))
  (cl-labels
      ((path (x)
         (org-memento-group-path (if key
                                     (funcall key x)
                                   x)))
       (pred (x)
         (equal group-path
                (seq-take (path x)
                          (length group-path)))))
    (thread-last
      (seq-filter #'pred items))))

(defun org-memento-set-group (group)
  "Set the group of the entry at point."
  (interactive (list (save-excursion
                       (org-back-to-heading)
                       (org-memento-read-group nil
                         :default (org-memento--get-group)))))
  (pcase-exhaustive (org-memento--template-group group)
    ((map :properties :tags)
     (org-set-tags tags)
     (pcase-dolist (`(,key . ,value) properties)
       (org-entry-put nil key value)))))

(cl-defun org-memento-group-agenda-files (group &key title)
  "Return a list of Org agenda files associated with GROUP."
  (with-temp-buffer
    (let ((org-inhibit-startup t)
          ;; Don't load modules.
          (org-modules-loaded t))
      (delay-mode-hooks (org-mode)))
    (insert (apply #'org-memento--event-template :title (or title "")
                   (org-memento--template-group group)))
    (goto-char (point-min))
    (funcall org-memento-agenda-files)))

(defun org-memento--groups-for-agenda-file (file)
  (thread-last
    (cl-remove-if-not (apply-partially #'equal file)
                      (org-memento--collect-group-files)
                      :key #'cdr)
    (mapcar #'car)))

(defun org-memento--collect-group-files ()
  "Returns a list of mappings between a group and an Org file.

This function collects information from planning blocks in past
activities. The returned value is a list of (GROUP . FILE) where
GROUP is a group path and FILE is an Org file."
  (thread-last
    (org-memento-map-past-blocks-1
     (lambda (_)
       (when-let (items (save-excursion
                          (org-memento-get-planning-items (point-marker))))
         (let ((group (org-memento--get-group))
               (files (thread-last
                        items
                        (mapcar #'car)
                        (mapcar #'org-id-find-id-file)
                        ;; (cl-remove-if-not #'cdr)
                        (seq-uniq))))
           (thread-last
             files
             (mapcar (apply-partially #'cons group)))))))
    ;; flatten the lists.
    (apply #'append)))

;;;; Taxy utilities

(defun org-memento-taxy-trees-with-item (taxy)
  "Given a taxy, return its descendants that have an item."
  (let (result)
    (cl-labels
        ((search (x)
           (if (taxy-items x)
               (push x result)
             (dolist (subtaxy (taxy-taxys x))
               (search subtaxy)))))
      (search taxy))
    (nreverse result)))

;;;; Zones

(cl-defmacro org-memento-def-zone (title &key description duration complete
                                         block-p planning-item-p order-p group-p
                                         children)
  (declare (indent 1))
  `(make-taxy :name ',(list title :duration duration :complete complete)
              :description ,description
              :taxys ',children
              :predicate (lambda (x)
                           (if-let (fn (cl-typecase x
                                         (org-memento-planning-item
                                          ,planning-item-p)
                                         (org-memento-block
                                          ,block-p)
                                         (org-memento-order
                                          ,order-p)))
                               (funcall fn x)
                             (and ,group-p
                                  (when-let (group (org-memento-group-path x))
                                    (funcall ,group-p group)))))))

(defun org-memento-set-zone (&rest zones)
  "Set the value of `org-memento-zone-taxy'."
  (setq org-memento-zone-taxy (make-taxy :taxys zones)))

;;;; Utility functions for time representations and Org timestamps

(defun org-memento-week-date-range (n)
  (let* ((today (thread-first
                  (org-memento--current-time)
                  (decode-time)
                  (org-memento--start-of-day)))
         (week-start (thread-last
                       (make-decoded-time
                        :day (+ (- (mod (+ 7 (- (org-day-of-week
                                                 (decoded-time-day today)
                                                 (decoded-time-month today)
                                                 (decoded-time-year today))
                                                org-agenda-start-on-weekday))
                                        7))
                                (* n 7)))
                       (decoded-time-add today)))
         (week-end (decoded-time-add
                    week-start (make-decoded-time :day 6))))
    (list (format-time-string "%F" (encode-time week-start))
          (format-time-string "%F" (encode-time week-end)))))

(defun org-memento--percentage-on-week ()
  (require 'org-memento-date)
  (pcase-exhaustive (org-memento-week-date-range 0)
    (`(,start-date ,end-date)
     (let ((planned-sum 0)
           (past-sum 0)
           (today (org-memento--start-of-day
                   (decode-time (org-memento--current-time))))
           (date (parse-time-string start-date)))
       (while (not (org-memento-date--le (parse-time-string end-date) date))
         (when-let (plist (org-memento--normal-workhour date))
           (let* ((duration (plist-get plist :normal-duration))
                  (saving (plist-get plist :normal-saving))
                  (mean-duration (when duration
                                   (- (org-duration-to-minutes duration)
                                      (if saving
                                          (org-duration-to-minutes saving)
                                        0)))))
             (cl-incf planned-sum mean-duration)
             (when (org-memento-date--le date today)
               (cl-incf past-sum mean-duration))))
         (setq date (decoded-time-add date (make-decoded-time :day 1))))
       (cl-incf past-sum (/ (- (float-time (org-memento--current-time))
                               (org-memento-started-time (org-memento-today-as-block)))
                            60))
       (* (/ past-sum planned-sum) 100)))))

(defun org-memento--fill-decoded-time (decoded-time)
  "Fill time fields of DECODED-TIME."
  (dolist (i (number-sequence 0 2))
    (unless (nth i decoded-time)
      (setf (nth i decoded-time) 0)))
  decoded-time)

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

(defun org-memento--today-string (&optional decoded-time)
  (format-time-string "%F"
                      (encode-time
                       (org-memento--maybe-decrement-date
                        (or decoded-time
                            (decode-time (org-memento--current-time)))))))

(defun org-memento--start-of-day (decoded-time)
  "Return the start of the day given as DECODED-TIME.

This respects `org-extend-today-until'."
  (org-memento--set-time-of-day (if (decoded-time-hour decoded-time)
                                    (org-memento--maybe-decrement-date decoded-time)
                                  decoded-time)
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

(defun org-memento--date-range-regexp (from-date to-date)
  (rx-to-string `(or ,@(mapcar (lambda (decoded-time)
                                 (format-time-string "%F" (encode-time decoded-time)))
                               (org-memento--date-list
                                (parse-time-string from-date)
                                (parse-time-string to-date))))))

(defun org-memento--read-date-range ()
  "Return a date range in a list of decoded times."
  (let ((org-extend-today-until 0))
    (mapcar #'parse-time-string
            (list (org-read-date nil nil nil "Start date")
                  (org-read-date nil nil nil "End date")))))

(defun org-memento--read-time-span (&optional default on-date)
  "Prompt for a time span.

DEFAULT is an optional timestamp string which contains the
default range.

If ON-DATE is given, the result will be adjusted to be on the date.

This function returns (START END) where START and END are time
representations. END can be nil if the user doesn't enter a time
range."
  (let ((ts (with-temp-buffer
              (when default
                (insert default)
                (goto-char (point-min)))
              ;; TODO: Add a custom prompt by using `org-read-date' directly
              ;;
              ;; It seems impossible to retrieve the end time from
              ;; `org-end-time-was-given' when lexical binding is enabled.
              ;; To be resolved in the future.
              (org-time-stamp nil)
              (goto-char (point-min))
              (org-element-timestamp-parser)))
        (date (when on-date
                (org-memento--start-of-day (decode-time on-date)))))
    (list (org-memento--timestamp-to-time ts nil date)
          (when (eq 'active-range (org-element-property :type ts))
            (org-memento--timestamp-to-time ts 'end date)))))

(defun org-memento--timestamp-to-time (ts &optional end date)
  (let ((hour (mod (org-element-property (if end :hour-end :hour-start) ts) 24)))
    (encode-time (make-decoded-time
                  :year (if date
                            (decoded-time-year date)
                          (org-element-property (if end :year-end :year-start) ts))
                  :month (if date
                             (decoded-time-month date)
                           (org-element-property (if end :month-end :month-start) ts))
                  :day (if date
                           (decoded-time-day date)
                         (org-element-property (if end :day-end :day-start) ts))
                  :hour (if (and org-extend-today-until
                                 (< hour org-extend-today-until))
                            (+ hour 24)
                          hour)
                  :minute (org-element-property (if end :minute-end :minute-start) ts)
                  :second 0))))

(defun org-memento--format-timestamp (start-time &optional end-time inactive)
  (let* ((midnight (org-memento--midnight start-time)))
    (format (format-time-string (if inactive
                                    "[%Y-%m-%d %a %%s]"
                                  "<%Y-%m-%d %a %%s>")
                                midnight)
            (if end-time
                (org-memento--format-army-time-range start-time end-time midnight)
              (org-memento--format-army-time start-time midnight)))))

(defun org-memento--format-active-range (start-time end-time)
  (org-memento--format-timestamp start-time end-time))

(defun org-memento--format-army-time-range (start end &optional midnight)
  (let* ((start (cl-etypecase start
                  (number (time-convert start 'list))
                  (list start)))
         (end (cl-etypecase end
                (number (time-convert end 'list))
                (list end)))
         (midnight (or midnight (org-memento--midnight start))))
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

(defun org-memento--midnight (time)
  (thread-first
    (org-memento--maybe-decrement-date (decode-time time))
    (org-memento--set-time-of-day 0 0 0)
    (encode-time)
    (float-time)))

(defun org-memento--format-duration (minutes)
  "An alternative function for formatting a duration."
  (format "%d:%02d"
          (floor (/ minutes 60))
          (mod minutes 60)))

(defun org-memento--format-diff-days (seconds)
  (let ((ndays (floor (/ seconds (* 3600 24)))))
    (cond
     ((< ndays 1)
      (format "today"))
     ((< ndays 2)
      (format "yesterday"))
     ((< ndays 14)
      (format "%d days ago" ndays))
     ((< ndays 30)
      (format "%d weeks ago" (floor (/ ndays 7))))
     ((< ndays 60)
      (format "1 month ago"))
     ((< ndays 365)
      (format "%d months ago" (floor (/ ndays 30))))
     ((< ndays 730)
      (format "1 year ago"))
     (t
      (format "%d years ago" (floor (/ ndays 365)))))))

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
(cl-defun org-memento-add-event (&key title category group start end duration
                                      body copy-from interactive away)
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
         (title (or title (if away
                              (org-memento-read-away-title)
                            (org-memento-read-title nil
                              :date (when start
                                      (thread-last
                                        (decode-time start)
                                        (org-memento--start-of-day)
                                        (format-time-string "%F")))))))
         (group (unless away
                  (or group
                      (org-memento-read-group nil :title title))))
         (arguments (org-memento--merge-template-arguments-1
                     (org-memento--template-group group)
                     (when copy-from
                       (save-current-buffer
                         (org-with-point-at copy-from
                           (list :tags (org-get-tags nil t)
                                 :properties
                                 (cl-remove-if
                                  (lambda (key)
                                    (member key org-memento-unique-properties))
                                  (org-entry-properties nil 'standard)
                                  :key #'car)))))))
         (category (unless (or away copy-from)
                     (or category
                         (cdr (assoc "MEMENTO_CATEGORY" (plist-get arguments :properties)))
                         (org-memento-read-category nil))))
         (template (if away
                       (org-memento--away-event-template
                        :start start :end end :title title
                        :interactive interactive)
                     (apply #'org-memento--event-template
                            :start start :end end :title title :category category
                            :body body
                            :duration duration
                            :interactive interactive
                            arguments)))
         (plist (unless interactive
                  '(:immediate-finish t)))
         (org-capture-entry `("" ""
                              entry (file+function ,org-memento-file ,jump-fn)
                              ,template ,@plist
                              :after-finalize org-memento-log-update)))
    (org-capture)))

(cl-defun org-memento--add-immediate-block (title &key start)
  "Add a block that has just started."
  (declare (indent 1))
  (when org-memento-current-block
    (user-error "Already in a block"))
  (let* ((now (float-time (org-memento--current-time)))
         (start (or start
                    now))
         (group (org-memento-read-group nil :title title))
         (duration (org-memento--read-duration
                    "Duration in minutes: "
                    :check-next-event t
                    :default "0:25"
                    :now now)))
    (org-memento-add-event :title title
                           :group group
                           :interactive nil
                           :start start
                           :duration (unless (string-empty-p duration)
                                       (org-duration-to-minutes duration)))))

;;;###autoload
(defun org-memento-start-quick-event (text)
  (let ((title (string-trim text)))
    (org-memento--add-immediate-block title
      :start (when (org-clocking-p)
               (thread-last
                 (org-memento--clock-start-ts)
                 (org-timestamp-to-time)
                 (float-time))))
    (org-memento-start-block title)))

;;;###autoload
(defun org-memento-add-quick-event (text)
  (let ((title (string-trim text))
        (duration (org-memento--read-duration
                   "Duration in minutes: "
                   :check-next-event t
                   :default "0:25")))
    ;; TODO: Parse time
    (org-memento-add-event :title title
                           :duration (unless (string-empty-p duration)
                                       (org-duration-to-minutes duration)))))

(cl-defun org-memento-schedule-block (start end-bound
                                            &key confirmed-time
                                            suggestions)
  "Schedule a block."
  (let ((event (org-memento-read-future-event start end-bound
                                              :suggestions suggestions)))
    (cl-etypecase event
      (org-memento-block
       (save-current-buffer
         (org-memento-with-block-title (org-memento-title event)
           (org-end-of-meta-data t)
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
      (org-memento-order
       (pcase-let*
           ((title (or (and (not (org-memento-order-sample-marker event))
                            (org-memento-order-title event))
                       (org-memento-read-title nil
                         :default (org-memento-order-title event)
                         :group (org-memento-order-group event))))
            (`(,start ,end) (if confirmed-time
                                (list start end-bound)
                              (org-memento--read-time-span
                               (org-memento--format-active-range
                                start
                                (if-let (duration (org-memento-order-duration event))
                                    (+ start (* 60 duration))
                                  end-bound))
                               start))))
         (org-memento-add-event :title title
                                :start start
                                :end end
                                :interactive t
                                :group (org-memento--default-group
                                        (org-memento-order-group event))
                                :body (org-memento--order-template event)
                                :copy-from (org-memento-order-sample-marker event)))))))

(defun org-memento--order-template (order)
  (pcase-exhaustive (org-memento-order-template order)
    (`nil)
    ((and (pred stringp) string)
     string)
    (`(link ,link)
     (org-memento--org-link-entry-body link))
    (`(id ,id)
     (org-memento--id-entry-body id))
    (`(file ,file)
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun org-memento--org-link-entry-body (link)
  (if (string-match org-link-bracket-re link)
      (let ((url (match-string 1 link)))
        (pcase-exhaustive url
          ((rx bol "id:" (group (+ anything)))
           (org-memento--id-entry-body (match-string 1 url)))))
    (error "Link must match `org-link-bracket-re': %s" link)))

(defun org-memento--id-entry-body (id)
  (save-current-buffer
    (org-with-point-at (org-id-find id 'marker)
      (save-excursion
        (org-end-of-meta-data t)
        (buffer-substring-no-properties (point) (org-entry-end-position))))))

(defun org-memento--valid-template-p (template)
  "Check the type of a template value.

This should be used at loading time."
  (or (stringp template)
      (pcase-exhaustive template
        (`(id ,id)
         (stringp id))
        (`(file ,file)
         (stringp file))
        (`(link ,link)
         (string-match-p org-link-bracket-re link))
        (_
         nil))))

(cl-defun org-memento--event-template (&key title category start end duration body
                                            interactive tags properties)
  (let ((started-past (time-less-p start (org-memento--current-time)))
        (ended-past (and end (time-less-p end (org-memento--current-time)))))
    (pcase-dolist (`(,key . ,value)
                   `(("MEMENTO_CATEGORY"
                      . ,(unless (and category (string-empty-p category))
                           category))
                     ("MEMENTO_CHECKIN_TIME"
                      . ,(when started-past
                           (org-memento--format-timestamp start nil 'inactive)))
                     ("Effort"
                      . ,(when duration
                           (org-duration-from-minutes duration)))))
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
            (or body
                (if interactive
                    "%?"
                  "")))))

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

(defun org-memento--template-group (group)
  (thread-last
    (org-memento--zip org-memento-group-taxonomy group)
    (mapcar (pcase-lambda (`(,plist ,arg))
              (when-let (fn (plist-get plist :template))
                (funcall fn
                         arg))))
    (org-memento--merge-template-arguments)))

(defun org-memento--merge-template-arguments (plists)
  "Merge plists for templating, later ones preceding."
  (cl-reduce #'org-memento--merge-template-arguments-1
             plists :initial-value nil))

(defun org-memento--merge-template-arguments-1 (plist1 plist2)
  (list :tags
        (cl-remove-duplicates
         (append (plist-get plist1 :tags)
                 (plist-get plist2 :tags))
         :test #'equal)
        :properties
        (cl-remove-duplicates
         (append (plist-get plist1 :properties)
                 (plist-get plist2 :properties))
         :test #'equal :key #'car)))

;;;; Exporting

;;;###autoload
(defun org-memento-export-groups-to-csv (file &optional append
                                              start-date end-date)
  "Export the data to CSV."
  (let ((data (org-memento--collect-groups-1 start-date end-date)))
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
           (insert (mapconcat #'escape-cell cells ",") "\n")))
      (with-temp-buffer
        (unless append
          (write-record (append (mapcar (lambda (i)
                                          (format "Group %d" i))
                                        (number-sequence
                                         1 (length org-memento-group-taxonomy)))
                                (list "Date")
                                (list "Title")
                                (list "Start")
                                (list "End"))))
        (dolist (record data)
          (write-record (append (org-memento--format-group-entries (car record))
                                (list (nth 2 (cdr record))
                                      (nth 3 (cdr record))
                                      (format-time-string "%FT%R" (nth 0 (cdr record)))
                                      (format-time-string "%FT%R" (nth 1 (cdr record)))))))
        (write-region (point-min) (point-max) file append)))))

;;;; org-link integration

(org-link-set-parameters "org-memento"
                         :follow #'org-memento-follow-link
                         :store #'org-memento-store-link)

(defun org-memento-follow-link (path &rest _args)
  (pcase (org-memento--parse-link path)
    (`(,func . ,args)
     (apply func args))))

(defun org-memento--parse-link (path)
  (save-match-data
    (when (string-match (rx bol (group (+ word))
                            "?" (group (+ anything))
                            eol)
                        path)
      (cl-flet
          ((parse-date-range (string)
             (save-match-data
               (when (string-match (rx bol (group (regexp org-memento-date-regexp))
                                       "," (group (regexp org-memento-date-regexp))
                                       eol)
                                   string)
                 (list (match-string 1 string)
                       (match-string 2 string))))))
        (let* ((type (match-string 1 path))
               (alist (url-parse-query-string (match-string 2 path)))
               (date-range (parse-date-range (cadr (assoc "date" alist)))))
          (pcase-exhaustive type
            ("timeline"
             `(org-memento-timeline
               ,@date-range
               ,@(when-let (span (cadr (assoc "span" alist)))
                   (cl-assert (member span '("day" "week" "month")))
                   `(:span ,(intern span)))))))))))

(defun org-memento-store-link ()
  (when (eq major-mode 'org-memento-timeline-mode)
    (cl-assert (and (= 2 (length org-memento-timeline-date-range))
                    (mapcar #'stringp org-memento-timeline-date-range)))
    (org-link-store-props
     :type "org-memento"
     :link (apply #'org-memento-make-timeline-link
                  (append org-memento-timeline-date-range
                          (list :span org-memento-timeline-span)))
     :description (if (eq org-memento-timeline-span 'day)
                      (format "Timeline for %s"
                              (car org-memento-timeline-date-range))
                    (apply #'format "Timeline for %s from %s to %s"
                           (or org-memento-timeline-span "the period")
                           org-memento-timeline-date-range)))))

(cl-defun org-memento-make-timeline-link (start-date end-date &key span)
  (concat (format "org-memento:timeline?date=%s,%s" start-date end-date)
          (if span
              (format "&span=%s" span)
            "")))

;;;; Integrations with third-party packages

;;;;; org-ql

(defcustom org-memento-super-groups
  '((:name "Closed" :todo "DONE")
    (:name "Working on" :property "MEMENTO_CHECKIN_TIME")
    (:auto-map org-memento--super-agenda-ts-map)
    (:name "Unscheduled" :anything t))
  "Groups of org-super-agenda used for org-memento."
  :type '(repeat sexp))

;;;###autoload
(cl-defun org-memento-agenda (date &optional to-date &key super-groups)
  "Display time blocks on the current date."
  (declare (indent 2))
  (interactive (if (equal current-prefix-arg '(4))
                   (list (org-read-date)
                         (org-read-date))
                 (list (org-memento--today-string (decode-time)))))
  (require 'org-ql-search)
  (let ((org-super-agenda-properties-inherit nil))
    (org-ql-search (list org-memento-file)
      (org-memento--ql-for-blocks date to-date)
      :title (format "Blocks %s%s"
                     date
                     (if to-date
                         (concat "-" to-date)
                       ""))
      :super-groups (or super-groups
                        org-memento-super-groups))))

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
  `(org-ql-block ',(org-memento--ql-for-blocks (org-memento--today-string
                                                (decode-time)))
                 ((org-ql-block-header "Time blocks")
                  (org-agenda-files '(,org-memento-file))
                  (org-super-agenda-properties-inherit nil)
                  (org-super-agenda-groups ',org-memento-super-groups))))

(defun org-memento--ql-for-blocks (date &optional end-date)
  (let ((parent-ql (if end-date
                       `(heading-regexp ,(org-memento--date-range-regexp date end-date))
                     `(heading ,date))))
    `(and (level 2)
          (parent ,parent-ql)
          (not (heading-regexp
                ,(rx-to-string `(or (and bol "COMMENT")
                                    ,org-memento-idle-heading)))))))

(defun org-memento--super-agenda-ts-map (item)
  (when-let* ((marker (or (get-text-property 0 'org-marker item)
                          (get-text-property 0 'org-hd-marker item)))
              (ts (save-current-buffer
                    (org-with-point-at marker
                      (when (re-search-forward (concat "<" org-ts-regexp1
                                                       "[^>\n]\\{5,16\\}>")
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
