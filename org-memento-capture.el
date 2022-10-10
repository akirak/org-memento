;;; org-memento-capture.el --- Transient-based capture command for Org Memento -*- lexical-binding: t -*-

(require 'org-memento)
(require 'transient)

(defgroup org-memento-capture nil
  "Transient-based capture command for Org Mement."
  :group 'org-memento)

;;;; Infix commands

(defclass org-memento-capture--variable-class (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj org-memento-capture--variable-class))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-set ((obj org-memento-capture--variable-class) value)
  (oset obj value value)
  (set (oref obj variable) value))

;;;;; Date

(defvar org-memento-capture--date nil
  "Date to work on.

The value should be an internal time of the midnight on the date.")

(defun org-memento-capture--default-date ()
  ;; TODO: In org-agenda-mode, use the current date
  (encode-time
   (org-memento--set-time-of-day
    (org-memento--maybe-decrement-date (decode-time))
    0 0 0)))

(defclass org-memento-capture--date-class (org-memento-capture--variable-class)
  ())

(cl-defmethod transient-infix-read ((obj org-memento-capture--date-class))
  (let ((org-read-date-popup-calendar nil)
        (value (or (oref obj value) (org-memento-capture--default-date))))
    (thread-first
      (org-read-date nil t nil nil value)
      ;; Set the time to the midnight
      (decode-time)
      (org-memento--set-time-of-day 0 0 0)
      (encode-time))))

(cl-defmethod transient-format-value ((obj org-memento-capture--date-class))
  (if-let (value (oref obj value))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize (format-time-string "%F" value)
                   'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    "today"))

(transient-define-infix org-memento-capture--date-infix ()
  :description "Date"
  :class 'org-memento-capture--date-class
  :variable 'org-memento-capture--date)

;;;;; Time range

(defvar org-memento-capture--time nil
  "Time range in (START END) format.

START and END are internal time representations.")

(defclass org-memento-capture--time-class (org-memento-capture--variable-class)
  ())

(cl-defmethod transient-infix-read ((obj org-memento-capture--time-class))
  (let* ((value (oref obj value))
         (midnight (or org-memento-capture--date
                       (org-memento-capture--default-date))))
    (cl-flet*
        ((to-time (time)
           (time-add midnight (* 60 time)))
         (read-time (initial start)
           (pcase (org-memento-read-time-of-day
                   :start-time start
                   :decoded-date (decode-time org-memento-capture--date)
                   :initial-value (when initial
                                    (/ (- (float-time initial)
                                          (float-time midnight))
                                       60)))
             (`nil)
             (`(,newstart ,newend)
              (list (to-time newstart)
                    (to-time newend)))
             (time
              (to-time time)))))
      (let ((t1 (read-time (car value) nil)))
        (pcase t1
          (`(,_ ,_)
           t1)
          (start
           (list start (read-time (nth 1 value) start))))))))

(cl-defmethod transient-format-value ((obj org-memento-capture--time-class))
  (pcase (oref obj value)
    (`(,start ,end)
     (concat
      (propertize "(" 'face 'transient-inactive-value)
      (propertize (concat (format-time-string "%R" start)
                          "-"
                          (format-time-string "%R" end))
                  'face 'transient-value)
      (propertize ")" 'face 'transient-inactive-value)))
    (_
     "")))

(transient-define-infix org-memento-capture--time-infix ()
  :description "Time range"
  :class 'org-memento-capture--time-class
  :variable 'org-memento-capture--time)

;;;; Suffix commands

(defun org-memento-capture-log ()
  "Log a past event."
  (interactive)
  (apply #'org-memento-log
         (org-memento-capture--read-time-span :past t)))

(defun org-memento-capture-block ()
  "Add a time block scheduled on today."
  (interactive)
  (apply #'org-memento-schedule-block
         (org-memento-capture--read-time-span :future t)))

;;;; Prefix commands

;;;###autoload (autoload 'org-memento-capture "org-memento-capture" nil 'interactive)
(transient-define-prefix org-memento-capture ()
  "Main entry point to capture commands for Memento."
  ["With a specific time/range"
   :class transient-subgroups
   [("-d" org-memento-capture--date-infix)]
   ["Past"
    ("l" "Log" org-memento-capture-log)]
   ["Future"
    ;; today's time block
    ;; away time
    ]]
  ["Without time"
   ;; ("c" "Category template" org-memento-add-template)
   ]
  (interactive)
  (transient-setup 'org-memento-capture))

;;;; Helper functions

(cl-defun org-memento-capture--read-time-span (&key past future)
  (let ((midnight (or org-memento-capture--date
                      (org-memento-capture--default-date))))
    (cl-flet*
        ((to-time (time)
           (time-add midnight (* 60 time)))
         (read-time (start)
           (pcase (org-memento-read-time-of-day
                   :past past :future future
                   :start-time start
                   :decoded-date (decode-time (or org-memento-capture--date
                                                  (org-memento-capture--default-date))))
             (`nil)
             (`(,newstart ,newend)
              (list (to-time newstart)
                    (to-time newend)))
             (time
              (to-time time)))))
      (let ((t1 (read-time nil)))
        (pcase t1
          (`(,_ ,_)
           t1)
          (start
           (list start (read-time start))))))))

(provide 'org-memento-capture)
;;; org-memento-capture.el ends here
