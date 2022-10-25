;;; org-memento-date.el --- Date spans -*- lexical-binding: t -*-

(require 'eieio)

(defclass org-memento-date-span ()
  ())

(defclass org-memento-date-span-1 (org-memento-date-span)
  ((on :initarg :on)))

(defclass org-memento-date-span-2 (org-memento-date-span)
  ((start :initarg :start)
   (end :initarg :end)))

(defun org-memento-date-span-parse (x)
  (pcase-exhaustive x
    ((pred stringp)
     (make-instance 'org-memento-date-span-1
                    :on (parse-time-string x)))
    (`(,start . ,end)
     (make-instance 'org-memento-date-span-2
                    :start (when start (parse-time-string start))
                    :end (when end (parse-time-string end))))))

(cl-defgeneric org-memento-date-within-span-p (date span)
  "Return non-nil if DATE is within SPAN.")

(cl-defmethod org-memento-date-within-span-p (date
                                              (x org-memento-date-span-1))
  (seq-every-p (apply-partially (lambda (ref target field)
                                  (if (funcall field ref)
                                      (equal (funcall field ref)
                                             (funcall field target))
                                    t))
                                (oref x on)
                                date)
               '(decoded-time-year
                 decoded-time-month
                 decoded-time-day)))

(cl-defmethod org-memento-date-within-span-p (date
                                              (x org-memento-date-span-2))
  (and (if (oref x start)
           (org-memento-date--le (oref x start) date)
         t)
       (if (oref x end)
           (org-memento-date--le date (oref x end))
         t)))

(cl-defgeneric org-memento-date-intersection-p (start-date
                                                end-date
                                                span)
  "Return non-nil if two date spans have an intersection.")

(cl-defmethod org-memento-date-intersection-p (start-date
                                               end-date
                                               (x org-memento-date-span-1))
  (let ((span (oref x on)))
    (org-memento-date-span-has-intersection-p
     start-date end-date
     (org-memento-date--initial-date span)
     (org-memento-date--final-date span))))

(cl-defmethod org-memento-date-intersection-p (start-date
                                               end-date
                                               (x org-memento-date-span-2))
  (org-memento-date-span-has-intersection-p
   start-date end-date
   (when-let (span-start (oref x start))
     (org-memento-date--initial-date span-start))
   (when-let (span-end (oref x end))
     (org-memento-date--final-date span-end))))

(defun org-memento-date-span-has-intersection-p (start-date-1
                                                 end-date-1
                                                 start-date-2
                                                 end-date-2)
  "Return non-nil if two date ranges have an intersection

Either START-DATE-2 or END-DATE-2 can be omitted."
  (and (not (and end-date-2
                 (org-memento-date--le end-date-2 start-date-1)))
       (not (and start-date-2
                 (org-memento-date--le end-date-1 start-date-2)))))

(defun org-memento-date--initial-date (decoded-time)
  (let ((date (copy-sequence decoded-time)))
    (unless (decoded-time-month date)
      (setf (decoded-time-month date) 1))
    (unless (decoded-time-day date)
      (setf (decoded-time-day date) 1))
    date))

(defun org-memento-date--final-date (decoded-time)
  (let ((date (copy-sequence decoded-time)))
    (unless (decoded-time-month date)
      (setf (decoded-time-month date) 12))
    (if (decoded-time-day date)
        date
      (setf (decoded-time-day date) 1)
      (decoded-time-add date (make-decoded-time :month 1 :day -1)))))

(defun org-memento-date--le (decoded-date-1 decoded-date-2)
  (let ((year1 (decoded-time-year decoded-date-1))
        (year2 (decoded-time-year decoded-date-2))
        (month1 (decoded-time-month decoded-date-1))
        (month2 (decoded-time-month decoded-date-2))
        (day1 (decoded-time-day decoded-date-1))
        (day2 (decoded-time-day decoded-date-2)))
    ;; Both dates must contain at least the year field
    ;; At least one of the arguments must be a complete date
    (or (< year1 year2)
        (and (= year1 year2)
             (or (null month1)
                 (null month2)
                 (< month1 month2)
                 (and (= month1 month2)
                      (or (null day1)
                          (null day2)
                          (< day1 day2))))))))

(provide 'org-memento-date)
;;; org-memento-date.el ends here
