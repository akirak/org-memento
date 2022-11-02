;;; org-memento-yield.el --- Task generators -*- lexical-binding: t -*-

(require 'eieio)
(require 'org-memento)
(require 'org-memento-policy)

(defvar org-memento-group-cache)
(defvar org-memento-todo-keyword-for-success)

;;;; Classes and generics

(defclass org-memento-yield-rule (org-memento-policy-rule)
  ())

(defun org-memento-yield-instance-p (x)
  (object-of-class-p x 'org-memento-yield-rule))

(cl-defgeneric org-memento-yield-some (yield-rule activities
                                                  &key demand start end)
  "Generate some plans according to a rule.

YIELD-RULE is an instance of `org-memento-yield-rule'.

ACTIVITIES is a list of recent activities in the group. Each
activity should be either a list as contained in taxies returned
by `org-memento-activity-taxy' or an olp in `org-memento-file'.

DEMAND is a specification of activities the user needs to work
on. It is usually an amount of time in the scope, specified in
minutes.

START and END specify the time scope. If specified, they should
be unix seconds. It is possible to omit the start time, or both.
If the start time is omitted, the time scope starts from now. If
the end time is omitted, it implies the check out time of the
day, or some time when the next activity should occur.

SLOTs is a list of concrete time spans currently available for
the user. Each entry is in the (START END) form where START and
END are unix seconds. This argument can be nil, in which case the
yield rule will be given less hints on allocation. Note that some
of the slots may be occupied by activities in other groups. It is
up to the user to resolve conflicts between multiple rules.

The method should return a list of plans. The first item in the
returned value should represent a primary plan; it is the best
plan according to the rule. Each plan is a list of tasks in a
chronological order. Each task may be of `org-memento-order' or
another type.")

(cl-defgeneric org-memento-yield-backtrack-spec (x)
  "Return a plist defining a backtrack spec for a given yield rule.")

;;;; Helper functions to use yield rules

(defun org-memento-yield--activities-1 (rule taxy)
  (cl-check-type rule org-memento-yield-rule)
  (let* ((plist (org-memento-yield-backtrack-spec rule))
         ;; (max-days (plist-get plist :max-days))
         (max-count (plist-get plist :max-count))
         (context (eieio-oref rule 'context))
         (group-path (eieio-oref context 'group-path)))
    (cl-labels
        ((match-group (group)
           (equal (seq-take group (length group-path))
                  group-path))
         (map-record (record)
           (let ((plist (seq-drop record 5)))
             (when (and (equal (plist-get plist :todo-keyword)
                               org-memento-todo-keyword-for-success)
                        (match-group (plist-get plist :group)))
               record)))
         (test-cache (group _olp)
           (match-group group)))
      (if (eq max-count 1)
          ;; Retrieve only the latest activity of the group
          (when-let (record (or (thread-last
                                  (org-memento--map-taxy-blocks taxy #'map-record)
                                  (seq-sort-by #'car #'>)
                                  (car))
                                (thread-last
                                  (map-filter #'test-cache org-memento-group-cache)
                                  (mapcar #'cdr)
                                  (seq-sort-by #'car #'string>)
                                  (car))))
            (list record))
        (error "More than 1 of :max-count value is not implemented")))))

(defun org-memento-yield--add-days (time-or-string n)
  (let ((decoded-time (cl-etypecase time-or-string
                        (string
                         (parse-time-string time-or-string))
                        (number
                         (org-memento--start-of-day (decode-time time-or-string))))))
    (unless (decoded-time-hour decoded-time)
      (setf (decoded-time-hour decoded-time) (or org-extend-today-until 0)))
    (unless (decoded-time-minute decoded-time)
      (setf (decoded-time-minute decoded-time) 0))
    (unless (decoded-time-second decoded-time)
      (setf (decoded-time-second decoded-time) 0))
    (decoded-time-add decoded-time (make-decoded-time :day n))))

(cl-defun org-memento-yield-for-span (taxy span &key start-date end-date)
  (declare (indent 2))
  (pcase-let*
      ((`(,start-date ,end-date) (cond
                                  (start-date
                                   (list start-date
                                         (or end-date
                                             (cl-ecase span
                                               (day start-date)
                                               (week (org-memento-yield--add-days
                                                      start-date 6))))))
                                  ((eq span 'day)
                                   (list (org-memento--today-string)
                                         (org-memento--today-string)))
                                  ((eq span 'week)
                                   (org-memento-week-date-range 0))))
       (scope-end (thread-first
                    (parse-time-string end-date)
                    (org-memento--start-of-day)
                    (decoded-time-add (make-decoded-time :hour 23 :minute 59))
                    (encode-time)
                    (float-time)))
       (planned (when (eq span 'day)
                  (save-current-buffer
                    (thread-last
                      (org-memento--blocks)
                      (cl-remove-if #'org-memento-started-time)
                      (mapcar (lambda (block)
                                (org-with-point-at (org-memento-block-hd-marker block)
                                  (cons (org-memento--get-group
                                         (org-memento-block-headline block))
                                        block)))))))))
    (cl-labels
        ((match-group (group-path group)
           (equal group-path (seq-take group (length group-path))))
         (block-to-record (block)
           (let ((duration (org-memento-duration block)))
             (list (or (org-memento-starting-time block)
                       (when duration
                         (- scope-end (* 60 duration)))
                       (- scope-end 60))
                   (or (org-memento-starting-time block)
                       (- scope-end 1))
                   (org-memento-title block)
                   nil
                   nil)))
         (convert-car-time (record)
           (cons (cl-etypecase (car record)
                   (string (thread-first
                             (parse-time-string (car record))
                             (org-memento--start-of-day)
                             (encode-time)
                             (float-time)))
                   (number (car record)))
                 (cdr record)))
         (generate-tasks (yield-rule)
           (let ((group-path (thread-first
                               (slot-value yield-rule 'context)
                               (slot-value 'group-path))))
             (org-memento-yield-some
              yield-rule
              (thread-last
                (org-memento-yield--activities-1 yield-rule taxy)
                (append (thread-last
                          (cl-remove-if-not (apply-partially #'match-group group-path)
                                            planned :key #'car)
                          (mapcar #'cdr)
                          (mapcar #'block-to-record)))
                (mapcar #'convert-car-time)
                (seq-sort-by #'car #'>))
              :end scope-end))))
      (thread-last
        (org-memento-policy-rules
         :span span :start-date start-date :end-date end-date)
        (seq-filter #'org-memento-yield-instance-p)
        (mapcar (lambda (rule)
                  (cons rule (generate-tasks rule))))))))

;;;; Default constructor function that supports the built-in yield rules

;;;###autoload
(defun org-memento-yield-init-1 (context &rest args)
  (list (apply #'make-instance
               (pcase-exhaustive args
                 ((and (map :interval :static)
                       (guard interval)
                       (guard static))
                  'org-memento-yield-simple-rule))
               :context context
               args)))

;;;; Built-in yield rules

(defclass org-memento-yield-simple-rule (org-memento-yield-rule)
  ((interval :initarg :interval :initform nil)
   (static :initarg :static)))

(cl-defmethod org-memento-yield-backtrack-spec ((x org-memento-yield-simple-rule))
  (list :max-days (oref x interval)
        :max-count 1))

(cl-defmethod org-memento-yield-some ((x org-memento-yield-simple-rule)
                                      activities &key start end
                                      &allow-other-keys)
  (let* ((activity (car activities))
         (no-earlier-than (when (and activity (oref x interval))
                            (thread-first
                              (org-memento-yield--add-days
                               (car activity)
                               (oref x interval))
                              (encode-time)
                              (float-time))))
         (start (or start (float-time (org-memento--current-time))))
         (plist (oref x static))
         (obj (apply #'make-org-memento-order
                     :group (oref (oref x context) group-path)
                     (thread-first
                       plist
                       (plist-put :duration
                                  (when-let (duration (plist-get plist :duration))
                                    (cl-etypecase duration
                                      (string (org-duration-to-minutes duration))
                                      (number duration))))))))
    (unless (and end no-earlier-than
                 (> no-earlier-than end))
      (when (and no-earlier-than
                 (> no-earlier-than start))
        (setf (org-memento-order-no-earlier-than obj) no-earlier-than))
      (list (list obj)))))

(provide 'org-memento-yield)
;;; org-memento-yield.el ends here
