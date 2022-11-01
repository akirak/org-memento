;;; org-memento-yield.el --- Task generators -*- lexical-binding: t -*-

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
  (pcase-let*
      (((map :max-days :max-count) (org-memento-yield-backtrack-spec rule))
       (group-path (oref (oref rule context) group-path)))
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
  (thread-first
    (cl-etypecase time-or-string
      (string
       (parse-time-string time-or-string))
      (number
       (org-memento--maybe-decrement-date
        (decode-time time-or-string))))
    (decoded-time-add (make-decoded-time :day n))
    (org-memento--start-of-day)))

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
                                      activities &key demand start end)
  (let* ((activity (car activities))
         (no-earlier-than (when (and activity (oref x interval))
                            (float-time (org-memento-yield--add-days
                                         (car activity)
                                         (oref x interval)))))
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
