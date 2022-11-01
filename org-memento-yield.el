;;; org-memento-yield.el --- Task generators -*- lexical-binding: t -*-

;;;; Classes and generics

(defclass org-memento-yield-rule (org-memento-policy-rule)
  ())

(cl-defgeneric org-memento-yield-some (yield-rule activities
                                                  &key demand start end slots)
  "Generate some plans according to a rule.

YIELD-RULE is an instance of `org-memento-yield-rule'.

ACTIVITIES is a list of recent activities in the group. Each
activity should be a list as contained in taxies returned by
`org-memento-activity-taxy'.

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

(cl-defmethod org-memento-yield-some ((x org-memento-yield-simple-rule)
                                      activities &key demand start end slots)

  )

(provide 'org-memento-yield)
;;; org-memento-yield.el ends here
