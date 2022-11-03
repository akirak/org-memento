;;; org-memento-policy.el --- Policy enforcement for Org Memento -*- lexical-binding: t -*-

(require 'eieio)
(require 'org-memento)
(require 'lisp-mode)
(require 'taxy)
(require 'org-memento-date)

(defgroup org-memento-policy nil
  "Policy enforcement for Org Memento."
  :group 'org-memento)

;;;; Constants

(defconst org-memento-policy-span-types '(day week month))

;;;; Custom variables

(defcustom org-memento-policy-file (locate-user-emacs-file "org-memento/policies")
  ""
  :type 'file)

(defcustom org-memento-policy-rule-types
  '((:budget . org-memento-policy-make-budgets)
    (:yield . org-memento-yield-init-1))
  ""
  :type '(alist :key-type keyword
                :value-type (function :tag "Function that instantiate a subclass of\
 org-memento-policy-rule")))

;;;; Classes

(defconst org-memento-policy-context-props
  '(:archived
    :link))

(defclass org-memento-policy-context ()
  ((label :initarg :label)
   (group-path :initarg :group-path)
   (period :initarg :period :initform nil)
   (link :initarg :link :initform nil :type (or string null))
   (archived :initarg :archived :initform nil)))

(cl-defmethod org-memento-group-path ((x org-memento-policy-context))
  (oref x group-path))

(defclass org-memento-policy-rule ()
  ((context :initarg :context
            :type org-memento-policy-context)
   (description :initarg :description :initform nil)))

(cl-defmethod org-memento-group-path ((x org-memento-policy-rule))
  (oref (oref x context) group-path))

;;;; Variables

(defvar org-memento-policy-data nil)

;;;; Generics

;;;;; Checking against a date span

(cl-defgeneric org-memento-policy-effective-p (x start-date &optional end-date))
(cl-defmethod org-memento-policy-effective-p ((x org-memento-policy-context)
                                              start-date &optional end-date)
  (if-let (period (oref x period))
      (if end-date
          (org-memento-date-intersection-p start-date end-date period)
        (org-memento-date-within-span-p start-date period))
    t))

;;;;; Group matching

(cl-defgeneric org-memento-policy-group (x))
(cl-defmethod org-memento-policy-group ((x org-memento-policy-context))
  (oref x group-path))
(cl-defmethod org-memento-policy-group ((x org-memento-policy-rule))
  (oref (oref x context) group-path))



;;;; Functions

;;;;; Loading data

;;;###autoload
(defun org-memento-policy-load ()
  (interactive)
  (with-temp-buffer
    (when (and org-memento-policy-file
               (file-readable-p org-memento-policy-file))
      (insert-file-contents org-memento-policy-file))
    (org-memento-policy-mode)
    (goto-char (point-min))
    (setq org-memento-policy-data (org-memento-policy--read-all))))

;;;###autoload
(defun org-memento-policy-maybe-load ()
  (unless org-memento-policy-data
    (org-memento-policy-load)))

(defconst org-memento-policy-skip-regexp
  (rx (or (+ space) "\n"
          (and bol (* blank)
               ";" (* nonl)))))

(defun org-memento-policy--read-all ()
  "Read all policies from the buffer."
  (let (exps)
    (while (not (eobp))
      (if (looking-at org-memento-policy-skip-regexp)
          (goto-char (match-end 0))
        (push (read (current-buffer)) exps)))
    (org-memento-policy--routes (nreverse exps))))

(defun org-memento-policy--routes (exps)
  "Return a taxy of policies built from expressions."
  (let ((root (make-instance 'org-memento-policy-context
                             :label nil
                             :group-path nil)))
    (cl-labels
        ((build-rules (context key args)
           (apply (or (alist-get key org-memento-policy-rule-types)
                      (error "Key %s is not included in org-memento-policy-rule-types"
                             key))
                  context
                  args))
         (build-context (parent exp)
           (pcase-let*
               ((`(,type ,label . ,args) exp)
                (`(,props . ,rest) (org-memento-policy--split-args args))
                (`(,context-props . ,rule-props) (org-memento-policy--group-plist
                                                  props org-memento-policy-context-props))
                (context-args (cl-ecase type
                                (group (list :group-path
                                             (append (oref parent group-path)
                                                     (list label))))
                                (period (list :period
                                              (org-memento-date-span-parse label)))))
                (new-context (apply #'clone parent
                                    :label (list type label)
                                    (append context-props context-args))))
             (make-taxy
              :name new-context
              :taxys (mapcar (apply-partially #'build-context new-context) rest)
              :items (cl-loop for (key value) on rule-props by #'cddr
                              append (build-rules new-context key value))))))
      (make-taxy
       :taxys (mapcar (apply-partially #'build-context root) exps)))))

(defun org-memento-policy--split-args (args)
  "Split arguments into a pair of a plist and the rest."
  (let (plist)
    (catch 'done
      (while args
        (unless (keywordp (car args))
          (throw 'done t))
        (push (pop args) plist)
        (push (pop args) plist)))
    (cons (nreverse plist) args)))

(defun org-memento-policy--group-plist (plist props)
  (let (first second kwd)
    (while (setq kwd (pop plist))
      (if (memq kwd props)
          (setq first (nconc first (list kwd (pop plist))))
        (setq second (nconc second (list kwd (pop plist))))))
    (cons first second)))

(defun org-memento-policy-find-definition (group-path)
  "Return a marker to the definition of GROUP-PATH."
  (with-current-buffer (or (find-buffer-visiting org-memento-policy-file)
                           (find-file-noselect org-memento-policy-file))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-labels
            ((parse-subexp (path)
               (while (and (not (eobp))
                           (looking-at org-memento-policy-skip-regexp))
                 (goto-char (match-end 0)))
               (unless (eobp)
                 (let ((start (point))
                       (exp (read (current-buffer)))
                       (end (point)))
                   (pcase exp
                     ((and `(group ,node . ,_)
                           (guard (equal node (car path))))
                      (goto-char start)
                      (pop path)
                      (if path
                          (progn
                            (down-list)
                            (narrow-to-region (point) (1- end))
                            (parse-subexp path))
                        (point-marker)))
                     (`(,_ . ,_)
                      (or (save-excursion
                            (save-restriction
                              (goto-char start)
                              (down-list)
                              (narrow-to-region (point) (1- end))
                              (parse-subexp path)))
                          (parse-subexp path)))
                     (_
                      (parse-subexp path)))))))
          (parse-subexp group-path))))))

;;;;; Using the data

(cl-defun org-memento-policy-rules (&key span start-date end-date)
  "Return rules effective during a certain period."
  (let* ((start-date (parse-time-string start-date))
         (end-date (if end-date
                       (parse-time-string end-date)
                     (cl-ecase span
                       (day nil)
                       (week (decoded-time-add
                              start-date (make-decoded-time :day 6)))
                       (month (decoded-time-add
                               start-date (make-decoded-time :month 1 :day -1)))))))
    (cl-flet
        ((effectivep (rule)
           (let ((context (slot-value rule 'context)))
             (and (not (slot-value context 'archived))
                  (org-memento-policy-effective-p context start-date end-date)))))
      (seq-filter #'effectivep (taxy-flatten org-memento-policy-data)))))

(cl-defun org-memento-policy-group-taxy (items &key prepend append)
  (cl-labels
      ((group-subpath (n x)
         (when (nth n (org-memento-policy-group x))
           (seq-take (org-memento-policy-group x) (1+ n))))
       (make-nth-key (n)
         (apply-partially #'group-subpath n))
       (taker (item taxy)
         (taxy-take-keyed
           (append prepend
                   (mapcar #'make-nth-key
                           (number-sequence 0 (1- (length org-memento-group-taxonomy))))
                   append)
           item taxy)))
    (thread-last
      (make-taxy :take #'taker)
      (taxy-emptied)
      (taxy-fill items))))

(defun org-memento-policy-find-context (pred &optional start-date end-date)
  (let (result)
    (cl-labels
        ((search (taxy)
           (when (and (funcall pred (taxy-name taxy))
                      (or (not start-date)
                          (org-memento-policy-effective-p
                           (taxy-name taxy) start-date end-date)))
             (setq result taxy)
             (dolist (subtaxy (taxy-taxys taxy))
               (search subtaxy)))))
      (dolist (taxy (taxy-taxys org-memento-policy-data))
        (search taxy)))
    result))

(defun org-memento-policy-contexts ()
  "Return a flat list of contexts."
  (let (contexts)
    (cl-labels
        ((collect-contexts (taxy)
           (unless (slot-value (taxy-name taxy) 'archived)
             (push (taxy-name taxy) contexts)
             (dolist (subtaxy (taxy-taxys taxy))
               (collect-contexts subtaxy)))))
      (dolist (taxy (taxy-taxys org-memento-policy-data))
        (collect-contexts taxy))
      contexts)))

(defun org-memento-policy-group-leaves ()
  "Return leafs of group paths."
  (let (paths)
    (cl-labels
        ((collect (taxy)
           (if (taxy-taxys taxy)
               (dolist (subtaxy (taxy-taxys taxy))
                 (collect subtaxy))
             (push (taxy-name taxy) paths))))
      (dolist (taxy (taxy-taxys (org-memento-policy-group-taxy
                                 (org-memento-policy-contexts))))
        (collect taxy)))
    (cl-remove-duplicates paths :test #'equal)))

(defun org-memento-policy-find-context-by-group (group)
  (org-memento-policy-find-context
   (apply-partially
    (lambda (group x)
      (equal (seq-take (org-memento-group-path x)
                       (length group))
             group))
    group)))

(defun org-memento-policy-group-archived-p (group)
  "Return non-nil if GROUP is archived."
  (when-let (taxy (org-memento-policy-find-context-by-group group))
    (oref (taxy-name taxy) archived)))

;;;; Specific rules

;;;;; Budget rules

(defclass org-memento-policy-budget-rule (org-memento-policy-rule)
  ((span :initarg :span)
   (level :initarg :level)
   (duration-minutes :initarg :duration-minutes)))

(defun org-memento-policy-make-budgets (context &rest specs)
  (mapcar (lambda (spec)
            (pcase-exhaustive spec
              (`(,span ,level ,duration)
               (cl-assert (memq span '(day week month)))
               (cl-assert (memq level '(minimum goal limit)))
               (make-instance 'org-memento-policy-budget-rule
                              :context context
                              :span span
                              :level level
                              :duration-minutes
                              (org-duration-to-minutes duration)))))
          specs))

;;;; Miscellaneous utilities

(defun org-memento-policy-parse-duration (x)
  (save-match-data
    (if (string-match (rx bol (group (+ digit))
                          ":" (group (+ digit)) eol)
                      x)
        (+ (* 60 (string-to-number (match-string 1 x)))
           (string-to-number (match-string 2 x)))
      (error "Invalid format %s" x))))

(defun org-memento-policy--compare-span-types (a b)
  (< (cl-position a org-memento-policy-span-types)
     (cl-position b org-memento-policy-span-types)))

;;;; Major mode for editing the policy file

(defconst org-memento-policy-font-lock-keywords
  `((,(rx (group (or "group" "period")))
     (1 font-lock-function-name-face))
    (,(rx symbol-start (group ":" (+ alnum)))
     (1 font-lock-keyword-face))))

;;;###autoload
(define-derived-mode org-memento-policy-mode lisp-data-mode
  "MmtPolicy"
  (setq-local lisp-indent-function #'org-memento-policy-indent)
  (setq font-lock-defaults '((org-memento-policy-font-lock-keywords))))

(defun org-memento-policy-indent (indent-point state)
  (let* ((normal-indent (current-column))
         (innermost (elt state 1)))
    (if (and innermost
             (eq ?\( (char-syntax (char-after (scan-lists innermost 1 -1)))))
        normal-indent
      (if (and innermost
               (eq ?_ (char-syntax (char-after (scan-lists innermost 1 -1)))))
          (save-excursion
            (goto-char (scan-lists innermost 1 -1))
            (current-column))
        (lisp-indent-specform 1 state indent-point (current-column))))))

(provide 'org-memento-policy)
;;; org-memento-policy.el ends here
