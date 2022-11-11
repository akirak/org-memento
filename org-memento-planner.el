;;; org-memento-planner.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This file is deprecated. Maybe I will move
;; `org-memento-planner-policies-section' to somewhere else to allow its
;; independent use.

;;; Code:

(require 'org-memento-policy)
(require 'org-memento-timeline)

;;;; Constants

(defconst org-memento-planner-buffer "*Org-Memento Planner*")

;;;; Variables

(defvar org-memento-planner-span nil)
(defvar org-memento-planner-date-range nil)

;;;; Major mode

(define-derived-mode org-memento-planner-mode magit-section-mode
  "MmtPlanner"
  (setq-local revert-buffer-function #'org-memento-planner-revert))

;;;; Commands

;;;###autoload
(defun org-memento-planner (start-day end-day &key span)
  (interactive (pcase current-prefix-arg
                 ('(4)
                  `(,@(mapcar (lambda (d)
                                (format "%4d-%02d-%02d"
                                        (decoded-time-year d)
                                        (decoded-time-month d)
                                        (decoded-time-day d)))
                              (org-memento--read-date-range))
                    :span week))
                 ((pred numberp)
                  `(,@(org-memento-week-date-range current-prefix-arg)
                    :span week))
                 (_
                  `(,@(org-memento-week-date-range 1)
                    :span week))))
  (setq org-memento-planner-span span
        org-memento-planner-date-range (list start-day end-day))
  (with-current-buffer (get-buffer-create org-memento-planner-buffer)
    (org-memento-planner-mode)
    (org-memento-planner-revert)
    (pop-to-buffer (current-buffer))))

(defun org-memento-planner-revert (&rest _args)
  (unless (derived-mode-p 'org-memento-planner-mode)
    (user-error "Not in org-memento-planner-mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-memento-policy-maybe-load)
    (magit-insert-section (magit-section)
      (org-memento-planner-policies-section))
    (goto-char (point-min))))

(defun org-memento-planner--rules ()
  (org-memento-policy-rules
   :span org-memento-planner-span
   :start-date (car org-memento-planner-date-range)
   :end-date (cadr org-memento-planner-date-range)))

(defun org-memento-planner-policies-section ()
  (let ((rules (org-memento-planner--rules)))
    (cl-labels
        ((match-group (group-path x)
           (equal group-path
                  (seq-take (org-memento-group-path x)
                            (length group-path))))
         (budgetp (group-path x)
           (and (match-group group-path x)
                (org-memento-policy-budget-rule-p x)
                (not (eq 'limit (slot-value x 'level)))))
         (yieldp (group-path x)
           (and (match-group group-path x)
                (org-memento-yield-rule-p x)))
         (insert-group (level taxy)
           (magit-insert-section (group (taxy-name taxy))
             (magit-insert-heading
               (make-string (* 2 (1+ level)) ?\s)
               (propertize (funcall (plist-get (nth level org-memento-group-taxonomy)
                                               :format)
                                    (nth level (taxy-name taxy)))
                           'face
                           'magit-section-heading)
               (when (seq-find (apply-partially #'budgetp (taxy-name taxy)) rules)
                 " ★"))
             (dolist (subtaxy (taxy-taxys taxy))
               (insert-group (1+ level) subtaxy)))))
      (magit-insert-section (policies)
        (magit-insert-heading
          "Groups")
        (dolist (group (taxy-taxys (org-memento-policy-group-taxy
                                    (org-memento-policy-contexts))))
          (insert-group 0 group)))
      (insert ?\n))))

(provide 'org-memento-planner)
;;; org-memento-planner.el ends here
