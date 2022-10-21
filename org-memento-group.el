;;; org-memento-group.el --- Display a dashboard grouped by hierarchy -*- lexical-binding: t -*-

(require 'org-memento)
(require 'taxy)
(require 'magit-section)

(defconst org-memento-group-buffer
  "*Org-Memento Groups*")

(defcustom org-memento-group-default-span 7
  ""
  :type 'number)

(defcustom org-memento-group-depth 1
  ""
  :type 'number)

(defcustom org-memento-group-hook
  '(org-memento-group-insert-groups)
  "Hook run to insert contents into the buffer.

Each function in this hook takes a taxy as an argument."
  :type 'hook)

(defvar org-memento-group-date-range nil)

;;;###autoload
(defun org-memento-group-timeline (start-day end-day)
  (interactive (if (equal current-prefix-arg '(4))
                   (list (org-read-date)
                         (org-read-date))
                 (let ((today (thread-first
                                (decode-time)
                                (org-memento--start-of-day))))
                   (mapcar `(lambda (n)
                              (thread-last
                                (make-decoded-time :day (- n))
                                (decoded-time-add ',today)
                                (encode-time)
                                (format-time-string "%F")))
                           (list org-memento-group-default-span 1)))))
  (with-current-buffer (get-buffer-create org-memento-group-buffer)
    (org-memento-group-mode)
    (setq-local org-memento-group-date-range (list start-day end-day))
    (org-memento-group-revert)
    (pop-to-buffer (current-buffer))))

(defun org-memento-group-revert (&rest _args)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (run-hook-with-args 'org-memento-group-hook
                        (apply #'org-memento-group-taxy org-memento-group-date-range)))
  (goto-char (point-min)))

(defun org-memento-group-insert-groups (root-taxy)
  (cl-labels
      ((item-date (item)
         (nth 2 item))
       (item-duration (item)
         (/ (- (cadr item) (car item)) 60))
       (group-total (taxy)
         (-sum (append (mapcar #'group-total (taxy-taxys taxy))
                       (mapcar #'item-duration (taxy-items taxy)))))
       (insert-group (level parent-path taxy)
         (let ((path (append parent-path
                             (when (car (taxy-name taxy))
                               (list (cdr (taxy-name taxy)))))))
           (magit-insert-section (group (cdr (taxy-name taxy)))
             (magit-insert-heading
               (make-string (* 2 level) ?\s)
               (if-let (depth (car (taxy-name taxy)))
                   (if-let (fn (nth depth org-memento-group-formatters))
                       (funcall fn (cdr (taxy-name taxy)))
                     (cdr (taxy-name taxy)))
                 (cdr (taxy-name taxy)))
               (propertize (format " (%s)" (org-memento--format-duration (group-total taxy)))
                           'face 'font-lock-comment-face))
             (dolist (item (taxy-items taxy))
               (magit-insert-section (item (list (nth 2 item)
                                                 (nth 3 item)))
                 (magit-insert-heading)
                 (insert (make-string (* 2 (1+ level)) ?\s)
                         (nth 3 item)
                         (format " (%s)" (org-memento--format-duration (item-duration item)))
                         "\n")))
             (dolist (child (taxy-taxys taxy))
               (insert-group (1+ level) path child))))))
    (magit-insert-section (magit-section)
      (magit-insert-heading
        (nth 0 org-memento-group-date-range)
        "/"
        (nth 1 org-memento-group-date-range))
      (dolist (taxy (taxy-taxys root-taxy))
        (insert-group 0 nil taxy)))))

(defun org-memento-group-taxy (start-day end-day &optional depth)
  (let ((depth (or depth (length org-memento-group-formatters))))
    (cl-labels
        ((nth-key (n)
           `(lambda (item)
              (cons ,n (nth ,n (car item)))))
         (take-date (item)
           (cons nil (nth 2 (cdr item))))
         (taker (item taxy)
           (taxy-take-keyed
             (-insert-at org-memento-group-depth
                         #'take-date
                         (mapcar #'nth-key (number-sequence 0 (1- depth))))
             item taxy)))
      (thread-last
        (make-taxy :name "Groups" :take #'taker)
        (taxy-emptied)
        (taxy-fill (org-memento--collect-groups-1 start-day end-day))
        (taxy-mapcar #'cdr)))))

;;;; Major mode

(defvar org-memento-group-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'org-memento-group-open-entry)
    (define-key map (kbd "SPC") #'org-memento-group-show-entry)
    map))

(define-derived-mode org-memento-group-mode
  magit-section-mode
  "Org-Memento Group"
  "Major mode for displaying activity groups."
  (setq-local revert-buffer-function #'org-memento-group-revert))

;;;; Commands

(defun org-memento-group-open-entry ()
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value)))
    (when (eq (oref section type) 'item)
      (pcase-exhaustive value
        (`(,date ,title)
         (org-memento-timeline--display-entry
          (org-find-olp (list org-memento-file date title))
          #'pop-to-buffer))))))

(defun org-memento-group-show-entry ()
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (oref section value)))
    (when (eq (oref section type) 'item)
      (pcase-exhaustive value
        (`(,date ,title)
         (org-memento-timeline--display-entry
          (org-find-olp (list org-memento-file date title))
          #'display-buffer))))))

(defun org-memento-group-export (file &optional append)
  "Export the data to CSV."
  (interactive "FOutput file: ")
  (unless (derived-mode-p 'org-memento-group-mode)
    (user-error "You must run this command from inside org-memento-group-mode"))
  (org-memento-export-groups-to-csv file append org-memento-group-date-range))

(provide 'org-memento-group)
;;; org-memento-group.el ends here
