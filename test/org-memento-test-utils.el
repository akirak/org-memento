;;; org-memento-test-utils.el ---  -*- lexical-binding: t -*-

(defmacro org-memento-with-test-context (file time-string &rest progn)
  (declare (indent 2))
  `(let ((org-memento-current-time ,(when time-string
                                      `(encode-time
                                        (parse-time-string ,time-string))))
         (org-memento-file ,(when file
                              `(expand-file-name ,file
                                                 (when-let (filename
                                                            (or load-file-name
                                                                (buffer-file-name)))
                                                   (file-name-directory filename))))))
     ,@progn))

(defmacro org-memento-revert-modifications (&rest progn)
  `(let ((handle (prepare-change-group)))
     (unwind-protect
         (progn
           ,@progn)
       (cancel-change-group handle))))

(defun org-memento-test-find-item-by-title (title items)
  (seq-find (lambda (block)
              (equal (org-memento-title block)
                     title))
            items))

(defun org-memento-test--internal-time (string)
  (encode-time (parse-time-string string)))

(defun org-memento-test--float-time (string)
  (float-time (encode-time (parse-time-string string))))

(provide 'org-memento-test-utils)
;;; org-memento-test-utils.el ends here
