;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-memento)

(defmacro org-memento-with-test-context (file time-string &rest progn)
  (declare (indent 2))
  `(let ((org-memento-current-time ,(when time-string
                                      `(encode-time
                                        (parse-time-string ,time-string))))
         (org-memento-file ,(when file
                              `(expand-file-name (concat "testdata/" ,file)
                                                 (file-name-directory
                                                  (or load-file-name
                                                      (buffer-file-name)))))))
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

;;;; Time utilities

;; Here are too many test cases for private functions, but these functions
;; should be stable.

(describe "org-memento--today-string"
  (it "returns a date string"
    (org-memento-with-test-context nil "2020-01-01 12:00:00"
      (expect (org-memento--today-string (decode-time (org-memento--current-time)))
              :to-equal "2020-01-01"))))

(describe "org-memento--fill-decoded-time"
  (it "returns a date string"
    (expect (org-memento--fill-decoded-time
             (parse-time-string "2020-01-01"))
            :to-equal '(0 0 0 1 1 2020 nil -1 nil))))

(describe "org-memento--inactive-ts-string"
  (it "returns a string"
    (expect (org-memento--inactive-ts-string
             (encode-time (parse-time-string "2020-01-01 12:00:00")))
            :to-equal "[2020-01-01 Wed 12:00]")))

(describe "org-memento--parse-time-range"
  (it "returns a single duration"
    (expect (org-memento--parse-time-range "9:00")
            :to-equal '(540 . nil))
    (expect (org-memento--parse-time-range "06:00")
            :to-equal '(360 . nil)))
  (it "returns a time range"
    (expect (org-memento--parse-time-range "9:00-12:00")
            :to-equal '(540 . 720))))

(describe "org-memento--start-of-day"
  (it "returns the beginning of the date"
    (let ((org-extend-today-until 5))
      (expect (org-memento--start-of-day (parse-time-string "2020-01-01 12:00:00"))
              :to-equal
              (parse-time-string "2020-01-01 05:00:00"))
      (expect (org-memento--start-of-day (parse-time-string "2020-01-02 03:00:00"))
              :to-equal
              (parse-time-string "2020-01-01 05:00:00")))))

(describe "org-memento--make-ts-regexp"
  (it "matches a date during the preiod"
    (expect "<2020-01-01 Wed>"
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00"))))
    (expect "<2020-01-03 Fri>"
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00")))))
  (it "does not match a date out of the period"
    (expect "<2020-01-01 Wed>"
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00"))))
    (expect "<2020-01-03 Fri>"
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00"))))
    (expect "<2020-01-04 Sat>"
            :not
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00"))))
    (expect "<2019-12-31 Tue>"
            :not
            :to-match
            (org-memento--make-ts-regexp
             (encode-time (parse-time-string "2020-01-01 12:00:00"))
             (encode-time (parse-time-string "2020-01-03 12:00:00"))))))

(describe "org-memento--make-past-date-regexp"
  (it "matches a past date"
    (expect "2019-12-31"
            :to-match
            (org-memento--make-past-date-regexp (parse-time-string "2020-01-01"))))
  (it "does not match the current date"
    (expect "2020-01-01"
            :to-match
            (org-memento--make-past-date-regexp (parse-time-string "2020-01-01"))))
  (it "does not match a future date"
    (expect "2020-01-02"
            :to-match
            (org-memento--make-past-date-regexp (parse-time-string "2020-01-01")))))

(describe "org-memento--seconds-since-midnight"
  (it "returns the number of seconds"
    (expect (org-memento--seconds-since-midnight
             (encode-time (parse-time-string "2020-01-01 09:00:00")))
            :to-be-close-to 32400 1)))

(describe "org-memento--time-min"
  (it "returns a smaller time"
    (expect (org-memento--time-min
             (encode-time (parse-time-string "2020-01-01 09:00:00"))
             (encode-time (parse-time-string "2020-01-01 12:00:00")))
            :to-equal
            (encode-time (parse-time-string "2020-01-01 09:00:00")))
    (expect (org-memento--time-min
             (encode-time (parse-time-string "2020-01-01 09:00:00"))
             (encode-time (parse-time-string "2019-12-31 23:00:00")))
            :to-equal
            (encode-time (parse-time-string "2019-12-31 23:00:00"))))
  (it "returns a non-nil element if the other is nil"
    (expect (org-memento--time-min
             (encode-time (parse-time-string "2020-01-01 09:00:00"))
             nil)
            :to-equal
            (encode-time (parse-time-string "2020-01-01 09:00:00")))
    (expect (org-memento--time-min
             nil
             (encode-time (parse-time-string "2020-01-01 09:00:00")))
            :to-equal
            (encode-time (parse-time-string "2020-01-01 09:00:00"))))
  (it "returns nil if both are nil"
    (expect (org-memento--time-min nil nil)
            :to-be nil)))

(describe "org-memento--date-list"
  (it "returns a list of dates"
    (expect (org-memento--date-list
             (parse-time-string "2020-01-01")
             (parse-time-string "2020-01-03"))
            :to-equal
            (list (parse-time-string "2020-01-01 00:00:00")
                  (parse-time-string "2020-01-02 00:00:00")
                  (parse-time-string "2020-01-03 00:00:00")))
    (expect (org-memento--date-list
             (parse-time-string "2020-01-01")
             (parse-time-string "2020-01-01"))
            :to-equal
            (list (parse-time-string "2020-01-01 00:00:00")))))

(describe "org-memento--format-active-range"
  (it "returns an active timestamp with a range"
    (expect (org-memento--format-active-range
             (encode-time (parse-time-string "2020-01-01 09:00:00"))
             (encode-time (parse-time-string "2020-01-01 15:00:00")))
            :to-equal
            "<2020-01-01 Wed 09:00-15:00>")))

;;;; Generics

(describe "org-memento-block"
  (describe "on the current daily entry"
    (let ((block (org-memento-with-test-context "memento1.org" "2020-01-01 12:00:00"
                   (org-memento-with-today-entry
                    (org-memento-block-entry)))))
      (it "The started time is the check-in time"
        (expect (org-memento-started-time block)
                :to-be-close-to
                (org-memento-test--float-time "2020-01-01 09:00:00")
                1))
      (it "The started time is the scheduled time"
        (expect (org-memento-starting-time block)
                :to-be nil))
      (it "The ended time is the closed time"
        (expect (org-memento-ended-time block)
                :to-be nil)))))

(describe "org-memento-template"
  (describe "read with org-memento-templates function"
    (let ((alist (thread-last
                   (org-memento-with-test-context "memento1.org" nil
                     (org-memento-templates))
                   (mapcar (lambda (x)
                             (cons (org-memento-template-title x)
                                   x))))))

      (describe "loading"
        (it "skips archived entries"
          (expect (assoc "English" alist)
                  :to-be nil)
          (expect (assoc "Writing" alist)
                  :to-be nil)))

      (describe "source"
        (it "is the spec of its source"
          (expect (org-memento-template-source (cdr (assoc "Emacs" alist)))
                  :to-equal
                  '(file+olp org-memento-file "Notes" "Templates"))))

      (describe "relative-olp"
        (it "is the olp from the source"
          (expect (org-memento-template-relative-olp (cdr (assoc "Emacs" alist)))
                  :to-equal
                  '("Emacs"))
          (expect (org-memento-template-relative-olp (cdr (assoc "Coding Exercises" alist)))
                  :to-equal
                  '("Programming" "Coding Exercises"))))

      (describe "leaf-p"
        (it "is t if it has no children"
          (expect (org-memento-template-leaf-p (cdr (assoc "Emacs" alist)))
                  :to-be t)
          (expect (org-memento-template-leaf-p (cdr (assoc "Speaking" alist)))
                  :to-be t)
          (expect (org-memento-template-leaf-p (cdr (assoc "Weekly Retrospective" alist)))
                  :to-be t))
        (it "is nil if it has children"
          (expect (org-memento-template-leaf-p (cdr (assoc "Spanish" alist)))
                  :to-be nil)))

      (describe "category"
        (it "is inherited from the top-level"
          (expect (org-memento-template-category (cdr (assoc "Emacs" alist)))
                  :to-equal
                  "Emacs")
          (expect (org-memento-template-category (cdr (assoc "Speaking" alist)))
                  :to-equal
                  "Spanish"))
        (it "respects the property"
          (expect (org-memento-template-category (cdr (assoc "Retrospectives" alist)))
                  :to-equal
                  "Work")
          (expect (org-memento-template-category (cdr (assoc "Weekly Retrospective" alist)))
                  :to-equal
                  "Work")))

      (describe "tags"
        (it "is nil unless specified"
          (expect (org-memento-template-tags (cdr (assoc "Spanish" alist)))
                  :to-be nil))
        (it "is inherited"
          (expect (org-memento-template-tags (cdr (assoc "Weekly Retrospective" alist)))
                  :to-equal
                  '("retrospectives"))))

      (describe "normal-dows"
        (it "is a list of numbers"
          (expect (org-memento-template-normal-dows (cdr (assoc "Speaking" alist)))
                  :to-equal
                  '(0 1 2 3 4 5 6))))

      (describe "normal-hour"
        (it "can be relative"
          (expect (org-memento-template-normal-hour (cdr (assoc "Emacs" alist)))
                  :to-equal
                  '(relative 480 nil)))

        (it "can be a relative range"
          (expect (org-memento-template-normal-hour (cdr (assoc "Daily Review" alist)))
                  :to-equal
                  '(relative 420 450)))

        (it "can be absolute"
          (expect (org-memento-template-normal-hour (cdr (assoc "Weekly Retrospective" alist)))
                  :to-equal
                  '(absolute 900 nil)))

        (it "can be an absolute range"
          (expect (org-memento-template-normal-hour (cdr (assoc "Daily Meeting" alist)))
                  :to-equal
                  '(absolute 600 610))))

      (describe "duration"
        (it "is the number of minutes"
          (expect (org-memento-template-duration (cdr (assoc "Emacs" alist)))
                  :to-be 20))
        (it "is nil if unset"
          (expect (org-memento-template-duration (cdr (assoc "Retrospectives" alist)))
                  :to-be nil))))))

(describe "org-memento-open-today"
  (it "If the initial point is on a past date, moves the point")
  (it "If the initial point is on a future date, moves the point")
  (it "If the initial point is inside the date subtree, don't move the point"))

;;;; Scaffolding

(describe "Default scaffolding"
  (let ((blocks (org-memento-with-test-context "memento1.org" "2020-01-02 08:30:00"
                  (org-memento-with-today-entry
                   (org-memento-revert-modifications
                    (org-memento--maybe-check-in)
                    (org-memento--scaffold-day)
                    (org-memento-status)
                    org-memento-status-data)))))
    (it "loads data"
      (expect (length blocks)
              ;; including 1 daily block
              :to-be 5)
      (expect (org-memento-test-find-item-by-title "Daily Meeting" blocks)
              :to-be-truthy)
      (expect (org-memento-test-find-item-by-title "Daily Review" blocks)
              :to-be-truthy)
      (expect (org-memento-test-find-item-by-title "Speaking" blocks)
              :to-be-truthy)
      (expect (org-memento-test-find-item-by-title "Vocabulary study" blocks)
              :to-be-truthy))

    (it "sets tags"
      (expect (thread-last
                (org-memento-test-find-item-by-title "Daily Review" blocks)
                (org-memento-headline-element)
                (org-element-property :tags))
              :to-equal '("retrospectives")))

    (it "sets a timestamp"
      (expect (thread-last
                (org-memento-test-find-item-by-title "Daily Meeting" blocks)
                (org-memento-starting-time))
              :to-be-close-to
              (org-memento-test--float-time "2020-01-02 10:00:00")
              1)
      (expect (thread-last
                (org-memento-test-find-item-by-title "Daily Meeting" blocks)
                (org-memento-ending-time))
              :to-be-close-to
              (org-memento-test--float-time "2020-01-02 10:10:00")
              1)
      (expect (thread-last
                (org-memento-test-find-item-by-title "Daily Review" blocks)
                (org-memento-starting-time))
              :to-be-close-to
              (org-memento-test--float-time "2020-01-02 15:30:00")
              1)
      (expect (thread-last
                (org-memento-test-find-item-by-title "Daily Review" blocks)
                (org-memento-ending-time))
              :to-be-close-to
              (org-memento-test--float-time "2020-01-02 16:00:00")
              1))

    (it "inserts Effort property"
      (expect (thread-last
                (org-memento-test-find-item-by-title "Speaking" blocks)
                (org-memento-duration))
              :to-be-close-to 15.0 1)
      (expect (thread-last
                (org-memento-test-find-item-by-title "Vocabulary study" blocks)
                (org-memento-duration))
              :to-be-close-to 15.0 1)))

  (it "inserts the body"
    (expect (org-memento-with-test-context "memento1.org" "2020-01-02 08:30:00"
              (org-memento-with-today-entry
               (org-memento-revert-modifications
                (org-memento--maybe-check-in)
                (org-memento--scaffold-day)
                (org-memento-status)
                (goto-char (thread-last
                             org-memento-status-data
                             (org-memento-test-find-item-by-title "Speaking")
                             (org-memento-block-hd-marker)))
                (org-end-of-meta-data t)
                (buffer-substring-no-properties
                 (point) (org-entry-end-position)))))
            :to-equal
            "Here should be an instruction.\n")))

;;;; Other functions

(describe "org-memento-goto-today"
  (it "goes to the start of the current date"
    (expect (org-memento-with-test-context "memento1.org" "2020-01-01 12:00:00"
              (save-window-excursion
                (save-current-buffer
                  (org-memento-goto-today)
                  (buffer-substring (point) (pos-eol)))))
            :to-equal "* 2020-01-01")))

(provide 'org-memento-test)
