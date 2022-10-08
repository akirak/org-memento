;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-memento)

(defconst org-memento-test-time-1
  (parse-time-string "2023-10-04 12:00"))

(defmacro org-memento-with-test-context (file time-string &rest progn)
  (declare (indent 2))
  `(let ((org-memento-current-time (encode-time
                                    (parse-time-string ,time-string)))
         (org-memento-file ,(when file
                              `(expand-file-name (concat "testdata/" ,file)
                                                 (file-name-directory
                                                  (or load-file-name
                                                      (buffer-file-name)))))))
     ,@progn))

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


(describe "org-memento-open-today"
  (it "If the initial point is on a past date, moves the point")
  (it "If the initial point is on a future date, moves the point")
  (it "If the initial point is inside the date subtree, don't move the point"))

(provide 'org-memento-test)
