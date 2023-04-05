;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-memento)
(require 'org-memento-test-utils (expand-file-name "org-memento-test-utils.el"))

;;;; Time utilities

;; Here are too many test cases for private functions, but these functions
;; should be stable.

(describe "org-memento--today-string"
  (it "returns a date string"
    (org-memento-with-test-context nil "2020-01-01 12:00:00"
      (expect (org-memento--today-string (decode-time (org-memento--current-time)))
              :to-equal "2020-01-01"))))

(describe "org-memento-minutes-from-now"
  (it "returns the number of minutes from now"
    (org-memento-with-test-context nil "2020-01-01 12:00:00"
      (expect (org-memento-minutes-from-now
               (time-add (org-memento--current-time) (* 60 10)))
              :to-be 10))))

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
      (xit "The closing time is calculated")

      (it "The ended time is the closed time"
        (expect (org-memento-ended-time block)
                :to-be nil)))))

(describe "org-memento-open-today"
  (it "If the initial point is on a past date, moves the point")
  (it "If the initial point is on a future date, moves the point")
  (it "If the initial point is inside the date subtree, don't move the point"))

;;;; Analytics

(describe "org-memento--agenda-activities"
  (it "collects clock data in a range"
    (let* ((data (org-memento--agenda-activities
                  (org-memento-test--internal-time "2020-01-01 5:00:00")
                  (org-memento-test--internal-time "2020-01-01 23:59:59")
                  '("sample1.org")))
           (task1-1 (seq-filter (lambda (record)
                                  (equal (caddr record)
                                         "Task 1.1"))
                                data)))
      (expect (caar task1-1)
              :to-be-close-to
              (org-memento-test--float-time "2020-01-01 15:25:00")
              1)
      (expect (cadar task1-1)
              :to-be-close-to
              (org-memento-test--float-time "2020-01-01 18:09:00")
              1)
      (expect (length task1-1)
              :to-be
              2))))

(describe "org-memento-activity-taxy"
  (describe "If the day has an active timestamp range")
  (describe "If the day has no active timestamp range")
  (describe "If the date range contains past dates")
  (describe "If the date range contains future dates"))

;;;; Date spans

(describe "org-memento-date-within-span-p"
  (describe "With org-memento-date-span-1"
    (it "returns non-nil if the second argument is within the range"
      (expect (org-memento-date-within-span-p (parse-time-string "2022-10-10")
                                              (org-memento-date-span-parse "2022-10"))
              :to-be-truthy))
    (it "returns nil if the second argument is out of the range"
      (expect (org-memento-date-within-span-p (parse-time-string "2022-09-30")
                                              (org-memento-date-span-parse "2022-10"))
              :not :to-be-truthy)

      (expect (org-memento-date-within-span-p (parse-time-string "2022-11-01")
                                              (org-memento-date-span-parse "2022-10"))
              :not :to-be-truthy)))

  (describe "With org-memento-date-span-2"
    (it "returns non-nil if the second argument is within the range"
      (expect (org-memento-date-within-span-p (parse-time-string "2022-11-01")
                                              (org-memento-date-span-parse '("2022-10" .
                                                                             "2022-12")))
              :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2023-01-01")
                                              (org-memento-date-span-parse '("2022" .
                                                                             "2024")))
              :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2022-10-10")
                                              (org-memento-date-span-parse '("2022-10-01" .
                                                                             "2022-10-20")))
              :to-be-truthy))
    (it "returns nil if the second argument is out of the range"
      (expect (org-memento-date-within-span-p (parse-time-string "2022-09-30")
                                              (org-memento-date-span-parse '("2022-10" .
                                                                             "2022-12")))
              :not :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2023-10-01")
                                              (org-memento-date-span-parse '("2022-10" .
                                                                             "2022-12")))
              :not :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2021-12-31")
                                              (org-memento-date-span-parse '("2022" .
                                                                             "2024")))
              :not :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2025-01-01")
                                              (org-memento-date-span-parse '("2022" .
                                                                             "2024")))
              :not :to-be-truthy))
    (it "allows omitting one of the bounds"
      (expect (org-memento-date-within-span-p (parse-time-string "2022-10-10")
                                              (org-memento-date-span-parse '("2022-10" .
                                                                             nil)))
              :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2022-09-30")
                                              (org-memento-date-span-parse '("2022-10" .
                                                                             nil)))
              :not :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2022-09-30")
                                              (org-memento-date-span-parse '(nil . "2022-10")))
              :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2022-10-30")
                                              (org-memento-date-span-parse '(nil . "2022-10")))
              :to-be-truthy)
      (expect (org-memento-date-within-span-p (parse-time-string "2022-11-01")
                                              (org-memento-date-span-parse '(nil . "2022-10")))
              :not :to-be-truthy))))

(describe "org-memento-date-intersection-p"
  (describe "With org-memento-date-span-1"
    (it "returns non-nil if the two ranges have an intersection"
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-10")
                                               (parse-time-string "2022-10-20")
                                               (org-memento-date-span-parse "2022-10"))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-09-30")
                                               (parse-time-string "2022-10-01")
                                               (org-memento-date-span-parse "2022-10"))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-20")
                                               (parse-time-string "2022-11-05")
                                               (org-memento-date-span-parse "2022-10"))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-01")
                                               (parse-time-string "2022-10-05")
                                               (org-memento-date-span-parse "2022"))
              :to-be-truthy))
    (org-memento-date--final-date (parse-time-string "2022-10"))

    (it "returns nil if there is no intersection between the two ranges"
      (expect (org-memento-date-intersection-p (parse-time-string "2022-09-01")
                                               (parse-time-string "2022-09-30")
                                               (org-memento-date-span-parse "2022-10"))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2023-01-01")
                                               (parse-time-string "2023-10-10")
                                               (org-memento-date-span-parse "2022-10"))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2021-01-01")
                                               (parse-time-string "2021-10-10")
                                               (org-memento-date-span-parse "2022"))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2023-01-01")
                                               (parse-time-string "2023-01-02")
                                               (org-memento-date-span-parse "2022"))
              :not :to-be-truthy)))

  (describe "With org-memento-date-span-2"
    (it "returns non-nil if the two ranges have an intersection"
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-01")
                                               (parse-time-string "2022-11-01")
                                               (org-memento-date-span-parse '("2022-10" .
                                                                              "2022-12")))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2024-12-01")
                                               (parse-time-string "2024-12-31")
                                               (org-memento-date-span-parse '("2022" .
                                                                              "2024")))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-10")
                                               (parse-time-string "2022-10-11")
                                               (org-memento-date-span-parse '("2022-10-01" .
                                                                              "2022-10-20")))
              :to-be-truthy))
    (it "returns nil if there is no intersection betweeh the two date ranges"
      (expect (org-memento-date-intersection-p (parse-time-string "2022-09-01")
                                               (parse-time-string "2022-09-30")
                                               (org-memento-date-span-parse '("2022-10" .
                                                                              "2022-12")))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2023-10-01")
                                               (parse-time-string "2023-10-10")
                                               (org-memento-date-span-parse '("2022-10" .
                                                                              "2022-12")))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2021-12-01")
                                               (parse-time-string "2021-12-31")
                                               (org-memento-date-span-parse '("2022" .
                                                                              "2024")))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2025-01-01")
                                               (parse-time-string "2025-02-01")
                                               (org-memento-date-span-parse '("2022" .
                                                                              "2024")))
              :not :to-be-truthy))
    (it "allows omitting one of the bounds"
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-10")
                                               (parse-time-string "2022-10-20")
                                               (org-memento-date-span-parse '("2022-10" .
                                                                              nil)))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-09-01")
                                               (parse-time-string "2022-09-30")
                                               (org-memento-date-span-parse '("2022-10" .
                                                                              nil)))
              :not :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2021-12-01")
                                               (parse-time-string "2021-12-31")
                                               (org-memento-date-span-parse '(nil . "2022-10")))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-10-20")
                                               (parse-time-string "2022-10-30")
                                               (org-memento-date-span-parse '(nil . "2022-10")))
              :to-be-truthy)
      (expect (org-memento-date-intersection-p (parse-time-string "2022-11-01")
                                               (parse-time-string "2022-11-05")
                                               (org-memento-date-span-parse '(nil . "2022-10")))
              :not :to-be-truthy))))

(describe "org-memento-policy-load"
  (it "loads a policy file"
    (let* ((org-memento-file org-memento-test-policy-file)
           (policies (org-memento-policy-load)))
      (expect (taxy-p policies)
              :to-be-truthy)
      (expect (length (taxy-flatten policies))
              :to-be-greater-than 0))))

;;;; Other functions

(describe "org-memento-with-today-entry"
  (it "goes to the start of the current date"
    (expect (org-memento-with-test-context "memento1.org" "2020-01-01 12:00:00"
              (org-memento-with-today-entry
               (buffer-substring (point) (pos-eol))))
            :to-equal "* 2020-01-01")))

(describe "org-memento-map-away-events"
  (it "runs a given function on each entry under the idle heading"
    (expect (org-memento-with-test-context "memento2.org" "2020-01-01 12:00:00"
              (org-memento-map-away-events
               (lambda ()
                 (when (looking-at org-complex-heading-regexp)
                   (match-string-no-properties 4)))))
            :to-equal '("Interruption"
                        "Rest"
                        "Gym workout"
                        "Rest"))))

(describe "org-memento--parse-quick-event"
  (it "default"
    (expect (org-memento--parse-quick-event "Go to hell")
            :to-equal (list "Go to hell")))
  (it "parses duration"
    (expect (org-memento--parse-quick-event "Walk for 15 min")
            :to-equal (list "Walk" :duration 15))
    (expect (org-memento--parse-quick-event "Implement new features for 1:30")
            :to-equal (list "Implement new features" :duration 90))))

(provide 'org-memento-test)
