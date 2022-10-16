;;; org-memento-timeline.el --- Visualize an activity timeline -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org-memento "0.1") (magit-section "3.3") (taxy "0.10") (dash "2.19"))
;; Keywords: calendar
;; URL: https://git.sr.ht/~akirak/org-memento

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `org-memento-timeline'.

;;; Code:

(require 'org-memento)
(require 'taxy)
(require 'dash)
(require 'magit-section)

(defgroup org-memento-timeline nil
  "Activity timeline."
  :prefix "org-memento-"
  :group 'org-memento)

(defconst org-memento-timeline-ms-buffer "*Org-Memento Timeline*")

;;;; Custom variables

(defcustom org-memento-timeline-pre-hook
  '(org-memento-timeline-insert-insights)
  "Hook run before the timeline sections are inserted.

The hook is run inside the timeline buffer.

Each function in the hook takes the taxy representing the
timeline as an argument."
  :type 'hook)

(defcustom org-memento-timeline-post-hook
  nil
  "Hook run after the timeline sections are inserted.

The hook is run inside the timeline buffer.

Each function in the hook takes the taxy representing the
timeline as an argument."
  :type 'hook)

;;;; Faces

(defface org-memento-timeline-time-face
  '((t (:inherit default :slant italic)))
  "Face for time ranges.")

(defface org-memento-timeline-active-face
  '((((class color) (min-colors 88) (background dark))
     :background "HotPink3")
    (((class color) (min-colors 88) (background light))
     :background "LightPink")
    (t (:inherit default :foreground "" :background "")))
  "Face for an item at the current time.")

;;;; Display the timeline

(defvar org-memento-timeline-date-range nil)

;;;###autoload
(defun org-memento-timeline (start-day end-day)
  (interactive (if (equal current-prefix-arg '(4))
                   (list (org-read-date)
                         (org-read-date))
                 (let ((today (org-memento--today-string (decode-time))))
                   (list today today))))
  (when (string-lessp end-day start-day)
    (user-error "The end day must be no earlier than the start day"))
  (with-current-buffer (get-buffer-create org-memento-timeline-ms-buffer)
    (magit-section-mode)
    (setq-local org-memento-timeline-date-range (list start-day end-day)
                revert-buffer-function #'org-memento-timeline-revert)
    (org-memento-timeline-revert)
    (pop-to-buffer (current-buffer))))

(defun org-memento-timeline-revert (&rest _args)
  (interactive)
  (let ((taxy (apply #'org-memento-activity-taxy
                     org-memento-timeline-date-range)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (run-hook-with-args 'org-memento-timeline-pre-hook taxy)
      (org-memento-timeline--insert taxy)
      (run-hook-with-args 'org-memento-timeline-post-hook taxy))))

(defun org-memento-timeline--insert (root-taxy)
  ;; TODO: Maybe set magit-section-set-visibility-hook
  (let ((now (float-time (org-memento--current-time))))
    (cl-labels
        ((get-record (item)
           (if (taxy-p item)
               (taxy-name item)
             item))
         (start-time (item)
           (car (get-record item)))
         (end-time (item)
           (cadr (get-record item)))
         (title (item)
           (caddr (get-record item)))
         (sort-trees (items)
           (cl-sort items #'< :key #'start-time))
         (format-time-range (start end)
           (concat (if start
                       (format-time-string "%R" start)
                     "")
                   "–"
                   (if end
                       (format-time-string "%R" end)
                     "")
                   (if (and end start)
                       (format " (%s)"
                               (org-duration-from-minutes
                                (/ (- end start) 60)))
                     "")))
         (unfinished-clock-p (item)
           (eq (nth 4 item) 'clock-unfinished))
         (highlight-previous-line ()
           (put-text-property (pos-bol 0) (1- (pos-bol))
                              'face 'org-memento-timeline-active-face))
         (insert-items (items end-time-of-block)
           (when items
             (let ((indent1 (make-string 6 ?\s))
                   (indent2 (make-string 15 ?\s)))
               (dolist (group (-partition-by #'caddr items))
                 (magit-insert-section (group (car group)
                                              'hide)
                   (let ((marker (cadddr (car group)))
                         (title (caddr (car group))))
                     (magit-insert-heading
                       indent1
                       (format-time-string "%R" (car (car group)))
                       (make-string 2 ?\s)
                       (if title
                           (propertize (org-link-display-format title)
                                       'face 'magit-section-heading)
                         (when (cadr (car group))
                           (propertize "Gap" 'face 'font-lock-comment-face)))
                       (format " (%s)\n"
                               (org-duration-from-minutes
                                (/ (- (cadr (car group))
                                      (car (car group)))
                                   60))))
                     (when (unfinished-clock-p (car (last group)))
                       (highlight-previous-line))
                     (when (or title (> (length group) 1))
                       (dolist (clock group)
                         (insert indent2
                                 (format-time-range (start-time clock)
                                                    (end-time clock))
                                 (if (unfinished-clock-p clock)
                                     " (continuing)"
                                   "")
                                 "\n")
                         (when (unfinished-clock-p clock)
                           (highlight-previous-line)))))))
               (let ((last-entry (car (last items))))
                 (unless (eq (nth 4 last-entry)
                             'clock-unfinished)
                   (when-let (last-end (cadr last-entry))
                     (magit-insert-section (clock-out)
                       (magit-insert-heading
                         indent1
                         (format-time-string "%R" last-end)
                         (make-string 2 ?\s)
                         (if (< last-end now)
                             (concat (propertize "Clocked out" 'face 'font-lock-comment-face)
                                     (if end-time-of-block
                                         (format " (until %s)"
                                                 (format-time-string "%R" end-time-of-block))
                                       ""))
                           (propertize "Ending" 'face 'font-lock-comment-face))
                         "\n"))))))
             (insert ?\n)))
         (insert-block (taxy)
           (magit-insert-section (block (taxy-name taxy) 'hide)
             (let ((indent1 (make-string 4 ?\s))
                   (indent2 (make-string 11 ?\s))
                   (indent2 (make-string 13 ?\s))
                   (start (start-time taxy))
                   (end (end-time taxy))
                   (nowp (eq 'now (nth 4 (taxy-name taxy)))))
               (if nowp
                   (magit-insert-heading
                     indent1
                     (make-string 6 ?-)
                     (propertize " Now " 'face 'font-lock-comment-face)
                     (make-string (- (window-width)
                                     (+ 4 6 5
                                        ;; margin
                                        5))
                                  ?-))
                 (magit-insert-heading
                   indent1
                   (if start
                       (format-time-string "%R- " start)
                     (make-string 7 ?\s))
                   (if-let (title (title taxy))
                       (propertize title
                                   'face (if (eq (cadddr (cdr (get-record taxy))) 'idle)
                                             'font-lock-comment-face
                                           'magit-section-heading))
                     (if (taxy-items taxy)
                         (let ((marker (cadddr (car (taxy-items taxy)))))
                           (propertize (file-name-base (buffer-name (marker-buffer marker)))
                                       'face 'font-lock-comment-face))
                       (propertize "Gap" 'face 'font-lock-comment-face)))
                   (if (and start end)
                       (format " (%s)"
                               (org-duration-from-minutes
                                (/ (- (end-time taxy)
                                      (start-time taxy))
                                   60)))
                     "")))
               (when (or (and (taxy-items taxy)
                              (unfinished-clock-p (car (last (taxy-items taxy)))))
                         (and org-memento-current-block
                              (equal (title taxy) org-memento-current-block)))
                 (highlight-previous-line))
               ;; TODO: Add sum of clocked minutes and the utilization
               (insert-items (taxy-items taxy)
                             ;; Don't show the end of the block if the block is
                             ;; anonymous
                             (when (title taxy)
                               end)))))
         (insert-date (taxy)
           (magit-insert-section (date (taxy-name taxy))
             (let ((title (title taxy))
                   (indent (make-string 2 ?\s)))
               (magit-insert-heading
                 (if title
                     (propertize (thread-last
                                   (parse-time-string title)
                                   (org-memento--fill-decoded-time)
                                   (encode-time)
                                   (format-time-string "%F (%a)"))
                                 'face 'magit-section-heading)
                   (propertize (format "(%s between the days)"
                                       (org-duration-from-minutes
                                        (/ (- (end-time taxy)
                                              (start-time taxy))
                                           60)))
                               'face 'font-lock-comment-face)))
               (insert indent
                       (if-let (time (start-time taxy))
                           (format-time-string "%R" time)
                         "??:??")
                       (if title
                           " Checked in"
                         "")
                       "\n")
               (dolist (block (taxy-taxys taxy))
                 (insert-block block))
               (when-let (end-time (end-time taxy))
                 (insert indent
                         (format-time-string "%R" end-time)
                         (if title
                             (format (if (< end-time now)
                                         " Checked out (%s)\n"
                                       " Checking out (%s)\n")
                                     (org-duration-from-minutes
                                      (/ (- (end-time taxy)
                                            (start-time taxy))
                                         60)))
                           "\n")))))
           (insert ?\n)))
      (magit-insert-section (magit-section)
        (dolist (taxy (taxy-taxys root-taxy))
          (insert-date taxy)))
      (goto-char (point-min)))))

;;;###autoload
(define-derived-mode org-memento-timeline-mode magit-section-mode
  "MementoTl"
  "Major mode that displays a timeline of Org Memento.")

;;;; Extra hooks

;;;;; Overview

(defun org-memento-timeline-insert-insights (taxy)
  (magit-insert-section (overview (taxy-name taxy) nil)
    (magit-insert-heading "Insights")
    (cl-labels
        ((get-record (x)
           (if (taxy-p x)
               (taxy-name x)
             x))
         (get-marker (x)
           (cadddr (get-record x)))
         (start-time (x)
           (car (get-record x)))
         (end-time (x)
           (cadr (get-record x)))
         (title (x)
           (caddr (get-record x))))
      (let ((indent1 (make-string 2 ?\s))
            (indent2 (make-string 4 ?\s))
            (ndays (thread-last
                     (taxy-taxys taxy)
                     (seq-filter #'title)
                     (length))))
        (magit-insert-section (magit-section)
          (magit-insert-heading indent1 "Time on categories")
          (let (block-activities)
            (dolist (date-taxy (taxy-taxys taxy))
              (dolist (block-taxy (taxy-taxys date-taxy))
                (when-let* ((block-marker (get-marker block-taxy))
                            (start (start-time block-taxy))
                            (end (end-time block-taxy)))
                  (save-current-buffer
                    (org-with-point-at block-marker
                      (when-let (category (org-entry-get nil "memento_category"))
                        (push (cons category
                                    (round (/ (- end start) 60)))
                              block-activities)))))))
            (if block-activities
                (let* ((statistics (thread-last
                                     (seq-group-by #'car block-activities)
                                     (mapcar (pcase-lambda (`(,category . ,alist))
                                               (let ((sum (cl-reduce #'+ (mapcar #'cdr alist)
                                                                     :initial-value 0)))
                                                 (list category
                                                       (org-duration-from-minutes sum)
                                                       (org-duration-from-minutes
                                                        (/ sum ndays))))))))
                       (heading1 "Category")
                       (width1 (thread-last
                                 (mapcar #'car statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading1))))
                       (heading2 "Actual")
                       (width2 (thread-last
                                 (mapcar #'cadr statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading2))))
                       (heading3 "Average")
                       (width3 (thread-last
                                 (mapcar #'caddr statistics)
                                 (mapcar #'length)
                                 (apply #'max)
                                 (max (length heading3)))))
                  (insert indent1 "| " (string-pad heading1 width1)
                          " | " (string-pad heading2 width2)
                          " | " (string-pad heading3 width3)
                          " |\n")
                  (pcase-dolist (`(,category ,actual ,average)
                                 statistics)
                    (insert indent1 "| "
                            (string-pad category width1)
                            " | " (string-pad actual width2)
                            " | " (string-pad average width3)
                            " |\n")))
              (insert indent2 "No activities yet.\n")))))))
  (insert ?\n))

(provide 'org-memento-timeline)
;;; org-memento-timeline.el ends here
