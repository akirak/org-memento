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
  :group 'org-memento)

(defconst org-memento-timeline-ms-buffer "*Org-Memento Timeline*")

;;;; Faces

(defface org-memento-timeline-time-face
  '((t (:inherit default :slant italic)))
  "Face for time ranges.")

;;;; Display the timeline

;;;###autoload
(defun org-memento-timeline (start-day end-day)
  (interactive (if (equal current-prefix-arg '(4))
                   (list (org-read-date)
                         (org-read-date))
                 (let ((today (org-memento--today-string (decode-time))))
                   (list today today))))
  (when (string-lessp end-day start-day)
    (user-error "The end day must be no earlier than the start day"))
  (let ((taxy (org-memento-activity-taxy start-day end-day)))
    (with-current-buffer (get-buffer-create org-memento-timeline-ms-buffer)
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-memento-timeline--insert taxy))
      (pop-to-buffer (current-buffer)))))

(defun org-memento-timeline--insert (root-taxy)
  ;; TODO: Maybe set magit-section-set-visibility-hook
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
       (insert-tree (tree level)
         (magit-insert-section (magit-section)
           (let ((indent (make-string (* 2 level) ?\s)))
             (magit-insert-heading
               indent
               (if-let (title (title tree))
                   (propertize title 'face 'magit-section-heading)
                 "Gap"))
             (insert indent " "
                     (format-time-range (start-time tree) (end-time tree))
                     "\n"))
           (when (taxy-p tree)
             (dolist (child (or (taxy-taxys tree)
                                (taxy-items tree)))
               (insert-tree child (1+ level))))))
       (insert-items (items end-time-of-block)
         (when items
           (let ((indent1 (make-string 6 ?\s))
                 (indent2 (make-string 15 ?\s)))
             (dolist (group (-partition-by #'caddr items))
               (magit-insert-section (magit-section)
                 (let ((marker (cadddr (car group))))
                   (magit-insert-heading
                     indent1
                     (format-time-string "%R" (car (car group)))
                     (make-string 2 ?\s)
                     (propertize (org-link-display-format (caddr (car group)))
                                 'face 'magit-section-heading)
                     "\n")
                   (dolist (clock group)
                     (insert indent2
                             (format-time-range (start-time clock)
                                                (end-time clock))
                             "\n")))))
             (when-let (last-end (cadr (car (last items))))
               (magit-insert-section (magit-section)
                 (magit-insert-heading
                   indent1
                   (format-time-string "%R" last-end)
                   (make-string 2 ?\s)
                   (propertize "Clocked out" 'face 'font-lock-comment-face)
                   (if end-time-of-block
                       (format " (until %s)" (format-time-string "%R" end-time-of-block))
                     "")
                   "\n"))))
           (insert ?\n)))
       (insert-block (taxy)
         (magit-insert-section (magit-section)
           (let ((indent1 (make-string 4 ?\s))
                 (indent2 (make-string 11 ?\s))
                 (indent2 (make-string 13 ?\s))
                 (start (start-time taxy))
                 (end (end-time taxy)))
             (magit-insert-heading
               indent1
               (if start
                   (format-time-string "%R- " start)
                 (make-string 7 ?\s))
               (if-let (title (title taxy))
                   (propertize title
                               'face (if (eq (cadddr (get-record taxy)) 'idle)
                                         'font-lock-comment-face
                                       'magit-section-heading))
                 (propertize "Gap" 'face 'font-lock-comment-face))
               (when (and start end)
                 (format " (%s)"
                         (org-duration-from-minutes
                          (/ (- (end-time taxy)
                                (start-time taxy))
                             60)))))
             ;; TODO: Add sum of clocked minutes and the utilization
             (insert-items (taxy-items taxy)
                           ;; Don't show the end of the block if the block is
                           ;; anonymous
                           (when (title taxy)
                             end)))))
       (insert-date (taxy)
         (magit-insert-section (magit-section)
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
                           (format " Checked out (%s)\n"
                                   (org-duration-from-minutes
                                    (/ (- (end-time taxy)
                                          (start-time taxy))
                                       60)))
                         "\n")))))
         (insert ?\n)))
    (magit-insert-section (magit-section)
      (dolist (taxy (taxy-taxys root-taxy))
        (insert-date taxy)))
    (goto-char (point-min))))

;;;###autoload
(define-derived-mode org-memento-timeline-mode magit-section-mode
  "MementoTl"
  "Major mode that displays a timeline of Org Memento.")

(provide 'org-memento-timeline)
;;; org-memento-timeline.el ends here
