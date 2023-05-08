;;; org-memento-export.el --- Export org-memento data -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/org-memento

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

;;

;;; Code:

(require 'map)
(require 'seq)
(require 'taxy)
(require 'org-memento)

(declare-function taxy-emptied "ext:taxy" t t)
(declare-function taxy-fill "ext:taxy" t t)
(declare-function taxy-sort-items "ext:taxy" t t)
(declare-function taxy-taxys "ext:taxy" t t)
(declare-function taxy-items "ext:taxy" t t)
(declare-function taxy-name "ext:taxy" t t)
(declare-function taxy-p "ext:taxy" t t)
(declare-function taxy-predicate "ext:taxy" t t)

(defsubst org-memento-export--format-datetime (time)
  (format-time-string "%FT%T%:z" time))

;;;###autoload
(defun org-memento-export-to-json (start-date end-date filename)
  "Export activity data to a JSON file."
  (declare (indent 2))
  (interactive (let ((start (org-read-date nil nil nil "Start"))
                     (end (org-read-date nil nil nil "End"))
                     (file (read-file-name "Enter a JSON file to export the activities to: ")))
                 (when (and (file-exists-p file)
                            (not (yes-or-no-p (format "File \"%s\" already exists. Overwrite it?"
                                                      file))))
                   (user-error "Aborted"))
                 (list start end file)))
  (cl-flet
      ((document-header ()
         (thread-first
           (make-hash-table :size 5)
           (map-insert "exportedAt" (org-memento-export--format-datetime (current-time)))
           (map-insert "identity" (format "%s@%s" (user-login-name) (system-name))))))
    (with-temp-buffer
      (insert "{"
              (json-encode "version") ":" (json-encode "0.1") ","
              (json-encode "meta") ":" (json-serialize (document-header)) ","
              (json-encode "body") ":" "[")
      (let ((first-date t))
        (dolist (date-taxy (taxy-taxys (org-memento-activity-taxy
                                        start-date end-date :groups t :todos t)))
          (if first-date
              (setq first-date nil)
            (insert ","))
          (org-memento-export--insert-date-object date-taxy)))
      (insert "]}")
      (write-file filename))))

(defun org-memento-export--insert-date-object (date-taxy)
  (cl-flet*
      ((activity-to-object (record)
         (pcase-exhaustive record
           ((and `(,start ,end ,title ,marker ,type)
                 (guard type '(clock clock-unfinished)))
            (let* ((has-link (string-match org-link-bracket-re title))
                   (link (if has-link
                             (match-string 1 title)
                           :null)))
              (thread-first
                (make-hash-table :size 7)
                (map-insert "type" "clock")
                (map-insert "title" (if has-link
                                        (org-link-display-format title)
                                      title))
                (map-insert "link" link)
                (map-insert "rawTitle" title)
                (map-insert "start" (org-memento-export--format-datetime start))
                (map-insert "end" (if (eq type 'clock)
                                      (org-memento-export--format-datetime end)
                                    :null))
                (map-insert "orgFile" (thread-last
                                        (marker-buffer marker)
                                        (buffer-file-name)
                                        (abbreviate-file-name)))
                (map-insert "orgId" (or (ignore-errors
                                          (org-id-get marker))
                                        :null))
                (map-insert "orgTags" (seq-into (ignore-errors
                                                  (org-get-tags marker))
                                                'vector)))))
           (`(,start ,end ,title ,marker event)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "event")
              (map-insert "title" title)
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))
              (map-insert "orgTags" (when marker
                                      (seq-into (ignore-errors
                                                  (org-get-tags marker))
                                                'vector)))))
           (`(,start ,end nil)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "gap")
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))))))
       (maybe-activities (items)
         (if items
             (seq-into (mapcar #'activity-to-object items) 'vector)
           :null))
       (format-group-segment (obj)
         (cl-typecase obj
           (null :null)
           (symbol (symbol-name obj))
           (string obj)
           (otherwise (prin1-to-string obj))))
       (event-to-object (block-taxy)
         (pcase-exhaustive (taxy-name block-taxy)
           (`(,start ,end ,title ,marker block . ,plist)
            (thread-first
              (make-hash-table :size 15)
              (map-insert "type" "block")
              (map-insert "title" title)
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))
              (map-insert "activities" (maybe-activities (taxy-items block-taxy)))
              (map-insert "state" (or (plist-get plist :todo)
                                      :null))
              (map-insert "group" (if-let (group (plist-get plist :group))
                                      (seq-into (org-memento--format-group-entries group)
                                                'vector)
                                    :null))
              (map-insert "orgProperties" (map-into
                                           (org-entry-properties marker 'standard)
                                           'hash-table))
              (map-insert "orgTags" (seq-into (ignore-errors
                                                (org-get-tags marker))
                                              'vector))
              (map-insert "orgText" (org-with-point-at marker
                                      (org-end-of-meta-data t)
                                      (when (looking-at (concat org-ts-regexp
                                                                "[[:space:]]+"))
                                        (goto-char (match-end 0)))
                                      (if (looking-at org-heading-regexp)
                                          :null
                                        (string-trim
                                         (buffer-substring-no-properties
                                          (point) (org-entry-end-position))))))))
           (`(,start ,end ,_ ,_ anonymous)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "anonymous")
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))
              (map-insert "activities" (maybe-activities (taxy-items block-taxy)))))
           (`(,start ,end nil)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "gap")
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))))
           (`(,start ,end ,_ ,_ away)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "away")
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))))
           (`(,start ,end ,_ ,_ idle)
            (thread-first
              (make-hash-table :size 7)
              (map-insert "type" "idle")
              (map-insert "start" (org-memento-export--format-datetime start))
              (map-insert "end" (org-memento-export--format-datetime end))))))
       (timeline-objects (date-taxy)
         (thread-last
           (taxy-taxys date-taxy)
           (mapcar #'event-to-object)
           (apply #'vector)))
       (date-taxy-to-object (date-taxy)
         (thread-first
           (pcase-exhaustive (taxy-name date-taxy)
             (`(,start ,end ,date ,_ date)
              (thread-first
                (make-hash-table :size 10)
                (map-insert "type" "day")
                (map-insert "date" date)
                (map-insert "start" (if start
                                        (org-memento-export--format-datetime start)
                                      :null))
                (map-insert "end" (if end
                                      (org-memento-export--format-datetime end)
                                    :null))))
             (`(,start ,end nil)
              (thread-first
                (make-hash-table :size 10)
                (map-insert "type" "gap")
                (map-insert "start" (org-memento-export--format-datetime start))
                (map-insert "end" (org-memento-export--format-datetime end)))))
           (map-insert "timeline" (timeline-objects date-taxy)))))
    (json-insert (date-taxy-to-object date-taxy)
                 :null-object :null
                 :false-object :false)))

(provide 'org-memento-export)
;;; org-memento-export.el ends here
