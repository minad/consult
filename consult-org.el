;;; consult-org.el --- Consult commands for org-mode -*- lexical-binding: t -*-

;; Author: Daniel Mendler and Consult contributors
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.6
;; Package-Requires: ((consult "0.6") (org "9.1") (emacs "26.1"))
;; Homepage: https://github.com/minad/consult

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a completing-read interface for some Org mode
;; actions. This is an extra package, to allow lazy loading of Org.

;;; Code:

(require 'consult)
(require 'org-agenda)
(require 'org-clock)

(defvar consult-org--history nil)

(defun consult-org--narrow ()
  "Narrowing configuration for `consult-org' commands."
  (let ((todo-keywords (seq-filter
                        (lambda (it) (<= ?a (car it) ?z))
                        (mapcar (lambda (s)
                                  (pcase-let ((`(,a ,b) (split-string s "(")))
                                    (cons (downcase (string-to-char (or b a))) a)))
                                (mapcan 'cdr org-todo-keywords)))))
    (cons (lambda (cand)
            (cond ((<= ?1 consult--narrow ?9)
                   (<= (get-text-property 0 'consult--outline-level cand)
                       (- consult--narrow ?0)))
                  ((<= ?A consult--narrow ?Z)
                   (eq (get-text-property 0 'consult-org--priority cand)
                       consult--narrow))
                  ((when-let ((todo (alist-get consult--narrow todo-keywords)))
                     (string-equal (get-text-property 0 'consult-org--todo cand)
                                   todo)))))
          (append (mapcar (lambda (c) (cons c (format "Level %c" c)))
                          (number-sequence ?1 ?9))
                  (mapcar (lambda (c) (cons c (format "Priority %c" c)))
                          (number-sequence (max ?A org-highest-priority)
                                           (min ?Z org-lowest-priority)))
                  todo-keywords))))

(defun consult-org--entries (match scope &rest skip)
  "Return a list of consult locations from Org entries.

MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (opoint line buffer)
    (apply
     #'org-map-entries
     (lambda ()
       (unless (eq buffer (current-buffer))
         (setq opoint (point-min))
         (setq line 1)
         (setq buffer (current-buffer))
         (setq org-outline-path-cache nil))
       (setq line (+ line (consult--count-lines (prog1 (point)
                                                  (goto-char opoint)))))
       (setq opoint (point))
       (pcase-let ((`(_ ,level ,todo ,prio) (org-heading-components)))
         (consult--location-candidate
          (org-format-outline-path (org-get-outline-path t t))
          (point-marker)
          line
          'consult--outline-level level
          'consult-org--todo todo
          'consult-org--priority prio)))
     match scope skip)))

;;;###autoload
(defun consult-org-heading (&optional match scope)
  "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
  (interactive (unless (derived-mode-p 'org-mode)
                 (user-error "Must be called from an Org buffer.")))
  (consult--jump
   (consult--read
    (consult--with-increased-gc (consult-org--entries match scope))
    :prompt "Go to heading: "
    :category 'consult-location
    :sort nil
    :title (unless (member scope '(nil tree region region-start-level file))
             ;; Don't add titles when only showing entries from current buffer
             (lambda (cand)
               (let ((marker (car (get-text-property 0 'consult-location cand))))
                 (buffer-name (marker-buffer marker)))))
    :narrow (consult-org--narrow)
    :require-match t
    :lookup #'consult--lookup-location
    :history '(:input consult-org--history)
    :state (consult--jump-preview))))

;;;###autoload
(defun consult-org-agenda (&optional match)
  "Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this."
  (interactive)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (consult-org-heading match 'agenda))

;; Only suitable for short-lived hash tables, since a marker's
;; position can change.
(define-hash-table-test 'consult-org--marker-test
  'equal
  (lambda (m)
    (sxhash-equal (cons (marker-position m) (marker-buffer m)))))

;;;###autoload
(defun consult-org-clock-in (&optional match scope)
  "Clock into an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered. By default, offer entries of files with a
recent clocked item."
  (interactive)
  (let* ((scope (or scope
                    (thread-last (progn (org-clock-load) org-clock-history)
                      (mapcar 'marker-buffer)
                      (mapcar 'buffer-file-name)
                      seq-uniq
                      (seq-remove 'null))
                    (user-error "No recent clocked tasks")))
         (candidates (consult--with-increased-gc
                      (consult-org--entries match scope)))
         (recent (let ((tbl (make-hash-table :test 'consult-org--marker-test
                                             :size org-clock-history-length)))
                   (dolist (m org-clock-history)
                     (puthash m t tbl))
                   (dolist (c candidates)
                     (let ((m (car (get-text-property 0 'consult-location c))))
                       (when (gethash m tbl)
                         (puthash m c tbl))))
                   tbl)))
    (org-clock-clock-in
     (list
      (consult--read
       (append
        (seq-filter 'stringp
                    (mapcar (lambda (m) (gethash m recent))
                            org-clock-history))
        (seq-remove (lambda (c) (gethash
                                 (car (get-text-property 0 'consult-location c))
                                 recent))
                    candidates))
       :prompt "Clock in: "
       :category 'consult-location
       :sort nil
       :title (lambda (cand)
                (let ((m (car (get-text-property 0 'consult-location cand))))
                  (if (gethash m recent)
                      "Recent"
                    (buffer-name (marker-buffer m)))))
       :narrow (consult-org--narrow)
       :require-match t
       :lookup #'consult--lookup-location
       :history '(:input consult-org--history))))))

(provide 'consult-org)
;;; consult-org.el ends here
