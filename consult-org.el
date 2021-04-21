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
  (let ((triples (append
                  ;; TODO keywords
                  (seq-filter
                   (lambda (it) (<= ?a (car it) ?z))
                   (mapcar (lambda (s)
                             (list (downcase (string-to-char s))
                                   s
                                   (lambda (cand)
                                     (string-equal s (get-text-property
                                                      0 'consult-org--todo cand)))))
                           (mapcan 'cdr org-todo-keywords)))
                  ;; Priorities
                  (mapcar (lambda (c)
                            (list c
                                  (format "Priority %c" c)
                                  (lambda (cand)
                                    (eq c (get-text-property
                                           0 'consult-org--priority cand)))))
                          (number-sequence (max ?A org-highest-priority)
                                           (min ?Z org-lowest-priority)))
                  ;; Outline levels
                  (mapcar (lambda (i)
                            (list (+ i ?0)
                                  (format "Level %i" i)
                                  (lambda (cand)
                                    (>= i (get-text-property
                                           0 'consult-org--level cand)))))
                          (number-sequence 1 9)))))
    (cons (lambda (cand)
            (funcall (nth 2 (assoc consult--narrow triples))
                     cand))
          (mapcar (pcase-lambda (`(,c ,s _)) (cons c s)) triples))))

(defun consult-org--entries (match scope &rest skip)
  "Return a list of consult locations from Org entries.

MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (setq org-outline-path-cache nil)
  (apply
   #'org-map-entries
   (lambda ()
     (pcase-let ((`(_ ,level ,todo ,prio) (org-heading-components)))
       (consult--location-candidate
        (org-format-outline-path (org-get-outline-path t t))
        (point-marker)
        (org-current-line)
        'consult-org--level level
        'consult-org--todo todo
        'consult-org--priority prio)))
   match scope skip))

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
    :category 'consult-org
    :sort nil
    :title (lambda (cand)
             (let ((marker (car (get-text-property 0 'consult-location cand))))
               (buffer-name (marker-buffer marker))))
    :narrow (consult-org--narrow)
    :require-match t
    :lookup #'consult--lookup-location
    :history '(:input consult-org--history)
    :add-history (when (featurep 'org-clock) org-clock-current-task)
    :state (consult--jump-preview))))

;;;###autoload
(defun consult-org-agenda (&optional match)
  "Jump to an Org agenda heading.

By default, all agenda entries are offered.  MATCH is as in
`org-map-entries' and can used to refine this."
  (interactive)
  (consult-org-heading match 'agenda))

;;;###autoload
(defun consult-org-clock (&optional match scope)
  "Clock into an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, offer entries of files with a
recent clocked item."
  (interactive)
  (unless scope
    (org-clock-load)
    (setq scope (thread-last org-clock-history
                  (mapcar 'marker-buffer)
                  (mapcar 'buffer-file-name)
                  seq-uniq
                  (seq-remove 'null))))
  (org-clock-clock-in
   (list
    (consult--read
     (consult--with-increased-gc
      (sort ;; Recent items first, everything else in their original order
       (consult-org--entries match scope)
       (lambda (c1 c2)
         (let ((m1 (car (get-text-property 0 'consult-location c1)))
               (m2 (car (get-text-property 0 'consult-location c2))))
           (and (member m1 org-clock-history)
                (not (member m1 (member m2 org-clock-history))))))))
     :prompt "Clock in: "
     :category 'consult-org
     :sort nil
     :title (lambda (cand)
              (let ((m (car (get-text-property 0 'consult-location cand))))
                (if (member m org-clock-history)
                    "Recent"
                  (buffer-name (marker-buffer m)))))
     :narrow (consult-org--narrow)
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult-org--history)))))

(provide 'consult-org)
;;; consult-org.el ends here
