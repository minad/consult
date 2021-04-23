;;; consult-org.el --- Consult commands for org-mode -*- lexical-binding: t -*-

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

(defvar consult-org--history nil)

(defun consult-org--narrow ()
  "Narrowing configuration for `consult-org' commands."
  (let ((todo-keywords
         ;; TODO: Is there no function in org which provides keywords+keys?
         ;; Why do we have to do our own splitting here?
         (seq-filter
          (lambda (it) (<= ?a (car it) ?z))
          (mapcar (lambda (s)
                    (pcase-let ((`(,a ,b) (split-string s "(")))
                      (cons (downcase (string-to-char (or b a))) a)))
                  (mapcan #'cdr org-todo-keywords)))))
    (cons (lambda (cand)
            (pcase-let ((`(_ ,level ,todo ,prio)
                         (get-text-property 0 'consult-org-heading cand)))
              (cond
               ((<= ?1 consult--narrow ?9) (<= level (- consult--narrow ?0)))
               ((<= ?A consult--narrow ?Z) (eq prio consult--narrow))
               ;; TODO instead attach a 'consult-org--todo-key to the candidate
               (t (equal todo (alist-get consult--narrow todo-keywords))))))
          (append (mapcar (lambda (c) (cons c (format "Level %c" c)))
                          (number-sequence ?1 ?9))
                  (mapcar (lambda (c) (cons c (format "Priority %c" c)))
                          (number-sequence (max ?A org-highest-priority)
                                           (min ?Z org-lowest-priority)))
                  todo-keywords))))

(defun consult-org--headlines (match scope &rest skip)
  "Return a list of Org heading candidates.

MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (buffer)
    (apply
     #'org-map-entries
     (lambda ()
       (unless (eq buffer (current-buffer))
         ;; TODO is it necessary to reset the cache? Is `org-format-outline-path' incapable of
         ;; detecting if the buffer changed?
         (setq buffer (current-buffer)
               org-outline-path-cache nil)) ;; Reset the cache since `org-get-outline-path' uses the cache
       (pcase-let ((`(_ ,level ,todo ,prio . _) (org-heading-components))
                   (cand (org-format-outline-path (org-get-outline-path 'with-self 'use-cache))))
         (put-text-property 0 1 'consult-org-heading (list (point-marker) level todo prio) cand)
         cand))
     match scope skip)))

;;;###autoload
(defun consult-org-heading (&optional match scope)
  "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
  (interactive (unless (derived-mode-p 'org-mode)
                 (user-error "Must be called from an Org buffer")))
  (consult--read
   (consult--with-increased-gc (consult-org--headlines match scope))
   :prompt "Go to heading: "
   :category 'consult-org-heading
   :sort nil
   :group (unless (memq scope '(nil tree region region-start-level file))
            ;; Don't add titles when only showing entries from current buffer
            (apply-partially
              #'consult--group-by-title
              (lambda (cand)
                (buffer-name (marker-buffer (car (get-text-property 0 'consult-org-heading cand)))))))
   :narrow (consult-org--narrow)
   :require-match t
   :lookup
   (lambda (_ candidates cand)
     (when-let (found (member cand candidates))
       (car (get-text-property 0 'consult-org-heading (car found)))))
   :history '(:input consult-org--history)
   :state (consult--jump-state)))

;;;###autoload
(defun consult-org-agenda (&optional match)
  "Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this."
  (interactive)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (consult-org-heading match 'agenda))

(provide 'consult-org)
;;; consult-org.el ends here
