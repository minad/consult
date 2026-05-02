;;; consult-org.el --- Consult commands to navigate Org files -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides the commands `consult-org-agenda' and `consult-org-heading'.  This
;; is an extra file to allow lazy loading of org.el.

;;; Code:

(require 'consult)
(require 'org)
(require 'org-agenda)

(defvar consult-org--history nil)

(defun consult-org--narrow ()
  "Narrowing configuration for `consult-org' commands."
  (let ((todo-kws
         (seq-filter
          (lambda (x) (<= ?a (car x) ?z))
          (mapcar (lambda (s)
                    (pcase-let ((`(,a ,b) (split-string s "(")))
                      (cons (downcase (string-to-char (or b a))) a)))
                  (apply #'append (mapcar #'cdr org-todo-keywords))))))
    (list :predicate
          (lambda (cand)
            (pcase-let ((`(,level ,todo ,prio . ,_)
                         (get-text-property 0 'consult-org--heading cand)))
              (cond
               ((<= ?1 consult--narrow ?9) (<= level (- consult--narrow ?0)))
               ((<= ?A consult--narrow ?Z) (eq prio consult--narrow))
               (t (equal todo (alist-get consult--narrow todo-kws))))))
          :keys
          (nconc (mapcar (lambda (c) (cons c (format "Level %c" c)))
                         (number-sequence ?1 ?9))
                 (mapcar (lambda (c) (cons c (format "Priority %c" c)))
                         (number-sequence (max ?A org-highest-priority)
                                          (min ?Z org-lowest-priority)))
                 todo-kws))))

(defun consult-org--headings (prefix match scope &rest skip)
  "Return a list of Org heading candidates.

If PREFIX is non-nil, prefix the candidates with the buffer name.
MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (buffer (idx 0))
    (apply
     #'org-map-entries
     (lambda ()
       ;; Reset the cache when the buffer changes, since `org-get-outline-path' uses the cache
       (unless (eq buffer (buffer-name))
         (setq buffer (buffer-name)
               org-outline-path-cache nil))
       (pcase-let* ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
                    (tags (if org-use-tag-inheritance
                              (when-let* ((tags (org-get-tags)))
                                (concat ":" (string-join tags ":") ":"))
                            tags))
                    (cand (org-format-outline-path
                           (org-get-outline-path 'with-self 'use-cache)
                           most-positive-fixnum)))
         (when todo
           (put-text-property 0 (length todo) 'face (org-get-todo-face todo) todo))
         (when tags
           (put-text-property 0 (length tags) 'face 'org-tag tags))
         (setq cand (concat (and prefix buffer) (and prefix " ") cand (and tags " ")
                            tags (consult--tofu-encode idx)))
         (incf idx)
         (add-text-properties 0 1
                              `(org-marker ,(point-marker)
                                consult-org--heading (,level ,todo ,prio . ,buffer))
                              cand)
         cand))
     match scope skip)))

(defun consult-org--annotate (cand)
  "Annotate CAND for `consult-org-heading'."
  (pcase-let ((`(,_level ,todo ,prio . ,_)
               (get-text-property 0 'consult-org--heading cand)))
    (consult--annotate-align
     cand
     (concat todo
             (and prio (format #(" [#%c]" 1 6 (face org-priority)) prio))))))

(defun consult-org--group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (pcase-let ((`(,_level ,_todo ,_prio . ,buffer)
               (get-text-property 0 'consult-org--heading cand)))
    (if transform (substring cand (1+ (length buffer))) buffer)))

;;;###autoload
(defun consult-org-heading (&optional match scope)
  "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
  (interactive (unless (derived-mode-p #'org-mode)
                 (user-error "Must be called from an Org buffer")))
  (let ((prefix (not (memq scope '(nil tree region region-start-level file)))))
    (consult--read
     (consult--slow-operation "Collecting headings..."
       (or (consult-org--headings prefix match scope)
           (user-error "No headings")))
     :prompt "Go to heading: "
     :category 'org-heading
     :sort nil
     :require-match t
     :history '(:input consult-org--history)
     :narrow (consult-org--narrow)
     :state (consult--jump-state)
     :annotate #'consult-org--annotate
     :group (and prefix #'consult-org--group)
     :lookup (apply-partially #'consult--lookup-prop 'org-marker))))

;;;###autoload
(defun consult-org-agenda (&optional match)
  "Jump to an Org agenda heading.

By default, all agenda entries are offered.  MATCH is as in
`org-map-entries' and can used to refine this."
  (interactive)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (consult-org-heading match 'agenda))

(defun consult-org-tag ()
  "Add or remove tags for an Org heading."
  (interactive)
  (let ((current-tags nil)
        (new-tags nil))
    (save-excursion
      ;; There are 3 cases:
      ;; - In Org Agenda, marked none: save current tags
      ;; - In Org Agenda, marked multiple (or one): don't
      ;; - In normal Org mode: save current tags
      (if (eq major-mode 'org-agenda-mode)
          (unless org-agenda-bulk-marked-entries
            (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                                (org-agenda-error))))
              (with-current-buffer (marker-buffer hdmarker)
                (goto-char hdmarker)
                (setq current-tags (org-get-tags nil t)))))
        (unless (org-at-heading-p)
          (org-back-to-heading t))
        (setq current-tags (org-get-tags nil t)))

      (let* ((org-last-tags-completion-table ; for `org-tags-completion-function'
              (append (and (or org-complete-tags-always-offer-all-agenda-tags
                               (eq major-mode 'org-agenda-mode))
                           (org-global-tags-completion-table
                            (org-agenda-files)))
                      (or org-current-tag-alist
                          ;; `org-get-buffer-tags' errors for me in Org Agenda
                          ;; about org-element-cache not being active. Since
                          ;; this is just for completion (convenience) if it
                          ;; errors just go on without it.
                          (ignore-errors (org-get-buffer-tags)))))
             (selected (consult--read
                        (lambda (str _pred _action)
                          (delete-dups
                           (all-completions str #'org-tags-completion-function)))
                        :prompt (if current-tags
                                    (format "Tags (%s): " (mapconcat #'identity current-tags ", "))
                                  "Tags: ")
                        :history 'org-tags-history)))

        (setq new-tags
              (cond ((member selected current-tags) (remove selected current-tags))
                    ((equal selected "") current-tags)
                    (t (append current-tags (list selected)))))

        ;; The same 3 cases:
        ;; - In Org Agenda, marked none: find the entry at point and set
        ;;   tags for it
        ;; - In Org Agenda, marked some: set tags for each of them, although for
        ;;   these we need to check their original tags here instead of having
        ;;   that done at the top
        ;; - In normal Org mode: set tags for the current entry
        (if (eq major-mode 'org-agenda-mode)
            (if (null org-agenda-bulk-marked-entries)
                (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                                    (org-agenda-error))))
                  (with-current-buffer (marker-buffer hdmarker)
                    (goto-char hdmarker)
                    (org-set-tags new-tags)))
              (dolist (m org-agenda-bulk-marked-entries)
                (with-current-buffer (marker-buffer m)
                  (save-excursion
                    (goto-char m)
                    (org-set-tags
                     (seq-uniq
                      (append (org-get-tags nil t) new-tags)))))))
          (org-set-tags new-tags)
          (unless (member selected new-tags)
            (consult--minibuffer-message "Tag %S has been removed." selected)))))))

(provide 'consult-org)
;;; consult-org.el ends here
