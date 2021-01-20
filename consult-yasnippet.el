;;; consult-yasnippet.el --- Provides the command `consult-yasnippet' -*- lexical-binding: t; -*-

;; Author: Michał Krzywkowski, Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.2") (yasnippet "0.14") (emacs "26.1"))
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

;; Provides the command `consult-yasnippet' which presents all the snippet
;; expansions available in the current buffer with in-buffer previews.
;;
;; This is an extra package, to avoid loading yasnippet in the base consult
;; package..

;;;; Credits

;; This package has been derived from the ivy-yasnippet[1] package.
;;
;;  [1] https://github.com/mkcms/ivy-yasnippet

;;; Code:

(require 'consult)
(require 'yasnippet)

(defun consult-yasnippet--expand-template (template region region-contents)
  "Expand the yasnippet template TEMPLATE at point."
  (deactivate-mark)
  (goto-char (car region))

  ;; Restore marked region (when it existed) so that `yas-expand-snippet'
  ;; overwrites it.
  (when (not (string-equal "" region-contents))
    (push-mark (point))
    (push-mark (cdr region) nil t))

  (cl-letf (((symbol-function 'yas-completing-read)
             (lambda (&rest _args) "")))
    (yas-expand-snippet (yas--template-content template)
                        nil nil
                        (yas--template-expand-env template))))

(defun consult-yasnippet--preview ()
  "Previewer for `consult--read'.
This function expands TEMPLATE at point in the buffer
`consult-yasnippet--read-template' was started in. This includes
overwriting any region that was active and removing any previous
previews that're already active.

When TEMPLATE is not given, this function essentially just resets
the state of the current buffer to before any snippets were previewed."
  (let* ((buf (current-buffer))
         (region (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point) (point))))
         (region-contents (buffer-substring (car region) (cdr region))))
    (lambda (template restore)
      (with-current-buffer buf
        (let ((yas-verbosity 0)
              (inhibit-redisplay t)
              (inhibit-read-only t)
              (orig-offset (- (point-max) (cdr region)))
              (yas-prompt-functions '(yas-no-prompt)))

          ;; We always undo any snippet previews before maybe setting up
          ;; some new previews.
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (setcar region (point))
          (insert region-contents)
          (setcdr region (point))

          (when (and template (not restore))
            (unwind-protect
                (consult-yasnippet--expand-template template region region-contents)
              (unwind-protect
                  (mapc #'yas--commit-snippet
                        (yas-active-snippets (point-min) (point-max)))
                (setcdr region (- (point-max) orig-offset))))
            (redisplay)))))))

(defun consult-yasnippet--candidates (&rest body)
  "Retrieve the list of available snippets in the current buffer."
  (unless (bound-and-true-p yas-minor-mode)
    (error "`yas-minor-mode' not enabled in current buffer"))

  (mapcar
   (lambda (template)
     (cons (concat (yas--template-name template)
                   (propertize " "
                               'display (concat
                                         " ["
                                         (propertize (yas--template-key template)
                                                     'face 'consult-key)
                                         "]")))
           template))
   (yas--all-templates (yas--get-snippet-tables))))

(defun consult-yasnippet--read-template ()
  "Backend implementation of `consult-yasnippet'.

This starts a `completing-read' session with all the snippets in the current
snippet table with support for previewing the snippet to be expanded and
replacing the active region with the snippet expansion.

This function doesn't actually expand the snippet, it only reads and then
returns a snippet template from the user."
  (barf-if-buffer-read-only)

  (let* ((buffer-undo-list t))                                                  ; Prevent querying user (and showing previews) from updating the undo-history
    (consult--read
     "Choose a snippet: "
     (consult-yasnippet--candidates)
     :lookup 'consult--lookup-cdr
     :require-match t
     :preview (consult-yasnippet--preview)
     :category 'yasnippet)))

(defun consult-yasnippet-visit-snippet-file (template)
  "Visit the snippet file associated with TEMPLATE.
When called interactively this command previews snippet completions in
the current buffer, and then opens the selected snippets template file
using `yas--visit-snippet-file-1'."
  (interactive (list (consult-yasnippet--read-template)))
  (yas--visit-snippet-file-1 template))

(defun consult-yasnippet (template)
  "Interactively select and expand the yasnippet template TEMPLATE.
When called interactively this command presents a completing read interface
containing all currently available snippet expansions, with live previews for
each snippet. Once selected a chosen snippet will be expanded at point using
`yas-expand-snippet'."
  (interactive (list (consult-yasnippet--read-template)))
  (barf-if-buffer-read-only)
  (let* ((region (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point) (point))))
         (region-contents (buffer-substring (car region) (cdr region))))
    (consult-yasnippet--expand-template template region region-contents)))

(provide 'consult-yasnippet)

;;; consult-yasnippet.el ends here
