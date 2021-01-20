;;; consult-yasnippet.el --- Provides the command `consult-yasnippet' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.1") (yasnippet "0.14") (emacs "26.1"))
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


;;; Code:

(require 'consult)
(require 'yasnippet)

(defgroup consult-yasnippet nil
  "Consulting `completing-read' for yasnippet snippets."
  :group 'consult
  :prefix "consult-yasnippet-")

(defvar consult-yasnippet--snippets nil
  "Snippet collection for current `consult-snippet' session.")

(defvar consult-yasnippet--buffer nil
  "The buffer in which `consult-yasnippet' was begun")

(defvar consult-yasnippet--region nil
  "The position (a cons of the start and end `point's) of where `consult-yasnippet' was begun")

(defvar consult-yasnippet--region-contents ""
  "The original contents of `consult-yasnippet--region'.")

(defun consult-yasnippet--expand-template (template)
  "Expand the yasnippet template TEMPLATE at point."
  (deactivate-mark)
  (goto-char (car consult-yasnippet--region))
  (when (not (string-equal "" consult-yasnippet--region-contents))
    (push-mark (point))
    (push-mark (cdr consult-yasnippet--region) nil t))

  (yas-expand-snippet (yas--template-content template)
                      nil nil
                      (yas--template-expand-env template)))

(defun consult-yasnippet--preview (template _)
  "Previewer for `consult--read'.
This function expands TEMPLATE at point in the buffer
`consult-yasnippet--read-snippet' was started in. This includes
overwriting any region that was active and removing any previous
previews that're already active.

When TEMPLATE is not given, this function essentially just resets
the state of the current buffer to before any snippets were previewed."
  (with-current-buffer consult-yasnippet--buffer
    (let ((yas-verbosity 0)
          (inhibit-redisplay t)
          (inhibit-read-only t)
          (orig-offset (- (point-max) (cdr consult-yasnippet--region)))
          (yas-prompt-functions '(yas-no-prompt)))

      ;; We always undo any snippet previews before maybe setting up
      ;; some new previews.
      (delete-region (car consult-yasnippet--region)
                     (cdr consult-yasnippet--region))
      (goto-char (car consult-yasnippet--region))
      (setcar consult-yasnippet--region (point))
      (insert consult-yasnippet--region-contents)
      (setcdr consult-yasnippet--region (point))

      (when template
        (unwind-protect
            (consult-yasnippet--expand-template template)
          (unwind-protect
              (mapc #'yas--commit-snippet
                    (yas-active-snippets (point-min) (point-max)))
            (setcdr consult-yasnippet--region (- (point-max) orig-offset))))
        (redisplay)))))

(defmacro consult-yasnippet--setup (&rest body)
  "Setup the local variables and environment for `consult-yasnippet'.
This environment is used both in `consult-yasnippet--preview'and
`consult-yasnippet--expand-template'."
  `(progn
     (barf-if-buffer-read-only)
     (unless (bound-and-true-p yas-minor-mode)
       (error "`yas-minor-mode' not enabled in current buffer"))

     (let* ((consult-yasnippet--snippets
             (mapcar (lambda (template)
                       (cons (yas--template-name template) template))
                     (yas--all-templates (yas--get-snippet-tables))))
            (consult-yasnippet--buffer (current-buffer))
            (consult-yasnippet--region (if (region-active-p)
                                           (cons (region-beginning) (region-end))
                                         (cons (point) (point))))
            (consult-yasnippet--region-contents (buffer-substring (car consult-yasnippet--region)
                                                                  (cdr consult-yasnippet--region))))
       ,@body)))

(defun consult-yasnippet--read-snippet ()
  "Backend implementation of `consult-yasnippet'.

This starts a `completing-read' session with all the snippets in the current
snippet table with support for previewing the snippet to be expanded and
replacing the active region with the snippet expansion.

This function doesn't actually expand the snippet, it only reads and then
returns a snippet template from the user."
  (consult-yasnippet--setup
   (let ((buffer-undo-list t))                                                  ; Prevent querying user (and showing previews) from updating the undo-history
     (unwind-protect
         (consult--read
          "Choose a snippet: "
          consult-yasnippet--snippets
          :lookup 'consult--lookup-cdr
          :require-match t
          :preview #'consult-yasnippet--preview
          :category 'yasnippet)
       (consult-yasnippet--preview nil t)))))                                   ; Restore contents of region from before preview (while still ignoring undo history).

(defun consult-yasnippet-visit-snippet-file (snippet)
  (interactive (list (consult-yasnippet--read-snippet)))
  (yas--visit-snippet-file-1 snippet))

(defun consult-yasnippet (template)
  (interactive (list (consult-yasnippet--read-snippet)))
  ;; We need to first restore the local environment for
  ;; `consult-yasnippet--expand-template' to work.
  (consult-yasnippet--setup
   (consult-yasnippet--expand-template template)))

(provide 'consult-yasnippet)

;;; consult-yasnippet.el ends here
