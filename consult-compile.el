;;; consult-compile.el --- Provides the command `consult-compile-error' -*- lexical-binding: t -*-

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

;; Provides the command `consult-compile-error'.  This is an extra package, to
;; allow lazy loading of compile.el.  The `consult-compile-error' command is
;; autoloaded.  See also the command `consult-grep-match' which adapts
;; `consult-compile-error' to Grep buffers.

;;; Code:

(require 'consult)
(require 'compile)

(defvar consult-compile--history nil)

(defconst consult-compile--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?i . "Info")))

(defun consult-compile--candidates (grep buffer)
  "Return list of error candidate strings in BUFFER.
If GREP is non-nil, the buffer is a Grep buffer."
  (with-current-buffer buffer
    (let ((pos (point-min)) candidates)
      (save-excursion
        (while (setq pos (compilation-next-single-property-change pos 'compilation-message))
          (when-let ((msg (get-text-property pos 'compilation-message))
                     ((compilation--message->loc msg)))
            (goto-char pos)
            (let ((str (consult--buffer-substring pos (pos-eol))))
              (add-text-properties
               0 1 (list 'consult--type (unless grep
                                          (pcase (compilation--message->type msg)
                                            (0 ?i) (1 ?w) (_ ?e)))
                         'consult--candidate (point-marker))
               str)
              (push str candidates)))))
      (nreverse candidates))))

(defun consult-compile--lookup (marker)
  "Lookup error position given error MARKER."
  (when-let (buffer (and marker (marker-buffer marker)))
    (with-current-buffer buffer
      (let ((next-error-highlight nil)
            (compilation-current-error marker)
            (overlay-arrow-position overlay-arrow-position))
        (ignore-errors
          (save-window-excursion
            (compilation-next-error-function 0)
            (point-marker)))))))

(defun consult-compile--buffers (grep file)
  "List of compilation buffers relevant to FILE.
If GREP is non-nil, search Grep buffers."
  (consult--buffer-query
   :sort 'alpha :predicate
   (lambda (buffer)
     (and (buffer-local-value 'compilation-locs buffer)
          (file-in-directory-p file (buffer-local-value 'default-directory buffer))
          (with-current-buffer buffer
            (eq (not grep) (not (derived-mode-p 'grep-mode 'grep-edit-mode))))))))

(defun consult-compile--state ()
  "Like `consult--jump-state', also setting the current compilation error."
  (let ((jump (consult--jump-state)))
    (lambda (action marker)
      (let ((pos (consult-compile--lookup marker)))
        (when-let (buffer (and (eq action 'return)
                               marker
                               (marker-buffer marker)))
          (with-current-buffer buffer
            (setq compilation-current-error marker
                  overlay-arrow-position marker)))
        (funcall jump action pos)))))

;;;###autoload
(defun consult-compile-error (&optional arg grep)
  "Jump to a compilation error related to the current project or file.

This command collects entries from all related compilation buffers.  The
command supports preview of the currently selected error.  With prefix
ARG, jump to the error message in the compilation buffer, instead of to
the actual location of the error.  If GREP is non-nil, Grep buffers are
searched.  See also `consult-grep-match'."
  (interactive "P")
  (consult--read
   (or (mapcan (apply-partially #'consult-compile--candidates grep)
               (or (consult-compile--buffers
                    grep (or (consult--project-root) default-directory))
                   (user-error "No related buffers")))
       (user-error "No %s" (if grep "matches" "errors")))
   :prompt (format "Go to %s: " (if grep "match" "error"))
   :category 'consult-compile-error
   :sort nil
   :require-match t
   :history t ;; disable history
   :lookup #'consult--lookup-candidate
   :group (consult--type-group consult-compile--narrow)
   :narrow (consult--type-narrow consult-compile--narrow)
   :history '(:input consult-compile--history)
   :state (if arg (consult--jump-state) (consult-compile--state))))

(provide 'consult-compile)
;;; consult-compile.el ends here
