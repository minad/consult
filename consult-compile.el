;;; consult-compile.el --- Provides the command `consult-compile-error' -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

;; Provides the command `consult-compile-error'.  This is an extra
;; package, to allow lazy loading of compile.el.  The
;; `consult-compile-error' command is autoloaded.

;;; Code:

(require 'consult)
(require 'compile)

(defvar consult-compile--history nil)

(defconst consult-compile--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?i . "Info")))

(defun consult-compile--font-lock (str)
  "Apply `font-lock' faces in STR, copy them to `face'."
  (let ((pos 0) (len (length str)))
    (while (< pos len)
      (let* ((face (get-text-property pos 'font-lock-face str))
             (end (or (text-property-not-all pos len 'font-lock-face face str) len)))
        (put-text-property pos end 'face face str)
        (setq pos end)))
    str))

(defun consult-compile--error-candidates (buffer)
  "Return alist of errors and positions in BUFFER, a compilation buffer."
  (with-current-buffer buffer
    (let ((candidates)
          (pos (point-min)))
      (save-excursion
        (while (setq pos (compilation-next-single-property-change pos 'compilation-message))
          (when-let (msg (get-text-property pos 'compilation-message))
            (goto-char pos)
            (push (propertize
                   (consult-compile--font-lock (consult--buffer-substring pos (pos-eol)))
                   'consult--type (pcase (compilation--message->type msg)
                                    (0 ?i)
                                    (1 ?w)
                                    (_ ?e))
                   'consult--candidate (point-marker))
                  candidates))))
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

(defun consult-compile--compilation-buffers (file)
  "Return a list of compilation buffers relevant to FILE."
  (consult--buffer-query
   :sort 'alpha :predicate
   (lambda (buffer)
     (with-current-buffer buffer
       (and (compilation-buffer-internal-p)
            (file-in-directory-p file default-directory))))))

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
(defun consult-compile-error ()
  "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error."
  (interactive)
  (consult--read
   (or (mapcan #'consult-compile--error-candidates
               (or (consult-compile--compilation-buffers
                    default-directory)
                   (user-error "No compilation buffers found for the current buffer")))
       (user-error "No compilation errors found"))
   :prompt "Go to error: "
   :category 'consult-compile-error
   :sort nil
   :require-match t
   :history t ;; disable history
   :lookup #'consult--lookup-candidate
   :group (consult--type-group consult-compile--narrow)
   :narrow (consult--type-narrow consult-compile--narrow)
   :history '(:input consult-compile--history)
   :state (consult-compile--state)))

(provide 'consult-compile)
;;; consult-compile.el ends here
