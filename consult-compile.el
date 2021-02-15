;;; consult-compile.el --- Provides the command `consult-compile-error' -*- lexical-binding: t -*-

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

;; Provides the command `consult-compile-error'. This is an extra
;; package, to avoid loading compile.el. The `consult-compile-error'
;; command is autoloaded.

;;; Code:

(require 'consult)
(require 'compile)

(defun consult-compile--error-candidates ()
  "Return alist of errors and positions."
  (let ((candidates)
        (pos (point-min)))
    (save-excursion
      (while (setq pos (compilation-next-single-property-change pos 'compilation-message))
        (when-let (msg (get-text-property pos 'compilation-message))
          (goto-char pos)
          (push (list
                 (consult--font-lock (consult--buffer-substring pos (line-end-position)))
                 (point-marker)
                 msg)
                candidates))))
    (nreverse candidates)))

;;;###autoload
(defun consult-compile-error ()
  "Jump to a compilation error in the current buffer.

This command works in compilation buffers and grep buffers.
The command supports preview of the currently selected error."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (user-error "Not a compilation buffer"))
  (consult--read
   (consult--with-increased-gc (consult-compile--error-candidates))
   :prompt "Go to error: "
   :category 'consult-compile-error
   :sort nil
   :require-match t
   :lookup (lambda (_ candidates cand)
             (when-let (cand (assoc cand candidates))
               (let ((marker (cadr cand))
                     (loc (compilation--message->loc (caddr cand))))
                 (when (buffer-live-p (marker-buffer marker))
                   (with-current-buffer
                       ;; taken from compile.el
                       (apply #'compilation-find-file
                              marker
                              (caar (compilation--loc->file-struct loc))
                              (cadar (compilation--loc->file-struct loc))
                              (compilation--file-struct->formats
                               (compilation--loc->file-struct loc)))
                     (goto-char (point-min))
                     ;; location might be invalid by now
                     (ignore-errors
                       (forward-line (- (compilation--loc->line loc) 1))
                       (forward-char (compilation--loc->col loc)))
                     (point-marker))))))
   :narrow `(,(lambda (cand)
                (= (pcase (compilation--message->type (caddr cand))
                     (0 ?i)
                     (1 ?w)
                     (_ ?e))
                   consult--narrow))
             (?e . "Error")
             (?w . "Warning")
             (?i . "Info"))
   :history '(:input consult--error-history)
   :state (consult--jump-state 'consult-preview-error)))

(provide 'consult-compile)
;;; consult-compile.el ends here
