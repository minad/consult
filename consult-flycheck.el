;;; consult-flycheck.el --- Jump to flycheck errors with consult  -*- lexical-binding: t; -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.1") (flycheck "31") (emacs "26.1"))
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

;; Provides the command `consult-flycheck'.

;;; Code:

(require 'consult)
(require 'flycheck)

;;;; Customization

(defcustom consult-flycheck-preview t
  "Enable flycheck preview during selection."
  :type 'boolean
  :group 'consult)

(defun consult-flycheck--candidates ()
  "Return flycheck errors as alist."
  (consult--forbid-minibuffer)
  (unless (bound-and-true-p flycheck-current-errors)
    (user-error "No flycheck errors (Status: %s)"
                (if (boundp 'flycheck-last-status-change)
                    flycheck-last-status-change
                  'not-installed)))
  (let* ((errors (mapcar
                  (lambda (err)
                    (list (file-name-nondirectory (flycheck-error-filename err))
                          (number-to-string (flycheck-error-line err))
                          err))
                  (seq-sort #'flycheck-error-level-< flycheck-current-errors)))
         (file-width (apply #'max (mapcar (lambda (x) (length (car x))) errors)))
         (line-width (apply #'max (mapcar (lambda (x) (length (cadr x))) errors)))
         (fmt (format "%%-%ds %%-%ds %%-7s %%s (%%s)" file-width line-width)))
    (mapcar
     (pcase-lambda (`(,file ,line ,err))
       (flycheck-jump-to-error err)
       (cons
        (format fmt
                (propertize file 'face 'flycheck-error-list-filename)
                (propertize line 'face 'flycheck-error-list-line-number)
                (let ((level (flycheck-error-level err)))
                  (propertize (symbol-name level) 'face (flycheck-error-level-error-list-face level)))
                (propertize (flycheck-error-message err)
                            'face 'flycheck-error-list-error-message)
                (propertize (symbol-name (flycheck-error-checker err))
                            'face 'flycheck-error-list-checker-name))
        (point-marker)))
     errors)))

;;;###autoload
(defun consult-flycheck ()
  "Jump to flycheck error."
  (interactive)
  (consult--goto
   (save-excursion
     (consult--read "Flycheck error: "
                    (consult--with-increased-gc (consult-flycheck--candidates))
                    :category 'flycheck-error
                    :require-match t
                    :sort nil
                    :lookup #'consult--lookup-list
                    :preview (and consult-flycheck-preview #'consult--preview-position)))))


(provide 'consult-flycheck)
;;; consult-flycheck.el ends here
