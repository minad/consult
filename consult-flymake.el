;;; consult-flymake.el --- Provides the command `consult-flymake'  -*- lexical-binding: t; -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.1") (emacs "26.1"))
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

;; Provides the command `consult-flymake'.  This is an extra package,
;; to avoid loading Flymake.

;;; Code:

(require 'consult)
(require 'flymake)

(defcustom consult-preview-flymake t
  "Enable Flymake preview during selection."
  :type 'boolean
  :group 'consult-preview)

(defun consult-flymake--diag-type-name (diag)
  "Return the type name of DIAG."
  (let ((type (flymake-diagnostic-type diag)))
    (format "%s"
            (flymake--lookup-type-property
             type 'flymake-type-name type))))

(defun consult-flymake--candidates ()
  "Return Flymake errors as alist."
  (consult--forbid-minibuffer)
  (unless (flymake-diagnostics)
    (user-error "No Flymake diagnostics"))
  (let* ((diagnostics (flymake-diagnostics))
         (buffer-width (apply #'max (mapcar (lambda (x) (length (format "%s" (flymake-diagnostic-buffer x)))) diagnostics)))
         (category-width (apply #'max (mapcar (lambda (x) (length (consult-flymake--diag-type-name x))) diagnostics)))
         (fmt (format "%%-%ds %%-%ds %%s" buffer-width category-width)))
    (mapcar
     (lambda (diag)
       (with-current-buffer (flymake--diag-buffer diag)
         (goto-char (flymake--diag-beg diag)))
       (let ((type (flymake-diagnostic-type diag)))
         (cons
          (consult--narrow-candidate
           (pcase type
             (:error "e")
             (:warning "w")
             (_ "n"))
           (format fmt
                   (flymake-diagnostic-buffer diag)
                   (propertize
                    (consult-flymake--diag-type-name diag)
                    'face
                    (flymake--lookup-type-property (flymake-diagnostic-type diag) 'mode-line-face))
                   (flymake-diagnostic-text diag)))
          (point-marker))))
     (flymake-diagnostics))))

;;;###autoload
(defun consult-flymake ()
  "Jump to Flymake diagnostic."
  (interactive)
  (consult--jump
   (consult--read "Flymake diagnostic: "
                  (consult--with-increased-gc (consult-flymake--candidates))
                  :category 'flymake-error
                  :require-match t
                  :sort nil
                  :narrow '(("e" . "Error")
                            ("w" . "Warning")
                            ("n" . "Note"))
                  :lookup #'consult--lookup-list
                  :preview (and consult-preview-flymake #'consult--preview-position))))

(provide 'consult-flymake)
;;; consult-flymake.el ends here
