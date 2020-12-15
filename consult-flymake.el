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

(defsubst consult-flymake--diag-line (diag)
  "Return the line number of DIAG."
  (save-excursion
    (with-current-buffer (flymake-diagnostic-buffer diag)
      (goto-char (flymake-diagnostic-beg diag))
      (line-number-at-pos))))

(defsubst consult-flymake--type-property (type prop)
  "Return the PROP of TYPE."
  (let* ((category (get type 'flymake-category))
         (default (get category prop))
         (val (get type prop)))
    (or val default)))

(defun consult-flymake--candidates ()
  "Return Flymake errors as alist."
  (consult--forbid-minibuffer)
  (unless (flymake-diagnostics)
    (user-error "No Flymake diagnostics"))
  (let* ((diagnostics (flymake-diagnostics))
         (buffer-width (apply #'max (mapcar (lambda (x) (length (format "%s" (flymake-diagnostic-buffer x)))) diagnostics)))
         (line-width (apply #'max (mapcar (lambda (x) (length (format "%s" (consult-flymake--diag-line x)))) diagnostics)))
         (type-name-width (apply #'max (mapcar (lambda (x) (length (consult-flymake--type-property
                                                                    (flymake-diagnostic-type x) 'flymake-type-name))) diagnostics)))
         (fmt (format "%%-%ds %%-%ds %%-%ds %%s" buffer-width line-width type-name-width)))
    (mapcar
     (lambda (diag)
       (with-current-buffer (flymake-diagnostic-buffer diag)
         (goto-char (flymake-diagnostic-beg diag)))
       (let* ((type (flymake-diagnostic-type diag))
              (category (get type 'flymake-category)))
         (cons
          (consult--narrow-candidate
           (pcase category
             ('flymake-error ?e)
             ('flymake-warning ?w)
             (_ ?n))
           (format fmt
                   (flymake-diagnostic-buffer diag)
                   (consult-flymake--diag-line diag)
                   (propertize
                    (consult-flymake--type-property type 'flymake-type-name)
                    'face
                    (consult-flymake--type-property type 'mode-line-face))
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
                  :narrow '((?e . "Error")
                            (?w . "Warning")
                            (?n . "Note"))
                  :lookup #'consult--lookup-list
                  :preview (and consult-preview-flymake #'consult--preview-position))))

(provide 'consult-flymake)
;;; consult-flymake.el ends here
