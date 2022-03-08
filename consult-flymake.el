;;; consult-flymake.el --- Provides the command `consult-flymake' -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides the command `consult-flymake'. This is an extra package,
;; to allow lazy loading of flymake.el. The `consult-flymake' command
;; is autoloaded.

;;; Code:

(require 'consult)
(require 'flymake)

(defconst consult-flymake--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?n . "Note")))

(defun consult-flymake--candidates ()
  "Return Flymake errors as alist."
  (consult--forbid-minibuffer)
  (let* ((raw-diags (or (flymake-diagnostics)
                        (user-error "No flymake errors (Status: %s)"
                                    (if (seq-difference (flymake-running-backends)
                                                        (flymake-reporting-backends))
                                        'running 'finished))))
         (diags
          (mapcar
           (lambda (diag)
             (let ((buffer (flymake-diagnostic-buffer diag))
                   (type (flymake-diagnostic-type diag)))
               (with-current-buffer buffer
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (flymake-diagnostic-beg diag))
                     (list (buffer-name buffer)
                           (line-number-at-pos)
                           type
                           (flymake-diagnostic-text diag)
                           (point-marker)
                           (pcase (flymake--lookup-type-property type 'flymake-category)
                              ('flymake-error ?e)
                              ('flymake-warning ?w)
                              (_ ?n))))))))
           raw-diags))
         (buffer-width (apply #'max (mapcar (lambda (x) (length (nth 0 x))) diags)))
         (line-width (apply #'max (mapcar (lambda (x) (length (number-to-string (nth 1 x)))) diags)))
         (fmt (format "%%-%ds %%-%dd %%-7s %%s" buffer-width line-width)))
    (mapcar
     (pcase-lambda (`(,buffer ,line ,type ,text ,marker ,narrow))
       (propertize (format fmt buffer line
                           (propertize (format "%s" (flymake--lookup-type-property
                                                     type 'flymake-type-name type))
                                       'face (flymake--lookup-type-property
                                              type 'mode-line-face 'flymake-error))
                           text)
                   'consult--candidate marker
                   'consult--type narrow))
     (sort diags
           (pcase-lambda (`(_ _ ,t1 _ ,m1 _) `(_ _ ,t2 _ ,m2 _))
             (let ((s1 (flymake--severity t1))
                   (s2 (flymake--severity t2)))
               (or (> s1 s2) (and (= s1 s2) (< m1 m2)))))))))

;;;###autoload
(defun consult-flymake ()
  "Jump to Flymake diagnostic."
  (interactive)
  (consult--read
   (consult--with-increased-gc (consult-flymake--candidates))
   :prompt "Flymake diagnostic: "
   :category 'consult-flymake-error
   :history t ;; disable history
   :require-match t
   :sort nil
   :group (consult--type-group consult-flymake--narrow)
   :narrow (consult--type-narrow consult-flymake--narrow)
   :lookup #'consult--lookup-candidate
   :state (consult--jump-state 'consult-preview-error)))

(provide 'consult-flymake)
;;; consult-flymake.el ends here
