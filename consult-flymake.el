;;; consult-flymake.el --- Provides the command `consult-flymake' -*- lexical-binding: t; -*-

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

;; Provides the command `consult-flymake'. This is an extra package,
;; to avoid loading Flymake.

;;; Code:

(require 'consult)
(require 'flymake)

(defun consult-flymake--candidates ()
  "Return Flymake errors as alist."
  (consult--forbid-minibuffer)
  (let* ((raw-diags (or (flymake-diagnostics)
                        (user-error "No flymake errors (Status: %s)"
                                    (if (flymake-is-running) 'running 'finished))))
         (diags
          (mapcar
           (lambda (x)
             (let* ((buffer (flymake-diagnostic-buffer x))
                    (type (flymake-diagnostic-type x))
                    (type-str (propertize (format "%s"
                                                  (flymake--lookup-type-property
                                                   type 'flymake-type-name type))
                                          'face (flymake--lookup-type-property
                                                 type 'mode-line-face 'flymake-error)))
                    (narrow (pcase (flymake--lookup-type-property type 'flymake-category)
                              ('flymake-error ?e)
                              ('flymake-warning ?w)
                              (_ ?n))))
               (with-current-buffer buffer
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (flymake-diagnostic-beg x))
                     (list (buffer-name buffer)
                           (line-number-at-pos)
                           type-str
                           (flymake-diagnostic-text x)
                           (point-marker)
                           narrow))))))
           raw-diags))
         (buffer-width (apply #'max (mapcar (lambda (x) (length (nth 0 x))) diags)))
         (line-width (apply #'max (mapcar (lambda (x) (length (number-to-string (nth 1 x)))) diags)))
         (type-width (apply #'max (mapcar (lambda (x) (length (nth 2 x))) diags)))
         (fmt (format "%%-%ds %%-%dd %%-%ds %%s" buffer-width line-width type-width)))
    (mapcar
     (pcase-lambda (`(,buffer ,line ,type ,text ,marker ,narrow))
       (list (format fmt buffer line type text) marker narrow))
     (sort diags
           (pcase-lambda (`(_ _ ,t1 _ ,m1 _) `(_ _ ,t2 _ ,m2 _))
             (or (string< t1 t2) (and (string= t1 t2) (< m1 m2))))))))

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
   :narrow `(,(lambda (cand) (= (caddr cand) consult--narrow))
             (?e . "Error")
             (?w . "Warning")
             (?n . "Note"))
   :lookup #'consult--lookup-cadr
   :state (consult--jump-state 'consult-preview-error)))

(provide 'consult-flymake)
;;; consult-flymake.el ends here
