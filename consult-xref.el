;;; consult-xref.el --- Xref integration for Consult -*- lexical-binding: t -*-

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

;; Provides Xref integration for Consult. This is an extra package, to avoid
;; loading compile.el. The `consult-xref' function is autoloaded.

;;; Code:

(require 'consult)
(require 'xref)

(defvar consult-xref--history nil)

(defun consult-xref--candidates (xrefs)
  "Return candidate list from XREFS."
  (mapcar (lambda (xref)
            (let ((loc (xref-item-location xref))
                  (xref-file-name-display 'nondirectory))
              (cons
               (concat
                (consult--format-location (xref-location-group loc)
                                          (or (xref-location-line loc) 0))
                (xref-item-summary xref))
               xref)))
          xrefs))

(defun consult-xref--preview ()
  "Xref preview function."
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (cand restore)
      (cond
       (restore
        (funcall preview nil t)
        (funcall open nil))
       (cand
        (let ((loc (xref-item-location cand)))
          (funcall preview
                   ;; Only preview file and buffer markers
                   (cond
                    ((xref-buffer-location-p loc)
                     (xref-location-marker loc))
                    ((xref-file-location-p loc)
                     (consult--position-marker
                      (funcall open (oref loc file))
                      (oref loc line)
                      (oref loc column)))
                    (t (message "No preview for %s" (type-of loc))))
                   nil)))))))

;;;###autoload
(defun consult-xref (fetcher &optional alist)
  "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments."
  (let ((candidates (consult--with-increased-gc
                     (consult-xref--candidates (funcall fetcher)))))
    (xref-pop-to-location
     (if (cdr candidates)
         (consult--read
          candidates
          :prompt "Go to xref: "
          :history 'consult-xref--history
          :require-match t
          :sort nil
          :category 'xref-location
          :state (consult-xref--preview)
          :lookup #'consult--lookup-cdr)
       (cdar candidates))
     (alist-get 'display-action alist))))

(provide 'consult-xref)
;;; consult-xref.el ends here
