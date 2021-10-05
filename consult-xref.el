;;; consult-xref.el --- Xref integration for Consult -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

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

;; Provides Xref integration for Consult. This is an extra package, to
;; allow lazy loading of xref.el. The `consult-xref' function is
;; autoloaded.

;;; Code:

(require 'consult)
(require 'xref)

(defvar consult-xref--history nil)

(defun consult-xref--candidates (xrefs)
  "Return candidate list from XREFS."
  (mapcar (lambda (xref)
            (let* ((loc (xref-item-location xref))
                   (group (xref-location-group loc))
                   (cand (consult--format-location group
                                                   (or (xref-location-line loc) 0)
                                                   (xref-item-summary xref))))
              (add-text-properties
               0 1 `(consult--candidate ,xref consult-xref--group ,group) cand)
              cand))
          xrefs))

(defun consult-xref--preview (display)
  "Xref preview with DISPLAY function."
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (cand restore)
      (cond
       (restore
        (funcall preview nil t)
        (funcall open nil))
       (cand
        (let ((loc (xref-item-location cand))
              (consult--buffer-display display))
          (funcall preview
                   ;; Only preview file and buffer markers
                   (cl-typecase loc
                     (xref-buffer-location
                      (xref-location-marker loc))
                     (xref-file-location
                      ;; As of Emacs 28, xref uses cl-defstruct,
                      ;; whereas earlier versions use EIEIO
                      (if (cl-struct-p loc)
                          (consult--position-marker
                           (funcall open (xref-file-location-file loc))
                           (xref-file-location-line loc)
                           (xref-file-location-column loc))
                        (consult--position-marker
                         (funcall open (oref loc file))
                         (oref loc line)
                         (oref loc column))))
                     (t (message "No preview for %s" (type-of loc)) nil))
                   nil)))))))

(defun consult-xref--group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (if transform
      (substring cand (1+ (length (get-text-property 0 'consult-xref--group cand))))
    (get-text-property 0 'consult-xref--group cand)))

;;;###autoload
(defun consult-xref (fetcher &optional alist)
  "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments."
  (let ((candidates (consult--with-increased-gc
                     (consult-xref--candidates (funcall fetcher))))
        (display (alist-get 'display-action alist)))
    (xref-pop-to-location
     (if (cdr candidates)
         (apply
          #'consult--read
          candidates
          (append
           (alist-get #'consult-xref consult--read-config)
           (list
            :prompt "Go to xref: "
            :history 'consult-xref--history
            :require-match t
            :sort nil
            :category 'xref-location
            :group #'consult-xref--group
            :state
            ;; do not preview other frame
            (when-let (fun (pcase-exhaustive display
                             ('frame nil)
                             ('window #'switch-to-buffer-other-window)
                             ('nil #'switch-to-buffer)))
              (consult-xref--preview fun))
            :lookup #'consult--lookup-candidate)))
       (get-text-property 0 'consult--candidate (car candidates)))
     display)))

(provide 'consult-xref)
;;; consult-xref.el ends here
