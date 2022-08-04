;;; consult-xref.el --- Xref integration for Consult -*- lexical-binding: t -*-

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

;; Provides Xref integration for Consult. This is an extra package, to
;; allow lazy loading of xref.el. The `consult-xref' function is
;; autoloaded.

;;; Code:

(require 'consult)
(require 'xref)

(defvar consult-xref--history nil)
(defvar consult-xref--fetcher nil)

(defun consult-xref--candidates ()
  "Return xref candidate list."
  (let ((root (consult--project-root)))
    (mapcar (lambda (xref)
              (let* ((loc (xref-item-location xref))
                     (group (if (fboundp 'xref--group-name-for-display)
                                ;; This function is available in xref 1.3.2
                                (xref--group-name-for-display
                                 (xref-location-group loc) root)
                              (xref-location-group loc)))
                     (cand (consult--format-location
                            group
                            (or (xref-location-line loc) 0)
                            (xref-item-summary xref))))
                (add-text-properties
                 0 1 `(consult-xref ,xref consult-xref--group ,group) cand)
                cand))
            (funcall consult-xref--fetcher))))

(defun consult-xref--preview (display)
  "Xref preview with DISPLAY function."
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (let ((consult--buffer-display display))
        (funcall preview action
                 (when-let (loc (and cand (eq action 'preview)
                                     (xref-item-location cand)))
                   (let ((type (type-of loc)))
                     ;; Only preview file and buffer markers
                     (pcase type
                       ('xref-buffer-location
                        (xref-location-marker loc))
                       ((or 'xref-file-location 'xref-etags-location)
                        (consult--position-marker
                         (funcall open
                                  ;; xref-location-group returns the file name
                                  (let ((xref-file-name-display 'abs))
                                    (xref-location-group loc)))
                         (xref-location-line loc)
                         (if (eq type 'xref-file-location)
                             (xref-file-location-column loc)
                           0)))))))))))

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
  (let* ((consult-xref--fetcher fetcher)
         (candidates (consult-xref--candidates))
         (display (alist-get 'display-action alist)))
    (xref-pop-to-location
     (if (cdr candidates)
         (apply
          #'consult--read
          candidates
          (append
           (consult--customize-get #'consult-xref)
           (list
            :prompt "Go to xref: "
            :history 'consult-xref--history
            :require-match t
            :sort nil
            :category 'consult-xref
            :group #'consult-xref--group
            :state
            ;; do not preview other frame
            (when-let (fun (pcase-exhaustive display
                             ('frame nil)
                             ('window #'switch-to-buffer-other-window)
                             ('nil #'switch-to-buffer)))
              (consult-xref--preview fun))
            :lookup (apply-partially #'consult--lookup-prop 'consult-xref))))
       (get-text-property 0 'consult-xref (car candidates)))
     display)))

(provide 'consult-xref)
;;; consult-xref.el ends here
