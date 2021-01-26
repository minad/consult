;;; consult-xref.el --- Provides the command `consult-xref' -*- lexical-binding: t; -*-

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

;; Provide support for showing xref with consult, with preview.
;; Use the function (setup-consult-xref) to set it up.

;;; Code:

(require 'consult)
(require 'xref)

(defun xref-show-definitions-consult (fetcher alist)
  "Let the user choose the target definition with completion.

When there is more than one definition, let the user choose
between them by typing in the minibuffer with completion."
  (let* ((xrefs (funcall fetcher))
         (xref-alist (xref--analyze xrefs))
         xref-alist-with-line-info
         xref
         (group-prefix-length
          ;; FIXME: Groups are not always file names, but they often
          ;; are.  At least this shouldn't make the other kinds of
          ;; groups look worse.
          (let ((common-prefix (try-completion "" xref-alist)))
            (if (> (length common-prefix) 0)
                (length (file-name-directory common-prefix))
              0))))

    (cl-loop for ((group . xrefs) . more1) on xref-alist
             do
             (cl-loop for (xref . more2) on xrefs do
                      (with-slots (summary location) xref
                        (let* ((line (xref-location-line location))
                               (line-fmt
                                (if line
                                    (format #("%d:" 0 2 (face xref-line-number))
                                            line)
                                  ""))
                               (group-fmt
                                (propertize
                                 (substring group group-prefix-length)
                                 'face 'xref-file-header))
                               (candidate
                                (format "%s:%s%s" group-fmt line-fmt summary)))
                          (push (cons candidate xref) xref-alist-with-line-info)))))

    (setq xref (if (not (cdr xrefs))
                   (car xrefs)
                 (let* ((collection (reverse xref-alist-with-line-info))
                        (def (caar collection)))
                   (cdr (assoc (consult--read collection
                                              :prompt "xref results: "
                                              :history nil
                                              :require-match t
                                              :predicate nil
                                              :default def
                                              :state (consult--xref-preview collection))
                               collection)))))
    (xref-pop-to-location xref (assoc-default 'display-action alist))))

(defun consult--xref-preview (collection)
  "Preview the xref result from consult.

COLLECTION is the collection of definition candidates given by xref."
  (let ((preview (consult--jump-preview))
        (open (consult--temporary-files)))
    (lambda (cand restore)
      (cond
       (restore
        (funcall open)
        (funcall preview nil t))
       (t (let ((cand-xref (cdr (assoc cand collection))))
                   (xref-pop-to-location cand-xref nil)))))))

(defun setup-consult-xref ()
  "Set up consult with xref.

One can also manually set the variables `xref-show-xrefs-function' and
`xref-show-definitions-function' to the function `xref-show-definitions-consult'."
  (interactive)
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'xref-show-definitions-consult))
  (setq xref-show-xrefs-function #'xref-show-definitions-consult))

(provide 'consult-xref)
;;; consult-xref.el ends here
