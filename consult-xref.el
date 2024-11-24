;;; consult-xref.el --- Xref integration for Consult -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides Xref integration for Consult.  This is an extra package, to
;; allow lazy loading of xref.el.  The `consult-xref' function is
;; autoloaded.

;;; Code:

(require 'consult)
(require 'xref)
(eval-when-compile (require 'subr-x))

(defvar consult-xref--history nil)

(defvar consult-xref--fetcher nil
  "The current xref fetcher.
The fetch is stored globally such that it can be accessed by
 Embark for `embark-export'.")

(defvar consult-xref--preview
  '(xref-buffer-location xref-file-location xref-etags-location)
  "Only the xref types listed here are previewed.")

(defun consult-xref--candidates ()
  "Return xref candidate list."
  (let ((root (consult--project-root)))
    (mapcar (lambda (xref)
              (let* ((loc (xref-item-location xref))
                     (group (xref-location-group loc))
                     (group (if root (string-remove-prefix root group) group))
                     (cand (consult--format-file-line-match
                            group
                            (or (xref-location-line loc) 0)
                            (xref-item-summary xref))))
                (add-text-properties
                 0 1 `(consult-xref ,xref consult--prefix-group ,group) cand)
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
                 (when-let ((loc (and cand (eq action 'preview)
                                      (xref-item-location cand)))
                            (type (type-of loc))
                            ;; Only preview xrefs listed in consult-xref--preview
                            ((memq type consult-xref--preview)))
                   (pcase type
                     ((or 'xref-file-location 'xref-etags-location)
                      (consult--marker-from-line-column
                       (funcall open
                                ;; xref-location-group returns the file name
                                (let ((xref-file-name-display 'abs))
                                  (xref-location-group loc)))
                       (xref-location-line loc)
                       (if (eq type 'xref-file-location)
                           (xref-file-location-column loc)
                         0)))
                     (_ (xref-location-marker loc)))))))))

;;;###autoload
(defun consult-xref (fetcher &optional alist)
  "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments."
  (let* ((consult-xref--fetcher fetcher)
         (candidates (consult-xref--candidates))
         (display (alist-get 'display-action alist))
         (this-command #'consult-xref))
    (unless candidates
      (user-error "No xref locations"))
    (xref-pop-to-location
     (if (cdr candidates)
         (consult--read
          candidates
          :prompt "Go to xref: "
          :history 'consult-xref--history
          :require-match t
          :sort nil
          :category 'consult-xref
          :group #'consult--prefix-group
          :state
          ;; do not preview other frame
          (when-let (fun (pcase-exhaustive display
                           ('frame nil)
                           ('window #'switch-to-buffer-other-window)
                           ('nil #'switch-to-buffer)))
            (consult-xref--preview fun))
          :lookup (apply-partially #'consult--lookup-prop 'consult-xref))
       (get-text-property 0 'consult-xref (car candidates)))
     display)))

(defun consult--xref-go-back (pos)
  "Jump to POS in the Xref backward stack."
  (when pos
    (when (consp pos) (setq pos (car pos)))
    ;; Pop from the backward stack until we reach the selected marker, and
    ;; push the current position to the forward stack.
    (let ((history (xref--get-history)))
      (while (not (or (null (car (car history))) (equal (car (car history)) pos)))
        (pop (car history)))
      (xref--push-forward (point-marker)))
    ;; Jump to the selected marker, ensuring the buffer exists and any narrowed
    ;; regions are visible.
    (when (consult--jump-ensure-buffer pos)
      (unless (= (goto-char pos) (point))
        (widen)
        (goto-char pos)))
    (consult--invisible-open-permanently)
    (run-hooks 'consult-after-jump-hook))
  nil)

(defun consult--xref-back-state ()
  "The state function used to select a candidate position in backward history."
  (consult--state-with-return (consult--jump-preview)
                              #'consult--xref-go-back))

;;;###autoload
(defun consult-xref-back ()
  "Jump to a marker in the current Xref backward stack.

The command supports preview of the currently selected marker position."
  (interactive)
  (consult--read
   (consult--global-mark-candidates (car (xref--get-history)))
   :prompt "Go to previous bookmark: "
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :state (consult--xref-back-state)))

(defun consult--xref-go-forward (pos)
  "Jump to POS in the Xref forward stack."
  (when pos
    (when (consp pos) (setq pos (car pos)))
    ;; Pop from the forward stack until we reach the selected marker, and push
    ;; the current position to the backward stack.
    (let ((history (xref--get-history)))
      (while (not (or (null (car (cdr history))) (equal (car (cdr history)) pos)))
        (pop (cdr history)))
      (xref--push-backward (point-marker)))
    ;; Jump to the selected marker, ensuring the buffer exists and any narrowed
    ;; regions are visible.
    (when (consult--jump-ensure-buffer pos)
      (unless (= (goto-char pos) (point))
        (widen)
        (goto-char pos)))
    (consult--invisible-open-permanently)
    (run-hooks 'consult-after-jump-hook))
  nil)

(defun consult--xref-forward-state ()
  "The state function used to selected a candidate position in forward history."
  (consult--state-with-return (consult--jump-preview)
                              #'consult--xref-go-forward))

;;;###autoload
(defun consult-xref-forward ()
  "Jump to a marker in the current Xref forward stack.

The command supports preview of the currently selected marker position."
  (interactive)
  (consult--read
   (consult--global-mark-candidates (cdr (xref--get-history)))
   :prompt "Go to previous bookmark: "
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :state (consult--xref-forward-state)))

(provide 'consult-xref)
;;; consult-xref.el ends here
