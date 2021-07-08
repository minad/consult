;;; consult-crm.el --- Consult `completing-read-multiple' -*- lexical-binding: t -*-

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

;; Provides `consult-completing-read-multiple', which is an an enhanced
;; version of `completing-read-multiple'.

;;; Code:

(require 'consult)
(require 'crm)

(defcustom consult-crm-prefix
  (cons "  " (propertize "✓ " 'face 'success))
  "Prefix for `consult-completing-read-multiple' candidates."
  :type '(cons string string)
  :group 'consult)

(defface consult-crm-selected
  '((t :inherit secondary-selection))
  "Face used to highlight selected items in `consult-completing-read-multiple'."
  :group 'consult-faces)

(defvar consult-crm-map (make-sparse-keymap)
  "Additional minibuffer keymap used by `consult-completing-read-multiple'.")

(defvar consult-crm--history nil
  "Current history list.")

(defvar consult-crm--all nil
  "List of all items.")

(defvar consult-crm--items nil
  "Reordered item lists, selected items in the front.")

(defvar consult-crm--selected nil
  "List of selected items.")

(defvar consult-crm--overlay nil
  "Selection indicator overlay.")

(defun consult-crm--format-selected (item)
  "Format selected ITEM."
  ;; Restore original candidate in order to preserve formatting
  (setq item (propertize (or (car (member item consult-crm--all)) item)
                         'consult--crm-selected t
                         'line-prefix (cdr consult-crm-prefix)))
  (add-face-text-property 0 (length item) 'consult-crm-selected 'append item)
  item)

(defun consult-crm--with-command-wrapper-1 (body)
  "Wrap every command in BODY with `consult-crm--command-wrapper'."
  (let ((hook (make-symbol "consult-crm--pre-command-hook"))
        (wrapped (make-symbol "consult-crm--command-wrapped"))
        (depth (1+ (recursion-depth)))
        (cmd))
    (fset hook (lambda ()
                 (when (and this-command (= depth (recursion-depth)))
                   (setq cmd this-command
                         this-command wrapped))))
    (fset wrapped (lambda ()
                    (interactive)
                    (consult-crm--command-wrapper cmd)))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook hook 90)
          (funcall body))
      (remove-hook 'pre-command-hook hook))))

(defmacro consult-crm--with-command-wrapper (&rest body)
  "Wrap every command in BODY with `consult-crm--command-wrapper'."
  `(consult-crm--with-command-wrapper-1 (lambda () ,@body)))

(defun consult-crm--command-wrapper (cmd)
  "Wrap CMD and catch minibuffer exit."
  (pcase (catch 'exit
           (call-interactively (setq this-command cmd))
           'continue)
    ('nil
     (with-selected-window (active-minibuffer-window)
       (let ((item (minibuffer-contents-no-properties)))
         (when (equal item "")
           (throw 'exit nil))
         (consult-crm--select item)
         (delete-minibuffer-contents)
         (run-hook-with-args 'consult--completion-refresh-hook 'reset))))
    ('t (throw 'exit t))))

(defun consult-crm--update-overlay ()
  "Update selection indicator overlay."
  (unless consult-crm--overlay
    (when-let (pos (string-match-p "\\(?: (default[^)]+)\\)?: \\'" (minibuffer-prompt)))
      (setq consult-crm--overlay (make-overlay (+ (point-min) pos) (minibuffer-prompt-end)))))
  (when consult-crm--overlay
    (overlay-put consult-crm--overlay 'display
                 (when consult-crm--selected
                   (format " (%s selected): " (length consult-crm--selected))))))

(defun consult-crm--select (item)
  "Select or deselect ITEM."
  (setq consult-crm--history (nthcdr (length consult-crm--selected) consult-crm--history)
        consult-crm--selected
        (if (member item consult-crm--selected)
            ;; Multi selections are not possible.
            ;; This is probably no problem, since this is rarely desired.
            (delete item consult-crm--selected)
          (nconc consult-crm--selected (list (consult-crm--format-selected item))))
        consult-crm--history
        (nconc (mapcar #'substring-no-properties consult-crm--selected) consult-crm--history)
        consult-crm--items
        (append consult-crm--selected
                (seq-remove (lambda (x) (member x consult-crm--selected))
                            consult-crm--all)))
  (consult-crm--update-overlay))

(defun consult-crm--sort (md sort)
  "Return sorting function given metadata MD and SORT key."
  (pcase (alist-get sort md)
    ('identity `((,sort . identity)))
    ((and orig (guard orig))
     `((,sort . ,(lambda (cands)
                   (setq cands (funcall orig cands))
                   (nconc
                    (seq-filter (lambda (x) (member x consult-crm--selected)) cands)
                    (seq-remove (lambda (x) (member x consult-crm--selected)) cands))))))))

;;;###autoload
(defun consult-completing-read-multiple (prompt table &optional
                                                pred require-match initial-input
                                                hist def inherit-input-method)
  "Enhanced replacement for `completing-read-multiple'.
See `completing-read-multiple' for the documentation of the arguments."
  (let* ((hist (pcase hist
                 ('nil 'minibuffer-history)
                 ('t 'consult-crm--history)
                 (`(,sym . ,_) sym) ;; ignore history position
                 (_ hist)))
         (consult-crm--all
          (funcall
           (if-let (prefix (car consult-crm-prefix))
               (apply-partially #'mapcar (lambda (item) (propertize item 'line-prefix prefix)))
             #'identity)
           (all-completions "" table pred)))
         (consult-crm--selected
          (and initial-input
               (or
                ;; initial-input is multiple items
                (string-match-p crm-separator initial-input)
                ;; initial-input is a single candidate
                (member initial-input consult-crm--all))
               (prog1
                   (mapcar #'consult-crm--format-selected
                           (split-string initial-input crm-separator 'omit-nulls))
                 (setq initial-input nil))))
         (consult-crm--history (append (mapcar #'substring-no-properties consult-crm--selected)
                                       (symbol-value hist)))
         (consult-crm--items (append consult-crm--selected
                        (seq-remove (lambda (x) (member x consult-crm--selected))
                                    consult-crm--all)))
         (consult-crm--overlay)
         (orig-md (and (functionp table) (cdr (funcall table "" nil 'metadata))))
         (group-fun (alist-get 'group-function orig-md))
         (md
          `(metadata
            (group-function
             . ,(lambda (cand transform)
                  (if (get-text-property 0 'consult--crm-selected cand)
                      (if transform cand "Selected")
                    (or (and group-fun (funcall group-fun cand transform))
                        (if transform cand "Select multiple")))))
            ,@(consult-crm--sort orig-md 'cycle-sort-function)
            ,@(consult-crm--sort orig-md 'display-sort-function)
            ,@(seq-filter (lambda (x) (memq (car x) '(annotation-function
                                                      affixation-function
                                                      category)))
                          orig-md))))
    (consult--minibuffer-with-setup-hook
        (:append (lambda ()
                   (consult-crm--update-overlay)
                   (use-local-map (make-composed-keymap (list consult-crm-map) (current-local-map)))))
      (consult-crm--with-command-wrapper
          (let ((result
                 (completing-read
                  prompt
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        md
                      (complete-with-action action consult-crm--items str pred)))
                  nil ;; predicate
                  require-match
                  initial-input
                  'consult-crm--history
                  "" ;; default
                  inherit-input-method)))
            (unless (or (equal result "") consult-crm--selected)
              (setq consult-crm--selected (list result)
                    consult-crm--history (cons result consult-crm--history))))))
    (set hist consult-crm--history)
    (when (consp def)
      (setq def (car def)))
    (if (and def (not (equal "" def)) (not consult-crm--selected))
        (split-string def crm-separator 'omit-nulls)
      consult-crm--selected)))

(provide 'consult-crm)
;;; consult-crm.el ends here
