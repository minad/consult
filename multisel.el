;;; multisel.el --- Consult `completing-read-multiple' -*- lexical-binding: t -*-

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

(require 'crm)
(require 'seq)

(defcustom multisel-prefix
  (cons "  " (propertize "✓ " 'face 'success))
  "Prefix for `consult-completing-read-multiple' candidates."
  :type '(cons string string)
  :group 'consult)

(defface multisel-selected
  '((t :inherit secondary-selection))
  "Face used to highlight selected items in `consult-completing-read-multiple'."
  :group 'consult-faces)

(defvar multisel--history nil
  "Current history list.")

(defvar multisel--all nil
  "List of all items.")

(defvar multisel--items nil
  "Reordered item lists, selected items in the front.")

(defvar multisel--selected nil
  "List of selected items.")

(defvar multisel--overlay nil
  "Selection indicator overlay.")

(defun multisel--format-selected (item)
  "Format selected ITEM."
  ;; Restore original candidate in order to preserve formatting
  (setq item (propertize (or (car (member item multisel--all)) item)
                         'multisel--selected t
                         'line-prefix (cdr multisel-prefix)))
  (add-face-text-property 0 (length item) 'multisel-selected 'append item)
  item)

(defun multisel--with-command-wrapper-1 (body)
  "Wrap every command in BODY with `multisel--command-wrapper'."
  (let ((hook (make-symbol "multisel--pre-command-hook"))
        (wrapped (make-symbol "multisel--command-wrapped"))
        (depth (1+ (recursion-depth)))
        (cmd))
    (fset hook (lambda ()
                 (when (and this-command (= depth (recursion-depth)))
                   (setq cmd this-command
                         this-command wrapped))))
    (fset wrapped (lambda ()
                    (interactive)
                    (multisel--command-wrapper cmd)))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook hook 90)
          (funcall body))
      (remove-hook 'pre-command-hook hook))))

(defmacro multisel--with-command-wrapper (&rest body)
  "Wrap every command in BODY with `multisel--command-wrapper'."
  `(multisel--with-command-wrapper-1 (lambda () ,@body)))

(defun multisel--command-wrapper (cmd)
  "Wrap CMD and catch minibuffer exit."
  (pcase (catch 'exit
           (call-interactively (setq this-command cmd))
           'continue)
    ('nil
     (with-selected-window (active-minibuffer-window)
       (let ((item (minibuffer-contents-no-properties)))
         (when (equal item "")
           (throw 'exit nil))
         (multisel--select item)
         (delete-minibuffer-contents)
         (when (bound-and-true-p vertico--input)
           (multisel-vertico-refresh)))))
    ('t (throw 'exit t))))

(defun multisel--update-overlay ()
  "Update selection indicator overlay."
  (unless multisel--overlay
    (when-let (pos (string-match-p "\\(?: (default[^)]+)\\)?: \\'" (minibuffer-prompt)))
      (setq multisel--overlay (make-overlay (+ (point-min) pos) (minibuffer-prompt-end)))))
  (when multisel--overlay
    (overlay-put multisel--overlay 'display
                 (when multisel--selected
                   (format " (%s selected): " (length multisel--selected))))))

(defun multisel--select (item)
  "Select or deselect ITEM."
  (setq multisel--history (nthcdr (length multisel--selected) multisel--history)
        multisel--selected
        (if (member item multisel--selected)
            ;; Multi selections are not possible.
            ;; This is probably no problem, since this is rarely desired.
            (delete item multisel--selected)
          (nconc multisel--selected (list (multisel--format-selected item))))
        multisel--history
        (nconc (mapcar #'substring-no-properties multisel--selected) multisel--history)
        multisel--items
        (append multisel--selected
                (seq-remove (lambda (x) (member x multisel--selected))
                            multisel--all)))
  (multisel--update-overlay))

(defun multisel--sort (md sort)
  "Return sorting function given metadata MD and SORT key."
  (pcase (alist-get sort md)
    ('identity `((,sort . identity)))
    ((and orig (guard orig))
     `((,sort . ,(lambda (cands)
                   (setq cands (funcall orig cands))
                   (nconc
                    (seq-filter (lambda (x) (member x multisel--selected)) cands)
                    (seq-remove (lambda (x) (member x multisel--selected)) cands))))))))

;;;###autoload
(defun consult-completing-read-multiple (prompt table &optional
                                                pred require-match initial-input
                                                hist def inherit-input-method)
  "Enhanced replacement for `completing-read-multiple'.
See `completing-read-multiple' for the documentation of the arguments."
  (let* ((hist (pcase hist
                 ('nil 'minibuffer-history)
                 ('t 'multisel--history)
                 (`(,sym . ,_) sym) ;; ignore history position
                 (_ hist)))
         (multisel--all
          (funcall
           (if-let (prefix (car multisel-prefix))
               (apply-partially #'mapcar (lambda (item) (propertize item 'line-prefix prefix)))
             #'identity)
           (all-completions "" table pred)))
         (multisel--selected
          (and initial-input
               (or
                ;; initial-input is multiple items
                (string-match-p crm-separator initial-input)
                ;; initial-input is a single candidate
                (member initial-input multisel--all))
               (prog1
                   (mapcar #'multisel--format-selected
                           (split-string initial-input crm-separator 'omit-nulls))
                 (setq initial-input nil))))
         (multisel--history (append (mapcar #'substring-no-properties multisel--selected)
                                       (symbol-value hist)))
         (multisel--items (append multisel--selected
                        (seq-remove (lambda (x) (member x multisel--selected))
                                    multisel--all)))
         (multisel--overlay)
         (orig-md (and (functionp table) (cdr (funcall table "" nil 'metadata))))
         (group-fun (alist-get 'group-function orig-md))
         (md
          `(metadata
            (group-function
             . ,(lambda (cand transform)
                  (if (get-text-property 0 'multisel--selected cand)
                      (if transform cand "Selected")
                    (or (and group-fun (funcall group-fun cand transform))
                        (if transform cand "Select multiple")))))
            ,@(multisel--sort orig-md 'cycle-sort-function)
            ,@(multisel--sort orig-md 'display-sort-function)
            ,@(seq-filter (lambda (x) (memq (car x) '(annotation-function
                                                      affixation-function
                                                      category)))
                          orig-md))))
    (minibuffer-with-setup-hook
        (:append (lambda ()
                   (multisel--update-overlay)
                   (if (bound-and-true-p vertico--input)
                       (use-local-map (make-composed-keymap (list multisel-vertico-map) (current-local-map))))))
      (multisel--with-command-wrapper
          (let ((result
                 (completing-read
                  prompt
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        md
                      (complete-with-action action multisel--items str pred)))
                  nil ;; predicate
                  require-match
                  initial-input
                  'multisel--history
                  "" ;; default
                  inherit-input-method)))
            (unless (or (equal result "") multisel--selected)
              (setq multisel--selected (list result)
                    multisel--history (cons result multisel--history))))))
    (set hist multisel--history)
    (when (consp def)
      (setq def (car def)))
    (if (and def (not (equal "" def)) (not multisel--selected))
        (split-string def crm-separator 'omit-nulls)
      multisel--selected)))

(defun multisel-vertico-select (&optional erase)
  (interactive)
  (let ((cand (vertico--candidate)))
    (when (and (not (equal cand ""))
               (vertico--match-p cand))
      (multisel--select cand)
      (if erase
          (progn
            (delete-minibuffer-contents)
            (setq vertico--lock-candidate nil))
        (vertico-next))
      (setq vertico--input t
            vertico--history-hash nil))))

(defun multisel-vertico-select-erase ()
  (interactive)
  (multisel-vertico-select t))

(defun multisel-vertico-commit ()
  (interactive)
  (when multisel--selected
      (delete-minibuffer-contents)
      (exit-minibuffer))
  (let ((cand (vertico--candidate)))
    (when (and (not (equal cand ""))
               (vertico--match-p cand))
      (multisel--select cand)
      (delete-minibuffer-contents)
      (add-hook 'post-command-hook #'exit-minibuffer nil 'local))))

(defvar multisel-vertico-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap vertico-insert] #'multisel-vertico-select-erase)
    (define-key map [remap exit-minibuffer] #'multisel-vertico-commit)
    (define-key map [backtab] #'multisel-vertico-select)
    map))

(defvar multisel-vertico-refresh ()
  (setq vertico--input t
        vertico--history-hash nil
        vertico--lock-candidate nil)
  (vertico--exhibit))

(provide 'multisel)
;;; multisel.el ends here
