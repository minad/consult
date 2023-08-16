;;; consult-imenu.el --- Consult commands for imenu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

;; Provides imenu-related Consult commands.

;;; Code:

(require 'consult)
(require 'imenu)

(defcustom consult-imenu-config
  '((emacs-lisp-mode :toplevel "Functions"
                     :types ((?f "Functions" font-lock-function-name-face)
                             (?m "Macros"    font-lock-function-name-face)
                             (?p "Packages"  font-lock-constant-face)
                             (?t "Types"     font-lock-type-face)
                             (?v "Variables" font-lock-variable-name-face))))
  "Imenu configuration, faces and narrowing keys used by `consult-imenu'.

For each type a narrowing key and a name must be specified.  The
face is optional.  The imenu representation provided by the
backend usually puts functions directly at the toplevel.
`consult-imenu' moves them instead under the type specified by
:toplevel."
  :type '(repeat (cons symbol plist))
  :group 'consult)

(defface consult-imenu-prefix
  '((t :inherit consult-key))
  "Face used to highlight imenu prefix in `consult-imenu'."
  :group 'consult-faces)

(defvar consult-imenu--history nil)
(defvar-local consult-imenu--cache nil)

(defun consult-imenu--switch-buffer (name pos buf fn &rest args)
  "Switch buffer before invoking special menu items.
NAME is the item name.
POS is the position.
BUF is the buffer.
FN is the original special item function.
ARGS are the arguments to the special item function."
  (funcall consult--buffer-display buf)
  (apply fn name pos args))

(defun consult-imenu--normalize (pos)
  "Return normalized imenu POS."
  (pcase pos
    ;; Simple marker item
    ((pred markerp) nil)
    ;; Simple integer item
    ((pred integerp) (setq pos (copy-marker pos)))
    ;; Semantic uses overlay for positions
    ((pred overlayp) (setq pos (copy-marker (overlay-start pos))))
    ;; Wrap special item
    (`(,pos ,fn . ,args)
     (setq pos `(,pos ,#'consult-imenu--switch-buffer ,(current-buffer)
                      ,fn ,@args)))
    (_ (error "Unknown imenu item: %S" pos)))
  (if (or (consp pos)
          (eq imenu-default-goto-function #'imenu-default-goto-function))
      pos
    (list pos #'consult-imenu--switch-buffer (current-buffer)
          imenu-default-goto-function)))

(defun consult-imenu--flatten (prefix face list types)
  "Flatten imenu LIST.
PREFIX is prepended in front of all items.
FACE is the item face.
TYPES is the mode-specific types configuration."
  (mapcan
   (lambda (item)
     (if (imenu--subalist-p item)
         (let* ((name (concat (car item)))
                (next-prefix name)
                (next-face face))
           (add-face-text-property 0 (length name)
                                   'consult-imenu-prefix 'append name)
           (if prefix
               (setq next-prefix (concat prefix "/" name))
             (when-let (type (cdr (assoc name types)))
               (put-text-property 0 (length name) 'consult--type (car type) name)
               (setq next-face (cadr type))))
           (consult-imenu--flatten next-prefix next-face (cdr item) types))
       (list (cons
              (if prefix
                  (let ((key (concat prefix " " (car item))))
                    (add-face-text-property (1+ (length prefix)) (length key)
                                            face 'append key)
                    key)
                (car item))
              (consult-imenu--normalize (cdr item))))))
   list))

(defun consult-imenu--compute ()
  "Compute imenu candidates."
  (consult--forbid-minibuffer)
  (let* ((imenu-use-markers t)
         ;; Generate imenu, see `imenu--make-index-alist'.
         (items (imenu--truncate-items
                 (save-excursion
                   (without-restriction
                     (funcall imenu-create-index-function)))))
         (config (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-config))))
    ;; Fix toplevel items, e.g., emacs-lisp-mode toplevel items are functions
    (when-let (toplevel (plist-get config :toplevel))
      (let ((tops (seq-remove (lambda (x) (listp (cdr x))) items))
            (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
        (setq items (nconc rest (and tops (list (cons toplevel tops)))))))
    ;; Apply our flattening in order to ease searching the imenu.
    (consult-imenu--flatten
     nil nil items
     (mapcar (pcase-lambda (`(,x ,y ,z)) (list y x z))
             (plist-get config :types)))))

(defun consult-imenu--deduplicate (items)
  "Deduplicate imenu ITEMS by appending a counter."
  ;; Some imenu backends generate duplicate items (e.g. for overloaded methods in java)
  (let ((ht (make-hash-table :test #'equal :size (length items))))
    (dolist (item items)
      (if-let (count (gethash (car item) ht))
          (setcar item (format "%s (%s)" (car item)
                               (puthash (car item) (1+ count) ht)))
        (puthash (car item) 0 ht)))))

(defun consult-imenu--items ()
  "Return cached imenu candidates, may error."
  (unless (equal (car consult-imenu--cache) (buffer-modified-tick))
    (setq consult-imenu--cache (cons (buffer-modified-tick) (consult-imenu--compute))))
  (cdr consult-imenu--cache))

(defun consult-imenu--items-safe ()
  "Return cached imenu candidates, will not error."
  (condition-case err
      (consult-imenu--items)
    (t (message "Cannot create Imenu for buffer %s (%s)"
                (buffer-name) (error-message-string err))
       nil)))

(defun consult-imenu--multi-items (buffers)
  "Return all imenu items from BUFFERS."
  (consult--with-increased-gc
   (let ((reporter (make-progress-reporter "Collecting" 0 (length buffers))))
     (prog1
         (apply #'append
                (seq-map-indexed (lambda (buf idx)
                                   (with-current-buffer buf
                                     (prog1 (consult-imenu--items-safe)
                                       (progress-reporter-update
                                        reporter (1+ idx) (buffer-name)))))
                                 buffers))
       (progress-reporter-done reporter)))))

(defun consult-imenu--jump (item)
  "Jump to imenu ITEM via `consult--jump'.
In contrast to the builtin `imenu' jump function,
this function can jump across buffers."
  (pcase item
    (`(,name ,pos ,fn . ,args)
     (push-mark nil t)
     (apply fn name pos args))
    (`(,_ . ,pos)
     (consult--jump pos))
    (_ (error "Unknown imenu item: %S" item)))
  (run-hooks 'imenu-after-jump-hook))

(defun consult-imenu--narrow ()
  "Return narrowing configuration for the current buffer."
  (mapcar (lambda (x) (cons (car x) (cadr x)))
          (plist-get (cdr (seq-find (lambda (x) (derived-mode-p (car x)))
                                    consult-imenu-config))
                     :types)))

(defun consult-imenu--group ()
  "Create a imenu group function for the current buffer."
  (when-let (narrow (consult-imenu--narrow))
    (lambda (cand transform)
      (let ((type (get-text-property 0 'consult--type cand)))
        (cond
         ((and transform type)
          (substring cand (1+ (next-single-property-change 0 'consult--type cand))))
         (transform cand)
         (type (alist-get type narrow)))))))

(defun consult-imenu--select (prompt items)
  "Select from imenu ITEMS given PROMPT string."
  (consult-imenu--deduplicate items)
  (consult-imenu--jump
   (consult--read
    (or items (user-error "Imenu is empty"))
    :state
    (let ((preview (consult--jump-preview)))
      (lambda (action cand)
        ;; Only preview simple menu items which are markers,
        ;; in order to avoid any bad side effects.
        (funcall preview action (and (markerp (cdr cand)) (cdr cand)))))
    :narrow
    (when-let (narrow (consult-imenu--narrow))
      (list :predicate
            (lambda (cand)
              (eq (get-text-property 0 'consult--type (car cand)) consult--narrow))
            :keys narrow))
    :group (consult-imenu--group)
    :prompt prompt
    :require-match t
    :category 'imenu
    :lookup #'consult--lookup-cons
    :history 'consult-imenu--history
    :add-history (thing-at-point 'symbol)
    :sort nil)))

;;;###autoload
(defun consult-imenu ()
  "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing.  See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'."
  (interactive)
  (consult-imenu--select
   "Go to item: "
   (consult--slow-operation "Building Imenu..."
     (consult-imenu--items))))

;;;###autoload
(defun consult-imenu-multi (&optional query)
  "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-function' is used.  Only the buffers with the
same major mode as the current buffer are used.  See also
`consult-imenu' for more details.  In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'."
  (interactive "P")
  (unless (keywordp (car-safe query))
    (setq query (list :sort 'alpha :mode major-mode
                      :directory (and (not query) 'project))))
  (let ((buffers (consult--buffer-query-prompt "Go to item" query)))
    (consult-imenu--select (car buffers)
                           (consult-imenu--multi-items (cdr buffers)))))

(provide 'consult-imenu)
;;; consult-imenu.el ends here
