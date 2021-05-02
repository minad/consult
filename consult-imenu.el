;;; consult-imenu.el --- Consult commands for imenu -*- lexical-binding: t -*-

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

For each type a narrowing key and a name must be specified. The face is
optional. The imenu representation provided by the backend usually puts
functions directly at the toplevel. `consult-imenu' moves them instead under the
type specified by :toplevel."
  :type '(repeat (cons symbol plist))
  :group 'consult)

(defface consult-imenu-prefix
  '((t :inherit consult-key))
  "Face used to highlight imenu prefix in `consult-imenu'."
  :group 'consult-faces)

(defvar consult-imenu--history nil)
(defvar-local consult-imenu--cache nil)

(defun consult-imenu--special (_name pos buf name fn &rest args)
  "Wrapper function for special imenu items.

POS is the position.
BUF is the buffer.
NAME is the item name.
FN is the original special item function.
ARGS are the arguments to the special item function."
  (funcall consult--buffer-display buf)
  (apply fn name pos args))

(defun consult-imenu--flatten (prefix face list types)
  "Flatten imenu LIST.

PREFIX is prepended in front of all items.
FACE is the item face.
TYPES is the mode-specific types configuration."
  (mapcan
   (lambda (item)
     (if (imenu--subalist-p item)
         (let ((name (car item))
               (next-prefix prefix)
               (next-face face))
           (if prefix
               (setq next-prefix (concat prefix "/" (propertize name 'face 'consult-imenu-prefix)))
             (if-let (type (cdr (assoc name types)))
                 (setq next-prefix (propertize name
                                               'face 'consult-imenu-prefix
                                               'consult--type (car type))
                       next-face (cadr type))
               (setq next-prefix (propertize name 'face 'consult-imenu-prefix))))
           (consult-imenu--flatten next-prefix next-face (cdr item) types))
       (let* ((name (car item))
              (key (if prefix (concat prefix " " (propertize name 'face face)) name))
              (payload (cdr item))
              marker)
         (setq marker (pcase payload
                       ;; Simple marker item
                       ((pred markerp) payload)
                       ;; Simple integer item
                       ((pred integerp) (copy-marker payload))
                       ;; Semantic uses overlay for positions
                       ((pred overlayp) (copy-marker (overlay-start payload)))
                       ;; Wrap special item
                       (`(,pos ,fn . ,args)
                        (nconc
                         (list pos #'consult-imenu--special (current-buffer) name fn)
                         args))
                       (_ (error "Unknown imenu item: %S" item))))
         (setq key (propertize key 'marker marker))
         (list (cons key
                     payload)))))
   list))

(defun consult-imenu--compute ()
  "Compute imenu candidates."
  (consult--forbid-minibuffer)
  (let* ((imenu-use-markers t)
         ;; Generate imenu, see `imenu--make-index-alist'.
         (items (imenu--truncate-items
	         (save-excursion
		   (save-restriction
		     (widen)
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

(defun consult-imenu--items ()
  "Return cached imenu candidates."
  (unless (equal (car consult-imenu--cache) (buffer-modified-tick))
    (setq consult-imenu--cache (cons (buffer-modified-tick) (consult-imenu--compute))))
  (cdr consult-imenu--cache))

(defun consult-imenu--all-items (buffers)
  "Return all imenu items from each BUFFERS."
  (seq-mapcat (lambda (buf) (with-current-buffer buf (consult-imenu--items))) buffers))

(defun consult-imenu--project-buffers ()
  "Return project buffers with the same `major-mode' as the current buffer."
  (if-let (root (consult--project-root))
      (seq-filter (lambda (buf)
                    (when-let (file (buffer-file-name buf))
                      (and (eq (buffer-local-value 'major-mode buf) major-mode)
                           (string-prefix-p root file))))
                  (buffer-list))
    (list (current-buffer))))

(defun consult-imenu--jump (item)
  "Jump to imenu ITEM via `consult--jump'.

In contrast to the builtin `imenu' jump function,
this function can jump across buffers."
  (pcase item
    (`(,name ,pos ,fn . ,args) (apply fn name pos args))
    (`(,_ . ,pos) (consult--jump pos))
    (_ (error "Unknown imenu item: %S" item))))

(defun consult-imenu--select (items)
  "Select from imenu ITEMS with preview.

The symbol at point is added to the future history."
  (let ((narrow
         (mapcar (lambda (x) (cons (car x) (cadr x)))
                 (plist-get (cdr (seq-find (lambda (x) (derived-mode-p (car x)))
                                           consult-imenu-config))
                            :types))))
    (consult-imenu--jump
     (consult--read
      (or items (user-error "Imenu is empty"))
      :prompt "Go to item: "
      :state
      (let ((preview (consult--jump-preview)))
        (lambda (cand restore)
          ;; Only preview simple menu items which are markers,
          ;; in order to avoid any bad side effects.
          (funcall preview (and (markerp (cdr cand)) (cdr cand)) restore)))
      :require-match t
      :title
      (when narrow
        (lambda (cand transform)
          (when-let (type (get-text-property 0 'consult--type cand))
            (if transform
                (substring cand (1+ (next-single-property-change 0 'consult--type cand)))
              (alist-get type narrow)))))
      :narrow
      (when narrow
        (cons
         (lambda (cand)
           (eq (get-text-property 0 'consult--type (car cand)) consult--narrow))
         narrow))
      :category 'imenu
      :lookup #'consult-imenu--lookup-cons
      :history 'consult-imenu--history
      :add-history (thing-at-point 'symbol)
      :sort nil))))

(defun consult-imenu--lookup-testfn (a b)
  (let ((am (get-text-property 0 'marker a))
        (bm (get-text-property 0 'marker b)))
    (and (equal a b)
         (equal am bm))))

(defun consult-imenu--lookup-cons (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return cons."
  (assoc cand candidates #'consult-imenu--lookup-testfn))


;;;###autoload
(defun consult-imenu ()
  "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-config', which configures the narrowing.

See also `consult-project-imenu'."
  (interactive)
  (consult-imenu--select (consult-imenu--items)))

;;;###autoload
(defun consult-project-imenu ()
  "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details."
  (interactive)
  (consult-imenu--select (consult-imenu--all-items (consult-imenu--project-buffers))))

(provide 'consult-imenu)
;;; consult-imenu.el ends here
