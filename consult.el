;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/minad/consult

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

;; Consult implements a set of `consult-<thing>' commands which use
;; `completing-read' to select from a list of candidates. Consult
;; provides an enhanced buffer switcher `consult-buffer' and many
;; search and navigation commands like `consult-line' and an
;; asynchronous `consult-grep'. Many commands support candidate
;; preview - if a candidate is selected in the completion view, the
;; buffer shows the candidate immediately.

;; The Consult commands are compatible with completion systems based
;; on the Emacs `completing-read' API, notably the default completion
;; system, Icomplete, Selectrum and Embark's live-occur.

;; This package took inspiration from Counsel by Oleh Krehel. Some of
;; the commands found in this package originated in the Selectrum
;; wiki. See the README for a full list of contributors.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'bookmark)
(require 'compile)
(require 'imenu)
(require 'kmacro)
(require 'outline)
(require 'recentf)
(require 'ring)
(require 'seq)

(defgroup consult nil
  "Consulting `completing-read'."
  :group 'convenience
  :prefix "consult-")

;;;; General customization

(defcustom consult-narrow-key nil
  "Prefix key for narrowing during completion."
  :type 'vector)

(defcustom consult-widen-key nil
  "Key used for widening during completion.
If this key is unset, defaults to 'consult-narrow-key SPC'."
  :type 'vector)

(defcustom consult-view-list-function nil
  "Function which returns a list of view names as strings, used by `consult-buffer'."
  :type 'function)

(defcustom consult-view-open-function nil
  "Function which opens a view, used by `consult-buffer'."
  :type 'function)

(defcustom consult-project-root-function nil
  "Function which returns project root, used by `consult-buffer' and `consult-grep'."
  :type 'function)

(defcustom consult-async-min-input 3
  "Minimum number of letters needed, before asynchronous process is called.
This applies for example to `consult-grep'."
  :type 'integer)

(defcustom consult-async-default-split "#"
  "Default async input separator used for splitting.
Can also be nil in order to disable it."
  :type 'string)

(defcustom consult-mode-histories
  '((eshell-mode . eshell-history-ring)
    (comint-mode . comint-input-ring)
    (term-mode   . term-input-ring))
  "Alist of (mode . history) pairs of mode histories.
The histories can be rings or lists."
  :type '(list (cons symbol symbol)))

(defcustom consult-themes nil
  "List of themes to be presented for selection.
nil shows all `custom-available-themes'."
  :type '(repeat symbol))

(defcustom consult-after-jump-hook '(recenter)
  "Function called after jumping to a location.

This is called during preview and for the jump after selection.
You may want to add a function which pulses the current line, e.g.,
`xref-pulse-momentarily'."
  :type 'hook)

(defcustom consult-line-point-placement 'match-beginning
  "Where to leave point after `consult-line' jumps to a match."
  :type '(choice (const :tag "Beginning of the line" line-beginning)
                 (const :tag "Beginning of the match" match-beginning)
                 (const :tag "End of the match" match-end)))

(defcustom consult-line-numbers-widen t
  "Show absolute line numbers when narrowing is active."
  :type 'boolean)

(defcustom consult-goto-line-numbers t
  "Show line numbers for `consult-goto-line'."
  :type 'boolean)

(defcustom consult-fontify-limit 1048576
  "Buffers larger than this limit are not fontified."
  :type 'integer)

(defcustom consult-imenu-narrow
  '((emacs-lisp-mode . ((?f . "Functions")
                        (?m . "Macros")
                        (?p . "Packages")
                        (?t . "Types")
                        (?v . "Variables"))))
  "Narrowing keys used by `consult-imenu'."
  :type 'alist)

(defcustom consult-imenu-toplevel
  '((emacs-lisp-mode . "Functions"))
  "Names of toplevel items, used by `consult-imenu'."
  :type 'alist)

(defcustom consult-mode-command-filter
  "-mode$\\|--"
  "Filter regexp for `consult-mode-command'."
  :type 'regexp)

;;;; Preview customization

(defgroup consult-preview nil
  "Preview settings of consult."
  :group 'consult)

(defcustom consult-preview-buffer t
  "Enable buffer preview during selection."
  :type 'boolean)

(defcustom consult-preview-imenu t
  "Enable imenu item preview during selection."
  :type 'boolean)

(defcustom consult-preview-theme t
  "Enable theme preview during selection."
  :type 'boolean)

(defcustom consult-preview-yank t
  "Enable yank preview during selection."
  :type 'boolean)

(defcustom consult-preview-global-mark t
  "Enable global mark preview during selection."
  :type 'boolean)

(defcustom consult-preview-mark t
  "Enable mark preview during selection."
  :type 'boolean)

(defcustom consult-preview-line t
  "Enable line preview during selection."
  :type 'boolean)

(defcustom consult-preview-error t
  "Enable error preview during selection."
  :type 'boolean)

(defcustom consult-preview-outline t
  "Enable outline preview during selection."
  :type 'boolean)

(defcustom consult-preview-grep t
  "Enable grep preview during selection."
  :type 'boolean)

(defcustom consult-preview-max-size 102400
  "Files larger than this limit are not previewed."
  :type 'integer)

(defcustom consult-preview-max-count 10
  "Number of files to keep open at once during preview."
  :type 'integer)

;;;###autoload
(define-minor-mode consult-preview-mode
  "Enable preview for consult commands."
  :global t)

;;;; Faces

(defgroup consult-faces nil
  "Faces used by Consult."
  :group 'consult
  :group 'faces)

(defface consult-preview-line
  '((t :inherit region))
  "Face used to for line previews.")

(defface consult-preview-match
  '((t :inherit match))
  "Face used to for match previews in `consult-grep'.")

(defface consult-preview-cursor
  '((t :inherit consult-preview-match))
  "Face used to for cursor previews and marks in `consult-mark'.")

(defface consult-preview-error
  '((t :inherit isearch-fail))
  "Face used to for cursor previews and marks in `consult-error'.")

(defface consult-preview-yank
  '((t :inherit consult-preview-line))
  "Face used to for yank previews in `consult-yank'.")

(defface consult-narrow-indicator
  '((t :inherit warning))
  "Face used for the narrowing indicator.")

(defface consult-async-indicator
  '((t :inherit consult-narrow-indicator))
  "Face used for the async indicator.")

(defface consult-key
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight keys, e.g., in `consult-register'.")

(defface consult-imenu-prefix
  '((t :inherit consult-key))
  "Face used to highlight imenu prefix in `consult-imenu'.")

(defface consult-line-number
  '((t :inherit consult-key))
  "Face used to highlight location line in `consult-global-mark'.")

(defface consult-file
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight files in `consult-buffer'.")

(defface consult-bookmark
  '((t :inherit font-lock-constant-face))
  "Face used to highlight bookmarks in `consult-buffer'.")

(defface consult-buffer
  '((t))
  "Face used to highlight buffers in `consult-buffer'.")

(defface consult-view
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight views in `consult-buffer'.")

(defface consult-line-number-prefix
  '((t :inherit line-number))
  "Face used to highlight line numbers in selections.")

;;;; History variables

(defvar consult--error-history nil)
(defvar consult--grep-history nil)
(defvar consult--find-history nil)
(defvar consult--line-history nil)
(defvar consult--apropos-history nil)
(defvar consult--theme-history nil)
(defvar consult--minor-mode-menu-history nil)
(defvar consult--mode-command-history nil)
(defvar consult--kmacro-history nil)
(defvar consult--buffer-history nil)
(defvar-local consult--imenu-history nil)

;;;; Internal variables

(defvar consult--completion-match-hook nil
  "Obtain match function from completion system.")

(defvar consult--completion-candidate-hook nil
  "Get candidate from completion system.")

(defvar consult--completion-refresh-hook nil
  "Refresh completion system.")

(defconst consult--special-char #x100000
  "Special character used to encode line prefixes for disambiguation.
We use the first character of the private unicode plane b.")

(defconst consult--special-range #xFFFE
  "Special character range.
Size of private unicode plane b.")

(defvar-local consult--narrow nil
  "Current narrowing key.")

(defvar-local consult--narrow-prefixes nil
  "Narrowing prefixes of the current completion.")

(defvar-local consult--narrow-predicate nil
  "Narrowing predicate of the current completion.")

(defvar-local consult--narrow-overlay nil
  "Narrowing indicator overlay.")

(defvar consult--gc-threshold 67108864
  "Large gc threshold for temporary increase.")

(defvar consult--gc-percentage 0.5
  "Large gc percentage for temporary increase.")

(defvar consult--async-stderr
  " *consult-async-stderr*"
  "Buffer for stderr output used by `consult--async-process'.")

;;;; Helper functions

(defun consult--format-directory-prompt (prompt dir)
  "Format PROMPT with directory DIR."
  (save-match-data
    (let ((edir (expand-file-name dir)))
      (cons
       (if (string= (expand-file-name default-directory) edir)
           (concat prompt ": ")
         (let ((adir (abbreviate-file-name edir)))
           (if (string-match "/\\([^/]+\\)/\\([^/]+\\)/$" adir)
               (format "%s in …/%s/%s/: " prompt (match-string 1 adir) (match-string 2 adir))
             (format "%s in %s: " prompt adir))))
       edir))))

(defun consult--directory-prompt (prompt dir)
  "Return prompt and directory.

PROMPT is the prompt prefix. The directory
is appended to the prompt prefix. For projects
only the project name is shown. The `default-directory'
is not shown. Other directories are abbreviated and
only the last two path components are shown.

If DIR is a string, it is returned.
If DIR is a true value, the user is asked.
Then the `consult-project-root-function' is tried.
Otherwise the `default-directory' is returned."
  (cond
   ((stringp dir) (consult--format-directory-prompt prompt dir))
   (dir (consult--format-directory-prompt prompt (read-directory-name "Directory: " nil nil t)))
   ((when-let (root (and consult-project-root-function
                         (funcall consult-project-root-function)))
      (save-match-data
        (if (string-match "/\\([^/]+\\)/$" root)
            (cons (format "%s in project %s: " prompt (match-string 1 root)) root)
          (consult--format-directory-prompt prompt root)))))
   (t (consult--format-directory-prompt prompt default-directory))))

(defsubst consult--strip-ansi-escape (str)
  "Strip ansi escape sequences from STR."
  (replace-regexp-in-string "\e\\[[0-9;]*[mK]" "" str))

(defsubst consult--format-location (file line)
  "Format location string 'FILE:LINE:'."
  (concat
   (propertize file 'face 'consult-file) ":"
   (propertize (number-to-string line) 'face 'consult-line-number) ":"))

(defun consult--line-position (line)
  "Compute position from LINE number."
  (save-excursion
    (save-restriction
      (when consult-line-numbers-widen
        (widen))
      (goto-char (point-min))
      (forward-line (- line 1))
      (point))))

(defmacro consult--overlay (beg end &rest props)
  "Make consult overlay between BEG and END with PROPS."
  (let ((ov (make-symbol "ov"))
        (puts))
    (while props
      (push `(overlay-put ,ov ,(car props) ,(cadr props)) puts)
      (setq props (cddr props)))
    `(let ((,ov (make-overlay ,beg ,end)))
       ,@puts
       ,ov)))

(defun consult--remove-dups (list &optional key)
  "Remove duplicate strings from LIST. Keep first occurrence of a key.
KEY is the key function."
  (let ((ht (make-hash-table :test #'equal :size (length list)))
        (accum)
        (key (or key #'identity)))
    (dolist (entry list (nreverse accum))
      (let ((k (funcall key entry)))
        (unless (gethash k ht)
          (puthash k t ht)
          (push entry accum))))))

(defsubst consult--in-range-p (pos)
  "Return t if position POS lies in range `point-min' to `point-max'."
  (and (>= pos (point-min)) (<= pos (point-max))))

(defun consult--lookup-cdr (_ candidates cand)
  "Lookup CAND in CANDIDATES."
  (cdr (assoc cand candidates)))

(defun consult--lookup-cadr (_ candidates cand)
  "Lookup CAND in CANDIDATES."
  (cadr (assoc cand candidates)))

(defun consult--forbid-minibuffer ()
  "Raise an error if executed from the minibuffer."
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer")))

(defun consult--fontify-all ()
  "Ensure that the whole buffer is fontified."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line. Therefore we have to enforce
  ;; font-locking now, which is slow. In order to prevent is hang-up we check the buffer size
  ;; against `consult-fontify-limit'.
  (when (and jit-lock-mode (< (buffer-size) consult-fontify-limit))
    (jit-lock-fontify-now)))

(defsubst consult--fontify-region (start end)
  "Ensure that region between START and END is fontified."
  (when jit-lock-mode
    (jit-lock-fontify-now start end)))

;; We must disambiguate the lines by adding a prefix such that two lines with the same text can be
;; distinguished. In order to avoid matching the line number, such that the user can search for
;; numbers with `consult-line', we encode the line number as unicode characters in the supplementary
;; private use plane b. By doing that, it is unlikely that accidential matching occurs.
(defsubst consult--unique (pos display)
  "Generate unique string for POS.
DISPLAY is the string to display instead of the unique string."
  (let ((str "") (n pos))
    (while (progn
             (setq str (concat (string (+ consult--special-char
                                          (% n consult--special-range)))
                               str))
             (and (>= n consult--special-range) (setq n (/ n consult--special-range)))))
    (propertize str 'display display)))

(defun consult--with-preview-1 (transform preview fun)
  "Install TRANSFORM and PREVIEW function for FUN."
  (if (or (not consult-preview-mode) (not preview))
      (let ((input ""))
        (minibuffer-with-setup-hook
            (apply-partially
             #'add-hook 'post-command-hook
             (lambda () (setq input (minibuffer-contents-no-properties)))
             nil t)
          (cons (when-let (result (funcall fun))
                  (funcall transform input result))
                input)))
    (let ((orig-window (selected-window))
          (selected)
          (input ""))
      (minibuffer-with-setup-hook
          (apply-partially
           #'add-hook 'post-command-hook
           (lambda ()
             (setq input (minibuffer-contents-no-properties))
             (when-let (cand (run-hook-with-args-until-success 'consult--completion-candidate-hook))
               (with-selected-window (if (window-live-p orig-window)
                                         orig-window
                                       (selected-window))
                 (funcall preview (and cand (funcall transform input cand)) nil))))
           nil t)
        (unwind-protect
            (save-excursion
              (save-restriction
                (setq selected (when-let (result (funcall fun))
                                 (funcall transform input result)))
                (cons selected input)))
          (funcall preview selected t))))))

(defmacro consult--with-preview (transform preview &rest body)
  "Install TRANSFORM and PREVIEW in BODY."
  (declare (indent 2))
  `(consult--with-preview-1 ,transform ,preview (lambda () ,@body)))

(defun consult--widen-key ()
  "Return widening key, if `consult-widen-key' is not set, default to 'consult-narrow-key SPC'."
  (or consult-widen-key (and consult-narrow-key (vconcat consult-narrow-key " "))))

(defun consult-narrow (key)
  "Narrow current completion with KEY."
  (interactive
   (list (unless (equal (this-single-command-keys) (consult--widen-key))
           last-command-event)))
  (unless (minibufferp) (error "Command must be executed in minibuffer"))
  (setq consult--narrow key)
  (when consult--narrow-predicate
    (setq minibuffer-completion-predicate (and consult--narrow consult--narrow-predicate)))
  (when consult--narrow-overlay
    (delete-overlay consult--narrow-overlay))
  (when consult--narrow
    (setq consult--narrow-overlay
          (consult--overlay (- (minibuffer-prompt-end) 1) (minibuffer-prompt-end)
                            'before-string
                            (propertize (format " [%s]" (cdr (assoc key consult--narrow-prefixes)))
                                        'face 'consult-narrow-indicator))))
  (run-hooks 'consult--completion-refresh-hook))

(defconst consult--narrow-delete
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (when (string= (minibuffer-contents-no-properties) "")
         (consult-narrow nil)
         #'ignore))))

(defconst consult--narrow-space
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (let ((str (minibuffer-contents-no-properties)))
         (when-let (pair (or (and (= 1 (length str)) (assoc (elt str 0) consult--narrow-prefixes))
                             (and (string= str "") (assoc 32 consult--narrow-prefixes))))
           (delete-minibuffer-contents)
           (consult-narrow (car pair))
           #'ignore)))))

(defun consult-narrow-help ()
  "Print narrowing help as `minibuffer-message'.
This command can be bound in `consult-narrow-map'."
  (interactive)
  (minibuffer-message
   (string-join
    (thread-last consult--narrow-prefixes
      (seq-filter (lambda (x) (/= (car x) 32)))
      (mapcar (lambda (x) (format "%c %s" (car x) (cdr x)))))
    " ")))

(defvar consult-narrow-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " consult--narrow-space)
    (define-key map [127] consult--narrow-delete)
    map)
  "Narrowing keymap which is added to the local minibuffer map.
Note that `consult-narrow-key' and `consult-widen-key' are bound dynamically.")

(defun consult--define-key (map key cmd desc)
  "Bind CMD to KEY in MAP and add which-key description DESC."
  (define-key map key cmd)
  ;; The which-key description is potentially fragile if something is changed on the side
  ;; of which-key. Keep an eye on that. An alternative more standard-compliant method
  ;; would be to use `menu-item', but this is unfortunately not yet supported by which-key
  ;; and `describe-buffer-bindings'.
  ;; See https://github.com/justbur/emacs-which-key/issues/177
  (let ((idx (- (length key) 1)))
    (define-key map (vconcat (seq-take key idx) (vector 'which-key (elt key idx)))
      `(which-key (,desc)))))

(defun consult--narrow-setup (settings)
  "Setup narrowing with SETTINGS."
  (if (functionp (car settings))
      (setq consult--narrow-predicate (car settings)
            consult--narrow-prefixes (cdr settings))
    (setq consult--narrow-predicate nil
          consult--narrow-prefixes settings))
  (let ((map (make-composed-keymap consult-narrow-map (current-local-map))))
    (when consult-narrow-key
      (dolist (pair consult--narrow-prefixes)
        (when (/= (car pair) 32)
          (consult--define-key map
                               (vconcat consult-narrow-key (vector (car pair)))
                               #'consult-narrow (cdr pair)))))
    (when-let (widen (consult--widen-key))
      (consult--define-key map widen #'consult-narrow "All"))
    (use-local-map map)))

(defmacro consult--with-increased-gc (&rest body)
  "Temporarily increase the gc limit in BODY to optimize for throughput."
  `(let* ((overwrite (> consult--gc-threshold gc-cons-threshold))
          (gc-cons-threshold (if overwrite consult--gc-threshold gc-cons-threshold))
          (gc-cons-percentage (if overwrite consult--gc-percentage gc-cons-percentage)))
     ,@body))

;; Derived from ctrlf, originally isearch
(defun consult--invisible-show (&optional permanently)
  "Disable any overlays that are currently hiding point.
PERMANENTLY non-nil means the overlays will not be restored later."
  (let ((opened))
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)) opened)
      (when (and (invisible-p (overlay-get ov 'invisible))
                 (overlay-get ov 'isearch-open-invisible))
        (if permanently
            (funcall (overlay-get ov 'isearch-open-invisible) ov)
          (push (cons ov (overlay-get ov 'invisible)) opened)
          (if-let ((func (overlay-get ov 'isearch-open-invisible-temporary)))
              (funcall func nil)
            (overlay-put ov 'invisible nil)))))))

;; Derived from ctrlf, originally isearch
(defun consult--invisible-restore (overlays)
  "Restore any opened OVERLAYS that were previously disabled."
  (dolist (ov overlays)
    (if-let ((func (overlay-get (car ov) 'isearch-open-invisible-temporary)))
        (funcall func t)
      (overlay-put (car ov) 'invisible (cdr ov)))))

(defun consult--jump-1 (pos)
  "Go to POS and recenter."
  (cond
   ((and (markerp pos) (not (buffer-live-p (marker-buffer pos))))
    ;; Only print a message, no error in order to not mess
    ;; with the minibuffer update hook.
    (message "Buffer is dead"))
   (t
    ;; Switch to buffer if it is not visible
    (when (and (markerp pos) (not (eq (current-buffer) (marker-buffer pos))))
      (switch-to-buffer (marker-buffer pos)))
    ;; Widen if we cannot jump to the position (idea from flycheck-jump-to-error)
    (unless (= (goto-char pos) (point))
      (widen)
      (goto-char pos))
    (run-hooks 'consult-after-jump-hook))))

(defun consult--jump (pos)
  "Push current position to mark ring, go to POS and recenter."
  (when pos
    ;; When the marker is in the same buffer,
    ;; record previous location such that the user can jump back quickly.
    (unless (and (markerp pos) (not (eq (current-buffer) (marker-buffer pos))))
      (push-mark (point) t))
    (consult--jump-1 pos)
    (consult--invisible-show t))
  nil)

;; Matched strings are not highlighted as of now.
;; see https://github.com/minad/consult/issues/7
(defun consult--preview-position (&optional face)
  "The preview function used if selecting from a list of candidate positions.
The function can be used as the `:preview' argument of `consult--read'.
FACE is the cursor face."
  (let ((overlays)
        (invisible)
        (face (or face 'consult-preview-cursor))
        (saved-buf (current-buffer)))
    (lambda (cand restore)
      (cond
       (restore
        (consult--invisible-restore invisible)
        (mapc #'delete-overlay overlays)
        (when (buffer-live-p saved-buf)
          (set-buffer saved-buf)))
       (cand
        (consult--jump-1 cand)
        (consult--invisible-restore invisible)
        (setq invisible (consult--invisible-show))
        (mapc #'delete-overlay overlays)
        (let ((pos (point)))
          (setq overlays
                (list (consult--overlay (line-beginning-position) (line-end-position) 'face 'consult-preview-line)
                      (consult--overlay pos (1+ pos) 'face face)))))))))

(defun consult--kill-clean-buffer (buf)
  "Kill BUF if it has not been modified."
  (unless (or (eq buf (current-buffer)) (buffer-modified-p buf))
    (kill-buffer buf)))

(defun consult--with-preview-files-1 (fun)
  "Provide a function to open files temporarily.
The files are closed automatically in the end.

FUN receives the open function as argument."
  (let* ((new-buffers)
         (old-recentf-list (copy-sequence recentf-list))
         (open-file
          (lambda (name)
            (or (get-file-buffer name)
                (when-let (attrs (file-attributes name))
                  (when (<= (file-attribute-size attrs) consult-preview-max-size)
                    (let ((buf (find-file-noselect name 'nowarn)))
                      (push buf new-buffers)
                      ;; Only keep a few buffers alive
                      (while (> (length new-buffers) consult-preview-max-count)
                        (consult--kill-clean-buffer (car (last new-buffers)))
                        (setq new-buffers (nbutlast new-buffers)))
                      buf)))))))
    (unwind-protect
        (funcall fun open-file)
      ;; Kill all temporary buffers
      (mapc #'consult--kill-clean-buffer new-buffers)
      ;; Restore old recentf-list and record the current buffer
      (setq recentf-list old-recentf-list)
      (when (member (current-buffer) new-buffers)
        (recentf-add-file (buffer-file-name (current-buffer)))))))

(defmacro consult--with-preview-files (args &rest body)
  "Provide a function to open files temporarily.
The files are closed automatically in the end.

ARGS is the open function argument for BODY."
  (declare (indent 1))
  `(consult--with-preview-files-1 (lambda ,args ,@body)))

(defun consult--with-async-1 (async fun)
  "Setup ASYNC for FUN."
  (if (not (functionp async)) (funcall fun (lambda (_) async))
    (minibuffer-with-setup-hook
        (lambda ()
          (funcall async 'setup)
          (add-hook 'post-command-hook
                    (lambda ()
                      ;; push input string to request refresh
                      (funcall async (minibuffer-contents-no-properties)))
                    nil t))
      (unwind-protect
          (funcall fun async)
        (funcall async 'destroy)))))

(defmacro consult--with-async (async &rest body)
  "Setup ASYNC for BODY."
  (declare (indent 1))
  `(consult--with-async-1 ,@(cdr async) (lambda (,(car async)) ,@body)))

(defun consult--add-history (items)
  "Add ITEMS to the minibuffer history via `minibuffer-default-add-function'."
  (when (setq items (delq nil items))
    (setq-local minibuffer-default-add-function
                (if-let (orig minibuffer-default-add-function)
                    (lambda ()
                      ;; the minibuffer-default-add-function may want generate more items
                      (setq-local minibuffer-default-add-function orig)
                      (consult--remove-dups (append items (funcall orig))))
                  (lambda () items)))))

(cl-defun consult--read (prompt candidates &key
                                predicate require-match history default
                                category initial preview narrow add-history
                                (sort t) (default-top t) (lookup (lambda (_input _cands x) x)))
  "Simplified completing read function.

PROMPT is the string to prompt with.
CANDIDATES is the candidate list or alist.
PREDICATE is a filter function for the candidates.
REQUIRE-MATCH equals t means that an exact match is required.
HISTORY is the symbol of the history variable.
DEFAULT is the default selected value.
ADD-HISTORY is a list of items to add to the history.
CATEGORY is the completion category.
SORT should be set to nil if the candidates are already sorted.
LOOKUP is a function which is applied to the result.
INITIAL is initial input.
DEFAULT-TOP must be nil if the default candidate should not be moved to the top.
PREVIEW is a preview function.
NARROW is an alist of narrowing prefix strings and description."
  (ignore default-top)
  ;; supported types
  (cl-assert (or (functionp candidates)     ;; async table
                 (not candidates)           ;; nil, empty list
                 (obarrayp candidates)      ;; obarray
                 (stringp (car candidates)) ;; string list
                 (symbolp (car candidates)) ;; symbol list
                 (consp (car candidates)))) ;; alist
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
         (consult--add-history (cons default (if (stringp add-history)
                                                 (list add-history)
                                               add-history)))
         (when narrow (consult--narrow-setup narrow))))
    (consult--with-async (async candidates)
      (let* ((metadata
              `(metadata
                ,@(when category `((category . ,category)))
                ,@(unless sort '((cycle-sort-function . identity)
                                 (display-sort-function . identity)))))
             (table
              (lambda (str pred action)
                (if (eq action 'metadata)
                    metadata
                  (complete-with-action action (funcall async 'get) str pred))))
             (transform
              (lambda (input cand)
                (funcall lookup input (funcall async 'get) cand)))
             (result
              (consult--with-preview transform preview
                (completing-read prompt table
                                 predicate require-match initial
                                 (if (symbolp history) history (cadr history))
                                 default))))
        (pcase-exhaustive history
          (`(:input ,var)
           (set var (cdr (symbol-value var)))
           (add-to-history var (cdr result)))
          ((pred symbolp)))
        (car result)))))

(defun consult--count-lines (pos)
  "Move to position POS and return number of lines."
  (let ((line 0))
    (while (< (point) pos)
      (forward-line 1)
      (when (<= (point) pos)
        (setq line (1+ line))))
    (goto-char pos)
    line))

(defsubst consult--line-number-prefix (width line)
  "Optimized formatting for LINE number with padding. WIDTH is the line number width."
  (setq line (number-to-string line))
  (propertize (concat
               (make-string (- width (length line)) 32)
               line
               " ")
              'face 'consult-line-number-prefix))

(defun consult--add-line-number (max-line candidates)
  "Add line numbers to unformatted CANDIDATES as prefix.
The MAX-LINE is needed to determine the width.
Since the line number is part of the candidate it will be matched-on during completion."
  (let ((width (length (number-to-string max-line))))
    (dolist (cand candidates candidates)
      (setcar cand
              (concat
               (consult--unique (cdr cand) (consult--line-number-prefix width (caar cand)))
               (cdar cand))))))

(defsubst consult--region-with-cursor (begin end marker)
  "Return region string with a marking at the cursor position.

BEGIN is the begin position.
END is the end position.
MARKER is the cursor position."
  (let ((marker-end (1+ marker)))
    (if (> marker-end end)
        (concat (buffer-substring begin marker)
                (propertize " " 'face 'consult-preview-cursor))
      (concat (buffer-substring begin marker)
              (propertize (buffer-substring marker marker-end)
                          'face 'consult-preview-cursor)
              (buffer-substring marker-end end)))))

(defsubst consult--line-with-cursor (line marker)
  "Return line candidate.

LINE is line number.
MARKER is the cursor marker."
  (cons (cons line (consult--region-with-cursor
                    (line-beginning-position)
                    (line-end-position)
                    marker))
        marker))

;;;; Async functions

(defun consult--async-sink ()
  "Create ASYNC sink function.

The async function should accept a single action argument.
Only for the 'setup action, it is guaranteed that the call
originates from the minibuffer. For the other actions no
assumptions can be made.
Depending on the argument, the caller context differ.

'setup   Setup the internal state.
'destroy Destroy the internal state.
'flush   Flush the list of candidates.
'refresh Request UI refresh.
'get     Get the list of candidates.
List     Append the list to the list of candidates.
String   The input string, called when the user enters something."
  (let ((candidates))
    (lambda (action)
      (pcase-exhaustive action
        ((or (pred stringp) 'setup 'destroy) nil)
        ('flush (setq candidates nil))
        ('get candidates)
        ('refresh
         (when-let (win (active-minibuffer-window))
           (with-selected-window win
             (run-hooks 'consult--completion-refresh-hook))))
        ((pred listp) (setq candidates (nconc candidates action)))))))

(defun consult--async-split-string (str)
  "Split STR in async input and filtering part.
If the first character is a punctuation character it determines
the separator. Examples: \"/async/filter\", \"#async#filter\"."
  (if (string-match-p "^[[:punct:]]" str)
      (save-match-data
        (let ((q (regexp-quote (substring str 0 1))))
          (string-match (concat "^" q "\\([^" q "]*\\)" q "?") str)
          (cons (match-string 1 str) (match-end 0))))
    (cons str (length str))))

(defun consult--async-split-wrap (fun)
  "Wrap completion style function FUN for `consult--async-split'."
  (lambda (str table pred point &optional metadata)
    (let ((completion-styles (cdr completion-styles))
          (pos (cdr (consult--async-split-string str))))
      (funcall fun
               (substring str pos)
               table pred
               (- point pos)
               metadata))))

(add-to-list 'completion-styles-alist
             (list 'consult--async-split
                   (consult--async-split-wrap #'completion-try-completion)
                   (consult--async-split-wrap #'completion-all-completions)
                   "Split async and filter part."))

(defun consult--async-split-setup ()
  "Setup `consult--async-split' completion styles."
  (setq-local completion-styles (cons 'consult--async-split completion-styles)))

(defun consult--async-split (async)
  "Create async function, which splits the input string.

The input string is split at the first comma. The part before
the comma is passed to ASYNC, the second part is used for filtering."
  (lambda (action)
    (pcase action
      ('setup
       (consult--async-split-setup)
       (funcall async 'setup))
      ((pred stringp)
       (let* ((pair (consult--async-split-string action))
              (len (length (car pair))))
         (when (or (>= (length action) (+ 2 len))
                   (>= len consult-async-min-input))
           (funcall async (car pair)))))
      (_ (funcall async action)))))

(defun consult--async-process (async cmd)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
CMD is the command argument list."
  (let* ((rest) (proc) (flush) (last-args) (indicator))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (let ((args (funcall cmd action)))
           (unless (equal args last-args)
             (setq last-args args)
             (ignore-errors (kill-process proc))
             (when args
               (overlay-put indicator 'display (propertize "*" 'face 'consult-async-indicator))
               (with-current-buffer (get-buffer-create consult--async-stderr)
                 (goto-char (point-max))
                 (insert (format "consult--async-process: %S\n" args)))
               (setq
                rest ""
                flush t
                proc
                (make-process
                 :name (car args)
                 :stderr consult--async-stderr
                 :noquery t
                 :command args
                 :filter
                 (lambda (_ out)
                   (when flush
                     (setq flush nil)
                     (funcall async 'flush))
                   (let ((lines (split-string out "\n")))
                     (if (cdr lines)
                         (progn
                           (setcar lines (concat rest (car lines)))
                           (setq rest (car (last lines)))
                           (funcall async (nbutlast lines)))
                       (setq rest (concat rest (car lines))))))
                 :sentinel
                 (lambda (_ event)
                   (with-current-buffer (get-buffer-create consult--async-stderr)
                     (goto-char (point-max))
                     (insert (format "consult--async-process sentinel: %s\n" event)))
                   (when flush
                     (setq flush nil)
                     (funcall async 'flush))
                   (when (string-match-p "finished\\|exited\\|failed" event)
                     (overlay-put indicator 'display nil))
                   (when (string-prefix-p "finished" event)
                     (unless (string= rest "")
                       (funcall async (list rest)))))))))))
        ('destroy
         (ignore-errors (kill-process proc))
         (delete-overlay indicator)
         (funcall async 'destroy))
        ('setup
         (setq indicator (make-overlay (- (minibuffer-prompt-end) 2) (- (minibuffer-prompt-end) 1)))
         (funcall async 'setup))
        (_ (funcall async action))))))

(defun consult--async-throttle (async &optional delay)
  "Create async function from ASYNC which limits the input rate by DELAY."
  (let ((delay (or delay 0.5)) (input "") (timer))
    (lambda (action)
      (pcase action
        ('setup
         (funcall async 'setup)
         (setq timer (run-at-time delay delay
                                  (lambda ()
                                    (unless (string= input "")
                                      (funcall async input))))))
        ((pred stringp) (setq input action))
        ('destroy (cancel-timer timer)
                  (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--async-refresh-immediate (async)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens immediately when candidates are pushed."
  (lambda (action)
    (pcase action
      ((or (pred listp) (pred stringp) 'flush)
       (prog1 (funcall async action)
         (funcall async 'refresh)))
      (_ (funcall async action)))))

(defun consult--async-refresh-timer (async &optional delay)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens after a DELAY, defaulting to 0.2."
  (let ((timer) (refresh t) (delay (or delay 0.2)))
    (lambda (action)
      (pcase action
        ((or (pred listp) (pred stringp) 'refresh 'flush)
         (setq refresh t))
        ('destroy (cancel-timer timer))
        ('setup
         (setq timer (run-with-timer
                      delay delay
                      (lambda ()
                        (when refresh
                          (setq refresh nil)
                          (funcall async 'refresh)))))))
      (funcall async action))))

(defmacro consult--async-transform (async &rest transform)
  "Use FUN to TRANSFORM candidates of ASYNC."
  (let ((async-var (make-symbol "async"))
        (action-var (make-symbol "action")))
    `(let ((,async-var ,async))
       (lambda (,action-var)
         (funcall ,async-var (if (listp ,action-var) (,@transform ,action-var) ,action-var))))))

(defun consult--async-map (async fun)
  "Map candidates of ASYNC by FUN."
  (consult--async-transform async mapcar fun))

(defun consult--async-filter (async fun)
  "Filter candidates of ASYNC by FUN."
  (consult--async-transform async seq-filter fun))

;;;; Commands

;; see https://github.com/raxod502/selectrum/issues/226
;;;###autoload
(defun consult-multi-occur (bufs regexp &optional nlines)
  "Improved version of `multi-occur' based on `completing-read-multiple'.
See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES."
  (interactive (cons
                (mapcar #'get-buffer
                        (completing-read-multiple "Buffer: "
                                                  #'internal-complete-buffer))
                (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun consult--outline-candidates ()
  "Return alist of outline headings and positions."
  (consult--forbid-minibuffer)
  (consult--fontify-all)
  (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (heading-regexp (concat "^\\(?:" outline-regexp "\\)"))
         (candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil t))
        (setq line (+ line (consult--count-lines (match-beginning 0))))
        (push (cons
               (cons line (buffer-substring (line-beginning-position) (line-end-position)))
               (point-marker))
              candidates)
        (unless (eobp) (forward-char 1))))
    (unless candidates
      (user-error "No headings"))
    (consult--add-line-number line (nreverse candidates))))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading."
  (interactive)
  (consult--jump
   (consult--read
    "Go to heading: "
    (consult--with-increased-gc (consult--outline-candidates))
    :category 'line
    :sort nil
    :require-match t
    :lookup #'consult--line-match
    :history '(:input consult--line-history)
    :add-history (thing-at-point 'symbol)
    :preview (and consult-preview-outline (consult--preview-position)))))

(defun consult--font-lock (str)
  "Apply `font-lock' faces in STR."
  (let ((pos 0) (len (length str)))
    (while (< pos len)
      (let* ((face (get-text-property pos 'font-lock-face str))
             (end (or (text-property-not-all pos len 'font-lock-face face str) len)))
        (put-text-property pos end 'face face str)
        (setq pos end)))
    str))

(defun consult--error-candidates ()
  "Return alist of errors and positions."
  (let ((candidates)
        (pos (point-min)))
    (save-excursion
      (while (setq pos (compilation-next-single-property-change pos 'compilation-message))
        (when-let* ((msg (get-text-property pos 'compilation-message))
                    (loc (compilation--message->loc msg)))
          (goto-char pos)
          (push (list
                 (consult--font-lock (buffer-substring pos (line-end-position)))
                 (with-current-buffer
                     ;; taken from compile.el
                     (apply #'compilation-find-file
                            (point-marker)
                            (caar (compilation--loc->file-struct loc))
                            (cadar (compilation--loc->file-struct loc))
                            (compilation--file-struct->formats
                             (compilation--loc->file-struct loc)))
                   (goto-char (point-min))
                   ;; location might be invalid by now
                   (ignore-errors
                     (forward-line (- (compilation--loc->line loc) 1))
                     (forward-char (compilation--loc->col loc)))
                   (point-marker))
                 (pcase (compilation--message->type msg)
                   (0 ?i)
                   (1 ?w)
                   (_ ?e)))
                candidates))))
    (nreverse candidates)))

;;;###autoload
(defun consult-error ()
  "Jump to an error in the current buffer."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (user-error "Not a compilation buffer"))
  (consult--jump
   (consult--read
    "Go to error: "
    (consult--with-increased-gc (consult--error-candidates))
    :category 'compilation-error
    :sort nil
    :require-match t
    :lookup #'consult--lookup-cadr
    :narrow '((lambda (cand) (= (caddr cand) consult--narrow))
              (?e . "Error")
              (?w . "Warning")
              (?i . "Info"))
    :history '(:input consult--error-history)
    :preview
    (and consult-preview-error (consult--preview-position 'consult-preview-error)))))

(defun consult--mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (consult--forbid-minibuffer)
  (unless (marker-position (mark-marker))
    (user-error "No marks"))
  (consult--fontify-all)
  (let* ((max-line 0)
         (candidates))
    (save-excursion
      (dolist (marker (cons (mark-marker) mark-ring))
        (let ((pos (marker-position marker)))
          (when (consult--in-range-p pos)
            (goto-char pos)
            ;; `line-number-at-pos' is a very slow function, which should be replaced everywhere.
            ;; However in this case the slow line-number-at-pos does not hurt much, since
            ;; the mark ring is usually small since it is limited by `mark-ring-max'.
            (let ((line (line-number-at-pos pos consult-line-numbers-widen)))
              (setq max-line (max line max-line))
              (push (consult--line-with-cursor line marker) candidates))))))
    (nreverse (consult--remove-dups (consult--add-line-number max-line candidates) #'car))))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in `mark-ring'."
  (interactive)
  (consult--jump
   (consult--read
    "Go to mark: "
    (consult--with-increased-gc (consult--mark-candidates))
    :category 'line
    :sort nil
    :require-match t
    :lookup #'consult--lookup-cdr
    :history '(:input consult--line-history)
    :preview (and consult-preview-mark (consult--preview-position)))))

(defun consult--global-mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (consult--forbid-minibuffer)
  (let ((candidates))
    (save-excursion
      (dolist (marker global-mark-ring)
        (let ((pos (marker-position marker))
              (buf (marker-buffer marker)))
          (when (and pos buf (buffer-live-p buf) (not (minibufferp buf)))
            (with-current-buffer buf
              (when (consult--in-range-p pos)
                (goto-char pos)
                ;; `line-number-at-pos' is slow, see comment in `consult--mark-candidates'.
                (let* ((line (line-number-at-pos pos consult-line-numbers-widen))
                       (begin (line-beginning-position))
                       (end (line-end-position))
                       (loc (consult--format-location (buffer-name buf) line)))
                  (consult--fontify-region begin end)
                  (push (cons (concat (consult--unique marker "")
                                      loc
                                      (consult--region-with-cursor begin end marker))
                              marker)
                        candidates))))))))
    (unless candidates
      (user-error "No global marks"))
    (nreverse (consult--remove-dups candidates #'car))))

;;;###autoload
(defun consult-global-mark ()
  "Jump to a marker in `global-mark-ring'."
  (interactive)
  (consult--jump
   (consult--read
    "Go to global mark: "
    (consult--with-increased-gc (consult--global-mark-candidates))
    :category 'line
    :sort nil
    :require-match t
    :lookup #'consult--lookup-cdr
    :history '(:input consult--line-history)
    :preview (and consult-preview-global-mark (consult--preview-position)))))

(defun consult--line-candidates ()
  "Return alist of lines and positions."
  (consult--forbid-minibuffer)
  (consult--fontify-all)
  (let* ((default-cand)
         (candidates)
         (pos (point-min))
         (max (point-max))
         (line (line-number-at-pos pos consult-line-numbers-widen))
         (curr-line (line-number-at-pos (point) consult-line-numbers-widen))
         (line-width (length (number-to-string (line-number-at-pos max consult-line-numbers-widen))))
         (default-cand-dist most-positive-fixnum))
    (save-excursion
      (goto-char pos)
      (while (< pos max)
        (let* ((end (line-end-position))
               (str (buffer-substring pos end)))
          (unless (string-blank-p str)
            (let ((cand (concat
                         (consult--unique line (consult--line-number-prefix line-width line))
                         str))
                  (dist (abs (- curr-line line))))
              (when (< dist default-cand-dist)
                (setq default-cand cand
                      default-cand-dist dist))
              (push (cons cand (point-marker)) candidates)))
          (setq line (1+ line)
                pos (1+ end))
          (goto-char pos))))
    (unless candidates
      (user-error "No lines"))
    (cons default-cand (nreverse candidates))))

(defun consult--line-match (input candidates cand)
  "Lookup position of match.

INPUT is the input string entered by the user.
CANDIDATES is the line candidates alist.
CAND is the currently selected candidate."
  (when-let (pos (cdr (assoc cand candidates)))
    (if (or (string-blank-p input)
            (eq consult-line-point-placement 'line-beginning))
        pos
      ;; Strip unique line number prefix
      (while (and (> (length cand) 0)
                  (>= (elt cand 0) consult--special-char)
                  (< (elt cand 0) (+ consult--special-char consult--special-range)))
        (setq cand (substring cand 1)))
      (let ((beg 0)
            (end (length cand))
            (match (run-hook-with-args-until-success 'consult--completion-match-hook)))
        ;; Find match end position, remove characters from line end until matching fails
        (let ((step 16))
          (while (> step 0)
            (while (and (> (- end step) 0)
                        (funcall match input (list (substring cand 0 (- end step)))))
              (setq end (- end step)))
            (setq step (/ step 2))))
        ;; Find match beginning position, remove characters from line beginning until matching fails
        (when (eq consult-line-point-placement 'match-beginning)
          (let ((step 16))
            (while (> step 0)
              (while (and (< (+ beg step) end)
                          (funcall match input (list (substring cand (+ beg step) end))))
                (setq beg (+ beg step)))
              (setq step (/ step 2)))
            (setq end beg)))
        ;; Marker can be dead
        (ignore-errors (+ pos end))))))

;;;###autoload
(defun consult-line (&optional initial)
  "Search for a matching line and jump to the line beginning.
The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optionally INITIAL input can be provided."
  (interactive)
  (let ((candidates (consult--with-increased-gc (consult--line-candidates))))
    (consult--jump
     (consult--read
      "Go to line: " (cdr candidates)
      :category 'line
      :sort nil
      :default-top nil
      :require-match t
      :add-history (list
                    (thing-at-point 'symbol)
                    (when isearch-string
                      (if isearch-regexp
                          isearch-string
                        (regexp-quote isearch-string))))
      :history '(:input consult--line-history)
      :lookup #'consult--line-match
      :default (car candidates)
      :initial initial
      :preview (and consult-preview-line (consult--preview-position))))))

;;;###autoload
(defun consult-goto-line ()
  "Read line number and jump to the line with preview.

Respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'."
  (interactive)
  (consult--forbid-minibuffer)
  (let ((display-line-numbers consult-goto-line-numbers)
        (display-line-numbers-widen consult-line-numbers-widen))
    (while (let ((ret (minibuffer-with-setup-hook
                          (lambda ()
                            (setq-local consult--completion-candidate-hook
                                        '(minibuffer-contents-no-properties)))
                        (consult--with-preview
                            (lambda (_ cand)
                              (when (and cand (string-match-p "^[[:digit:]]+$" cand))
                                (string-to-number cand)))
                            (let ((preview (consult--preview-position)))
                              (lambda (cand restore)
                                (funcall preview
                                         (when-let (pos (and cand (consult--line-position cand)))
                                           (and (consult--in-range-p pos) pos))
                                         restore)))
                          (read-from-minibuffer "Go to line: ")))))
             (if (car ret)
                 (let ((pos (consult--line-position (car ret))))
                   (if (consult--in-range-p pos)
                       (consult--jump pos)
                     (message "Line number out of range.")
                     (sit-for 1)
                     t))
               (message "Please enter a number.")
               (sit-for 1)
               t)))))

(defun consult--recent-file-read ()
  "Read recent file via `completing-read'."
  (consult--read
   "Find recent file: "
   (or (mapcar #'abbreviate-file-name recentf-list)
       (user-error "No recent files"))
   :sort nil
   :require-match t
   :category 'file
   :history 'file-name-history))

;;;###autoload
(defun consult-recent-file ()
  "Find recent using `completing-read'."
  (interactive)
  (find-file (consult--recent-file-read)))

;;;###autoload
(defun consult-recent-file-other-frame ()
  "Find recent using `completing-read'."
  (interactive)
  (find-file-other-frame (consult--recent-file-read)))

;;;###autoload
(defun consult-recent-file-other-window ()
  "Find recent using `completing-read'."
  (interactive)
  (find-file-other-window (consult--recent-file-read)))

;;;###autoload
(defun consult-file-externally (file)
  "Open FILE using system's default application."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (expand-file-name file))))

;; Use minibuffer completion as the UI for completion-at-point
;;;###autoload
(defun consult-completion-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'."
  (let* ((initial (buffer-substring-no-properties start end))
         (limit (car (completion-boundaries initial collection predicate "")))
         (metadata (completion-metadata initial collection predicate))
         (category (completion-metadata-get metadata 'category))
         (all (completion-all-completions initial collection predicate
                                          (length initial)))
         (exit-status 'finished)
         (completion
          (cond
           ((atom all) nil)
           ((and (consp all) (atom (cdr all)))
            (setq exit-status 'sole)
            (concat (substring initial 0 limit) (car all)))
           (t (let ((enable-recursive-minibuffers t))
                (if (eq category 'file)
                    ;; When completing files with consult-completion-in-region, the point in the
                    ;; minibuffer gets placed initially at the beginning of the last path component.
                    ;; By using the filename as DIR argument (second argument of read-file-name), it
                    ;; starts at the end of minibuffer contents, as for other types of completion.
                    ;; However this is undefined behavior since initial does not only contain the
                    ;; directory, but also the filename.
                    (read-file-name
                     "Completion: " initial initial t nil predicate)
                  (completing-read
                   "Completion: " collection predicate t initial)))))))
    (if (null completion)
        (progn (message "No completion") nil)
      (delete-region start end)
      (insert (substring-no-properties completion))
      (when-let ((exit (plist-get completion-extra-properties :exit-function)))
        (funcall exit completion exit-status))
      t)))

(defun consult--mode-commands (mode)
  "Extract commands from MODE."
  (let ((library-path (symbol-file mode))
        (key (if (memq mode minor-mode-list)
                 (if (local-variable-if-set-p mode) ?l ?g)
               ?m))
        (mode-rx (regexp-quote
                  (substring (if (eq major-mode 'c-mode)
                                 "cc-mode"
                               (symbol-name major-mode))
                             0 -5))))
    (mapcan
     (lambda (feature)
       (let ((path (car feature)))
         (when (or (string= path library-path)
                   (string-match-p mode-rx (file-name-nondirectory path)))
           (mapcar
            (lambda (x) (cons (cdr x) key))
            (seq-filter
             (lambda (cmd)
               (and (consp cmd)
                    (eq (car cmd) 'defun)
                    (commandp (cdr cmd))
                    (not (string-match-p consult-mode-command-filter
                                         (symbol-name (cdr cmd))))))
             (cdr feature))))))
     load-history)))

;;;###autoload
(defun consult-mode-command (&rest modes)
  "Run a command from the MODES.

If no modes are specified, use currently active major and minor modes."
  (interactive)
  (unless modes
    (setq modes (cons major-mode
                      (seq-filter (lambda (m)
                                    (and (boundp m) (symbol-value m)))
                                  minor-mode-list))))
  (command-execute
   (intern
    (consult--read
     "Mode command: "
     (consult--remove-dups
      (mapcan #'consult--mode-commands modes) #'car)
     :predicate
     (lambda (cand)
       (if consult--narrow
           (= (cdr cand) consult--narrow)
         (/= (cdr cand) ?g)))
     :narrow '((?m . "Major")
               (?l . "Local Minor")
               (?g . "Global Minor"))
     :require-match t
     :history 'consult--mode-command-history
     :category 'command))))

(defun consult--yank-read ()
  "Open kill ring menu and return selected text."
  (consult--read
   "Yank text: "
   (consult--remove-dups kill-ring)
   :history t ;; disable history
   :sort nil
   :category 'kill-ring
   :require-match t
   :preview (when consult-preview-yank
              (let* ((pt (point))
                     ;; If previous command is yank, hide previously yanked text
                     (mk (or (and (eq last-command 'yank) (mark t)) pt))
                     (ov (consult--overlay (min pt mk) (max pt mk) 'invisible t)))
                (lambda (cand restore)
                  (if restore
                      (delete-overlay ov)
                    ;; Use `add-face-text-property' on a copy of "cand in order to merge face properties
                    (setq cand (copy-sequence cand))
                    (add-face-text-property 0 (length cand) 'consult-preview-yank t cand)
                    ;; Use the `before-string' property since the overlay might be empty.
                    (overlay-put ov 'before-string cand)))))))

;; Insert selected text.
;; Adapted from the Emacs yank function.
;;;###autoload
(defun consult-yank ()
  "Select text from the kill ring and insert it."
  (interactive)
  (let ((text (consult--yank-read)))
    (setq yank-window-start (window-start))
    (push-mark)
    (insert-for-yank text)
    (setq this-command 'yank)
    nil))

;;;###autoload
(defun consult-yank-pop (&optional arg)
  "If there is a recent yank act like `yank-pop'.
Otherwise select text from the kill ring and insert it.
See `yank-pop' for the meaning of ARG."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop (or arg 1))
    (consult-yank)))

;; Replace just-yanked text with selected text.
;; Adapted from the Emacs yank-pop function.
;;;###autoload
(defun consult-yank-replace ()
  "Select text from the kill ring.
If there was no recent yank, insert the text.
Otherwise replace the just-yanked text with the selected text."
  (interactive)
  (if (not (eq last-command 'yank))
      (consult-yank)
    (let ((text (consult--yank-read))
          (inhibit-read-only t)
          (pt (point))
          (mk (mark t)))
      (setq this-command 'yank)
      (funcall (or yank-undo-function 'delete-region) (min pt mk) (max pt mk))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) pt (current-buffer))
      (insert-for-yank text)
      (set-window-start (selected-window) yank-window-start t)
      (if (< pt mk)
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))
  nil)

(defun consult--register-candidates ()
  "Return alist of register descriptions and register names."
  (mapcar (lambda (r)
            (setq r (car r))
            (cons (concat
                   (propertize (single-key-description r) 'face 'consult-key)
                   " "
                   (register-describe-oneline r))
                  r))
          ;; Sometimes, registers are made without a `cdr'.
          ;; Such registers don't do anything, and can be ignored.
          (or (sort (seq-filter #'cdr register-alist) #'car-less-than-car)
              (user-error "All registers are empty"))))

;;;###autoload
(defun consult-register (reg)
  "Use register REG. Either jump to location or insert the stored text."
  (interactive
   (list
    (consult--read
     "Register: "
     (consult--register-candidates)
     :category 'register
     :sort nil
     :require-match t
     :history t ;; disable history
     :lookup #'consult--lookup-cdr)))
  (condition-case nil
      (jump-to-register reg)
    (error (insert-register reg))))

;;;###autoload
(defun consult-bookmark (name)
  "If bookmark NAME exists, open it, otherwise set bookmark under the given NAME."
  (interactive (list (consult--read
                      "Bookmark: " (bookmark-all-names)
                      :history 'bookmark-history
                      :category 'bookmark)))
  (if (assoc name bookmark-alist)
      (bookmark-jump name)
    (bookmark-set name)))

;;;###autoload
(defun consult-apropos ()
  "Select pattern and call `apropos'."
  (interactive)
  (let ((pattern
         (consult--read
          "Apropos: "
          obarray
          :predicate (lambda (x) (or (fboundp x) (boundp x) (facep x) (symbol-plist x)))
          :history 'consult--apropos-history
          :category 'symbol
          :default (thing-at-point 'symbol))))
    (when (string= pattern "")
      (user-error "No pattern given"))
    (apropos pattern)))

;;;###autoload
(defun consult-complex-command ()
  "Select and evaluate command from the command history."
  (interactive)
  (let* ((history (or (consult--remove-dups (mapcar #'prin1-to-string command-history))
                      (user-error "There are no previous complex commands")))
         (cmd (read (consult--read
                     "Command: " history
                     :default (car history)
                     :sort nil
                     :history t ;; disable history
                     :category 'expression))))
    ;; Taken from `repeat-complex-command'
    (add-to-history 'command-history cmd)
    (apply #'funcall-interactively
	   (car cmd)
	   (mapcar (lambda (e) (eval e t)) (cdr cmd)))))

(defun consult--current-history ()
  "Return the history relevant to the current buffer.

If the minibuffer is active, returns the minibuffer history,
otherwise the history corresponding to the mode is returned.
There is a special case for `repeat-complex-command',
for which the command history is used."
  (cond
   ;; If pressing "C-x M-:", i.e., `repeat-complex-command',
   ;; we are instead querying the `command-history' and get a full s-expression.
   ;; Alternatively you might want to use `consult-complex-command',
   ;; which can also be bound to "C-x M-:"!
   ((eq last-command 'repeat-complex-command)
    (mapcar #'prin1-to-string command-history))
   ;; In the minibuffer we use the current minibuffer history,
   ;; which can be configured by setting `minibuffer-history-variable'.
   ((minibufferp)
    (symbol-value minibuffer-history-variable)) ;; (minibuffer-history-value) is Emacs 27 only
   ;; Otherwise we use a mode-specific history, see `consult-mode-histories'.
   (t (when-let (history
                 (or (seq-find (lambda (ring)
                                 (and (derived-mode-p (car ring))
                                      (boundp (cdr ring))))
                               consult-mode-histories)
                     (user-error
                      "No history configured for `%s', see `consult-mode-histories'"
                      major-mode)))
        (symbol-value (cdr history))))))

;; This command has been adopted from https://github.com/oantolin/completing-history/.
;;;###autoload
(defun consult-history (&optional history)
  "Insert string from buffer HISTORY."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (str (consult--read
               "History: "
               (let ((history (or history (consult--current-history))))
                 (or (consult--remove-dups (if (ring-p history)
                                               (ring-elements history)
                                             history))
                     (user-error "History is empty")))
               :history t ;; disable history
               :category ;; Report command category for M-x history
               (and (minibufferp)
                    (eq minibuffer-history-variable 'extended-command-history)
                    'command)
               :sort nil)))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (insert (substring-no-properties str))))

(defun consult--minor-mode-candidates ()
  "Return list of minor-mode candidate strings."
  (mapcar
   (pcase-lambda (`(,name . ,sym))
     (list name
           sym
           (concat
            (if (local-variable-if-set-p sym) "l" "g")
            (if (and (boundp sym) (symbol-value sym)) "i" "o"))))
   (append
    ;; according to describe-minor-mode-completion-table-for-symbol
    ;; the minor-mode-list contains *all* minor modes
    (mapcar (lambda (sym) (cons (symbol-name sym) sym)) minor-mode-list)
    ;; take the lighters from minor-mode-alist
    (delq nil
          (mapcar (pcase-lambda (`(,sym ,lighter))
                    (when (and lighter (not (equal "" lighter)))
                      (setq lighter (string-trim (format-mode-line lighter)))
                      (unless (string-blank-p lighter)
                        (cons lighter sym))))
                  minor-mode-alist)))))

;;;###autoload
(defun consult-minor-mode-menu ()
  "Enable or disable minor mode.
This is an alternative to `minor-mode-menu-from-indicator'."
  (interactive)
  (call-interactively
   (consult--read
    "Minor mode: "
    (consult--minor-mode-candidates)
    :require-match t
    :category 'minor-mode
    :narrow '((lambda (cand) (seq-position (caddr cand) consult--narrow))
              (?l . "Local")
              (?g . "Global")
              (?i . "On")
              (?o . "Off"))
    :lookup #'consult--lookup-cadr
    :history 'consult--minor-mode-menu-history)))

;;;###autoload
(defun consult-theme (theme)
  "Disable current themes and enable THEME from `consult-themes'."
  (interactive
   (list
    (let ((avail-themes (seq-filter (lambda (x) (or (not consult-themes)
                                                    (memq x consult-themes)))
                                    (cons nil (custom-available-themes))))
          (saved-theme (car custom-enabled-themes)))
      (consult--read
       "Theme: "
       (mapcar (lambda (x) (or x 'default)) avail-themes)
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (_input _cands x)
                 (and x (not (string= x "default")) (intern-soft x)))
       :preview (when consult-preview-theme
                  (lambda (cand restore)
                    (cond
                     ((and restore (not cand)) (consult-theme saved-theme))
                     ((memq cand avail-themes) (consult-theme cand)))))
       :default (symbol-name (or saved-theme 'default))))))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

(defsubst consult--buffer-candidate (type cand face)
  "Format virtual buffer candidate.

CAND is the candidate string.
TYPE is the type character.
FACE is the face for the candidate."
  (concat (propertize (string (+ consult--special-char type)) 'display "") (propertize cand 'face face)))

(defun consult--buffer (open-buffer open-file open-bookmark)
  "Backend implementation of `consult-buffer'.
Depending on the selected item OPEN-BUFFER, OPEN-FILE or OPEN-BOOKMARK will be used to display the item."
  (let* ((curr-buf (current-buffer))
         (all-bufs-list (buffer-list))
         (buf-file-hash (let ((ht (make-hash-table :test #'equal)))
                          (dolist (buf all-bufs-list ht)
                            (when-let (file (buffer-file-name buf))
                              (puthash file t ht)))))
         (all-bufs (append (delq curr-buf all-bufs-list) (list curr-buf)))
         (bufs (mapcar (lambda (x)
                         (consult--buffer-candidate ?b (buffer-name x) 'consult-buffer))
                       all-bufs))
         (views (when consult-view-list-function
                  (mapcar (lambda (x)
                            (consult--buffer-candidate ?v x 'consult-view))
                          (funcall consult-view-list-function))))
         (bookmarks (mapcar (lambda (x)
                              (consult--buffer-candidate ?m (car x) 'consult-bookmark))
                            bookmark-alist))
         (all-files (seq-remove (lambda (x) (gethash x buf-file-hash)) recentf-list))
         (files (mapcar (lambda (x)
                          (consult--buffer-candidate ?f (abbreviate-file-name x) 'consult-file))
                        all-files))
         (proj-root (and consult-project-root-function (funcall consult-project-root-function)))
         (proj-bufs (when proj-root
                         (mapcar (lambda (x)
                                   (consult--buffer-candidate ?p (buffer-name x) 'consult-buffer))
                                 (seq-filter (lambda (x)
                                               (when-let (file (buffer-file-name x))
                                                 (string-prefix-p proj-root file)))
                                             all-bufs))))
         (proj-files (when proj-root
                          (mapcar (lambda (x)
                                    (consult--buffer-candidate ?q (string-remove-prefix proj-root x) 'consult-file))
                                  (seq-filter (lambda (x) (string-prefix-p proj-root x)) all-files))))
         (saved-buf (current-buffer))
         (selected
          (consult--read
           "Switch to: " (append bufs files proj-bufs proj-files views bookmarks)
           :history 'consult--buffer-history
           :sort nil
           :predicate
           (lambda (cand)
             (let ((type (- (elt cand 0) consult--special-char)))
               (when (= type ?q) (setq type ?p)) ;; q=project files
               (if (eq consult--narrow 32) ;; narrowed to ephemeral
                   (and (= type ?b)
                        (= (elt cand 1) 32))
                 (and
                  (or (/= type ?b) ;; non-ephemeral
                      (/= (elt cand 1) 32))
                  (or (not consult--narrow) ;; narrowed
                      (= type consult--narrow))))))
           :narrow `((32 . "Ephemeral")
                     (?b . "Buffer")
                     (?f . "File")
                     (?m . "Bookmark")
                     ,@(when proj-root '((?p . "Project")))
                     ,@(when consult-view-list-function '((?v . "View"))))
           :category 'virtual-buffer
           :lookup
           (lambda (_ candidates cand)
             (if (member cand candidates)
                 (cons (pcase-exhaustive (- (elt cand 0) consult--special-char)
                         (?b open-buffer)
                         (?m open-bookmark)
                         (?v consult-view-open-function)
                         (?p open-buffer)
                         (?q open-file)
                         (?f open-file))
                       (substring cand 1))
               ;; When candidate is not found in the alist,
               ;; default to creating a new buffer.
               (and (not (string-blank-p cand)) (cons open-buffer cand))))
           :preview
           (when consult-preview-buffer
             (lambda (cand restore)
               (cond
                (restore
                 (when (buffer-live-p saved-buf)
                   (set-buffer saved-buf)))
                ;; In order to avoid slowness and unnecessary complexity, we
                ;; only preview buffers. Loading recent files, bookmarks or
                ;; views can result in expensive operations.
                ((and (eq (car cand) open-buffer) (get-buffer (cdr cand)))
                 (funcall open-buffer (cdr cand) 'norecord))))))))
    (when selected (funcall (car selected) (cdr selected)))))

;;;###autoload
(defun consult-buffer-other-frame ()
  "Enhanced `switch-to-buffer-other-frame' command with support for virtual buffers."
  (interactive)
  ;; bookmark-jump-other-frame is supported on Emacs >= 27.1, we want to support at least 26
  (consult--buffer #'switch-to-buffer-other-frame #'find-file-other-frame
                   (if (fboundp 'bookmark-jump-other-frame) #'bookmark-jump-other-frame #'bookmark-jump)))

;;;###autoload
(defun consult-buffer-other-window ()
  "Enhanced `switch-to-buffer-other-window' command with support for virtual buffers."
  (interactive)
  (consult--buffer #'switch-to-buffer-other-window #'find-file-other-window #'bookmark-jump-other-window))

;;;###autoload
(defun consult-buffer ()
  "Enhanced `switch-to-buffer' command with support for virtual buffers."
  (interactive)
  (consult--buffer #'switch-to-buffer #'find-file #'bookmark-jump))

(defun consult--kmacro-candidates ()
  "Return alist of kmacros and indices."
  (consult--remove-dups
   (thread-last
       ;; List of macros
       (cons (list last-kbd-macro
                   kmacro-counter
                   kmacro-counter-format)
             kmacro-ring)
     ;; Add indices
     (seq-map-indexed #'cons)
     ;; Filter mouse clicks
     (seq-remove (lambda (x) (seq-some #'mouse-event-p (caar x))))
     ;; Format macros
     (mapcar (pcase-lambda (`((,keys ,counter ,format) . ,index))
               (cons
                (concat
                 ;; If the counter is 0 and the counter format is its default,
                 ;; then there is a good chance that the counter isn't actually
                 ;; being used.  This can only be wrong when a user
                 ;; intentionally starts the counter with a negative value and
                 ;; then increments it to 0.
                 (cond
                  ((not (string= format "%d")) ;; show counter for non-default format
                   (propertize " " 'display (format "%d(%s) " counter format)))
                  ((/= counter 0) ;; show counter if non-zero
                   (propertize " " 'display (format "%d " counter))))
                 (format-kbd-macro keys 1))
                index))))
   #'car))

;;;###autoload
(defun consult-kmacro (arg)
  "Run a chosen keyboard macro.  With prefix ARG, run the macro that many times.

Macros containing mouse clicks aren't displayed."
  (interactive "p")
  (let ((selected (consult--read
                   "Keyboard macro: "
                   (or (consult--kmacro-candidates)
                       (user-error "No keyboard macros defined"))
                   :category 'kmacro
                   :require-match t
                   :sort nil
                   :history 'consult--kmacro-history
                   :lookup #'consult--lookup-cdr)))
    (if (zerop selected)
        ;; If the first element has been selected, just run the last macro.
        (kmacro-call-macro (or arg 1) t nil)
      ;; Otherwise, run a kmacro from the ring.
      (let* ((selected (- selected 1))
             (kmacro (nth selected kmacro-ring))
             ;; Temporarily change the variables to retrieve the correct
             ;; settings.  Mainly, we want the macro counter to persist, which
             ;; automatically happens when cycling the ring.
             (last-kbd-macro (car kmacro))
             (kmacro-counter (cadr kmacro))
             (kmacro-counter-format (caddr kmacro)))
        (kmacro-call-macro (or arg 1) t)
        ;; Once done, put updated variables back into the ring.
        (setf (nth selected kmacro-ring)
              (list last-kbd-macro
                    kmacro-counter
                    kmacro-counter-format))))))

(defun consult--imenu-flatten (prefix list)
  "Flatten imenu LIST.
Prepend PREFIX in front of all items."
  (mapcan
   (lambda (item)
     (if (imenu--subalist-p item)
         (consult--imenu-flatten
          (concat prefix (and prefix "/") (car item))
          (mapcar (pcase-lambda (`(,e . ,v))
                    (cons e (if (integerp v) (copy-marker v) v)))
                  (cdr item)))
       (let ((key (concat
                   (and prefix (concat (propertize prefix 'face 'consult-imenu-prefix) " "))
                   (car item)))
             (pos (cdr item)))
         (list (cons key (cons key
                               (cond
                                ;; Semantic uses overlay for positions
                                ((overlayp pos) (overlay-start pos))
                                ;; Replace integer positions with markers
                                ((integerp pos) (copy-marker pos))
                                (t pos))))))))
   list))

(defun consult--imenu-candidates ()
  "Return imenu candidates."
  (consult--forbid-minibuffer)
  (let* ((imenu-auto-rescan t)
         (imenu-use-markers t)
         (items (imenu--make-index-alist t)))
    (setq items (remove (assoc "*Rescan*" items) items))
    ;; Fix toplevel items, e.g., emacs-lisp-mode toplevel items are functions
    (when-let ((toplevel (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-toplevel))))
      (let ((tops (seq-remove (lambda (x) (listp (cdr x))) items))
            (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
        (setq items (append rest (list (cons toplevel tops))))))
    (seq-sort-by #'car #'string<
                 (consult--imenu-flatten nil items))))

;;;###autoload
(defun consult-imenu ()
  "Choose from flattened `imenu' using `completing-read'."
  (interactive)
  (let ((narrow (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-narrow))))
    (imenu
     (consult--read
      "Go to item: "
      (or (consult--imenu-candidates)
          (user-error "Imenu is empty"))
      :preview
      (when consult-preview-imenu
        (let ((preview (consult--preview-position)))
          (lambda (cand restore)
            ;; Only preview imenu items which are markers,
            ;; in order to avoid any bad side effects.
            (funcall preview (and (consp cand) (markerp (cdr cand)) (cdr cand)) restore))))
      :require-match t
      :narrow
      (cons (lambda (cand)
              (when-let (n (cdr (assoc consult--narrow narrow)))
                (let* ((c (car cand))
                       (l (length n)))
                  (and (> (length c) l)
                       (eq t (compare-strings n 0 l c 0 l))
                       (= (elt c l) 32)))))
            narrow)
      :category 'imenu
      :lookup #'consult--lookup-cdr
      :history 'consult--imenu-history
      :add-history (thing-at-point 'symbol)
      :sort nil))
    (run-hooks 'consult-after-jump-hook)))

(defconst consult--grep-regexp "\\([^\0\n]+\\)\0\\([^:\0]+\\)[:\0]"
  "Regexp used to match file and line of grep output.")

(defconst consult--grep-match-regexp "\e\\[[0-9;]+m\\(.*?\\)\e\\[[0-9;]*m"
  "Regexp used to find matches in grep output.")

(defun consult--grep-matches (lines)
  "Find grep match for REGEXP in LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
        (when (string-match consult--grep-regexp str)
          (let* ((file (expand-file-name (consult--strip-ansi-escape (match-string 1 str))))
                 (line (string-to-number (consult--strip-ansi-escape (match-string 2 str))))
                 (str (substring str (match-end 0)))
                 (loc (consult--format-location (file-relative-name file) line)))
            (while (string-match consult--grep-match-regexp str)
              (setq str (concat (substring str 0 (match-beginning 0))
                                (propertize (substring (match-string 1 str)) 'face 'consult-preview-match)
                                (substring str (match-end 0)))))
            (setq str (consult--strip-ansi-escape str))
            (push (list (concat loc str)
                  file line
                  (next-single-char-property-change 0 'face str))
                  candidates)))))
    (nreverse candidates)))

(defun consult--grep-marker (open)
  "Grep candidate to marker.

OPEN is the function to open new files."
  (lambda (_input candidates cand)
    (when-let* ((loc (cdr (assoc cand candidates)))
                (buf (funcall open (car loc))))
      (with-current-buffer buf
        (save-restriction
          (save-excursion
            (widen)
            (goto-char (point-min))
            ;; Location data might be invalid by now!
            (ignore-errors
              (forward-line (- (cadr loc) 1))
              (forward-char (caddr loc)))
            (point-marker)))))))

(defvar consult--git-grep-command '("git" "--no-pager" "grep" "--null" "--color=always" "--extended-regexp" "--line-number" "-I" "-e"))
(defvar consult--grep-command '("grep" "--null" "--line-buffered" "--color=always" "--extended-regexp" "--exclude-dir=.git" "--line-number" "-I" "-r" "." "-e"))
(defvar consult--ripgrep-command '("rg" "--null" "--line-buffered" "--color=always" "--max-columns=500" "--no-heading" "--line-number" "." "-e"))

(defun consult--grep-async (cmd)
  "Async function for `consult-grep'.

CMD is the grep argument list."
  (thread-first (consult--async-sink)
    (consult--async-refresh-timer)
    (consult--async-transform consult--grep-matches)
    (consult--async-process (lambda (input) (append cmd (list input))))
    (consult--async-throttle)
    (consult--async-split)))

(defun consult--grep (prompt cmd dir initial)
  "Run grep CMD in DIR with INITIAL input.

PROMPT is the prompt string."
  (pcase-let ((`(,prompt . ,default-directory) (consult--directory-prompt prompt dir)))
    (consult--with-preview-files (open)
      (consult--jump
       (consult--read
        prompt
        (consult--grep-async cmd)
        :lookup (consult--grep-marker open)
        :preview (and consult-preview-grep (consult--preview-position))
        :initial (concat consult-async-default-split initial)
        :add-history (concat consult-async-default-split (thing-at-point 'symbol))
        :require-match t
        :category 'xref-location
        :history '(:input consult--grep-history)
        :sort nil)))))

;;;###autoload
(defun consult-grep (&optional dir initial)
  "Search for regexp with grep in DIR with INITIAL input."
  (interactive "P")
  (consult--grep "Grep" consult--grep-command dir initial))

;;;###autoload
(defun consult-git-grep (&optional dir initial)
  "Search for regexp with grep in DIR with INITIAL input."
  (interactive "P")
  (consult--grep "Git-grep" consult--git-grep-command dir initial))

;;;###autoload
(defun consult-ripgrep (&optional dir initial)
  "Search for regexp with rg in DIR with INITIAL input."
  (interactive "P")
  (consult--grep "Ripgrep" consult--ripgrep-command dir initial))

(defun consult--find-async (cmd)
  "Async function for `consult--find'.

CMD is the find argument list."
  (thread-first (consult--async-sink)
    (consult--async-refresh-timer)
    (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
    (consult--async-process (lambda (input) (append cmd (list input))))
    (consult--async-throttle)
    (consult--async-split)))

(defun consult--find (prompt cmd)
  "Generalized function for `consult-find' and similar functions.

PROMPT is the prompt.
CMD is the find argument list."
  (find-file
   (consult--read
    prompt
    (consult--find-async cmd)
    :sort nil
    :require-match t
    :initial consult-async-default-split
    :add-history (concat consult-async-default-split (thing-at-point 'symbol))
    :category 'file
    :history '(:input consult--find-history))))

(defvar consult--find-cmd '("find" "-not" "(" "-wholename" "*/.*" "-prune" ")" "-ipath"))
(defvar consult--fdfind-cmd '("fdfind" "--color=never" "--full-path"))
(defvar consult--locate-cmd '("locate" "--ignore-case" "--existing" "--regexp"))

;;;###autoload
(defun consult-find (&optional dir)
  "Search for regexp with find in DIR."
  (interactive "P")
  (pcase-let ((`(,prompt . ,default-directory) (consult--directory-prompt "Find" dir)))
    (consult--find prompt consult--find-cmd)))

;;;###autoload
(defun consult-fdfind (&optional dir)
  "Search for regexp with fdfind in DIR."
  (interactive "P")
  (pcase-let ((`(,prompt . ,default-directory) (consult--directory-prompt "Fdfind" dir)))
    (consult--find prompt consult--fdfind-cmd)))

;;;###autoload
(defun consult-locate ()
  "Search for regexp with locate."
  (interactive)
  (consult--find "Locate: " consult--locate-cmd))

;;;; default completion-system support

(defun consult--default-completion-candidate ()
  "Return current candidate from default completion system."
  (when (and (not icomplete-mode) (eq completing-read-function #'completing-read-default))
    (let ((cand (minibuffer-contents-no-properties)))
      (when (test-completion cand
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
        cand))))

(defun consult--default-completion-match ()
  "Return default matching function."
  (lambda (str cands) (completion-all-completions str cands nil (length str))))

(add-hook 'consult--completion-candidate-hook #'consult--default-completion-candidate)
(add-hook 'consult--completion-match-hook #'consult--default-completion-match)

;;;; icomplete support

(defun consult--icomplete-candidate ()
  "Return current icomplete candidate."
  (and icomplete-mode (car completion-all-sorted-completions)))

(declare-function icomplete-exhibit "icomplete")
(declare-function icomplete--field-beg "icomplete")
(declare-function icomplete--field-end "icomplete")
(defun consult--icomplete-refresh ()
  "Refresh icomplete view, keep current candidate selected if possible."
  (when icomplete-mode
    (let ((top (car completion-all-sorted-completions)))
      (completion--flush-all-sorted-completions)
      (when top
        (let* ((completions (completion-all-sorted-completions
                             (icomplete--field-beg) (icomplete--field-end)))
               (last (last completions))
               (before)) ;; completions before top
          ;; warning: completions is an improper list
          (while (consp completions)
            (if (equal (car completions) top)
                (progn
                  (setcdr last (append (nreverse before) (cdr last)))
                  (setq completion-all-sorted-completions completions
                        completions nil))
              (push (car completions) before)
              (setq completions (cdr completions)))))))
    (icomplete-exhibit)))

(add-hook 'consult--completion-candidate-hook #'consult--icomplete-candidate)
(add-hook 'consult--completion-refresh-hook #'consult--icomplete-refresh)

(provide 'consult)
;;; consult.el ends here
