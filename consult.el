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

;; Consult implements a set of commands which use `completing-read' to select
;; from a list of candidates. Most provided commands follow the naming scheme
;; `consult-<thing>'. Some commands are drop-in replacements for existing
;; functions, e.g., `consult-apropos' or the enhanced buffer switcher
;; `consult-buffer.' Other commands provide additional functionality, e.g.,
;; `consult-line', to search for a line. Many commands support candidate
;; preview. If a candidate is selected in the completion view, the buffer shows
;; the candidate immediately.

;;; Acknowledgements:

;; This package took inspiration from Counsel by Oleh Krehel. Some of the
;; commands found in this package originated in the Selectrum wiki. See the
;; README for a full list of contributors.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'imenu)
(require 'kmacro)
(require 'outline)
(require 'recentf)
(require 'ring)
(require 'seq)
(require 'subr-x)

(defgroup consult nil
  "Consultation using `completing-read'."
  :group 'convenience
  :prefix "consult-")

;;;; General customization

(defcustom consult-narrow-key nil
  "Prefix key for narrowing during completion."
  :type 'vector)

(defcustom consult-widen-key nil
  "Key used for widening during completion."
  :type 'vector)

(defcustom consult-view-list-function nil
  "Function which returns a list of view names as strings, used by `consult-buffer'."
  :type 'function)

(defcustom consult-view-open-function nil
  "Function which opens a view, used by `consult-buffer'."
  :type 'function)

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

(defcustom consult-line-numbers-widen t
  "Show absolute line numbers when narrowing is active."
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

;;;; Faces

(defgroup consult-faces nil
  "Faces used by Consult."
  :group 'consult
  :group 'faces)

(defface consult-preview-line
  '((t :inherit region))
  "Face used to for line previews.")

(defface consult-preview-cursor
  '((t :inherit match))
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

(defface consult-key
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight keys, e.g., in `consult-register'.")

(defface consult-imenu-prefix
  '((t :inherit consult-key))
  "Face used to highlight imenu prefix in `consult-imenu'.")

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

(defface consult-line-number
  '((t :inherit line-number))
  "Face used to highlight line numbers in selections.")

;;;; History variables

(defvar-local consult-error-history nil
  "Buffer-local history for the command `consult-error'.")

(defvar-local consult-outline-history nil
  "Buffer-local history for the command `consult-outline'.")

(defvar-local consult-mark-history nil
  "Buffer-local history for the command `consult-mark'.")

(defvar-local consult-line-history nil
  "Buffer-local history for the command `consult-line'.")

(defvar-local consult-imenu-history nil
  "Buffer-local history for the command `consult-imenu'.")

(defvar consult-buffer-history nil
  "History for the command `consult-buffer'.")

(defvar consult-apropos-history nil
  "History for the command `consult-apropos'.")

(defvar consult-register-history nil
  "History for the command `consult-register'.")

(defvar consult-theme-history nil
  "History for the command `consult-theme'.")

(defvar consult-minor-mode-menu-history nil
  "History for the command `consult-minor-mode-menu'.")

(defvar consult-kmacro-history nil
  "History for the command `consult-kmacro'.")

;;;; Internal variables

(defvar-local consult--narrow-prefixes nil
  "Narrowing prefixes of the current completion.")

(defvar consult--narrow-separator (concat (string 8203) " ") ;; zero width space
  "String used to separate prefix for narrowing.
This string must be made of unique characters,
such that no accidential matching occurs. Therefore
we use a zero-width-space, which generally
does not occur in candidate strings.")

(defvar consult--gc-threshold 67108864
  "Large gc threshold for temporary increase.")

(defvar consult--gc-percentage 0.5
  "Large gc percentage for temporary increase.")

(defvar-local consult--preview-function nil
  "Active preview function.")

;;;; Helper functions

(defun consult--lookup-list (alist key)
  "Lookup KEY in ALIST."
  (cdr (assoc key alist)))

(defun consult--forbid-minibuffer ()
  "Raise an error if executed from the minibuffer."
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer")))

(defsubst consult--fontify ()
  "Ensure that the whole buffer is fontified."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line. Therefore we have to enforce
  ;; font-locking now, which is slow. In order to prevent is hang-up we check the buffer size
  ;; against `consult-fontify-limit'.
  (when (and jit-lock-mode (< (buffer-size) consult-fontify-limit))
    (jit-lock-fontify-now)))

;; We must disambiguate the lines by adding a prefix such that two lines with the same text can be
;; distinguished. In order to avoid matching the line number, such that the user can search for
;; numbers with `consult-line', we encode the line number as unicode characters in the supplementary
;; private use plane b. By doing that, it is unlikely that accidential matching occurs.
(defsubst consult--unique (pos display)
  "Generate unique string for POS.
DISPLAY is the string to display instead of the unique string."
  (let ((unique-prefix "") (n pos))
    (while (progn
             (setq unique-prefix (concat (string (+ #x100000 (% n #xFFFE))) unique-prefix))
             (and (>= n #xFFFE) (setq n (/ n #xFFFE)))))
    (propertize unique-prefix 'display display)))

(defun consult--preview-install (preview fun)
  "Install preview support to minibuffer completion.

PREVIEW is the preview function.
FUN is the body function."
  (let ((orig-window (selected-window))
        (selected)
        (state (funcall preview 'save nil nil)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq consult--preview-function
                (lambda (cand)
                  (with-selected-window orig-window
                    (funcall preview 'preview cand nil)))))
      (unwind-protect
          (save-excursion
            (save-restriction
              (setq selected (funcall fun))))
        (funcall preview 'restore selected state)))))

(defmacro consult--with-preview (preview &rest body)
  "Install preview in BODY.

PREVIEW is the preview function."
  (declare (indent 1))
  (let ((preview-var (make-symbol "preview")))
    `(let ((,preview-var ,preview))
       (if ,preview-var
           (consult--preview-install ,preview-var (lambda () ,@body))
         ,@body))))

(defsubst consult--narrowed-p (str)
  "Return t if STR has some of the narrowing PREFIXES."
  (string-match-p (concat "^." consult--narrow-separator) str))

(defsubst consult--narrow-strip (str)
  "Strip narrowing prefix from STR."
  (if (consult--narrowed-p str)
      (replace-regexp-in-string "^[^ ]+ " "" str)
    str))

(defsubst consult--narrow-candidate (prefix &rest strings)
  "Add narrowing prefix PREFIX and concatenate with STRINGS."
  (apply #'concat
         (propertize (concat (string prefix) consult--narrow-separator) 'display "")
         strings))

(defsubst consult--narrow-indicator (pair)
  "Narrowing indicator string for PAIR."
  (propertize (concat (string (car pair)) consult--narrow-separator)
              'display
              (propertize (format "[%s] " (cdr pair))
                          'face 'consult-narrow-indicator)))

(defun consult-widen ()
  "Widen current completion."
  (interactive)
  (let ((str (consult--narrow-strip (minibuffer-contents))))
    (delete-minibuffer-contents)
    (insert str)))

(defun consult-narrow ()
  "Narrow current completion."
  (interactive)
  (let ((str (consult--narrow-strip (minibuffer-contents))))
    (delete-minibuffer-contents)
    (insert (concat (consult--narrow-indicator
                     (assoc last-command-event consult--narrow-prefixes)))
            str)))

(defconst consult--narrow-delete
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (let ((str (minibuffer-contents-no-properties)))
         (when (and (string-suffix-p consult--narrow-separator str)
                    (consult--narrowed-p str))
           #'delete-minibuffer-contents)))))

(defconst consult--narrow-space
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (let ((str (minibuffer-contents-no-properties)))
         (when (= 1 (length str))
           (when-let (pair (assoc (elt str 0) consult--narrow-prefixes))
             (delete-minibuffer-contents)
             (insert (consult--narrow-indicator pair))
             #'ignore))))))

(defsubst consult--define-key (map key cmd desc)
  "Bind CMD to KEY in MAP and add which-key description DESC."
  (define-key map key cmd)
  ;; The which-key description is potentially fragile if something is changed on the side
  ;; of which-key. Keep an eye on that. An alternative more standard-compliant method
  ;; would be to use `menu-item', but this is unfortunately not yet supported by which-key
  ;; and `describe-buffer-bindings'.
  ;; See https://github.com/justbur/emacs-which-key/issues/177
  (let ((idx (- (length key) 1)))
    (define-key map (vconcat (seq-take key idx) (vector 'which-key (elt key idx)))
      `(which-key (,desc . ,cmd)))))

(defun consult--narrow-install (prefixes fun)
  "Install narrowing in FUN.

PREFIXES is an alist of narrowing prefix strings."
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
         (setq consult--narrow-prefixes prefixes)
         (let ((map (make-composed-keymap nil (current-local-map))))
           (when consult-narrow-key
             (dolist (pair prefixes)
               (consult--define-key
                map
                (vconcat consult-narrow-key (vector (car pair)))
                #'consult-narrow (cdr pair))))
           (when consult-widen-key
             (consult--define-key map consult-widen-key #'consult-widen "All"))
           (define-key map " " consult--narrow-space)
           (define-key map [127] consult--narrow-delete)
           (use-local-map map))))
    (funcall fun)))

(defmacro consult--with-narrow (prefixes &rest body)
  "Setup narrowing in BODY.

PREFIXES is an alist of narrowing prefix strings."
  (declare (indent 1))
  (let ((prefixes-var (make-symbol "prefixes")))
    `(let ((,prefixes-var ,prefixes))
       (if ,prefixes-var
           (consult--narrow-install ,prefixes-var (lambda () ,@body))
         ,@body))))

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
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
      (when (and (invisible-p (overlay-get ov 'invisible))
                 (overlay-get ov 'isearch-open-invisible))
        (if permanently
            (funcall (overlay-get ov 'isearch-open-invisible) ov)
          (push (cons ov (overlay-get ov 'invisible)) opened)
          (if-let ((func (overlay-get ov 'isearch-open-invisible-temporary)))
              (funcall func nil)
            (overlay-put ov 'invisible nil)))))
    opened))

;; Derived from ctrlf, originally isearch
(defun consult--invisible-restore (overlays)
  "Restore any opened OVERLAYS that were previously disabled."
  (dolist (ov overlays)
    (if-let ((func (overlay-get (car ov) 'isearch-open-invisible-temporary)))
        (funcall func t)
      (overlay-put (car ov) 'invisible (cdr ov)))))

(defsubst consult--jump-1 (pos)
  "Go to POS and recenter."
  (when pos
    (when (and (markerp pos) (not (eq (current-buffer) (marker-buffer pos))))
      (switch-to-buffer (marker-buffer pos)))
    ;; Widen if we cannot jump to the position (idea from flycheck-jump-to-error)
    (unless (= (goto-char pos) (point))
      (widen)
      (goto-char pos))
    (run-hooks 'consult-after-jump-hook)))

(defsubst consult--jump (pos)
  "Push current position to mark ring, go to POS and recenter."
  (when pos
    ;; When the marker is in the same buffer,
    ;; record previous location such that the user can jump back quickly.
    (unless (and (markerp pos) (not (eq (current-buffer) (marker-buffer pos))))
      (push-mark (point) t))
    (consult--jump-1 pos)
    (consult--invisible-show t))
  nil)

(defsubst consult--overlay (beg end face)
  "Make consult overlay between BEG and END with FACE."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    ov))

;; TODO Matched strings are not highlighted as of now
;; see https://github.com/minad/consult/issues/7
(defun consult--preview-position (&optional face)
  "The preview function used if selecting from a list of candidate positions.
The function can be used as the `:preview' argument of `consult--read'.
FACE is the cursor face."
  (let ((overlays)
        (invisible)
        (face (or face 'consult-preview-cursor)))
    (lambda (cmd cand state)
      (pcase cmd
        ('save (current-buffer))
        ('restore
         (consult--invisible-restore invisible)
         (mapc #'delete-overlay overlays)
         (when (buffer-live-p state)
           (set-buffer state)))
        ('preview
         (consult--jump-1 cand)
         (consult--invisible-restore invisible)
         (setq invisible (consult--invisible-show))
         (mapc #'delete-overlay overlays)
         (let ((pos (point)))
           (setq overlays
                 (list (consult--overlay (line-beginning-position) (line-end-position) 'consult-preview-line)
                       (consult--overlay pos (1+ pos) face)))))))))

(cl-defun consult--read (prompt candidates &key
                                predicate require-match history default
                                category initial preview narrow
                                (sort t) (default-top t) (lookup (lambda (_ x) x)))
  "Simplified completing read function.

PROMPT is the string to prompt with.
CANDIDATES is the candidate list or alist.
PREDICATE is a filter function for the candidates.
REQUIRE-MATCH equals t means that an exact match is required.
HISTORY is the symbol of the history variable.
DEFAULT is the default input.
CATEGORY is the completion category.
SORT should be set to nil if the candidates are already sorted.
LOOKUP is a function which is applied to the result.
INITIAL is initial input.
DEFAULT-TOP must be nil if the default candidate should not be moved to the top.
PREVIEW is a preview function.
NARROW is an alist of narrowing prefix strings and description."
  (ignore default-top)
  ;; supported types
  (cl-assert (and
              (not (functionp candidates))
              (or (not candidates) ;; nil
                  (obarrayp candidates) ;; obarray
                  (stringp (car candidates)) ;; string list
                  (symbolp (car candidates)) ;; symbol list
                  (consp (car candidates))))) ;; alist
  (let ((candidates-fun
         (if (and sort (not category))
             candidates
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   ,@(if category `((category . ,category)))
                   ,@(if (not sort) '((cycle-sort-function . identity)
                                      (display-sort-function . identity))))
               (complete-with-action action candidates str pred))))))
    (funcall
     lookup candidates
     (consult--with-preview
         (and preview
              (lambda (cmd cand state)
                (funcall preview cmd (and cand (funcall lookup candidates cand)) state)))
       (consult--with-narrow narrow
         (completing-read prompt candidates-fun
                          predicate require-match initial history default))))))

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
              'face 'consult-line-number))

(defun consult--add-line-number (max-line candidates)
  "Add line numbers to unformatted CANDIDATES as prefix.
The MAX-LINE is needed to determine the width.
Since the line number is part of the candidate it will be matched-on during completion."
  (let ((width (length (number-to-string max-line))))
    (dolist (cand candidates)
      (setcar cand
              (concat
               (consult--unique (cdr cand) (consult--line-number-prefix width (caar cand)))
               (cdar cand))))
    candidates))

(defsubst consult--line-with-cursor-1 (marker &optional face)
  "Return current line string with a marking at the current cursor position.

MARKER is the cursor position.
FACE is the face to use for the cursor marking."
  (let* ((begin (line-beginning-position))
         (col (- marker begin))
         (str (buffer-substring begin (line-end-position)))
         (end (1+ col))
         (face (or face 'consult-preview-cursor)))
    (if (> end (length str))
        (concat (substring str 0 col)
                (propertize " " 'face face))
      (concat (substring str 0 col)
              (propertize (substring str col end) 'face face)
              (substring str end)))))

(defun consult--line-with-cursor (line marker &optional face)
  "Return line candidate.

LINE is line number.
MARKER is the cursor marker.
FACE is the cursor face."
  (cons (cons line (consult--line-with-cursor-1 marker face)) marker))

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
  (consult--fontify)
  (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (heading-regexp (concat "^\\(?:" outline-regexp "\\)"))
         (unformatted-candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil t))
        (setq line (+ line (consult--count-lines (match-beginning 0))))
        (push (cons
               (cons line (buffer-substring (line-beginning-position) (line-end-position)))
               (point-marker))
              unformatted-candidates)
        (unless (eobp) (forward-char 1))))
    (or (consult--add-line-number line (nreverse unformatted-candidates))
        (user-error "No headings"))))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading."
  (interactive)
  (consult--jump
   (consult--read "Go to heading: " (consult--with-increased-gc (consult--outline-candidates))
                  :category 'line
                  :sort nil
                  :require-match t
                  :lookup #'consult--lookup-list
                  :history 'consult-outline-history
                  :preview (and consult-preview-outline (consult--preview-position)))))

(defun consult--error-next ()
  "Return position of next error or nil."
  ;; next-error prints messages
  (cl-letf (((symbol-function 'message) #'format))
    (condition-case nil
        (save-excursion
          (while (let ((last-pos (point)))
                   (funcall next-error-function 1 (= last-pos (point-min)))
                   ;; next-error can jump backwards
                   (when (<= (point) last-pos)
                     (or (end-of-line) t))))
          (point))
      (error nil))))

(defun consult--error-candidates ()
  "Return alist of errors and positions."
  (unless next-error-function
    (user-error "Buffer does not support errors"))
  (consult--forbid-minibuffer)
  (consult--fontify)
  (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (unformatted-candidates))
      (save-excursion
        (goto-char (point-min))
        (while (when-let (pos (consult--error-next))
                 (setq line (+ line (consult--count-lines pos))))
          (push (consult--line-with-cursor line (point-marker) 'consult-preview-error)
                unformatted-candidates)))
    (or (consult--add-line-number line (nreverse unformatted-candidates))
        (user-error "No errors"))))

;;;###autoload
(defun consult-error ()
  "Jump to an error in the current buffer."
  (interactive)
  (consult--jump
   (consult--read "Go to error: " (consult--with-increased-gc (consult--error-candidates))
                  :category 'line
                  :sort nil
                  :require-match t
                  :lookup #'consult--lookup-list
                  :history 'consult-error-history
                  :preview
                  (and consult-preview-error (consult--preview-position 'consult-preview-error)))))

(defun consult--mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (consult--forbid-minibuffer)
  (unless (marker-position (mark-marker))
    (user-error "No marks"))
  (consult--fontify)
  (let* ((all-markers (delete-dups (cons (mark-marker) (reverse mark-ring))))
         (max-line 0)
         (min (point-min))
         (max (point-max))
         (unformatted-candidates))
    (save-excursion
      (dolist (marker all-markers)
        (let ((pos (marker-position marker)))
          (when (and (>= pos min) (<= pos max))
            (goto-char pos)
            ;; `line-number-at-pos' is a very slow function, which should be replaced everywhere.
            ;; However in this case the slow line-number-at-pos does not hurt much, since
            ;; the mark ring is usually small since it is limited by `mark-ring-max'.
            (let ((line (line-number-at-pos pos consult-line-numbers-widen)))
              (setq max-line (max line max-line))
              (push (consult--line-with-cursor line marker) unformatted-candidates))))))
    (consult--add-line-number max-line unformatted-candidates)))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in `mark-ring'."
  (interactive)
  (consult--jump
   (consult--read "Go to mark: " (consult--with-increased-gc (consult--mark-candidates))
                  :category 'line
                  :sort nil
                  :require-match t
                  :lookup #'consult--lookup-list
                  :history 'consult-mark-history
                  :preview (and consult-preview-mark (consult--preview-position)))))

(defun consult--line-candidates ()
  "Return alist of lines and positions."
  (consult--forbid-minibuffer)
  (consult--fontify)
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

;;;###autoload
(defun consult-line (&optional initial)
  "Search for a matching line and jump to the line beginning.
The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optionally INITIAL input can be provided."
  (interactive)
  (consult--jump
   (let ((candidates (consult--with-increased-gc (consult--line-candidates))))
     (consult--read "Go to line: " (cdr candidates)
                    :category 'line
                    :sort nil
                    :default-top nil
                    :require-match t
                    :history 'consult-line-history
                    :lookup #'consult--lookup-list
                    :default (car candidates)
                    :initial initial
                    :preview (and consult-preview-line (consult--preview-position))))))

;;;###autoload
(defun consult-line-symbol-at-point ()
  "Search for a symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun consult-line-from-isearch ()
  "Search by lines from isearch string."
  (interactive)
  (consult-line (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))

(defun consult--recent-file-read ()
  "Read recent file via `completing-read'."
  (consult--read
   "Find recent file: "
   (or (mapcar #'abbreviate-file-name recentf-list) (user-error "No recent files"))
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

(defun consult--yank-read ()
  "Open kill ring menu and return selected text."
  (consult--read
   "Yank text: "
   (delete-dups (seq-copy kill-ring))
   :sort nil
   :category 'kill-ring
   :require-match t
   :preview (and consult-preview-yank
                 (let* ((pt (point))
                        ;; If previous command is yank, hide previously yanked text
                        (mk (or (and (eq last-command 'yank) (mark t)) pt))
                        (ov (make-overlay (min pt mk) (max pt mk))))
                   (overlay-put ov 'invisible t)
                   (lambda (cmd cand _state)
                     (pcase cmd
                       ('restore (delete-overlay ov))
                       ('preview
                        ;; Use `add-face-text-property' on a copy of "cand in order to merge face properties
                        (setq cand (copy-sequence cand))
                        (add-face-text-property 0 (length cand) 'consult-preview-yank t cand)
                        ;; Use the `before-string' property since the overlay might be empty.
                        (overlay-put ov 'before-string cand))))))))

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
    (consult--read "Register: "
                   (consult--register-candidates)
                   :category 'register
                   :sort nil
                   :require-match t
                   :lookup #'consult--lookup-list
                   :history 'consult-register-history)))
  (condition-case nil
      (jump-to-register reg)
    (error (insert-register reg))))

;;;###autoload
(defun consult-bookmark (name)
  "If bookmark NAME exists, open it, otherwise set bookmark under the given NAME."
  (interactive (list (consult--read "Bookmark: " (bookmark-all-names)
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
         (consult--read "Apropos: "
                        obarray
                        :predicate (lambda (x) (or (fboundp x) (boundp x) (facep x) (symbol-plist x)))
                        :history 'consult-apropos-history
                        :category 'symbol
                        :default (thing-at-point 'symbol))))
    (if (string= pattern "")
        (user-error "No pattern given")
      (apropos pattern))))

;;;###autoload
(defun consult-command-history ()
  "Select and evaluate command from the command history."
  (interactive)
  (eval (read (consult--read
               "Command: "
               (or (delete-dups (mapcar #'prin1-to-string command-history))
                   (user-error "History is empty"))
               :sort nil
               :category 'expression))))

(defun consult--current-history ()
  "Return the history relevant to the current buffer.

If the minibuffer is active, returns the minibuffer history,
otherwise the history corresponding to the mode is returned.
There is a special case for `repeat-complex-command',
for which the command history is used."
  (cond
   ;; If pressing "C-x M-:", i.e., `repeat-complex-command',
   ;; we are instead querying the `command-history' and get a full s-expression.
   ((eq last-command 'repeat-complex-command)
    (delete-dups (mapcar #'prin1-to-string command-history)))
   ;; In the minibuffer we use the current minibuffer history,
   ;; which can be configured by setting `minibuffer-history-variable'.
   ((minibufferp)
    (if (fboundp 'minibuffer-history-value)
        (minibuffer-history-value) ;; Emacs 27 only
      (symbol-value minibuffer-history-variable)))
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

(defun consult--history-elements (history)
  "Return elements from HISTORY.
Can handle lists and rings."
  (delete-dups (seq-copy (if (ring-p history)
                             (ring-elements history)
                           history))))

;; This command has been adopted from https://github.com/oantolin/completing-history/.
;;;###autoload
(defun consult-history (&optional history)
  "Insert string from buffer HISTORY."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (str (consult--read "History: "
                             (or (consult--history-elements
                                  (or history (consult--current-history)))
                                 (user-error "History is empty"))
                             :sort nil)))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (insert (substring-no-properties str))))

;;;###autoload
(defun consult-minor-mode-menu ()
  "Enable or disable minor mode.
This is an alternative to `minor-mode-menu-from-indicator'."
  (interactive)
  (let ((mode (consult--read "Minor mode: "
                             ;; Taken from describe-minor-mode
		             (nconc
		              (describe-minor-mode-completion-table-for-symbol)
		              (describe-minor-mode-completion-table-for-indicator))
                             :require-match t
                             :history 'consult-minor-mode-menu-history)))
    (call-interactively (or (lookup-minor-mode-from-indicator mode)
                            (intern mode)))))

;;;###autoload
(defun consult-theme (theme)
  "Disable current themes and enable THEME from `consult-themes'.

During theme selection the theme is shown as
preview if `consult-preview-mode' is enabled."
  (interactive
   (list
    (let ((avail-themes (seq-filter (lambda (x) (or (not consult-themes)
                                                    (memq x consult-themes)))
                                    (cons nil (custom-available-themes)))))
      (consult--read
       "Theme: "
       (mapcar (lambda (x) (or x 'default)) avail-themes)
       :require-match t
       :category 'theme
       :history 'consult-theme-history
       :lookup (lambda (_ x)
                 (and x (not (string= x "default")) (intern-soft x)))
       :preview (and consult-preview-theme
                     (lambda (cmd cand state)
                       (pcase cmd
                         ('save (car custom-enabled-themes))
                         ('restore (consult-theme state))
                         ('preview (when (memq cand avail-themes)
                                     (consult-theme cand))))))
       :default (symbol-name (or (car custom-enabled-themes) 'default))))))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

(defsubst consult--buffer-candidate (prefix cand face)
  "Format virtual buffer candidate.

CAND is the candidate string.
PREFIX is the prefix string for narrowing.
FACE is the face for the candidate."
   (consult--narrow-candidate prefix (propertize cand 'face face)))

(defun consult--buffer (open-buffer open-file open-bookmark)
  "Backend implementation of `consult-buffer'.
Depending on the selected item OPEN-BUFFER, OPEN-FILE or OPEN-BOOKMARK will be used to display the item."
  (let* ((buf-file-hash (let ((ht (make-hash-table)))
                          (dolist (buf (buffer-list))
                            (when-let (file (buffer-file-name buf))
                              (puthash file t ht)))
                          ht))
         (curr-buf (buffer-name))
         ;; TODO right now we only show visible buffers.
         ;; This is a regression in contrast to the old dynamic narrowing implementation
         ;; and a regression to the default switch-to-buffer implementation.
         (bufs (mapcar
                (lambda (x)
                  (consult--buffer-candidate ?b x 'consult-buffer))
                (append
                 (seq-remove
                  ;; Visible buffers only
                  (lambda (x) (or (string= x curr-buf) (= (elt x 0) 32)))
                  (mapcar #'buffer-name (buffer-list)))
                 (list curr-buf))))
         (views (when consult-view-list-function
                  (mapcar (lambda (x)
                            (consult--buffer-candidate ?v x 'consult-view))
                          (funcall consult-view-list-function))))
         (bookmarks (mapcar (lambda (x)
                              (consult--buffer-candidate ?m (car x) 'consult-bookmark))
                            bookmark-alist))
         (files (mapcar (lambda (x)
                          (consult--buffer-candidate ?f (abbreviate-file-name x) 'consult-file))
                        (seq-remove (lambda (x) (gethash x buf-file-hash)) recentf-list)))
         (selected
          (consult--read
           "Switch to: " (append bufs files views bookmarks)
           :history 'consult-buffer-history
           :sort nil
           :narrow `((?b . "Buffer")
                     (?f . "File")
                     (?m . "Bookmark")
                     ,@(when consult-view-list-function '((?v . "View"))))
           :category 'virtual-buffer
           :lookup
           (lambda (candidates cand)
             (if (member cand candidates)
                 (cons (pcase (elt cand 0)
                         (?b open-buffer)
                         (?m open-bookmark)
                         (?v consult-view-open-function)
                         (?f open-file))
                       (consult--narrow-strip cand))
               ;; When candidate is not found in the alist,
               ;; default to creating a new buffer.
               (and (not (string-blank-p cand)) (cons open-buffer cand))))
           :preview
           (and consult-preview-buffer
                (lambda (cmd cand state)
                  (pcase cmd
                    ('save (current-buffer))
                    ('restore (when (buffer-live-p state)
                                (set-buffer state)))
                    ('preview
                     ;; In order to avoid slowness and unnecessary complexity, we
                     ;; only preview buffers. Loading recent files, bookmarks or
                     ;; views can result in expensive operations.
                     (when (and (eq (car cand) open-buffer) (get-buffer (cdr cand)))
                       (funcall open-buffer (cdr cand) 'norecord)))))))))
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
  (seq-uniq
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
   ;; Remove duplicates
   (lambda (x y) (equal (car x) (car y)))))

;;;###autoload
(defun consult-kmacro (arg)
  "Run a chosen keyboard macro.  With prefix ARG, run the macro that many times.

Macros containing mouse clicks aren't displayed."
  (interactive "p")
  (let ((selected (consult--read
                   "Keyboard macro: "
                   (or (consult--kmacro-candidates) (user-error "No keyboard macros defined"))
                   :category 'kmacro
                   :require-match t
                   :sort nil
                   :history 'consult-kmacro-history
                   :lookup #'consult--lookup-list)))
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
    ;; Functions appear at the top-level for emacs-lisp-mode. Fix this!
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((fns (seq-remove (lambda (x) (listp (cdr x))) items))
            (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
        (setq items (append rest (list (cons "Functions" fns))))))
    ;; Narrowing support
    (when-let (narrow (consult--imenu-narrow))
      (dolist (x items)
        (when-let (n (seq-find (lambda (n)
                                 (string-prefix-p (car x) (concat (cdr n) " ")))
                               narrow))
          (setcar x (consult--narrow-candidate (car n) (car x))))))
    (seq-sort-by #'car #'string<
                 (consult--imenu-flatten nil items))))

(defun consult--imenu-narrow ()
  "Return narrowing list for imenu."
  (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-narrow)))

;;;###autoload
(defun consult-imenu ()
  "Choose from flattened `imenu' using `completing-read'."
  (interactive)
  (imenu
   (consult--read
    "Go to item: "
    (or (consult--imenu-candidates) (user-error "Imenu is empty"))
    :preview (and consult-preview-imenu
                  (let ((preview (consult--preview-position)))
                    (lambda (cmd cand state)
                      (if (eq cmd 'preview)
                          ;; Only handle imenu items which are markers for preview,
                          ;; in order to avoid any bad side effects.
                          (when (and (consp cand) (markerp (cdr cand)))
                            (funcall preview cmd (cdr cand) state))
                        (funcall preview cmd cand state)))))
    :require-match t
    :narrow (consult--imenu-narrow)
    :category 'imenu
    :lookup #'consult--lookup-list
    :history 'consult-imenu-history
    :sort nil))
  (run-hooks 'consult-after-jump-hook))

;;;###autoload
(define-minor-mode consult-preview-mode
  "Enable preview for consult commands."
  :global t
  :group 'consult-preview)

;;;; default completion-system support for preview

(defun consult--default-preview-update (&rest _)
  "Preview function used for the default completion system."
  (when consult--preview-function
    (let ((cand (minibuffer-contents-no-properties)))
      (when (test-completion cand
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
        (funcall consult--preview-function cand)))))

(defun consult--default-preview-hook ()
  "Add preview update to `after-change-functions' if the default completion system is active."
  ;; Check if the default completion-system is active, by looking
  ;; at `completing-read-function' and `icomplete-mode'.
  (when (and (not icomplete-mode) (eq completing-read-function #'completing-read-default))
    (add-hook 'after-change-functions #'consult--default-preview-update nil t)))

(defun consult--default-preview-setup ()
  "Setup preview support for the default completion-system."
  (remove-hook 'minibuffer-setup-hook #'consult--default-preview-hook)
  (when consult-preview-mode
    (add-hook 'minibuffer-setup-hook #'consult--default-preview-hook)))

(add-hook 'consult-preview-mode-hook #'consult--default-preview-setup)

;;;; icomplete support for preview

(defun consult--icomplete-preview-update ()
  "Preview function used for Icomplete."
  (when consult--preview-function
    (when-let (cand (car completion-all-sorted-completions))
      (funcall consult--preview-function cand))))

(defun consult--icomplete-preview-setup ()
  "Setup preview support for Icomplete."
  (advice-remove 'icomplete-post-command-hook #'consult--icomplete-preview-update)
  (when consult-preview-mode
    (advice-add 'icomplete-post-command-hook :after #'consult--icomplete-preview-update)))

(add-hook 'consult-preview-mode-hook #'consult--icomplete-preview-setup)

(provide 'consult)
;;; consult.el ends here
