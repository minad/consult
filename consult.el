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
;; from a list of candidates. Most provided commands follow the naming scheme `consult-thing`.
;; Some commands are drop-in replacements for existing functions, e.g., consult-apropos.
;; Other commands provide additional non-existing functionality, e.g., consult-line.

;;; This package is inspired by and partially derived from counsel by Oleh Krehel,
;;; Copyright Free Software Foundation, Inc.
;;; Furthermore some of the commands found in this package were taken from the Selectrum wiki.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'kmacro)
(require 'outline)
(require 'recentf)
(require 'seq)
(require 'subr-x)

(defgroup consult nil
  "Consultation using `completing-read'."
  :group 'convenience
  :prefix "consult-")

;;;; Faces

(defface consult-preview-line
  '((t :inherit region))
  "Face used to for line previews."
  :group 'consult)

(defface consult-preview-cursor
  '((t :inherit match))
  "Face used to for cursor previews and marks in `cursor-mark'."
  :group 'consult)

(defface consult-key
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight keys, e.g., in `consult-register'."
  :group 'consult)

(defface consult-lighter
  '((t :inherit consult-key))
  "Face used to highlight lighters in `consult-minor-mode'."
  :group 'consult)

(defface consult-annotation
  '((t :inherit completions-annotations :weight normal))
  "Face used to highlight annotation in `consult-buffer'."
  :group 'consult)

(defface consult-file
  '((t :inherit font-lock-function-name-face :weight normal))
  "Face used to highlight files in `consult-buffer'."
  :group 'consult)

(defface consult-bookmark
  '((t :inherit font-lock-constant-face :weight normal))
  "Face used to highlight bookmarks in `consult-buffer'."
  :group 'consult)

(defface consult-view
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight views in `consult-buffer'."
  :group 'consult)

(defface consult-line-number
  '((t :inherit line-number :weight normal))
  "Face used to highlight line numbers in selections."
  :group 'consult)

(defface consult-on
  '((t :inherit success :weight bold))
  "Face used to signal enabled modes."
  :group 'consult)

(defface consult-off
  '((t :inherit error :weight bold))
  "Face used to signal disabled modes."
  :group 'consult)

;;;; Customization

(defcustom consult-preview-buffer t
  "Enable buffer preview during selection."
  :type 'boolean
  :group 'consult)

(defcustom consult-preview-theme t
  "Enable theme preview during selection."
  :type 'boolean
  :group 'consult)

(defcustom consult-preview-mark t
  "Enable mark preview during selection."
  :type 'boolean
  :group 'consult)

(defcustom consult-preview-line t
  "Enable line preview during selection."
  :type 'boolean
  :group 'consult)

(defcustom consult-preview-outline t
  "Enable outline preview during selection."
  :type 'boolean
  :group 'consult)

(defcustom consult-themes nil
  "List of themes to be presented for selection.
nil shows all `custom-available-themes'."
  :type '(repeat symbol)
  :group 'consult)

(defcustom consult-line-numbers-widen t
  "Show absolute line numbers when narrowing is active."
  :type 'boolean
  :group 'consult)

(defcustom consult-fontify-limit 1048576
  "Buffers larger than this limit are not fontified."
  :type 'integer
  :group 'consult)

;;;; History variables

(defvar-local consult-outline-history nil
  "Buffer-local history for the command `consult-outline'.")

(defvar-local consult-mark-history nil
  "Buffer-local history for the command `consult-mark'.")

(defvar-local consult-line-history nil
  "Buffer-local history for the command `consult-line'.")

(defvar consult-buffer-history nil
  "History for the command `consult-buffer'.")

(defvar consult-apropos-history nil
  "History for the command `consult-apropos'.")

(defvar consult-minibuffer-history nil
  "History for the command `consult-minibuffer-history'.")

(defvar consult-command-history nil
  "History for the command `consult-command-history'.")

(defvar consult-register-history nil
  "History for the command `consult-register'.")

(defvar consult-theme-history nil
  "History for the command `consult-theme'.")

(defvar consult-minor-mode-history nil
  "History for the command `consult-minor-mode'.")

(defvar consult-kmacro-history nil
  "History for the command `consult-kmacro'.")

;;;; Internal variables

(defvar consult--gc-threshold 67108864
  "Large gc threshold for temporary increase.")

(defvar consult--gc-percentage 0.5
  "Large gc percentage for temporary increase.")

(defvar consult--preview-stack nil
  "Stack of active preview functions.")

(defvar-local consult--overlays nil
  "List of overlays used by consult.")

;;;; Pre-declarations for external packages

(defvar selectrum-should-sort-p)
(declare-function selectrum-read "selectrum")
(declare-function selectrum-get-current-candidate "selectrum")

;;;; Helper functions

(defsubst consult--fontify ()
  "Ensure that the whole buffer is fontified."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line. Therefore we have to enforce
  ;; font-locking now, which is slow. In order to prevent is hang-up we check the buffer size
  ;; against `consult-fontify-limit'.
  (when (and font-lock-mode (< (buffer-size) consult-fontify-limit))
    (font-lock-ensure)))

(defun consult--status-prefix (enabled)
  "Status prefix for given boolean ENABLED."
  (propertize " " 'display
              (if enabled
                  (propertize "+ " 'face 'consult-on)
                (propertize "- " 'face 'consult-off))))

(defmacro consult--with-preview (enabled args save restore preview &rest body)
  "Add preview support to minibuffer completion.

The preview will only be enabled if `consult-preview-mode' is active.
This function should not be used directly, use `consult--read' instead.
ENABLED must be t to enable preview.
ARGS are the argument variables (cand state).
SAVE is an expression which returns state to save before preview.
RESTORE is an expression which restores the state.
PREVIEW is an expresion which previews the candidate.
BODY are the body expressions."
  (declare (indent 5))
  `(if ,enabled
       (let ((,(car args))
             (,@(cdr args) ,save))
         (ignore ,@args) ;; Disable unused variable warnings
         (push (lambda (,(car args)) ,preview) consult--preview-stack)
         (unwind-protect
             (setq ,(car args) ,(if (cdr body) `(progn ,@body) (car body)))
           (pop consult--preview-stack)
           ,restore))
     ,@body))

(defmacro consult--with-increased-gc (&rest body)
  "Temporarily increase the gc limit in BODY to optimize for throughput."
  `(let* ((overwrite (> consult--gc-threshold gc-cons-threshold))
          (gc-cons-threshold (if overwrite consult--gc-threshold gc-cons-threshold))
          (gc-cons-percentage (if overwrite consult--gc-percentage gc-cons-percentage)))
     ,@body))

(defmacro consult--with-window (&rest body)
  "Run BODY with current live window."
  `(with-selected-window
       (or (minibuffer-selected-window) (selected-window))
     ,@body))

(defun consult--overlay-add (beg end face)
  "Make consult overlay between BEG and END with FACE."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (push ov consult--overlays)))

(defun consult--overlay-cleanup ()
  "Remove all consult overlays."
  (mapc #'delete-overlay consult--overlays)
  (setq consult--overlays nil))

;; TODO Matched strings are not highlighted as of now
;; see https://github.com/minad/consult/issues/7
(defun consult--preview-line (cmd &optional cand _state)
  "The preview function used if selected from a list of candidate lines.

The function can be used as the `:preview' argument of `consult--read'.
CMD is the preview command.
CAND is the selected candidate.
_STATE is the saved state."
  (pcase cmd
    ('restore
     (consult--overlay-cleanup))
    ('preview
     (consult--with-window
      (goto-char cand)
      (recenter)
      (consult--overlay-cleanup)
      (consult--overlay-add (line-beginning-position) (line-end-position) 'consult-preview-line)
      (let ((pos (point)))
        (consult--overlay-add pos (1+ pos) 'consult-preview-cursor))))))

;; HACK: Hopefully selectrum adds something like this to the official API.
;; https://github.com/raxod502/selectrum/issues/243
;; https://github.com/raxod502/selectrum/pull/244
(defun consult--selectrum-config (options)
  "Add OPTIONS to the next `selectrum-read' call."
  (when (and options (bound-and-true-p selectrum-mode))
    (letrec ((advice (lambda (orig prompt candidates &rest args)
                       (advice-remove #'selectrum-read advice)
                       (apply orig prompt candidates (append options args)))))
      (advice-add #'selectrum-read :around advice))))

(cl-defun consult--read (prompt candidates &key
                                predicate require-match history default
                                category initial preview
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
PREVIEW is a preview function."
  ;; supported types
  (cl-assert (or (not candidates) ;; nil
                 (obarrayp candidates) ;; obarray
                 (stringp (car candidates)) ;; string list
                 (consp (car candidates)))) ;; alist
  ;; alists can only be used if require-match=t
  (cl-assert (or (not (and (consp candidates) (consp (car candidates)))) require-match))
  ;; HACK: We are explicitly injecting the default input, since default inputs are deprecated
  ;; in the completing-read API. Selectrum's completing-read consequently does not support
  ;; them. Maybe Selectrum should add support for initial inputs, even if this is deprecated
  ;; since the argument does not seem to go away any time soon. There are a few special cases
  ;; where one wants to use an initial input, even though it should not be overused and the use
  ;; of initial inputs is discouraged by the Emacs documentation.
  (consult--selectrum-config
   `(,@(unless default-top '(:no-move-default-candidate t))
     ,@(when initial `(:initial-input ,initial))))
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
    (consult--with-preview preview
        (cand state)
        (funcall preview 'save)
        (funcall preview 'restore cand state)
        (when-let (cand (funcall lookup candidates cand))
          (funcall preview 'preview cand))
      (funcall
       lookup candidates
       (completing-read prompt candidates-fun
                        predicate require-match initial history default)))))

(defsubst consult--pad-line-number (width line)
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
               (consult--pad-line-number width (caar cand))
               " "
               (cdar cand))))
    candidates))

(defun consult--goto (pos)
  "Push current position to mark ring, go to POS and recenter."
  (when pos
    (push-mark (point) t)
    (goto-char pos)
    (recenter)))

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
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer"))
  (consult--fontify)
  (let* ((max-line 0)
         (line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (heading-regexp (concat "^\\(?:" outline-regexp "\\)"))
         (unformatted-candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil 'move))
        (let ((match-pos (match-beginning 0)))
          (while (< (point) match-pos)
            (setq line (1+ line))
            (forward-line 1))
          (goto-char match-pos))
        (setq max-line (max line max-line))
        (push (cons
               (cons
                line
                (buffer-substring (line-beginning-position) (line-end-position)))
               (point-marker))
              unformatted-candidates)
        (if (and (bolp) (not (eobp))) (forward-char 1))))
    (or (consult--add-line-number max-line (nreverse unformatted-candidates))
        (user-error "No headings"))))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading."
  (interactive)
  (consult--goto
   (save-excursion
     (consult--read "Go to heading: " (consult--with-increased-gc (consult--outline-candidates))
                    :category 'line
                    :sort nil
                    :require-match t
                    :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                    :history 'consult-outline-history
                    :preview (and consult-preview-outline #'consult--preview-line)))))

(defun consult--mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer"))
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
            (let* ((col  (current-column))
                   ;; `line-number-at-pos' is a very slow function, which should be replaced everywhere.
                   ;; However in this case the slow line-number-at-pos does not hurt much, since
                   ;; the mark ring is usually small since it is limited by `mark-ring-max'.
                   (line (line-number-at-pos pos consult-line-numbers-widen))
                   (lstr (buffer-substring (- pos col) (line-end-position)))
                   (end (1+ col))
                   (cand (if (> end (length lstr))
                             (concat (substring lstr 0 col)
                                     (propertize " " 'face 'consult-preview-cursor))
                           (concat (substring lstr 0 col)
                                   (propertize (substring lstr col end) 'face 'consult-preview-cursor)
                                   (substring lstr end)))))
              (setq max-line (max line max-line))
              (push (cons (cons line cand) marker) unformatted-candidates))))))
    (consult--add-line-number max-line unformatted-candidates)))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in `mark-ring'."
  (interactive)
  (consult--goto
   (save-excursion
     (consult--read "Go to mark: " (consult--with-increased-gc (consult--mark-candidates))
                    :category 'mark
                    :sort nil
                    :require-match t
                    :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                    :history 'consult-mark-history
                    :preview (and consult-preview-mark #'consult--preview-line)))))

;; HACK: Disambiguate the line by prepending it with unicode
;; characters in the supplementary private use plane b.
;; This will certainly have many ugly consequences.
(defsubst consult--line-prefix (width line)
  "Generate unique line number prefix string for LINE.
WIDTH is the line number width."
  (let ((unique-prefix "") (n line))
    (while (progn
             (setq unique-prefix (concat (string (+ #x100000 (% n #xFFFE))) unique-prefix))
             (and (>= n #xFFFE) (setq n (/ n #xFFFE)))))
    (propertize unique-prefix 'display (consult--pad-line-number width line))))

(defun consult--line-candidates ()
  "Return alist of lines and positions."
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer"))
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
            (let ((cand (concat (consult--line-prefix line-width line) str))
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
  (consult--goto
   (let ((candidates (consult--with-increased-gc (consult--line-candidates))))
     (save-excursion
       (consult--read "Go to line: " (cdr candidates)
                      :category 'line
                      :sort nil
                      :default-top nil
                      :require-match t
                      :history 'consult-line-history
                      :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                      :default (car candidates)
                      :initial initial
                      :preview (and consult-preview-line #'consult--preview-line))))))

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
(defun consult-open-externally (file)
  "Open FILE using system's default application."
  (interactive "fOpen: ")
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
                    (read-file-name "Completion: "
                                    (file-name-directory initial)
                                    initial t
                                    (file-name-nondirectory initial)
                                    predicate)
                  (completing-read
                   "Completion: " collection predicate t initial)))))))
    (if (null completion)
        (progn (message "No completion") nil)
      (delete-region start end)
      (insert (substring-no-properties completion))
      (when-let ((exit (plist-get completion-extra-properties :exit-function)))
        (funcall exit completion exit-status))
      t)))

;; TODO consult--yank-read should support preview
;; see https://github.com/minad/consult/issues/8
(defun consult--yank-read ()
  "Open kill ring menu and return selected text."
  (consult--read "Ring: "
                 (delete-dups (seq-copy kill-ring))
                 :require-match t))

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
          (before (< (point) (mark t))))
      (setq this-command 'yank)
      (if before
          (funcall (or yank-undo-function 'delete-region) (point) (mark t))
        (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (insert-for-yank text)
      (set-window-start (selected-window) yank-window-start t)
      (if before
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
                   :lookup (lambda (candidates x) (cdr (assoc x candidates)))
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
  (eval
   (read
    (consult--read "Command: "
                   (delete-dups (mapcar #'prin1-to-string command-history))
                   :category 'expression
                   :history 'consult-command-history))))

;;;###autoload
(defun consult-minibuffer-history ()
  "Insert string from minibuffer history."
  (interactive)
  (insert
   (substring-no-properties
    (consult--read "Minibuffer: "
                   (delete-dups (seq-copy minibuffer-history))
                   :history 'consult-minibuffer-history))))

(defun consult--minor-mode-candidates ()
  "Return alist of minor-mode names and symbols."
  (let ((candidates-alist))
    (dolist (mode minor-mode-list)
      (when (and (boundp mode) (commandp mode))
        (push (cons (concat
                     (consult--status-prefix (symbol-value mode))
                     (symbol-name mode)
                     (let* ((lighter (cdr (assq mode minor-mode-alist)))
                            (str (and lighter (propertize (string-trim (format-mode-line (cons t lighter)))
                                                          'face 'consult-lighter))))
                       (and str (not (string-blank-p str)) (format " [%s]" str)))
                     ;; TODO it would be a bit nicer if marginalia could do this automatically based on the command
                     ;; category. but since the candidate is already prefixed with a string this is not possible.
                     ;; See https://github.com/minad/marginalia/issues/13.
                     (when (and (bound-and-true-p marginalia-mode)
                                (fboundp 'marginalia--documentation))
                       (marginalia--documentation (documentation mode))))
                    mode)
              candidates-alist)))
    (sort
     (sort candidates-alist (lambda (x y) (string< (car x) (car y))))
     (lambda (x y)
       (> (if (symbol-value (cdr x)) 1 0)
          (if (symbol-value (cdr y)) 1 0))))))

;;;###autoload
(defun consult-minor-mode ()
  "Enable or disable minor mode."
  (interactive)
  (call-interactively
   (consult--read "Minor modes: " (consult--minor-mode-candidates)
                  :category 'command
                  :sort nil
                  :require-match t
                  :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                  :history 'consult-minor-mode-history)))

;;;###autoload
(defun consult-theme (theme)
  "Disable current themes and enable THEME from `consult-themes'.

During theme selection the theme is shown as
preview if `consult-preview-mode' is enabled."
  (interactive
   (list
    (let ((avail-themes (custom-available-themes)))
      (consult--read
       "Theme: "
       (mapcar #'symbol-name
               (seq-filter (lambda (x) (or (not consult-themes)
                                           (memq x consult-themes)))
                           avail-themes))
       :require-match t
       :category 'theme
       :history 'consult-theme-history
       :lookup (lambda (_ x) (and x (intern x)))
       :preview (and consult-preview-theme
                     (lambda (cmd &optional cand state)
                       (pcase cmd
                         ('save (car custom-enabled-themes))
                         ('restore
                          (unless cand
                            (consult-theme state)))
                         ('preview
                          (when (memq cand avail-themes)
                            (consult-theme cand))))))
       :default (and (car custom-enabled-themes)
                     (symbol-name (car custom-enabled-themes)))))))
  (unless (equal theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;; TODO consult--buffer-selectrum performs dynamic computation of the candidate set.
;; this is currently not supported by completing-read+selectrum.
;; therefore the selectrum api is used directly.
;; consult-buffer with selectrum supports prefixes for narrowing b, f, m, v
;; see discussion https://github.com/raxod502/selectrum/issues/235#issuecomment-734835414
;; see also https://github.com/minad/consult/issues/10
(defun consult--buffer-selectrum (open-buffer hidden-bufs visible-bufs files views bookmarks)
  "Select virtual buffer using `selectrum-read'.
HIDDEN-BUFS, VISIBLE-BUFS, FILES, VIEWS and BOOKMARKS are the candidate lists.
OPEN-BUFFER is used for preview."
  (let* ((all-cands (append visible-bufs files bookmarks))
         (generate
          (lambda (input)
            (cond
             ((string-prefix-p " " input)
              (list (cons 'input (substring input 1))
                    (cons 'candidates hidden-bufs)))
             ((string-prefix-p "b " input)
              (list (cons 'input (substring input 2))
                    (cons 'candidates visible-bufs)))
             ((string-prefix-p "f " input)
              (list (cons 'input (substring input 2))
                    (cons 'candidates files)))
             ((and views (string-prefix-p "v " input)) ;; Only narrow if there are views
              (list (cons 'input (substring input 2))
                    (cons 'candidates views)))
             ((and bookmarks (string-prefix-p "m " input)) ;; Only narrow if there are bookmarks
              (list (cons 'input (substring input 2))
                    (cons 'candidates bookmarks)))
             (t
              (list (cons 'input input)
                    (cons 'candidates all-cands)))))))
    ;; TODO preview of virtual buffers is not implemented yet
    ;; see https://github.com/minad/consult/issues/9
    (consult--with-preview consult-preview-buffer
        (buf state)
        (current-window-configuration)
        (set-window-configuration state)
        (when (get-buffer buf)
          (consult--with-window
           (funcall open-buffer buf)))
      (minibuffer-with-setup-hook
          (lambda () (setq-local selectrum-should-sort-p nil))
        (selectrum-read "Switch to: " generate
                        :history 'consult-buffer-history)))))

;; TODO consult--buffer-default does not support prefixes
;; for narrowing like the Selectrum variant!
;; see discussion https://github.com/raxod502/selectrum/issues/235#issuecomment-734835414
(defun consult--buffer-default (open-buffer candidates)
  "Select virtual buffer from a list of CANDIDATES using `completing-read'.
OPEN-BUFFER is used for preview."
  (consult--read "Switch to: " candidates
                 :history 'consult-buffer-history
                 :sort nil
                 :preview (lambda (cmd &optional cand _state)
                            (pcase cmd
                              ('preview
                               (when (get-buffer cand)
                                 (consult--with-window
                                  (funcall open-buffer cand))))))))

(defun consult--buffer (open-buffer open-file open-bookmark)
  "Backend implementation of `consult-buffer'.
Depending on the selected item OPEN-BUFFER, OPEN-FILE or OPEN-BOOKMARK will be used to display the item."
  (let* ((curr-buf (window-buffer (minibuffer-selected-window)))
         (curr-file (or (buffer-file-name curr-buf) ""))
         (all-bufs (mapcar #'buffer-name (delq curr-buf (buffer-list))))
         (hidden-bufs (seq-filter (lambda (x) (= (aref x 0) 32)) all-bufs))
         (visible-bufs (seq-filter (lambda (x) (/= (aref x 0) 32)) all-bufs))
         ;; TODO implement a solution to allow registration of custom virtual buffers.
         ;; Alternatively just hard-code other view libraries like perspective etc?
         ;; Right now only bookmarks-view is supported.
         ;; https://github.com/minad/bookmark-view/blob/master/bookmark-view.el
         (views (if (fboundp 'bookmark-view-names)
                    (mapcar (lambda (x)
                              (propertize x
                                          'face 'consult-view
                                          ;; TODO remove selectrum specifics if possible?
                                          'selectrum-candidate-display-right-margin
                                          (propertize " View" 'face 'consult-annotation)))
                            (bookmark-view-names))))
         (bookmarks (mapcar (lambda (x)
                              (propertize (car x)
                                          'face 'consult-bookmark
                                          ;; TODO remove selectrum specifics if possible?
                                          'selectrum-candidate-display-right-margin
                                          (propertize " Bookmark" 'face 'consult-annotation)))
                            bookmark-alist))
         (all-files (mapcar (lambda (x)
                              (propertize (abbreviate-file-name x)
                                          'face 'consult-file
                                          ;; TODO remove selectrum specifics if possible?
                                          'selectrum-candidate-display-right-margin
                                          (propertize " File" 'face 'consult-annotation)))
                            recentf-list))
         (files (remove curr-file all-files))
         (selected (if (bound-and-true-p selectrum-mode)
                       (consult--buffer-selectrum open-buffer hidden-bufs visible-bufs files views bookmarks)
                     (consult--buffer-default open-buffer (append visible-bufs files bookmarks)))))
    (cond
     ((get-buffer selected) (funcall open-buffer selected)) ;; buffers have priority
     ((member selected all-files) (funcall open-file selected))
     ((member selected bookmarks) (funcall open-bookmark selected))
     ((and selected (not (string= "" selected))) (funcall open-buffer selected)))))

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
  "Enhanced `switch-to-buffer-other-window' command with support for virtual buffers."
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
                 (propertize " "
                             'display
                             (format "%d(%s) " counter format))
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
                   :lookup (lambda (candidates x) (cdr (assoc x candidates))))))
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

;;;; consult-preview-mode - Enabling preview for consult commands

(defun consult--preview-update-selectrum ()
  "Preview function used for Selectrum."
  (when-let* ((fun (car consult--preview-stack))
              (cand (selectrum-get-current-candidate)))
    (funcall fun cand)))

(defun consult--preview-update-icomplete ()
  "Preview function used for Icomplete."
  (when-let* ((fun (car consult--preview-stack))
              (cand (car completion-all-sorted-completions)))
    (funcall fun cand)))

(defun consult--preview-update-default (&rest _)
  "Preview function used for the default completion system."
  (unless (or (bound-and-true-p selectrum-mode)
              (bound-and-true-p icomplete-mode))
    (when-let (fun (car consult--preview-stack))
      (let ((cand (minibuffer-contents)))
        (when (test-completion cand
                               minibuffer-completion-table
                               minibuffer-completion-predicate)
          (funcall fun cand))))))

;; this function contains completion-system specifics, since there is no general mechanism of
;; completion systems to get the current candidate.
;;;###autoload
(define-minor-mode consult-preview-mode
  "Enable preview for consult commands."
  :global t

  ;; Reset first to get a clean slate.
  (advice-remove 'selectrum--minibuffer-post-command-hook #'consult--preview-update-selectrum)
  (advice-remove 'icomplete-post-command-hook #'consult--preview-update-icomplete)
  (advice-remove #'minibuffer-complete #'consult--preview-update-default)
  (advice-remove #'minibuffer-complete-word #'consult--preview-update-default)
  (advice-remove #'minibuffer-completion-help #'consult--preview-update-default)

  ;; Now add our advices.
  (when consult-preview-mode
    ;; It is possible to advice functions which do not yet exist
    (advice-add 'selectrum--minibuffer-post-command-hook :after #'consult--preview-update-selectrum)
    (advice-add 'icomplete-post-command-hook :after #'consult--preview-update-icomplete)

    ;; TODO for default Emacs completion, I advise three functions. Is there a better way?
    (advice-add #'minibuffer-complete-word  :after #'consult--preview-update-default)
    (advice-add #'minibuffer-complete :after #'consult--preview-update-default)
    (advice-add #'minibuffer-completion-help  :after #'consult--preview-update-default)))

(provide 'consult)
;;; consult.el ends here
