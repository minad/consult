;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

;; Author: Daniel Mendler, Selectrum contributors
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

(defface consult-lighter
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight lighters in `consult-minor-mode'."
  :group 'consult)

(defface consult-key
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight keys in `consult-annotate-mode'."
  :group 'consult)

(defface consult-variable
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight variable values in `consult-annotate-mode'."
  :group 'consult)

(defface consult-annotation
  '((t :inherit completions-annotations :weight normal))
  "Face used to highlight documentation string in `consult-annotate-mode'."
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

(defcustom consult-annotation-width 80
  "Width of annotation string."
  :type 'integer
  :group 'consult)

;;;; History variables

(defvar consult-buffer-history nil
  "History for the command `consult-buffer'.")

(defvar consult-outline-history nil
  "History for the command `consult-outline'.")

(defvar consult-mark-history nil
  "History for the command `consult-mark'.")

(defvar consult-line-history nil
  "History for the command `consult-line'.")

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

;;;; Pre-declarations for external packages

(defvar icomplete-mode)
(declare-function icomplete-post-command-hook "icomplete")

(defvar selectrum-mode)
(defvar selectrum-should-sort-p)
(defvar selectrum--move-default-candidate-p)
(defvar selectrum-highlight-candidates-function)
(declare-function selectrum-read "selectrum")
(declare-function selectrum-get-current-candidate "selectrum")
(declare-function selectrum--minibuffer-post-command-hook "selectrum")

(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)
(declare-function package-desc-summary "package")
(declare-function package--from-builtin "package")

;;;; Helper functions

;; HACK until selectrum provides a better api
;; see https://github.com/minad/consult/issues/11
(defmacro consult--configure-minibuffer (config &rest body)
  "Set CONFIG in minibuffer opened by BODY."
  (declare (indent 1))
  `(minibuffer-with-setup-hook
       (lambda () (setq-local ,@config))
     ,@body))

(defun consult--truncate (str width)
  "Truncate string STR to WIDTH."
  (truncate-string-to-width (car (split-string str "\n")) width 0 32 "…"))

(defun consult--status-prefix (enabled)
  "Status prefix for given boolean ENABLED."
  (propertize " " 'display
              (if enabled
                  (propertize "+ " 'face 'consult-on)
                (propertize "- " 'face 'consult-off))))

;; TODO this function contains completion-system specifics
;; is there a more general mechanism which works everywhere or can this be cleaned up?
(defun consult--preview-setup (callback)
  "Begin preview by hooking into the completion system.
Returns a function which must be called at the end of the preview.
CALLBACK is called with the current candidate."
  (cond
   ;; TODO is there a better selectrum api to achieve this?
   ;; see https://github.com/raxod502/selectrum/issues/239
   ((bound-and-true-p selectrum-mode)
    (let ((advice (lambda ()
                    (when-let (cand (selectrum-get-current-candidate))
                      (funcall callback cand)))))
      (advice-add #'selectrum--minibuffer-post-command-hook :after advice)
      (lambda () (advice-remove #'selectrum--minibuffer-post-command-hook advice))))
   ;; TODO is icomplete-post-command-hook the right function to add the advice?
   ((bound-and-true-p icomplete-mode)
    (let ((advice (lambda ()
                    (when-let (cand (car completion-all-sorted-completions))
                      (funcall callback cand)))))
      (advice-add #'icomplete-post-command-hook :after advice)
      (lambda () (advice-remove #'icomplete-post-command-hook advice))))
   ;; TODO for default Emacs completion, I advise three functions. Is there a better way?
   (t
    (let ((advice (lambda (&rest _)
                    (let ((cand (minibuffer-contents)))
                      (when (test-completion cand
                                             minibuffer-completion-table
                                             minibuffer-completion-predicate)
                        (funcall callback cand))))))
      (advice-add #'minibuffer-complete-word  :after advice)
      (advice-add #'minibuffer-complete :after advice)
      (advice-add #'minibuffer-completion-help  :after advice)
      (lambda ()
        (advice-remove #'minibuffer-complete advice)
        (advice-remove #'minibuffer-complete-word advice)
        (advice-remove #'minibuffer-completion-help advice))))))

(defmacro consult--preview (enabled save restore preview body)
  "Preview support for completion.
ENABLED must be t to enable preview.
SAVE is an expression which returns some state to save before preview.
RESTORE is a pair (variable . expression) which restores the state.
PREVIEW is a pair (variable . expression) which previews the given candidate.
BODY is the body expression."
  (declare (indent 4))
  (let ((finalize (make-symbol "finalize")))
    `(if ,enabled
         (let ((,(car restore) ,save)
               (,finalize (consult--preview-setup (lambda (,(car preview)) ,@(cdr preview)))))
           (unwind-protect
               ,body
             (funcall ,finalize)
             ,@(cdr restore)))
       ,body)))

(defmacro consult--gc-increase (&rest body)
  "Temporarily increase the gc limit in BODY to optimize for throughput."
  `(let ((gc-cons-threshold (max gc-cons-threshold 67108864))
         (gc-cons-percentage 0.5))
     ,@body))

(defun consult--window ()
  "Return live window."
  (let ((win (minibuffer-selected-window)))
    (while (not (window-live-p win))
      (setq win (next-window)))
    win))

(defmacro consult--with-window (&rest body)
  "Run BODY with current live window."
  `(with-selected-window (consult--window) ,@body))

;; TODO Matched strings are not highlighted as of now
;; see https://github.com/minad/consult/issues/7
(defun consult--preview-line (cmd &optional arg)
  "Preview function for lines.
CMD is the preview command.
ARG is the command argument."
  (pcase cmd
    ('restore
     (remove-overlays nil nil 'consult-overlay t))
    ('preview
     (consult--with-window
      (goto-char arg)
      (recenter)
      (remove-overlays nil nil 'consult-overlay t)
      (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
        (overlay-put ov 'face 'consult-preview-line)
        (overlay-put ov 'consult-overlay t))
      (let* ((pos (point))
             (ov (make-overlay pos (1+ pos))))
        (overlay-put ov 'face 'consult-preview-cursor)
        (overlay-put ov 'consult-overlay t))))))

(cl-defun consult--read (prompt candidates &key
                                predicate require-match history default
                                category (sort t) (lookup (lambda (_ x) x)) preview)
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
PREVIEW is a preview function."
  ;; supported types
  (cl-assert (or (not candidates) ;; nil
                 (obarrayp candidates) ;; obarray
                 (stringp (car candidates)) ;; string list
                 (consp (car candidates)))) ;; alist
  ;; alists can only be used if require-match=t
  (cl-assert (or (not (and (consp candidates) (consp (car candidates)))) require-match))
  (consult--preview preview
      (funcall preview 'save)
      (state (funcall preview 'restore state))
      (cand (when-let (cand (funcall lookup candidates cand))
              (funcall preview 'preview cand)))
    (funcall lookup candidates
             (completing-read
              prompt
              (if (and sort (not category))
                  candidates
                (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        ,@(if category `((category . ,category)))
                        ,@(if (not sort) '((cycle-sort-function . identity)
                                           (display-sort-function . identity))))
                    (complete-with-action action candidates str pred))))
              predicate require-match nil history default))))

(defsubst consult--pad-line-number (width line)
  "Optimized formatting for LINE number with padding. WIDTH is the line number width."
  (setq line (number-to-string line))
  (propertize (concat
               (make-string (- width (length line)) 32)
               line
               " ")
              'face 'consult-line-number))

(defun consult--add-line-number (max-line candidates)
  "Add line numbers to unformatted CANDIDATES. The MAX-LINE is needed to determine the width."
  (let ((width (length (number-to-string max-line))))
    (dolist (cand candidates)
      ;; TODO use prefix here or keep the line number as part of string?
      ;; If we would use a prefix, the alist approach would not work for duplicate lines!
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
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  ;; TODO can this be optimized, at least add some progress message?
  (jit-lock-fontify-now)
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
     (consult--read "Go to heading: " (consult--gc-increase (consult--outline-candidates))
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
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  ;; TODO can this be optimized, at least add some progress message?
  (jit-lock-fontify-now)
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
                   ;; TODO line-number-at-pos is a very slow function, can this be replaced?
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
     (consult--read "Go to mark: " (consult--gc-increase (consult--mark-candidates))
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
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  ;; TODO can this be optimized, at least add some progress message?
  (jit-lock-fontify-now)
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
               (str (buffer-substring (line-beginning-position) end))
               (dist (abs (- curr-line line))))
          (unless (string-blank-p str)
            (let ((cand (concat (consult--line-prefix line-width line) str)))
              (when (or (not default-cand) (< dist default-cand-dist))
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
(defun consult-line ()
  "Search for a matching line and jump to the line beginning.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
  (interactive)
  (consult--goto
   (let ((candidates (consult--gc-increase (consult--line-candidates))))
     (save-excursion
       (consult--configure-minibuffer
           (selectrum--move-default-candidate-p nil)
         (consult--read "Go to line: " (cdr candidates)
                        :sort nil
                        :require-match t
                        :history 'consult-line-history
                        :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                        :default (car candidates)
                        :preview (and consult-preview-line #'consult--preview-line)))))))

(defun consult--recent-file-read ()
  "Read recent file via `completing-read'."
  (consult--read
   "Find recent file: "
   (or (mapcar #'abbreviate-file-name recentf-list) (user-error "No recent files"))
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
  (mapcar
   (lambda (r)
     (setq r (car r))
     (cons (format "%s: %s"
                   (single-key-description r)
                   (register-describe-oneline r))
           r))
   (or (sort (copy-sequence register-alist) #'car-less-than-car)
       (user-error "All registers are empty"))))

;;;###autoload
(defun consult-register (reg)
  "Use register REG. Either jump to location or insert the stored text."
  (interactive
   (list
    (consult--read "Register: "
                   (consult--register-candidates)
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
                   ;; :category 'command ;; TODO command category is wrong here? category "sexp"?
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
                       (and str (not (string-blank-p str)) (format " [%s]" str))))
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
                  :sort nil
                  :require-match t
                  :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                  :history 'consult-minor-mode-history)))

;;;###autoload
(defun consult-theme (theme)
  "Enable THEME from `consult-themes'."
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
                     (lambda (cmd &optional arg)
                       (pcase cmd
                         ('save (car custom-enabled-themes))
                         ('restore (consult-theme arg))
                         ('preview
                          (when (memq arg avail-themes)
                            (consult-theme arg))))))
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
    (consult--preview consult-preview-buffer
        (current-window-configuration)
        (state (set-window-configuration state))
        (buf (when (get-buffer buf)
               (consult--with-window
                (funcall open-buffer buf))))
      (consult--configure-minibuffer
          (selectrum-should-sort-p nil)
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
                 :preview (lambda (cmd &optional arg)
                            (pcase cmd
                              ('preview
                               (when (get-buffer arg)
                                 (consult--with-window
                                  (funcall open-buffer arg))))))))

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
                                          ;; TODO the consult-annotation face is ignored by selectrum?
                                          ;; see https://github.com/raxod502/selectrum/issues/236
                                          (propertize " View" 'face 'consult-annotation)))
                            (bookmark-view-names))))
         (bookmarks (mapcar (lambda (x)
                              (propertize (car x)
                                          'face 'consult-bookmark
                                          ;; TODO remove selectrum specifics if possible?
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the consult-annotation face is ignored by selectrum?
                                          ;; see https://github.com/raxod502/selectrum/issues/236
                                          (propertize " Bookmark" 'face 'consult-annotation)))
                            bookmark-alist))
         (all-files (mapcar (lambda (x)
                              (propertize (abbreviate-file-name x)
                                          'face 'consult-file
                                          ;; TODO remove selectrum specifics if possible?
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the consult-annotation face is ignored by selectrum?
                                          ;; see https://github.com/raxod502/selectrum/issues/236
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
  (consult--buffer #'switch-to-buffer-other-frame #'find-file-other-frame
                   ;; bookmark-jump-other-frame is supported on Emacs >= 27.1
                   ;; TODO which Emacs versions do we want to support?
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

;;;; consult-annotate-mode - Enhancing existing commands with annotations

(defcustom consult-annotate-alist
  '((execute-extended-command . consult-annotate-command-binding)
    (consult-apropos . consult-annotate-symbol)
    (describe-function . consult-annotate-symbol)
    (describe-variable . consult-annotate-variable)
    (describe-face . consult-annotate-face)
    (describe-symbol . consult-annotate-symbol)
    (helpful-callable . consult-annotate-symbol)
    (helpful-command . consult-annotate-symbol)
    (helpful-function . consult-annotate-symbol)
    (helpful-macro . consult-annotate-symbol)
    (helpful-symbol . consult-annotate-symbol)
    (helpful-variable . consult-annotate-variable)
    (describe-package . consult-annotate-package)
    (package-install . consult-annotate-package)
    (package-delete . consult-annotate-package)
    (package-reinstall . consult-annotate-package))
  "List of commands which should be enriched during completion.
The annotation function must return a string which is appended to the completion candidate.
Annotations are only shown if `consult-annotate-mode' is enabled."
  :type '(alist :key-type symbol :value-type function)
  :group 'consult)

(defvar consult--annotate-this-command nil
  "Last command symbol saved in order to allow annotations.")

(defvar consult--selectrum-highlight-candidates nil
  "Original highlighting function stored by `consult-annotate-mode'.")

(defun consult-annotate-command-binding (cand)
  "Annotate command CAND with keybinding."
  ;; Taken from Emacs 28, read-extended-command--annotation
  (when-let* ((binding (where-is-internal (intern cand) overriding-local-map t))
              (desc (and (not (stringp binding)) (key-description binding))))
    (propertize (format " (%s)" desc) 'face 'consult-key)))

(defun consult-annotate-command-full (cand)
  "Annotate command CAND with binding and documentation string."
  (concat
   (consult-annotate-command-binding cand)
   (consult-annotate-symbol cand)))

(defun consult--annotation (ann)
  "Format annotation string ANN."
  (concat " "
          (propertize
           " "
           'display
           '(space :align-to (- right-fringe consult-annotation-width)))
          (propertize (consult--truncate ann consult-annotation-width)
                      'face 'consult-annotation)))

(defun consult-annotate-symbol (cand)
  "Annotate symbol CAND with documentation string."
  (when-let (doc (let ((sym (intern cand)))
                   (cond
                    ((fboundp sym) (ignore-errors (documentation sym)))
                    ((facep sym) (documentation-property sym 'face-documentation))
                    (t (documentation-property sym 'variable-documentation)))))
    (consult--annotation doc)))

(defun consult-annotate-variable (cand)
  "Annotate variable CAND with documentation string."
  (let ((sym (intern cand)))
    (when-let (doc (documentation-property sym 'variable-documentation))
      (concat " "
              (propertize
               " "
               'display
               '(space :align-to (- right-fringe consult-annotation-width 30)))
              (propertize (consult--truncate (format "%S" (if (boundp sym)
                                                              (symbol-value sym)
                                                            'unbound))
                                             40)
                          'face 'consult-variable)
              "    "
              (propertize (consult--truncate doc consult-annotation-width)
                          'face 'consult-annotation)))))

(defun consult-annotate-face (cand)
  "Annotate face CAND with documentation string and face example."
  (let ((sym (intern cand)))
    (when-let (doc (documentation-property sym 'face-documentation))
      (concat " "
              (propertize
               " "
               'display
               '(space :align-to (- right-fringe consult-annotation-width 30)))
              (propertize "abcdefghijklmNOPQRSTUVWXYZ" 'face sym)
              "    "
              (propertize (consult--truncate doc consult-annotation-width)
                          'face 'consult-annotation)))))

(defun consult-annotate-package (cand)
  "Annotate package CAND with documentation."
  (when-let* ((pkg (intern (replace-regexp-in-string "-[[:digit:]\\.-]+$" "" cand)))
              ;; taken from embark.el, originally `describe-package-1`
              (desc (or (car (alist-get pkg package-alist))
                        (if-let ((built-in (assq pkg package--builtins)))
                            (package--from-builtin built-in)
                          (car (alist-get pkg package-archive-contents))))))
    (consult--annotation (package-desc-summary desc))))

(defun consult--selectrum-annotate-candidates (input candidates)
  "Annotate CANDIDATES with richer information.
INPUT is the input string."
  (funcall
   consult--selectrum-highlight-candidates
   input
   (if-let (annotate
            (and consult--annotate-this-command
                 (alist-get consult--annotate-this-command consult-annotate-alist)))
       (mapcar (lambda (cand) (concat cand (funcall annotate cand))) candidates)
     candidates)))

(defun consult--annotate-remember-command ()
  "Remember `this-command' for annotation."
  (setq-local consult--annotate-this-command this-command))

(defun consult--replace-annotation-function (fun metadata prop)
  "Advice for `completion-metadata-get'.
Replaces the annotation function.
FUN is the original function.
METADATA is the metadata.
PROP is the property which is looked up."
  (or
   (and (eq prop 'annotation-function)
        (not (bound-and-true-p selectrum-mode))
        consult--annotate-this-command
        (alist-get consult--annotate-this-command consult-annotate-alist))
   (funcall fun metadata prop)))

;;;###autoload
(define-minor-mode consult-annotate-mode
  "Annotate candidates with richer information."
  :global t

  ;; Reset first to get a clean slate.
  (advice-remove #'completion-metadata-get #'consult--replace-annotation-function)
  (remove-hook 'minibuffer-setup-hook #'consult--annotate-remember-command)
  (when (boundp 'selectrum-highlight-candidates-function)
    (setq selectrum-highlight-candidates-function (or consult--selectrum-highlight-candidates
                                                      selectrum-highlight-candidates-function)))

  ;; Now add our tweaks.
  (when consult-annotate-mode
    ;; Ensure that we remember this-command in order to select the annotation function.
    (add-hook 'minibuffer-setup-hook #'consult--annotate-remember-command)

    ;; Replace Selectrum highlighter.
    (when (boundp 'selectrum-highlight-candidates-function)
      (setq consult--selectrum-highlight-candidates selectrum-highlight-candidates-function
            selectrum-highlight-candidates-function #'consult--selectrum-annotate-candidates)
      (when (eq consult--selectrum-highlight-candidates #'consult--selectrum-annotate-candidates)
        (message "Invalid consult-annotate-mode state. Defaulting to selectrum-default-candidate-highlight-function.")
        (setq consult--selectrum-highlight-candidates #'selectrum-default-candidate-highlight-function)))

    ;; Replace the default annotation function if not using Selectrum.
    ;; TODO is there a better way?
    ;; TODO unfortunately annotations are not shown in the icomplete-vertical minibuffer it seem
    ;; https://github.com/oantolin/icomplete-vertical/issues/16
    (unless (bound-and-true-p selectrum-mode)
      (advice-add #'completion-metadata-get :around #'consult--replace-annotation-function))))

(provide 'consult)
;;; consult.el ends here
