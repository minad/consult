;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler and Consult contributors
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2020
;; Version: 0.8
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/minad/consult

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

;; Consult implements a set of `consult-<thing>' commands which use
;; `completing-read' to select from a list of candidates. Consult provides an
;; enhanced buffer switcher `consult-buffer' and search and navigation commands
;; like `consult-imenu' and `consult-line'. Searching through multiple files is
;; supported by the asynchronous `consult-grep' command. Many Consult commands
;; allow previewing candidates - if a candidate is selected in the completion
;; view, the buffer shows the candidate immediately.

;; The Consult commands are compatible with completion systems based
;; on the Emacs `completing-read' API, including the default completion
;; system, Icomplete, Selectrum, Vertico and Embark.

;; Consult has been inspired by Counsel. Some of the Consult commands
;; originated in the Counsel package or the Selectrum wiki. See the
;; README for a full list of contributors.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'bookmark)
(require 'kmacro)
(require 'recentf)
(require 'seq)

(defgroup consult nil
  "Consulting `completing-read'."
  :group 'convenience
  :prefix "consult-")

;;;; Customization

(defcustom consult-narrow-key nil
  "Prefix key for narrowing during completion.

Good choices for this key are (kbd \"<\") or (kbd \"C-+\") for example.

The key must be either a string or a vector.
This is the key representation accepted by `define-key'."
  :type '(choice key-sequence (const nil)))

(defcustom consult-widen-key nil
  "Key used for widening during completion.

If this key is unset, defaults to twice the `consult-narrow-key'.

The key must be either a string or a vector.
This is the key representation accepted by `define-key'."
  :type '(choice key-sequence (const nil)))

(defcustom consult-project-root-function nil
  "Function which returns project root directory.

The root directory is used by `consult-buffer' and `consult-grep'."
  :type '(choice function (const nil)))

(defcustom consult-async-refresh-delay 0.4
  "Refreshing delay of the completion ui for asynchronous commands.

The completion ui is only updated every `consult-async-refresh-delay'
seconds. This applies to asynchronous commands like for example
`consult-grep'."
  :type 'float)

(defcustom consult-async-input-throttle 0.5
  "Input throttle for asynchronous commands.

The asynchronous process is started only every
`consult-async-input-throttle' seconds. This applies to asynchronous
commands, e.g., `consult-grep'."
  :type 'float)

(defcustom consult-async-input-debounce 0.2
  "Input debounce for asynchronous commands.

The asynchronous process is started only when there has not been new
input for `consult-async-input-debounce' seconds. This applies to
asynchronous commands, e.g., `consult-grep'."
  :type 'float)

(defcustom consult-async-min-input 3
  "Minimum number of letters needed, before asynchronous process is called.

This applies to asynchronous commands, e.g., `consult-grep'."
  :type 'integer)

(defcustom consult-async-split-style 'perl
  "Async splitter style, see `consult-async-split-styles-alist'."
  :type '(choice (const :tag "Space" space)
                 (const :tag "Comma" comma)
                 (const :tag "Semicolon" semicolon)
                 (const :tag "Perl" perl)))

(defcustom consult-async-split-styles-alist
  '((space :separator ?\s :type separator)
    (comma :separator ?, :type separator)
    (semicolon :separator ?\; :type separator)
    (perl :initial "#" :type perl))
  "Async splitter styles."
  :type 'alist)

(defcustom consult-mode-histories
  '((eshell-mode . eshell-history-ring)
    (comint-mode . comint-input-ring)
    (term-mode   . term-input-ring))
  "Alist of (mode . history) pairs of mode histories.
The histories can be rings or lists."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom consult-themes nil
  "List of themes to be presented for selection.
nil shows all `custom-available-themes'."
  :type '(repeat symbol))

(defcustom consult-after-jump-hook '(recenter)
  "Function called after jumping to a location.

Commonly used functions for this hook are `recenter' and
`reposition-window'.

This is called during preview and for the jump after selection.
You may want to add a function which pulses the current line, e.g.,
`xref-pulse-momentarily'."
  :type 'hook)

(defcustom consult-line-start-from-top nil
  "Start search from the top if non-nil.
Otherwise start the search at the current line and wrap around."
  :type 'boolean)

(defcustom consult-line-point-placement 'match-beginning
  "Where to leave point after `consult-line' jumps to a match."
  :type '(choice (const :tag "Beginning of the line" line-beginning)
                 (const :tag "Beginning of the match" match-beginning)
                 (const :tag "End of the match" match-end)))

(defcustom consult-line-numbers-widen t
  "Show absolute line numbers when narrowing is active.

See also `display-line-numbers-widen'."
  :type 'boolean)

(defcustom consult-goto-line-numbers t
  "Show line numbers for `consult-goto-line'."
  :type 'boolean)

(defcustom consult-fontify-preserve t
  "Preserve fontification for line-based commands."
  :type 'boolean)

(defcustom consult-fontify-max-size 1048576
  "Buffers larger than this byte limit are not fontified.

This is necessary in order to prevent a large startup time
for navigation commands like `consult-line'."
  :type 'integer)

(defcustom consult-buffer-filter
  '("\\` "
    "\\`\\*Completions\\*\\'"
    "\\`\\*Flymake log\\*\\'"
    "\\`\\*Semantic SymRef\\*\\'"
    "\\`\\*tramp/.*\\*\\'")
  "Filter regexps for `consult-buffer'.

The default setting is to filter ephemeral buffer names beginning with a space
character, the *Completions* buffer and a few log buffers."
  :type '(repeat regexp))

(defcustom consult-buffer-sources
  '(consult--source-hidden-buffer
    consult--source-buffer
    consult--source-file
    consult--source-bookmark
    consult--source-project-buffer
    consult--source-project-file)
  "Sources used by `consult-buffer'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol))

(defcustom consult-mode-command-filter
  '(;; Filter commands
    "-mode\\'" "--"
    ;; Filter whole features
    simple mwheel time so-long recentf)
  "Filter commands for `consult-mode-command'."
  :type '(repeat (choice symbol regexp)))

(defcustom consult-git-grep-command
  "git --no-pager grep --null --color=always --extended-regexp\
   --line-number -I -e ARG OPTS"
  "Command line string for git-grep, see `consult-git-grep'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options."
  :type 'string)

(defcustom consult-grep-command
  "grep --null --line-buffered --color=always --extended-regexp\
   --exclude-dir=.git --line-number -I -r . -e ARG OPTS"
  "Command line string for grep, see `consult-grep'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options."
  :type 'string)

(defcustom consult-grep-max-columns 300
  "Maximal number of columns of grep output."
  :type 'integer)

(defcustom consult-ripgrep-command
  "rg --null --line-buffered --color=ansi --max-columns=1000\
   --no-heading --line-number . -e ARG OPTS"
  "Command line string for ripgrep, see `consult-ripgrep'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options."
  :type 'string)

(defcustom consult-find-command
  "find . -not ( -wholename */.* -prune ) -ipath *ARG* OPTS"
  "Command line string for find, see `consult-find'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options. By default the ARG is wrapped in wildcards."
  :type 'string)

(defcustom consult-locate-command
  "locate --ignore-case --existing --regexp ARG OPTS"
  "Command line string for locate, see `consult-locate'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options."
  :type 'string)

(defcustom consult-man-command
  "man -k ARG OPTS"
  "Command line string for man, see `consult-man'.

The command string must have a specific format, including ARG and OPTS
substrings. ARG is replaced by the filter string and OPTS by the auxillary
command options."
  :type 'string)

(defcustom consult-preview-key 'any
  "Preview trigger keys, can be nil, 'any, a single key or a list of keys."
  :type '(choice (const any) (const nil) key-sequence (repeat key-sequence)))

(defcustom consult-preview-max-size 10485760
  "Files larger than this byte limit are not previewed."
  :type 'integer)

(defcustom consult-preview-raw-size 102400
  "Files larger than this byte limit are previewed in raw form."
  :type 'integer)

(defcustom consult-preview-max-count 10
  "Number of files to keep open at once during preview."
  :type 'integer)

(defcustom consult-preview-excluded-hooks
  '(epa-file-find-file-hook
    recentf-track-opened-file
    vc-refresh-state)
  "List of `find-file' hooks, which should not be executed during file preview.
In particular we don't want to modify the list of recent files and we
don't want to see epa password prompts."
  :type '(repeat symbol))

(defcustom consult-bookmark-narrow
  `((?f "File" ,#'bookmark-default-handler)
    (?h "Help" ,#'help-bookmark-jump)
    (?i "Info" ,#'Info-bookmark-jump)
    (?p "Picture" ,#'image-bookmark-jump)
    (?d "Docview" ,#'doc-view-bookmark-jump)
    (?m "Man" ,#'Man-bookmark-jump)
    (?w "Woman" ,#'woman-bookmark-jump)
    (?g "Gnus" ,#'gnus-summary-bookmark-jump))
  "Bookmark narrowing configuration.

Each element of the list must have the form '(char name handler)."
  :type '(repeat (list character string function)))

;;;; Faces

(defgroup consult-faces nil
  "Faces used by Consult."
  :group 'consult
  :group 'faces)

(defface consult-preview-line
  '((t :inherit consult-preview-insertion :extend t))
  "Face used to for line previews.")

(defface consult-preview-match
  '((t :inherit match))
  "Face used to for match previews in `consult-grep'.")

(defface consult-preview-cursor
  '((t :inherit consult-preview-match))
  "Face used to for cursor previews and marks in `consult-mark'.")

(defface consult-preview-error
  '((t :inherit isearch-fail))
  "Face used to for cursor previews and marks in `consult-compile-error'.")

(defface consult-preview-insertion
  '((t :inherit region))
  "Face used to for previews of text to be inserted.
Used by `consult-completion-in-region', `consult-yank' and `consult-history'.")

(defface consult-narrow-indicator
  '((t :inherit warning))
  "Face used for the narrowing indicator.")

(defface consult-async-running
  '((t :inherit consult-narrow-indicator))
  "Face used if asynchronous process is running.")

(defface consult-async-finished
  '((t :inherit success))
  "Face used if asynchronous process has finished.")

(defface consult-async-failed
  '((t :inherit error))
  "Face used if asynchronous process has failed.")

(defface consult-async-split
  '((t :inherit font-lock-negation-char-face))
  "Face used to highlight punctuation character.")

(defface consult-help
  '((t :inherit shadow))
  "Face used to highlight help, e.g., in `consult-register-store'.")

(defface consult-key
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight keys, e.g., in `consult-register'.")

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

(defface consult-line-number-prefix
  '((t :inherit line-number))
  "Face used to highlight line numbers in selections.")

(defface consult-separator
  '((((class color) (min-colors 88) (background light))
     :foreground "#ccc")
    (((class color) (min-colors 88) (background dark))
     :foreground "#333"))
  "Face used for thin line separators in `consult-register-window'.")

;;;; History variables

(defvar consult--keep-lines-history nil)
(defvar consult--grep-history nil)
(defvar consult--find-history nil)
(defvar consult--man-history nil)
(defvar consult--line-history nil)
(defvar consult--apropos-history nil)
(defvar consult--theme-history nil)
(defvar consult--minor-mode-menu-history nil)
(defvar consult--mode-command-history nil)
(defvar consult--kmacro-history nil)
(defvar consult--buffer-history nil)

;;;; Internal variables

(defvar consult-async-default-split "#")
(make-obsolete-variable 'consult-async-default-split
                        "Deprecated in favor of `consult-async-split-style'." "0.7")

(defvaralias 'consult-config 'consult--read-config)
(make-obsolete-variable 'consult-config "Deprecated in favor of `consult-customize'." "0.7")

(defvar consult--read-config nil
  "Command configuration alist, which allows fine-grained configuration.

Each element of the list must have the form (command-name plist...). The options
set here will be passed to `consult--read', when called from the corresponding
command. Note that the options depend on the private `consult--read' API and
should not be considered as stable as the public API.")

(defvar consult--multi-sources nil
  "Vector of currently active `consult--multi' sources.")

(defvar consult--multi-align nil
  "Alignment string for annotations for `consult--multi-annotate'.")

(defvar consult--buffer-display #'switch-to-buffer
  "Buffer display function.")

(defvar consult--cache nil
  "Cached data populated by `consult--define-cache'.")

(defvar consult--completion-candidate-hook
  (list #'consult--default-completion-mb-candidate
        #'consult--default-completion-list-candidate)
  "Get candidate from completion system.")

(defvar consult--completion-refresh-hook nil
  "Refresh completion system.")

(defvar-local consult--preview-function nil
  "Minibuffer-local variable which exposes the current preview function.
This function can be called by custom completion systems from outside the minibuffer.")

(defconst consult--tofu-char #x100000
  "Special character used to encode line prefixes for disambiguation.
We use the first character of the private unicode plane b.")

(defconst consult--tofu-range #xFFFE
  "Special character range.
Size of private unicode plane b.")

(defvar-local consult--narrow nil
  "Current narrowing key.")

(defvar-local consult--narrow-keys nil
  "Narrowing prefixes of the current completion.")

(defvar-local consult--narrow-predicate nil
  "Narrowing predicate of the current completion.")

(defvar-local consult--narrow-overlay nil
  "Narrowing indicator overlay.")

(defvar consult--gc-threshold 67108864
  "Large gc threshold for temporary increase.")

(defvar consult--gc-percentage 0.5
  "Large gc percentage for temporary increase.")

(defvar consult--async-log
  " *consult-async*"
  "Buffer for async logging output used by `consult--async-process'.")

(defconst consult--grep-regexp
  (let ((esc "\\(?:\e\\[[0-9;]*[mK]\\)*")
        (name "\\([^\0\n\e]+\\)")
        (line "\\([0-9]+\\)")
        (reset "\\(?:\e\\[[mK]\\)*"))
    (concat esc name esc "\0" esc line esc "[:\0]" reset))
  "Regexp used to match file and line of grep output.")

(defconst consult--grep-match-regexp
  "\e\\[[0-9;]+m\\(?:\e\\[K\\)?\\([^\e]*\\)\e\\[[0-9;]*m\\(?:\e\\[K\\)?"
  "Regexp used to find matches in grep output.")

(defvar-local consult--focus-lines-overlays nil
  "Overlays used by `consult-focus-lines'.")

;;;; Customization helper

(defun consult--customize-set (cmds prop val)
  "Set property PROP to VAL of commands CMDS."
  (dolist (cmd cmds)
    (cond
     ((and (boundp cmd) (consp (symbol-value cmd)))
      (set cmd (plist-put (symbol-value cmd) prop val)))
     ((functionp cmd)
      (setf (alist-get cmd consult--read-config)
            (plist-put (alist-get cmd consult--read-config) prop val)))
     (t (user-error "%s is neither a Consult command nor a Consult source"
                    cmd))))
  nil)

(defmacro consult-customize (&rest args)
  "Set properties of commands or sources.
ARGS is a list of commands or sources followed by the list of keyword-value pairs."
  ;;(declare (indent 2))
  (let ((setter))
    (while args
      (let ((cmds (seq-take-while (lambda (x) (not (keywordp x))) args)))
        (setq args (seq-drop-while (lambda (x) (not (keywordp x))) args))
        (while (keywordp (car args))
          (push `(consult--customize-set ',cmds ,(car args) ,(cadr args)) setter)
          (setq args (cddr args)))))
    (macroexp-progn setter)))

;;;; Helper functions and macros

;; Upstream bug#46326, Consult issue https://github.com/minad/consult/issues/193
(defmacro consult--minibuffer-with-setup-hook (fun &rest body)
  "Variant of `minibuffer-with-setup-hook' using a symbol and `fset'.

This macro is only needed to prevent memory leaking issues with
the upstream `minibuffer-with-setup-hook' macro.
FUN is the hook function and BODY opens the minibuffer."
  (declare (indent 1) (debug t))
  (let ((hook (make-symbol "hook"))
        (append))
    (when (eq (car-safe fun) :append)
      (setq append '(t) fun (cadr fun)))
    `(let ((,hook (make-symbol "consult--minibuffer-setup")))
       (fset ,hook (lambda ()
                     (remove-hook 'minibuffer-setup-hook ,hook)
                     (funcall ,fun)))
       (unwind-protect
           (progn
             (add-hook 'minibuffer-setup-hook ,hook ,@append)
             ,@body)
         (remove-hook 'minibuffer-setup-hook ,hook)))))

(defmacro consult--define-cache (name &rest body)
  "Define cached value with NAME and BODY."
  (declare (indent 1))
  `(defun ,name ()
     (or (alist-get ',name consult--cache)
         (progn
           ;; XXX Emacs bug#35546, fixed in Emacs 27.1
           ;; (setf (alist-get key list) val) should return val
           (setf (alist-get ',name consult--cache) ,(macroexp-progn body))
           (alist-get ',name consult--cache)))))

(defun consult--completion-filter (pattern cands category _highlight)
  "Filter CANDS with PATTERN.

CATEGORY is the completion category, used to find the completion style via
`completion-category-defaults' and `completion-category-overrides'.
HIGHLIGHT must be non-nil if the resulting strings should be highlighted."
  ;; completion-all-completions returns an improper list
  ;; where the last link is not necessarily nil.
  ;; TODO Implement support to disable highlighting as in Vertico deferred highlighting.
  (nconc (completion-all-completions
          pattern cands nil (length pattern)
          `(metadata (category . ,category)))
         nil))

(defun consult--completion-filter-complement (pattern cands category _highlight)
  "Filter CANDS with complement of PATTERN.
See `consult--completion-filter' for the arguments CATEGORY and HIGHLIGHT."
  (let ((ht (consult--string-hash (consult--completion-filter pattern cands category nil))))
    (seq-remove (lambda (x) (gethash x ht)) cands)))

(defun consult--completion-filter-dispatch (pattern cands category highlight)
  "Filter CANDS with PATTERN with optional complement.
Either using `consult--completion-filter' or
`consult--completion-filter-complement', depending on if the pattern starts
with a bang. See `consult--completion-filter' for the arguments CATEGORY and
HIGHLIGHT."
  (cond
   ((string-match-p "\\`!? ?\\'" pattern) cands) ;; empty pattern
   ((string-prefix-p "! " pattern) (consult--completion-filter-complement
                                    (substring pattern 2) cands category nil))
   (t (consult--completion-filter pattern cands category highlight))))

(defmacro consult--each-line (beg end &rest body)
  "Iterate over each line.

The line beginning/ending BEG/END is bound in BODY."
  (declare (indent 2))
  (let ((max (make-symbol "max")))
    `(save-excursion
       (let ((,beg (point-min)) (,max (point-max)) end)
         (while (< ,beg ,max)
           (goto-char ,beg)
           (setq ,end (line-end-position))
           ,@body
           (setq ,beg (1+ ,end)))))))

(defmacro consult--static-if (cond then &rest else)
  "If COND yields non-nil at compile time, do THEN, else do ELSE."
  (declare (indent 2))
  (if (eval cond) then (macroexp-progn else)))

(defun consult--display-width (string)
  "Compute width of STRING taking display and invisible properties into account."
  (let ((pos 0) (width 0) (end (length string)))
    (while (< pos end)
      (let ((nextd (next-single-property-change pos 'display string end))
            (display (get-text-property pos 'display string)))
        (if (stringp display)
            (setq width (+ width (string-width display))
                  pos nextd)
          (while (< pos nextd)
            (let ((nexti (next-single-property-change pos 'invisible string nextd)))
              (unless (get-text-property pos 'invisible string)
                (setq width (+ width
                               ;; bug#47712: Emacs 28 can compute `string-width' of substrings
                               (consult--static-if (= 3 (cdr (func-arity #'string-width)))
                                   (string-width string pos nexti)
                                 (string-width
                                  ;; Avoid allocation for the full string.
                                  (if (and (= pos 0) (= nexti end))
                                      string
                                    (substring-no-properties string pos nexti)))))))
              (setq pos nexti))))))
    width))

(defun consult--string-hash (strings)
  "Create hashtable from STRINGS."
  (let ((ht (make-hash-table :test #'equal :size (length strings))))
    (dolist (str strings)
      (puthash str t ht))
    ht))

(defmacro consult--local-let (binds &rest body)
  "Buffer local let BINDS of dynamic variables in BODY."
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer"))
        (local (mapcar (lambda (x) (cons (make-symbol "local") (car x))) binds)))
    `(let ((,buffer (current-buffer))
           ,@(mapcar (lambda (x) `(,(car x) (local-variable-p ',(cdr x)))) local))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (x) `(make-local-variable ',(car x))) binds)
             (let (,@binds)
               ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             ,@(mapcar (lambda (x)
                         `(unless ,(car x)
                            (kill-local-variable ',(cdr x))))
                       local)))))))

(defun consult--regexp-filter (regexps)
  "Create filter regexp from REGEXPS."
  (mapconcat (lambda (x) (concat "\\(?:" x "\\)")) regexps "\\|"))

(defun consult--format-directory-prompt (prompt dir)
  "Format PROMPT, expand directory DIR and return them as a pair."
  (save-match-data
    (let ((edir (file-name-as-directory (expand-file-name dir)))
          (ddir (file-name-as-directory (expand-file-name default-directory))))
      (cons
       (if (string= ddir edir)
           (concat prompt ": ")
         (let ((adir (abbreviate-file-name edir)))
           (if (string-match "/\\([^/]+\\)/\\([^/]+\\)/\\'" adir)
               (format "%s in …/%s/%s/: " prompt
                       (match-string 1 adir) (match-string 2 adir))
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
   ((when-let (root (consult--project-root))
      (save-match-data
        (if (string-match "/\\([^/]+\\)/\\'" root)
            (cons (format "%s in project %s: " prompt (match-string 1 root)) root)
          (consult--format-directory-prompt prompt root)))))
   (t (consult--format-directory-prompt prompt default-directory))))

(defun consult--project-root ()
  "Return project root as absolute path."
  (when-let (root (and consult-project-root-function (funcall consult-project-root-function)))
    (expand-file-name root)))

(defun consult--format-location (file line &optional str)
  "Format location string 'FILE:LINE:STR'."
  (setq line (number-to-string line)
        str (concat file ":" line ":" str)
        file (length file))
  (put-text-property 0 file 'face 'consult-file str)
  (put-text-property (1+ file) (+ 1 file (length line)) 'face 'consult-line-number str)
  str)

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

(defun consult--remove-dups (list)
  "Remove duplicate strings from LIST."
  (delete-dups (copy-sequence list)))

(defsubst consult--in-range-p (pos)
  "Return t if position POS lies in range `point-min' to `point-max'."
  (<= (point-min) pos (point-max)))

(define-obsolete-function-alias
  'consult--type-title
  'consult--type-group
  "0.7")

(defun consult--type-group (types)
  "Return title function for TYPES."
  (lambda (cand transform)
    (if transform
        cand
      (alist-get (get-text-property 0 'consult--type cand) types))))

(defun consult--type-narrow (types)
  "Return narrowing configuration from TYPES."
  (list :predicate
        (lambda (cand) (eq (get-text-property 0 'consult--type cand) consult--narrow))
        :keys types))

(defun consult--lookup-member (_ candidates cand)
  "Lookup CAND in CANDIDATES list, return original element."
  (car (member cand candidates)))

(defun consult--lookup-cons (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return cons."
  (assoc cand candidates))

(defun consult--lookup-cdr (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return cdr of element."
  (cdr (assoc cand candidates)))

(defun consult--lookup-location (_ candidates cand)
  "Lookup CAND in CANDIDATES list of 'consult-location category, return the marker."
  (when-let (found (member cand candidates))
    (car (get-text-property 0 'consult-location (car found)))))

(defun consult--lookup-candidate (_ candidates cand)
  "Lookup CAND in CANDIDATES list and return property 'consult--candidate."
  (when-let (found (member cand candidates))
    (get-text-property 0 'consult--candidate (car found))))

(defun consult--forbid-minibuffer ()
  "Raise an error if executed from the minibuffer."
  (when (minibufferp)
    (user-error "`%s' called inside the minibuffer" this-command)))

(defun consult--require-minibuffer ()
  "Raise an error if executed outside the minibuffer."
  (unless (minibufferp)
    (user-error "`%s' must be called inside the minibuffer" this-command)))

(defun consult--fontify-all ()
  "Ensure that the whole buffer is fontified."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line
  ;; is not font-locked. We would observe this if consulting an unfontified
  ;; line. Therefore we have to enforce font-locking now, which is slow. In
  ;; order to prevent is hang-up we check the buffer size against
  ;; `consult-fontify-max-size'.
  (when (and consult-fontify-preserve jit-lock-mode
             (< (buffer-size) consult-fontify-max-size))
    (jit-lock-fontify-now)))

(defun consult--fontify-region (start end)
  "Ensure that region between START and END is fontified."
  (when (and consult-fontify-preserve jit-lock-mode)
    (jit-lock-fontify-now start end)))

(defun consult--define-key (map key cmd desc)
  "Bind CMD to KEY with DESC in MAP.
Also create a which-key pseudo key to show the description."
  (define-key map key (cons desc cmd))
  ;; The which-key description is potentially fragile if something is changed on the side
  ;; of which-key. Keep an eye on that. An alternative more standard-compliant method
  ;; would be to use `menu-item', but this is unfortunately not yet supported by which-key
  ;; and `describe-buffer-bindings'.
  ;; See https://github.com/justbur/emacs-which-key/issues/177
  (let ((idx (1- (length key))))
    (define-key map (vconcat (seq-take key idx) (vector 'which-key (elt key idx)))
      `(which-key (,(copy-sequence desc))))))

(defmacro consult--with-increased-gc (&rest body)
  "Temporarily increase the gc limit in BODY to optimize for throughput."
  (let ((overwrite (make-symbol "overwrite")))
    `(let* ((,overwrite (> consult--gc-threshold gc-cons-threshold))
            (gc-cons-threshold (if ,overwrite consult--gc-threshold gc-cons-threshold))
            (gc-cons-percentage (if ,overwrite consult--gc-percentage gc-cons-percentage)))
       ,@body)))

(defun consult--count-lines (pos)
  "Move to position POS and return number of lines."
  (let ((line 0))
    (while (< (point) pos)
      (forward-line)
      (when (<= (point) pos)
        (setq line (1+ line))))
    (goto-char pos)
    line))

(defun consult--position-marker (buffer line column)
  "Get marker in BUFFER from LINE and COLUMN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (save-excursion
          (widen)
          (goto-char (point-min))
          ;; Location data might be invalid by now!
          (ignore-errors
            (forward-line (1- line))
            (forward-char column))
          (point-marker))))))

(defun consult--line-prefix ()
  "Annotate `consult-location' candidates with line numbers."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (fmt (propertize (format "%%%dd " width) 'face 'consult-line-number-prefix)))
    (lambda (cand)
      (list cand (format fmt (cdr (get-text-property 0 'consult-location cand))) ""))))

(defun consult--location-candidate (cand marker line &rest props)
  "Add MARKER and LINE as 'consult-location text property to CAND.
Furthermore add the additional text properties PROPS, and append
tofu-encoded MARKER suffix for disambiguation."
  (setq cand (concat cand (consult--tofu-encode marker)))
  (add-text-properties 0 1 `(consult-location (,marker . ,line) ,@props) cand)
  cand)

(defsubst consult--buffer-substring (beg end &optional fontify)
  "Return buffer substring between BEG and END.
If FONTIFY and `consult-fontify-preserve' are non-nil, first ensure that the
region has been fontified."
  (if consult-fontify-preserve
      (progn
        (when fontify
          (consult--fontify-region beg end))
        (buffer-substring beg end))
    (buffer-substring-no-properties beg end)))

(defun consult--region-with-cursor (beg end marker)
  "Return region string with a marking at the cursor position.

BEG is the begin position.
END is the end position.
MARKER is the cursor position."
  (let ((str (consult--buffer-substring beg end 'fontify)))
    (if (>= marker end)
        (concat str #(" " 0 1 (face consult-preview-cursor)))
      (put-text-property (- marker beg) (- (1+ marker) beg)
                         'face 'consult-preview-cursor str)
      str)))

(defun consult--line-with-cursor (marker)
  "Return current line where the cursor MARKER is highlighted."
  (consult--region-with-cursor (line-beginning-position) (line-end-position) marker))

;;;; Preview support

(defun consult--kill-clean-buffer (buf)
  "Kill BUF if it has not been modified."
  (unless (buffer-modified-p buf)
    (kill-buffer buf)))

(defun consult--temporary-files ()
  "Return a function to open files temporarily."
  (let* ((new-buffers))
    (lambda (&optional name)
      (if name
          (or (get-file-buffer name)
              (when-let (attrs (file-attributes name))
                (let ((size (file-attribute-size attrs)))
                  (if (> size consult-preview-max-size)
                      (prog1 nil
                        (message "File `%s' (%s) is too large for preview"
                                 name (file-size-human-readable size)))
                    (cl-letf* (((default-value 'find-file-hook)
                                (seq-remove (lambda (x) (memq x consult-preview-excluded-hooks))
                                            (default-value 'find-file-hook)))
                               (inhibit-message t)
                               (non-essential t)
                               (enable-dir-local-variables nil)
                               (enable-local-variables (and enable-local-variables :safe))
                               (buf (find-file-noselect
                                     name 'nowarn
                                     (> size consult-preview-raw-size))))
                      (push buf new-buffers)
                      ;; Only keep a few buffers alive
                      (while (> (length new-buffers) consult-preview-max-count)
                        (consult--kill-clean-buffer (car (last new-buffers)))
                        (setq new-buffers (nbutlast new-buffers)))
                      buf)))))
        (mapc #'consult--kill-clean-buffer new-buffers)))))

(defun consult--invisible-open-permanently ()
  "Open overlays which hide the current line.
See `isearch-open-necessary-overlays' and `isearch-open-overlay-temporary'."
  (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
    (when-let (fun (overlay-get ov 'isearch-open-invisible))
      (when (invisible-p (overlay-get ov 'invisible))
        (funcall fun ov)))))

(defun consult--invisible-open-temporarily ()
  "Temporarily open overlays which hide the current line.
See `isearch-open-necessary-overlays' and `isearch-open-overlay-temporary'."
  (let ((restore))
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)) restore)
      (let ((inv (overlay-get ov 'invisible)))
        (when (and (invisible-p inv) (overlay-get ov 'isearch-open-invisible))
          (push (if-let (fun (overlay-get ov 'isearch-open-invisible-temporary))
                    (progn
                      (funcall fun nil)
                      (lambda () (funcall fun t)))
                  (overlay-put ov 'invisible nil)
                  (lambda () (overlay-put ov 'invisible inv)))
                restore))))))

(defun consult--jump-nomark (pos)
  "Go to POS and recenter."
  (cond
   ((and (markerp pos) (not (buffer-live-p (marker-buffer pos))))
    ;; Only print a message, no error in order to not mess
    ;; with the minibuffer update hook.
    (message "Buffer is dead"))
   (t
    ;; Switch to buffer if it is not visible
    (when (and (markerp pos) (not (eq (current-buffer) (marker-buffer pos))))
      (consult--buffer-action (marker-buffer pos) 'norecord))
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
    (consult--jump-nomark pos)
    (consult--invisible-open-permanently))
  nil)

;; Matched strings are not highlighted as of now.
;; see https://github.com/minad/consult/issues/7
(defun consult--jump-preview (&optional face)
  "The preview function used if selecting from a list of candidate positions.
The function can be used as the `:state' argument of `consult--read'.
FACE is the cursor face."
  (let ((overlays)
        (invisible)
        (face (or face 'consult-preview-cursor))
        (saved-min (point-min-marker))
        (saved-max (point-max-marker))
        (saved-pos (point-marker)))
    (set-marker-insertion-type saved-max t) ;; Grow when text is inserted
    (lambda (cand restore)
      (mapc #'funcall invisible)
      (mapc #'delete-overlay overlays)
      (setq invisible nil overlays nil)
      (cond
       (restore
        (let ((saved-buffer (marker-buffer saved-pos)))
          (if (not (buffer-live-p saved-buffer))
              (message "Buffer is dead")
            (set-buffer saved-buffer)
            (narrow-to-region saved-min saved-max)
            (goto-char saved-pos))))
       ;; Jump to position
       (cand
        (consult--jump-nomark cand)
        (setq invisible (consult--invisible-open-temporarily)
              overlays
              (list (save-excursion
                      (let ((vbeg (progn (beginning-of-visual-line) (point)))
                            (vend (progn (end-of-visual-line) (point)))
                            (end (line-end-position)))
                        (consult--overlay vbeg (if (= vend end) (1+ end) vend)
                                          'face 'consult-preview-line
                                          'window (selected-window))))
                    (consult--overlay (point) (1+ (point))
                                      'face face
                                      'window (selected-window)))))
       ;; If position cannot be previewed, return to saved position
       (t (consult--jump-nomark saved-pos))))))

(defun consult--jump-state (&optional face)
  "The state function used if selecting from a list of candidate positions.
The function can be used as the `:state' argument of `consult--read'.
FACE is the cursor face."
  (let ((preview (consult--jump-preview face)))
    (lambda (cand restore)
      (funcall preview cand restore)
      (when (and cand restore)
        (consult--jump cand)))))

(defmacro consult--define-state (type)
  "Define state function for TYPE."
  `(defun ,(intern (format "consult--%s-state" type)) ()
     (let ((preview (,(intern (format "consult--%s-preview" type)))))
       (lambda (cand restore)
         (funcall preview cand restore)
         (when (and cand restore)
           (,(intern (format "consult--%s-action" type)) cand))))))

(defun consult--preview-key-normalize (preview-key)
  "Normalize PREVIEW-KEY, return alist of keys and debounce times."
  (let ((keys)
        (debounce 0))
    (unless (listp preview-key)
      (setq preview-key (list preview-key)))
    (while preview-key
      (if (eq (car preview-key) :debounce)
          (setq debounce (cadr preview-key)
                preview-key (cddr preview-key))
        (push (cons (car preview-key) debounce) keys)
        (pop preview-key)))
    keys))

(defun consult--preview-key-pressed-p (preview-key cand)
  "Return t if PREVIEW-KEY has been pressed given the current candidate CAND."
  (when (and (consp preview-key) (memq :keys preview-key))
    (setq preview-key (funcall (plist-get preview-key :predicate) cand)))
  (setq preview-key (consult--preview-key-normalize preview-key))
  (let ((keys (this-single-command-keys)))
    (cdr (or (seq-find (lambda (x)
                         (and (not (eq (car x) 'any))
                              (equal (vconcat (car x)) keys)))
                       preview-key)
             (assq 'any preview-key)))))

(defun consult--with-preview-1 (preview-key state transform candidate fun)
  "Add preview support for FUN.

See `consult--with-preview' for the arguments PREVIEW-KEY, STATE, TRANSFORM and CANDIDATE."
  (let ((input "") (selected) (timer))
    (consult--minibuffer-with-setup-hook
        (if (and state preview-key)
            (lambda ()
              (setq consult--preview-function
                    (let ((last-preview))
                      (lambda ()
                        (when-let (cand (funcall candidate))
                          (with-selected-window (active-minibuffer-window)
                            (let ((input (minibuffer-contents-no-properties)))
                              (with-selected-window (or (minibuffer-selected-window) (next-window))
                                (let ((transformed (funcall transform input cand))
                                      (new-preview (cons input cand)))
                                  (when-let (debounce (consult--preview-key-pressed-p preview-key transformed))
                                    (when timer
                                      (cancel-timer timer)
                                      (setq timer nil))
                                    (unless (equal last-preview new-preview)
                                      (if (> debounce 0)
                                          (let ((win (selected-window)))
                                            (setq timer
                                                  (run-at-time
                                                   debounce
                                                   nil
                                                   (lambda ()
                                                     (when (window-live-p win)
                                                       (with-selected-window win
                                                         (funcall state transformed nil)
                                                         (setq last-preview new-preview)))))))
                                        (funcall state transformed nil)
                                        (setq last-preview new-preview))))))))))))
              (let ((post-command-sym (make-symbol "consult--preview-post-command")))
                (fset post-command-sym (lambda ()
                                         (setq input (minibuffer-contents-no-properties))
                                         (funcall consult--preview-function)))
                (add-hook 'post-command-hook post-command-sym nil 'local)))
          (lambda ()
            (let ((post-command-sym (make-symbol "consult--preview-post-command")))
              (fset post-command-sym (lambda () (setq input (minibuffer-contents-no-properties))))
              (add-hook 'post-command-hook post-command-sym nil 'local))))
      (unwind-protect
          (cons (setq selected (when-let (result (funcall fun))
                                 (funcall transform input result)))
                input)
        (when timer
          (cancel-timer timer))
        ;; If there is a state function, always call restore!
        ;; The preview function should be seen as a stateful object,
        ;; and we call the destructor here.
        (when state
          (funcall state selected t))))))

(defmacro consult--with-preview (preview-key state transform candidate &rest body)
  "Add preview support to BODY.

STATE is the state function.
TRANSFORM is the transformation function.
CANDIDATE is the function returning the current candidate.
PREVIEW-KEY are the keys which triggers the preview.

The preview function takes two arguments, the selected candidate and a restore
flag. It is called every time with restore=nil after a preview-key keypress, as
long as a new candidate is selected. Finally the preview function is called in
any case with restore=t even if no preview has actually taken place. The
candidate argument can be nil if the selection has been aborted."
  (declare (indent 4))
  `(consult--with-preview-1 ,preview-key ,state ,transform ,candidate (lambda () ,@body)))

;;;; Narrowing support

(defun consult--widen-key ()
  "Return widening key, if `consult-widen-key' is not set, defaults to twice `consult-narrow-key'."
  (or consult-widen-key (and consult-narrow-key (vconcat consult-narrow-key consult-narrow-key))))

(defun consult-narrow (key)
  "Narrow current completion with KEY.

This command is used internally by the narrowing system of `consult--read'."
  (interactive
   (list (unless (equal (this-single-command-keys) (consult--widen-key))
           last-command-event)))
  (consult--require-minibuffer)
  (setq consult--narrow key)
  (when consult--narrow-predicate
    (setq minibuffer-completion-predicate (and consult--narrow consult--narrow-predicate)))
  (when consult--narrow-overlay
    (delete-overlay consult--narrow-overlay))
  (when consult--narrow
    (setq consult--narrow-overlay
          (consult--overlay
           (1- (minibuffer-prompt-end)) (minibuffer-prompt-end)
           'before-string
           (propertize (format " [%s]" (alist-get consult--narrow
                                                  consult--narrow-keys))
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
         (when-let (pair (or (and (= 1 (length str))
                                  (assoc (aref str 0) consult--narrow-keys))
                             (and (string= str "")
                                  (assoc 32 consult--narrow-keys))))
           (delete-minibuffer-contents)
           (consult-narrow (car pair))
           #'ignore)))))

(defun consult-narrow-help ()
  "Print narrowing help as a `minibuffer-message'.

This command can be bound to a key in `consult-narrow-map',
to make it available for commands with narrowing."
  (interactive)
  (consult--require-minibuffer)
  (let ((minibuffer-message-timeout 1000000))
    (minibuffer-message
     (mapconcat
      (lambda (x) (concat
                   (propertize (char-to-string (car x)) 'face 'consult-key) " "
                   (propertize (cdr x) 'face 'consult-help)))
      (seq-filter (lambda (x) (/= (car x) 32))
                  consult--narrow-keys)
      " "))))

(defun consult--narrow-setup (settings map)
  "Setup narrowing with SETTINGS and keymap MAP."
  (cond
   ((functionp (car settings))
    (message "Deprecation: `%s' passed obsolete :narrow value to `consult--read'" this-command)
    (setq consult--narrow-predicate (car settings)
          consult--narrow-keys (cdr settings)))
   ((memq :keys settings)
    (setq consult--narrow-predicate (plist-get settings :predicate)
          consult--narrow-keys (plist-get settings :keys)))
   (t
    (setq consult--narrow-predicate nil
          consult--narrow-keys settings)))
  (when consult-narrow-key
    (dolist (pair consult--narrow-keys)
      (consult--define-key map
                           (vconcat consult-narrow-key (vector (car pair)))
                           #'consult-narrow (cdr pair))))
  (when-let (widen (consult--widen-key))
    (consult--define-key map widen #'consult-narrow "All")))

;;;; Splitting completion style

(defun consult--split-perl (str point)
  "Split input STR in async input and filtering part.

The function returns a list with four elements: The async string, the
completion filter string, the new point position computed from POINT and a
force flag. If the first character is a punctuation character it determines the
separator. Examples: \"/async/filter\", \"#async#filter\"."
  (if (string-match-p "^[[:punct:]]" str)
      (save-match-data
        (let ((q (regexp-quote (substring str 0 1))))
          (string-match (concat "^" q "\\([^" q "]*\\)\\(" q "\\)?") str)
          `(,(match-string 1 str)
            ,(substring str (match-end 0))
            ,(max 0 (- point (match-end 0)))
            ;; Force update it two punctuation characters are entered.
            ,(match-end 2)
            ;; List of highlights
            (0 . ,(match-beginning 1))
            ,@(and (match-end 2) `((,(match-beginning 2) . ,(match-end 2)))))))
    `(,str "" 0)))

(defun consult--split-separator (sep str point)
  "Split input STR in async input and filtering part at the first separator SEP.
POINT is the point position."
  (setq sep (regexp-quote (char-to-string sep)))
  (save-match-data
    (if (string-match (format "^\\([^%s]+\\)\\(%s\\)?" sep sep) str)
        `(,(match-string 1 str)
          ,(substring str (match-end 0))
          ,(max 0 (- point (match-end 0)))
          ;; Force update it space is entered.
          ,(match-end 2)
          ;; List of highlights
          (0 . ,(match-end 1)))
      `(,str "" 0))))

(defun consult--split-setup (split)
  "Setup splitting completion style with splitter function SPLIT."
  (let* ((styles completion-styles)
         (catdef completion-category-defaults)
         (catovr completion-category-overrides)
         (try (lambda (str table pred point)
                (let ((completion-styles styles)
                      (completion-category-defaults catdef)
                      (completion-category-overrides catovr)
                      (parts (funcall split str point)))
                  (completion-try-completion (cadr parts) table pred (caddr parts)))))
         (all (lambda (str table pred point)
                (let ((completion-styles styles)
                      (completion-category-defaults catdef)
                      (completion-category-overrides catovr)
                      (parts (funcall split str point)))
                  (completion-all-completions (cadr parts) table pred (caddr parts))))))
    (setq-local completion-styles-alist (cons `(consult--split ,try ,all "")
                                              completion-styles-alist))
    (setq-local completion-styles '(consult--split))
    (setq-local completion-category-defaults nil)
    (setq-local completion-category-overrides nil)))

;;;; Async support

(defmacro consult--with-async (bind &rest body)
  "Setup asynchronous completion in BODY.

BIND is the asynchronous function binding."
  (declare (indent 1))
  (let ((async (car bind)))
    `(let ((,async ,@(cdr bind)))
       (consult--minibuffer-with-setup-hook
           (lambda ()
             (when (functionp ,async)
               (funcall ,async 'setup)
               ;; Push input string to request refresh.
               ;; We use a symbol in order to avoid adding lambdas to the hook variable.
               (let ((sym (make-symbol "consult--async-after-change")))
                 (fset sym (lambda (&rest _) (funcall ,async (minibuffer-contents-no-properties))))
                 (run-at-time 0 nil sym)
                 (add-hook 'after-change-functions sym nil 'local))))
         (let ((,async (if (functionp ,async) ,async (lambda (_) ,async))))
           (unwind-protect
               ,(macroexp-progn body)
             (funcall ,async 'destroy)))))))

(defun consult--async-sink ()
  "Create ASYNC sink function.

An async function must accept a single action argument. For the 'setup action
it is guaranteed that the call originates from the minibuffer. For the other
actions no assumption about the context can be made.

'setup   Setup the internal closure state.
'destroy Destroy the internal closure state.
'flush   Flush the list of candidates.
'refresh Request UI refresh.
nil      Return the current list of candidates.
list     Append the list to the already existing list of candidates.
string   The input string. Called when the user enters something."
  (let ((candidates) (last) (buffer))
    (lambda (action)
      (pcase-exhaustive action
        ('nil candidates)
        ('setup
         (setq buffer (current-buffer))
         nil)
        ((or (pred stringp) 'destroy) nil)
        ('flush (setq candidates nil last nil))
        ('refresh
         ;; Refresh the UI when the current minibuffer window belongs
         ;; to the current asynchronous completion session.
         (when-let (win (active-minibuffer-window))
           (when (eq (window-buffer win) buffer)
             (with-selected-window win
               (run-hooks 'consult--completion-refresh-hook)))))
        ((pred consp)
         (setq last (last (if last (setcdr last action) (setq candidates action))))
         candidates)))))

(defun consult--async-split-style ()
  "Return the async split style function and initial string."
  (or (alist-get consult-async-split-style consult-async-split-styles-alist)
      (user-error "Split style `%s' not found" consult-async-split-style)))

(defun consult--async-split-initial (initial)
  "Return initial string for async command.
INITIAL is the additional initial string."
  (concat (plist-get (consult--async-split-style) :initial) initial))

(defun consult--async-split (async &optional split)
  "Create async function, which splits the input string.
ASYNC is the async sink.
SPLIT is the splitting function."
  (unless split
    (let ((style (consult--async-split-style)))
      (setq split (pcase (plist-get style :type)
                    ('separator (apply-partially #'consult--split-separator
                                                 (plist-get style :separator)))
                    ('perl #'consult--split-perl)
                    (type (user-error "Invalid style type `%s'" type))))))
  (lambda (action)
    (pcase action
      ('setup
       (consult--split-setup split)
       (funcall async 'setup))
      ((pred stringp)
       (pcase-let* ((`(,async-str ,_ ,_ ,force . ,highlights)
                     (funcall split action 0))
                    (async-len (length async-str))
                    (input-len (length action))
                    (end (minibuffer-prompt-end)))
         ;; Highlight punctuation characters
         (remove-list-of-text-properties end (+ end input-len) '(face))
         (dolist (hl highlights)
           (put-text-property (+ end (car hl)) (+ end (cdr hl))
                              'face 'consult-async-split))
         (funcall async
                  ;; Pass through if the input is long enough!
                  (if (or force (>= async-len consult-async-min-input))
                      async-str
                    ;; Pretend that there is no input
                    ""))))
      (_ (funcall async action)))))

(defun consult--async-log (formatted &rest args)
  "Log FORMATTED ARGS to `v/consult--async-log'."
  (with-current-buffer (get-buffer-create consult--async-log)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(defun consult--async-process (async cmd &rest props)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
CMD is the command argument list.
PROPS are optional properties passed to `make-process'."
  (let ((proc) (last-args) (indicator) (count))
    (lambda (action)
      (pcase action
        ("" ;; If no input is provided kill current process
         (ignore-errors (delete-process proc))
         (setq last-args nil))
        ((pred stringp)
         (let ((args (funcall cmd action))
               (stderr-buffer (generate-new-buffer " *consult-async-stderr*"))
               (flush t)
               (rest ""))
           (unless (equal args last-args)
             (setq last-args args)
             (ignore-errors (delete-process proc))
             (when args
               (overlay-put indicator 'display #("*" 0 1 (face consult-async-running)))
               (consult--async-log "consult--async-process started %S\n" args)
               (setq
                count 0
                proc
                (apply
                 #'make-process
                 (append
                  props
                  (list
                   :connection-type 'pipe
                   :name (car args)
                   :stderr stderr-buffer ;;; XXX tramp bug, the stderr buffer must be empty
                   :noquery t
                   :command args
                   :filter
                   (lambda (_ out)
                     (when flush
                       (setq flush nil)
                       (funcall async 'flush))
                     (let ((lines (split-string out "\n")))
                       (if (not (cdr lines))
                           (setq rest (concat rest (car lines)))
                         (setcar lines (concat rest (car lines)))
                         (let* ((len (length lines))
                                (last (nthcdr (- len 2) lines)))
                           (setq rest (cadr last)
                                 count (+ count len -1))
                           (setcdr last nil)
                           (funcall async lines)))))
                   :sentinel
                   (lambda (_ event)
                     (when flush
                       (setq flush nil)
                       (funcall async 'flush))
                     (overlay-put indicator 'display
                                  (cond
                                   ((string-prefix-p "killed" event)
                                    #(";" 0 1 (face consult-async-failed)))
                                   ((string-prefix-p "finished" event)
                                    #(":" 0 1 (face consult-async-finished)))
                                   (t
                                    #("!" 0 1 (face consult-async-failed)))))
                     (when (and (string-prefix-p "finished" event) (not (string= rest "")))
                       (setq count (+ count 1))
                       (funcall async (list rest)))
                     (consult--async-log
                      "consult--async-process sentinel: event=%s lines=%d\n"
                      (string-trim event) count)
                     (with-current-buffer (get-buffer-create consult--async-log)
                       (goto-char (point-max))
                       (insert ">>>>> stderr >>>>>\n")
                       (insert-buffer-substring stderr-buffer)
                       (insert "<<<<< stderr <<<<<\n")
                       (kill-buffer stderr-buffer)))))))))))
        ('destroy
         (ignore-errors (delete-process proc))
         (delete-overlay indicator)
         (funcall async 'destroy))
        ('setup
         (setq indicator (make-overlay (- (minibuffer-prompt-end) 2)
                                       (- (minibuffer-prompt-end) 1)))
         (funcall async 'setup))
        (_ (funcall async action))))))

(defun consult--async-throttle (async &optional throttle debounce)
  "Create async function from ASYNC which throttles input.

The THROTTLE delay defaults to `consult-async-input-throttle'.
The DEBOUNCE delay defaults to `consult-async-input-debounce'."
  (setq throttle (or throttle consult-async-input-throttle)
        debounce (or debounce consult-async-input-debounce))
  (let ((input "") (last) (timer))
    (lambda (action)
      (pcase action
        ('setup
         (funcall async 'setup))
        ((pred stringp)
         (unless (string= action input)
           (when timer
             (cancel-timer timer)
             (setq timer nil))
           (funcall async "") ;; cancel running process
           (setq input action)
           (unless (string= action "")
             (setq timer
                   (run-at-time
                    (+ debounce
                       (if last
                           (min (- (float-time) last)
                                consult-async-input-throttle)
                         0))
                    nil
                    (lambda ()
                      (setq last (float-time))
                      (funcall async action)))))))
        ('destroy
         (when timer (cancel-timer timer))
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--async-refresh-immediate (async)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens immediately when candidates are pushed."
  (lambda (action)
    (pcase action
      ((or (pred consp) (pred stringp) 'flush)
       (prog1 (funcall async action)
         (funcall async 'refresh)))
      (_ (funcall async action)))))

(defun consult--async-refresh-timer (async &optional delay)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens after a DELAY, defaulting to `consult-async-refresh-delay'."
  (let ((timer) (refresh) (delay (or delay consult-async-refresh-delay)))
    (lambda (action)
      (prog1 (funcall async action)
        (pcase action
          ((or (pred consp) (pred stringp) 'flush)
           (setq refresh t)
           (unless timer
             (setq timer (run-at-time
                          nil delay
                          (lambda ()
                            (when refresh
                              (setq refresh nil)
                              (funcall async 'refresh)))))))
          ('destroy (when timer (cancel-timer timer))))))))

(defmacro consult--async-transform (async &rest transform)
  "Use FUN to TRANSFORM candidates of ASYNC."
  (let ((async-var (make-symbol "async"))
        (action-var (make-symbol "action")))
    `(let ((,async-var ,async))
       (lambda (,action-var)
         (funcall ,async-var (if (consp ,action-var) (,@transform ,action-var) ,action-var))))))

(defun consult--async-map (async fun)
  "Map candidates of ASYNC by FUN."
  (consult--async-transform async mapcar fun))

(defun consult--async-filter (async fun)
  "Filter candidates of ASYNC by FUN."
  (consult--async-transform async seq-filter fun))

(defun consult--command-args (cmd)
  "Split command arguments and append to CMD."
  (setq cmd (split-string-and-unquote cmd))
  (lambda (input)
    (save-match-data
      (let ((opts))
        (when (string-match " +--\\( +\\|\\'\\)" input)
          ;; split-string-and-unquote modifies the match data
          ;; and fails if the quotes are invalid. Ignore it.
          (setq opts (substring input (match-end 0))
                input (substring input 0 (match-beginning 0))
                opts (ignore-errors (split-string-and-unquote opts))))
        (unless (string-blank-p input)
          (mapcan (lambda (x)
                    (if (string= x "OPTS")
                        opts
                      (list (replace-regexp-in-string "ARG" input x 'fixedcase 'literal))))
                  cmd))))))

(defmacro consult--async-command (cmd &rest args)
  "Asynchronous CMD pipeline.

ARGS is a list of `make-process' properties and transforms."
  (declare (indent 1))
  `(thread-first (consult--async-sink)
     (consult--async-refresh-timer)
     ,@(seq-take-while (lambda (x) (not (keywordp x))) args)
     (consult--async-process
      (consult--command-args ,cmd)
      ,@(seq-drop-while (lambda (x) (not (keywordp x))) args))
     (consult--async-throttle)
     (consult--async-split)))

;;;; Special keymaps

(defvar consult-async-map (make-sparse-keymap)
  "Keymap added for commands with asynchronous candidates.")

(defvar consult-preview-map (make-sparse-keymap)
  "Keymap added for commands with preview.")

(defvar consult-narrow-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " consult--narrow-space)
    (define-key map [127] consult--narrow-delete)
    map)
  "Narrowing keymap which is added to the local minibuffer map.
Note that `consult-narrow-key' and `consult-widen-key' are bound dynamically.")

;;;; Internal API: consult--read

(defun consult--add-history (async items)
  "Add ITEMS to the minibuffer future history.
ASYNC must be non-nil for async completion functions."
  (delete-dups
   (append
    ;; the defaults are at the beginning of the future history
    (if (listp minibuffer-default)
        minibuffer-default
      (list minibuffer-default))
    ;; then our custom items
    (remove "" (remq nil (if (listp items)
                             items
                           (list items))))
    ;; Add all the completions for non-async commands. For async commands this feature
    ;; is not useful, since if one selects a completion candidate, the async search is
    ;; restarted using that candidate string. This usually does not yield a desired
    ;; result since the async input uses a special format, e.g., `#grep#filter'.
    (unless async
      (all-completions ""
                       minibuffer-completion-table
                       minibuffer-completion-predicate)))))

(defun consult--setup-keymap (keymap async narrow preview-key)
  "Setup minibuffer keymap.

KEYMAP is a command-specific keymap.
ASYNC must be non-nil for async completion functions.
NARROW are the narrow settings.
PREVIEW-KEY are the preview keys."
  (let ((old-map (current-local-map))
        (map (make-sparse-keymap)))

    ;; Async keys overwriting some unusable defaults for the default completion
    (when async
      (when (eq (lookup-key old-map " ") #'minibuffer-complete-word)
        (define-key map " " #'self-insert-command))
      (when (eq (lookup-key old-map "\t") #'minibuffer-complete)
        (define-key map "\t" #'minibuffer-completion-help)))

    ;; Add narrow keys
    (when narrow
      (consult--narrow-setup narrow map))

    ;; Preview trigger keys
    (when (and (consp preview-key) (memq :keys preview-key))
      (setq preview-key (plist-get preview-key :keys)))
    (setq preview-key (mapcar #'car (consult--preview-key-normalize preview-key)))
    (when preview-key
      (dolist (key preview-key)
        (unless (or (eq key 'any) (lookup-key old-map key))
          (define-key map key #'ignore))))

    ;; Put the keymap together
    (use-local-map
     (make-composed-keymap
      (delq nil (list keymap
                      (and async consult-async-map)
                      (and narrow consult-narrow-map)
                      (and preview-key consult-preview-map)
                      map))
      old-map))))

(defun consult--fry-the-tofus (&rest _)
  "Fry the tofus in the minibuffer."
  (let* ((min (minibuffer-prompt-end))
         (max (point-max))
         (pos max)
         (high (+ consult--tofu-char consult--tofu-range -1)))
    (while (and (> pos min) (<= consult--tofu-char (char-before pos) high))
      (setq pos (1- pos)))
    (when (< pos max)
      (add-text-properties pos max '(invisible t rear-nonsticky t cursor-intangible t)))))

(defsubst consult--tofu-append (cand id)
  "Append tofu-encoded ID to CAND."
  (setq id (char-to-string (+ consult--tofu-char id)))
  (add-text-properties 0 1 '(invisible t consult-strip t) id)
  (concat cand id))

(defsubst consult--tofu-get (cand)
  "Extract tofu-encoded ID from CAND."
  (- (aref cand (1- (length cand))) consult--tofu-char))

;; We must disambiguate the lines by adding a prefix such that two lines with
;; the same text can be distinguished. In order to avoid matching the line
;; number, such that the user can search for numbers with `consult-line', we
;; encode the line number as unicode characters in the supplementary private use
;; plane b. By doing that, it is unlikely that accidential matching occurs.
(defun consult--tofu-encode (n)
  "Return tofu-encoded number N."
  (let ((str ""))
    (while (progn
             (setq str (concat (char-to-string (+ consult--tofu-char
                                                  (% n consult--tofu-range)))
                               str))
             (and (>= n consult--tofu-range) (setq n (/ n consult--tofu-range)))))
    (add-text-properties 0 (length str) '(invisible t consult-strip t) str)
    str))

(defun consult--read-annotate (fun cand)
  "Annotate CAND with annotation function FUN."
  (pcase (funcall fun cand)
    (`(,_ ,_ ,suffix) suffix)
    (ann ann)))

(defun consult--read-affixate (fun cands)
  "Affixate CANDS with annotation function FUN."
  (mapcar (lambda (cand)
            (let ((ann (funcall fun cand)))
              (if (consp ann)
                  ann
                (setq ann (or ann ""))
                (list cand ""
                      ;; The default completion UI adds the `completions-annotations' face
                      ;; if no other faces are present.
                      (if (text-property-not-all 0 (length ann) 'face nil ann)
                          ann
                        (propertize ann 'face 'completions-annotations))))))
          cands))

(cl-defun consult--read-1 (candidates &key
                                      prompt predicate require-match history default
                                      keymap category initial narrow add-history annotate
                                      state preview-key sort lookup title group inherit-input-method)
  "See `consult--read' for the documentation of the arguments."
  (when title
    (message "Deprecation: `%s' passed obsolete :title argument to `consult--read'" this-command)
    (setq group title))
  (consult--minibuffer-with-setup-hook
      (:append (lambda ()
                 (add-hook 'after-change-functions #'consult--fry-the-tofus nil 'local)
                 (consult--setup-keymap keymap (functionp candidates) narrow preview-key)
                 (setq-local minibuffer-default-add-function
                             (apply-partially #'consult--add-history (functionp candidates) add-history))))
    (consult--with-async (async candidates)
      ;; NOTE: Do not unnecessarily let-bind the lambdas to avoid
      ;; overcapturing in the interpreter. This will make closures and the
      ;; lambda string representation larger, which makes debugging much worse.
      ;; Fortunately the overcapturing problem does not affect the bytecode
      ;; interpreter which does a proper scope analyis.
      (let* ((metadata `(metadata
                         ,@(when category `((category . ,category)))
                         ,@(when group `((group-function . ,group)))
                         ,@(when annotate
                             `((affixation-function
                                . ,(apply-partially #'consult--read-affixate annotate))
                               (annotation-function
                                . ,(apply-partially #'consult--read-annotate annotate))))
                         ,@(unless sort '((cycle-sort-function . identity)
                                          (display-sort-function . identity)))))
             (result
              (consult--with-preview preview-key state
                                     (lambda (input cand)
                                       (funcall lookup input (funcall async nil) cand))
                                     (apply-partially #'run-hook-with-args-until-success
                                                      'consult--completion-candidate-hook)
                (completing-read prompt
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       metadata
                                     (complete-with-action action (funcall async nil) str pred)))
                                 predicate require-match initial
                                 (if (symbolp history) history (cadr history))
                                 default
                                 inherit-input-method))))
        (pcase-exhaustive history
          (`(:input ,var)
           (set var (cdr (symbol-value var)))
           (add-to-history var (cdr result)))
          ((pred symbolp)))
        (car result)))))

(cl-defun consult--read (candidates &rest options &key
                                      prompt predicate require-match history default
                                      keymap category initial narrow add-history annotate
                                      state preview-key sort lookup title group inherit-input-method)
  "Enhanced completing read function selecting from CANDIDATES.

Keyword OPTIONS:

PROMPT is the string which is shown as prompt message in the minibuffer.
PREDICATE is a filter function called for each candidate.
REQUIRE-MATCH equals t means that an exact match is required.
HISTORY is the symbol of the history variable.
DEFAULT is the default selected value.
ADD-HISTORY is a list of items to add to the history.
CATEGORY is the completion category.
SORT should be set to nil if the candidates are already sorted.
LOOKUP is a lookup function passed the input, candidates and candidate string.
ANNOTATE is a function passed a candidate string to return an annotation.
INITIAL is the initial input.
STATE is the state function, see `consult--with-preview'.
GROUP is a completion metadata `group-function'.
PREVIEW-KEY are the preview keys (nil, 'any, a single key or a list of keys).
NARROW is an alist of narrowing prefix strings and description.
KEYMAP is a command-specific keymap.
INHERIT-INPUT-METHOD, if non-nil the minibuffer inherits the input method."
  ;; supported types
  (cl-assert (or (functionp candidates)     ;; async table
                 (obarrayp candidates)      ;; obarray
                 (hash-table-p candidates)  ;; hash table
                 (not candidates)           ;; empty list
                 (stringp (car candidates)) ;; string list
                 (and (consp (car candidates)) (stringp (caar candidates)))   ;; string alist
                 (and (consp (car candidates)) (symbolp (caar candidates))))) ;; symbol alist
  (ignore prompt predicate require-match history default
          keymap category initial narrow add-history annotate
          state preview-key sort lookup title group inherit-input-method)
  (apply #'consult--read-1 candidates
         (append
          (alist-get this-command consult--read-config)
          options
          (list :prompt "Select: "
                :preview-key consult-preview-key
                :sort t
                :lookup (lambda (_input _cands x) x)))))

;;;; Internal API: consult--multi

(defsubst consult--multi-source (cand)
  "Lookup source for CAND from `consult--multi-sources' list."
  (aref consult--multi-sources (consult--tofu-get cand)))

(defun consult--multi-predicate (cand)
  "Predicate function called for each candidate CAND by `consult--multi'."
  (let* ((src (consult--multi-source cand))
         (narrow (plist-get src :narrow))
         (type (or (car-safe narrow) narrow -1)))
    (or (eq consult--narrow type)
        (not (or consult--narrow (plist-get src :hidden))))))

(defun consult--multi-narrow ()
  "Return narrow list used by `consult--multi'."
  (thread-last consult--multi-sources
    (mapcar (lambda (src)
              (when-let (narrow (plist-get src :narrow))
                (if (consp narrow)
                    narrow
                  (when-let (name (plist-get src :name))
                    (cons narrow name))))))
    (delq nil)
    (delete-dups)))

(defun consult--multi-annotate (cand)
  "Annotate candidate CAND, used by `consult--multi'."
  (let* ((src (consult--multi-source cand))
         (annotate (plist-get src :annotate))
         (ann (if annotate
                  (funcall annotate (cdr (get-text-property 0 'consult-multi cand)))
                (plist-get src :name))))
    (and ann (concat consult--multi-align ann))))

(defun consult--multi-group (cand transform)
  "Return title of candidate CAND or TRANSFORM the candidate."
  (if transform
      cand
    (let ((src (consult--multi-source cand)))
      (or (plist-get src :name) (capitalize (symbol-name (plist-get src :category)))))))

(defun consult--multi-preview-key ()
  "Return preview keys, used by `consult--multi'."
  (list :predicate
        (lambda (cand)
          (if (plist-member (cdr cand) :preview-key)
              (plist-get (cdr cand) :preview-key)
            consult-preview-key))
        :keys
        (delete-dups
         (seq-mapcat (lambda (src)
                       (let ((key (if (plist-member src :preview-key)
                                      (plist-get src :preview-key)
                                    consult-preview-key)))
                         (if (listp key) key (list key))))
                     consult--multi-sources))))

(defun consult--multi-lookup (_ candidates cand)
  "Lookup CAND in CANDIDATES, used by `consult--multi'."
  (if-let (found (member cand candidates))
      (cons (cdr (get-text-property 0 'consult-multi (car found)))
            (consult--multi-source cand))
    (unless (string-blank-p cand)
      (list cand))))

(defun consult--multi-candidates ()
  "Return `consult--multi' candidates from `consult--multi-sources'."
  (let ((def) (idx 0) (max-width 0) (candidates))
    (seq-doseq (src consult--multi-sources)
      (let* ((face (plist-get src :face))
             (cat (plist-get src :category))
             (items (plist-get src :items))
             (items (if (functionp items) (funcall items) items)))
        (when (and (not def) (plist-get src :default) items)
          (setq def (consult--tofu-append (car items) idx)))
        (dolist (item items)
          (let ((cand (consult--tofu-append item idx))
                (width (consult--display-width item)))
            (add-text-properties 0 (length item) `(face ,face consult-multi (,cat . ,item)) cand)
            (when (> width max-width) (setq max-width width))
            (push cand candidates))))
      (setq idx (1+ idx)))
    (list def (+ 3 max-width) (nreverse candidates))))

(defun consult--multi-preprocess (sources)
  "Preprocess SOURCES, return vector of sources without disabled sources."
  (vconcat
   (seq-filter (lambda (src)
                 (if-let (pred (plist-get src :enabled))
                     (funcall pred)
                   t))
               (mapcar (lambda (src)
                         (if (symbolp src) (symbol-value src) src))
                       sources))))

(defun consult--multi-state ()
  "State function for `consult--multi'."
  (when-let (states (delq nil (mapcar (lambda (src)
                                        (when-let (fun (plist-get src :state))
                                          (cons src (funcall fun))))
                                      consult--multi-sources)))
    (let ((last-fun))
      (pcase-lambda (`(,cand . ,src) restore)
        (if restore
            ;; Restore all state functions
            (dolist (state states)
              (funcall (cdr state) (and (eq (car state) src) cand) t))
          ;; Get state function to call for preview
          (let ((fun (cdr (assq src states))))
            ;; If the candidate source changed during preview communicate to
            ;; the last source, that none of its candidates is previewed anymore.
            (when (and last-fun (not (eq last-fun fun)))
              (funcall last-fun nil nil))
            (setq last-fun fun)
            ;; Call the state function.
            (when fun
              (funcall fun cand nil))))))))

(defun consult--multi (sources &rest options)
  "Select from candidates taken from a list of SOURCES.

OPTIONS is the plist of options passed to `consult--read'.

The function returns the selected candidate in the form (cons candidate
source-value). The sources of the source list can either be symbols of source
variables or source values. Source values must be plists with the following
fields:

Required source fields:
* :category - Completion category.
* :items - List of strings to select from or function returning list of strings.

Optional source fields:
* :name - Name of the source, used for narrowing, group titles and annotations.
* :narrow - Narrowing character or (character . string) pair.
* :enabled - Function which must return t if the source is enabled.
* :hidden - When t candidates of this source are hidden by default.
* :face - Face used for highlighting the candidates.
* :annotate - Annotation function called for each candidate, returns string.
* :history - Name of history variable to add selected candidate.
* :default - Must be t if the first item of the source is the default value.
* :action - Action function called with the selected candidate.
* :state - State constructor for the source, must return the state function.
* Other source fields can be added specifically to the use case."
  (let* ((consult--multi-sources (consult--multi-preprocess sources))
         (candidates (consult--with-increased-gc
                      (let ((consult--cache))
                        (consult--multi-candidates))))
         (consult--multi-align (propertize
                                " " 'display
                                `(space :align-to (+ left ,(cadr candidates)))))
         (selected (apply #'consult--read
                          (caddr candidates)
                          (append
                           options
                           (list
                            :default     (car candidates)
                            :category    'consult-multi
                            :predicate   #'consult--multi-predicate
                            :annotate    #'consult--multi-annotate
                            :group       #'consult--multi-group
                            :lookup      #'consult--multi-lookup
                            :preview-key (consult--multi-preview-key)
                            :narrow      (consult--multi-narrow)
                            :state       (consult--multi-state))))))
    (when-let (history (plist-get (cdr selected) :history))
      (add-to-history history (car selected)))
    (when-let (action (plist-get (cdr selected) :action))
      (funcall action (car selected)))
    selected))

;;;; Internal API: consult--prompt

(cl-defun consult--prompt-1 (&key prompt history add-history initial default
                                  keymap state preview-key transform inherit-input-method)
  "See `consult--prompt' for documentation."
  (consult--minibuffer-with-setup-hook
      (:append (lambda ()
                 (consult--setup-keymap keymap nil nil preview-key)
                 (setq-local minibuffer-default-add-function
                             (apply-partially #'consult--add-history nil add-history))))
    (car (consult--with-preview preview-key state
                                (lambda (inp _) (funcall transform inp)) (lambda () t)
           (read-from-minibuffer prompt initial nil nil history default inherit-input-method)))))

(cl-defun consult--prompt (&rest options &key prompt history add-history initial default
                                 keymap state preview-key transform inherit-input-method)
  "Read from minibuffer.

Keyword OPTIONS:

PROMPT is the string to prompt with.
TRANSFORM is a function which is applied to the current input string.
HISTORY is the symbol of the history variable.
INITIAL is initial input.
DEFAULT is the default selected value.
ADD-HISTORY is a list of items to add to the history.
STATE is the state function, see `consult--with-preview'.
PREVIEW-KEY are the preview keys (nil, 'any, a single key or a list of keys).
KEYMAP is a command-specific keymap."
  (ignore prompt history add-history initial default
          keymap state preview-key transform inherit-input-method)
  (apply #'consult--prompt-1
         (append
          (alist-get this-command consult--read-config)
          options
          (list :prompt "Input: "
                :preview-key consult-preview-key
                :transform #'identity))))

;;;; Commands

;;;;; Command: consult-multi-occur

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

;;;;; Command: consult-outline

(defun consult--outline-candidates ()
  "Return alist of outline headings and positions."
  (consult--forbid-minibuffer)
  (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (heading-regexp (concat "^\\(?:"
                                 ;; default definition from outline.el
                                 (or (bound-and-true-p outline-regexp) "[*\^L]+")
                                 "\\)"))
         (heading-alist (bound-and-true-p outline-heading-alist))
         (level-fun (or (bound-and-true-p outline-level)
                        (lambda () ;; as in the default from outline.el
                          (or (cdr (assoc (match-string 0) heading-alist))
                              (- (match-end 0) (match-beginning 0))))))
         (candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil t))
        (setq line (+ line (consult--count-lines (match-beginning 0))))
        (push (consult--location-candidate
               (consult--buffer-substring (line-beginning-position)
                                          (line-end-position)
                                          'fontify)
               (point-marker) line 'consult--outline-level (funcall level-fun))
              candidates)
        (unless (eobp) (forward-char 1))))
    (unless candidates
      (user-error "No headings"))
    (nreverse candidates)))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate preview.
The symbol at point is added to the future history."
  (interactive)
  (let* ((cands (consult--with-increased-gc (consult--outline-candidates)))
         (min-level (- (apply #'min (mapcar
                                     (lambda (cand)
                                       (get-text-property 0 'consult--outline-level cand))
                                     cands))
                       ?1))
         (narrow-pred (lambda (cand)
                       (<= (get-text-property 0 'consult--outline-level cand)
                           (+ consult--narrow min-level))))
         (narrow-keys (mapcar (lambda (c) (cons c (format "Level %c" c)))
                              (number-sequence ?1 ?9))))
    (consult--read
     cands
     :prompt "Go to heading: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--line-match
     :narrow `(:predicate ,narrow-pred :keys ,narrow-keys)
     :history '(:input consult--line-history)
     :add-history (thing-at-point 'symbol)
     :state (consult--jump-state))))

;;;;; Command: consult-mark

(defun consult--mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (consult--forbid-minibuffer)
  (unless (marker-position (mark-marker))
    (user-error "No marks"))
  (let ((candidates))
    (save-excursion
      (dolist (marker (cons (mark-marker) mark-ring))
        (let ((pos (marker-position marker)))
          (when (consult--in-range-p pos)
            (goto-char pos)
            ;; `line-number-at-pos' is a very slow function, which should be replaced everywhere.
            ;; However in this case the slow line-number-at-pos does not hurt much, since
            ;; the mark ring is usually small since it is limited by `mark-ring-max'.
            (push (consult--location-candidate
                   (consult--line-with-cursor marker) marker
                   (line-number-at-pos pos consult-line-numbers-widen))
                  candidates)))))
    (nreverse (delete-dups candidates))))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in the buffer-local `mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
   (consult--with-increased-gc (consult--mark-candidates))
   :prompt "Go to mark: "
   :annotate (consult--line-prefix)
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--line-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

;;;;; Command: consult-global-mark

(defun consult--global-mark-candidates ()
  "Return alist of lines containing markers.

The alist contains (string . position) pairs."
  (consult--forbid-minibuffer)
  (let ((candidates))
    (save-excursion
      (dolist (marker global-mark-ring)
        (let ((pos (marker-position marker))
              (buf (marker-buffer marker)))
          (when (and pos (buffer-live-p buf) (not (minibufferp buf)))
            (with-current-buffer buf
              (when (consult--in-range-p pos)
                (goto-char pos)
                ;; `line-number-at-pos' is slow, see comment in `consult--mark-candidates'.
                (let ((line (line-number-at-pos pos consult-line-numbers-widen)))
                  (push (concat
                         (propertize (consult--format-location (buffer-name buf) line)
                                     'consult-location (cons marker line)
                                     'consult-strip t)
                         (consult--line-with-cursor marker)
                         (consult--tofu-encode marker))
                        candidates))))))))
    (unless candidates
      (user-error "No global marks"))
    (nreverse (delete-dups candidates))))

;;;###autoload
(defun consult-global-mark ()
  "Jump to a marker in `global-mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
   (consult--with-increased-gc (consult--global-mark-candidates))
   :prompt "Go to global mark: "
   ;; Despite `consult-global-mark' formating the candidates in grep-like
   ;; style, we are not using the 'consult-grep category, since the candidates
   ;; have location markers attached.
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--line-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

;;;;; Command: consult-line

(defun consult--line-candidates (top)
  "Return list of line candidates; start from top if TOP non-nil."
  (consult--forbid-minibuffer)
  (consult--fontify-all)
  (let* ((default-cand)
         (candidates)
         (line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (curr-line (line-number-at-pos (point) consult-line-numbers-widen)))
    (consult--each-line beg end
      (let ((str (consult--buffer-substring beg end)))
        (unless (string-blank-p str)
          (push (consult--location-candidate str (point-marker) line) candidates)
          (when (and (not default-cand) (>= line curr-line))
            (setq default-cand candidates)))
        (setq line (1+ line))))
    (unless candidates
      (user-error "No lines"))
    (nreverse
     (if (or top (not default-cand))
         candidates
       (let ((before (cdr default-cand)))
         (setcdr default-cand nil)
         (nconc before candidates))))))

(defun consult--line-match (input candidates cand)
  "Lookup position of match.

INPUT is the input string entered by the user.
CANDIDATES is the line candidates alist.
CAND is the currently selected candidate."
  (when-let (pos (consult--lookup-location input candidates cand))
    (if (or (string-blank-p input)
            (eq consult-line-point-placement 'line-beginning))
        pos
      (let ((beg 0)
            (end (length cand))
            (high (+ consult--tofu-char consult--tofu-range -1)))
        ;; Ignore tofu-encoded unique line number suffix
        (while (and (> end 0) (<= consult--tofu-char (aref cand (1- end)) high))
          (setq end (1- end)))
        ;; Find match end position, remove characters from line end until
        ;; matching fails
        (let ((step 16))
          (while (> step 0)
            (while (and (> (- end step) 0)
                        ;; Use consult-location completion category when
                        ;; filtering lines. Highlighting is not necessary here,
                        ;; but it is actually cheaper to highlight a single
                        ;; candidate, since setting up deferred highlighting is
                        ;; costly.
                        (consult--completion-filter input
                                                    (list (substring cand 0 (- end step)))
                                                    'consult-location 'highlight))
              (setq end (- end step)))
            (setq step (/ step 2))))
        ;; Find match beginning position, remove characters from line beginning
        ;; until matching fails
        (when (eq consult-line-point-placement 'match-beginning)
          (let ((step 16))
            (while (> step 0)
              (while (and (< (+ beg step) end)
                          ;; See comment above, call to `consult--completion-filter'.
                          (consult--completion-filter input
                                                      (list (substring cand (+ beg step) end))
                                                      'consult-location 'highlight))
                (setq beg (+ beg step)))
              (setq step (/ step 2)))
            (setq end beg)))
        ;; Marker can be dead
        (ignore-errors (+ pos end))))))

;;;###autoload
(defun consult-line (&optional initial start)
  "Search for a matching line and jump to the line beginning.

The default candidate is the non-empty line next to point.
This command obeys narrowing. Optional INITIAL input can be provided.
The search starting point is changed if the START prefix argument is set.
The symbol at point and the last `isearch-string' is added to the future history."
  (interactive (list nil (not (not current-prefix-arg))))
  (let ((candidates (consult--with-increased-gc
                     (consult--line-candidates
                      (not (eq start consult-line-start-from-top))))))
    (consult--read
     candidates
     :prompt "Go to line: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last isearch string to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add isearch-string as initial input if starting from isearch
     :initial (or initial
                  (and isearch-mode (prog1 isearch-string (isearch-done))))
     :state (consult--jump-state))))

;;;;; Command: consult-keep-lines

(defun consult--keep-lines-state (filter)
  "State function for `consult-keep-lines' with FILTER function."
  (let* ((lines)
         (buffer-orig (current-buffer))
         (font-lock-orig font-lock-mode)
         (hl-line-orig (bound-and-true-p hl-line-mode))
         (point-orig (point))
         (content-orig)
         (replace)
         (last-input))
    (if (use-region-p)
        (save-restriction
          ;; Use the same behavior as `keep-lines'.
          (let ((rbeg (region-beginning))
                (rend (save-excursion
		        (goto-char (region-end))
		        (unless (or (bolp) (eobp))
		          (forward-line 0))
		        (point))))
            (consult--fontify-region rbeg rend)
            (narrow-to-region rbeg rend)
            (consult--each-line beg end
              (push (consult--buffer-substring beg end) lines))
            (setq content-orig (buffer-string)
                  replace (lambda (content &optional pos)
                            (delete-region rbeg rend)
                            (insert content)
                            (goto-char (or pos rbeg))
                            (setq rend (+ rbeg (length content)))
                            (add-face-text-property rbeg rend 'region t)))))
      (consult--fontify-all)
      (setq content-orig (buffer-string)
            replace (lambda (content &optional pos)
                      (delete-region (point-min) (point-max))
                      (insert content)
                      (goto-char (or pos (point-min)))))
      (consult--each-line beg end
        (push (consult--buffer-substring beg end) lines)))
    (setq lines (nreverse lines))
    (lambda (input restore)
      (with-current-buffer buffer-orig
        ;; Restoring content and point position
        (when (and restore last-input)
          ;; No undo recording, modification hooks, buffer modified-status
          (with-silent-modifications (funcall replace content-orig point-orig)))
        ;; Committing or new input provided -> Update
        (when (and input ;; Input has been povided
                   (or
                    ;; Committing, but not with empty input
                    (and restore (not (string-match-p "\\`!? ?\\'" input)))
                    ;; Input has changed
                    (not (equal input last-input))))
          (let ((filtered-content
                 (if (string-match-p "\\`!? ?\\'" input)
                     ;; Special case the empty input for performance.
                     ;; Otherwise it could happen that the minibuffer is empty,
                     ;; but the buffer has not been updated.
                     content-orig
                   (if restore
                       (apply #'concat (mapcan (lambda (x) (list x "\n"))
                                               (funcall filter input lines)))
                     (while-no-input
                       ;; Heavy computation is interruptible if *not* committing!
                       ;; Allocate new string candidates since the matching function mutates!
                       (apply #'concat (mapcan (lambda (x) (list x "\n"))
                                               (funcall filter input (mapcar #'copy-sequence lines)))))))))
            (when (stringp filtered-content)
              (when font-lock-mode (font-lock-mode -1))
              (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
              (if restore
                  (atomic-change-group
                    ;; Disable modification hooks for performance
                    (let ((inhibit-modification-hooks t))
                      (funcall replace filtered-content)))
                ;; No undo recording, modification hooks, buffer modified-status
                (with-silent-modifications
                  (funcall replace filtered-content)
                  (setq last-input input))))))
        ;; Restore modes
        (when restore
          (when hl-line-orig (hl-line-mode 1))
          (when font-lock-orig (font-lock-mode 1)))))))

;;;###autoload
(defun consult-keep-lines (&optional filter initial)
  "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. When
called from elisp, the filtering is performed by a FILTER function. This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input."
  (interactive
   (list (lambda (pattern cands)
           ;; Use consult-location completion category when filtering lines
           (consult--completion-filter-dispatch
            pattern cands 'consult-location 'highlight))))
  (consult--forbid-minibuffer)
  (barf-if-buffer-read-only)
  (consult--with-increased-gc
   (consult--prompt
    :prompt "Keep lines: "
    :initial initial
    :history 'consult--keep-lines-history
    :state (consult--keep-lines-state filter))))

;;;;; Command: consult-focus-lines

(defun consult--focus-lines-state (filter)
  "State function for `consult-focus-lines' with FILTER function."
  (let ((lines) (overlays) (last-input) (point-orig (point)))
    (save-excursion
      (save-restriction
        (if (not (use-region-p))
            (consult--fontify-all)
          (consult--fontify-region (region-beginning) (region-end))
          (narrow-to-region
           (region-beginning)
           ;; Behave the same as `keep-lines'.
           ;; Move to the next line.
           (save-excursion
	     (goto-char (region-end))
	     (unless (or (bolp) (eobp))
	       (forward-line 0))
	     (point))))
        (consult--each-line beg end
          (push (buffer-substring-no-properties beg end) lines)
          (push (make-overlay beg (1+ end)) overlays))))
    (unless (use-region-p)
      (goto-char (point-min)))
    (lambda (input restore)
      ;; New input provided -> Update
      (when (and input (not (equal input last-input)))
        (if (string-match-p "\\`!? ?\\'" input)
            ;; Special case the empty input for performance.
            (progn
              (dolist (ov overlays)
                (overlay-put ov 'invisible nil))
              (setq last-input input))
          (let* ((not (string-prefix-p "! " input))
                 (stripped (string-remove-prefix "! " input))
                 ;; Heavy computation is interruptible if *not* committing!
                 (ht (if restore
                         (consult--string-hash (funcall filter stripped lines))
                       (while-no-input
                         (consult--string-hash (funcall filter stripped lines))))))
            (when (hash-table-p ht)
              (let ((ov overlays) (li lines))
                (while ov
                  (overlay-put (car ov) 'invisible (eq not (gethash (car li) ht)))
                  (setq li (cdr li) ov (cdr ov))))
              (setq last-input input)))))
      (when restore
        (if (not input)
            (goto-char point-orig)
          ;; Sucessfully terminated -> Remember invisible overlays
          (dolist (ov overlays)
            (if (overlay-get ov 'invisible)
                (push ov consult--focus-lines-overlays)
              (delete-overlay ov)))
          (setq overlays nil))
        ;; Destroy remaining overlays
        (mapc #'delete-overlay overlays)))))

;;;###autoload
(defun consult-focus-lines (&optional show filter initial)
  "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. With
optional prefix argument SHOW reveal the hidden lines. When called from elisp,
the filtering is performed by a FILTER function. This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input."
  (interactive
   (list current-prefix-arg
         (lambda (pattern cands)
           ;; Use consult-location completion category when filtering lines
           (consult--completion-filter-dispatch
            pattern cands 'consult-location nil))))
  (consult--forbid-minibuffer)
  (if show
      (progn
        (mapc #'delete-overlay consult--focus-lines-overlays)
        (setq consult--focus-lines-overlays nil))
    (consult--with-increased-gc
     (consult--prompt
      :prompt "Focus on lines: "
      :initial initial
      :history 'consult--keep-lines-history
      :state (consult--focus-lines-state filter)))))

;;;;; Command: consult-goto-line

(defun consult--goto-line-position (str msg)
  "Transform input STR to line number.
Print an error message with MSG function."
  (if-let (line (and str
                     (string-match-p "\\`[[:digit:]]+\\'" str)
                     (string-to-number str)))
      (let ((pos (save-excursion
                   (save-restriction
                     (when consult-line-numbers-widen
                       (widen))
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (point)))))
        (if (consult--in-range-p pos)
            pos
          (funcall msg "Line number out of range.")
          nil))
    (when (and str (not (string= str "")))
      (funcall msg "Please enter a number."))
    nil))

;;;###autoload
(defun consult-goto-line (&optional arg)
  "Read line number and jump to the line with preview.

Jump directly if a line number is given as prefix ARG. The command respects
narrowing and the settings `consult-goto-line-numbers' and
`consult-line-numbers-widen'."
  (interactive "P")
  (if arg
      (call-interactively #'goto-line)
    (consult--forbid-minibuffer)
    (consult--local-let ((display-line-numbers consult-goto-line-numbers)
                         (display-line-numbers-widen consult-line-numbers-widen))
      (while (if-let (pos (consult--goto-line-position
                           (consult--prompt
                            :prompt "Go to line: "
                            :state (let ((preview (consult--jump-preview)))
                                     (lambda (str restore)
                                       (funcall preview
                                                (consult--goto-line-position str #'ignore)
                                                restore))))
                           #'minibuffer-message))
                 (consult--jump pos)
               t)))))

;;;;; Command: consult-recent-file

(defun consult--file-preview ()
  "Create preview function for files."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (cand restore)
      (if restore
          (progn
            (funcall preview nil t)
            (funcall open))
        (funcall preview (and cand (funcall open cand)) nil)))))

(defun consult--file-action (file)
  "Open FILE via `consult--buffer-action'."
  (consult--buffer-action (find-file-noselect file)))

(consult--define-state file)

;;;###autoload
(defun consult-recent-file ()
  "Find recent using `completing-read'."
  (interactive)
  (find-file
   (consult--read
    (or (mapcar #'abbreviate-file-name recentf-list)
        (user-error "No recent files"))
    :prompt "Find recent file: "
    :sort nil
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))

;;;;; Command: consult-file-externally

;;;###autoload
(defun consult-file-externally (file)
  "Open FILE externally using the default application of the system."
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

;;;;; Command: consult-completion-in-region

(defun consult--insertion-preview (start end)
  "State function for previewing a candidate in a specific region.
The candidates are previewed in the region from START to END. This function is
used as the `:state' argument for `consult--read' in the `consult-yank' family
of functions and in `consult-completion-in-region'."
  (unless (minibufferp)
    (let (ov)
      (lambda (cand restore)
        (if restore
            (when ov (delete-overlay ov))
          (unless ov (setq ov (consult--overlay start end
                                                'invisible t
                                                'window (selected-window))))
          ;; Use `add-face-text-property' on a copy of "cand in order to merge face properties
          (setq cand (copy-sequence cand))
          (add-face-text-property 0 (length cand) 'consult-preview-insertion t cand)
          ;; Use the `before-string' property since the overlay might be empty.
          (overlay-put ov 'before-string cand))))))

;; Use minibuffer completion as the UI for completion-at-point
;;;###autoload
(defun consult-completion-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer"
  (cl-letf* ((config (alist-get #'consult-completion-in-region consult--read-config))
             ;; Overwrite both the local and global value of `completion-styles', such that the
             ;; `completing-read' minibuffer sees the overwritten value in any case. This is
             ;; necessary if `completion-styles' is buffer-local.
             ;; NOTE: The completion-styles will be overwritten for recursive editing sessions!
             (cs (or (plist-get config :completion-styles) completion-styles))
             (completion-styles cs)
             ((default-value 'completion-styles) cs)
             (prompt (or (plist-get config :prompt) "Completion: "))
             (require-match (plist-get config :require-match))
             (preview-key (if (plist-member config :preview-key)
                              (plist-get config :preview-key)
                            consult-preview-key))
             (initial (buffer-substring-no-properties start end))
             (metadata (completion-metadata initial collection predicate))
             (threshold (or (plist-get config :cycle-threshold) (completion--cycle-threshold metadata)))
             (all (completion-all-completions initial collection predicate (length initial))))
    ;; error if `threshold' is t or the improper list `all' is too short
    (if (and threshold
	     (or (not (consp (ignore-errors (nthcdr threshold all))))
		 (and completion-cycling completion-all-sorted-completions)))
        (completion--in-region start end collection predicate)
      (let* ((limit (car (completion-boundaries initial collection predicate "")))
             (category (completion-metadata-get metadata 'category))
             (exit-status 'finished)
             (completion
              (cond
               ((atom all) nil)
               ((and (consp all) (atom (cdr all)))
                (setq exit-status 'sole)
                (concat (substring initial 0 limit) (car all)))
               (t (car
                   (consult--with-preview
                       preview-key
                       ;; preview state
                       (consult--insertion-preview start end)
                       ;; transformation function
                       (if (eq category 'file)
                           (if (file-name-absolute-p initial)
                               (lambda (_inp cand) (substitute-in-file-name cand))
                             (lambda (_inp cand) (file-relative-name (substitute-in-file-name cand))))
                         (lambda (_inp cand) cand))
                       ;; candidate function
                       (apply-partially #'run-hook-with-args-until-success
                                        'consult--completion-candidate-hook)
                     (let ((enable-recursive-minibuffers t))
                       (if (eq category 'file)
                           ;; When completing files with consult-completion-in-region, the point in the
                           ;; minibuffer gets placed initially at the beginning of the last path component.
                           ;; By using the filename as DIR argument (second argument of read-file-name), it
                           ;; starts at the end of minibuffer contents, as for other types of completion.
                           ;; However this is undefined behavior since initial does not only contain the
                           ;; directory, but also the filename.
                           (read-file-name prompt initial initial require-match nil predicate)
                         (completing-read prompt collection predicate require-match initial)))))))))
        (if completion
            (progn
              (delete-region start end)
              (insert (substring-no-properties completion))
              (when-let (exit (plist-get completion-extra-properties :exit-function))
                (funcall exit completion exit-status))
              t)
          (message "No completion")
          nil)))))

;;;;; Command: consult-mode-command

(defun consult--mode-name (mode)
  "Return name part of MODE."
  (replace-regexp-in-string
   "global-\\(.*\\)-mode" "\\1"
   (replace-regexp-in-string
    "\\(-global\\)?-mode\\'" ""
    (if (eq mode 'c-mode)
        "cc"
      (symbol-name mode))
    'fixedcase)
   'fixedcase))

(defun consult--mode-command-candidates (modes)
  "Extract commands from MODES.

The list of features is searched for files belonging to the modes.
From these files, the commands are extracted."
  (let* ((command-filter (consult--regexp-filter (seq-filter #'stringp consult-mode-command-filter)))
         (feature-filter (seq-filter #'symbolp consult-mode-command-filter))
         (minor-hash (consult--string-hash minor-mode-list))
         (minor-local-modes (seq-filter (lambda (m)
                                          (and (gethash m minor-hash)
                                               (local-variable-if-set-p m)))
                                        modes))
         (minor-global-modes (seq-filter (lambda (m)
                                           (and (gethash m minor-hash)
                                                (not (local-variable-if-set-p m))))
                                         modes))
         (major-modes (seq-remove (lambda (m)
                                    (gethash m minor-hash))
                                  modes))
         (major-paths-hash (consult--string-hash (mapcar #'symbol-file major-modes)))
         (minor-local-paths-hash (consult--string-hash (mapcar #'symbol-file minor-local-modes)))
         (minor-global-paths-hash (consult--string-hash (mapcar #'symbol-file minor-global-modes)))
         (major-name-regexp (regexp-opt (mapcar #'consult--mode-name major-modes)))
         (minor-local-name-regexp (regexp-opt (mapcar #'consult--mode-name minor-local-modes)))
         (minor-global-name-regexp (regexp-opt (mapcar #'consult--mode-name minor-global-modes)))
         (commands))
    (dolist (feature load-history commands)
      (when-let (name (alist-get 'provide feature))
        (let* ((path (car feature))
               (file (file-name-nondirectory path))
               (key (cond
                     ((memq name feature-filter) nil)
                     ((or (gethash path major-paths-hash)
                          (string-match-p major-name-regexp file))
                      ?m)
                     ((or (gethash path minor-local-paths-hash)
                          (string-match-p minor-local-name-regexp file))
                      ?l)
                     ((or (gethash path minor-global-paths-hash)
                          (string-match-p minor-global-name-regexp file))
                      ?g))))
          (when key
            (dolist (cmd (cdr feature))
              (when (and (consp cmd)
                         (eq (car cmd) 'defun)
                         (commandp (cdr cmd))
                         (not (get (cdr cmd) 'byte-obsolete-info)))
                (let ((name (symbol-name (cdr cmd))))
                  (unless (string-match-p command-filter name)
                    (push (propertize name
                                      'consult--candidate (cdr cmd)
                                      'consult--type key)
                          commands)))))))))))

(defconst consult--mode-command-narrow
  '((?m . "Major")
    (?l . "Local Minor")
    (?g . "Global Minor")))

;;;###autoload
(defun consult-mode-command (&rest modes)
  "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes."
  (interactive)
  (unless modes
    (setq modes (cons major-mode
                      (seq-filter (lambda (m)
                                    (and (boundp m) (symbol-value m)))
                                  minor-mode-list))))
  (command-execute
   (consult--read
    (consult--mode-command-candidates modes)
    :prompt "Mode command: "
    :predicate
    (lambda (cand)
      (let ((key (get-text-property 0 'consult--type cand)))
        (if consult--narrow
            (= key consult--narrow)
          (/= key ?g))))
    :lookup #'consult--lookup-candidate
    :group (consult--type-group consult--mode-command-narrow)
    :narrow consult--mode-command-narrow
    :require-match t
    :history 'consult--mode-command-history
    :category 'command)))

;;;;; Command: consult-yank

(defun consult--read-from-kill-ring ()
  "Open kill ring menu and return selected string."
  ;; Do not specify a :lookup function in order to preserve completion-styles
  ;; highlighting of the current candidate. We have to perform a final lookup
  ;; to obtain the original candidate which may be propertized with
  ;; yank-specific properties, like 'yank-handler.
  (consult--lookup-member
   nil kill-ring
   (consult--read
    (consult--remove-dups kill-ring)
    :prompt "Yank from kill-ring: "
    :history t ;; disable history
    :sort nil
    :category 'consult-yank
    :require-match t
    :state
    (consult--insertion-preview
     (point)
     ;; If previous command is yank, hide previously yanked string
     (or (and (eq last-command 'yank) (mark t)) (point))))))

;; Adapted from the Emacs `yank-from-kill-ring' function.
;;;###autoload
(defun consult-yank-from-kill-ring (string &optional arg)
  "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string."
  (interactive (list (consult--read-from-kill-ring) current-prefix-arg))
  (when string
    (setq yank-window-start (window-start))
    (push-mark)
    (insert-for-yank string)
    (setq this-command 'yank)
    (when (consp arg)
      ;; Swap point and mark like in `yank'.
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer)))))))

(define-obsolete-function-alias
  'consult-yank
  'consult-yank-from-kill-ring
  "0.7")

(put 'consult-yank-replace 'delete-selection 'yank)
(put 'consult-yank-pop 'delete-selection 'yank)
(put 'consult-yank 'delete-selection 'yank)
(put 'consult-yank-from-kill-ring 'delete-selection 'yank)

;;;###autoload
(defun consult-yank-pop (&optional arg)
  "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop (or arg 1))
    (call-interactively #'consult-yank-from-kill-ring)))

;; Adapted from the Emacs yank-pop function.
;;;###autoload
(defun consult-yank-replace (string)
  "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28."
  (interactive (list (consult--read-from-kill-ring)))
  (when string
    (if (not (eq last-command 'yank))
        (consult-yank-from-kill-ring string)
      (let ((inhibit-read-only t)
            (pt (point))
            (mk (mark t)))
        (setq this-command 'yank)
        (funcall (or yank-undo-function 'delete-region) (min pt mk) (max pt mk))
        (setq yank-undo-function nil)
        (set-marker (mark-marker) pt (current-buffer))
        (insert-for-yank string)
        (set-window-start (selected-window) yank-window-start t)
        (if (< pt mk)
            (goto-char (prog1 (mark t)
                         (set-marker (mark-marker) (point) (current-buffer)))))))))

;;;;; Command: consult-bookmark

(defun consult--bookmark-preview ()
  "Create preview function for bookmarks."
  (let ((preview (consult--jump-preview))
        (open (consult--temporary-files)))
    (lambda (cand restore)
      (if restore
          (progn
            (funcall open)
            (funcall preview nil t))
        (funcall
         preview
         (when-let (bm (and cand
                            (bookmark-get-bookmark-record
                             (assoc cand bookmark-alist))))
           (let ((handler (alist-get 'handler bm #'bookmark-default-handler)))
             ;; Only preview bookmarks with the default handler.
             (if-let* ((file (and (eq handler #'bookmark-default-handler)
                                  (alist-get 'filename bm)))
                       (pos (alist-get 'position bm))
                       (buf (funcall open file)))
                 (set-marker (make-marker) pos buf)
               (message "No preview for %s" handler)
               nil)))
         nil)))))

(defun consult--bookmark-action (bm)
  "Open BM via `consult--buffer-action'."
  (bookmark-jump bm consult--buffer-display))

(consult--define-state bookmark)

(defun consult--bookmark-candidates ()
  "Return bookmark candidates."
  (bookmark-maybe-load-default-file)
  (let ((narrow (mapcar (pcase-lambda (`(,y ,_ ,x)) (cons x y))
                        consult-bookmark-narrow)))
    (mapcar (lambda (cand)
              (let ((bm (bookmark-get-bookmark-record cand)))
                (propertize (car cand)
                            'consult--type
                            (alist-get
                             (alist-get 'handler bm #'bookmark-default-handler)
                             narrow))))
            bookmark-alist)))

;;;###autoload
(defun consult-bookmark (name)
  "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration."
  (interactive
   (list
    (let ((narrow (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                          consult-bookmark-narrow)))
      (consult--read
       (consult--bookmark-candidates)
       :prompt "Bookmark: "
       :state (consult--bookmark-preview)
       :category 'bookmark
       :history 'bookmark-history
       ;; Add default names to future history.
       ;; Ignore errors such that `consult-bookmark' can be used in
       ;; buffers which are not backed by a file.
       :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
       :group (consult--type-group narrow)
       :narrow (consult--type-narrow narrow)))))
  (bookmark-maybe-load-default-file)
  (if (assoc name bookmark-alist)
      (bookmark-jump name)
    (bookmark-set name)))

;;;;; Command: consult-apropos

;;;###autoload
(defun consult-apropos ()
  "Select pattern and call `apropos'.

The default value of the completion is the symbol at point."
  (interactive)
  (let ((pattern
         (consult--read
          obarray
          :prompt "Apropos: "
          :predicate (lambda (x) (or (fboundp x) (boundp x) (facep x) (symbol-plist x)))
          :history 'consult--apropos-history
          :category 'symbol
          :default (thing-at-point 'symbol))))
    (when (string= pattern "")
      (user-error "No pattern given"))
    (apropos pattern)))

;;;;; Command: consult-complex-command

;;;###autoload
(defun consult-complex-command ()
  "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'."
  (interactive)
  (let* ((history (or (delete-dups (mapcar #'prin1-to-string command-history))
                      (user-error "There are no previous complex commands")))
         (cmd (read (consult--read
                     history
                     :prompt "Command: "
                     :default (car history)
                     :sort nil
                     :history t ;; disable history
                     :category 'expression))))
    ;; Taken from `repeat-complex-command'
    (add-to-history 'command-history cmd)
    (apply #'funcall-interactively
	   (car cmd)
	   (mapcar (lambda (e) (eval e t)) (cdr cmd)))))

;;;;; Command: consult-history

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
    (if (eq minibuffer-history-variable t)
        (user-error "Minibuffer history is disabled for `%s'" this-command)
      (symbol-value minibuffer-history-variable))) ;; (minibuffer-history-value) is Emacs 27 only
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

(declare-function ring-elements "ring")
;; This command has been adopted from https://github.com/oantolin/completing-history/.
;;;###autoload
(defun consult-history (&optional history)
  "Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument."
  (interactive)
  (let ((str (consult--local-let ((enable-recursive-minibuffers t))
               (consult--read
                (let ((history (or history (consult--current-history))))
                  (or (consult--remove-dups (if (ring-p history)
                                                (ring-elements history)
                                              history))
                      (user-error "History is empty")))
                :prompt "History: "
                :history t ;; disable history
                :category ;; Report command category for M-x history
                (and (minibufferp)
                     (eq minibuffer-history-variable 'extended-command-history)
                     'command)
                :state
                (consult--insertion-preview (point) (point))
                :sort nil))))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (insert (substring-no-properties str))))

;;;;; Command: consult-isearch

(defun consult-isearch-forward (&optional reverse)
  "Continue isearch forward optionally in REVERSE."
  (interactive)
  (consult--require-minibuffer)
  (setq isearch-new-forward (not reverse) isearch-new-nonincremental nil)
  (funcall (or (command-remapping #'exit-minibuffer) #'exit-minibuffer)))

(defun consult-isearch-reverse (&optional reverse)
  "Continue isearch backward optionally in REVERSE."
  (interactive)
  (consult-isearch-forward (not reverse)))

(defvar consult-isearch-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'consult-isearch-forward)
    (define-key map "\C-r" #'consult-isearch-reverse)
    map)
  "Additional keymap used by `consult-isearch'.")

(defun consult--isearch-candidates ()
  "Return isearch history candidates."
  ;; NOTE: Do not throw an error on empty history,
  ;; in order to allow starting a search.
  ;; We do not :require-match here!
  (let ((history (if (eq t search-default-mode)
                     (append regexp-search-ring search-ring)
                   (append search-ring regexp-search-ring))))
    (cons
     (delete-dups
      (mapcar
       (lambda (cand)
         ;; Emacs 27.1 uses settings on the search string, we can use that for narrowing.
         (let* ((props (plist-member (text-properties-at 0 cand)
                                     'isearch-regexp-function))
                (type (pcase (cadr props)
                        ((and 'nil (guard (not props))) ?r)
                        ('nil                           ?l)
                        ('word-search-regexp            ?w)
                        ('isearch-symbol-regexp         ?s)
                        ('char-fold-to-regexp           ?c)
                        (_                              ?u))))
           ;; Disambiguate history items. The same string could
           ;; occur with different search types.
           (consult--tofu-append cand type)))
       history))
     (if history
         (+ 4 (apply #'max (mapcar #'length history)))
       0))))

(defconst consult--isearch-narrow
  '((?c . "Char")
    (?u . "Custom")
    (?l . "Literal")
    (?r . "Regexp")
    (?s . "Symbol")
    (?w . "Word")))

;;;###autoload
(defun consult-isearch ()
  "Read a search string with completion from history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise."
  (interactive)
  (consult--forbid-minibuffer)
  (let* ((isearch-message-function 'ignore) ;; Avoid flicker in echo area
         (inhibit-redisplay t)              ;; Avoid flicker in mode line
         (candidates (consult--isearch-candidates))
         (align (propertize " " 'display `(space :align-to (+ left ,(cdr candidates))))))
    (unless isearch-mode (isearch-mode t))
    (with-isearch-suspended
     (setq isearch-new-string
           (consult--read
            (car candidates)
            :prompt "I-search: "
            :category 'consult-isearch
            :history t ;; disable history
            :sort nil
            :initial isearch-string
            :keymap consult-isearch-map
            :annotate
            (lambda (cand)
              (concat align (alist-get (consult--tofu-get cand) consult--isearch-narrow)))
            :group
            (lambda (cand transform)
              (if transform
                  cand
                (alist-get (consult--tofu-get cand) consult--isearch-narrow)))
            :lookup
            (lambda (_ candidates str)
              (if-let (found (member str candidates)) (substring (car found) 0 -1) str))
            :state
            (lambda (cand restore)
              (unless restore
                (setq isearch-string cand)
                ;; Emacs 27.1 uses properties on the search string to store settings
                (when (fboundp 'isearch-update-from-string-properties)
                  (isearch-update-from-string-properties cand))
                (isearch-update)))
            :narrow
            (list :predicate
                  (lambda (cand) (= (consult--tofu-get cand) consult--narrow))
                  :keys consult--isearch-narrow))
           isearch-new-message
           (mapconcat 'isearch-text-char-description isearch-new-string "")))
    ;; Setting `isearch-regexp' etc only works outside of `with-isearch-suspended'.
    (unless (plist-member (text-properties-at 0 isearch-string) 'isearch-regexp-function)
      (setq isearch-regexp t
            isearch-regexp-function nil))))

;;;;; Command: consult-minor-mode-menu

(defun consult--minor-mode-candidates ()
  "Return list of minor-mode candidate strings."
  (mapcar
   (pcase-lambda (`(,name . ,sym))
     (propertize
      name
      'consult--candidate sym
      'consult--minor-mode-narrow
      (logior
       (lsh (if (local-variable-if-set-p sym) ?l ?g) 8)
       (if (and (boundp sym) (symbol-value sym)) ?i ?o))
      'consult--minor-mode-group
      (concat
       (if (local-variable-if-set-p sym) "Local " "Global ")
       (if (and (boundp sym) (symbol-value sym)) "On" "Off"))))
   (nconc
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

(defconst consult--minor-mode-menu-narrow
  '((?l . "Local")
    (?g . "Global")
    (?i . "On")
    (?o . "Off")))

;;;###autoload
(defun consult-minor-mode-menu ()
  "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'."
  (interactive)
  (call-interactively
   (consult--read
    (consult--minor-mode-candidates)
    :prompt "Minor mode: "
    :require-match t
    :category 'minor-mode
    :group
    (lambda (cand transform)
      (if transform cand (get-text-property 0 'consult--minor-mode-group cand)))
    :narrow
    (list :predicate
          (lambda (cand)
            (let ((narrow (get-text-property 0 'consult--minor-mode-narrow cand)))
              (or (= (logand narrow 255) consult--narrow)
                  (= (lsh narrow -8) consult--narrow))))
          :keys
          consult--minor-mode-menu-narrow)
    :lookup #'consult--lookup-candidate
    :history 'consult--minor-mode-menu-history)))

;;;;; Command: consult-theme

;;;###autoload
(defun consult-theme (theme)
  "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme."
  (interactive
   (list
    (let ((avail-themes (seq-filter (lambda (x) (or (not consult-themes)
                                                    (memq x consult-themes)))
                                    (cons nil (custom-available-themes))))
          (saved-theme (car custom-enabled-themes)))
      (consult--read
       (mapcar (lambda (x) (if x (symbol-name x) "default")) avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (_input _cands x)
                 (and x (not (equal x "default")) (intern-soft x)))
       :state (lambda (cand restore)
                (cond
                 ((and restore (not cand)) (consult-theme saved-theme))
                 ((memq cand avail-themes) (consult-theme cand))))
       :default (symbol-name (or saved-theme 'default))))))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;;;;; Command: consult-buffer

(consult--define-cache consult--cached-buffers
  (let ((buffers (delq (current-buffer) (buffer-list)))
        (visible-p (lambda (x) (get-buffer-window x 'visible))))
    (nconc (seq-remove visible-p buffers)
           (seq-filter visible-p buffers)
           (list (current-buffer)))))

(consult--define-cache consult--cached-buffer-names
  (mapcar #'buffer-name (consult--cached-buffers)))

(consult--define-cache consult--cached-buffer-file-hash
  (consult--string-hash (delq nil (mapcar #'buffer-file-name (consult--cached-buffers)))))

(defun consult--buffer-preview ()
  "Buffer preview function."
  (let ((orig-buf (current-buffer)))
    (lambda (cand restore)
      (when (and (not restore)
                 ;; Only preview in current window and other window.
                 ;; Preview in frames and tabs is not possible since these don't get cleaned up.
                 (or (eq consult--buffer-display #'switch-to-buffer)
                     (eq consult--buffer-display #'switch-to-buffer-other-window)))
        (cond
         ((and cand (get-buffer cand)) (consult--buffer-action cand 'norecord))
         ((buffer-live-p orig-buf) (consult--buffer-action orig-buf 'norecord)))))))

(defun consult--buffer-action (buffer &optional norecord)
  "Switch to BUFFER via `consult--buffer-display' function.
If NORECORD is non-nil, do not record the buffer switch in the buffer list."
  (funcall consult--buffer-display buffer norecord))

(consult--define-state buffer)

(defvar consult--source-bookmark
  `(:name     "Bookmark"
    :narrow   ?m
    :category bookmark
    :face     consult-bookmark
    :history  bookmark-history
    :items    ,#'bookmark-all-names
    :state    ,#'consult--bookmark-state)
  "Bookmark candidate source for `consult-buffer'.")

(defvar consult--source-project-buffer
  `(:name     "Project Buffer"
    :narrow   (?p . "Project")
    :hidden   t
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :enabled  ,(lambda () consult-project-root-function)
    :items
    ,(lambda ()
       (when-let (root (consult--project-root))
         (mapcar #'buffer-name
                 (seq-filter (lambda (x)
                               (when-let (file (buffer-file-name x))
                                 (string-prefix-p root file)))
                             (consult--cached-buffers))))))
  "Project buffer candidate source for `consult-buffer'.")

(defvar consult--source-project-file
  `(:name     "Project File"
    :narrow   (?p . "Project")
    :hidden   t
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (and consult-project-root-function
                                recentf-mode))
    :items
    ,(lambda ()
      (when-let (root (consult--project-root))
        (let ((len (length root))
              (inv-root (propertize root 'invisible t))
              (ht (consult--cached-buffer-file-hash)))
          (mapcar (lambda (x)
                    (concat inv-root (substring x len)))
                  (seq-filter (lambda (x)
                                (and (not (gethash x ht))
                                     (string-prefix-p root x)))
                              recentf-list))))))
  "Project file candidate source for `consult-buffer'.")

(defvar consult--source-hidden-buffer
  `(:name     "Hidden Buffer"
    :narrow   32
    :hidden   t
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :items
    ,(lambda ()
       (let ((filter (consult--regexp-filter consult-buffer-filter)))
         (seq-filter (lambda (x) (string-match-p filter x))
                     (consult--cached-buffer-names)))))
  "Hidden buffer candidate source for `consult-buffer'.")

(defvar consult--source-buffer
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda ()
       (let ((filter (consult--regexp-filter consult-buffer-filter)))
         (seq-remove (lambda (x) (string-match-p filter x))
                     (consult--cached-buffer-names)))))
  "Buffer candidate source for `consult-buffer'.")

(defvar consult--source-file
  `(:name     "File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () recentf-mode)
    :items
    ,(lambda ()
       (let ((ht (consult--cached-buffer-file-hash)))
         (mapcar #'abbreviate-file-name
                 (seq-remove (lambda (x) (gethash x ht)) recentf-list)))))
  "Recent file candidate source for `consult-buffer'.")

;;;###autoload
(defun consult-buffer ()
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources."
  (interactive)
  (when-let (buffer (consult--multi consult-buffer-sources
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Switch to: "
                                    :history 'consult--buffer-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (consult--buffer-action (car buffer)))))

;;;###autoload
(defun consult-buffer-other-window ()
  "Variant of `consult-buffer' which opens in other window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-buffer)))

;;;###autoload
(defun consult-buffer-other-frame ()
  "Variant of `consult-buffer' which opens in other frame."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-frame))
    (consult-buffer)))

;;;;; Command: consult-kmacro

(defun consult--kmacro-candidates ()
  "Return alist of kmacros and indices."
  (thread-last
      ;; List of macros
      (append (when last-kbd-macro
                `((,last-kbd-macro ,kmacro-counter ,kmacro-counter-format)))
              kmacro-ring)
    ;; Add indices
    (seq-map-indexed #'cons)
    ;; Filter mouse clicks
    (seq-remove (lambda (x) (seq-some #'mouse-event-p (caar x))))
    ;; Format macros
    (mapcar (pcase-lambda (`((,keys ,counter ,format) . ,index))
              (propertize
               (format-kbd-macro keys 1)
               'consult--candidate index
               'consult--kmacro-annotation
               ;; If the counter is 0 and the counter format is its default,
               ;; then there is a good chance that the counter isn't actually
               ;; being used.  This can only be wrong when a user
               ;; intentionally starts the counter with a negative value and
               ;; then increments it to 0.
               (cond
                ((not (string= format "%d")) ;; show counter for non-default format
                 (format " (counter=%d, format=%s) " counter format))
                ((/= counter 0) ;; show counter if non-zero
                 (format " (counter=%d)" counter))))))
    (delete-dups)))

;;;###autoload
(defun consult-kmacro (arg)
  "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted."
  (interactive "p")
  (let ((selected (consult--read
                   (or (consult--kmacro-candidates)
                       (user-error "No keyboard macros defined"))
                   :prompt "Keyboard macro: "
                   :category 'consult-kmacro
                   :require-match t
                   :sort nil
                   :history 'consult--kmacro-history
                   :annotate
                   (lambda (cand)
                     (get-text-property 0 'consult--kmacro-annotation cand))
                   :lookup #'consult--lookup-candidate)))
    (if (= 0 selected)
        ;; If the first element has been selected, just run the last macro.
        (kmacro-call-macro (or arg 1) t nil)
      ;; Otherwise, run a kmacro from the ring.
      (let* ((selected (1- selected))
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

;;;;; Command: consult-grep

(defun consult--grep-matches (lines)
  "Find grep match for REGEXP in LINES."
  (save-match-data
    (let ((candidates)
          (paths (make-hash-table :test #'equal)))
      (dolist (str lines (nreverse candidates))
        (when (string-match consult--grep-regexp str)
          (let* ((raw-path (match-string 1 str))
                 (line (match-string 2 str))
                 (start (match-end 0))
                 (path (or (gethash raw-path paths)
                           (let* ((expanded (expand-file-name raw-path))
                                  (file (string-remove-prefix default-directory expanded)))
                             (puthash raw-path (cons expanded file) paths))))
                 (file (cdr path))
                 (file-len (length file))
                 (max-len (+ start consult-grep-max-columns))
                 (matches)
                 (col) (pos))
            (when (> (length str) max-len)
              (setq str (substring str start max-len)
                    start 0))
            (setq pos start)
            (while (string-match consult--grep-match-regexp str pos)
              (unless col (setq col (- (match-beginning 0) start)))
              (push (substring str pos (match-beginning 0)) matches)
              (push (match-string 1 str) matches)
              (put-text-property 0 (length (car matches)) 'face 'consult-preview-match (car matches))
              (setq pos (match-end 0)))
            (push (substring str pos) matches)
            ;; XXX consult--format-location inlined here for performance and to reduce allocations
            (setq str (apply #'concat file ":" line ":" (nreverse matches)))
            (add-text-properties 0 file-len `(face consult-file consult--grep-file ,file) str)
            (put-text-property (1+ file-len) (+ 1 file-len (length line)) 'face 'consult-line-number str)
            (push `(,str ,(car path) ,(string-to-number line) . ,(or col 0)) candidates)))))))

(defun consult--grep-state ()
  "Grep preview state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open))
      (funcall
       jump
       (consult--position-marker
        (and cand (funcall (if restore #'find-file open) (car cand)))
        (cadr cand) (cddr cand))
       restore))))

(defun consult--grep-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (if transform
      (substring cand (1+ (length (get-text-property 0 'consult--grep-file cand))))
    (get-text-property 0 'consult--grep-file cand)))

(defun consult--grep (prompt cmd dir initial)
  "Run grep CMD in DIR with INITIAL input.

PROMPT is the prompt string.
The symbol at point is added to the future history."
  (let* ((prompt-dir (consult--directory-prompt prompt dir))
         (default-directory (cdr prompt-dir)))
    (consult--read
     (consult--async-command cmd
       (consult--async-transform consult--grep-matches)
       :file-handler t) ;; allow tramp
     :prompt (car prompt-dir)
     :lookup #'consult--lookup-cdr
     :state (consult--grep-state)
     :initial (consult--async-split-initial initial)
     :add-history
     (when-let (thing (thing-at-point 'symbol))
       (consult--async-split-initial thing))
     :require-match t
     :category 'consult-grep
     :group #'consult--grep-group
     :history '(:input consult--grep-history)
     :sort nil)))

;;;###autoload
(defun consult-grep (&optional dir initial)
  "Search for regexp with grep in DIR with INITIAL input.

The input string is split, the first part of the string is passed to
the asynchronous grep process and the second part of the string is
passed to the completion-style filtering. The input string is split at
a punctuation character, which is given as the first character of the
input string. The format is similar to Perl-style regular expressions,
e.g., /regexp/. Furthermore command line options can be passed to
grep, specified behind --.

Example: #async-regexp -- grep-opts#filter-string

The symbol at point is added to the future history. If `consult-grep'
is called interactively with a prefix argument, the user can specify
the directory to search in. By default the project directory is used
if `consult-project-root-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched."
  (interactive "P")
  (consult--grep "Grep" consult-grep-command dir initial))

;;;###autoload
(defun consult-git-grep (&optional dir initial)
  "Search for regexp with grep in DIR with INITIAL input.

See `consult-grep' for more details."
  (interactive "P")
  (consult--grep "Git-grep" consult-git-grep-command dir initial))

;;;###autoload
(defun consult-ripgrep (&optional dir initial)
  "Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details."
  (interactive "P")
  (consult--grep "Ripgrep" consult-ripgrep-command dir initial))

;;;;; Command: consult-find

(defun consult--find (prompt cmd initial)
  "Run find CMD in current directory with INITIAL input.

The function returns the selected file.
The filename at point is added to the future history.

PROMPT is the prompt.
CMD is the find argument string."
  (consult--read
   (consult--async-command cmd
     (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
     :file-handler t) ;; allow tramp
   :prompt prompt
   :sort nil
   :require-match t
   :initial (consult--async-split-initial initial)
   :add-history
   (when-let (thing (thing-at-point 'filename))
     (consult--async-split-initial thing))
   :category 'file
   :history '(:input consult--find-history)))

;;;###autoload
(defun consult-find (&optional dir initial)
  "Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Find" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) consult-find-command initial))))

;;;###autoload
(defun consult-locate (&optional initial)
  "Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive)
  (find-file (consult--find "Locate: " consult-locate-command initial)))

;;;;; Command: consult-man

(defun consult--man-format (lines)
  "Format man candidates from LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
        (when (string-match "\\`\\(.*?\\([^ ]+\\) *(\\([^,)]+\\)[^)]*).*?\\) +- +\\(.*\\)\\'" str)
          (let ((names (match-string 1 str))
                (name (match-string 2 str))
                (section (match-string 3 str))
                (desc (match-string 4 str)))
            (push (cons
                   (format "%s - %s"
                           (propertize names 'face 'consult-key)
                           desc)
                   (concat section " " name))
                  candidates)))))
    (nreverse candidates)))

;;;###autoload
(defun consult-man (&optional initial)
  "Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive)
  (man (consult--read
        (consult--async-command consult-man-command
          (consult--async-transform consult--man-format))
        :prompt "Manual entry: "
        :require-match t
        :lookup #'consult--lookup-cdr
        :initial (consult--async-split-initial initial)
        :add-history
        (when-let (thing (thing-at-point 'symbol))
          (consult--async-split-initial thing))
        :history '(:input consult--man-history))))

;;;; Preview at point in completions buffers

(define-minor-mode consult-preview-at-point-mode
  "Preview minor mode for *Completions* buffers.
When moving around in the *Completions* buffer, the candidate at point is automatically previewed."
  :init-value nil
  (if consult-preview-at-point-mode
      (add-hook 'post-command-hook #'consult-preview-at-point nil 'local)
    (remove-hook 'post-command-hook #'consult-preview-at-point 'local)))

(defun consult-preview-at-point ()
  "Preview candidate at point in *Completions* buffer."
  (interactive)
  (when-let* ((win (active-minibuffer-window))
              (buf (window-buffer win))
              (fun (buffer-local-value 'consult--preview-function buf)))
    (funcall fun)))

;;;; Integration with the default completion system

(defun consult--default-completion-mb-candidate ()
  "Return current minibuffer candidate from default completion system or Icomplete."
  (when (and (minibufferp)
             (eq completing-read-function #'completing-read-default))
    (let ((content (minibuffer-contents-no-properties)))
      ;; When the current minibuffer content matches a candidate, return it!
      (if (test-completion content
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
          content
        ;; Return the full first candidate of the sorted completion list.
        (when-let ((completions (completion-all-sorted-completions)))
          (concat
           (substring content 0 (or (cdr (last completions)) 0))
           (car completions)))))))

(defun consult--default-completion-list-candidate ()
  "Return current candidate at point from completions buffer."
  (let (beg end)
    (when (and
           (derived-mode-p 'completion-list-mode)
           ;; Logic taken from `choose-completion'.
           ;; TODO Upstream a `completion-list-get-candidate' function.
           (cond
            ((and (not (eobp)) (get-text-property (point) 'mouse-face))
             (setq end (point) beg (1+ (point))))
            ((and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
             (setq end (1- (point)) beg (point)))))
      (setq beg (previous-single-property-change beg 'mouse-face)
            end (or (next-single-property-change end 'mouse-face) (point-max)))
      (buffer-substring-no-properties beg end))))

;; Announce now that consult has been loaded
(provide 'consult)

;;;; Integration with other completion systems

(with-eval-after-load 'icomplete (require 'consult-icomplete))
(with-eval-after-load 'selectrum (require 'consult-selectrum))
(with-eval-after-load 'vertico (require 'consult-vertico))

;;; consult.el ends here
