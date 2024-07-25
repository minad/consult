;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler and Consult contributors
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2020
;; Version: 1.8
;; Package-Requires: ((emacs "27.1") (compat "30"))
;; Homepage: https://github.com/minad/consult
;; Keywords: matching, files, completion

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

;; Consult implements a set of `consult-<thing>' commands, which aim to
;; improve the way you use Emacs.  The commands are founded on
;; `completing-read', which selects from a list of candidate strings.
;; Consult provides an enhanced buffer switcher `consult-buffer' and
;; search and navigation commands like `consult-imenu' and
;; `consult-line'.  Searching through multiple files is supported by the
;; asynchronous `consult-grep' command.  Many Consult commands support
;; previewing candidates.  If a candidate is selected in the completion
;; view, the buffer shows the candidate immediately.

;; The Consult commands are compatible with multiple completion systems
;; based on the Emacs `completing-read' API, including the default
;; completion system, Vertico, Mct and Icomplete.

;; See the README for an overview of the available Consult commands and
;; the documentation of the configuration and installation of the
;; package.

;; The full list of contributors can be found in the acknowledgments
;; section of the README.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'compat)
(require 'bookmark)

(defgroup consult nil
  "Consulting `completing-read'."
  :link '(info-link :tag "Info Manual" "(consult)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/consult")
  :link '(emacs-library-link :tag "Library Source" "consult.el")
  :group 'files
  :group 'outlines
  :group 'minibuffer
  :prefix "consult-")

;;;; Customization

(defcustom consult-narrow-key nil
  "Prefix key for narrowing during completion.

Good choices for this key are \"<\" and \"C-+\" for example. The
key must be a string accepted by `key-valid-p'."
  :type '(choice key (const :tag "None" nil)))

(defcustom consult-widen-key nil
  "Key used for widening during completion.

If this key is unset, defaults to twice the `consult-narrow-key'.
The key must be a string accepted by `key-valid-p'."
  :type '(choice key (const :tag "None" nil)))

(defcustom consult-project-function
  #'consult--default-project-function
  "Function which returns project root directory.
The function takes one boolean argument MAY-PROMPT.  If
MAY-PROMPT is non-nil, the function may ask the prompt the user
for a project directory.  The root directory is used by
`consult-buffer' and `consult-grep'."
  :type `(choice
          (const :tag "Default project function" ,#'consult--default-project-function)
          (function :tag "Custom function")
          (const :tag "No project integration" nil)))

(defcustom consult-async-refresh-delay 0.2
  "Refreshing delay of the completion UI for asynchronous commands.

The completion UI is only updated every
`consult-async-refresh-delay' seconds.  This applies to
asynchronous commands like for example `consult-grep'."
  :type '(float :tag "Delay in seconds"))

(defcustom consult-async-input-throttle 0.5
  "Input throttle for asynchronous commands.

The asynchronous process is started only every
`consult-async-input-throttle' seconds.  This applies to asynchronous
commands, e.g., `consult-grep'."
  :type '(float :tag "Delay in seconds"))

(defcustom consult-async-input-debounce 0.2
  "Input debounce for asynchronous commands.

The asynchronous process is started only when there has not been new
input for `consult-async-input-debounce' seconds.  This applies to
asynchronous commands, e.g., `consult-grep'."
  :type '(float :tag "Delay in seconds"))

(defcustom consult-async-min-input 3
  "Minimum number of characters needed, before asynchronous process is called.

This applies to asynchronous commands, e.g., `consult-grep'."
  :type '(natnum :tag "Number of characters"))

(defcustom consult-async-split-style 'perl
  "Async splitting style, see `consult-async-split-styles-alist'."
  :type '(choice (const :tag "No splitting" nil)
                 (const :tag "Comma" comma)
                 (const :tag "Semicolon" semicolon)
                 (const :tag "Perl" perl)))

(defcustom consult-async-split-styles-alist
  `((nil :function ,#'consult--split-nil)
    (comma :separator ?, :function ,#'consult--split-separator)
    (semicolon :separator ?\; :function ,#'consult--split-separator)
    (perl :initial "#" :function ,#'consult--split-perl))
  "Async splitting styles."
  :type '(alist :key-type symbol :value-type plist))

(defcustom consult-mode-histories
  '((eshell-mode eshell-history-ring eshell-history-index    eshell-bol)
    (comint-mode comint-input-ring   comint-input-ring-index comint-bol)
    (term-mode   term-input-ring     term-input-ring-index   term-bol))
  "Alist of mode histories (mode history index bol).
The histories can be rings or lists.  Index, if provided, is a
variable to set to the index of the selection within the ring or
list.  Bol, if provided is a function which jumps to the beginning
of the line after the prompt."
  :type '(alist :key-type symbol
                :value-type (group :tag "Include Index"
                                   (symbol :tag "List/Ring")
                                   (symbol :tag "Index Variable")
                                   (symbol :tag "Bol Function"))))

(defcustom consult-themes nil
  "List of themes (symbols or regexps) to be presented for selection.
nil shows all `custom-available-themes'."
  :type '(repeat (choice symbol regexp)))

(defcustom consult-after-jump-hook (list #'recenter)
  "Function called after jumping to a location.

Commonly used functions for this hook are `recenter' and
`reposition-window'.  You may want to add a function which pulses
the current line, e.g., `pulse-momentary-highlight-one-line' is
supported on Emacs 28 and newer.  The hook called during preview
and for the jump after selection."
  :type 'hook)

(defcustom consult-line-start-from-top nil
  "Start search from the top if non-nil.
Otherwise start the search at the current line and wrap around."
  :type 'boolean)

(defcustom consult-point-placement 'match-beginning
  "Where to leave point when jumping to a match.
This setting affects the command `consult-line' and the `consult-grep' variants."
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
  :type '(natnum :tag "Buffer size in bytes"))

(defcustom consult-buffer-filter
  '("\\` "
    "\\`\\*Completions\\*\\'"
    "\\`\\*Flymake log\\*\\'"
    "\\`\\*Semantic SymRef\\*\\'"
    "\\`\\*vc\\*\\'"
    "\\`newsrc-dribble\\'" ;; Gnus
    "\\`\\*tramp/.*\\*\\'")
  "Filter regexps for `consult-buffer'.

The default setting is to filter ephemeral buffer names beginning
with a space character, the *Completions* buffer and a few log
buffers.  The regular expressions are matched case sensitively."
  :type '(repeat regexp))

(defcustom consult-buffer-sources
  '(consult--source-hidden-buffer
    consult--source-modified-buffer
    consult--source-buffer
    consult--source-recent-file
    consult--source-file-register
    consult--source-bookmark
    consult--source-project-buffer-hidden
    consult--source-project-recent-file-hidden)
  "Sources used by `consult-buffer'.
See also `consult-project-buffer-sources'.
See `consult--multi' for a description of the source data structure."
  :type '(repeat symbol))

(defcustom consult-project-buffer-sources
  '(consult--source-project-buffer
    consult--source-project-recent-file)
  "Sources used by `consult-project-buffer'.
See also `consult-buffer-sources'.
See `consult--multi' for a description of the source data structure."
  :type '(repeat symbol))

(defcustom consult-mode-command-filter
  '(;; Filter commands
    "-mode\\'" "--"
    ;; Filter whole features
    simple mwheel time so-long recentf tab-bar tab-line)
  "Filter commands for `consult-mode-command'."
  :type '(repeat (choice symbol regexp)))

(defcustom consult-grep-max-columns 300
  "Maximal number of columns of grep output."
  :type 'natnum)

(defconst consult--grep-match-regexp
  "\\`\\(?:\\./\\)?\\([^\n\0]+\\)\0\\([0-9]+\\)\\([-:\0]\\)"
  "Regexp used to match file and line of grep output.")

(defcustom consult-grep-args
  '("grep" (consult--grep-exclude-args)
    "--null --line-buffered --color=never --ignore-case\
     --with-filename --line-number -I -r")
  "Command line arguments for grep, see `consult-grep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-git-grep-args
  "git --no-pager grep --null --color=never --ignore-case\
   --extended-regexp --line-number -I"
  "Command line arguments for git-grep, see `consult-git-grep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-ripgrep-args
  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --search-zip"
  "Command line arguments for ripgrep, see `consult-ripgrep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-find-args
  "find . -not ( -path */.[A-Za-z]* -prune )"
  "Command line arguments for find, see `consult-find'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-fd-args
  '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
    "--full-path --color=never")
  "Command line arguments for fd, see `consult-fd'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-locate-args
  "locate --ignore-case" ;; --existing not supported by Debian plocate
  "Command line arguments for locate, see `consult-locate'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-man-args
  "man -k"
  "Command line arguments for man, see `consult-man'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-preview-key 'any
  "Preview trigger keys, can be nil, `any', a single key or a list of keys.
Debouncing can be specified via the `:debounce' attribute.  The
individual keys must be strings accepted by `key-valid-p'."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))

(defcustom consult-preview-partial-size 1048576
  "Files larger than this byte limit are previewed partially."
  :type '(natnum :tag "File size in bytes"))

(defcustom consult-preview-partial-chunk 102400
  "Partial preview chunk size in bytes.
If a file is larger than `consult-preview-partial-size' only the
chunk from the beginning of the file is previewed."
  :type '(natnum :tag "Chunk size in bytes"))

(defcustom consult-preview-max-count 10
  "Number of file buffers to keep open temporarily during preview."
  :type '(natnum :tag "Number of buffers"))

(defcustom consult-preview-excluded-buffers nil
  "Buffers excluded from preview.
The value should conform to the predicate format demanded by the
function `buffer-match-p'."
  :type 'sexp)

(defcustom consult-preview-excluded-files
  '("\\`/[^/|:]+:") ;; Do not preview remote files
  "List of regexps matched against names of files, which are not previewed."
  :type '(repeat regexp))

(defcustom consult-preview-allowed-hooks
  '(global-font-lock-mode
    save-place-find-file-hook)
  "List of hooks, which should be executed during file preview.
This variable applies to `find-file-hook', `change-major-mode-hook' and
mode hooks, e.g., `prog-mode-hook'."
  :type '(repeat symbol))

(defcustom consult-preview-variables
  '((inhibit-message . t)
    (enable-dir-local-variables . nil)
    (enable-local-variables . :safe)
    (non-essential . t)
    (delay-mode-hooks . t))
  "Variables which are bound for file preview."
  :type '(alist :key-type symbol))

(defcustom consult-bookmark-narrow
  `((?f "File" bookmark-default-handler)
    (?h "Help" help-bookmark-jump Info-bookmark-jump
               Man-bookmark-jump woman-bookmark-jump)
    (?p "Picture" image-bookmark-jump)
    (?d "Docview" doc-view-bookmark-jump)
    (?m "Mail" gnus-summary-bookmark-jump)
    (?s "Eshell" eshell-bookmark-jump)
    (?w "Web" eww-bookmark-jump xwidget-webkit-bookmark-jump-handler)
    (?v "VC Directory" vc-dir-bookmark-jump)
    (nil "Other"))
  "Bookmark narrowing configuration.

Each element of the list must have the form (char name handlers...)."
  :type '(alist :key-type character :value-type (cons string (repeat function))))

(defcustom consult-yank-rotate
  (if (boundp 'yank-from-kill-ring-rotate)
      yank-from-kill-ring-rotate
    t)
  "Rotate the `kill-ring' in the `consult-yank' commands."
  :type 'boolean)

;;;; Faces

(defgroup consult-faces nil
  "Faces used by Consult."
  :group 'consult
  :group 'faces)

(defface consult-preview-line
  '((t :inherit consult-preview-insertion :extend t))
  "Face used for line previews.")

(defface consult-highlight-match
  '((t :inherit match))
  "Face used to highlight matches in the completion candidates.
Used for example by `consult-grep'.")

(defface consult-highlight-mark
  '((t :inherit consult-highlight-match))
  "Face used for mark positions in completion candidates.
Used for example by `consult-mark'.  The face should be different
than the `cursor' face to avoid confusion.")

(defface consult-preview-match
  '((t :inherit isearch))
  "Face used for match previews, e.g., in `consult-line'.")

(defface consult-preview-insertion
  '((t :inherit region))
  "Face used for previews of text to be inserted.
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

(defface consult-grep-context
  '((t :inherit shadow))
  "Face used to highlight grep context in `consult-grep'.")

(defface consult-bookmark
  '((t :inherit font-lock-constant-face))
  "Face used to highlight bookmarks in `consult-buffer'.")

(defface consult-buffer
  '((t))
  "Face used to highlight buffers in `consult-buffer'.")

(defface consult-line-number-prefix
  '((t :inherit line-number))
  "Face used to highlight line number prefixes.")

(defface consult-line-number-wrapped
  '((t :inherit consult-line-number-prefix :inherit font-lock-warning-face))
  "Face used to highlight line number prefixes after wrap around.")

(defface consult-separator
  '((((class color) (min-colors 88) (background light))
     :foreground "#ccc")
    (((class color) (min-colors 88) (background dark))
     :foreground "#333"))
  "Face used for thin line separators in `consult-register-window'.")

;;;; Input history variables

(defvar consult--path-history nil)
(defvar consult--grep-history nil)
(defvar consult--find-history nil)
(defvar consult--man-history nil)
(defvar consult--line-history nil)
(defvar consult--line-multi-history nil)
(defvar consult--theme-history nil)
(defvar consult--minor-mode-menu-history nil)
(defvar consult--buffer-history nil)

;;;; Internal variables

(defvar consult--regexp-compiler
  #'consult--default-regexp-compiler
  "Regular expression compiler used by `consult-grep' and other commands.
The function must return a list of regular expressions and a highlighter
function.")

(defvar consult--customize-alist
  ;; Disable preview in frames, since `consult--jump-preview' does not properly
  ;; clean up.  See gh:minad/consult#593. This issue should better be fixed in
  ;; `consult--jump-preview'.
  `((,#'consult-buffer-other-frame :preview-key nil)
    (,#'consult-buffer-other-tab :preview-key nil))
  "Command configuration alist for fine-grained configuration.

Each element of the list must have the form (command-name plist...).  The
options set here will be evaluated and passed to `consult--read', when
called from the corresponding command.  Note that the options depend on
the private `consult--read' API and should not be considered as stable
as the public API.")

(defvar consult--buffer-display #'switch-to-buffer
  "Buffer display function.")

(defvar consult--completion-candidate-hook
  (list #'consult--default-completion-minibuffer-candidate
        #'consult--default-completion-list-candidate)
  "Get candidate from completion system.")

(defvar consult--completion-refresh-hook nil
  "Refresh completion system.")

(defvar-local consult--preview-function nil
  "Minibuffer-local variable which exposes the current preview function.
This function can be called by custom completion systems from
outside the minibuffer.")

(defvar consult--annotate-align-step 10
  "Round candidate width.")

(defvar consult--annotate-align-width 0
  "Maximum candidate width used for annotation alignment.")

(defconst consult--tofu-char #x200000
  "Special character used to encode line prefixes for disambiguation.
We use invalid characters outside the Unicode range.")

(defconst consult--tofu-range #x100000
  "Special character range.")

(defvar-local consult--narrow nil
  "Current narrowing key.")

(defvar-local consult--narrow-keys nil
  "Narrowing prefixes of the current completion.")

(defvar-local consult--narrow-predicate nil
  "Narrowing predicate of the current completion.")

(defvar-local consult--narrow-overlay nil
  "Narrowing indicator overlay.")

(defvar consult--gc-threshold (* 64 1024 1024)
  "Large GC threshold for temporary increase.")

(defvar consult--gc-percentage 0.5
  "Large GC percentage for temporary increase.")

(defvar consult--process-chunk (* 1024 1024)
  "Increase process output chunk size.")

(defvar consult--async-log
  " *consult-async*"
  "Buffer for async logging output used by `consult--async-process'.")

(defvar-local consult--focus-lines-overlays nil
  "Overlays used by `consult-focus-lines'.")

(defvar-local consult--org-fold-regions nil
  "Stored regions for the org-fold API.")

;;;; Miscellaneous helper functions

(defun consult--key-parse (key)
  "Parse KEY or signal error if invalid."
  (unless (key-valid-p key)
    (error "%S is not a valid key definition; see `key-valid-p'" key))
  (key-parse key))

(defun consult--in-buffer (fun &optional buffer)
  "Ensure that FUN is executed inside BUFFER."
  (unless buffer (setq buffer (current-buffer)))
  (lambda (&rest args)
    (with-current-buffer buffer
      (apply fun args))))

(defun consult--completion-table-in-buffer (table &optional buffer)
  "Ensure that completion TABLE is executed inside BUFFER."
  (if (functionp table)
      (consult--in-buffer
       (lambda (str pred action)
         (let ((result (funcall table str pred action)))
           (pcase action
             ('metadata
              (setq result
                    (mapcar
                     (lambda (x)
                       (if (and (string-suffix-p "-function" (symbol-name (car-safe x))) (cdr x))
                           (cons (car x) (consult--in-buffer (cdr x)))
                         x))
                     result)))
             ((and 'completion--unquote (guard (functionp (cadr result))))
              (cl-callf consult--in-buffer (cadr result) buffer)
              (cl-callf consult--in-buffer (cadddr result) buffer)))
           result))
       buffer)
    table))

(defun consult--build-args (arg)
  "Return ARG as a flat list of split strings.

Turn ARG into a list, and for each element either:
- split it if it a string.
- eval it if it is an expression."
  (seq-mapcat (lambda (x)
                (if (stringp x)
                    (split-string-and-unquote x)
                  (ensure-list (eval x 'lexical))))
              (ensure-list arg)))

(defun consult--command-split (str)
  "Return command argument and options list given input STR."
  (save-match-data
    (let ((opts (when (string-match " +--\\( +\\|\\'\\)" str)
                  (prog1 (substring str (match-end 0))
                    (setq str (substring str 0 (match-beginning 0)))))))
      ;; split-string-and-unquote fails if the quotes are invalid.  Ignore it.
      (cons str (and opts (ignore-errors (split-string-and-unquote opts)))))))

(defmacro consult--keep! (list form)
  "Evaluate FORM for every element of LIST and keep the non-nil results."
  (declare (indent 1))
  (cl-with-gensyms (head prev result)
    `(let* ((,head (cons nil ,list))
            (,prev ,head))
       (while (cdr ,prev)
         (if-let (,result (let ((it (cadr ,prev))) ,form))
             (progn
               (pop ,prev)
               (setcar ,prev ,result))
           (setcdr ,prev (cddr ,prev))))
       (setq ,list (cdr ,head))
       nil)))

;; Upstream bug#46326, Consult issue gh:minad/consult#193.
(defmacro consult--minibuffer-with-setup-hook (fun &rest body)
  "Variant of `minibuffer-with-setup-hook' using a symbol and `fset'.

This macro is only needed to prevent memory leaking issues with
the upstream `minibuffer-with-setup-hook' macro.
FUN is the hook function and BODY opens the minibuffer."
  (declare (indent 1) (debug t))
  (let ((hook (gensym "hook"))
        (append))
    (when (eq (car-safe fun) :append)
      (setq append '(t) fun (cadr fun)))
    `(let ((,hook (make-symbol "consult--minibuffer-setup-hook")))
       (fset ,hook (lambda ()
                     (remove-hook 'minibuffer-setup-hook ,hook)
                     (funcall ,fun)))
       (unwind-protect
           (progn
             (add-hook 'minibuffer-setup-hook ,hook ,@append)
             ,@body)
         (remove-hook 'minibuffer-setup-hook ,hook)))))

(defun consult--completion-filter (pattern cands category _highlight)
  "Filter CANDS with PATTERN.

CATEGORY is the completion category, used to find the completion style via
`completion-category-defaults' and `completion-category-overrides'.
HIGHLIGHT must be non-nil if the resulting strings should be highlighted."
  ;; completion-all-completions returns an improper list
  ;; where the last link is not necessarily nil.
  (nconc (completion-all-completions pattern cands nil (length pattern)
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
with a bang.  See `consult--completion-filter' for the arguments CATEGORY and
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
  (cl-with-gensyms (max)
    `(save-excursion
       (let ((,beg (point-min)) (,max (point-max)) ,end)
         (while (< ,beg ,max)
           (goto-char ,beg)
           (setq ,end (pos-eol))
           ,@body
           (setq ,beg (1+ ,end)))))))

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
                (setq width (+ width (compat-call string-width string pos nexti))))
              (setq pos nexti))))))
    width))

(defun consult--string-hash (strings)
  "Create hash table from STRINGS."
  (let ((ht (make-hash-table :test #'equal :size (length strings))))
    (dolist (str strings)
      (puthash str t ht))
    ht))

(defmacro consult--local-let (binds &rest body)
  "Buffer local let BINDS of dynamic variables in BODY."
  (declare (indent 1))
  (let ((buffer (gensym "buffer"))
        (local (mapcar (lambda (x) (cons (gensym "local") (car x))) binds)))
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

(defvar consult--fast-abbreviate-file-name nil)
(defun consult--fast-abbreviate-file-name (name)
  "Return abbreviate file NAME.
This function is a pure variant of `abbreviate-file-name', which
does not access the file system.  This is important if we require
that the operation is fast, even for remote paths or paths on
network file systems."
  (save-match-data
    (let (case-fold-search) ;; Assume that file system is case sensitive.
      (setq name (directory-abbrev-apply name))
      (if (string-match (with-memoization consult--fast-abbreviate-file-name
                          (directory-abbrev-make-regexp (expand-file-name "~")))
                        name)
          (concat "~" (substring name (match-beginning 1)))
        name))))

(defun consult--left-truncate-file (file)
  "Return abbreviated file name of FILE for use in `completing-read' prompt."
  (save-match-data
    (let ((afile (abbreviate-file-name file)))
      (if (string-match "/\\([^/]+\\)/\\([^/]+/?\\)\\'" afile)
          (propertize (format "…/%s/%s" (match-string 1 afile) (match-string 2 afile))
                      'help-echo afile)
        afile))))

(defun consult--directory-prompt (prompt dir)
  "Return prompt, paths and default directory.

PROMPT is the prompt prefix.  The directory is appended to the
prompt prefix.  For projects only the project name is shown.  The
`default-directory' is not shown.  Other directories are
abbreviated and only the last two path components are shown.

If DIR is a string, it is returned as default directory.  If DIR
is a list of strings, the list is returned as search paths.  If
DIR is nil the `consult-project-function' is tried to retrieve
the default directory.  If no project is found the
`default-directory' is returned as is.  Otherwise the user is
asked for the directories or files to search via
`completing-read-multiple'."
  (let* ((paths nil)
         (dir
          (pcase dir
            ((pred stringp) dir)
            ('nil (or (consult--project-root) default-directory))
            (_
               (pcase (if (stringp (car-safe dir))
                          dir
                        ;; Preserve this-command across `completing-read-multiple' call,
                        ;; such that `consult-customize' continues to work.
                        (let ((this-command this-command)
                              (def (abbreviate-file-name default-directory))
                              ;; TODO: `minibuffer-completing-file-name' is
                              ;; mostly deprecated, but still in use. Packages
                              ;; should instead use the completion metadata.
                              (minibuffer-completing-file-name t)
                              (ignore-case read-file-name-completion-ignore-case))
                          (consult--minibuffer-with-setup-hook
                              (lambda ()
                                (setq-local completion-ignore-case ignore-case)
                                (set-syntax-table minibuffer-local-filename-syntax))
                            (completing-read-multiple "Directories or files: "
                                                      #'completion-file-name-table
                                                      nil t def 'consult--path-history def))))
                 ((and `(,p) (guard (file-directory-p p))) p)
                 (ps (setq paths (mapcar (lambda (p)
                                           (file-relative-name (expand-file-name p)))
                                         ps))
                     default-directory)))))
         (edir (file-name-as-directory (expand-file-name dir)))
         (pdir (let ((default-directory edir))
                 ;; Bind default-directory in order to find the project
                 (consult--project-root))))
    (list
     (format "%s (%s): " prompt
             (pcase paths
               (`(,p) (consult--left-truncate-file p))
               (`(,p . ,_)
                (format "%d paths, %s, …" (length paths) (consult--left-truncate-file p)))
               ((guard (equal edir pdir)) (concat "Project " (consult--project-name pdir)))
               (_ (consult--left-truncate-file edir))))
     (or paths '("."))
     edir)))

(defun consult--default-project-function (may-prompt)
  "Return project root directory.
When no project is found and MAY-PROMPT is non-nil ask the user."
  (when-let (proj (project-current may-prompt))
    (cond
     ((fboundp 'project-root) (project-root proj))
     ((fboundp 'project-roots) (car (project-roots proj))))))

(defun consult--project-root (&optional may-prompt)
  "Return project root as absolute path.
When no project is found and MAY-PROMPT is non-nil ask the user."
  ;; Preserve this-command across project selection,
  ;; such that `consult-customize' continues to work.
  (let ((this-command this-command))
    (when-let (root (and consult-project-function
                         (funcall consult-project-function may-prompt)))
      (expand-file-name root))))

(defun consult--project-name (dir)
  "Return the project name for DIR."
  (if (string-match "/\\([^/]+\\)/\\'" dir)
      (propertize (match-string 1 dir) 'help-echo (abbreviate-file-name dir))
    dir))

(defun consult--format-file-line-match (file line match)
  "Format string FILE:LINE:MATCH with faces."
  (setq line (number-to-string line)
        match (concat file ":" line ":" match)
        file (length file))
  (put-text-property 0 file 'face 'consult-file match)
  (put-text-property (1+ file) (+ 1 file (length line)) 'face 'consult-line-number match)
  match)

(defun consult--make-overlay (beg end &rest props)
  "Make consult overlay between BEG and END with PROPS."
  (let ((ov (make-overlay beg end)))
    (while props
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    ov))

(defun consult--remove-dups (list)
  "Remove duplicate strings from LIST."
  (delete-dups (copy-sequence list)))

(defsubst consult--in-range-p (pos)
  "Return t if position POS lies in range `point-min' to `point-max'."
  (<= (point-min) pos (point-max)))

(defun consult--completion-window-p ()
  "Return non-nil if the selected window belongs to the completion UI."
  (or (eq (selected-window) (active-minibuffer-window))
      (eq #'completion-list-mode (buffer-local-value 'major-mode (window-buffer)))))

(defun consult--original-window ()
  "Return window which was just selected just before the minibuffer was entered.
In contrast to `minibuffer-selected-window' never return nil and
always return an appropriate non-minibuffer window."
  (or (minibuffer-selected-window)
      (if (window-minibuffer-p (selected-window))
          (next-window)
        (selected-window))))

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
  ;; is not font-locked.  We would observe this if consulting an unfontified
  ;; line.  Therefore we have to enforce font-locking now, which is slow.  In
  ;; order to prevent is hang-up we check the buffer size against
  ;; `consult-fontify-max-size'.
  (when (and consult-fontify-preserve jit-lock-mode
             (< (buffer-size) consult-fontify-max-size))
    (jit-lock-fontify-now)))

(defun consult--fontify-region (start end)
  "Ensure that region between START and END is fontified."
  (when (and consult-fontify-preserve jit-lock-mode)
    (jit-lock-fontify-now start end)))

(defmacro consult--with-increased-gc (&rest body)
  "Temporarily increase the GC limit in BODY to optimize for throughput."
  (cl-with-gensyms (overwrite)
    `(let* ((,overwrite (> consult--gc-threshold gc-cons-threshold))
            (gc-cons-threshold (if ,overwrite consult--gc-threshold gc-cons-threshold))
            (gc-cons-percentage (if ,overwrite consult--gc-percentage gc-cons-percentage)))
       ,@body)))

(defmacro consult--slow-operation (message &rest body)
  "Show delayed MESSAGE if BODY takes too long.
Also temporarily increase the GC limit via `consult--with-increased-gc'."
  (declare (indent 1))
  `(let (set-message-function) ;; bug#63253: Broken `with-delayed-message'
     (with-delayed-message (1 ,message)
       (consult--with-increased-gc
        ,@body))))

(defun consult--count-lines (pos)
  "Move to position POS and return number of lines."
  (let ((line 1))
    (while (< (point) pos)
      (forward-line)
      (when (<= (point) pos)
        (cl-incf line)))
    (goto-char pos)
    line))

(defun consult--marker-from-line-column (buffer line column)
  "Get marker in BUFFER from LINE and COLUMN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (without-restriction
          (goto-char (point-min))
          ;; Location data might be invalid by now!
          (ignore-errors
            (forward-line (1- line))
            (goto-char (min (+ (point) column) (pos-eol))))
          (point-marker))))))

(defun consult--line-prefix (&optional curr-line)
  "Annotate `consult-location' candidates with line numbers.
CURR-LINE is the current line number."
  (setq curr-line (or curr-line -1))
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (before (format #("%%%dd " 0 6 (face consult-line-number-wrapped)) width))
         (after (format #("%%%dd " 0 6 (face consult-line-number-prefix)) width)))
    (lambda (cand)
      (let ((line (cdr (get-text-property 0 'consult-location cand))))
        (list cand (format (if (< line curr-line) before after) line) "")))))

(defsubst consult--location-candidate (cand marker line tofu &rest props)
  "Add MARKER and LINE as `consult-location' text property to CAND.
Furthermore add the additional text properties PROPS, and append
TOFU suffix for disambiguation."
  (setq cand (concat cand (consult--tofu-encode tofu)))
  (add-text-properties 0 1 `(consult-location (,marker . ,line) ,@props) cand)
  cand)

;; There is a similar variable `yank-excluded-properties'.  Unfortunately
;; we cannot use it here since it excludes too much (e.g., invisible)
;; and at the same time not enough (e.g., cursor-sensor-functions).
(defconst consult--remove-text-properties
  '(category cursor cursor-intangible cursor-sensor-functions field follow-link
    fontified front-sticky help-echo insert-behind-hooks insert-in-front-hooks
    intangible keymap local-map modification-hooks mouse-face pointer read-only
    rear-nonsticky yank-handler)
  "List of text properties to remove from buffer strings.")

(defsubst consult--buffer-substring (beg end &optional fontify)
  "Return buffer substring between BEG and END.
If FONTIFY and `consult-fontify-preserve' are non-nil, first ensure that the
region has been fontified."
  (if consult-fontify-preserve
      (let (str)
        (when fontify (consult--fontify-region beg end))
        (setq str (buffer-substring beg end))
        ;; TODO Propose the upstream addition of a function
        ;; `preserve-list-of-text-properties', which should be as efficient as
        ;; `remove-list-of-text-properties'.
        (remove-list-of-text-properties
         0 (- end beg) consult--remove-text-properties str)
        str)
    (buffer-substring-no-properties beg end)))

(defun consult--line-with-mark (marker)
  "Current line string where the MARKER position is highlighted."
  (let* ((beg (pos-bol))
         (end (pos-eol))
         (str (consult--buffer-substring beg end 'fontify)))
    (if (>= marker end)
        (concat str #(" " 0 1 (face consult-highlight-mark)))
      (put-text-property (- marker beg) (- (1+ marker) beg)
                         'face 'consult-highlight-mark str)
      str)))

;;;; Tofu cooks

(defsubst consult--tofu-p (char)
  "Return non-nil if CHAR is a tofu."
  (<= consult--tofu-char char (+ consult--tofu-char consult--tofu-range -1)))

(defun consult--tofu-hide (str)
  "Hide the tofus in STR."
  (let* ((max (length str))
         (end max))
    (while (and (> end 0) (consult--tofu-p (aref str (1- end))))
      (cl-decf end))
    (when (< end max)
      (setq str (copy-sequence str))
      (put-text-property end max 'invisible t str))
    str))

(defsubst consult--tofu-append (cand id)
  "Append tofu-encoded ID to CAND.
The ID must fit within a single character.  It must be smaller
than `consult--tofu-range'."
  (setq id (char-to-string (+ consult--tofu-char id)))
  (add-text-properties 0 1 '(invisible t consult-strip t) id)
  (concat cand id))

(defsubst consult--tofu-get (cand)
  "Extract tofu-encoded ID from CAND.
See `consult--tofu-append'."
  (- (aref cand (1- (length cand))) consult--tofu-char))

;; We must disambiguate the lines by adding a prefix such that two lines with
;; the same text can be distinguished.  In order to avoid matching the line
;; number, such that the user can search for numbers with `consult-line', we
;; encode the line number as characters outside the Unicode range.  By doing
;; that, no accidental matching can occur.
(defun consult--tofu-encode (n)
  "Return tofu-encoded number N as a string.
Large numbers are encoded as multiple tofu characters."
  (let (str tofu)
    (while (progn
             (setq tofu (char-to-string
                         (+ consult--tofu-char (% n consult--tofu-range)))
                   str (if str (concat tofu str) tofu))
             (and (>= n consult--tofu-range)
                  (setq n (/ n consult--tofu-range)))))
    (add-text-properties 0 (length str) '(invisible t consult-strip t) str)
    str))

;;;; Regexp utilities

(defun consult--find-highlights (str start &rest ignored-faces)
  "Find highlighted regions in STR from position START.
Highlighted regions have a non-nil face property.
IGNORED-FACES are ignored when searching for matches."
  (let (highlights
        (end (length str))
        (beg start))
    (while (< beg end)
      (let ((next (next-single-property-change beg 'face str end))
            (val (get-text-property beg 'face str)))
        (when (and val
                   (not (memq val ignored-faces))
                   (not (and (consp val)
                             (seq-some (lambda (x) (memq x ignored-faces)) val))))
          (push (cons (- beg start) (- next start)) highlights))
        (setq beg next)))
    (nreverse highlights)))

(defun consult--point-placement (str start &rest ignored-faces)
  "Compute point placement from STR with START offset.
IGNORED-FACES are ignored when searching for matches.
Return cons of point position and a list of match begin/end pairs."
  (let* ((matches (apply #'consult--find-highlights str start ignored-faces))
         (pos (pcase-exhaustive consult-point-placement
                ('match-beginning (or (caar matches) 0))
                ('match-end (or (cdar (last matches)) 0))
                ('line-beginning 0))))
    (dolist (match matches)
      (cl-decf (car match) pos)
      (cl-decf (cdr match) pos))
    (cons pos matches)))

(defun consult--highlight-regexps (regexps ignore-case str)
  "Highlight REGEXPS in STR.
If a regular expression contains capturing groups, only these are highlighted.
If no capturing groups are used highlight the whole match.  Case is ignored
if IGNORE-CASE is non-nil."
  (dolist (re regexps)
    (let ((i 0))
      (while (and (let ((case-fold-search ignore-case))
                    (string-match re str i))
                  ;; Ensure that regexp search made progress (edge case for .*)
                  (> (match-end 0) i))
        ;; Unfortunately there is no way to avoid the allocation of the match
        ;; data, since the number of capturing groups is unknown.
        (let ((m (match-data)))
          (setq i (cadr m) m (or (cddr m) m))
          (while m
            (when (car m)
              (add-face-text-property (car m) (cadr m)
                                      'consult-highlight-match nil str))
            (setq m (cddr m)))))))
  str)

(defconst consult--convert-regexp-table
  (append
   ;; For simplicity, treat word beginning/end as word boundaries,
   ;; since PCRE does not make this distinction.  Usually the
   ;; context determines if \b is the beginning or the end.
   '(("\\<" . "\\b") ("\\>" . "\\b")
     ("\\_<" . "\\b") ("\\_>" . "\\b"))
   ;; Treat \` and \' as beginning and end of line.  This is more
   ;; widely supported and makes sense for line-based commands.
   '(("\\`" . "^") ("\\'" . "$"))
   ;; Historical: Unescaped *, +, ? are supported at the beginning
   (mapcan (lambda (x)
             (mapcar (lambda (y)
                       (cons (concat x y)
                             (concat (string-remove-prefix "\\" x) "\\" y)))
                     '("*" "+" "?")))
           '("" "\\(" "\\(?:" "\\|" "^"))
   ;; Different escaping
   (mapcan (lambda (x) `(,x (,(cdr x) . ,(car x))))
           '(("\\|" . "|")
             ("\\(" . "(") ("\\)" . ")")
             ("\\{" . "{") ("\\}" . "}"))))
  "Regexp conversion table.")

(defun consult--convert-regexp (regexp type)
  "Convert Emacs REGEXP to regexp syntax TYPE."
  (if (memq type '(emacs basic))
      regexp
    ;; Support for Emacs regular expressions is fairly complete for basic
    ;; usage.  There are a few unsupported Emacs regexp features:
    ;; - \= point matching
    ;; - Syntax classes \sx \Sx
    ;; - Character classes \cx \Cx
    ;; - Explicitly numbered groups (?3:group)
    (replace-regexp-in-string
     (rx (or "\\\\" "\\^"                         ;; Pass through
             (seq (or "\\(?:" "\\|") (any "*+?")) ;; Historical: \|+ or \(?:* etc
             (seq "\\(" (any "*+"))               ;; Historical: \(* or \(+
             (seq (or bos "^") (any "*+?"))       ;; Historical: + or * at the beginning
             (seq (opt "\\") (any "(){|}"))       ;; Escape parens/braces/pipe
             (seq "\\" (any "'<>`"))              ;; Special escapes
             (seq "\\_" (any "<>"))))             ;; Beginning or end of symbol
     (lambda (x) (or (cdr (assoc x consult--convert-regexp-table)) x))
     regexp 'fixedcase 'literal)))

(defun consult--default-regexp-compiler (input type ignore-case)
  "Compile the INPUT string to a list of regular expressions.
The function should return a pair, the list of regular expressions and a
highlight function.  The highlight function should take a single
argument, the string to highlight given the INPUT.  TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'.  If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
  (setq input (consult--split-escaped input))
  (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
        (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
          (apply-partially #'consult--highlight-regexps regexps ignore-case))))

(defun consult--split-escaped (str)
  "Split STR at spaces, which can be escaped with backslash."
  (mapcar
   (lambda (x) (string-replace "\0" " " x))
   (split-string (replace-regexp-in-string
                  "\\\\\\\\\\|\\\\ "
                  (lambda (x) (if (equal x "\\ ") "\0" x))
                  str 'fixedcase 'literal)
                 " +" t)))

(defun consult--join-regexps (regexps type)
  "Join REGEXPS of TYPE."
  ;; Add look-ahead wrapper only if there is more than one regular expression
  (cond
   ((and (eq type 'pcre) (cdr regexps))
    (concat "^" (mapconcat (lambda (x) (format "(?=.*%s)" x))
                           regexps "")))
   ((eq type 'basic)
    (string-join regexps ".*"))
   (t
    (when (length> regexps 3)
      (message "Too many regexps, %S ignored. Use post-filtering!"
               (string-join (seq-drop regexps 3) " "))
      (setq regexps (seq-take regexps 3)))
    (consult--join-regexps-permutations regexps (and (eq type 'emacs) "\\")))))

(defun consult--join-regexps-permutations (regexps esc)
  "Join all permutations of REGEXPS.
ESC is the escaping string for choice and groups."
  (pcase regexps
    ('nil "")
    (`(,r) r)
    (_ (mapconcat
        (lambda (r)
          (concat esc "(" r esc ").*" esc "("
                  (consult--join-regexps-permutations (remove r regexps) esc)
                  esc ")"))
        regexps (concat esc "|")))))

(defun consult--valid-regexp-p (re)
  "Return t if regexp RE is valid."
  (condition-case nil
      (progn (string-match-p re "") t)
    (invalid-regexp nil)))

(defun consult--regexp-filter (regexps)
  "Create filter regexp from REGEXPS."
  (if (stringp regexps)
      regexps
    (mapconcat (lambda (x) (concat "\\(?:" x "\\)")) regexps "\\|")))

;;;; Lookup functions

(defun consult--lookup-member (selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES list, return original element."
  (car (member selected candidates)))

(defun consult--lookup-cons (selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES alist, return cons."
  (assoc selected candidates))

(defun consult--lookup-cdr (selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES alist, return `cdr' of element."
  (cdr (assoc selected candidates)))

(defun consult--lookup-location (selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES list of `consult-location' category.
Return the location marker."
  (when-let (found (member selected candidates))
    (setq found (car (consult--get-location (car found))))
    ;; Check that marker is alive
    (and (or (not (markerp found)) (marker-buffer found)) found)))

(defun consult--lookup-prop (prop selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES list and return PROP value."
  (when-let (found (member selected candidates))
    (get-text-property 0 prop (car found))))

(defun consult--lookup-candidate (selected candidates &rest _)
  "Lookup SELECTED in CANDIDATES list and return property `consult--candidate'."
  (consult--lookup-prop 'consult--candidate selected candidates))

;;;; Preview support

(defun consult--preview-allowed-p (fun)
  "Return non-nil if FUN is an allowed preview mode hook."
  (or (memq fun consult-preview-allowed-hooks)
      (when-let (((symbolp fun))
                 (name (symbol-name fun))
                 ;; Global modes in Emacs 29 are activated via a
                 ;; `find-file-hook' ending with `-check-buffers'. This has been
                 ;; changed in Emacs 30. Now a `change-major-mode-hook' is used
                 ;; instead with the suffix `-check-buffers'.
                 (suffix (static-if (>= emacs-major-version 30)
                             "-enable-in-buffer"
                           "-check-buffers"))
                 ((string-suffix-p suffix name)))
        (memq (intern (string-remove-suffix suffix name))
              consult-preview-allowed-hooks))))

(defun consult--filter-find-file-hook (orig &rest hooks)
  "Filter `find-file-hook' by `consult-preview-allowed-hooks'.
This function is an advice for `run-hooks'.
ORIG is the original function, HOOKS the arguments."
  (if (memq 'find-file-hook hooks)
      (cl-letf* (((default-value 'find-file-hook)
                  (seq-filter #'consult--preview-allowed-p
                              (default-value 'find-file-hook)))
                 (find-file-hook (default-value 'find-file-hook)))
        (apply orig hooks))
    (apply orig hooks)))

(defun consult--find-file-temporarily-1 (name)
  "Open file NAME, helper function for `consult--find-file-temporarily'."
  (when-let (((not (seq-find (lambda (x) (string-match-p x name))
                             consult-preview-excluded-files)))
             ;; file-attributes may throw permission denied error
             (attrs (ignore-errors (file-attributes name)))
             (size (file-attribute-size attrs)))
    (let* ((partial (>= size consult-preview-partial-size))
           (buffer (if partial
                       (generate-new-buffer (format "consult-partial-preview-%s" name))
                     (find-file-noselect name 'nowarn)))
           (success nil))
      (unwind-protect
          (with-current-buffer buffer
            (if (not partial)
                (when (or (eq major-mode 'hexl-mode)
                        (and (eq major-mode 'fundamental-mode)
                             (save-excursion (search-forward "\0" nil 'noerror))))
                  (error "No preview of binary file `%s'"
                         (file-name-nondirectory name)))
              (with-silent-modifications
                (setq buffer-read-only t)
                (insert-file-contents name nil 0 consult-preview-partial-chunk)
                (goto-char (point-max))
                (insert "\nFile truncated. End of partial preview.\n")
                (goto-char (point-min)))
              (when (save-excursion (search-forward "\0" nil 'noerror))
                (error "No partial preview of binary file `%s'"
                       (file-name-nondirectory name)))
              ;; Auto detect major mode and hope for the best, given that the
              ;; file is only previewed partially.  If an error is thrown the
              ;; buffer will be killed and preview is aborted.
              (set-auto-mode)
              (font-lock-mode 1))
            (when (bound-and-true-p so-long-detected-p)
              (error "No preview of file `%s' with long lines"
                     (file-name-nondirectory name)))
            ;; Run delayed hooks listed in `consult-preview-allowed-hooks'.
            (dolist (hook (reverse (cons 'after-change-major-mode-hook delayed-mode-hooks)))
              (run-hook-wrapped hook (lambda (fun)
                                       (when (consult--preview-allowed-p fun)
                                         (funcall fun))
                                       nil)))
            (setq success (current-buffer)))
        (unless success
          (kill-buffer buffer))))))

(defun consult--find-file-temporarily (name)
  "Open file NAME temporarily for preview."
  (let ((vars (delq nil
                    (mapcar
                     (pcase-lambda (`(,k . ,v))
                       (if (boundp k)
                           (list k v (default-value k) (symbol-value k))
                         (message "consult-preview-variables: The variable `%s' is not bound" k)
                         nil))
                     consult-preview-variables))))
    (condition-case err
        (unwind-protect
            (progn
              (advice-add #'run-hooks :around #'consult--filter-find-file-hook)
              (pcase-dolist (`(,k ,v . ,_) vars)
                (set-default k v)
                (set k v))
              (consult--find-file-temporarily-1 name))
          (advice-remove #'run-hooks #'consult--filter-find-file-hook)
          (pcase-dolist (`(,k ,_ ,d ,v) vars)
            (set-default k d)
            (set k v)))
      (error
       (message "%s" (error-message-string err))
       nil))))

(defun consult--temporary-files ()
  "Return a function to open files temporarily for preview."
  (let ((dir default-directory)
        (hook (make-symbol "consult--temporary-files-upgrade-hook"))
        (orig-buffers (buffer-list))
        temporary-buffers)
    (fset hook
          (lambda (_)
            ;; Fully initialize previewed files and keep them alive.
            (unless (consult--completion-window-p)
              (let (live-files)
                (pcase-dolist (`(,file . ,buf) temporary-buffers)
                  (when-let (wins (and (buffer-live-p buf)
                                       (get-buffer-window-list buf)))
                    (push (cons file (mapcar
                                      (lambda (win)
                                        (cons win (window-state-get win t)))
                                      wins))
                          live-files)))
                (pcase-dolist (`(,_ . ,buf) temporary-buffers)
                  (kill-buffer buf))
                (setq temporary-buffers nil)
                (pcase-dolist (`(,file . ,wins) live-files)
                  (when-let (buf (consult--file-action file))
                    (push buf orig-buffers)
                    (pcase-dolist (`(,win . ,state) wins)
                      (setf (car (alist-get 'buffer state)) buf)
                      (window-state-put state win))))))))
    (lambda (&optional name)
      (if name
          (let ((default-directory dir))
            (setq name (abbreviate-file-name (expand-file-name name)))
            (or
             ;; Find existing fully initialized buffer (non-previewed).  We have
             ;; to check for fully initialized buffer before accessing the
             ;; previewed buffers, since `embark-act' can open a buffer which is
             ;; currently previewed, such that we end up with two buffers for
             ;; the same file - one previewed and only partially initialized and
             ;; one fully initialized.  In this case we prefer the fully
             ;; initialized buffer.  For directories `get-file-buffer' returns nil,
             ;; therefore we have to special case Dired.
             (if (and (fboundp 'dired-find-buffer-nocreate) (file-directory-p name))
                 (dired-find-buffer-nocreate name)
               (get-file-buffer name))
             ;; Find existing previewed buffer.  Previewed buffers are not fully
             ;; initialized (hooks are delayed) in order to ensure fast preview.
             (cdr (assoc name temporary-buffers))
             ;; Finally, if no existing buffer has been found, open the file for
             ;; preview.
             (when-let (buf (consult--find-file-temporarily name))
               ;; Only add new buffer if not already in the list
               (unless (or (rassq buf temporary-buffers) (memq buf orig-buffers))
                 (add-hook 'window-selection-change-functions hook)
                 (push (cons name buf) temporary-buffers)
                 ;; Disassociate buffer from file by setting `buffer-file-name'
                 ;; and `dired-directory' to nil and rename the buffer.  This
                 ;; lets us open an already previewed buffer with the Embark
                 ;; default action C-. RET.
                 (with-current-buffer buf
                   (rename-buffer
                    (format " Preview:%s"
                            (file-name-nondirectory (directory-file-name name)))
                    'unique))
                 ;; The buffer disassociation is delayed to avoid breaking modes
                 ;; like `pdf-view-mode' or `doc-view-mode' which rely on
                 ;; `buffer-file-name'.  Executing (set-visited-file-name nil)
                 ;; early also prevents the major mode initialization.
                 (let ((hook (make-symbol "consult--temporary-files-disassociate-hook")))
                   (fset hook (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (remove-hook 'pre-command-hook hook)
                                    (setq-local buffer-read-only t
                                                dired-directory nil
                                                buffer-file-name nil)))))
                   (add-hook 'pre-command-hook hook))
                 ;; Only keep a few buffers alive
                 (while (length> temporary-buffers consult-preview-max-count)
                   (kill-buffer (cdar (last temporary-buffers)))
                   (setq temporary-buffers (nbutlast temporary-buffers))))
               buf)))
        (remove-hook 'window-selection-change-functions hook)
        (pcase-dolist (`(,_ . ,buf) temporary-buffers)
          (kill-buffer buf))
        (setq temporary-buffers nil)))))

(defun consult--invisible-open-permanently ()
  "Open overlays which hide the current line.
See `isearch-open-necessary-overlays' and `isearch-open-overlay-temporary'."
  (if (and (derived-mode-p 'org-mode) (fboundp 'org-fold-show-set-visibility))
      ;; New Org 9.6 fold-core API
      (let ((inhibit-redisplay t)) ;; HACK: Prevent flicker due to premature redisplay
        (org-fold-show-set-visibility 'canonical))
    (dolist (ov (overlays-in (pos-bol) (pos-eol)))
      (when-let (fun (overlay-get ov 'isearch-open-invisible))
        (when (invisible-p (overlay-get ov 'invisible))
          (funcall fun ov))))))

(defun consult--invisible-open-temporarily ()
  "Temporarily open overlays which hide the current line.
See `isearch-open-necessary-overlays' and `isearch-open-overlay-temporary'."
  (if (and (derived-mode-p 'org-mode)
           (fboundp 'org-fold-show-set-visibility)
           (fboundp 'org-fold-core-get-regions)
           (fboundp 'org-fold-core-region))
      ;; New Org 9.6 fold-core API
      ;; TODO The provided Org API `org-fold-show-set-visibility' cannot be used
      ;; efficiently.  We obtain all regions in the whole buffer in order to
      ;; restore them.  A better show API would return all the applied
      ;; modifications such that we can restore the ones which got modified.
      (progn
        (unless consult--org-fold-regions
          (setq consult--org-fold-regions
                (delq nil (org-fold-core-get-regions
                           :with-markers t :from (point-min) :to (point-max))))
          (when consult--org-fold-regions
            (let ((hook (make-symbol "consult--invisible-open-temporarily-cleanup-hook"))
                  (buffer (current-buffer))
                  (depth (recursion-depth)))
              (fset hook
                    (lambda ()
                      (when (= (recursion-depth) depth)
                        (remove-hook 'minibuffer-exit-hook hook)
                        (run-at-time
                         0 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (pcase-dolist (`(,beg ,end ,_) consult--org-fold-regions)
                                 (when (markerp beg) (set-marker beg nil))
                                 (when (markerp end) (set-marker end nil)))
                               (kill-local-variable 'consult--org-fold-regions))))))))
              (add-hook 'minibuffer-exit-hook hook))))
        (let ((inhibit-redisplay t)) ;; HACK: Prevent flicker due to premature redisplay
          (org-fold-show-set-visibility 'canonical))
        (list (lambda ()
                (pcase-dolist (`(,beg ,end ,spec) consult--org-fold-regions)
                  (org-fold-core-region beg end t spec)))))
    (let (restore)
      (dolist (ov (overlays-in (pos-bol) (pos-eol)))
        (let ((inv (overlay-get ov 'invisible)))
          (when (and (invisible-p inv) (overlay-get ov 'isearch-open-invisible))
            (push (if-let (fun (overlay-get ov 'isearch-open-invisible-temporary))
                      (progn
                        (funcall fun ov nil)
                        (lambda () (funcall fun ov t)))
                    (overlay-put ov 'invisible nil)
                    (lambda () (overlay-put ov 'invisible inv)))
                  restore))))
      restore)))

(defun consult--jump-ensure-buffer (pos)
  "Ensure that buffer of marker POS is displayed, return t if successful."
  (or (not (markerp pos))
      ;; Switch to buffer if it is not visible
      (when-let ((buf (marker-buffer pos)))
        (or (and (eq (current-buffer) buf) (eq (window-buffer) buf))
            (consult--buffer-action buf 'norecord)
            t))))

(defun consult--jump (pos)
  "Jump to POS.
First push current position to mark ring, then move to new
position and run `consult-after-jump-hook'."
  (when pos
    ;; Extract marker from list with with overlay positions, see `consult--line-match'
    (when (consp pos) (setq pos (car pos)))
    ;; When the marker is in the same buffer, record previous location
    ;; such that the user can jump back quickly.
    (when (or (not (markerp pos)) (eq (current-buffer) (marker-buffer pos)))
      ;; push-mark mutates markers in the mark-ring and the mark-marker.
      ;; Therefore we transform the marker to a number to be safe.
      ;; We all love side effects!
      (setq pos (+ pos 0))
      (push-mark (point) t))
    (when (consult--jump-ensure-buffer pos)
      (unless (= (goto-char pos) (point)) ;; Widen if jump failed
        (widen)
        (goto-char pos))
      (consult--invisible-open-permanently)
      (run-hooks 'consult-after-jump-hook)))
  nil)

(defun consult--jump-preview ()
  "The preview function used if selecting from a list of candidate positions.
The function can be used as the `:state' argument of `consult--read'."
  (let (restore)
    (lambda (action cand)
      (when (eq action 'preview)
        (mapc #'funcall restore)
        (setq restore nil)
        ;; TODO Better buffer preview support
        ;; 1. Use consult--buffer-preview instead of consult--jump-ensure-buffer
        ;; 2. Remove function consult--jump-ensure-buffer
        ;; 3. Remove consult-buffer-other-* from consult-customize-alist
        (when-let ((pos (or (car-safe cand) cand)) ;; Candidate can be previewed
                   ((consult--jump-ensure-buffer pos)))
          (let ((saved-min (point-min-marker))
                (saved-max (point-max-marker))
                (saved-pos (point-marker)))
            (set-marker-insertion-type saved-max t) ;; Grow when text is inserted
            (push (lambda ()
                    (when-let ((buf (marker-buffer saved-pos)))
                      (with-current-buffer buf
                        (narrow-to-region saved-min saved-max)
                        (goto-char saved-pos)
                        (set-marker saved-pos nil)
                        (set-marker saved-min nil)
                        (set-marker saved-max nil))))
                  restore))
          (unless (= (goto-char pos) (point)) ;; Widen if jump failed
            (widen)
            (goto-char pos))
          (setq restore (nconc (consult--invisible-open-temporarily) restore))
          ;; Ensure that cursor is properly previewed (gh:minad/consult#764)
          (unless (eq cursor-in-non-selected-windows 'box)
            (let ((orig cursor-in-non-selected-windows)
                  (buf (current-buffer)))
              (push
               (if (local-variable-p 'cursor-in-non-selected-windows)
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq-local cursor-in-non-selected-windows orig))))
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (kill-local-variable 'cursor-in-non-selected-windows)))))
               restore)
              (setq-local cursor-in-non-selected-windows 'box)))
          ;; Match previews
          (let ((overlays
                 (list (save-excursion
                         (let ((vbeg (progn (beginning-of-visual-line) (point)))
                               (vend (progn (end-of-visual-line) (point)))
                               (end (pos-eol)))
                           (consult--make-overlay vbeg (if (= vend end) (1+ end) vend)
                                                  'face 'consult-preview-line
                                                  'window (selected-window)
                                                  'priority 1))))))
            (dolist (match (cdr-safe cand))
              (push (consult--make-overlay (+ (point) (car match))
                                           (+ (point) (cdr match))
                                           'face 'consult-preview-match
                                           'window (selected-window)
                                           'priority 2)
                    overlays))
            (push (lambda () (mapc #'delete-overlay overlays)) restore))
          (run-hooks 'consult-after-jump-hook))))))

(defun consult--jump-state ()
  "The state function used if selecting from a list of candidate positions."
  (consult--state-with-return (consult--jump-preview) #'consult--jump))

(defun consult--get-location (cand)
  "Return location from CAND."
  (let ((loc (get-text-property 0 'consult-location cand)))
    (when (consp (car loc))
      ;; Transform cheap marker to real marker
      (setcar loc (set-marker (make-marker) (cdar loc) (caar loc))))
    loc))

(defun consult--location-state (candidates)
  "Location state function.
The cheap location markers from CANDIDATES are upgraded on window
selection change to full Emacs markers."
  (let ((jump (consult--jump-state))
        (hook (make-symbol "consult--location-upgrade-hook")))
    (fset hook
          (lambda (_)
            (unless (consult--completion-window-p)
              (remove-hook 'window-selection-change-functions hook)
              (mapc #'consult--get-location
                    (if (functionp candidates) (funcall candidates) candidates)))))
    (lambda (action cand)
      (pcase action
        ('setup (add-hook 'window-selection-change-functions hook))
        ('exit (remove-hook 'window-selection-change-functions hook)))
      (funcall jump action cand))))

(defun consult--state-with-return (state return)
  "Compose STATE function with RETURN function."
  (lambda (action cand)
    (funcall state action cand)
    (when (and cand (eq action 'return))
      (funcall return cand))))

(defmacro consult--define-state (type)
  "Define state function for TYPE."
  `(defun ,(intern (format "consult--%s-state" type)) ()
     ,(format "State function for %ss with preview.
The result can be passed as :state argument to `consult--read'." type)
     (consult--state-with-return (,(intern (format "consult--%s-preview" type)))
                                 #',(intern (format "consult--%s-action" type)))))

(defun consult--preview-key-normalize (preview-key)
  "Normalize PREVIEW-KEY, return alist of keys and debounce times."
  (let ((keys)
        (debounce 0))
    (setq preview-key (ensure-list preview-key))
    (while preview-key
      (if (eq (car preview-key) :debounce)
          (setq debounce (cadr preview-key)
                preview-key (cddr preview-key))
        (let ((key (car preview-key)))
          (unless (eq key 'any)
            (setq key (consult--key-parse key)))
          (push (cons key debounce) keys))
        (pop preview-key)))
    keys))

(defun consult--preview-key-debounce (preview-key cand)
  "Return debounce value of PREVIEW-KEY given the current candidate CAND."
  (when (and (consp preview-key) (memq :keys preview-key))
    (setq preview-key (funcall (plist-get preview-key :predicate) cand)))
  (let ((map (make-sparse-keymap))
        (keys (this-single-command-keys))
        any)
    (pcase-dolist (`(,k . ,d) (consult--preview-key-normalize preview-key))
      (if (eq k 'any)
          (setq any d)
        (define-key map k `(lambda () ,d))))
    (setq keys (lookup-key map keys))
    (if (functionp keys) (funcall keys) any)))

(defun consult--preview-append-local-pch (fun)
  "Append FUN to local `post-command-hook' list."
  ;; Symbol indirection because of bug#46407.
  (let ((hook (make-symbol "consult--preview-post-command-hook")))
    (fset hook fun)
    ;; TODO Emacs 28 has a bug, where the hook--depth-alist is not cleaned up properly
    ;; Do not use the broken add-hook here.
    ;;(add-hook 'post-command-hook hook 'append 'local)
    (setq-local post-command-hook
                (append
                 (remove t post-command-hook)
                 (list hook)
                 (and (memq t post-command-hook) '(t))))))

(defun consult--with-preview-1 (preview-key state transform candidate save-input fun)
  "Add preview support for FUN.
See `consult--with-preview' for the arguments
PREVIEW-KEY, STATE, TRANSFORM, CANDIDATE and SAVE-INPUT."
  (let ((mb-input "") mb-narrow selected timer previewed)
    (consult--minibuffer-with-setup-hook
        (if (and state preview-key)
            (lambda ()
              (let ((hook (make-symbol "consult--preview-minibuffer-exit-hook"))
                    (depth (recursion-depth)))
                (fset hook
                      (lambda ()
                        (when (= (recursion-depth) depth)
                          (remove-hook 'minibuffer-exit-hook hook)
                          (when timer
                            (cancel-timer timer)
                            (setq timer nil))
                          (with-selected-window (consult--original-window)
                            ;; STEP 3: Reset preview
                            (when previewed
                              (funcall state 'preview nil))
                            ;; STEP 4: Notify the preview function of the minibuffer exit
                            (funcall state 'exit nil)))))
                (add-hook 'minibuffer-exit-hook hook))
              ;; STEP 1: Setup the preview function
              (with-selected-window (consult--original-window)
                (funcall state 'setup nil))
              (setq consult--preview-function
                    (lambda ()
                      (when-let ((cand (funcall candidate)))
                        ;; Drop properties to prevent bugs regarding candidate
                        ;; lookup, which must handle candidates without
                        ;; properties.  Otherwise the arguments passed to the
                        ;; lookup function are confusing, since during preview
                        ;; the candidate has properties but for the final lookup
                        ;; after completion it does not.
                        (setq cand (substring-no-properties cand))
                        (with-selected-window (active-minibuffer-window)
                          (let ((input (minibuffer-contents-no-properties))
                                (narrow consult--narrow)
                                (win (consult--original-window)))
                            (with-selected-window win
                              (when-let ((transformed (funcall transform narrow input cand))
                                         (debounce (consult--preview-key-debounce preview-key transformed)))
                                (when timer
                                  (cancel-timer timer)
                                  (setq timer nil))
                                ;; The transformed candidate may have text
                                ;; properties, which change the preview display.
                                ;; This matters for example for `consult-grep',
                                ;; where the current candidate and input may
                                ;; stay equal, but the highlighting of the
                                ;; candidate changes while the candidates list
                                ;; is lagging a bit behind and updates
                                ;; asynchronously.
                                ;;
                                ;; In older Consult versions we instead compared
                                ;; the input without properties, since I worried
                                ;; that comparing the transformed candidates
                                ;; could be potentially expensive. However
                                ;; comparing the transformed candidates is more
                                ;; correct. The transformed candidate is the
                                ;; thing which is actually previewed.
                                (unless (equal-including-properties previewed transformed)
                                  (if (> debounce 0)
                                      (setq timer
                                            (run-at-time
                                             debounce nil
                                             (lambda ()
                                               ;; Preview only when a completion
                                               ;; window is selected and when
                                               ;; the preview window is alive.
                                               (when (and (consult--completion-window-p)
                                                          (window-live-p win))
                                                 (with-selected-window win
                                                   ;; STEP 2: Preview candidate
                                                   (funcall state 'preview (setq previewed transformed)))))))
                                    ;; STEP 2: Preview candidate
                                    (funcall state 'preview (setq previewed transformed)))))))))))
              (consult--preview-append-local-pch
               (lambda ()
                 (setq mb-input (minibuffer-contents-no-properties)
                       mb-narrow consult--narrow)
                 (funcall consult--preview-function))))
          (lambda ()
            (consult--preview-append-local-pch
             (lambda ()
               (setq mb-input (minibuffer-contents-no-properties)
                     mb-narrow consult--narrow)))))
      (unwind-protect
          (setq selected (when-let (result (funcall fun))
                           (when-let ((save-input)
                                      (list (symbol-value save-input))
                                      ((equal (car list) result)))
                             (set save-input (cdr list)))
                           (funcall transform mb-narrow mb-input result)))
        (when save-input
          (add-to-history save-input mb-input))
        (when state
          ;; STEP 5: The preview function should perform its final action
          (funcall state 'return selected))))))

(defmacro consult--with-preview (preview-key state transform candidate save-input &rest body)
  "Add preview support to BODY.

STATE is the state function.
TRANSFORM is the transformation function.
CANDIDATE is the function returning the current candidate.
PREVIEW-KEY are the keys which triggers the preview.
SAVE-INPUT can be a history variable symbol to save the input.

The state function takes two arguments, an action argument and the
selected candidate.  The candidate argument can be nil if no candidate is
selected or if the selection was aborted.  The function is called in
sequence with the following arguments:

  1. \\='setup nil         After entering the mb (minibuffer-setup-hook).
⎧ 2. \\='preview CAND/nil  Preview candidate CAND or reset if CAND is nil.
⎪    \\='preview CAND/nil
⎪    \\='preview CAND/nil
⎪    ...
⎩ 3. \\='preview nil       Reset preview.
  4. \\='exit nil          Before exiting the mb (minibuffer-exit-hook).
  5. \\='return CAND/nil   After leaving the mb, CAND has been selected.

The state function is always executed with the original window selected,
see `consult--original-window'.  The state function is called once in
the beginning of the minibuffer setup with the `setup' argument.  This is
useful in order to perform certain setup operations which require that
the minibuffer is initialized.  During completion candidates are
previewed.  Then the function is called with the `preview' argument and a
candidate CAND or nil if no candidate is selected.  Furthermore if nil is
passed for CAND, then the preview must be undone and the original state
must be restored.  The call with the `exit' argument happens once at the
end of the completion process, just before exiting the minibuffer.  The
minibuffer is still alive at that point.  Both `setup' and `exit' are
only useful for setup and cleanup operations.  They don't receive a
candidate as argument.  After leaving the minibuffer, the selected
candidate or nil is passed to the state function with the action
argument `return'.  At this point the state function can perform the
actual action on the candidate.  The state function with the `return'
argument is the continuation of `consult--read'.  Via `unwind-protect' it
is guaranteed, that if the `setup' action of a state function is
invoked, the state function will also be called with `exit' and
`return'."
  (declare (indent 5))
  `(consult--with-preview-1 ,preview-key ,state ,transform ,candidate ,save-input (lambda () ,@body)))

;;;; Narrowing and grouping

(defun consult--prefix-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate.
The candidate must have a `consult--prefix-group' property."
  (if transform
      (substring cand (1+ (length (get-text-property 0 'consult--prefix-group cand))))
    (get-text-property 0 'consult--prefix-group cand)))

(defun consult--type-group (types)
  "Return group function for TYPES."
  (lambda (cand transform)
    (if transform cand
      (alist-get (get-text-property 0 'consult--type cand) types))))

(defun consult--type-narrow (types)
  "Return narrowing configuration from TYPES."
  (list :predicate
        (lambda (cand) (eq (get-text-property 0 'consult--type cand) consult--narrow))
        :keys types))

(defun consult--widen-key ()
  "Return widening key, if `consult-widen-key' is not set.
The default is twice the `consult-narrow-key'."
  (cond
   (consult-widen-key
    (consult--key-parse consult-widen-key))
   (consult-narrow-key
    (let ((key (consult--key-parse consult-narrow-key)))
      (vconcat key key)))))

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
          (consult--make-overlay
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
       (when (equal (minibuffer-contents-no-properties) "")
         (lambda ()
           (interactive)
           (consult-narrow nil))))))

(defconst consult--narrow-space
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (let ((str (minibuffer-contents-no-properties)))
         (when-let (pair (or (and (length= str 1)
                                  (assoc (aref str 0) consult--narrow-keys))
                             (and (equal str "")
                                  (assoc ?\s consult--narrow-keys))))
           (lambda ()
             (interactive)
             (delete-minibuffer-contents)
             (consult-narrow (car pair))))))))

(defun consult-narrow-help ()
  "Print narrowing help as a `minibuffer-message'.

This command can be bound to a key in `consult-narrow-map',
to make it available for commands with narrowing."
  (interactive)
  (consult--require-minibuffer)
  (let ((minibuffer-message-timeout 1000000))
    (minibuffer-message
     (mapconcat (lambda (x)
                  (concat
                   (propertize (key-description (list (car x))) 'face 'consult-key)
                   " "
                   (propertize (cdr x) 'face 'consult-help)))
                consult--narrow-keys
                " "))))

(defun consult--narrow-setup (settings map)
  "Setup narrowing with SETTINGS and keymap MAP."
  (if (memq :keys settings)
      (setq consult--narrow-predicate (plist-get settings :predicate)
            consult--narrow-keys (plist-get settings :keys))
    (setq consult--narrow-predicate nil
          consult--narrow-keys settings))
  (when-let ((key consult-narrow-key))
    (setq key (consult--key-parse key))
    (dolist (pair consult--narrow-keys)
      (define-key map (vconcat key (vector (car pair)))
                  (cons (cdr pair) #'consult-narrow))))
  (when-let ((widen (consult--widen-key)))
    (define-key map widen (cons "All" #'consult-narrow)))
  (when-let ((init (and (memq :keys settings) (plist-get settings :initial))))
    (consult-narrow init)))

;; Emacs 28: hide in M-X
(put #'consult-narrow-help 'completion-predicate #'ignore)
(put #'consult-narrow 'completion-predicate #'ignore)

;;;; Splitting completion style

(defun consult--split-perl (str &optional _plist)
  "Split input STR in async input and filtering part.

The function returns a list with three elements: The async
string, the start position of the completion filter string and a
force flag.  If the first character is a punctuation character it
determines the separator.  Examples: \"/async/filter\",
\"#async#filter\"."
  (if (string-match-p "^[[:punct:]]" str)
      (save-match-data
        (let ((q (regexp-quote (substring str 0 1))))
          (string-match (concat "^" q "\\([^" q "]*\\)\\(" q "\\)?") str)
          `(,(match-string 1 str)
            ,(match-end 0)
            ;; Force update it two punctuation characters are entered.
            ,(match-end 2)
            ;; List of highlights
            (0 . ,(match-beginning 1))
            ,@(and (match-end 2) `((,(match-beginning 2) . ,(match-end 2)))))))
    `(,str ,(length str))))

(defun consult--split-nil (str &optional _plist)
  "Treat the complete input STR as async input."
  `(,str ,(length str)))

(defun consult--split-separator (str plist)
  "Split input STR in async input and filtering part at first separator.
PLIST is the splitter configuration, including the separator."
  (let ((sep (regexp-quote (char-to-string (plist-get plist :separator)))))
    (save-match-data
      (if (string-match (format "^\\([^%s]+\\)\\(%s\\)?" sep sep) str)
          `(,(match-string 1 str)
            ,(match-end 0)
            ;; Force update it space is entered.
            ,(match-end 2)
            ;; List of highlights
            ,@(and (match-end 2) `((,(match-beginning 2) . ,(match-end 2)))))
        `(,str ,(length str))))))

(defun consult--split-setup (split)
  "Setup splitting completion style with splitter function SPLIT."
  (let* ((styles completion-styles)
         (catdef completion-category-defaults)
         (catovr completion-category-overrides)
         (try (lambda (str table pred point)
                (let ((completion-styles styles)
                      (completion-category-defaults catdef)
                      (completion-category-overrides catovr)
                      (pos (cadr (funcall split str))))
                  (pcase (completion-try-completion (substring str pos) table pred
                                                    (max 0 (- point pos)))
                    ('t t)
                    (`(,newstr . ,newpt)
                     (cons (concat (substring str 0 pos) newstr)
                           (+ pos newpt)))))))
         (all (lambda (str table pred point)
                (let ((completion-styles styles)
                      (completion-category-defaults catdef)
                      (completion-category-overrides catovr)
                      (pos (cadr (funcall split str))))
                  (completion-all-completions (substring str pos) table pred
                                              (max 0 (- point pos)))))))
    (setq-local completion-styles-alist (cons `(consult--split ,try ,all "")
                                              completion-styles-alist)
                completion-styles '(consult--split)
                completion-category-defaults nil
                completion-category-overrides nil)))

;;;; Asynchronous filtering functions

(defun consult--async-p (fun)
  "Return t if FUN is an asynchronous completion function."
  (and (functionp fun)
       (condition-case nil
           (progn (funcall fun "" nil 'metadata) nil)
         (wrong-number-of-arguments t))))

(defmacro consult--with-async (bind &rest body)
  "Setup asynchronous completion in BODY.

BIND is the asynchronous function binding."
  (declare (indent 1))
  (let ((async (car bind)))
    `(let ((,async ,@(cdr bind))
           (new-chunk (max read-process-output-max consult--process-chunk))
           orig-chunk)
       (consult--minibuffer-with-setup-hook
           ;; Append such that we overwrite the completion style setting of
           ;; `fido-mode'.  See `consult--async-split' and
           ;; `consult--split-setup'.
           (:append
            (lambda ()
              (when (consult--async-p ,async)
                (setq orig-chunk read-process-output-max
                      read-process-output-max new-chunk)
                (funcall ,async 'setup)
                (let* ((mb (current-buffer))
                       (fun (lambda ()
                              (when-let (win (active-minibuffer-window))
                                (when (eq (window-buffer win) mb)
                                  (with-current-buffer mb
                                    (let ((inhibit-modification-hooks t))
                                      ;; Push input string to request refresh.
                                      (funcall ,async (minibuffer-contents-no-properties))))))))
                       ;; We use a symbol in order to avoid adding lambdas to
                       ;; the hook variable.  Symbol indirection because of
                       ;; bug#46407.
                       (hook (make-symbol "consult--async-after-change-hook")))
                  ;; Delay modification hook to ensure that minibuffer is still
                  ;; alive after the change, such that we don't restart a new
                  ;; asynchronous search right before exiting the minibuffer.
                  (fset hook (lambda (&rest _) (run-at-time 0 nil fun)))
                  (add-hook 'after-change-functions hook nil 'local)
                  (funcall hook)))))
         (let ((,async (if (consult--async-p ,async) ,async (lambda (_) ,async))))
           (unwind-protect
               ,(macroexp-progn body)
             (funcall ,async 'destroy)
             (when (and orig-chunk (eq read-process-output-max new-chunk))
               (setq read-process-output-max orig-chunk))))))))

(defun consult--async-sink ()
  "Create ASYNC sink function.

An async function must accept a single action argument.  For the
\\='setup action it is guaranteed that the call originates from
the minibuffer.  For the other actions no assumption about the
context can be made.

\\='setup   Setup the internal closure state.  Return nil.
\\='destroy Destroy the internal closure state.  Return nil.
\\='flush   Flush the list of candidates.  Return nil.
\\='refresh Request UI refresh.  Return nil.
nil      Return the list of candidates.
list     Append the list to the already existing candidates list and return it.
string   Update with the current user input string.  Return nil."
  (let (candidates last buffer)
    (lambda (action)
      (pcase-exhaustive action
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
               (run-hooks 'consult--completion-refresh-hook)
               ;; Interaction between asynchronous completion functions and
               ;; preview: We have to trigger preview immediately when
               ;; candidates arrive (gh:minad/consult#436).
               (when (and consult--preview-function candidates)
                 (funcall consult--preview-function)))))
         nil)
        ('nil candidates)
        ((pred consp)
         (setq last (last (if last (setcdr last action) (setq candidates action))))
         candidates)))))

(defun consult--async-split-style ()
  "Return the async splitting style function and initial string."
  (or (alist-get consult-async-split-style consult-async-split-styles-alist)
      (user-error "Splitting style `%s' not found" consult-async-split-style)))

(defun consult--async-split-initial (initial)
  "Return initial string for async command.
INITIAL is the additional initial string."
  (concat (plist-get (consult--async-split-style) :initial) initial))

(defun consult--async-split-thingatpt (thing)
  "Return THING at point with async initial prefix."
  (when-let (str (thing-at-point thing))
    (consult--async-split-initial str)))

(defun consult--async-split (async &optional split)
  "Create async function, which splits the input string.
ASYNC is the async sink.
SPLIT is the splitting function."
  (unless split
    (let* ((style (consult--async-split-style))
           (fn (plist-get style :function)))
      (setq split (lambda (str) (funcall fn str style)))))
  (lambda (action)
    (pcase action
      ('setup
       (consult--split-setup split)
       (funcall async 'setup))
      ((pred stringp)
       (pcase-let* ((`(,async-str ,_ ,force . ,highlights)
                     (funcall split action))
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

(defun consult--async-indicator (async)
  "Create async function with a state indicator overlay.
ASYNC is the async sink."
  (let (ov)
    (lambda (action &optional state)
      (pcase action
        ('indicator
         (overlay-put ov 'display
                      (pcase-exhaustive state
                        ('running  #("*" 0 1 (face consult-async-running)))
                        ('finished #(":" 0 1 (face consult-async-finished)))
                        ('killed   #(";" 0 1 (face consult-async-failed)))
                        ('failed   #("!" 0 1 (face consult-async-failed))))))
        ('setup
         (setq ov (make-overlay (- (minibuffer-prompt-end) 2)
                                (- (minibuffer-prompt-end) 1)))
         (funcall async 'setup))
        ('destroy
         (delete-overlay ov)
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--async-log (formatted &rest args)
  "Log FORMATTED ARGS to variable `consult--async-log'."
  (with-current-buffer (get-buffer-create consult--async-log)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(defun consult--async-process (async builder &rest props)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
BUILDER is the command line builder function.
PROPS are optional properties passed to `make-process'."
  (setq async (consult--async-indicator async))
  (let (proc proc-buf last-args count)
    (lambda (action)
      (pcase action
        ("" ;; If no input is provided kill current process
         (when proc
           (delete-process proc)
           (kill-buffer proc-buf)
           (setq proc nil proc-buf nil))
         (setq last-args nil))
        ((pred stringp)
         (funcall async action)
         (let* ((args (funcall builder action)))
           (unless (stringp (car args))
             (setq args (car args)))
           (unless (equal args last-args)
             (setq last-args args)
             (when proc
               (delete-process proc)
               (kill-buffer proc-buf)
               (setq proc nil proc-buf nil))
             (when args
               (let* ((flush t)
                      (rest "")
                      (proc-filter
                       (lambda (_ out)
                         (when flush
                           (setq flush nil)
                           (funcall async 'flush))
                         (let ((lines (split-string out "[\r\n]+")))
                           (if (not (cdr lines))
                               (setq rest (concat rest (car lines)))
                             (setcar lines (concat rest (car lines)))
                             (let* ((len (length lines))
                                    (last (nthcdr (- len 2) lines)))
                               (setq rest (cadr last)
                                     count (+ count len -1))
                               (setcdr last nil)
                               (funcall async lines))))))
                      (proc-sentinel
                       (lambda (_ event)
                         (when flush
                           (setq flush nil)
                           (funcall async 'flush))
                         (funcall async 'indicator
                                  (cond
                                   ((string-prefix-p "killed" event)   'killed)
                                   ((string-prefix-p "finished" event) 'finished)
                                   (t 'failed)))
                         (when (and (string-prefix-p "finished" event) (not (equal rest "")))
                           (cl-incf count)
                           (funcall async (list rest)))
                         (consult--async-log
                          "consult--async-process sentinel: event=%s lines=%d\n"
                          (string-trim event) count)
                         (when (> (buffer-size proc-buf) 0)
                           (with-current-buffer (get-buffer-create consult--async-log)
                             (goto-char (point-max))
                             (insert ">>>>> stderr >>>>>\n")
                             (let ((beg (point)))
                               (insert-buffer-substring proc-buf)
                               (save-excursion
                                 (goto-char beg)
                                 (message #("%s" 0 2 (face error))
                                          (buffer-substring-no-properties (pos-bol) (pos-eol)))))
                             (insert "<<<<< stderr <<<<<\n")))))
                      (process-adaptive-read-buffering nil))
                 (funcall async 'indicator 'running)
                 (consult--async-log "consult--async-process started %S\n" args)
                 (setq count 0
                       proc-buf (generate-new-buffer " *consult-async-stderr*")
                       proc (apply #'make-process
                                   `(,@props
                                     :connection-type pipe
                                     :name ,(car args)
                                     ;;; XXX tramp bug, the stderr buffer must be empty
                                     :stderr ,proc-buf
                                     :noquery t
                                     :command ,args
                                     :filter ,proc-filter
                                     :sentinel ,proc-sentinel)))))))
         nil)
        ('destroy
         (when proc
           (delete-process proc)
           (kill-buffer proc-buf)
           (setq proc nil proc-buf nil))
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--async-highlight (async builder)
  "Return a new ASYNC function with candidate highlighting.
BUILDER is the command line builder function."
  (let (highlight)
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (cdr (funcall builder action)))
        (funcall async action))
       ((and (consp action) highlight)
        (dolist (str action)
          (funcall highlight str))
        (funcall async action))
       (t (funcall async action))))))

(defun consult--async-throttle (async &optional throttle debounce)
  "Create async function from ASYNC which throttles input.

The THROTTLE delay defaults to `consult-async-input-throttle'.
The DEBOUNCE delay defaults to `consult-async-input-debounce'."
  (setq throttle (or throttle consult-async-input-throttle)
        debounce (or debounce consult-async-input-debounce))
  (let* ((input "") (timer (timer-create)) (last 0))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (equal action input)
           (cancel-timer timer)
           (funcall async "") ;; cancel running process
           (setq input action)
           (unless (equal action "")
             (timer-set-function timer (lambda ()
                                         (setq last (float-time))
                                         (funcall async action)))
             (timer-set-time
              timer
              (timer-relative-time
               nil (max debounce (- (+ last throttle) (float-time)))))
             (timer-activate timer)))
         nil)
        ('destroy
         (cancel-timer timer)
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--async-refresh-immediate (async)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens immediately when candidates are pushed."
  (lambda (action)
    (pcase action
      ((or (pred consp) 'flush)
       (prog1 (funcall async action)
         (funcall async 'refresh)))
      (_ (funcall async action)))))

(defun consult--async-refresh-timer (async &optional delay)
  "Create async function from ASYNC, which refreshes the display.

The refresh happens after a DELAY, defaulting to `consult-async-refresh-delay'."
  (let ((delay (or delay consult-async-refresh-delay))
        (timer (timer-create)))
    (timer-set-function timer async '(refresh))
    (lambda (action)
      (prog1 (funcall async action)
        (pcase action
          ((or (pred consp) 'flush)
           (unless (memq timer timer-list)
             (timer-set-time timer (timer-relative-time nil delay))
             (timer-activate timer)))
          ('destroy
           (cancel-timer timer)))))))

(defmacro consult--async-command (builder &rest args)
  "Asynchronous command pipeline.
ARGS is a list of `make-process' properties and transforms.
BUILDER is the command line builder function, which takes the
input string and must either return a list of command line
arguments or a pair of the command line argument list and a
highlighting function."
  (declare (indent 1))
  `(thread-first
     (consult--async-sink)
     (consult--async-refresh-timer)
     ,@(seq-take-while (lambda (x) (not (keywordp x))) args)
     (consult--async-process
      ,builder
      ,@(seq-drop-while (lambda (x) (not (keywordp x))) args))
     (consult--async-throttle)
     (consult--async-split)))

(defmacro consult--async-transform (async &rest transform)
  "Use FUN to TRANSFORM candidates of ASYNC."
  (cl-with-gensyms (async-var action-var)
    `(let ((,async-var ,async))
       (lambda (,action-var)
         (funcall ,async-var (if (consp ,action-var) (,@transform ,action-var) ,action-var))))))

(defun consult--async-map (async fun)
  "Map candidates of ASYNC by FUN."
  (consult--async-transform async mapcar fun))

(defun consult--async-filter (async fun)
  "Filter candidates of ASYNC by FUN."
  (consult--async-transform async seq-filter fun))

;;;; Dynamic collections based

(defun consult--dynamic-compute (async fun &optional debounce)
  "Dynamic computation of candidates.
ASYNC is the sink.
FUN computes the candidates given the input.
DEBOUNCE is the time after which an interrupted computation
should be restarted."
  (setq debounce (or debounce consult-async-input-debounce))
  (setq async (consult--async-indicator async))
  (let* ((request) (current) (timer)
         (cancel (lambda () (when timer (cancel-timer timer) (setq timer nil))))
         (start (lambda (req) (setq request req) (funcall async 'refresh))))
    (lambda (action)
      (pcase action
        ((and 'nil (guard (not request)))
         (funcall async nil))
        ('nil
         (funcall cancel)
         (let ((state 'killed))
           (unwind-protect
               (progn
                 (funcall async 'indicator 'running)
                 (redisplay)
                 ;; Run computation
                 (let ((response (funcall fun request)))
                   ;; Flush and update candidate list
                   (funcall async 'flush)
                   (setq state 'finished current request)
                   (funcall async response)))
             (funcall async 'indicator state)
             ;; If the computation was killed, restart it after some time.
             (when (eq state 'killed)
               (setq timer (run-at-time debounce nil start request)))
             (setq request nil))))
        ((pred stringp)
         (funcall cancel)
         (if (or (equal action "") (equal action current))
             (funcall async 'indicator 'finished)
           (funcall start action)))
        ('destroy
         (funcall cancel)
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult--dynamic-collection (fun)
  "Dynamic collection with input splitting.
FUN computes the candidates given the input."
  (thread-first
    (consult--async-sink)
    (consult--dynamic-compute fun)
    (consult--async-throttle)
    (consult--async-split)))

;;;; Special keymaps

(defvar-keymap consult-async-map
  :doc "Keymap added for commands with asynchronous candidates."
  ;; Overwriting some unusable defaults of default minibuffer completion.
  "<remap> <minibuffer-complete-word>" #'self-insert-command
  ;; Remap Emacs 29 history and default completion for now
  ;; (gh:minad/consult#613).
  "<remap> <minibuffer-complete-defaults>" #'ignore
  "<remap> <minibuffer-complete-history>" #'consult-history)

(defvar-keymap consult-narrow-map
  :doc "Narrowing keymap which is added to the local minibuffer map.
Note that `consult-narrow-key' and `consult-widen-key' are bound dynamically."
  "SPC" consult--narrow-space
  "DEL" consult--narrow-delete)

;;;; Internal API: consult--read

(defun consult--annotate-align (cand ann)
  "Align annotation ANN by computing the maximum CAND width."
  (setq consult--annotate-align-width
        (max consult--annotate-align-width
             (* (ceiling (consult--display-width cand)
                         consult--annotate-align-step)
                consult--annotate-align-step)))
  (when ann
    (concat
     #("   " 0 1 (display (space :align-to (+ left consult--annotate-align-width))))
     ann)))

(defun consult--add-history (async items)
  "Add ITEMS to the minibuffer future history.
ASYNC must be non-nil for async completion functions."
  (delete-dups
   (append
    ;; the defaults are at the beginning of the future history
    (ensure-list minibuffer-default)
    ;; then our custom items
    (remove "" (remq nil (ensure-list items)))
    ;; Add all the completions for non-async commands.  For async commands this
    ;; feature is not useful, since if one selects a completion candidate, the
    ;; async search is restarted using that candidate string.  This usually does
    ;; not yield a desired result since the async input uses a special format,
    ;; e.g., `#grep#filter'.
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
                      map))
      old-map))))

(defun consult--tofu-hide-in-minibuffer (&rest _)
  "Hide the tofus in the minibuffer."
  (let* ((min (minibuffer-prompt-end))
         (max (point-max))
         (pos max))
    (while (and (> pos min) (consult--tofu-p (char-before pos)))
      (cl-decf pos))
    (when (< pos max)
      (add-text-properties pos max '(invisible t rear-nonsticky t cursor-intangible t)))))

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
                      ;; The default completion UI adds the
                      ;; `completions-annotations' face if no other faces are
                      ;; present.
                      (if (text-property-not-all 0 (length ann) 'face nil ann)
                          ann
                        (propertize ann 'face 'completions-annotations))))))
          cands))

(cl-defun consult--read-1 (table &key
                                 prompt predicate require-match history default
                                 keymap category initial narrow add-history annotate
                                 state preview-key sort lookup group inherit-input-method)
  "See `consult--read' for the documentation of the arguments."
  (consult--minibuffer-with-setup-hook
      (:append (lambda ()
                 (add-hook 'after-change-functions #'consult--tofu-hide-in-minibuffer nil 'local)
                 (consult--setup-keymap keymap (consult--async-p table) narrow preview-key)
                 (setq-local minibuffer-default-add-function
                             (apply-partially #'consult--add-history (consult--async-p table) add-history))))
    (consult--with-async (async table)
      (consult--with-preview
          preview-key state
          (lambda (narrow input cand)
            (funcall lookup cand (funcall async nil) input narrow))
          (apply-partially #'run-hook-with-args-until-success
                           'consult--completion-candidate-hook)
          (pcase-exhaustive history
            (`(:input ,var) var)
            ((pred symbolp)))
        ;; Do not unnecessarily let-bind the lambdas to avoid over-capturing in
        ;; the interpreter.  This will make closures and the lambda string
        ;; representation larger, which makes debugging much worse.  Fortunately
        ;; the over-capturing problem does not affect the bytecode interpreter
        ;; which does a proper scope analysis.
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
               (consult--annotate-align-width 0)
               (selected
                (completing-read
                 prompt
                 (lambda (str pred action)
                   (let ((result (complete-with-action action (funcall async nil) str pred)))
                     (if (eq action 'metadata)
                         (if (and (eq (car result) 'metadata) (cdr result))
                             ;; Merge metadata
                             `(metadata ,@(cdr metadata) ,@(cdr result))
                           metadata)
                       result)))
                 predicate require-match initial
                 (if (symbolp history) history (cadr history))
                 default
                 inherit-input-method)))
          ;; Repair the null completion semantics. `completing-read' may return
          ;; an empty string even if REQUIRE-MATCH is non-nil. One can always
          ;; opt-in to null completion by passing the empty string for DEFAULT.
          (when (and (eq require-match t) (not default) (equal selected ""))
            (user-error "No selection"))
          selected)))))

(cl-defun consult--read (table &rest options &key
                               prompt predicate require-match history default
                               keymap category initial narrow add-history annotate
                               state preview-key sort lookup group inherit-input-method)
  "Enhanced completing read function to select from TABLE.

The function is a thin wrapper around `completing-read'.  Keyword
arguments are used instead of positional arguments for code
clarity.  On top of `completing-read' it additionally supports
computing the candidate list asynchronously, candidate preview
and narrowing.  You should use `completing-read' instead of
`consult--read' if you don't use asynchronous candidate
computation or candidate preview.

Keyword OPTIONS:

PROMPT is the string which is shown as prompt in the minibuffer.
PREDICATE is a filter function called for each candidate, returns
nil or t.
REQUIRE-MATCH equals t means that an exact match is required.
HISTORY is the symbol of the history variable.
DEFAULT is the default selected value.
ADD-HISTORY is a list of items to add to the history.
CATEGORY is the completion category symbol.
SORT should be set to nil if the candidates are already sorted.
This will disable sorting in the completion UI.
LOOKUP is a lookup function passed the selected candidate string,
the list of candidates, the current input string and the current
narrowing value.
ANNOTATE is a function passed a candidate string.  The function
should either return an annotation string or a list of three
strings (candidate prefix postfix).
INITIAL is the initial input string.
STATE is the state function, see `consult--with-preview'.
GROUP is a completion metadata `group-function' as documented in
the Elisp manual.
PREVIEW-KEY are the preview keys.  Can be nil, `any', a single
key or a list of keys.
NARROW is an alist of narrowing prefix strings and description.
KEYMAP is a command-specific keymap.
INHERIT-INPUT-METHOD, if non-nil the minibuffer inherits the
input method."
  ;; supported types
  (cl-assert (or (functionp table)     ;; dynamic table or asynchronous function
                 (obarrayp table)      ;; obarray
                 (hash-table-p table)  ;; hash table
                 (not table)           ;; empty list
                 (stringp (car table)) ;; string list
                 (and (consp (car table)) (stringp (caar table)))   ;; string alist
                 (and (consp (car table)) (symbolp (caar table))))) ;; symbol alist
  (ignore prompt predicate require-match history default
          keymap category initial narrow add-history annotate
          state preview-key sort lookup group inherit-input-method)
  (apply #'consult--read-1 table
         (append
          (consult--customize-get)
          options
          (list :prompt "Select: "
                :preview-key consult-preview-key
                :sort t
                :lookup (lambda (selected &rest _) selected)))))

;;;; Internal API: consult--prompt

(cl-defun consult--prompt-1 (&key prompt history add-history initial default
                                  keymap state preview-key transform inherit-input-method)
  "See `consult--prompt' for documentation."
  (consult--minibuffer-with-setup-hook
      (:append (lambda ()
                 (consult--setup-keymap keymap nil nil preview-key)
                 (setq-local minibuffer-default-add-function
                             (apply-partially #'consult--add-history nil add-history))))
    (consult--with-preview
        preview-key state
        (lambda (_narrow inp _cand) (funcall transform inp))
        (lambda () "")
        history
        (read-from-minibuffer prompt initial nil nil history default inherit-input-method))))

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
PREVIEW-KEY are the preview keys (nil, `any', a single key or a list of keys).
KEYMAP is a command-specific keymap."
  (ignore prompt history add-history initial default
          keymap state preview-key transform inherit-input-method)
  (apply #'consult--prompt-1
         (append
          (consult--customize-get)
          options
          (list :prompt "Input: "
                :preview-key consult-preview-key
                :transform #'identity))))

;;;; Internal API: consult--multi

(defsubst consult--multi-source (sources cand)
  "Lookup source for CAND in SOURCES list."
  (aref sources (consult--tofu-get cand)))

(defun consult--multi-predicate (sources cand)
  "Predicate function called for each candidate CAND given SOURCES."
  (let* ((src (consult--multi-source sources cand))
         (narrow (plist-get src :narrow))
         (type (or (car-safe narrow) narrow -1)))
    (or (eq consult--narrow type)
        (not (or consult--narrow (plist-get src :hidden))))))

(defun consult--multi-narrow (sources)
  "Return narrow list from SOURCES."
  (thread-last sources
    (mapcar (lambda (src)
              (when-let (narrow (plist-get src :narrow))
                (if (consp narrow)
                    narrow
                  (when-let (name (plist-get src :name))
                    (cons narrow name))))))
    (delq nil)
    (delete-dups)))

(defun consult--multi-annotate (sources cand)
  "Annotate candidate CAND from multi SOURCES."
  (consult--annotate-align
   cand
   (let ((src (consult--multi-source sources cand)))
     (if-let ((fun (plist-get src :annotate)))
         (funcall fun (cdr (get-text-property 0 'multi-category cand)))
       (plist-get src :name)))))

(defun consult--multi-group (sources cand transform)
  "Return title of candidate CAND or TRANSFORM the candidate given SOURCES."
  (if transform cand
    (plist-get (consult--multi-source sources cand) :name)))

(defun consult--multi-preview-key (sources)
  "Return preview keys from SOURCES."
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
                         (ensure-list key)))
                     sources))))

(defun consult--multi-lookup (sources selected candidates _input narrow &rest _)
  "Lookup SELECTED in CANDIDATES given SOURCES, with potential NARROW."
  (if (or (string-blank-p selected)
          (not (consult--tofu-p (aref selected (1- (length selected))))))
      ;; Non-existing candidate without Tofu or default submitted (empty string)
      (let* ((src (cond
                   (narrow (seq-find (lambda (src)
                                       (let ((n (plist-get src :narrow)))
                                         (eq (or (car-safe n) n -1) narrow)))
                                     sources))
                   ((seq-find (lambda (src) (plist-get src :default)) sources))
                   ((seq-find (lambda (src) (not (plist-get src :hidden))) sources))
                   ((aref sources 0))))
             (idx (seq-position sources src))
             (def (and (string-blank-p selected) ;; default candidate
                       (seq-find (lambda (cand) (eq idx (consult--tofu-get cand))) candidates))))
        (if def
            (cons (cdr (get-text-property 0 'multi-category def)) src)
          `(,selected :match nil ,@src)))
    (if-let (found (member selected candidates))
        ;; Existing candidate submitted
        (cons (cdr (get-text-property 0 'multi-category (car found)))
              (consult--multi-source sources selected))
      ;; Non-existing Tofu'ed candidate submitted, e.g., via Embark
      `(,(substring selected 0 -1) :match nil ,@(consult--multi-source sources selected)))))

(defun consult--multi-candidates (sources)
  "Return `consult--multi' candidates from SOURCES."
  (let ((idx 0) candidates)
    (seq-doseq (src sources)
      (let* ((face (and (plist-member src :face) `(face ,(plist-get src :face))))
             (cat (plist-get src :category))
             (items (plist-get src :items))
             (items (if (functionp items) (funcall items) items)))
        (dolist (item items)
          (let* ((str (or (car-safe item) item))
                 (cand (consult--tofu-append str idx)))
            ;; Preserve existing `multi-category' datum of the candidate.
            (if (and (eq str item) (get-text-property 0 'multi-category str))
                (when face (add-text-properties 0 (length str) face cand))
              ;; Attach `multi-category' datum and face.
              (add-text-properties
               0 (length str)
               `(multi-category (,cat . ,(or (cdr-safe item) item)) ,@face) cand))
            (push cand candidates))))
      (cl-incf idx))
    (nreverse candidates)))

(defun consult--multi-enabled-sources (sources)
  "Return vector of enabled SOURCES."
  (vconcat
   (seq-filter (lambda (src)
                 (if-let (pred (plist-get src :enabled))
                     (funcall pred)
                   t))
               (mapcar (lambda (src)
                         (if (symbolp src) (symbol-value src) src))
                       sources))))

(defun consult--multi-state (sources)
  "State function given SOURCES."
  (when-let (states (delq nil (mapcar (lambda (src)
                                        (when-let (fun (plist-get src :state))
                                          (cons src (funcall fun))))
                                      sources)))
    (let (last-fun)
      (pcase-lambda (action `(,cand . ,src))
        (pcase action
          ('setup
           (pcase-dolist (`(,_ . ,fun) states)
             (funcall fun 'setup nil)))
          ('exit
           (pcase-dolist (`(,_ . ,fun) states)
             (funcall fun 'exit nil)))
          ('preview
           (let ((selected-fun (cdr (assq src states))))
             ;; If the candidate source changed during preview communicate to
             ;; the last source, that none of its candidates is previewed anymore.
             (when (and last-fun (not (eq last-fun selected-fun)))
               (funcall last-fun 'preview nil))
             (setq last-fun selected-fun)
             (when selected-fun
               (funcall selected-fun 'preview cand))))
          ('return
           (let ((selected-fun (cdr (assq src states))))
             ;; Finish all the sources, except the selected one.
             (pcase-dolist (`(,_ . ,fun) states)
               (unless (eq fun selected-fun)
                 (funcall fun 'return nil)))
             ;; Finish the source with the selected candidate
             (when selected-fun
               (funcall selected-fun 'return cand)))))))))

(defun consult--multi (sources &rest options)
  "Select from candidates taken from a list of SOURCES.

OPTIONS is the plist of options passed to `consult--read'.  The following
options are supported: :require-match, :history, :keymap, :initial,
:add-history, :sort and :inherit-input-method.  The other options of
`consult--read' are used by the implementation of `consult--multi' and
should not be overwritten, except in in special scenarios.

The function returns the selected candidate in the form (cons candidate
source-plist).  The plist has the key :match with a value nil if the
candidate does not exist, t if the candidate exists and `new' if the
candidate has been created.  The sources of the source list can either be
symbols of source variables or source values.  Source values must be
plists with fields from the following list.

Required source fields:
* :category - Completion category symbol.
* :items - List of strings to select from or function returning
  list of strings.  Note that the strings can use text properties
  to carry metadata, which is then available to the :annotate,
  :action and :state functions.

Optional source fields:
* :name - Name of the source as a string, used for narrowing,
  group titles and annotations.
* :narrow - Narrowing character or (character . string) pair.
* :enabled - Function which must return t if the source is enabled.
* :hidden - When t candidates of this source are hidden by default.
* :face - Face used for highlighting the candidates.
* :annotate - Annotation function called for each candidate, returns string.
* :history - Name of history variable to add selected candidate.
* :default - Must be t if the first item of the source is the default value.
* :action - Function called with the selected candidate.
* :new - Function called with new candidate name, only if :require-match is nil.
* :state - State constructor for the source, must return the
  state function.  The state function is informed about state
  changes of the UI and can be used to implement preview.
* Other custom source fields can be added depending on the use
  case.  Note that the source is returned by `consult--multi'
  together with the selected candidate."
  (let* ((sources (consult--multi-enabled-sources sources))
         (candidates (consult--with-increased-gc
                      (consult--multi-candidates sources)))
         (selected
          (apply #'consult--read
                 candidates
                 (append
                  options
                  (list
                   :category    'multi-category
                   :predicate   (apply-partially #'consult--multi-predicate sources)
                   :annotate    (apply-partially #'consult--multi-annotate sources)
                   :group       (apply-partially #'consult--multi-group sources)
                   :lookup      (apply-partially #'consult--multi-lookup sources)
                   :preview-key (consult--multi-preview-key sources)
                   :narrow      (consult--multi-narrow sources)
                   :state       (consult--multi-state sources))))))
    (when-let (history (plist-get (cdr selected) :history))
      (add-to-history history (car selected)))
    (if (plist-member (cdr selected) :match)
        (when-let (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))

;;;; Customization macro

(defun consult--customize-put (cmds prop form)
  "Set property PROP to FORM of commands CMDS."
  (dolist (cmd cmds)
    (cond
     ((and (boundp cmd) (consp (symbol-value cmd)))
      (setf (plist-get (symbol-value cmd) prop) (eval form 'lexical)))
     ((functionp cmd)
      (setf (plist-get (alist-get cmd consult--customize-alist) prop) form))
     (t (user-error "%s is neither a Command command nor a source" cmd))))
  nil)

(defmacro consult-customize (&rest args)
  "Set properties of commands or sources.
ARGS is a list of commands or sources followed by the list of
keyword-value pairs.  For `consult-customize' to succeed, the
customized sources and commands must exist.  When a command is
invoked, the value of `this-command' is used to lookup the
corresponding customization options."
  (let (setter)
    (while args
      (let ((cmds (seq-take-while (lambda (x) (not (keywordp x))) args)))
        (setq args (seq-drop-while (lambda (x) (not (keywordp x))) args))
        (while (keywordp (car args))
          (push `(consult--customize-put ',cmds ,(car args) ',(cadr args)) setter)
          (setq args (cddr args)))))
    (macroexp-progn setter)))

(defun consult--customize-get ()
  "Get configuration from `consult--customize-alist' for `this-command'."
  (mapcar (lambda (x) (eval x 'lexical))
          (alist-get this-command consult--customize-alist)))

;;;; Commands

;;;;; Command: consult-completion-in-region

(defun consult--insertion-preview (start end)
  "State function for previewing a candidate in a specific region.
The candidates are previewed in the region from START to END.  This function is
used as the `:state' argument for `consult--read' in the `consult-yank' family
of functions and in `consult-completion-in-region'."
  (unless (or (minibufferp)
              ;; Disable preview if anything odd is going on with the markers.
              ;; Otherwise we get "Marker points into wrong buffer errors".  See
              ;; gh:minad/consult#375, where Org mode source blocks are
              ;; completed in a different buffer than the original buffer.  This
              ;; completion is probably also problematic in my Corfu completion
              ;; package.
              (not (eq (window-buffer) (current-buffer)))
              (and (markerp start) (not (eq (marker-buffer start) (current-buffer))))
              (and (markerp end) (not (eq (marker-buffer end) (current-buffer)))))
    (let (ov)
      (lambda (action cand)
        (cond
         ((and (not cand) ov)
          (delete-overlay ov)
          (setq ov nil))
         ((and (eq action 'preview) cand)
          (unless ov
            (setq ov (consult--make-overlay start end
                                            'invisible t
                                            'window (selected-window))))
          ;; Use `add-face-text-property' on a copy of "cand in order to merge face properties
          (setq cand (copy-sequence cand))
          (add-face-text-property 0 (length cand) 'consult-preview-insertion t cand)
          ;; Use the `before-string' property since the overlay might be empty.
          (overlay-put ov 'before-string cand)))))))

;;;###autoload
(defun consult-completion-in-region (start end collection &optional predicate)
  "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION
PREDICATE.  The arguments and expected return value are as
specified for `completion-in-region'.  Use this function as a
value for `completion-in-region-function'."
  (barf-if-buffer-read-only)
  (let* ((initial (buffer-substring-no-properties start end))
         (metadata (completion-metadata initial collection predicate))
         ;; TODO: `minibuffer-completing-file-name' is mostly deprecated, but
         ;; still in use. Packages should instead use the completion metadata.
         (minibuffer-completing-file-name
          (eq 'file (completion-metadata-get metadata 'category)))
         (threshold (completion--cycle-threshold metadata))
         (all (completion-all-completions initial collection predicate (length initial)))
         ;; Wrap all annotation functions to ensure that they are executed
         ;; in the original buffer.
         (exit-fun (plist-get completion-extra-properties :exit-function))
         (ann-fun (plist-get completion-extra-properties :annotation-function))
         (aff-fun (plist-get completion-extra-properties :affixation-function))
         (docsig-fun (plist-get completion-extra-properties :company-docsig))
         (completion-extra-properties
          `(,@(and ann-fun (list :annotation-function (consult--in-buffer ann-fun)))
            ,@(and aff-fun (list :affixation-function (consult--in-buffer aff-fun)))
            ;; Provide `:annotation-function' if `:company-docsig' is specified.
            ,@(and docsig-fun (not ann-fun) (not aff-fun)
                   (list :annotation-function
                         (consult--in-buffer
                          (lambda (cand)
                            (concat (propertize " " 'display '(space :align-to center))
                                    (funcall docsig-fun cand)))))))))
    ;; error if `threshold' is t or the improper list `all' is too short
    (if (and threshold
             (or (not (consp (ignore-errors (nthcdr threshold all))))
                 (and completion-cycling completion-all-sorted-completions)))
        (completion--in-region start end collection predicate)
      (let* ((limit (car (completion-boundaries initial collection predicate "")))
             (this-command #'consult-completion-in-region)
             (completion
              (cond
               ((atom all) nil)
               ((and (consp all) (atom (cdr all)))
                (concat (substring initial 0 limit) (car all)))
               (t
                (consult--local-let ((enable-recursive-minibuffers t))
                  ;; Evaluate completion table in the original buffer.
                  ;; This is a reasonable thing to do and required by
                  ;; some completion tables in particular by lsp-mode.
                  ;; See gh:minad/vertico#61.
                  (consult--read (consult--completion-table-in-buffer collection)
                                 :prompt "Completion: "
                                 :state (consult--insertion-preview start end)
                                 :predicate predicate
                                 :initial initial))))))
        (if completion
            (progn
              ;; bug#55205: completion--replace removes properties!
              (completion--replace start end (setq completion (concat completion)))
              (when exit-fun
                (funcall exit-fun completion
                         ;; If completion is finished and cannot be further
                         ;; completed, return `finished'.  Otherwise return
                         ;; `exact'.
                         (if (eq (try-completion completion collection predicate) t)
                             'finished 'exact)))
              t)
          (message "No completion")
          nil)))))

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
         (buffer (current-buffer))
         candidates)
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion
               (if-let (fun (bound-and-true-p outline-search-function))
                   (funcall fun)
                 (re-search-forward heading-regexp nil t)))
        (cl-incf line (consult--count-lines (match-beginning 0)))
        (push (consult--location-candidate
               (consult--buffer-substring (pos-bol) (pos-eol) 'fontify)
               (cons buffer (point)) (1- line) (1- line)
               'consult--outline-level (funcall level-fun))
              candidates)
        (goto-char (1+ (pos-eol)))))
    (unless candidates
      (user-error "No headings"))
    (nreverse candidates)))

;;;###autoload
(defun consult-outline (&optional level)
  "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate
preview.  The initial narrowing LEVEL can be given as prefix
argument.  The symbol at point is added to the future history."
  (interactive
   (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((candidates (consult--slow-operation
                         "Collecting headings..."
                       (consult--outline-candidates)))
         (min-level (- (cl-loop for cand in candidates minimize
                                (get-text-property 0 'consult--outline-level cand))
                       ?1))
         (narrow-pred (lambda (cand)
                        (<= (get-text-property 0 'consult--outline-level cand)
                            (+ consult--narrow min-level))))
         (narrow-keys (mapcar (lambda (c) (cons c (format "Level %c" c)))
                              (number-sequence ?1 ?9)))
         (narrow-init (and level (max ?1 (min ?9 (+ level ?0))))))
    (consult--read
     candidates
     :prompt "Go to heading: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--line-match
     :narrow `(:predicate ,narrow-pred :keys ,narrow-keys :initial ,narrow-init)
     :history '(:input consult--line-history)
     :add-history (thing-at-point 'symbol)
     :state (consult--location-state candidates))))

;;;;; Command: consult-mark

(defun consult--mark-candidates (markers)
  "Return list of candidates strings for MARKERS."
  (consult--forbid-minibuffer)
  (let ((candidates)
        (current-buf (current-buffer)))
    (save-excursion
      (dolist (marker markers)
        (when-let ((pos (marker-position marker))
                   (buf (marker-buffer marker)))
          (when (and (eq buf current-buf)
                     (consult--in-range-p pos))
            (goto-char pos)
            ;; `line-number-at-pos' is a very slow function, which should be
            ;; replaced everywhere.  However in this case the slow
            ;; line-number-at-pos does not hurt much, since the mark ring is
            ;; usually small since it is limited by `mark-ring-max'.
            (push (consult--location-candidate
                   (consult--line-with-mark marker) marker
                   (line-number-at-pos pos consult-line-numbers-widen)
                   marker)
                  candidates)))))
    (unless candidates
      (user-error "No marks"))
    (nreverse (delete-dups candidates))))

;;;###autoload
(defun consult-mark (&optional markers)
  "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
   (consult--mark-candidates
    (or markers (cons (mark-marker) mark-ring)))
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

(defun consult--global-mark-candidates (markers)
  "Return list of candidates strings for MARKERS."
  (consult--forbid-minibuffer)
  (let ((candidates))
    (save-excursion
      (dolist (marker markers)
        (when-let ((pos (marker-position marker))
                   (buf (marker-buffer marker)))
          (unless (minibufferp buf)
            (with-current-buffer buf
              (when (consult--in-range-p pos)
                (goto-char pos)
                ;; `line-number-at-pos' is slow, see comment in `consult--mark-candidates'.
                (let* ((line (line-number-at-pos pos consult-line-numbers-widen))
                       (prefix (consult--format-file-line-match (buffer-name buf) line ""))
                       (cand (concat prefix (consult--line-with-mark marker) (consult--tofu-encode marker))))
                  (put-text-property 0 (length prefix) 'consult-strip t cand)
                  (put-text-property 0 (length cand) 'consult-location (cons marker line) cand)
                  (push cand candidates))))))))
    (unless candidates
      (user-error "No global marks"))
    (nreverse (delete-dups candidates))))

;;;###autoload
(defun consult-global-mark (&optional markers)
  "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
   (consult--global-mark-candidates
    (or markers global-mark-ring))
   :prompt "Go to global mark: "
   ;; Despite `consult-global-mark' formatting the candidates in grep-like
   ;; style, we are not using the `consult-grep' category, since the candidates
   ;; have location markers attached.
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--line-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

;;;;; Command: consult-line

(defun consult--line-candidates (top curr-line)
  "Return list of line candidates.
Start from top if TOP non-nil.
CURR-LINE is the current line number."
  (consult--forbid-minibuffer)
  (consult--fontify-all)
  (let* ((buffer (current-buffer))
         (line (line-number-at-pos (point-min) consult-line-numbers-widen))
         default-cand candidates)
    (consult--each-line beg end
      (unless (looking-at-p "^\\s-*$")
        (push (consult--location-candidate
               (consult--buffer-substring beg end)
               (cons buffer beg) line line)
              candidates)
        (when (and (not default-cand) (>= line curr-line))
          (setq default-cand candidates)))
      (cl-incf line))
    (unless candidates
      (user-error "No lines"))
    (nreverse
     (if (or top (not default-cand))
         candidates
       (let ((before (cdr default-cand)))
         (setcdr default-cand nil)
         (nconc before candidates))))))

(defun consult--line-point-placement (selected candidates highlighted &rest ignored-faces)
  "Find point position on matching line.
SELECTED is the currently selected candidate.
CANDIDATES is the list of candidates.
HIGHLIGHTED is the highlighted string to determine the match position.
IGNORED-FACES are ignored when determining the match position."
  (when-let (pos (consult--lookup-location selected candidates))
    (if highlighted
        (let* ((matches (apply #'consult--point-placement highlighted 0 ignored-faces))
               (dest (+ pos (car matches))))
          ;; Only create a new marker when jumping across buffers (for example
          ;; `consult-line-multi').  Avoid creating unnecessary markers, when
          ;; scrolling through candidates, since creating markers is not free.
          (when (and (markerp pos) (not (eq (marker-buffer pos) (current-buffer))))
            (setq dest (move-marker (make-marker) dest (marker-buffer pos))))
          (cons dest (cdr matches)))
      pos)))

(defun consult--line-match (selected candidates input &rest _)
  "Lookup position of match.
SELECTED is the currently selected candidate.
CANDIDATES is the list of candidates.
INPUT is the input string entered by the user."
  (consult--line-point-placement selected candidates
                                 (and (not (string-blank-p input))
                                      (car (consult--completion-filter
                                            input
                                            (list (substring-no-properties selected))
                                            'consult-location 'highlight)))
                                 'completions-first-difference))

;;;###autoload
(defun consult-line (&optional initial start)
  "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history."
  (interactive (list nil (not (not current-prefix-arg))))
  (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
         (top (not (eq start consult-line-start-from-top)))
         (candidates (consult--slow-operation "Collecting lines..."
                       (consult--line-candidates top curr-line))))
    (consult--read
     candidates
     :prompt (if top "Go to line from top: " "Go to line: ")
     :annotate (consult--line-prefix curr-line)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last `isearch-string' to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add `isearch-string' as initial input if starting from Isearch
     :initial (or initial
                  (and isearch-mode
                       (prog1 isearch-string (isearch-done))))
     :state (consult--location-state candidates))))

;;;;; Command: consult-line-multi

(defun consult--line-multi-match (selected candidates &rest _)
  "Lookup position of match.
SELECTED is the currently selected candidate.
CANDIDATES is the list of candidates."
  (consult--line-point-placement selected candidates
                                 (car (member selected candidates))))

(defun consult--line-multi-group (cand transform)
  "Group function used by `consult-line-multi'.
If TRANSFORM non-nil, return transformed CAND, otherwise return title."
  (if transform cand
    (let* ((marker (car (get-text-property 0 'consult-location cand)))
          (buf (if (consp marker)
                   (car marker) ;; Handle cheap marker
                 (marker-buffer marker))))
      (if buf (buffer-name buf) "Dead buffer"))))

(defun consult--line-multi-candidates (buffers input)
  "Collect matching candidates from multiple buffers.
INPUT is the user input which should be matched.
BUFFERS is the list of buffers."
  (pcase-let ((`(,regexps . ,hl)
               (funcall consult--regexp-compiler
                        input 'emacs completion-ignore-case))
              (candidates nil)
              (cand-idx 0))
    (save-match-data
      (dolist (buf buffers (nreverse candidates))
        (with-current-buffer buf
          (save-excursion
            (let ((line (line-number-at-pos (point-min) consult-line-numbers-widen)))
              (goto-char (point-min))
              (while (and (not (eobp))
                          (save-excursion (re-search-forward (car regexps) nil t)))
                (cl-incf line (consult--count-lines (match-beginning 0)))
                (let ((bol (pos-bol))
                      (eol (pos-eol)))
                  (goto-char bol)
                  (when (and (not (looking-at-p "^\\s-*$"))
                             (seq-every-p (lambda (r)
                                            (goto-char bol)
                                            (re-search-forward r eol t))
                                          (cdr regexps)))
                    (push (consult--location-candidate
                           (funcall hl (buffer-substring-no-properties bol eol))
                           (cons buf bol) (1- line) cand-idx)
                          candidates)
                    (cl-incf cand-idx))
                  (goto-char (1+ eol)))))))))))

;;;###autoload
(defun consult-line-multi (query &optional initial)
  "Search for a matching line in multiple buffers.

By default search across all project buffers.  If the prefix
argument QUERY is non-nil, all buffers are searched.  Optional
INITIAL input can be provided.  The symbol at point and the last
`isearch-string' is added to the future history.  In order to
search a subset of buffers, QUERY can be set to a plist according
to `consult--buffer-query'."
  (interactive "P")
  (unless (keywordp (car-safe query))
    (setq query (list :sort 'alpha-current :directory (and (not query) 'project))))
  (pcase-let* ((`(,prompt . ,buffers) (consult--buffer-query-prompt "Go to line" query))
               (collection (consult--dynamic-collection
                            (apply-partially #'consult--line-multi-candidates
                                             buffers))))
    (consult--read
     collection
     :prompt prompt
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last Isearch string to future history
     :add-history (mapcar #'consult--async-split-initial
                          (delq nil (list (thing-at-point 'symbol)
                                          isearch-string)))
     :history '(:input consult--line-multi-history)
     :lookup #'consult--line-multi-match
     ;; Add `isearch-string' as initial input if starting from Isearch
     :initial (consult--async-split-initial
               (or initial
                   (and isearch-mode
                        (prog1 isearch-string (isearch-done)))))
     :state (consult--location-state (lambda () (funcall collection nil)))
     :group #'consult--line-multi-group)))

;;;;; Command: consult-keep-lines

(defun consult--keep-lines-state (filter)
  "State function for `consult-keep-lines' with FILTER function."
  (let ((font-lock-orig font-lock-mode)
        (whitespace-orig (bound-and-true-p whitespace-mode))
        (hl-line-orig (bound-and-true-p hl-line-mode))
        (point-orig (point))
        lines content-orig replace last-input)
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
                            (insert-before-markers content)
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
    (lambda (action input)
      ;; Restoring content and point position
      (when (and (eq action 'return) last-input)
        ;; No undo recording, modification hooks, buffer modified-status
        (with-silent-modifications (funcall replace content-orig point-orig)))
      ;; Committing or new input provided -> Update
      (when (and input ;; Input has been provided
                 (or
                  ;; Committing, but not with empty input
                  (and (eq action 'return) (not (string-match-p "\\`!? ?\\'" input)))
                  ;; Input has changed
                  (not (equal input last-input))))
        (let ((filtered-content
               (if (string-match-p "\\`!? ?\\'" input)
                   ;; Special case the empty input for performance.
                   ;; Otherwise it could happen that the minibuffer is empty,
                   ;; but the buffer has not been updated.
                   content-orig
                 (if (eq action 'return)
                     (apply #'concat (mapcan (lambda (x) (list x "\n"))
                                             (funcall filter input lines)))
                   (while-no-input
                     ;; Heavy computation is interruptible if *not* committing!
                     ;; Allocate new string candidates since the matching function mutates!
                     (apply #'concat (mapcan (lambda (x) (list x "\n"))
                                             (funcall filter input (mapcar #'copy-sequence lines)))))))))
          (when (stringp filtered-content)
            (when font-lock-mode (font-lock-mode -1))
            (when (bound-and-true-p whitespace-mode) (whitespace-mode -1))
            (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
            (if (eq action 'return)
                (atomic-change-group
                  ;; Disable modification hooks for performance
                  (let ((inhibit-modification-hooks t))
                    (funcall replace filtered-content)))
              ;; No undo recording, modification hooks, buffer modified-status
              (with-silent-modifications
                (funcall replace filtered-content)
                (setq last-input input))))))
      ;; Restore modes
      (when (eq action 'return)
        (when hl-line-orig (hl-line-mode 1))
        (when whitespace-orig (whitespace-mode 1))
        (when font-lock-orig (font-lock-mode 1))))))

;;;###autoload
(defun consult-keep-lines (filter &optional initial)
  "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  When
called from Elisp, the filtering is performed by a FILTER function.  This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input."
  (interactive
   (list (lambda (pattern cands)
           ;; Use consult-location completion category when filtering lines
           (consult--completion-filter-dispatch
            pattern cands 'consult-location 'highlight))))
  (consult--forbid-minibuffer)
  (let ((ro buffer-read-only))
    (unwind-protect
        (consult--minibuffer-with-setup-hook
            (lambda ()
              (when ro
                (minibuffer-message
                 (substitute-command-keys
                  " [Unlocked read-only buffer. \\[minibuffer-keyboard-quit] to quit.]"))))
          (setq buffer-read-only nil)
          (consult--with-increased-gc
           (consult--prompt
            :prompt "Keep lines: "
            :initial initial
            :history 'consult--line-history
            :state (consult--keep-lines-state filter))))
      (setq buffer-read-only ro))))

;;;;; Command: consult-focus-lines

(defun consult--focus-lines-state (filter)
  "State function for `consult-focus-lines' with FILTER function."
  (let (lines overlays last-input pt-orig pt-min pt-max)
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
        (setq pt-orig (point) pt-min (point-min) pt-max (point-max))
        (let ((i 0))
          (consult--each-line beg end
            ;; Use "\n" for empty lines, since we need a non-empty string to
            ;; attach the text property to.
            (let ((line (if (eq beg end) (char-to-string ?\n)
                          (buffer-substring-no-properties beg end))))
              (put-text-property 0 1 'consult--focus-line (cons (cl-incf i) beg) line)
              (push line lines)))
          (setq lines (nreverse lines)))))
    (lambda (action input)
      ;; New input provided -> Update
      (when (and input (not (equal input last-input)))
        (let (new-overlays)
          (pcase (while-no-input
                   (unless (string-match-p "\\`!? ?\\'" input) ;; Empty input.
                     (let* ((inhibit-quit (eq action 'return)) ;; Non interruptible, when quitting!
                            (not (string-prefix-p "! " input))
                            (stripped (string-remove-prefix "! " input))
                            (matches (funcall filter stripped lines))
                            (old-ind 0)
                            (block-beg pt-min)
                            (block-end pt-min))
                       (while old-ind
                         (let ((match (pop matches)) (ind nil) (beg pt-max) (end pt-max) prop)
                           (when match
                             (setq prop (get-text-property 0 'consult--focus-line match)
                                   ind (car prop)
                                   beg (cdr prop)
                                   ;; Check for empty lines, see above.
                                   end (+ 1 beg (if (equal match "\n") 0 (length match)))))
                           (unless (eq ind (1+ old-ind))
                             (let ((a (if not block-beg block-end))
                                   (b (if not block-end beg)))
                               (when (/= a b)
                                 (push (consult--make-overlay a b 'invisible t) new-overlays)))
                             (setq block-beg beg))
                           (setq block-end end old-ind ind)))))
                   'commit)
            ('commit
             (mapc #'delete-overlay overlays)
             (setq last-input input overlays new-overlays))
            (_ (mapc #'delete-overlay new-overlays)))))
      (when (eq action 'return)
        (cond
         ((not input)
          (mapc #'delete-overlay overlays)
          (goto-char pt-orig))
         ((equal input "")
          (consult-focus-lines nil 'show)
          (goto-char pt-orig))
         (t
          ;; Successfully terminated -> Remember invisible overlays
          (setq consult--focus-lines-overlays
                (nconc consult--focus-lines-overlays overlays))
          ;; move point past invisible
          (goto-char (if-let (ov (and (invisible-p pt-orig)
                                      (seq-find (lambda (ov) (overlay-get ov 'invisible))
                                                (overlays-at pt-orig))))
                         (overlay-end ov)
                       pt-orig))))))))

;;;###autoload
(defun consult-focus-lines (filter &optional show initial)
  "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  With
optional prefix argument SHOW reveal the hidden lines.  Alternatively the
command can be restarted to reveal the lines.  When called from Elisp, the
filtering is performed by a FILTER function.  This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input."
  (interactive
   (list (lambda (pattern cands)
           ;; Use consult-location completion category when filtering lines
           (consult--completion-filter-dispatch
            pattern cands 'consult-location nil))
         current-prefix-arg))
  (if show
      (progn
        (mapc #'delete-overlay consult--focus-lines-overlays)
        (setq consult--focus-lines-overlays nil)
        (message "All lines revealed"))
    (consult--forbid-minibuffer)
    (consult--with-increased-gc
     (consult--prompt
      :prompt
      (if consult--focus-lines-overlays
          "Focus on lines (RET to reveal): "
        "Focus on lines: ")
      :initial initial
      :history 'consult--line-history
      :state (consult--focus-lines-state filter)))))

;;;;; Command: consult-goto-line

(defun consult--goto-line-position (str msg)
  "Transform input STR to line number.
Print an error message with MSG function."
  (save-match-data
    (if (and str (string-match "\\`\\([[:digit:]]+\\):?\\([[:digit:]]*\\)\\'" str))
        (let ((line (string-to-number (match-string 1 str)))
              (col (string-to-number (match-string 2 str))))
          (save-excursion
            (save-restriction
              (when consult-line-numbers-widen
                (widen))
              (goto-char (point-min))
              (forward-line (1- line))
              (goto-char (min (+ (point) col) (pos-eol)))
              (point))))
      (when (and str (not (equal str "")))
        (funcall msg "Please enter a number."))
      nil)))

;;;###autoload
(defun consult-goto-line (&optional arg)
  "Read line number and jump to the line with preview.

Enter either a line number to jump to the first column of the
given line or line:column in order to jump to a specific column.
Jump directly if a line number is given as prefix ARG.  The
command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'."
  (interactive "P")
  (if arg
      (call-interactively #'goto-line)
    (consult--forbid-minibuffer)
    (consult--local-let ((display-line-numbers consult-goto-line-numbers)
                         (display-line-numbers-widen consult-line-numbers-widen))
      (while (if-let (pos (consult--goto-line-position
                           (consult--prompt
                            :prompt "Go to line: "
                            :history 'goto-line-history
                            :state
                            (let ((preview (consult--jump-preview)))
                              (lambda (action str)
                                (funcall preview action
                                         (consult--goto-line-position str #'ignore)))))
                           #'minibuffer-message))
                 (consult--jump pos)
               t)))))

;;;;; Command: consult-recent-file

(defun consult--file-preview ()
  "Create preview function for files."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall preview action
               (and cand
                    (eq action 'preview)
                    (funcall open cand))))))

(defun consult--file-action (file)
  "Open FILE via `consult--buffer-action'."
  ;; Try to preserve the buffer as is, if it has already been opened, for
  ;; example in literal or raw mode.
  (setq file (abbreviate-file-name (expand-file-name file)))
  (consult--buffer-action (or (get-file-buffer file) (find-file-noselect file))))

(consult--define-state file)

;;;###autoload
(defun consult-recent-file ()
  "Find recent file using `completing-read'."
  (interactive)
  (find-file
   (consult--read
    (or
     (mapcar #'consult--fast-abbreviate-file-name (bound-and-true-p recentf-list))
     (user-error "No recent files, `recentf-mode' is %s"
                 (if recentf-mode "enabled" "disabled")))
    :prompt "Find recent file: "
    :sort nil
    :require-match t
    :category 'file
    :state (consult--file-preview)
    :history 'file-name-history)))

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
  (let* ((case-fold-search)
         (buffer (current-buffer))
         (command-filter (consult--regexp-filter (seq-filter #'stringp consult-mode-command-filter)))
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
              (let ((sym (cdr-safe cmd)))
                (when (and (consp cmd)
                           (eq (car cmd) 'defun)
                           (commandp sym)
                           (not (get sym 'byte-obsolete-info))
                           ;; Emacs 28 has a `read-extended-command-predicate'
                           (if (bound-and-true-p read-extended-command-predicate)
                               (funcall read-extended-command-predicate sym buffer)
                             t))
                  (let ((name (symbol-name sym)))
                    (unless (string-match-p command-filter name)
                      (push (propertize name
                                        'consult--candidate sym
                                        'consult--type key)
                            commands))))))))))))

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
  (let ((narrow `((?m . ,(format "Major: %s" major-mode))
                  (?l . "Local Minor")
                  (?g . "Global Minor"))))
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
      :group (consult--type-group narrow)
      :narrow narrow
      :require-match t
      :history 'extended-command-history
      :category 'command))))

;;;;; Command: consult-yank

(defun consult--read-from-kill-ring ()
  "Open kill ring menu and return selected string."
  ;; `current-kill' updates `kill-ring' with interprogram paste, see
  ;; gh:minad/consult#443.
  (current-kill 0)
  ;; Do not specify a :lookup function in order to preserve completion-styles
  ;; highlighting of the current candidate. We have to perform a final lookup to
  ;; obtain the original candidate which may be propertized with yank-specific
  ;; properties, like 'yank-handler.
  (consult--lookup-member
   (consult--read
    (consult--remove-dups
     (or (if consult-yank-rotate
             (append kill-ring-yank-pointer
                     (butlast kill-ring (length kill-ring-yank-pointer)))
           kill-ring)
         (user-error "Kill ring is empty")))
    :prompt "Yank from kill-ring: "
    :history t ;; disable history
    :sort nil
    :category 'kill-ring
    :require-match t
    :state
    (consult--insertion-preview
     (point)
     ;; If previous command is yank, hide previously yanked string
     (or (and (eq last-command 'yank) (mark t)) (point))))
   kill-ring))

;; Adapted from the Emacs `yank-from-kill-ring' function.
;;;###autoload
(defun consult-yank-from-kill-ring (string &optional arg)
  "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'.  Additionally the Consult
version supports preview of the selected string."
  (interactive (list (consult--read-from-kill-ring) current-prefix-arg))
  (when string
    (setq yank-window-start (window-start))
    (push-mark)
    (insert-for-yank string)
    (setq this-command 'yank)
    (when consult-yank-rotate
      (if-let (pos (seq-position kill-ring string))
          (setq kill-ring-yank-pointer (nthcdr pos kill-ring))
        (kill-new string)))
    (when (consp arg)
      ;; Swap point and mark like in `yank'.
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer)))))))

(put 'consult-yank-replace 'delete-selection 'yank)
(put 'consult-yank-pop 'delete-selection 'yank)
(put 'consult-yank-from-kill-ring 'delete-selection 'yank)

;;;###autoload
(defun consult-yank-pop (&optional arg)
  "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'.  Additionally the Consult
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
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall
       preview action
       ;; Only preview bookmarks with the default handler.
       (when-let ((bm (and cand (eq action 'preview) (assoc cand bookmark-alist)))
                  (handler (or (bookmark-get-handler bm) #'bookmark-default-handler))
                  ((eq handler #'bookmark-default-handler))
                  (file (bookmark-get-filename bm))
                  (pos (bookmark-get-position bm))
                  (buf (funcall open file)))
         (set-marker (make-marker) pos buf))))))

(defun consult--bookmark-action (bm)
  "Open BM via `consult--buffer-action'."
  (bookmark-jump bm consult--buffer-display))

(consult--define-state bookmark)

(defun consult--bookmark-candidates ()
  "Return bookmark candidates."
  (bookmark-maybe-load-default-file)
  (let ((narrow (cl-loop for (y _ . xs) in consult-bookmark-narrow nconc
                         (cl-loop for x in xs collect (cons x y)))))
    (cl-loop for bm in bookmark-alist collect
             (propertize (car bm)
                         'consult--type
                         (alist-get
                          (or (bookmark-get-handler bm) #'bookmark-default-handler)
                          narrow)))))

;;;###autoload
(defun consult-bookmark (name)
  "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing.  See the
variable `consult-bookmark-narrow' for the narrowing configuration."
  (interactive
   (list
    (let ((narrow (cl-loop for (x y . _) in consult-bookmark-narrow collect (cons x y))))
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

(declare-function ring-elements "ring")

(defun consult--current-history ()
  "Return the history and index variable relevant to the current buffer.
If the minibuffer is active, the minibuffer history is returned,
otherwise the history corresponding to the mode.  There is a
special case for `repeat-complex-command', for which the command
history is used."
  (cond
   ;; In the minibuffer we use the current minibuffer history,
   ;; which can be configured by setting `minibuffer-history-variable'.
   ((minibufferp)
    (when (eq minibuffer-history-variable t)
      (user-error "Minibuffer history is disabled for `%s'" this-command))
    (list (mapcar #'consult--tofu-hide
                  (if (eq minibuffer-history-variable 'command-history)
                      ;; If pressing "C-x M-:", i.e., `repeat-complex-command',
                      ;; we are instead querying the `command-history' and get a
                      ;; full s-expression.  Alternatively you might want to use
                      ;; `consult-complex-command', which can also be bound to
                      ;; "C-x M-:"!
                      (mapcar #'prin1-to-string command-history)
                    (symbol-value minibuffer-history-variable)))))
   ;; Otherwise we use a mode-specific history, see `consult-mode-histories'.
   (t (let ((found (seq-find (lambda (h)
                               (and (derived-mode-p (car h))
                                    (boundp (if (consp (cdr h)) (cadr h) (cdr h)))))
                             consult-mode-histories)))
        (unless found
          (user-error "No history configured for `%s', see `consult-mode-histories'"
                      major-mode))
        (cons (symbol-value (cadr found)) (cddr found))))))

;;;###autoload
(defun consult-history (&optional history index bol)
  "Insert string from HISTORY of current buffer.
In order to select from a specific HISTORY, pass the history
variable as argument.  INDEX is the name of the index variable to
update, if any.  BOL is the function which jumps to the beginning
of the prompt.  See also `cape-history' from the Cape package."
  (interactive)
  (pcase-let* ((`(,history ,index ,bol) (if history
                                            (list history index bol)
                                          (consult--current-history)))
               (history (if (ring-p history) (ring-elements history) history))
               (`(,beg . ,end)
                (if (minibufferp)
                    (cons (minibuffer-prompt-end) (point-max))
                  (if bol
                      (save-excursion
                        (funcall bol)
                        (cons (point) (pos-eol)))
                    (cons (point) (point)))))
               (str (consult--local-let ((enable-recursive-minibuffers t))
                      (consult--read
                       (or (consult--remove-dups history)
                           (user-error "History is empty"))
                       :prompt "History: "
                       :history t ;; disable history
                       :category ;; Report category depending on history variable
                       (and (minibufferp)
                            (pcase minibuffer-history-variable
                              ('extended-command-history 'command)
                              ('buffer-name-history 'buffer)
                              ('face-name-history 'face)
                              ('read-envvar-name-history 'environment-variable)
                              ('bookmark-history 'bookmark)
                              ('file-name-history 'file)))
                       :sort nil
                       :initial (buffer-substring-no-properties beg end)
                       :state (consult--insertion-preview beg end)))))
    (delete-region beg end)
    (when index
      (set index (seq-position history str)))
    (insert (substring-no-properties str))))

;;;;; Command: consult-isearch-history

(defun consult-isearch-forward (&optional reverse)
  "Continue Isearch forward optionally in REVERSE."
  (interactive)
  (consult--require-minibuffer)
  (setq isearch-new-forward (not reverse) isearch-new-nonincremental nil)
  (funcall (or (command-remapping #'exit-minibuffer) #'exit-minibuffer)))

(defun consult-isearch-backward (&optional reverse)
  "Continue Isearch backward optionally in REVERSE."
  (interactive)
  (consult-isearch-forward (not reverse)))

;; Emacs 28: hide in M-X
(put #'consult-isearch-backward 'completion-predicate #'ignore)
(put #'consult-isearch-forward 'completion-predicate #'ignore)

(defvar-keymap consult-isearch-history-map
  :doc "Additional keymap used by `consult-isearch-history'."
  "<remap> <isearch-forward>" #'consult-isearch-forward
  "<remap> <isearch-backward>" #'consult-isearch-backward)

(defun consult--isearch-history-candidates ()
  "Return Isearch history candidates."
  ;; Do not throw an error on empty history, in order to allow starting a
  ;; search.  We do not :require-match here.
  (let ((history (if (eq t search-default-mode)
                     (append regexp-search-ring search-ring)
                   (append search-ring regexp-search-ring))))
    (delete-dups
     (mapcar
      (lambda (cand)
        ;; The search type can be distinguished via text properties.
        (let* ((props (plist-member (text-properties-at 0 cand)
                                    'isearch-regexp-function))
               (type (pcase (cadr props)
                       ((and 'nil (guard (not props))) ?r)
                       ('nil                           ?l)
                       ('word-search-regexp            ?w)
                       ('isearch-symbol-regexp         ?s)
                       ('char-fold-to-regexp           ?c)
                       (_                              ?u))))
          ;; Disambiguate history items.  The same string could
          ;; occur with different search types.
          (consult--tofu-append cand type)))
      history))))

(defconst consult--isearch-history-narrow
  '((?c . "Char")
    (?u . "Custom")
    (?l . "Literal")
    (?r . "Regexp")
    (?s . "Symbol")
    (?w . "Word")))

;;;###autoload
(defun consult-isearch-history ()
  "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise."
  (interactive)
  (consult--forbid-minibuffer)
  (let* ((isearch-message-function #'ignore)
         (cursor-in-echo-area t) ;; Avoid cursor flickering
         (candidates (consult--isearch-history-candidates)))
    (unless isearch-mode (isearch-mode t))
    (with-isearch-suspended
     (setq isearch-new-string
           (consult--read
            candidates
            :prompt "I-search: "
            :category 'consult-isearch-history
            :history t ;; disable history
            :sort nil
            :initial isearch-string
            :keymap consult-isearch-history-map
            :annotate
            (lambda (cand)
              (consult--annotate-align
               cand
               (alist-get (consult--tofu-get cand) consult--isearch-history-narrow)))
            :group
            (lambda (cand transform)
              (if transform
                  cand
                (alist-get (consult--tofu-get cand) consult--isearch-history-narrow)))
            :lookup
            (lambda (selected candidates &rest _)
              (if-let (found (member selected candidates))
                  (substring (car found) 0 -1)
                selected))
            :state
            (lambda (action cand)
              (when (and (eq action 'preview) cand)
                (setq isearch-string cand)
                (isearch-update-from-string-properties cand)
                (isearch-update)))
            :narrow
            (list :predicate
                  (lambda (cand) (= (consult--tofu-get cand) consult--narrow))
                  :keys consult--isearch-history-narrow))
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
       (ash (if (local-variable-if-set-p sym) ?l ?g) 8)
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
                      (let (message-log-max)
                        (setq lighter (string-trim (format-mode-line lighter)))
                        (unless (string-blank-p lighter)
                          (cons lighter sym)))))
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
                  (= (ash narrow -8) consult--narrow))))
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
    (let* ((regexp (consult--regexp-filter
                    (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                            consult-themes)))
           (avail-themes (seq-filter
                          (lambda (x) (string-match-p regexp (symbol-name x)))
                          (cons 'default (custom-available-themes))))
           (saved-theme (car custom-enabled-themes)))
      (consult--read
       (mapcar #'symbol-name avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (selected &rest _)
                 (setq selected (and selected (intern-soft selected)))
                 (or (and selected (car (memq selected avail-themes)))
                     saved-theme))
       :state (lambda (action theme)
                (pcase action
                  ('return (consult-theme (or theme saved-theme)))
                  ((and 'preview (guard theme)) (consult-theme theme))))
       :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;;;;; Command: consult-buffer

(defun consult--buffer-sort-alpha (buffers)
  "Sort BUFFERS alphabetically, put starred buffers at the end."
  (sort buffers
        (lambda (x y)
          (setq x (buffer-name x) y (buffer-name y))
          (let ((a (and (length> x 0) (eq (aref x 0) ?*)))
                (b (and (length> y 0) (eq (aref y 0) ?*))))
            (if (eq a b)
                (string< x y)
              (not a))))))

(defun consult--buffer-sort-alpha-current (buffers)
  "Sort BUFFERS alphabetically, put current at the beginning."
  (let ((buffers (consult--buffer-sort-alpha buffers))
        (current (current-buffer)))
    (if (memq current buffers)
        (cons current (delq current buffers))
      buffers)))

(defun consult--buffer-sort-visibility (buffers)
  "Sort BUFFERS by visibility."
  (let ((hidden)
        (current (car (memq (current-buffer) buffers))))
    (consult--keep! buffers
      (unless (eq it current)
        (if (get-buffer-window it 'visible)
            it
          (push it hidden)
          nil)))
    (nconc (nreverse hidden) buffers (and current (list current)))))

(defun consult--normalize-directory (dir)
  "Normalize directory DIR.
DIR can be project, nil or a path."
  (cond
   ((eq dir 'project) (consult--project-root))
   (dir (expand-file-name dir))))

(defun consult--buffer-query-prompt (prompt query)
  "Return a list of buffers and create an appropriate prompt string.
Return a pair of a prompt string and a list of buffers.  PROMPT
is the prefix of the prompt string.  QUERY specifies the buffers
to search and is passed to `consult--buffer-query'."
  (let* ((dir (plist-get query :directory))
         (ndir (consult--normalize-directory dir))
         (buffers (apply #'consult--buffer-query :directory ndir query))
         (count (length buffers)))
    (cons (format "%s (%d buffer%s%s): " prompt count
                  (if (= count 1) "" "s")
                  (cond
                   ((and ndir (eq dir 'project))
                    (format ", Project %s" (consult--project-name ndir)))
                   (ndir (concat  ", " (consult--left-truncate-file ndir)))
                   (t "")))
          buffers)))

(cl-defun consult--buffer-query (&key sort directory mode as predicate (filter t)
                                      include (exclude consult-buffer-filter)
                                      (buffer-list t))
  "Query for a list of matching buffers.
The function supports filtering by various criteria which are
used throughout Consult.  In particular it is the backbone of
most `consult-buffer-sources'.
DIRECTORY can either be the symbol project or a file name.
SORT can be visibility, alpha or nil.
FILTER can be either t, nil or invert.
EXCLUDE is a list of regexps.
INCLUDE is a list of regexps.
MODE can be a mode or a list of modes to restrict the returned buffers.
PREDICATE is a predicate function.
BUFFER-LIST is the unfiltered list of buffers.
AS is a conversion function."
  (let ((root (consult--normalize-directory directory)))
    (setq buffer-list (if (eq buffer-list t) (buffer-list) (copy-sequence buffer-list)))
    (when sort
      (setq buffer-list (funcall (intern (format "consult--buffer-sort-%s" sort)) buffer-list)))
    (when (or filter mode as root)
      (let ((exclude-re (consult--regexp-filter exclude))
            (include-re (consult--regexp-filter include))
            (case-fold-search))
        (consult--keep! buffer-list
          (and
           (or (not mode)
               (let ((mm (buffer-local-value 'major-mode it)))
                 (if (consp mode)
                     (seq-some (lambda (m) (provided-mode-derived-p mm m)) mode)
                   (provided-mode-derived-p mm mode))))
           (pcase-exhaustive filter
             ('nil t)
             ((or 't 'invert)
              (eq (eq filter t)
                  (and
                   (or (not exclude)
                       (not (string-match-p exclude-re (buffer-name it))))
                   (or (not include)
                       (not (not (string-match-p include-re (buffer-name it)))))))))
           (or (not root)
               (when-let (dir (buffer-local-value 'default-directory it))
                 (string-prefix-p root
                                  (if (and (/= 0 (length dir)) (eq (aref dir 0) ?/))
                                      dir
                                    (expand-file-name dir)))))
           (or (not predicate) (funcall predicate it))
           (if as (funcall as it) it)))))
    buffer-list))

(defun consult--buffer-file-hash ()
  "Return hash table of all buffer file names."
  (consult--string-hash (consult--buffer-query :as #'buffer-file-name)))

(defun consult--buffer-pair (buffer)
  "Return a pair of name of BUFFER and BUFFER."
  (cons (buffer-name buffer) buffer))

(defun consult--buffer-preview ()
  "Buffer preview function."
  (let ((orig-buf (window-buffer (consult--original-window)))
        (orig-prev (copy-sequence (window-prev-buffers)))
        (orig-next (copy-sequence (window-next-buffers)))
        other-win)
    (lambda (action cand)
      (pcase action
        ('exit
         (set-window-prev-buffers other-win orig-prev)
         (set-window-next-buffers other-win orig-next))
        ('preview
         (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                    (not other-win))
           (switch-to-buffer-other-window orig-buf 'norecord)
           (setq other-win (selected-window)))
         (let ((win (or other-win (selected-window)))
               (buf (or (and cand (get-buffer cand)) orig-buf)))
           (when (and (window-live-p win) (buffer-live-p buf)
                      (not (buffer-match-p consult-preview-excluded-buffers buf)))
             (with-selected-window win
               (unless (or orig-prev orig-next)
                 (setq orig-prev (copy-sequence (window-prev-buffers))
                       orig-next (copy-sequence (window-next-buffers))))
               (switch-to-buffer buf 'norecord)))))))))

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
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :enabled  ,(lambda () consult-project-function)
    :items
    ,(lambda ()
       (when-let (root (consult--project-root))
         (consult--buffer-query :sort 'visibility
                                :directory root
                                :as #'consult--buffer-pair))))
  "Project buffer candidate source for `consult-buffer'.")

(defvar consult--source-project-recent-file
  `(:name     "Project File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new
    ,(lambda (file)
       (consult--file-action
        (expand-file-name file (consult--project-root))))
    :enabled
    ,(lambda ()
       (and consult-project-function
            recentf-mode))
    :items
    ,(lambda ()
       (when-let (root (consult--project-root))
         (let ((len (length root))
               (ht (consult--buffer-file-hash))
               items)
           (dolist (file (bound-and-true-p recentf-list) (nreverse items))
             ;; Emacs 29 abbreviates file paths by default, see
             ;; `recentf-filename-handlers'.  I recommend to set
             ;; `recentf-filename-handlers' to nil to avoid any slow down.
             (unless (eq (aref file 0) ?/)
               (let (file-name-handler-alist) ;; No Tramp slowdown please.
                 (setq file (expand-file-name file))))
             (when (and (not (gethash file ht)) (string-prefix-p root file))
               (let ((part (substring file len)))
                 (when (equal part "") (setq part "./"))
                 (put-text-property 0 1 'multi-category `(file . ,file) part)
                 (push part items))))))))
  "Project file candidate source for `consult-buffer'.")

(defvar consult--source-project-buffer-hidden
  `(:hidden t :narrow (?p . "Project") ,@consult--source-project-buffer)
  "Like `consult--source-project-buffer' but hidden by default.")

(defvar consult--source-project-recent-file-hidden
  `(:hidden t :narrow (?p . "Project") ,@consult--source-project-recent-file)
  "Like `consult--source-project-recent-file' but hidden by default.")

(defvar consult--source-hidden-buffer
  `(:name     "Hidden Buffer"
    :narrow   ?\s
    :hidden   t
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :action   ,#'consult--buffer-action
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :filter 'invert
                                       :as #'consult--buffer-pair)))
  "Hidden buffer candidate source for `consult-buffer'.")

(defvar consult--source-modified-buffer
  `(:name     "Modified Buffer"
    :narrow   ?*
    :hidden   t
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :as #'consult--buffer-pair
                                       :predicate
                                       (lambda (buf)
                                         (and (buffer-modified-p buf)
                                              (buffer-file-name buf))))))
  "Modified buffer candidate source for `consult-buffer'.")

(defvar consult--source-buffer
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :as #'consult--buffer-pair)))
  "Buffer candidate source for `consult-buffer'.")

(defun consult--file-register-p (reg)
  "Return non-nil if REG is a file register."
  (memq (car-safe (cdr reg)) '(file-query file)))

(autoload 'consult-register--candidates "consult-register")
(defvar consult--source-file-register
  `(:name     "File Register"
    :narrow   (?r . "Register")
    :category file
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (seq-some #'consult--file-register-p register-alist))
    :items    ,(lambda () (consult-register--candidates #'consult--file-register-p)))
  "File register source.")

(defvar consult--source-recent-file
  `(:name     "File"
    :narrow   ?f
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new      ,#'consult--file-action
    :enabled  ,(lambda () recentf-mode)
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash))
             items)
         (dolist (file (bound-and-true-p recentf-list) (nreverse items))
           ;; Emacs 29 abbreviates file paths by default, see
           ;; `recentf-filename-handlers'.  I recommend to set
           ;; `recentf-filename-handlers' to nil to avoid any slow down.
           (unless (eq (aref file 0) ?/)
             (let (file-name-handler-alist) ;; No Tramp slowdown please.
               (setq file (expand-file-name file))))
           (unless (gethash file ht)
             (push (consult--fast-abbreviate-file-name file) items))))))
  "Recent file candidate source for `consult-buffer'.")

;;;###autoload
(defun consult-buffer (&optional sources)
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources."
  (interactive)
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Switch to: "
                                  :history 'consult--buffer-history
                                  :sort nil)))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

(defmacro consult--with-project (&rest body)
  "Ensure that BODY is executed with a project root."
  ;; We have to work quite hard here to ensure that the project root is
  ;; only overridden at the current recursion level.  When entering a
  ;; recursive minibuffer session, we should be able to still switch the
  ;; project.  But who does that? Working on the first level on project A
  ;; and on the second level on project B and on the third level on project C?
  ;; You mustn't be afraid to dream a little bigger, darling.
  `(let ((consult-project-function
          (let ((root (or (consult--project-root t) (user-error "No project found")))
                (depth (recursion-depth))
                (orig consult-project-function))
            (lambda (may-prompt)
              (if (= depth (recursion-depth))
                  root
                (funcall orig may-prompt))))))
     ,@body))

;;;###autoload
(defun consult-project-buffer ()
  "Enhanced `project-switch-to-buffer' command with support for virtual buffers.
The command may prompt you for a project directory if it is invoked from
outside a project.  See `consult-buffer' for more details."
  (interactive)
  (consult--with-project
   (consult-buffer consult-project-buffer-sources)))

;;;###autoload
(defun consult-buffer-other-window ()
  "Variant of `consult-buffer', switching to a buffer in another window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-buffer)))

;;;###autoload
(defun consult-buffer-other-frame ()
  "Variant of `consult-buffer', switching to a buffer in another frame."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-frame))
    (consult-buffer)))

;;;###autoload
(defun consult-buffer-other-tab ()
  "Variant of `consult-buffer', switching to a buffer in another tab."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

;;;;; Command: consult-grep

(defun consult--grep-format (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (highlight)
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (cdr (funcall builder action)))
        (funcall async action))
       ((consp action)
        (let ((file "") (file-len 0) result)
          (save-match-data
            (dolist (str action)
              (when (and (string-match consult--grep-match-regexp str)
                         ;; Filter out empty context lines
                         (or (/= (aref str (match-beginning 3)) ?-)
                             (/= (match-end 0) (length str))))
                ;; We share the file name across candidates to reduce
                ;; the amount of allocated memory.
                (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                             (eq t (compare-strings
                                    file 0 file-len
                                    str (match-beginning 1) (match-end 1) nil)))
                  (setq file (match-string 1 str)
                        file-len (length file)))
                (let* ((line (match-string 2 str))
                       (ctx (= (aref str (match-beginning 3)) ?-))
                       (sep (if ctx "-" ":"))
                       (content (substring str (match-end 0)))
                       (line-len (length line)))
                  (when (length> content consult-grep-max-columns)
                    (setq content (substring content 0 consult-grep-max-columns)))
                  (when highlight
                    (funcall highlight content))
                  (setq str (concat file sep line sep content))
                  ;; Store file name in order to avoid allocations in `consult--prefix-group'
                  (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                  (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (when ctx
                    (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push str result)))))
          (funcall async (nreverse result))))
       (t (funcall async action))))))

(defun consult--grep-position (cand &optional find-file)
  "Return the grep position marker for CAND.
FIND-FILE is the file open function, defaulting to `find-file-noselect'."
  (when cand
    (let* ((file-end (next-single-property-change 0 'face cand))
           (line-end (next-single-property-change (1+ file-end) 'face cand))
           (matches (consult--point-placement cand (1+ line-end) 'consult-grep-context))
           (file (substring-no-properties cand 0 file-end))
           (line (string-to-number (substring-no-properties cand (+ 1 file-end) line-end))))
      (when-let (pos (consult--marker-from-line-column
                      (funcall (or find-file #'consult--file-action) file)
                      line (or (car matches) 0)))
        (cons pos (cdr matches))))))

(defun consult--grep-state ()
  "Grep state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall jump action (consult--grep-position
                            cand
                            (and (not (eq action 'return)) open))))))

(defun consult--grep-exclude-args ()
  "Produce grep exclude arguments.
Take the variables `grep-find-ignored-directories' and
`grep-find-ignored-files' into account."
  (unless (boundp 'grep-find-ignored-files) (require 'grep))
  (nconc (mapcar (lambda (s) (concat "--exclude=" s))
                 (bound-and-true-p grep-find-ignored-files))
         (mapcar (lambda (s) (concat "--exclude-dir=" s))
                 (bound-and-true-p grep-find-ignored-directories))))

(defun consult--grep (prompt make-builder dir initial)
  "Run asynchronous grep.

MAKE-BUILDER is the function that returns the command line
builder function.  DIR is a directory or a list of file or
directories.  PROMPT is the prompt string.  INITIAL is initial
input."
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt prompt dir))
               (default-directory dir)
               (builder (funcall make-builder paths)))
    (consult--read
     (consult--async-command builder
       (consult--grep-format builder)
       :file-handler t) ;; allow tramp
     :prompt prompt
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial (consult--async-split-initial initial)
     :add-history (consult--async-split-thingatpt 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'consult--prefix-group
     :history '(:input consult--grep-history)
     :sort nil)))

(defun consult--grep-lookahead-p (&rest cmd)
  "Return t if grep CMD supports look-ahead."
  (eq 0 (process-file-shell-command
         (concat "echo xaxbx | "
                 (mapconcat #'shell-quote-argument `(,@cmd "^(?=.*b)(?=.*a)") " ")))))

(defun consult--grep-make-builder (paths)
  "Build grep command line and grep across PATHS."
  (let* ((cmd (consult--build-args consult-grep-args))
         (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case (or (member "-i" flags) (member "--ignore-case" flags))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "-e" arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
            (when re
              (cons (append cmd
                            (list (if (eq type 'pcre) "-P" "-E") ;; perl or extended
                                  "-e" (consult--join-regexps re type))
                            opts paths)
                    hl))))))))

;;;###autoload
(defun consult-grep (&optional dir initial)
  "Search with `grep' for files in DIR where the content matches a regexp.

The initial input is given by the INITIAL argument.  DIR can be
nil, a directory string or a list of file/directory paths.  If
`consult-grep' is called interactively with a prefix argument,
the user can specify the directories or files to search in.
Multiple directories must be separated by comma in the
minibuffer, since they are read via `completing-read-multiple'.
By default the project directory is used if
`consult-project-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

The input string is split, the first part of the string (grep
input) is passed to the asynchronous grep process and the second
part of the string is passed to the completion-style filtering.

The input string is split at a punctuation character, which is
given as the first character of the input string.  The format is
similar to Perl-style regular expressions, e.g., /regexp/.
Furthermore command line options can be passed to grep, specified
behind --.  The overall prompt input has the form
`#async-input -- grep-opts#filter-string'.

Note that the grep input string is transformed from Emacs regular
expressions to Posix regular expressions.  Always enter Emacs
regular expressions at the prompt.  `consult-grep' behaves like
builtin Emacs search commands, e.g., Isearch, which take Emacs
regular expressions.  Furthermore the asynchronous input split
into words, each word must match separately and in any order.
See `consult--regexp-compiler' for the inner workings.  In order
to disable transformations of the grep input, adjust
`consult--regexp-compiler' accordingly.

Here we give a few example inputs:

#alpha beta         : Search for alpha and beta in any order.
#alpha.*beta        : Search for alpha before beta.
#\\(alpha\\|beta\\) : Search for alpha or beta (Note Emacs syntax!)
#word -- -C3        : Search for word, include 3 lines as context
#first#second       : Search for first, quick filter for second.

The symbol at point is added to the future history."
  (interactive "P")
  (consult--grep "Grep" #'consult--grep-make-builder dir initial))

;;;;; Command: consult-git-grep

(defun consult--git-grep-make-builder (paths)
  "Create grep command line builder given PATHS."
  (let ((cmd (consult--build-args consult-git-grep-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case (or (member "-i" flags) (member "--ignore-case" flags))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "-e" arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended ignore-case)))
            (when re
              (cons (append cmd
                            (cdr (mapcan (lambda (x) (list "--and" "-e" x)) re))
                            opts paths)
                    hl))))))))

;;;###autoload
(defun consult-git-grep (&optional dir initial)
  "Search with `git grep' for files in DIR with INITIAL input.
See `consult-grep' for details."
  (interactive "P")
  (consult--grep "Git-grep" #'consult--git-grep-make-builder dir initial))

;;;;; Command: consult-ripgrep

(defun consult--ripgrep-make-builder (paths)
  "Create ripgrep command line builder given PATHS."
  (let* ((cmd (consult--build-args consult-ripgrep-args))
         (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case
                    (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                         (or (member "-i" flags) (member "--ignore-case" flags)
                             (and (or (member "-S" flags) (member "--smart-case" flags))
                                  (let (case-fold-search)
                                    ;; Case insensitive if there are no uppercase letters
                                    (not (string-match-p "[[:upper:]]" arg))))))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "-e" arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
            (when re
              (cons (append cmd (and (eq type 'pcre) '("-P"))
                            (list "-e" (consult--join-regexps re type))
                            opts paths)
                    hl))))))))

;;;###autoload
(defun consult-ripgrep (&optional dir initial)
  "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details."
  (interactive "P")
  (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir initial))

;;;;; Command: consult-find

(defun consult--find (prompt builder initial)
  "Run find command in current directory.

The function returns the selected file.
The filename at point is added to the future history.

BUILDER is the command line builder function.
PROMPT is the prompt.
INITIAL is initial input."
  (consult--read
   (consult--async-command builder
     (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
     (consult--async-highlight builder)
     :file-handler t) ;; allow tramp
   :prompt prompt
   :sort nil
   :require-match t
   :initial (consult--async-split-initial initial)
   :add-history (consult--async-split-thingatpt 'filename)
   :category 'file
   :history '(:input consult--find-history)))

(defun consult--find-make-builder (paths)
  "Build find command line, finding across PATHS."
  (let* ((cmd (seq-mapcat (lambda (x)
                            (if (equal x ".") paths (list x)))
                          (consult--build-args consult-find-args)))
         (type (if (eq 0 (process-file-shell-command
                          (concat (car cmd) " -regextype emacs -version")))
                   'emacs 'basic)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   ;; ignore-case=t since -iregex is used below
                   (`(,re . ,hl) (funcall consult--regexp-compiler arg type t)))
        (when re
          (cons (append cmd
                        (cdr (mapcan
                              (lambda (x)
                                `("-and" "-iregex"
                                  ,(format ".*%s.*"
                                           ;; Replace non-capturing groups with capturing groups.
                                           ;; GNU find does not support non-capturing groups.
                                           (replace-regexp-in-string
                                            "\\\\(\\?:" "\\(" x 'fixedcase 'literal))))
                              re))
                        opts)
                hl))))))

;;;###autoload
(defun consult-find (&optional dir initial)
  "Search for files with `find' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Find" dir))
               (default-directory dir)
               (builder (consult--find-make-builder paths)))
    (find-file (consult--find prompt builder initial))))

;;;;; Command: consult-fd

(defun consult--fd-make-builder (paths)
  "Build find command line, finding across PATHS."
  (let ((cmd (consult--build-args consult-fd-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case
                    (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                         (or (member "-i" flags) (member "--ignore-case" flags)
                             (let (case-fold-search)
                               ;; Case insensitive if there are no uppercase letters
                               (not (string-match-p "[[:upper:]]" arg)))))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'pcre ignore-case)))
            (when re
              (cons (append cmd
                            (mapcan (lambda (x) `("--and" ,x)) re)
                            opts
                            (mapcan (lambda (x) `("--search-path" ,x)) paths))
                    hl))))))))

;;;###autoload
(defun consult-fd (&optional dir initial)
  "Search for files with `fd' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir)
               (builder (consult--fd-make-builder paths)))
    (find-file (consult--find prompt builder initial))))

;;;;; Command: consult-locate

(defun consult--locate-builder (input)
  "Build command line from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append (consult--build-args consult-locate-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

;;;###autoload
(defun consult-locate (&optional initial)
  "Search with `locate' for files which match input given INITIAL input.

The input is treated literally such that locate can take advantage of
the locate database index.  Regular expressions would often force a slow
linear search through the entire database.  The locate process is started
asynchronously, similar to `consult-grep'.  See `consult-grep' for more
details regarding the asynchronous search."
  (interactive)
  (find-file (consult--find "Locate: " #'consult--locate-builder initial)))

;;;;; Command: consult-man

(defun consult--man-builder (input)
  "Build command line from INPUT."
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
    (when re
      (cons (append (consult--build-args consult-man-args)
                    (list (consult--join-regexps re 'extended))
                    opts)
            hl))))

(defun consult--man-format (lines)
  "Format man candidates from LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
        (when (string-match "\\`\\(.*?\\([^ ]+\\) *(\\([^,)]+\\)[^)]*).*?\\) +- +\\(.*\\)\\'" str)
          (let* ((names (match-string 1 str))
                 (name (match-string 2 str))
                 (section (match-string 3 str))
                 (desc (match-string 4 str))
                 (cand (format "%s - %s" names desc)))
            (add-text-properties 0 (length names)
                                 (list 'face 'consult-file
                                       'consult-man (concat section " " name))
                                 cand)
            (push cand candidates)))))
    (nreverse candidates)))

;;;###autoload
(defun consult-man (&optional initial)
  "Search for man page given INITIAL input.

The input string is not preprocessed and passed literally to the
underlying man commands.  The man process is started asynchronously,
similar to `consult-grep'.  See `consult-grep' for more details regarding
the asynchronous search."
  (interactive)
  (man (consult--read
        (consult--async-command #'consult--man-builder
          (consult--async-transform consult--man-format)
          (consult--async-highlight #'consult--man-builder))
        :prompt "Manual entry: "
        :require-match t
        :category 'consult-man
        :lookup (apply-partially #'consult--lookup-prop 'consult-man)
        :initial (consult--async-split-initial initial)
        :add-history (consult--async-split-thingatpt 'symbol)
        :history '(:input consult--man-history))))

;;;; Preview at point in completions buffers

(define-minor-mode consult-preview-at-point-mode
  "Preview minor mode for *Completions* buffers.
When moving around in the *Completions* buffer, the candidate at point is
automatically previewed."
  :group 'consult
  (if consult-preview-at-point-mode
      (add-hook 'post-command-hook #'consult-preview-at-point nil 'local)
    (remove-hook 'post-command-hook #'consult-preview-at-point 'local)))

(defun consult-preview-at-point ()
  "Preview candidate at point in *Completions* buffer."
  (interactive)
  (when-let ((win (active-minibuffer-window))
             (buf (window-buffer win))
             (fun (buffer-local-value 'consult--preview-function buf)))
    (funcall fun)))

;;;; Integration with completion systems

;;;;; Integration: Default *Completions*

(defun consult--default-completion-minibuffer-candidate ()
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
      (or (get-text-property beg 'completion--string)
          (buffer-substring-no-properties beg end)))))

;;;;; Integration: Vertico

(defvar vertico--input)
(declare-function vertico--exhibit "ext:vertico")
(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--filter-completions "ext:vertico")

(defun consult--vertico-candidate ()
  "Return current candidate for Consult preview."
  (and vertico--input (vertico--candidate 'highlight)))

(defun consult--vertico-refresh ()
  "Refresh completion UI."
  (when vertico--input
    (setq vertico--input t)
    (vertico--exhibit)))

(defun consult--vertico-filter-adv (orig pattern cands category highlight)
  "Advice for ORIG `consult--completion-filter' function.
See `consult--completion-filter' for arguments PATTERN, CANDS, CATEGORY
and HIGHLIGHT."
  (if (and (not highlight) (bound-and-true-p vertico-mode))
      ;; Optimize `consult--completion-filter' using the deferred highlighting
      ;; from Vertico.  The advice is not necessary - it is a pure optimization.
      (nconc (car (vertico--filter-completions pattern cands nil (length pattern)
                                               `(metadata (category . ,category))))
             nil)
    (funcall orig pattern cands category highlight)))

(with-eval-after-load 'vertico
  (advice-add #'consult--completion-filter :around #'consult--vertico-filter-adv)
  (add-hook 'consult--completion-candidate-hook #'consult--vertico-candidate)
  (add-hook 'consult--completion-refresh-hook #'consult--vertico-refresh)
  (define-key consult-async-map [remap vertico-insert] 'vertico-next-group))

;;;;; Integration: Mct

(with-eval-after-load 'mct (add-hook 'consult--completion-refresh-hook
                                     'mct--live-completions-refresh))

;;;;; Integration: Icomplete

(defvar icomplete-mode)
(declare-function icomplete-exhibit "icomplete")

(defun consult--icomplete-refresh ()
  "Refresh icomplete view."
  (when icomplete-mode
    (let ((top (car completion-all-sorted-completions)))
      (completion--flush-all-sorted-completions)
      ;; force flushing, otherwise narrowing is broken!
      (setq completion-all-sorted-completions nil)
      (when top
        (let* ((completions (completion-all-sorted-completions))
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

(with-eval-after-load 'icomplete
  (add-hook 'consult--completion-refresh-hook #'consult--icomplete-refresh))

(provide 'consult)
;;; consult.el ends here
