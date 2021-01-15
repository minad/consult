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
;; search and navigation commands like `consult-imenu' and
;; `consult-line'. Searching through multiple files is supported by
;; the powerful asynchronous `consult-grep' command. Many Consult
;; commands allow previewing candidates - if a candidate is selected
;; in the completion view, the buffer shows the candidate immediately.

;; The Consult commands are compatible with completion systems based
;; on the Emacs `completing-read' API, notably the default completion
;; system, Icomplete, Selectrum and Embark.

;; Consult has been inspired by Counsel. Some of the Consult commands
;; originated in the Selectrum wiki. See the README for a full list of
;; contributors.

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

;;;; Customization

(defcustom consult-narrow-key nil
  "Prefix key for narrowing during completion.

Good choices for this key are (kbd \"<\") or (kbd \"C-+\") for example.

The key must be either a string or a vector.
This is the key representation accepted by `define-key'."
  :type '(choice vector string (const nil)))

(defcustom consult-widen-key nil
  "Key used for widening during completion.

If this key is unset, defaults to 'consult-narrow-key SPC'.

The key must be either a string or a vector.
This is the key representation accepted by `define-key'."
  :type '(choice vector string (const nil)))

(defcustom consult-view-list-function nil
  "Function which returns a list of view names as strings, used by `consult-buffer'."
  :type '(choice function (const nil)))

(defcustom consult-view-open-function nil
  "Function which opens a view, used by `consult-buffer'."
  :type '(choice function (const nil)))

(defcustom consult-project-root-function nil
  "Function which returns project root directory, used by `consult-buffer' and `consult-grep'."
  :type '(choice function (const nil)))

(defcustom consult-async-refresh-delay 0.25
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

(defcustom consult-async-input-debounce 0.25
  "Input debounce for asynchronous commands.

The asynchronous process is started only when there has not been new
input for `consult-async-input-debounce' seconds. This applies to
asynchronous commands, e.g., `consult-grep'."
  :type 'float)

(defcustom consult-async-min-input 3
  "Minimum number of letters needed, before asynchronous process is called.

This applies to asynchronous commands, e.g., `consult-grep'."
  :type 'integer)

(defcustom consult-async-default-split "#"
  "Default async input separator used for splitting.

Can also be nil in order to not automatically insert a separator. This
applies to asynchronous commands, e.g., `consult-grep'."
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

Commonly used functions for this hook are `recenter' and
`reposition-window'.

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
  "Show absolute line numbers when narrowing is active.

See also `display-line-numbers-widen'."
  :type 'boolean)

(defcustom consult-goto-line-numbers t
  "Show line numbers for `consult-goto-line'."
  :type 'boolean)

(defcustom consult-fontify-max-size 1048576
  "Buffers larger than this byte limit are not fontified.

This is necessary in order to prevent a large startup time
for navigation commands like `consult-line'."
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
  "Category of toplevel items, used by `consult-imenu'.

The imenu representation provided by the backend usually puts
functions directly at the toplevel. `consult-imenu' moves them instead
under the category specified by this variable."
  :type 'alist)

(defcustom consult-buffer-filter
  '("^ ")
  "Filter regexps for `consult-buffer'.

The default setting is to filter only ephemeral buffer names beginning
with a space character."
  :type '(repeat regexp))

(defcustom consult-mode-command-filter
  '("-mode$" "--")
  "Filter regexps for `consult-mode-command'."
  :type '(repeat regexp))

(defcustom consult-git-grep-command
  '("git" "--no-pager" "grep" "--null" "--color=always" "--extended-regexp" "--line-number" "-I" "-e")
  "Command line arguments for git-grep, see `consult-git-grep'."
  :type '(repeat string))

(defcustom consult-grep-command
  '("grep" "--null" "--line-buffered" "--color=always" "--extended-regexp" "--exclude-dir=.git" "--line-number" "-I" "-r" "." "-e")
  "Command line arguments for grep, see `consult-grep'."
  :type '(repeat string))

(defcustom consult-ripgrep-command
  '("rg" "--null" "--line-buffered" "--color=always" "--max-columns=500" "--no-heading" "--line-number" "." "-e")
  "Command line arguments for ripgrep, see `consult-ripgrep'."
  :type '(repeat string))

(defcustom consult-find-command
  '("find" "." "-not" "(" "-wholename" "*/.*" "-prune" ")" "-ipath")
  "Command line arguments for find, see `consult-find'."
  :type '(repeat string))

(defcustom consult-locate-command
  '("locate" "--ignore-case" "--existing" "--regexp")
  "Command line arguments for locate, see `consult-locate'."
  :type '(repeat string))

(defcustom consult-man-command
  '("man" "-k")
  "Command line arguments for man apropos, see `consult-man'."
  :type '(repeat string))

(defcustom consult-preview-key 'any
  "Preview trigger keys, can be nil, 'any, a single key or a list of keys."
  :type '(choice (const any) vector string (repeat (choice vector string))))

(defcustom consult-preview-max-size 10485760
  "Files larger than this byte limit are not previewed."
  :type 'integer)

(defcustom consult-preview-max-count 10
  "Number of files to keep open at once during preview."
  :type 'integer)

(defcustom consult-register-narrow
  `((?n "Number" ,#'numberp)
    (?s "String" ,#'stringp)
    (?r "Rectangle" ,(lambda (x) (stringp (car-safe x))))
    ;; (?f "Frameset" ,#'frameset-register-p) ;; only 27.1
    (?p "Position" ,(lambda (x)
                      (or (markerp x) (eq (car-safe x) 'file-query))))
    (?w "Window" ,(lambda (x) (window-configuration-p (car-safe x)))))
  "Register narrowing configuration.

Each element of the list must have the form '(char name predicate)."
  :type 'list)

(defcustom consult-bookmark-narrow
  `((?f "File" #'bookmark-default-handler)
    (?h "Help" #'help-bookmark-jump)
    (?i "Info" #'Info-bookmark-jump)
    (?p "Picture" #'image-bookmark-jump)
    (?d "Docview" #'doc-view-bookmark-jump)
    (?m "Man" #'Man-bookmark-jump)
    (?w "Woman" #'woman-bookmark-jump)
    (?g "Gnus" #'gnus-summary-bookmark-jump))
  "Bookmark narrowing configuration.

Each element of the list must have the form '(char name handler)."
  :type 'list)

(defcustom consult-config nil
  "Command configuration alists, which allows fine-grained configuration.

The options set here will be passed to `consult--read', when called
from the corresponding command. Note that the options depend on the
private `consult--read' API and should not be considered as stable as
the public API."
  :type '(list (cons symbol plist)))

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
(defvar consult--man-history nil)
(defvar consult--line-history nil)
(defvar consult--apropos-history nil)
(defvar consult--theme-history nil)
(defvar consult--minor-mode-menu-history nil)
(defvar consult--mode-command-history nil)
(defvar consult--kmacro-history nil)
(defvar consult--buffer-history nil)
(defvar consult--imenu-history nil)

;;;; Internal variables

(defvar consult--completion-match-hook
  (list #'consult--default-completion-match)
  "Obtain match function from completion system.")

(defvar consult--completion-candidate-hook
  (list #'consult--default-completion-candidate)
  "Get candidate from completion system.")

(defvar consult--completion-refresh-hook nil
  "Refresh completion system.")

(defvar-local consult--preview-function nil
  "Minibuffer-local variable which exposes the current preview function.
This function can be called by custom completion systems from outside the minibuffer.
The preview function expects two arguments, the current input string and the candidate string.")

(defconst consult--tofu-char #x100000
  "Special character used to encode line prefixes for disambiguation.
We use the first character of the private unicode plane b.")

(defconst consult--tofu-range #xFFFE
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

(defvar-local consult--imenu-cache nil
  "Buffer local cached imenu.")

(defconst consult--grep-regexp "\\([^\0\n]+\\)\0\\([^:\0]+\\)[:\0]"
  "Regexp used to match file and line of grep output.")

(defconst consult--grep-match-regexp "\e\\[[0-9;]+m\\(.*?\\)\e\\[[0-9;]*m"
  "Regexp used to find matches in grep output.")

;;;; Helper functions and macros

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
  (string-join (mapcar (lambda (x) (concat "\\(?:" x "\\)")) regexps) "\\|"))

(defun consult--font-lock (str)
  "Apply `font-lock' faces in STR, copy them to `face'."
  (let ((pos 0) (len (length str)))
    (while (< pos len)
      (let* ((face (get-text-property pos 'font-lock-face str))
             (end (or (text-property-not-all pos len 'font-lock-face face str) len)))
        (put-text-property pos end 'face face str)
        (setq pos end)))
    str))

(defun consult--format-directory-prompt (prompt dir)
  "Format PROMPT, expand directory DIR and return them as a pair."
  (save-match-data
    (let ((edir (file-name-as-directory (expand-file-name dir)))
          (ddir (file-name-as-directory (expand-file-name default-directory))))
      (cons
       (if (string= ddir edir)
           (concat prompt ": ")
         (let ((adir (abbreviate-file-name edir)))
           (if (string-match "/\\([^/]+\\)/\\([^/]+\\)/$" adir)
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
   ((when-let (root (and consult-project-root-function
                         (funcall consult-project-root-function)))
      (save-match-data
        (if (string-match "/\\([^/]+\\)/$" root)
            (cons (format "%s in project %s: " prompt (match-string 1 root)) root)
          (consult--format-directory-prompt prompt root)))))
   (t (consult--format-directory-prompt prompt default-directory))))

(defsubst consult--strip-ansi-escape (str)
  "Strip ANSI escape sequences from STR."
  (replace-regexp-in-string "\e\\[[0-9;]*[mK]" "" str))

(defsubst consult--format-location (file line)
  "Format location string 'FILE:LINE:'."
  (concat
   (propertize file 'face 'consult-file) ":"
   (propertize (number-to-string line) 'face 'consult-line-number) ":"))

(defun consult--line-position (line)
  "Compute character position from LINE number."
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
  (<= (point-min) pos (point-max)))

(defun consult--lookup-elem (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return element."
  (assoc cand candidates))

(defun consult--lookup-cdr (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return cdr of element."
  (cdr (assoc cand candidates)))

(defun consult--lookup-cadr (_ candidates cand)
  "Lookup CAND in CANDIDATES alist, return cadr of element."
  (cadr (assoc cand candidates)))

(defun consult--lookup-location (_ candidates cand)
  "Lookup CAND in CANDIDATES list of 'consult-location category, return the marker."
  (when-let (found (seq-find (lambda (x) (string= x cand)) candidates))
    (car (get-text-property 0 'consult-location found))))

(defun consult--forbid-minibuffer ()
  "Raise an error if executed from the minibuffer."
  (when (minibufferp)
    (user-error "Consult called inside the minibuffer")))

(defun consult--fontify-all ()
  "Ensure that the whole buffer is fontified."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line. Therefore we have to enforce
  ;; font-locking now, which is slow. In order to prevent is hang-up we check the buffer size
  ;; against `consult-fontify-max-size'.
  (when (and jit-lock-mode (< (buffer-size) consult-fontify-max-size))
    (jit-lock-fontify-now)))

(defsubst consult--fontify-region (start end)
  "Ensure that region between START and END is fontified."
  (when jit-lock-mode
    (jit-lock-fontify-now start end)))

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

(defmacro consult--with-increased-gc (&rest body)
  "Temporarily increase the gc limit in BODY to optimize for throughput."
  `(let* ((overwrite (> consult--gc-threshold gc-cons-threshold))
          (gc-cons-threshold (if overwrite consult--gc-threshold gc-cons-threshold))
          (gc-cons-percentage (if overwrite consult--gc-percentage gc-cons-percentage)))
     ,@body))

(defun consult--count-lines (pos)
  "Move to position POS and return number of lines."
  (let ((line 0))
    (while (< (point) pos)
      (forward-line 1)
      (when (<= (point) pos)
        (setq line (1+ line))))
    (goto-char pos)
    line))

;; We must disambiguate the lines by adding a prefix such that two lines with the same text can be
;; distinguished. In order to avoid matching the line number, such that the user can search for
;; numbers with `consult-line', we encode the line number as unicode characters in the supplementary
;; private use plane b. By doing that, it is unlikely that accidential matching occurs.
(defsubst consult--encode-location (marker)
  "Generate unique string for MARKER.
DISPLAY is the string to display instead of the unique string."
  (let ((str "") (n marker))
    (while (progn
             (setq str (concat str
                               (char-to-string (+ consult--tofu-char (% n consult--tofu-range)))))
             (and (>= n consult--tofu-range) (setq n (/ n consult--tofu-range)))))
    str))

(defsubst consult--line-number-prefix (marker line width)
  "Format LINE number prefix number with padding.

MARKER and LINE are added as 'consult-location text property.
WIDTH is the line number width."
  (let* ((unique-str (consult--encode-location marker))
         (line-str (number-to-string line))
         (prefix-str (concat
                      (make-string (- width (length line-str)) 32)
                      line-str
                      " ")))
    (put-text-property 0 (length prefix-str) 'face 'consult-line-number-prefix prefix-str)
    (add-text-properties 0 (length unique-str)
                         `(display ,prefix-str consult-location (,marker . ,line))
                         unique-str)
    unique-str))

(defun consult--add-line-number (max-line candidates)
  "Add line numbers to unformatted CANDIDATES as prefix.
The MAX-LINE is needed to determine the width.
Since the line number is part of the candidate it will be matched-on during completion."
  (let ((width (length (number-to-string max-line))))
    (mapcar (pcase-lambda (`(,marker ,line ,str))
              (concat
               (consult--line-number-prefix marker line width)
               str))
            candidates)))

(defsubst consult--region-with-cursor (begin end marker)
  "Return region string with a marking at the cursor position.

BEGIN is the begin position.
END is the end position.
MARKER is the cursor position."
  (let ((str (buffer-substring begin end)))
    (if (>= marker end)
        (concat str (propertize " " 'face 'consult-preview-cursor))
      (put-text-property (- marker begin) (- (1+ marker) begin) 'face 'consult-preview-cursor str)
      str)))

(defsubst consult--line-with-cursor (marker)
  "Return current line where the cursor MARKER is highlighted."
  (consult--region-with-cursor
   (line-beginning-position)
   (line-end-position)
   marker))

;;;; Preview support

(defun consult--kill-clean-buffer (buf)
  "Kill BUF if it has not been modified."
  (unless (or (eq buf (current-buffer)) (buffer-modified-p buf))
    (kill-buffer buf)))

(defun consult--with-file-preview-1 (fun)
  "Provide a function to open files temporarily.
The files are closed automatically in the end.

FUN receives the open function as argument."
  (let* ((new-buffers)
         (recentf-should-restore recentf-mode)
         (recentf-saved-list (when recentf-should-restore (copy-sequence recentf-list)))
         (open-file
          (lambda (name)
            (or (get-file-buffer name)
                (when-let (attrs (file-attributes name))
                  (if (> (file-attribute-size attrs) consult-preview-max-size)
                      (and (minibuffer-message "File `%s' too large for preview" name) nil)
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
      (when recentf-should-restore
        (setq recentf-list recentf-saved-list)
        (when (member (current-buffer) new-buffers)
          (recentf-add-file (buffer-file-name (current-buffer))))))))

(defmacro consult--with-file-preview (args &rest body)
  "Provide a function to open files temporarily.
The files are closed automatically in the end.

ARGS is the open function argument for BODY."
  (declare (indent 1))
  `(consult--with-file-preview-1 (lambda ,args ,@body)))

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
          (if-let (func (overlay-get ov 'isearch-open-invisible-temporary))
              (funcall func nil)
            (overlay-put ov 'invisible nil)))))))

;; Derived from ctrlf, originally isearch
(defun consult--invisible-restore (overlays)
  "Restore any opened OVERLAYS that were previously disabled."
  (dolist (ov overlays)
    (if-let (func (overlay-get (car ov) 'isearch-open-invisible-temporary))
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
        (saved-pos (point-marker)))
    (lambda (cand restore)
      (consult--invisible-restore invisible)
      (mapc #'delete-overlay overlays)
      (cond
       ;; Do nothing when restoring
       (restore)
       ;; Jump to position
       (cand
        (consult--jump-1 cand)
        (setq invisible (consult--invisible-show))
        (let ((pos (point)))
          (setq overlays
                (list (consult--overlay (line-beginning-position) (line-end-position) 'face 'consult-preview-line)
                      (consult--overlay pos (1+ pos) 'face face)))))
       ;; If position cannot be previewed, return to saved position
       (t (consult--jump-1 saved-pos))))))

(defun consult--with-preview-1 (preview-key preview transform fun)
  "Install TRANSFORM and PREVIEW function for FUN.

PREVIEW-KEY are the keys which trigger the preview."
  (let ((input ""))
    (if (and preview preview-key)
        (minibuffer-with-setup-hook
            (lambda ()
              (setq consult--preview-function
                    (lambda (inp cand)
                      (unless (window-minibuffer-p)
                        (error "Minibuffer window is not selected in consult--preview-function"))
                      (with-selected-window (or (minibuffer-selected-window) (next-window))
                        (funcall preview (funcall transform inp cand) nil))))
              (add-hook 'post-command-hook
                        (lambda ()
                          (setq input (minibuffer-contents-no-properties))
                          (when (or (eq preview-key 'any)
                                    (let ((keys (this-single-command-keys)))
                                      (seq-find (lambda (x) (equal (vconcat x) keys))
                                                (if (listp preview-key) preview-key (list preview-key)))))
                            (when-let (cand (run-hook-with-args-until-success 'consult--completion-candidate-hook))
                              (funcall consult--preview-function input cand))))
                        nil t))
          (let ((selected))
            (unwind-protect
                (save-excursion
                  (save-restriction
                    (setq selected (when-let (result (funcall fun))
                                     (funcall transform input result)))
                    (cons selected input)))
              (funcall preview selected t))))
      (minibuffer-with-setup-hook
          (apply-partially
           #'add-hook 'post-command-hook
           (lambda () (setq input (minibuffer-contents-no-properties)))
           nil t)
        (cons (when-let (result (funcall fun))
                (funcall transform input result))
              input)))))

(defmacro consult--with-preview (preview-key preview transform &rest body)
  "Install TRANSFORM and PREVIEW in BODY.

PREVIEW-KEY are the keys which triggers the preview."
  (declare (indent 3))
  `(consult--with-preview-1 ,preview-key ,preview ,transform (lambda () ,@body)))

;;;; Narrowing support

(defun consult--widen-key ()
  "Return widening key, if `consult-widen-key' is not set, default to 'consult-narrow-key SPC'."
  (or consult-widen-key (and consult-narrow-key (vconcat consult-narrow-key " "))))

(defun consult-narrow (key)
  "Narrow current completion with KEY.

This command is used internally by the narrowing system of `consult--read'."
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
         (when-let (pair (or (and (= 1 (length str)) (assoc (aref str 0) consult--narrow-prefixes))
                             (and (string= str "") (assoc 32 consult--narrow-prefixes))))
           (delete-minibuffer-contents)
           (consult-narrow (car pair))
           #'ignore)))))

(defun consult-narrow-help ()
  "Print narrowing help as a `minibuffer-message'.

This command can be bound to a key in `consult-narrow-map',
to make it available for commands with narrowing."
  (interactive)
  (unless (minibufferp)
    (user-error "Narrow help must be called in the minibuffer"))
  (let ((minibuffer-message-timeout 1000000))
    (minibuffer-message
     (string-join
      (thread-last consult--narrow-prefixes
        (seq-filter (lambda (x) (/= (car x) 32)))
        (mapcar (lambda (x) (concat
                             (propertize (char-to-string (car x)) 'face 'consult-key)
                             " " (cdr x)))))
      " "))))

(defun consult--narrow-setup (settings map)
  "Setup narrowing with SETTINGS and keymap MAP."
  (if (functionp (car settings))
      (setq consult--narrow-predicate (car settings)
            consult--narrow-prefixes (cdr settings))
    (setq consult--narrow-predicate nil
          consult--narrow-prefixes settings))
  (when consult-narrow-key
    (dolist (pair consult--narrow-prefixes)
      (when (/= (car pair) 32)
        (consult--define-key map
                             (vconcat consult-narrow-key (vector (car pair)))
                             #'consult-narrow (cdr pair)))))
  (when-let (widen (consult--widen-key))
    (consult--define-key map widen #'consult-narrow "All")))

;;;; Async support

(defun consult--with-async-1 (async fun)
  "Setup ASYNC for FUN."
  (if (not (functionp async)) (funcall fun (lambda (_) async))
    (minibuffer-with-setup-hook
        (lambda ()
          (funcall async 'setup)
          ;; push input string to request refresh
          (let ((push (lambda (&rest _) (funcall async (minibuffer-contents-no-properties)))))
            (run-at-time 0 nil push)
            (add-hook 'after-change-functions push nil t)))
      (unwind-protect
          (funcall fun async)
        (funcall async 'destroy)))))

(defmacro consult--with-async (async &rest body)
  "Setup ASYNC for BODY."
  (declare (indent 1))
  `(consult--with-async-1 ,@(cdr async) (lambda (,(car async)) ,@body)))

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

(defmacro consult--async-split-wrap (suffix)
  "Create completion style function with name SUFFIX."
  (let ((name (intern (format "consult--async-split-%s" suffix))))
    `(progn
       (defun ,name (str table pred point &optional metadata)
         (let ((completion-styles (cdr completion-styles))
               (pos (cdr (consult--async-split-string str))))
           (,(intern (format "completion-%s" suffix))
            (substring str pos) table pred (- point pos) metadata)))
       ',name)))

(defun consult--async-split-setup ()
  "Setup `consult--async-split' completion styles."
  (setq-local completion-styles-alist (cons (list 'consult--async-split
                                                  (consult--async-split-wrap try-completion)
                                                  (consult--async-split-wrap all-completions) "")
                                            completion-styles-alist))
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
              (input-len (length action))
              (async-str (car pair))
              (async-len (length async-str))
              (end (minibuffer-prompt-end)))
         ;; Highlight punctuation characters
         (remove-list-of-text-properties end (+ end input-len) '(face))
         (when (> input-len async-len)
           (put-text-property end (1+ end) 'face 'consult-async-split)
           (when (> input-len (1+ async-len))
             (put-text-property (+ 1 end async-len) (+ 2 end async-len) 'face 'consult-async-split)))
         (funcall async
                  ;; Pass through if forced by two punctuation characters
                  ;; or if the input is long enough!
                  (if (or (>= input-len (+ 2 async-len))
                          (>= async-len consult-async-min-input))
                      async-str
                    ;; Pretend that there is no input
                    ""))))
      (_ (funcall async action)))))

(defun consult--async-process (async cmd)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
CMD is the command argument list."
  (let* ((rest) (proc) (flush) (last-args) (indicator))
    (lambda (action)
      (pcase action
        (""
         ;; If no input is provided kill current process
         (ignore-errors (delete-process proc))
         (setq last-args nil
               rest nil))
        ((pred stringp)
         (let ((args (funcall cmd action)))
           (unless (equal args last-args)
             (setq last-args args
                   rest nil)
             (ignore-errors (delete-process proc))
             (when args
               (overlay-put indicator 'display (propertize "*" 'face 'consult-async-running))
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
                   (overlay-put indicator 'display
                                (cond
                                 ((string-prefix-p "killed" event)
                                  (propertize ";" 'face 'consult-async-failed))
                                 ((string-prefix-p "finished" event)
                                  (propertize ":" 'face 'consult-async-finished))
                                 (t (propertize "!" 'face 'consult-async-failed))))
                   (when (string-prefix-p "finished" event)
                     (unless (string= rest "")
                       (funcall async (list rest)))))))))))
        ('destroy
         (ignore-errors (delete-process proc))
         (delete-overlay indicator)
         (funcall async 'destroy))
        ('setup
         (setq indicator (make-overlay (- (minibuffer-prompt-end) 2) (- (minibuffer-prompt-end) 1)))
         (funcall async 'setup))
        (_ (funcall async action))))))

(defun consult--async-throttle (async &optional throttle debounce)
  "Create async function from ASYNC which throttles input.

The THROTTLE delay defaults to `consult-async-input-throttle'.
The DEBOUNCE delay defaults to `consult-async-input-debounce'."
  (let* ((throttle (or throttle consult-async-input-throttle))
         (debounce (or debounce consult-async-input-debounce))
         (input "")
         (unlocked t)
         (throttle-timer)
         (debounce-timer))
    (lambda (action)
      (pcase action
        ('setup
         (funcall async 'setup)
         (setq throttle-timer (run-at-time throttle throttle (lambda () (setq unlocked t)))))
        ((pred stringp)
         (when debounce-timer
           (cancel-timer debounce-timer))
         (unless (string= action input)
           (funcall async (setq input "")) ;; cancel running process
           (unless (string= action "")
             (setq debounce-timer (run-at-time
                                   (+ debounce (if unlocked 0 throttle)) nil
                                   (lambda () (funcall async (setq unlocked nil input action))))))))
        ('destroy
         (cancel-timer throttle-timer)
         (when debounce-timer
           (cancel-timer debounce-timer))
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

The refresh happens after a DELAY, defaulting to `consult-async-refresh-delay'."
  (let ((timer) (refresh t) (delay (or delay consult-async-refresh-delay)))
    (lambda (action)
      (pcase action
        ((or (pred listp) (pred stringp) 'refresh 'flush)
         (setq refresh t))
        ('destroy (cancel-timer timer))
        ('setup
         (setq timer (run-at-time
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

(defun consult--command-args (cmd)
  "Split command arguments and append to CMD."
  (lambda (input)
    (let ((args (split-string input " +-- +")))
      (append cmd (list (car args)) (mapcan #'split-string (cdr args))))))

(defmacro consult--async-command (cmd &rest transforms)
  "Asynchronous CMD pipeline with TRANSFORMS."
  (declare (indent 1))
  `(thread-first (consult--async-sink)
     (consult--async-refresh-timer)
     ,@transforms
     (consult--async-process (consult--command-args ,cmd))
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

;;;; Main consult--read API

(defun consult--add-history (items)
  "Add ITEMS to the minibuffer history via `minibuffer-default-add-function'."
  (setq-local minibuffer-default-add-function
              (lambda ()
                (consult--remove-dups
                 (append
                  ;; the defaults are at the beginning of the future history
                  (if (listp minibuffer-default)
                      minibuffer-default
                    (list minibuffer-default))
                  ;; then our custom items
                  (delete "" (delq nil (if (listp items)
                                           items
                                         (list items))))
                  ;; then all the completions
                  (all-completions ""
                                   minibuffer-completion-table
                                   minibuffer-completion-predicate))))))

(defun consult--setup-keymap (async narrow preview-key)
  "Setup keymap for ASYNC, NARROW and PREVIEW-KEY."
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
    (when (and preview-key (not (eq preview-key 'any)))
      (let ((preview-key (if (listp preview-key) preview-key (list preview-key))))
        (dolist (key preview-key)
          (unless (lookup-key old-map key)
            (define-key map key #'ignore)))))

    ;; Put the keymap together
    (use-local-map
     (make-composed-keymap
      (append
       (when async (list consult-async-map))
       (when narrow (list consult-narrow-map))
       (when preview-key (list consult-preview-map))
       map)
      old-map))))

(defun consult--fry-the-tofus (&rest _)
  "Fry the tofus in the minibuffer."
  (let* ((min (minibuffer-prompt-end))
         (max (point-max))
         (pos min)
         (cmin consult--tofu-char)
         (cmax (- (+ consult--tofu-char consult--tofu-range) 1)))
    (while (and (< pos max) (<= cmin (char-after pos) cmax))
      (setq pos (1+ pos)))
    (when (> pos min)
      (remove-list-of-text-properties min pos '(display))
      (put-text-property min pos 'invisible t))))

(cl-defun consult--read-setup (_prompt candidates
                                       &key add-history narrow preview-key &allow-other-keys)
  "Minibuffer setup for `consult--read'.

See `consult--read' for the CANDIDATES, ADD-HISTORY, NARROW and PREVIEW-KEY arguments."
  (add-hook 'after-change-functions #'consult--fry-the-tofus nil t)
  (consult--setup-keymap (functionp candidates) narrow preview-key)
  (consult--add-history add-history))

(cl-defun consult--read (prompt candidates &rest options &key
                                predicate require-match history default
                                category initial narrow add-history
                                preview (preview-key consult-preview-key)
                                (sort t) (default-top t) (lookup (lambda (_input _cands x) x)))
  "Enhanced completing read function.

Arguments:

PROMPT is the string to prompt with.
CANDIDATES is the candidate list or alist.

Keyword OPTIONS:

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
PREVIEW-KEY are the preview keys (nil, 'any, a single key or a list of keys).
NARROW is an alist of narrowing prefix strings and description."
  (ignore default-top add-history narrow)
  ;; supported types
  (cl-assert (or (functionp candidates)     ;; async table
                 (not candidates)           ;; nil, empty list
                 (obarrayp candidates)      ;; obarray
                 (stringp (car candidates)) ;; string list
                 (symbolp (car candidates)) ;; symbol list
                 (consp (car candidates)))) ;; alist
  (minibuffer-with-setup-hook
      (:append (lambda () (apply #'consult--read-setup prompt candidates options)))
    (consult--with-async (async candidates)
      ;; NOTE: Do not unnecessarily let-bind the lambdas to avoid
      ;; overcapturing in the interpreter. This will make closures and the
      ;; lambda string representation larger, which makes debugging much worse.
      ;; Fortunately the overcapturing problem does not affect the bytecode
      ;; interpreter which does a proper scope analyis.
      (let ((result
             (consult--with-preview preview-key preview
                                    (lambda (input cand)
                                      (funcall lookup input (funcall async 'get) cand))
               (completing-read prompt
                                (lambda (str pred action)
                                  (if (eq action 'metadata)
                                      `(metadata
                                        ,@(when category `((category . ,category)))
                                        ,@(unless sort '((cycle-sort-function . identity)
                                                         (display-sort-function . identity))))
                                    (complete-with-action action (funcall async 'get) str pred)))
                                predicate require-match initial
                                (if (symbolp history) history (cadr history))
                                default))))
        (pcase-exhaustive history
          (`(:input ,var)
           (set var (cdr (symbol-value var)))
           (add-to-history var (cdr result)))
          ((pred symbolp)))
        (car result)))))

(defun consult--read-config (args)
  "Advice for `consult--read' which adjusts ARGS, merging the command configuration."
  (when-let (config (alist-get this-command consult-config))
    (setq args (copy-sequence args))
    (let ((options (cddr args)))
      (while config
        (setq options (plist-put options (car config) (cadr config))
              config (cddr config)))
      (setf (cddr args) options)))
  args)
(advice-add #'consult--read :filter-args #'consult--read-config)

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
  (consult--fontify-all)
  (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
         (heading-regexp (concat "^\\(?:" outline-regexp "\\)"))
         (candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil t))
        (setq line (+ line (consult--count-lines (match-beginning 0))))
        (push (list (point-marker) line
                    (buffer-substring (line-beginning-position) (line-end-position)))
              candidates)
        (unless (eobp) (forward-char 1))))
    (unless candidates
      (user-error "No headings"))
    (consult--add-line-number line (nreverse candidates))))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports candidate preview.
The symbol at point is added to the future history."
  (interactive)
  (consult--jump
   (consult--read
    "Go to heading: "
    (consult--with-increased-gc (consult--outline-candidates))
    :category 'consult-location
    :sort nil
    :require-match t
    :lookup #'consult--line-match
    :history '(:input consult--line-history)
    :add-history (thing-at-point 'symbol)
    :preview (consult--preview-position))))

;;;;; Command: consult-error

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
  "Jump to a compilation error in the current buffer.

This command works in compilation buffers and grep buffers.
The command supports preview of the currently selected error."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (user-error "Not a compilation buffer"))
  (consult--jump
   (consult--read
    "Go to error: "
    (consult--with-increased-gc (consult--error-candidates))
    :category 'consult-error
    :sort nil
    :require-match t
    :lookup #'consult--lookup-cadr
    :narrow `(,(lambda (cand) (= (caddr cand) consult--narrow))
              (?e . "Error")
              (?w . "Warning")
              (?i . "Info"))
    :history '(:input consult--error-history)
    :preview
    (consult--preview-position 'consult-preview-error))))

;;;;; Command: consult-mark

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
              (push (list marker line (consult--line-with-cursor marker))
                    candidates))))))
    (nreverse (consult--remove-dups (consult--add-line-number max-line candidates)))))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in the buffer-local `mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--jump
   (consult--read
    "Go to mark: "
    (consult--with-increased-gc (consult--mark-candidates))
    :category 'consult-location
    :sort nil
    :require-match t
    :lookup #'consult--lookup-location
    :history '(:input consult--line-history)
    :add-history (thing-at-point 'symbol)
    :preview (consult--preview-position))))

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
                (let* ((line (line-number-at-pos pos consult-line-numbers-widen))
                       (begin (line-beginning-position))
                       (end (line-end-position))
                       (loc (consult--format-location (buffer-name buf) line)))
                  (consult--fontify-region begin end)
                  (push (concat
                         (propertize
                          (concat (propertize (consult--encode-location marker) 'invisible t) loc)
                          'consult-location (cons marker line))
                         (consult--region-with-cursor begin end marker))
                        candidates))))))))
    (unless candidates
      (user-error "No global marks"))
    (nreverse (consult--remove-dups candidates))))

;;;###autoload
(defun consult-global-mark ()
  "Jump to a marker in `global-mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--jump
   (consult--read
    "Go to global mark: "
    (consult--with-increased-gc (consult--global-mark-candidates))
    ;; While `consult-global-mark' formats the candidates in grep-like
    ;; style, we are still not using the 'xref-location category,
    ;; since the locations are formatted using abbreviated buffer
    ;; names instead of file paths. If the 'xref-location category
    ;; would be used, Embark would embark-export to a broken grep-mode
    ;; buffer. By using the 'consult-location category, Embark will
    ;; export to an occur buffer instead! See also
    ;; https://github.com/minad/consult/issues/107.
    :category 'consult-location
    :sort nil
    :require-match t
    :lookup #'consult--lookup-location
    :history '(:input consult--line-history)
    :add-history (thing-at-point 'symbol)
    :preview (consult--preview-position))))

;;;;; Command: consult-line

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
                         (consult--line-number-prefix (point-marker) line line-width)
                         str))
                  (dist (abs (- curr-line line))))
              (when (< dist default-cand-dist)
                (setq default-cand cand
                      default-cand-dist dist))
              (push cand candidates)))
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
  (when-let (pos (consult--lookup-location input candidates cand))
    (if (or (string-blank-p input)
            (eq consult-line-point-placement 'line-beginning))
        pos
      ;; Strip unique line number prefix
      (let ((i 0)
            (n (length cand))
            (cmin consult--tofu-char)
            (cmax (- (+ consult--tofu-char consult--tofu-range) 1)))
        (while (and (< i n) (<= cmin (aref cand i) cmax))
          (setq i (1+ i)))
        (when (> i 0)
          (setq cand (substring cand i))))
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
This command obeys narrowing. Optionally INITIAL input can be provided.
The symbol at point and the last `isearch-string' is added to the future history."
  (interactive)
  (let ((candidates (consult--with-increased-gc (consult--line-candidates))))
    (consult--jump
     (consult--read
      "Go to line: " (cdr candidates)
      :category 'consult-location
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
      :preview (consult--preview-position)))))

;;;;; Command: consult-keep/flush-lines

(defun consult--filter-lines (prompt match)
  "Select a subset of the lines in the current buffer with live preview.

The lines selected are those that match the minibuffer input
according to the function MATCH. This command obeys narrowing.
PROMPT is the prompt shown to the user."
  (consult--forbid-minibuffer)
  (barf-if-buffer-read-only)
  (consult--with-increased-gc
   (consult--fontify-all)
   (let* ((buffer (current-buffer))
          (lines)
          (font-lock-orig font-lock-mode)
          (point-orig (point))
          ;; See `atomic-change-group' for these settings
          (undo-outer-limit nil)
          (undo-limit most-positive-fixnum)
          (undo-strong-limit most-positive-fixnum)
          (changes (prepare-change-group)))
     (save-excursion
       (let ((pos (point-min))
             (max (point-max))
             end)
         (while (< pos max)
           (goto-char pos)
           (setq end (line-end-position))
           (push (buffer-substring pos end) lines)
           (setq pos (1+ end))))
       (setq lines (nreverse lines)))
     (minibuffer-with-setup-hook
         (lambda ()
           (add-hook
            'after-change-functions
            (lambda (&rest _)
              (let* ((input (minibuffer-contents-no-properties))
                     (filtered-contents
                      ;; Special case the empty input for performance.
                      ;; Otherwise it could happen that the minibuffer is empty,
                      ;; but the buffer has not been updated.
                      (if (string= input "")
                          (string-join lines "\n")
                        ;; Allocate new string candidates since the matching function mutates!
                        (while-no-input
                          (string-join (funcall match input (mapcar #'copy-sequence lines)) "\n")))))
                (when (stringp filtered-contents)
                  (with-current-buffer buffer
                    (when font-lock-mode (font-lock-mode -1))
                    ;; Disable modification hooks for performance
                    (let ((inhibit-modification-hooks t))
                      (delete-region (point-min) (point-max))
                      (insert filtered-contents)
                      ;; Amalgamate immediately in order to avoid long undo list
                      (undo-amalgamate-change-group changes)
                      (goto-char (point-min)))))))
            nil t))
       (unwind-protect
           (progn
             (activate-change-group changes)
             (read-from-minibuffer prompt)
             (setq point-orig nil))
         ;; Disable modification hooks for performance
         (let ((inhibit-modification-hooks t))
           (if (not point-orig)
               (accept-change-group changes)
             (cancel-change-group changes)
             (goto-char point-orig)))
         (when (and font-lock-orig (not font-lock-mode))
           (font-lock-mode)))))))

;;;###autoload
(defun consult-keep-lines ()
  "Select a subset of the lines in the current buffer with live preview.

The lines selected are those that match the minibuffer input
according to your completion system.  This command obeys
narrowing."
  (interactive)
  (consult--filter-lines
   "Keep lines: "
   (run-hook-with-args-until-success 'consult--completion-match-hook 'highlight)))

;;;###autoload
(defun consult-flush-lines ()
  "Remove a subset of the lines in the current buffer with live preview.

The lines removed are those that match the minibuffer input
according to your completion system.  This command obeys
narrowing."
  (interactive)
  (consult--filter-lines
   "Flush lines: "
   (let ((match (run-hook-with-args-until-success 'consult--completion-match-hook 'highlight)))
     (lambda (input lines)
       (let ((filtered (funcall match input lines))
             (ht (make-hash-table :test #'equal :size (length lines))))
         (dolist (line filtered)
           (puthash line t ht))
         (seq-remove (lambda (line) (gethash line ht)) lines))))))

;;;;; Command: consult-goto-line

;;;###autoload
(defun consult-goto-line ()
  "Read line number and jump to the line with preview.

The command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'."
  (interactive)
  (consult--forbid-minibuffer)
  (consult--local-let ((display-line-numbers consult-goto-line-numbers)
                       (display-line-numbers-widen consult-line-numbers-widen))
    (let* ((config (alist-get 'consult-goto-line consult-config))
           (preview-key (if (plist-member config 'preview-key)
                            (plist-get config 'preview-key)
                          consult-preview-key)))
      (while (let ((ret (minibuffer-with-setup-hook
                            (:append (lambda ()
                                       (consult--setup-keymap nil nil preview-key)
                                       (setq-local consult--completion-candidate-hook
                                                   '(minibuffer-contents-no-properties))))
                          (consult--with-preview
                              preview-key
                              (let ((preview (consult--preview-position)))
                                (lambda (cand restore)
                                  (funcall preview
                                           (when-let (pos (and cand (consult--line-position cand)))
                                             (and (consult--in-range-p pos) pos))
                                           restore)))
                              (lambda (_ cand)
                                (when (and cand (string-match-p "^[[:digit:]]+$" cand))
                                  (string-to-number cand)))
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
                 t))))))

;;;;; Command: consult-recent-file

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
      (when-let (exit (plist-get completion-extra-properties :exit-function))
        (funcall exit completion exit-status))
      t)))

;;;;; Command: consult-mode-command

(defun consult--mode-commands (mode)
  "Extract commands from MODE.

The list of features is searched for files belonging to MODE.
From these files, the commands are extracted."
  (let ((library-path (symbol-file mode))
        (filter (consult--regexp-filter consult-mode-command-filter))
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
                    (not (string-match-p filter (symbol-name (cdr cmd))))))
             (cdr feature))))))
     load-history)))

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

;;;;; Command: consult-yank

(defun consult--yank-read ()
  "Open kill ring menu and return selected text."
  (consult--read
   "Yank text: "
   (consult--remove-dups kill-ring)
   :history t ;; disable history
   :sort nil
   :category 'kill-ring
   :require-match t
   :preview
   ;; If previous command is yank, hide previously yanked text
   (let* ((ov) (pt (point)) (mk (or (and (eq last-command 'yank) (mark t)) pt)))
     (lambda (cand restore)
       (if restore
           (when ov (delete-overlay ov))
         (unless ov (setq ov (consult--overlay (min pt mk) (max pt mk) 'invisible t)))
         ;; Use `add-face-text-property' on a copy of "cand in order to merge face properties
         (setq cand (copy-sequence cand))
         (add-face-text-property 0 (length cand) 'consult-preview-yank t cand)
         ;; Use the `before-string' property since the overlay might be empty.
         (overlay-put ov 'before-string cand))))))

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

;;;;; Command: consult-register

;;;###autoload
(defun consult-register-preview (reg)
  "Enhanced preview of register REG.

This function can be used as `register-preview-function'."
  (concat (truncate-string-to-width (consult--register-format reg) 100 0 nil "…") "\n"))

(defun consult--register-format (reg)
  "Format register REG for preview."
  (pcase-let ((`(,key . ,val) reg))
    (concat
     (propertize (single-key-description key) 'face 'consult-key)
     " "
     ;; Special printing for certain register types
     (cond
      ;; Display full string
      ((stringp val)
       (string-trim
        (replace-regexp-in-string
         "[ \t\n]+" " "
         val)))
      ;; Display 'file-query
      ((eq (car-safe val) 'file-query)
       (format "%s at position %d"
               (propertize (abbreviate-file-name (cadr val)) 'face 'consult-file)
               (caddr val)))
      ;; Display full line of buffer
      ((and (markerp val) (buffer-live-p (marker-buffer val)))
       (with-current-buffer (marker-buffer val)
         (save-restriction
           (save-excursion
             (widen)
             (goto-char val)
             (concat
              (consult--format-location (buffer-name) (line-number-at-pos))
              (consult--line-with-cursor val))))))
      ;; Default printing for the other types
      (t (register-describe-oneline key))))))

(defun consult--register-candidates ()
  "Return alist of register descriptions and register names."
  (mapcar (lambda (reg) (cons (consult--register-format reg) (car reg)))
          ;; Sometimes, registers are made without a `cdr'.
          ;; Such registers don't do anything, and can be ignored.
          (or (sort (seq-filter #'cdr register-alist) #'car-less-than-car)
              (user-error "All registers are empty"))))

;;;###autoload
(defun consult-register (reg)
  "Use register REG and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick
access to registers it is still recommended to use the built-in
register access functions, like `jump-to-register',
`point-to-register', `insert-register' and `copy-to-register'. The
command supports narrowing, see `consult-register-narrow'. Marker
positions are previewed."
  (interactive
   (list
    (consult--read
     "Register: "
     (consult--register-candidates)
     :category 'register
     :preview
     (let ((preview (consult--preview-position)))
       (lambda (cand restore)
         (funcall preview
                  ;; Preview markers
                  (when-let (reg (get-register cand))
                    (when (markerp reg)
                      reg))
                  restore)))
     :narrow
     (cons
      (lambda (cand)
        (let ((reg (get-register (cdr cand))))
          (seq-find (lambda (x)
                      (and
                       (= (car x) consult--narrow)
                       (funcall (caddr x) reg)))
                    consult-register-narrow)))
      (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
              consult-register-narrow))
     :sort nil
     :require-match t
     :history t ;; disable history
     :lookup #'consult--lookup-cdr)))
  (condition-case nil
      (jump-to-register reg)
    (error (insert-register reg))))

;;;;; Command: consult-bookmark

;;;###autoload
(defun consult-bookmark (name)
  "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration."
  (interactive
   (list
    (consult--with-file-preview (open)
      (consult--read
       "Bookmark: "
       (bookmark-all-names)
       ;; Add default names to future history.
       ;; Ignore errors such that `consult-bookmark' can be used in
       ;; buffers which are not backed by a file.
       :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
       :narrow
       (cons
        (lambda (cand)
          (if-let ((n consult--narrow)
                   (bm (bookmark-get-bookmark-record
                        (assoc cand bookmark-alist))))
              (eq n (caddr (alist-get
                            (or (alist-get 'handler bm) #'bookmark-default-handler)
                            consult-bookmark-narrow)))
            t))
        (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                consult-bookmark-narrow))
       :preview
       (let ((preview (consult--preview-position)))
         (lambda (cand restore)
           (funcall
            preview
            (when-let (bm (bookmark-get-bookmark-record
                           (assoc cand bookmark-alist)))
              (if-let* ((file (alist-get 'filename bm))
                        (pos (alist-get 'position bm))
                        ;; Only preview bookmarks without a handler
                        ;; aka `bookmark-default-handler'!
                        (buf (and (not (alist-get 'handler bm))
                                  (funcall open file))))
                  (set-marker (make-marker) pos buf)
                (unless restore (minibuffer-message "No preview for special bookmark"))
                nil))
            restore)))
       :history 'bookmark-history
       :category 'bookmark))))
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
          "Apropos: "
          obarray
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

;; This command has been adopted from https://github.com/oantolin/completing-history/.
;;;###autoload
(defun consult-history (&optional history)
  "Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument."
  (interactive)
  (let ((str (consult--local-let ((enable-recursive-minibuffers t))
               (consult--read
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
                :sort nil))))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (insert (substring-no-properties str))))

;;;;; Command: consult-minor-mode-menu

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
    :narrow `(,(lambda (cand) (seq-position (caddr cand) consult--narrow))
              (?l . "Local")
              (?g . "Global")
              (?i . "On")
              (?o . "Off"))
    :lookup #'consult--lookup-cadr
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
       "Theme: "
       (mapcar (lambda (x) (or x 'default)) avail-themes)
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (_input _cands x)
                 (and x (not (string= x "default")) (intern-soft x)))
       :preview (lambda (cand restore)
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

(defsubst consult--buffer-candidate (type cand face)
  "Format virtual buffer candidate.

CAND is the candidate string.
TYPE is the type character.
FACE is the face for the candidate."
  (concat (propertize (char-to-string (+ consult--tofu-char type)) 'invisible t) (propertize cand 'face face)))

(defun consult--buffer (open-buffer open-file open-bookmark)
  "Backend implementation of `consult-buffer'.

Depending on the selected item OPEN-BUFFER, OPEN-FILE or OPEN-BOOKMARK will be used to display the item."
  (let* ((curr-buf (current-buffer))
         (all-bufs (append (delq curr-buf (buffer-list)) (list curr-buf)))
         (buf-file-hash (let ((ht (make-hash-table :test #'equal)))
                          (dolist (buf all-bufs ht)
                            (when-let (file (buffer-file-name buf))
                              (puthash file t ht)))))
         (buf-filter (consult--regexp-filter consult-buffer-filter))
         (bufs (mapcar (lambda (x)
                         (let ((name (buffer-name x)))
                           (consult--buffer-candidate
                            (if (string-match-p buf-filter name) ?h ?b)
                            name 'consult-buffer)))
                       all-bufs))
         (views (when consult-view-list-function
                  (mapcar (lambda (x)
                            (consult--buffer-candidate ?v x 'consult-view))
                          (funcall consult-view-list-function))))
         (bookmarks (progn
                      (bookmark-maybe-load-default-file)
                      (mapcar (lambda (x)
                                (consult--buffer-candidate ?m (car x) 'consult-bookmark))
                              bookmark-alist)))
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
                       (let ((len (length proj-root))
                             (hidden-root (propertize proj-root 'invisible t)))
                         (mapcar (lambda (x)
                                   (consult--buffer-candidate
                                    ?q
                                    (concat hidden-root (substring x len))
                                    'consult-file))
                                 (seq-filter (lambda (x) (string-prefix-p proj-root x)) all-files)))))
         (selected
          (consult--read
           "Switch to: " (append bufs files proj-bufs proj-files views bookmarks)
           :history 'consult--buffer-history
           :sort nil
           :predicate
           (lambda (cand)
             (let ((type (- (aref cand 0) consult--tofu-char)))
               (when (= type ?q) (setq type ?p)) ;; q=project files
               (if (eq consult--narrow 32) ;; narrowed to hidden buffers
                   (= type ?h)
                 (and
                  (/= type ?h) ;; non-hidden buffers
                  (or (not consult--narrow) ;; narrowed
                      (= type consult--narrow))))))
           :narrow `((32 . "Hidden")
                     (?b . "Buffer")
                     (?f . "File")
                     (?m . "Bookmark")
                     ,@(when proj-root '((?p . "Project")))
                     ,@(when consult-view-list-function '((?v . "View"))))
           :category 'consult-buffer
           :lookup
           (lambda (_ candidates cand)
             (if (member cand candidates)
                 (cons (pcase-exhaustive (- (aref cand 0) consult--tofu-char)
                         (?b open-buffer)
                         (?h open-buffer)
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
           (lambda (cand restore)
             (cond
              (restore)
              ;; In order to avoid slowness and unnecessary complexity, we
              ;; only preview buffers. Loading recent files, bookmarks or
              ;; views can result in expensive operations.
              ((and (or (eq (car cand) #'switch-to-buffer)
                        (eq (car cand) #'switch-to-buffer-other-window))
                    (get-buffer (cdr cand)))
               (funcall (car cand) (cdr cand) 'norecord)))))))
    (when selected (funcall (car selected) (cdr selected)))))

;;;###autoload
(defun consult-buffer-other-frame ()
  "Enhanced `switch-to-buffer-other-frame' with support for virtual buffers.

See `consult-buffer'."
  (interactive)
  (consult--buffer #'switch-to-buffer-other-frame #'find-file-other-frame
                   ;; bookmark-jump-other-frame is supported on Emacs >= 27.1, we want to support at least 26
                   (lambda (bm) (bookmark-jump bm #'switch-to-buffer-other-frame))))

;;;###autoload
(defun consult-buffer-other-window ()
  "Enhanced `switch-to-buffer-other-window' with support for virtual buffers.

See `consult-buffer'."
  (interactive)
  (consult--buffer #'switch-to-buffer-other-window #'find-file-other-window #'bookmark-jump-other-window))

;;;###autoload
(defun consult-buffer ()
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files
as virtual buffers. Buffers are previewed. Furthermore narrowing to
buffers (b), files (f), bookmarks (m), views (v) and project files (p)
is supported via the corresponding keys. In order to determine the
project-specific files and buffers, the
`consult-project-root-function' is used. The list of available views
is obtained by calling `consult-view-list-function'."
  (interactive)
  (consult--buffer #'switch-to-buffer #'find-file #'bookmark-jump))

;;;;; Command: consult-kmacro

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
  "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted."
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

;;;;; Command: consult-imenu

(defun consult--imenu-special (_name pos buf name fn &rest args)
  "Wrapper function for special imenu items.

POS is the position.
BUF is the buffer.
NAME is the item name.
FN is the original special item function.
ARGS are the arguments to the special item function."
  (switch-to-buffer buf)
  (apply fn name pos args))

(defun consult--imenu-flatten (prefix list)
  "Flatten imenu LIST.
Prepend PREFIX in front of all items."
  (mapcan
   (lambda (item)
     (if (imenu--subalist-p item)
         (consult--imenu-flatten
          (concat prefix (and prefix "/") (car item))
          (cdr item))
       (let ((key (concat
                   (and prefix (concat (propertize prefix 'face 'consult-imenu-prefix) " "))
                   (car item)))
             (payload (cdr item)))
         (list (cons key
                     (pcase payload
                       ;; Simple marker item
                       ((pred markerp) payload)
                       ;; Simple integer item
                       ((pred integerp) (copy-marker payload))
                       ;; Semantic uses overlay for positions
                       ((pred overlayp) (copy-marker (overlay-start payload)))
                       ;; Wrap special item
                       (`(,pos ,fn . ,args)
                        (append
                         (list pos #'consult--imenu-special (current-buffer) (car item) fn)
                         args))
                       (_ (error "Unknown imenu item: %S" item))))))))
   list))

(defun consult--imenu-compute ()
  "Compute imenu candidates."
  (consult--forbid-minibuffer)
  (let* ((imenu-auto-rescan t)
         (imenu-use-markers t)
         (items (imenu--make-index-alist t)))
    (setq items (remove imenu--rescan-item items))
    ;; Fix toplevel items, e.g., emacs-lisp-mode toplevel items are functions
    (when-let (toplevel (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-toplevel)))
      (let ((tops (seq-remove (lambda (x) (listp (cdr x))) items))
            (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
        (setq items (append rest (and tops (list (cons toplevel tops)))))))
    (consult--imenu-flatten nil items)))

(defun consult--imenu-items ()
  "Return cached imenu candidates."
  (unless (equal (car consult--imenu-cache) (buffer-modified-tick))
    (setq consult--imenu-cache (cons (buffer-modified-tick) (consult--imenu-compute))))
  (cdr consult--imenu-cache))

(defun consult--project-imenu-items ()
  "Return imenu items from every buffer with the same `major-mode'."
  (let ((proj-root (and consult-project-root-function (funcall consult-project-root-function))))
    (seq-mapcat (lambda (buf)
                  (when-let (file (buffer-file-name buf))
                    (when (and (eq (buffer-local-value 'major-mode buf) major-mode)
                               (or (not proj-root) (string-prefix-p proj-root file)))
                      (with-current-buffer buf
                        (consult--imenu-items)))))
                (buffer-list))))

(defun consult--imenu-jump (item)
  "Jump to imenu ITEM via `consult--jump'.

In contrast to the builtin `imenu' jump function,
this function can jump across buffers."
  (pcase item
    (`(,name ,pos ,fn . ,args) (apply fn name pos args))
    (`(,_ . ,pos) (consult--jump pos))
    (_ (error "Unknown imenu item: %S" item))))

(defun consult--imenu (items)
  "Choose from imenu ITEMS with preview.

The symbol at point is added to the future history."
  (consult--imenu-jump
   (consult--read
    "Go to item: "
    (or items (user-error "Imenu is empty"))
    :preview
    (let ((preview (consult--preview-position)))
      (lambda (cand restore)
        ;; Only preview simple menu items which are markers,
        ;; in order to avoid any bad side effects.
        (funcall preview (and (markerp (cdr cand)) (cdr cand)) restore)))
    :require-match t
    :narrow
    (let ((narrow (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-narrow))))
      (cons (lambda (cand)
              (when-let (n (cdr (assoc consult--narrow narrow)))
                (let* ((c (car cand))
                       (l (length n)))
                  (and (> (length c) l)
                       (eq t (compare-strings n 0 l c 0 l))
                       (= (aref c l) 32)))))
            narrow))
    :category 'imenu
    :lookup #'consult--lookup-elem
    :history 'consult--imenu-history
    :add-history (thing-at-point 'symbol)
    :sort nil)))

;;;###autoload
(defun consult-imenu ()
  "Choose item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-narrow', which configures the narrowing.

See also `consult-project-imenu'."
  (interactive)
  (consult--imenu (consult--imenu-items)))

;;;###autoload
(defun consult-project-imenu ()
  "Choose item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details."
  (interactive)
  (consult--imenu (consult--project-imenu-items)))

(defun consult--grep-matches (lines)
  "Find grep match for REGEXP in LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
        (when (string-match consult--grep-regexp str)
          (let* ((file (expand-file-name (consult--strip-ansi-escape (match-string 1 str))))
                 (line (string-to-number (consult--strip-ansi-escape (match-string 2 str))))
                 (str (substring str (match-end 0)))
                 (loc (consult--format-location (string-remove-prefix default-directory file) line))
                 (pos))
            (while (string-match consult--grep-match-regexp str)
              (unless pos
                (setq pos (match-beginning 0)))
              (setq str (concat (substring str 0 (match-beginning 0))
                                (propertize (match-string 1 str) 'face 'consult-preview-match)
                                (substring str (match-end 0)))))
            (setq str (consult--strip-ansi-escape str))
            (push (list (concat loc str) file line (or pos 0)) candidates)))))
    (nreverse candidates)))

;;;;; Command: consult-grep

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

(defun consult--grep (prompt cmd dir initial)
  "Run grep CMD in DIR with INITIAL input.

PROMPT is the prompt string.
The symbol at point is added to the future history."
  (let* ((prompt-dir (consult--directory-prompt prompt dir))
         (default-directory (cdr prompt-dir)))
    (consult--with-file-preview (open)
      (consult--jump
       (consult--read
        (car prompt-dir)
        (consult--async-command cmd
          (consult--async-transform consult--grep-matches))
        :lookup (consult--grep-marker open)
        :preview (consult--preview-position)
        :initial (concat consult-async-default-split initial)
        :add-history (concat consult-async-default-split (thing-at-point 'symbol))
        :require-match t
        :category 'xref-location
        :history '(:input consult--grep-history)
        :sort nil)))))

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

PROMPT is the prompt.
CMD is the find argument list.
The filename at point is added to the future history."
  (find-file
   (consult--read
    prompt
    (consult--async-command cmd
      (consult--async-map (lambda (x) (string-remove-prefix "./" x))))
    :sort nil
    :require-match t
    :initial (concat consult-async-default-split initial)
    :add-history (concat consult-async-default-split (thing-at-point 'filename))
    :category 'file
    :history '(:input consult--find-history))))

;;;###autoload
(defun consult-find (&optional dir initial)
  "Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Find" dir))
         (default-directory (cdr prompt-dir)))
    (consult--find (car prompt-dir) consult-find-command initial)))

;;;###autoload
(defun consult-locate (&optional initial)
  "Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive)
  (consult--find "Locate: " consult-locate-command initial))

;;;;; Command: consult-man

(defun consult--man-format (lines)
  "Format man candidates from LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
        (when (string-match "^\\(.*?\\) +- +\\(.*\\)$" str)
          (let ((name (match-string 1 str))
                (desc (match-string 2 str)))
            (push (cons
                   (format "%-30s %s"
                           (propertize name 'face 'consult-key)
                           desc)
                   name)
                  candidates)))))
    (nreverse candidates)))

;;;###autoload
(defun consult-man (&optional initial)
  "Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
  (interactive)
  (man (consult--read
        "Manual entry: "
        (consult--async-command consult-man-command
          (consult--async-transform consult--man-format))
        :require-match t
        :lookup #'consult--lookup-cdr
        :initial (concat consult-async-default-split initial)
        :add-history (concat consult-async-default-split (thing-at-point 'symbol))
        :history '(:input consult--man-history))))

;;;; Integration with the default completion system

(defun consult--default-completion-candidate ()
  "Return current candidate from default completion system."
  (when (and (not icomplete-mode) (eq completing-read-function #'completing-read-default))
    (let ((cand (minibuffer-contents-no-properties)))
      (when (test-completion cand
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
        cand))))

(defun consult--default-completion-match (&optional _highlight)
  "Return default matching function."
  (lambda (str cands)
    ;; completion-all-completions returns an improper list
    ;; where the last link is not necessarily nil. Fix this!
    (nconc (completion-all-completions str cands nil (length str)) nil)))

;; Announce now that consult has been loaded
(provide 'consult)

;;;; Integration with other completion systems

(with-eval-after-load 'icomplete (require 'consult-icomplete))
(with-eval-after-load 'selectrum (require 'consult-selectrum))

;; Local Variables:
;; outline-regexp: ";;;;* \\|(def\\(un\\|custom\\|var\\) consult-[a-z]"
;; End:
;;; consult.el ends here
