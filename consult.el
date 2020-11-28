;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

;; Author: Daniel Mendler, Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
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

;; TODO implement preview of virtual buffers? but it must be ensured that any newly opened files are closed again
;; TODO implement preview for consult--yank-read

(defgroup consult nil
  "Consultation using `completing-read'."
  :group 'convenience
  :prefix "consult-")

(defface consult-preview-line
  '((t :inherit region))
  "Face used to for line previews."
  :group 'consult)

(defface consult-preview-cursor
  '((t :inherit cursor))
  "Face used to for cursor previews."
  :group 'consult)

(defface consult-mark
  '((t :inherit error :weight normal))
  "Face used to highlight marks in `consult-mark'."
  :group 'consult)

(defface consult-lighter
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight lighters in `consult-minor-mode'."
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
  '((t :inherit completions-annotations :weight normal))
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

(defvar consult-face-history ()
  "History for the command `consult-face'.")

(defvar consult-outline-history ()
  "History for the command `consult-outline'.")

(defvar consult-mark-history ()
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

(defun consult--status-prefix (enabled)
  "Status prefix for given boolean ENABLED."
  (propertize " " 'display
              (if enabled
                  (propertize "+ " 'face 'consult-on)
                (propertize "- " 'face 'consult-off))))

;; TODO get rid of selectrum specifics where possible
(defvar selectrum-should-sort-p)
(defvar selectrum-mode)
(declare-function selectrum-read "selectrum")
(declare-function selectrum-get-current-candidate "selectrum")
(declare-function selectrum--minibuffer-post-command-hook "selectrum")

;; TODO implement consult--preview-begin/end for icomplete
;; TODO maybe selectrum could offer an api instead of advicing an internal function
(defun consult--preview-begin (callback)
  "Begin preview by hooking into the completion system.
Returns a handle which must be passed to `consult--preview-end'.
CALLBACK is called with the current candidate."
  (when (bound-and-true-p selectrum-mode)
    (let ((advice (lambda ()
                    (let ((cand (selectrum-get-current-candidate)))
                      (when cand (funcall callback cand))))))
      (advice-add #'selectrum--minibuffer-post-command-hook :after advice)
      advice)))

(defun consult--preview-end (handle)
  "End preview and destroy HANDLE."
  (when handle
    (advice-remove #'selectrum--minibuffer-post-command-hook handle)))

(defmacro consult--preview (enabled save restore preview body)
  "Preview support for completion.
ENABLED must be t to enable preview.
SAVE is an expression which returns some state to save before preview.
RESTORE is a pair (variable . expression) which restores the state.
PREVIEW is a pair (variable . expression) which previews the given candidate.
BODY is the body expression."
  (declare (indent 4))
  (let ((handle (make-symbol "handle")))
    `(if ,enabled
         (let ((,(car restore) ,save)
               (,handle (consult--preview-begin (lambda (,(car preview)) ,@(cdr preview)))))
           (unwind-protect
               ,body
             (consult--preview-end ,handle)
             ,@(cdr restore)))
       ,body)))

(defun consult--window ()
  "Return live window."
  (let ((win (minibuffer-selected-window)))
    (while (not (window-live-p win))
      (setq win (next-window)))
    win))

(defun consult--preview-line (cmd &optional arg)
  "Preview function for lines.
CMD is the preview command.
ARG is the command argument."
  (pcase cmd
    ('restore
     (remove-overlays nil nil 'consult-overlay t))
    ('preview
     (with-selected-window (consult--window)
       (goto-char arg)
       (recenter)
       (remove-overlays nil nil 'consult-overlay t)
       (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
         (overlay-put ov 'face 'consult-preview-line)
         (overlay-put ov 'consult-overlay t))
       (let ((ov (make-overlay (point) (1+ (point)))))
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

(defun consult--add-line-number (max-line candidates)
  "Add line numbers to unformatted CANDIDATES. The MAX-LINE is needed to determine the width."
  (let ((form (format "%%%dd" (length (number-to-string max-line)))))
    (mapc (lambda (cand)
            ;; TODO use prefix here or keep the line number as part of string?
            ;; If we would use a prefix, the alist approach would not work for duplicate lines!
            (setcar cand (concat (propertize (format form (caar cand))
                                             'face 'consult-line-number)
                                 " " (cdar cand))))
          candidates)))

(defun consult--outline-candidates ()
  "Return alist of outline headings and positions."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  (jit-lock-fontify-now)
  (let* ((max-line 0)
         (heading-regexp (concat "^\\(?:" outline-regexp "\\)"))
         (unformatted-candidates
          (save-excursion
            (let ((candidates))
              (goto-char (point-min))
              (while (re-search-forward heading-regexp nil 'move)
                (goto-char (match-beginning 0))
                (let ((line (line-number-at-pos (point) t)))
                  (setq max-line (max line max-line))
                  (push (cons
                         (cons
                          line
                          (buffer-substring (line-beginning-position) (line-end-position)))
                         (point-marker))
                        candidates)
                  (if (and (bolp) (not (eobp))) (forward-char 1))))
              (nreverse candidates)))))
    (or (consult--add-line-number max-line unformatted-candidates)
        (user-error "No headings"))))

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading."
  (interactive)
  (consult--goto
   (save-excursion
     (consult--read "Go to heading: " (consult--outline-candidates)
                    :sort nil
                    :require-match t
                    :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                    :history 'consult-outline-history
                    :preview (and consult-preview-outline #'consult--preview-line)))))

(defun consult--mark-candidates ()
  "Return alist of lines containing markers.
The alist contains (string . position) pairs."
  (unless (marker-position (mark-marker))
    (user-error "No marks"))
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  (jit-lock-fontify-now)
  (let* ((all-markers (cl-remove-duplicates (cons (mark-marker) mark-ring)
                                            :test (lambda (x y) (= (marker-position x) (marker-position y)))))
         (max-line 0)
         (unformatted-candidates
          (save-excursion
            (mapcar (lambda (marker)
                      (let* ((pos  (goto-char (marker-position marker)))
                             (col  (current-column))
                             ;; TODO line-number-at-pos is a very slow function, can this be replaced?
                             (line (line-number-at-pos pos t))
                             (lstr (buffer-substring (- pos col) (line-end-position)))
                             (cand (concat (substring lstr 0 col)
                                           #("┃" 0 1 (face consult-mark))
                                           (substring lstr col))))
                        (setq max-line (max line max-line))
                        (cons (cons line cand) marker)))
                    all-markers))))
    (consult--add-line-number max-line unformatted-candidates)))

(defun consult--goto (pos)
  "Push current position to mark ring, go to POS and recenter."
  (push-mark (point) t)
  (goto-char pos)
  (recenter))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in `mark-ring', signified by a highlighted vertical bar."
  (interactive)
  (consult--goto
   (save-excursion
     (consult--read "Go to mark: " (consult--mark-candidates)
                    :sort nil
                    :require-match t
                    :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                    :history 'consult-mark-history
                    :preview (and consult-preview-mark #'consult--preview-line)))))

(defun consult--line-candidates ()
  "Return alist of lines and positions."
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  (jit-lock-fontify-now)
  (let* ((default-cand)
         (candidates)
         (line (line-number-at-pos (point-min) t))
         (curr-line (line-number-at-pos (point) t))
         (line-format (format "%%%dd " (length (number-to-string (line-number-at-pos (point-max))))))
         (default-cand-dist most-positive-fixnum))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((str (buffer-substring (line-beginning-position) (line-end-position)))
              (dist (abs (- curr-line line))))
          (unless (string-blank-p str)
            (let ((cand (concat (propertize
                            ;; HACK: Disambiguate the line by prepending it with a unicode
                            ;; character in the supplementary private use plane b.
                            ;; This will certainly have many ugly consequences.
                            (concat (list (+ #x100000 (mod line #xFFFE))))
                            'display (propertize (format line-format line)
                                                 'face 'consult-line-number))
                           str)))
              (when (or (not default-cand) (< dist default-cand-dist))
                (setq default-cand cand
                      default-cand-dist dist))
              (push (cons cand (point-marker)) candidates)))
          (forward-line 1)
          (setq line (1+ line)))))
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
   (let ((candidates (consult--line-candidates)))
     (save-excursion
       (consult--read "Go to line: " (cdr candidates)
                      :sort nil
                      :require-match t
                      :history 'consult-line-history
                      :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                      :default (car candidates)
                      :preview (and consult-preview-line #'consult--preview-line))))))

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

(defun consult--yank-read ()
  "Open kill ring menu and return selected text."
  (consult--read "Ring: "
                 (cl-remove-duplicates kill-ring :test #'equal :from-end t)
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
                   (cl-remove-duplicates (mapcar #'prin1-to-string command-history) :test #'equal)
                   ;; :category 'command ;; TODO command category is wrong here I think? category "sexp"?
                   :history 'consult-command-history))))

;;;###autoload
(defun consult-minibuffer-history ()
  "Insert string from minibuffer history."
  (interactive)
  (insert
   (substring-no-properties
    (consult--read "Minibuffer: "
                   (cl-remove-duplicates minibuffer-history :test #'equal)
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
(defun consult-face ()
  "Select face and show description."
  (interactive)
  (describe-face
   (consult--read "Face: "
                  (sort
                   (mapcar (lambda (x)
                             (cons
                              (concat
                               (format "%-40s " (car x))
                               (propertize "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'face (car x)))
                              (car x)))
                           face-new-frame-defaults)
                   (lambda (x y) (string< (car x) (car y))))
                  :sort nil
                  :require-match t
                  :lookup (lambda (candidates x) (cdr (assoc x candidates)))
                  :history 'consult-face-history)))

;;;###autoload
(defun consult-theme (theme)
  "Enable THEME from `consult-themes'."
  (interactive
   (list
    (consult--read
     "Theme: "
     (mapcar #'symbol-name
             (seq-filter (lambda (x) (or (not consult-themes)
                                         (memq x consult-themes)))
                         (custom-available-themes)))
     :require-match t
     :category 'theme
     :history 'consult-theme-history
     :lookup (lambda (_ x) (and x (intern x)))
     :preview (and consult-preview-theme
                   (lambda (cmd &optional arg)
                     (pcase cmd
                       ('save (car custom-enabled-themes))
                       ('restore (consult-theme arg))
                       ('preview (consult-theme arg)))))
     :default (and (car custom-enabled-themes)
                   (symbol-name (car custom-enabled-themes))))))
  (unless (equal theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selectrum specific code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO consult--buffer performs dynamic computation of the candidate set.
;; this is currently not supported by completing-read+selectrum.
;; therefore the selectrum api is used directly.
(defun consult--buffer (open-buffer open-file open-bookmark)
  "Generic implementation of `consult-buffer'.
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
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the completions-annotations face is ignored by selectrum?
                                          (propertize "View" 'face 'completions-annotations)))
                            (bookmark-view-names))))
         (bookmarks (mapcar (lambda (x)
                              (propertize (car x)
                                          'face 'consult-bookmark
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the completions-annotations face is ignored by selectrum?
                                          (propertize "Bookmark" 'face 'completions-annotations)))
                            bookmark-alist))
         (all-files (mapcar (lambda (x)
                              (propertize (abbreviate-file-name x)
                                          'face 'consult-file
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the completions-annotations face is ignored by selectrum?
                                          (propertize "File" 'face 'completions-annotations)))
                            recentf-list))
         (files (remove curr-file all-files))
         (all-cands (append visible-bufs files bookmarks))
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
                    (cons 'candidates all-cands))))))
         (selected
          (consult--preview consult-preview-buffer
              (current-window-configuration)
              (state (set-window-configuration state))
              (buf (when (get-buffer buf)
                     (with-selected-window (consult--window)
                       (funcall open-buffer buf))))
            (let ((selectrum-should-sort-p))
              (selectrum-read "Switch to: " generate)))))
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

(provide 'consult)
;;; consult.el ends here
