;;; consult.el --- Consultation using completing-read -*- lexical-binding: t -*-

;; Author: The Selectrum contributors
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

;; TODO Decide on a consistent interactive-style, move all consult--read code to (interactive ...)?
;;      This makes sense for functions which can be used both interactively and non-interactively.
;; TODO Is it possible to add prefix/suffix/margin annotations using the standard completing-read api?
;; TODO consult-bindings
;; TODO consult-personal-bindings
;; TODO consult-major-mode
;; TODO reduce code duplication between consult-line, consult-mark, consult-outline?

(defgroup consult nil
  "Consultation using `completing-read'."
  :group 'convenience
  :prefix "consult-")

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

(defface consult-linum
  '((t :inherit completions-annotations :weight normal))
  "Face used to highlight line numbers in selections."
  :group 'consult)

(defface consult-on
  '((t :inherit success :weight bold))
  "Face used for `consult-on'."
  :group 'consult)

(defface consult-off
  '((t :inherit error :weight bold))
  "Face used for `consult-off'."
  :group 'consult)

(defcustom consult-on "+ "
  "Prefix string for active modes."
  :type 'string
  :group 'consult)

(defcustom consult-off "- "
  "Prefix string for disabled modes."
  :type 'string
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

(defun consult--status-prefix (flag)
  "Status prefix for given boolean FLAG."
  (propertize " " 'display
              (if flag
                  (propertize consult-on 'face 'consult-on)
                (propertize consult-off 'face 'consult-off))))

(cl-defun consult--read (prompt candidates &key predicate require-match history default category (sort t))
  "Simplified completing read function.

PROMPT is the string to prompt with.
CANDIDATES is the candidate list or alist. For alists, the cdr is returned.
PREDICATE is a filter function for the candidates.
REQUIRE-MATCH equals t means that an exact match is required.
HISTORY is the symbol of the history variable.
DEFAULT is the default input.
CATEGORY is the completion category.
SORT should be set to nil if the candidates are already sorted."
  ;; supported types
  (cl-assert (or (not candidates) ;; nil
                 (obarrayp candidates) ;; obarray
                 (stringp (car candidates)) ;; string list
                 (consp (car candidates)))) ;; alist
  ;; alists can only be used if require-match=t
  (cl-assert (or (not (and (consp candidates) (consp (car candidates)))) require-match))
  (let ((result (completing-read
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
                 predicate require-match nil history default)))
    ;; if candidates is an alist, we lookup the key and return the value
    (if (and result (consp candidates) (consp (car candidates)))
        (cdr (assoc result candidates))
      result)))

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

;;;###autoload
(defun consult-outline ()
  "Jump to an outline heading."
  (interactive)
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
                          (point))
                          candidates)
                  (if (and (bolp) (not (eobp))) (forward-char 1))))
              (nreverse candidates))))
         (form (format "%%%dd" (length (number-to-string max-line))))
         (candidates-alist (mapc (lambda (cand)
                                   ;; TODO use prefix here or keep the line number as part of string?
                                   ;; If we would use a prefix, the alist approach would not work for duplicate lines!
                                   (setcar cand (concat (propertize (format form (caar cand))
                                                                    'face 'consult-linum)
                                                        " " (cdar cand))))
                                 unformatted-candidates)))
    (goto-char (consult--read "Go to heading: "
                              candidates-alist
                              :sort nil
                              :require-match t
                              :history 'consult-outline-history))))

;;;###autoload
(defun consult-mark ()
  "Jump to a marker in `mark-ring', signified by a highlighted vertical bar."
  (interactive)
  (unless (marker-position (mark-marker))
    (user-error "No marks exist"))
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
                        (cons (cons line cand) pos)))
                    all-markers)))
         (form (format "%%%dd" (length (number-to-string max-line))))
         (candidates-alist (mapc (lambda (cand)
                                   ;; TODO use prefix here or keep the line number as part of string?
                                   ;; If we would use a prefix, the alist approach would not work for duplicate lines!
                                   (setcar cand (concat (propertize (format form (caar cand))
                                                                    'face 'consult-linum)
                                                        " " (cdar cand))))
                                 unformatted-candidates)))
    (goto-char (consult--read "Go to mark: "
                              candidates-alist
                              :sort nil
                              :require-match t
                              :history 'consult-mark-history))))

;;;###autoload
(defun consult-line ()
  "Search for a matching line and jump to the line beginning.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
  (interactive)
  ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line is not font-locked.
  ;; We would observe this if consulting an unfontified line.
  ;; Therefore we have to enforce font-locking now.
  (jit-lock-fontify-now)
  (let* ((default-cand)
         (candidates-alist
          (let* ((candidates)
                 (pos (point-min))
                 (line (line-number-at-pos pos t))
                 (curr-line (line-number-at-pos (point) t))
                 (buffer-lines (split-string (buffer-string) "\n"))
                 (line-format (format "%%%dd " (length (number-to-string (length buffer-lines)))))
                 (default-cand-dist most-positive-fixnum))
            (dolist (str buffer-lines)
              (unless (string-blank-p str)
                (let ((cand (concat (propertize
                                     ;; HACK: Disambiguate the line by prepending it with a unicode
                                     ;; character in the supplementary private use plane b.
                                     ;; This will certainly have many ugly consequences.
                                     (concat (list (+ #x100000 (mod line #xFFFE))))
                                     'display (propertize (format line-format line)
                                                          'face 'consult-linum))
                                    str))
                      (dist (abs (- curr-line line))))
                  (when (or (not default-cand) (< dist default-cand-dist))
                    (setq default-cand cand
                          default-cand-dist dist))
                  (push (cons cand pos) candidates)))
              (setq line (1+ line)
                    pos (+ pos (length str) 1)))
            (nreverse candidates)))
         (chosen (consult--read "Jump to matching line: "
                                (or candidates-alist (user-error "No lines"))
                                :sort nil
                                :require-match t
                                :history 'consult-line-history
                                :default default-cand)))
    (push-mark (point) t)
    (goto-char chosen)))

(defmacro consult--recent-file-read ()
  "Read recent file via `completing-read'."
  '(list (consult--read
          "Find recent file: "
          (or (mapcar #'abbreviate-file-name recentf-list) (user-error "No recent files"))
          :require-match t
          :category 'file
          :history 'file-name-history)))

;;;###autoload
(defun consult-recent-file (file)
  "Find recent FILE using `completing-read'."
  (interactive (consult--recent-file-read))
  (find-file file))

;;;###autoload
(defun consult-recent-file-other-frame (file)
  "Find recent FILE using `completing-read'."
  (interactive (consult--recent-file-read))
  (find-file-other-frame file))

;;;###autoload
(defun consult-recent-file-other-window (file)
  "Find recent FILE using `completing-read'."
  (interactive (consult--recent-file-read))
  (find-file-other-window file))

(defmacro consult--yank-read ()
  "Open kill ring menu and return chosen text."
  '(list (consult--read "Ring: "
                        (cl-remove-duplicates kill-ring :test #'equal :from-end t)
                        :require-match t)))

;; Insert chosen text.
;; Adapted from the Emacs yank function.
;;;###autoload
(defun consult-yank (text)
  "Choose TEXT from the kill ring and insert it."
  (interactive (consult--yank-read))
  (setq yank-window-start (window-start))
  (push-mark)
  (insert-for-yank text)
  (setq this-command 'yank)
  nil)

;;;###autoload
(defun consult-yank-pop (&optional arg)
  "If there is a recent yank act like `yank-pop'.
Otherwise choose text from the kill ring and insert it.
See `yank-pop' for the meaning of ARG."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop (or arg 1))
    (call-interactively #'consult-yank)))

;; Replace just-yanked text with chosen text.
;; Adapted from the Emacs yank-pop function.
;;;###autoload
(defun consult-yank-replace (text)
  "Choose TEXT from the kill ring.
If there was no recent yank, insert the text.
Otherwise replace the just-yanked text with the chosen text."
  (interactive (consult--yank-read))
  (if (not (eq last-command 'yank))
      (consult-yank text)
    (let ((inhibit-read-only t)
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

;;;###autoload
(defun consult-register (reg)
  "Use register REG. Either jump to location or insert the stored text."
  (interactive
   (list
    (let ((candidates-alist (mapcar
                             (lambda (r)
                               (setq r (car r))
                               (cons (format "%s: %s"
                                             (single-key-description r)
                                             (register-describe-oneline r))
                                     r))
                             (sort (copy-sequence register-alist) #'car-less-than-car))))
      (consult--read "Register: "
                     (or candidates-alist (user-error "All registers are empty"))
                     :sort nil
                     :require-match t
                     :history 'consult-register-history))))
  (condition-case nil
      (jump-to-register reg)
    (error (insert-register reg))))

;;;###autoload
(defun consult-theme (theme)
  "Enable THEME from the list of `custom-available-themes'."
  (interactive (list (intern
		      (consult--read "Theme: " (mapcar #'symbol-name (custom-available-themes))
                                     :require-match t
                                     :category 'theme
                                     :history 'consult-theme-history))))
  (mapc #'disable-theme custom-enabled-themes)
  (if (custom-theme-p theme)
      (enable-theme theme)
    (load-theme theme :no-confirm)))

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
(defun consult-apropos (pattern)
  "Call `apropos' for selected PATTERN."
  (interactive (list (consult--read "Apropos: "
                                    obarray
                                    :predicate (lambda (x) (or (fboundp x) (boundp x) (facep x) (symbol-plist x)))
                                    :history 'consult-apropos-history
                                    :category 'symbol
                                    :default (thing-at-point 'symbol))))
  (when (string= pattern "")
    (user-error "No pattern given"))
  (apropos pattern))

;;;###autoload
(defun consult-command-history (cmd)
  "Select CMD from the command history."
  (interactive (list (consult--read "Command: "
                                    (cl-remove-duplicates (mapcar #'prin1-to-string command-history) :test #'equal)
                                    ;; :category 'command ;; TODO command category is wrong here I think? category "sexp"?
                                    :history 'consult-command-history)))
  (eval (read cmd)))

;;;###autoload
(defun consult-minibuffer-history (str)
  "Insert STR from minibuffer history."
  (interactive (list (consult--read "Minibuffer: "
                                    (cl-remove-duplicates minibuffer-history :test #'equal)
                                    :history 'consult-minibuffer-history)))
  (insert (substring-no-properties str)))

;;;###autoload
(defun consult-minor-mode (mode)
  "Enable or disable minor MODE."
  (interactive
   (list
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
      (setq candidates-alist (sort candidates-alist (lambda (x y) (string< (car x) (car y)))))
      (setq candidates-alist (sort candidates-alist
                                   (lambda (x y)
                                     (> (if (symbol-value (cdr x)) 1 0)
                                        (if (symbol-value (cdr y)) 1 0)))))
      (consult--read "Minor modes: " candidates-alist
                     :sort nil
                     :require-match t
                     :history 'consult-minor-mode-history))))
  (call-interactively mode))

;;;###autoload
(defun consult-face (face)
  "Select FACE and show description."
  (interactive
   (list
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
                   :history 'consult-face-history)))
  (describe-face face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selectrum specific code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult--buffer performs dynamic computation of the candidate set.
;; this is currently not supported by completing-read+selectrum.
;; therefore the selectrum api is used directly.
(defvar selectrum-should-sort-p)
(declare-function selectrum-read "selectrum")

(defun consult--buffer (buffer-switch file-switch bookmark-switch)
  "Generic implementation of `consult-buffer'.
Depending on the selected item BUFFER-SWITCH, FILE-SWITCH or BOOKMARK-SWITCH will be used to display the item."
  (let* ((curr-buf (window-buffer (minibuffer-selected-window)))
         (curr-file (or (buffer-file-name curr-buf) ""))
         (bufs (mapcar #'buffer-name (delq curr-buf (buffer-list))))
         (hidden-bufs (seq-filter (lambda (x) (= (aref x 0) 32)) bufs))
         (visible-bufs (seq-filter (lambda (x) (/= (aref x 0) 32)) bufs))
         ;; TODO implement a solution to allow registration of custom virtual buffers.
         ;; Alternatively just hard-code other view libraries like perspective etc?
         ;; Right now only bookmarks-view is supported.
         ;; https://github.com/minad/bookmark-view/blob/master/bookmark-view.el
         (views (if (fboundp 'bookmark-view-names)
                    (mapcar (lambda (x)
                              (propertize x
                                          'face 'consult-view
                                          'consult-switch bookmark-switch
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the completions-annotations face is ignored by selectrum?
                                          (propertize "View" 'face 'completions-annotations)))
                            (bookmark-view-names))))
         (bookmarks (mapcar (lambda (x)
                              (propertize (car x)
                                          'face 'consult-bookmark
                                          'consult-switch bookmark-switch
                                          'selectrum-candidate-display-right-margin
                                          ;; TODO the completions-annotations face is ignored by selectrum?
                                          (propertize "Bookmark" 'face 'completions-annotations)))
                            bookmark-alist))
         (all-files (mapcar (lambda (x)
                              (propertize (abbreviate-file-name x)
                                          'face 'consult-file
                                          'consult-switch file-switch
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
         (selectrum-should-sort-p)
         (chosen (selectrum-read "Switch to: " generate)))
    (funcall (or (get-text-property 0 'consult-switch chosen) buffer-switch) chosen)))

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
