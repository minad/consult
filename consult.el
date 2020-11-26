;;; consult.el --- Various interactive functions using completing-read -*- lexical-binding: t -*-

;; Author: The selectrum contributors
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

;; Consult implements a set of interactive functions which
;; use `completing-read' to select from a list of candidates.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'recentf)
(require 'seq)

(defgroup consult nil
  "Various functions using `completing-read'."
  :group 'convenience)

(defface consult-mark
  '((t :inherit error :weight normal))
  "Face used to highlight marks in `consult-marks'."
  :group 'consult)

(defface consult-file
  '((t :inherit font-lock-function-name-face :weight normal))
  "Face used to highlight files in `consult-switch-buffer'."
  :group 'consult)

(defface consult-bookmark
  '((t :inherit font-lock-constant-face :weight normal))
  "Face used to highlight bookmarks in `consult-switch-buffer'."
  :group 'consult)

(defface consult-view
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight views in `consult-switch-buffer'."
  :group 'consult)

;; TODO is there a more generic solution for sorting?
(defvar selectrum-should-sort-p)

;; TODO try to reduce selectrum-read usage
;; or move selectrum-dependent functions to a separate file
(declare-function selectrum-read "selectrum")

(defvar consult-marks-history ()
  "History for the command `consult-marks'.")

;; see https://github.com/raxod502/selectrum/issues/226
;;;###autoload
(defun consult-multi-occur (bufs regexp &optional nlines)
  "Improved version of `multi-occur' based on `completing-read-multiple'."
  (interactive (cons
                (mapcar #'get-buffer
                        (completing-read-multiple "Buffer: "
                                                  #'internal-complete-buffer))
                (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

;;;###autoload
(defun consult-marks ()
  "Jump to a marker in `mark-ring', signified by a highlighted vertical bar."
  (interactive)
  (unless (marker-position (mark-marker))
    (user-error "No marks exist"))
  (let* ((candidates-alist
          (save-excursion
            (cl-loop for marker in (cl-remove-duplicates
                                    (cons (mark-marker) mark-ring)
                                    :test (lambda (x y) (= (marker-position x) (marker-position y))))

                     for pos      = (goto-char (marker-position marker))
                     for col-num  = (current-column)
                     ;; TODO line-number-at-pos is a very slow function, replace it!
                     for line-num = (line-number-at-pos pos t)
                     for line-str = (buffer-substring (- pos col-num) (line-end-position))
                     for cand-str = (concat (substring line-str 0 col-num)
                                            #("┃" 0 1 (face consult-mark))
                                            (substring line-str col-num))

                     maximize line-num into max-line-num
                     maximize col-num into max-col-num

                     collect (list pos line-num col-num cand-str) into candidates

                     finally return
                     (cl-loop with form = (format "%%%dd %%%dd %%s"
                                                  (length (number-to-string max-line-num))
                                                  (length (number-to-string max-col-num)))
                              for cand in candidates
                              collect (cons (apply #'format form (cdr cand)) (car cand))))))
         (selectrum-should-sort-p)
         (chosen (completing-read "Go to marker: " candidates-alist nil t nil consult-marks-history)))
    (goto-char (cdr (assoc chosen candidates-alist)))))

;;;###autoload
(defun consult-find-recent-file (file)
  "Find recent FILE using `completing-read'."
  (interactive (list (completing-read
                      "Find recent file: "
                      (mapcar #'abbreviate-file-name recentf-list)
                      nil t)))
  (find-file file))

;;;###autoload
(defun consult-switch-buffer ()
  "Enhanced `switch-to-buffer' function with support for virtual buffers."
  (interactive)
  (let* ((selectrum-should-sort-p)
         (curr-buf (window-buffer (minibuffer-selected-window)))
         (curr-file (or (buffer-file-name curr-buf) ""))
         (bufs (mapcar #'buffer-name (delq curr-buf (buffer-list))))
         (hidden-bufs (seq-filter (lambda (x) (= (aref x 0) 32)) bufs))
         (visible-bufs (seq-filter (lambda (x) (/= (aref x 0) 32)) bufs))
         ;; TODO implement a solution to allow registration of custom virtual buffers.
         ;; Right now this uses view bookmarks.
         ;; https://github.com/minad/bookmark-view/blob/master/bookmark-view.el
         (views (if (fboundp 'bookmark-view-names)
                    (mapcar (lambda (x)
                              (propertize
                               x
                               'face 'consult-view
                               'consult-candidate 'bookmark
                               'selectrum-candidate-display-right-margin
                               (propertize "View" 'face 'completions-annotations)))
                            (bookmark-view-names))))
         (bookmarks (mapcar (lambda (x)
                              (propertize
                               (car x)
                               'face 'consult-bookmark
                               'consult-candidate 'bookmark
                               'selectrum-candidate-display-right-margin
                               (propertize "Bookmark" 'face 'completions-annotations)))
                            bookmark-alist))
         (all-files (mapcar (lambda (x)
                              (propertize
                               (abbreviate-file-name x)
                               'face 'consult-file
                               'consult-candidate 'file
                               'selectrum-candidate-display-right-margin
                               (propertize "File" 'face 'completions-annotations)))
                            recentf-list))
         (files (remove curr-file all-files))
         (all-cands (append visible-bufs files bookmarks))
         (gen-cands
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
             ((string-prefix-p "v " input)
              (list (cons 'input (substring input 2))
                    (cons 'candidates views)))
             ((string-prefix-p "m " input)
              (list (cons 'input (substring input 2))
                    (cons 'candidates bookmarks)))
             (t
              (list (cons 'input input)
                    (cons 'candidates all-cands))))))
         ;; TODO can this be replaced by completing-read?
         (chosen (selectrum-read "Switch to: " gen-cands)))
    (pcase (get-text-property 0 'consult-candidate chosen)
      ('file (find-file chosen))
      ('bookmark (bookmark-jump chosen))
      (_ (switch-to-buffer chosen)))))

(defun consult--yank-read ()
  "Open kill ring menu and return chosen text."
  (completing-read "Ring: "
                   (cl-remove-duplicates kill-ring :test #'equal :from-end t)
                   nil ':require-match))

;; Insert chosen text.
;; Adapted from the Emacs yank function.
;;;###autoload
(defun consult-yank (text)
  "Choose TEXT from the kill ring and insert it."
  (interactive (list (consult--yank-read)))
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
  (interactive (list (consult--yank-read)))
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
(defun consult-register ()
  "Use a register. Either jump to location or insert the stored text."
  (interactive)
  (let* ((selectrum-should-sort-p)
         (candidates-alist (mapcar
                            (lambda (r)
                              (setq r (car r))
                              (cons (format "%s: %s"
                                            (single-key-description r)
                                            (register-describe-oneline r))
                                    r))
                            (sort (copy-sequence register-alist) #'car-less-than-car)))
         (chosen (completing-read "Register: " candidates-alist nil t))
         (chosen-reg (cdr (assoc chosen candidates-alist))))
    (condition-case nil
        (jump-to-register chosen-reg)
      (error (insert-register chosen-reg)))))

(provide 'consult)
;;; consult.el ends here
