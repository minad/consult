;;; consult-search.el --- Providing search via Consult and Projectile  -*- lexical-binding: t; -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((projectile "2.0") (emacs "26.1"))
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

;; consult-search integrates command line search tools and presents the results
;; using completing-read.  This was written to work with consult

;; This only works with ag for now

;;; Code:

(require 'projectile)

(defun consult-search--marked-string ()
  "Return the marked input (anything highlighted in buffer), nil if nothing is marked."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun consult-search--search-string ()
  "Get the input for search."
  (or (consult-search--marked-string)
      (thing-at-point 'symbol t)))

(defun consult-search--do-ag (s path)
  "Call ag with search string S on PATH and return all results.  Return a list of results (strings)."
  (with-temp-buffer
    (let ((exit-code (process-file "ag" nil t nil "--nocolor" "--nogroup" "--column" s path)))
      (when (= exit-code 0)
        (split-string (buffer-string) "\n" t)))))

(defun consult-search--show-search-completions (search-string)
  "Call ag with SEARCH-STRING on projectile's project root."
  (completing-read
   "ag: "
   (consult-search--do-ag search-string (projectile-project-root))
   nil t search-string))

(defun consult-search--get-or-read-search-string ()
  "Either get search string from or ask user for search string.  For use in interactive."
  (list (or (consult-search--search-string) (read-string "Search for: "))))

(defun consult-search--parse-filename-line-number (string)
  "Parse filename and line number from STRING, which is assumed to be from ag output.  Input format is expected to be <file path>:<line number>:<column number>:<line from file>.  Return list of <file path>, <line number as int>, <column number as int>."
  (let ((parts (split-string string ":")))
    (list (car parts) (string-to-number (cadr parts)) (string-to-number (caddr parts)))))

(defun consult-search--open-user-choice (string)
  "Open user choice STRING, which is assumed to be from ag output."
  (let* ((parsed (consult-search--parse-filename-line-number string))
         (filename (car parsed))
         (line-number (cadr parsed))
         (column-number (caddr parsed)))
    (find-file filename)
    ;; Go back to the start of the file, then go to line-number
    (goto-char (point-min)) (beginning-of-line line-number)
    ;; Go forward to the right column
    (forward-char (1- column-number)))
  (let ((parts (split-string string ":")))
    (list (car parts) (string-to-number (cadr parts)) (string-to-number (caddr parts)))))

(defun consult-ag (search-string)
  "Search for the marked string, or symbol at point, or if neither of the above are available, a SEARCH-STRING from prompt, then presents the user with search results from ag, and then opens the user's choice in the buffer."
  (interactive (consult-search--get-or-read-search-string))
  (let ((user-choice (consult-search--show-search-completions search-string)))
    (consult-search--open-user-choice user-choice)))

(provide 'consult-search)

;;; consult-search.el ends here
