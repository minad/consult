;;; consult-git-commit.el --- Consult commands for git-commit -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

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

;; Provides git-commit-related Consult commands.

;;; Code:

(require 'consult)
(require 'git-commit)

(defvar consult-git-commit--message-history nil)

(defun consult-git-commit--message-candidates ()
  "Return alist of git commit messages and indices."
  (thread-last
    ;; List of comments
    (ring-elements log-edit-comment-ring)
    (seq-map #'s-trim)
    ;; Add indices
    (seq-map-indexed #'cons)
    (mapcar (pcase-lambda (`(,message . ,index))
              (propertize
               message
               'consult--candidate index)))
    ;; Format macros
    (delete-dups)))

(defun consult-git-commit--message-select ()
  "Select from recent Git commit messages."
  (consult--read
   (or (consult-git-commit--message-candidates)
       (user-error "No recent Git commit messages"))
   :prompt "Commit message: "
   :category 'consult-git-commit-message
   :require-match t
   :sort nil
   :history 'consult-git-commit--message-history
   :lookup #'consult--lookup-candidate))

;;;###autoload
(defun consult-git-commit-message ()
  "Insert selected git commit message."
  (interactive)
  (let ((selected (consult-git-commit--message-select)))
    (setq log-edit-comment-ring-index selected)
    (git-commit-prev-message 0)))

(provide 'consult-git-commit)
;;; consult-git-commit.el ends here
