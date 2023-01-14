;;; consult-vertico.el --- Vertico integration for Consult -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

;; Integration code for the Vertico completion system.  This package
;; is automatically loaded by Consult.

;;; Code:

(require 'consult)

;; NOTE: It is not guaranteed that Vertico is available during compilation!
(defvar vertico--input)
(defvar vertico--history-hash)
(defvar vertico--lock-candidate)
(declare-function vertico--exhibit "ext:vertico")
(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--all-completions "ext:vertico")

(defun consult-vertico--candidate ()
  "Return current candidate for Consult preview."
  (and vertico--input (vertico--candidate 'highlight)))

(defun consult-vertico--refresh (&optional reset)
  "Refresh completion UI, keep current candidate unless RESET is non-nil."
  (when vertico--input
    (setq vertico--input t)
    (when reset
      (setq vertico--history-hash nil
            vertico--lock-candidate nil))
    (vertico--exhibit)))

(defun consult-vertico--filter-adv (orig pattern cands category highlight)
  "Advice for ORIG `consult--completion-filter' function.
See `consult--completion-filter' for arguments PATTERN, CANDS, CATEGORY
and HIGHLIGHT."
  (if (and (bound-and-true-p vertico-mode) (not highlight))
      ;; Optimize `consult--completion-filter' using the deferred highlighting
      ;; from Vertico.  The advice is not necessary - it is a pure optimization.
      (nconc (car (vertico--all-completions pattern cands nil (length pattern)
                                            `(metadata (category . ,category))))
             nil)
    (funcall orig pattern cands category highlight)))

(advice-add #'consult--completion-filter :around #'consult-vertico--filter-adv)
(add-hook 'consult--completion-candidate-hook #'consult-vertico--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-vertico--refresh)
(define-key consult-async-map [remap vertico-insert] 'vertico-next-group)

(provide 'consult-vertico)
;;; consult-vertico.el ends here
