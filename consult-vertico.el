;;; consult-vertico.el --- Vertico integration for Consult -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

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

;; Integration code for the Vertico completion system. This package
;; is automatically loaded by Consult.

;;; Code:

(require 'consult)

;; NOTE: It is not guaranteed that Vertico is available during compilation!
(defvar vertico--input)
(declare-function vertico--exhibit "ext:vertico")
(declare-function vertico--candidate "ext:vertico")

(defun consult-vertico--candidate ()
  "Return current candidate for Consult preview."
  (and vertico--input (vertico--candidate 'highlight)))

(defun consult-vertico--refresh ()
  "Refresh completion UI, used by Consult async/narrowing."
  (when vertico--input
    (setq vertico--input t)
    (vertico--exhibit)))

(add-hook 'consult--completion-candidate-hook #'consult-vertico--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-vertico--refresh)

(provide 'consult-vertico)
;;; consult-vertico.el ends here
