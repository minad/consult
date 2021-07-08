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
(defvar vertico--history-hash)
(defvar vertico--lock-candidate)
(declare-function vertico-exit "ext:vertico")
(declare-function vertico--exhibit "ext:vertico")
(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--match-p "ext:vertico")

(defun consult-vertico--candidate ()
  "Return current candidate for Consult preview."
  (and vertico--input (vertico--candidate 'highlight)))

(defun consult-vertico--refresh ()
  "Refresh completion UI."
  (when vertico--input
    (when consult--crm-reset
      (setq vertico--history-hash nil
            vertico--lock-candidate nil))
    (setq vertico--input t)
    (vertico--exhibit)))

(defun consult-vertico--crm-select ()
  "Select/deselect current candidate."
  (interactive)
  (let ((cand (vertico--candidate))
        (content (minibuffer-contents)))
    (when (and (not (equal cand "")) (vertico--match-p cand))
      (setq consult--crm-reset (lambda ()
                                 (setq vertico--history-hash nil
                                       vertico--input t)
                                 (delete-minibuffer-contents)
                                 (insert content)
                                 (vertico-next)
                                 (vertico--exhibit)))
      (vertico-exit))))

(defun consult-vertico--crm-select-erase ()
  "Select/deselect candidate."
  (interactive)
  (when (let ((cand (vertico--candidate)))
          (and (vertico--match-p cand) (not (equal cand ""))))
    (vertico-exit)))

(defun consult-vertico--crm-exit ()
  "Select/deselect candidate and exit."
  (interactive)
  (setq consult--crm-reset 'exit)
  (vertico-exit))

(defvar consult-vertico--crm-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap vertico-insert] #'consult-vertico--crm-select-erase)
    (define-key map [remap exit-minibuffer] #'consult-vertico--crm-exit)
    (define-key map [backtab] #'consult-vertico--crm-select)
    map))

(defun consult-vertico--crm-setup ()
  "Setup crm for Vertico."
  (message "SETUP")
  (when vertico--input
    (use-local-map (make-composed-keymap (list consult-vertico--crm-map) (current-local-map)))))

(add-hook 'consult--completion-candidate-hook #'consult-vertico--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-vertico--refresh)
(add-hook 'consult--crm-setup-hook #'consult-vertico--crm-setup)
(define-key consult-async-map [remap vertico-insert] 'vertico-next-group)

(provide 'consult-vertico)
;;; consult-vertico.el ends here
