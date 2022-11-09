;;; consult-selectrum.el --- Selectrum integration for Consult -*- lexical-binding: t -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration code for the Selectrum completion system. This package
;; is automatically loaded by Consult.

;;; Code:

(require 'consult)

;; NOTE: It is not guaranteed that Selectrum is available during compilation!
(defvar selectrum-default-value-format)
(defvar selectrum-highlight-candidates-function)
(defvar selectrum-is-active)
(defvar selectrum-refine-candidates-function)
(defvar selectrum--history-hash)
(declare-function selectrum-exhibit "ext:selectrum")
(declare-function selectrum-get-current-candidate "ext:selectrum")

(defun consult-selectrum--filter-adv (orig pattern cands category highlight)
  "Advice for ORIG `consult--completion-filter' function.
See `consult--completion-filter' for arguments PATTERN, CANDS, CATEGORY
and HIGHLIGHT."
  ;; Do not use selectrum-is-active here, since we want to always use
  ;; the Selectrum filtering when Selectrum is installed, even when
  ;; Selectrum is currently not active.
  ;; However if `selectrum-refine-candidates-function' is the default
  ;; function, which uses the completion styles, the Selectrum filtering
  ;; is not used and the original function is called.
  (if (and (eq completing-read-function 'selectrum-completing-read)
           (not (eq selectrum-refine-candidates-function
                    'selectrum-refine-candidates-using-completions-styles)))
      (if highlight
          (funcall selectrum-highlight-candidates-function pattern
                   (funcall selectrum-refine-candidates-function pattern cands))
        (funcall selectrum-refine-candidates-function pattern cands))
    (funcall orig pattern cands category highlight)))

(defun consult-selectrum--candidate ()
  "Return current selectrum candidate."
  (and selectrum-is-active (selectrum-get-current-candidate)))

(defun consult-selectrum--refresh (&optional reset)
  "Refresh completion UI, keep current candidate unless RESET is non-nil."
  (when selectrum-is-active
    (when consult--narrow
      (setq-local selectrum-default-value-format nil))
    (when reset
      (setq-local selectrum--history-hash nil))
    (selectrum-exhibit (not reset))))

(defun consult-selectrum--split-wrap (orig split)
  "Wrap candidates highlight/refinement ORIG function.
The input is split by the SPLIT function."
  (lambda (str cands)
    (funcall orig (substring str (cadr (funcall split str))) cands)))

(defun consult-selectrum--split-setup-adv (orig split)
  "Advice for `consult--split-setup' to be used by Selectrum.

ORIG is the original function.
SPLIT is the splitter function."
  (if (not selectrum-is-active)
      (funcall orig split)
    (setq-local
     selectrum-refine-candidates-function
     (consult-selectrum--split-wrap selectrum-refine-candidates-function split)
     selectrum-highlight-candidates-function
     (consult-selectrum--split-wrap selectrum-highlight-candidates-function split))))

(defun consult-selectrum--deprecated (&rest _)
  (warn (propertize "Consult (%s): Selectrum support has been deprecated in favor of Vertico" 'face 'warning) this-command))

(add-hook 'consult--completion-candidate-hook #'consult-selectrum--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-selectrum--refresh)
(advice-add #'consult--completion-filter :around #'consult-selectrum--filter-adv)
(advice-add #'consult--split-setup :around #'consult-selectrum--split-setup-adv)
(define-key consult-async-map [remap selectrum-insert-current-candidate] 'selectrum-next-page)
(advice-add #'consult--read :before #'consult-selectrum--deprecated)

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
