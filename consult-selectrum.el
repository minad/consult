;;; consult-selectrum.el --- Selectrum integration for Consult -*- lexical-binding: t; -*-

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

;; Integration code for Selectrum.

;;; Code:

(require 'consult)

;; NOTE: It is not guaranteed that Selectrum is available during compilation!
(defvar selectrum--move-default-candidate-p)
(defvar selectrum-active-p)
(defvar selectrum-fix-vertical-window-height)
(defvar selectrum-highlight-candidates-function)
(defvar selectrum-refine-candidates-function)
(declare-function selectrum-exhibit "selectrum")
(declare-function selectrum-get-current-candidate "selectrum")

(defun consult-selectrum--filter (_category highlight)
  "Return selectrum filter function with HIGHLIGHT."
  ;; Do not use selectrum-active-p here, since we want to always use
  ;; the Selectrum filtering when Selectrum is installed, even when
  ;; Selectrum is currently not active.
  ;; However if `selectrum-refine-candidates-function' is the default
  ;; function, which uses the completion styles, the Selectrum filtering
  ;; is not used and `consult--default-completion-filter' takes over.
  (when (and (eq completing-read-function 'selectrum-completing-read)
             (not (eq selectrum-refine-candidates-function
                      'selectrum-refine-candidates-using-completions-styles)))
    (if highlight
        (lambda (str cands)
          (funcall selectrum-highlight-candidates-function str
                   (funcall selectrum-refine-candidates-function str cands)))
      selectrum-refine-candidates-function)))

(defun consult-selectrum--candidate ()
  "Return current selectrum candidate."
  (and selectrum-active-p (selectrum-get-current-candidate)))

(defun consult-selectrum--refresh ()
  "Refresh selectrum view."
  (and selectrum-active-p (selectrum-exhibit 'keep-selected)))

(cl-defun consult-selectrum--read-setup-adv (candidates &key default-top &allow-other-keys)
  "Advice for `consult--read-setup' for Selectrum specific setup.

See `consult--read' for the CANDIDATES and DEFAULT-TOP arguments."
  (setq-local selectrum--move-default-candidate-p default-top)
  ;; Fix selectrum height for async completion table
  (when (functionp candidates) (setq-local selectrum-fix-vertical-window-height t)))

(defun consult-selectrum--async-split-wrap (orig)
  "Wrap selectrum candidates highlight/refinement ORIG function for `consult--async-split'."
  (lambda (str cands)
    (funcall orig (substring str (cdr (consult--async-split-string str))) cands)))

(defun consult-selectrum--async-split-setup-adv (fun)
  "Advice for `consult--async-split-setup' to be used by Selectrum.

FUN is the original function."
  (if (not selectrum-active-p)
      (funcall fun)
    (setq-local selectrum-refine-candidates-function
		(consult-selectrum--async-split-wrap selectrum-refine-candidates-function))
    (setq-local selectrum-highlight-candidates-function
		(consult-selectrum--async-split-wrap selectrum-highlight-candidates-function))))

(add-hook 'consult--completion-filter-hook #'consult-selectrum--filter)
(add-hook 'consult--completion-candidate-hook #'consult-selectrum--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-selectrum--refresh)
(advice-add #'consult--read-setup :before #'consult-selectrum--read-setup-adv)
(advice-add #'consult--async-split-setup :around #'consult-selectrum--async-split-setup-adv)

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
