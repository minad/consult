;;; consult-selectrum.el --- Selectrum integration for Consult  -*- lexical-binding: t; -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.1") (selectrum "3.0") (emacs "26.1"))
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

;; Selectrum integration for Consult. This is an extra package, since
;; the consult.el package only depends on Emacs core components.

;;; Code:

(require 'consult)
(require 'selectrum)

(defun consult-selectrum--match ()
  "Return selectrum matching function."
  (and selectrum-active-p selectrum-refine-candidates-function))

(defun consult-selectrum--candidate ()
  "Return current selectrum candidate."
  (and selectrum-active-p (ignore-errors (selectrum-get-current-candidate))))

(defun consult-selectrum--refresh ()
  "Refresh selectrum view."
  (and selectrum-active-p (selectrum-exhibit 'keep-selected)))

(defun consult-selectrum--read-setup (fun prompt candidates &rest opts)
  "Advice, which configures `consult--read' for selectrum.

FUN is the original function.
See `consult--read' for the PROMPT, CANDIDATES and OPTS arguments."
  (minibuffer-with-setup-hook
      (lambda ()
        ;; Set mode-default-candidate selectrum option according to :default-top
        (setq-local selectrum--move-default-candidate-p (plist-get opts :default-top))
        ;; Fix selectrum height for async completion table
        (when (functionp candidates) (setq-local selectrum-fix-minibuffer-height t)))
    (apply fun prompt candidates opts)))

(defun consult-selectrum--async-split-wrap (orig)
  "Wrap selectrum candidates highlight/refinement ORIG function for `consult--async-split'."
  (lambda (str cands)
    (funcall orig (substring str (cdr (consult--async-split-string str))) cands)))

(defun consult-selectrum--async-split-setup ()
  "Advice for `consult--async-split-setup' to be used by Selectrum."
  (setq-local selectrum-refine-candidates-function
              (consult-selectrum--async-split-wrap selectrum-refine-candidates-function))
  (setq-local selectrum-highlight-candidates-function
              (consult-selectrum--async-split-wrap selectrum-highlight-candidates-function)))

(add-hook 'consult--completion-match-hook #'consult-selectrum--match)
(add-hook 'consult--completion-candidate-hook #'consult-selectrum--candidate)
(add-hook 'consult--completion-refresh-hook #'consult-selectrum--refresh)
(advice-add #'consult--read :around #'consult-selectrum--read-setup)
(advice-add #'consult--async-split-setup :before #'consult-selectrum--async-split-setup)

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
