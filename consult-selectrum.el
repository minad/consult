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

;; The Selectrum integration for Consult ensures that previews work when using
;; Selectrum. Furthermore, some minor Selectrum-specific `completing-read'
;; tweaks are applied. This is an extra package, since the consult.el package
;; only depends on Emacs core components.

;;; Code:

(require 'consult)
(require 'selectrum)

(defun consult-selectrum--match ()
  "Return selectrum matching function."
  (when (eq completing-read-function #'selectrum-completing-read)
    selectrum-refine-candidates-function))

(add-hook 'consult--completion-match-hook #'consult-selectrum--match)

(defun consult-selectrum--candidate ()
  "Return current selectrum candidate."
  (when (eq completing-read-function #'selectrum-completing-read)
    (selectrum-get-current-candidate)))

(add-hook 'consult--completion-candidate-hook #'consult-selectrum--candidate)

(defun consult-selectrum--refresh ()
  "Refresh selectrum view."
  (when (eq completing-read-function #'selectrum-completing-read)
    (selectrum-exhibit)))

(add-hook 'consult--completion-refresh-hook #'consult-selectrum--refresh)

;; HACK: Hopefully selectrum adds something like this to the official API.
;; https://github.com/raxod502/selectrum/issues/243
;; https://github.com/raxod502/selectrum/pull/244
(advice-add #'consult--read :around
            (lambda (fun prompt candidates &rest opts)
              (minibuffer-with-setup-hook
                  (lambda ()
                    (setq-local selectrum--move-default-candidate-p (plist-get opts :default-top))
                    ;; Fix height for async completion table
                    (when (functionp candidates)
                      (setq-local selectrum-fix-minibuffer-height t)))
                (apply fun prompt candidates opts))))

(defun consult-selectrum--async-split-wrap (orig)
  "Wrap selectrum candidates highlight/refinement ORIG function for `consult--async-split'."
  (lambda (str cands)
    (funcall orig
             (if-let (pos (cdr (consult--async-split-string str)))
                 (substring str pos) "")
             cands)))

(defun consult-selectrum--async-split (orig async)
  "Advice for `consult--async-split' to be used by Selectrum.

ORIG is the original function.
ASYNC is the async function, argument to the original function."
  (if (eq completing-read-function #'selectrum-completing-read)
      (lambda (action)
        (pcase action
          ('setup
           (setq-local selectrum-refine-candidates-function
                       (consult-selectrum--async-split-wrap selectrum-refine-candidates-function))
           (setq-local selectrum-highlight-candidates-function
                       (consult-selectrum--async-split-wrap selectrum-highlight-candidates-function))
           (funcall async 'setup))
          ((pred stringp) (funcall async (car (consult--async-split-string action))))
          (_ (funcall async action))))
    (funcall orig async)))

(advice-add #'consult--async-split :around #'consult-selectrum--async-split)

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
