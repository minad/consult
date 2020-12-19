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

(defun consult-selectrum--preview-update ()
  "Preview function used for Selectrum."
  (when consult--preview-function
    (when-let (cand (selectrum-get-current-candidate))
      (funcall consult--preview-function cand))))

(defun consult-selectrum--preview-setup ()
  "Setup preview support for selectrum."
  (advice-remove 'selectrum--minibuffer-post-command-hook #'consult-selectrum--preview-update)
  (when consult-preview-mode
    (advice-add 'selectrum--minibuffer-post-command-hook :after #'consult-selectrum--preview-update)))

(add-hook 'consult-preview-mode-hook #'consult-selectrum--preview-setup)
(consult-selectrum--preview-setup) ;; call immediately to ensure load-order independence

;; HACK: Hopefully selectrum adds something like this to the official API.
;; https://github.com/raxod502/selectrum/issues/243
;; https://github.com/raxod502/selectrum/pull/244
(defsubst consult-selectrum--configure (options)
  "Add OPTIONS to the next `selectrum-read' call."
  (when (and options (eq completing-read-function #'selectrum-completing-read))
    (letrec ((advice (lambda (orig prompt candidates &rest args)
                       (advice-remove #'selectrum-read advice)
                       (apply orig prompt candidates (append options args)))))
      (advice-add #'selectrum-read :around advice))))

(cl-defun consult-selectrum--read-advice (_prompt _candidates &rest rest &key default-top &allow-other-keys)
  "Advice for `consult--read' performing Selectrum-specific configuration.

_PROMPT, _CANDIDATES and REST are ignored.
DEFAULT-TOP keyword argument is used to configure Selectrum."
  (consult-selectrum--configure
   `(,@(unless default-top '(:no-move-default-candidate t)))))

(advice-add #'consult--read :before #'consult-selectrum--read-advice)

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
