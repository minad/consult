;;; consult-kmacro.el --- Provides the command `consult-kmacro' -*- lexical-binding: t -*-

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

;; Provides the command `consult-kmacro'.  This is an extra package,
;; to allow lazy loading of kmacro.el.  The `consult-kmacro' command
;; is autoloaded.

;;; Code:

(require 'consult)
(require 'kmacro)

(defvar consult-kmacro--history nil)

(defun consult-kmacro--candidates ()
  "Return alist of kmacros and indices."
  (thread-last
    ;; List of macros
    (append (and last-kbd-macro (list (kmacro-ring-head))) kmacro-ring)
    ;; Emacs 29 uses OClosures.  I like OClosures but it would have been better
    ;; if public APIs wouldn't change like that.
    (mapcar (lambda (x)
              (if (eval-when-compile (> emacs-major-version 28))
                  (list (kmacro--keys x) (kmacro--counter x) (kmacro--format x) x)
                `(,@x ,x))))
    ;; Filter mouse clicks
    (seq-remove (lambda (x) (seq-some #'mouse-event-p (car x))))
    ;; Format macros
    (mapcar (pcase-lambda (`(,keys ,counter ,format ,km))
              (propertize
               (format-kbd-macro keys 1)
               'consult--candidate km
               'consult-kmacro--annotation
               ;; If the counter is 0 and the counter format is its default,
               ;; then there is a good chance that the counter isn't actually
               ;; being used.  This can only be wrong when a user
               ;; intentionally starts the counter with a negative value and
               ;; then increments it to 0.
               (cond
                ((not (equal format "%d")) ;; show counter for non-default format
                 (format " (counter=%d, format=%s) " counter format))
                ((/= counter 0) ;; show counter if non-zero
                 (format " (counter=%d)" counter))))))
    (delete-dups)))

;;;###autoload
(defun consult-kmacro (arg)
  "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted."
  (interactive "p")
  (let ((km (consult--read
             (or (consult-kmacro--candidates)
                 (user-error "No keyboard macros defined"))
             :prompt "Keyboard macro: "
             :category 'consult-kmacro
             :require-match t
             :sort nil
             :history 'consult-kmacro--history
             :annotate
             (lambda (cand)
               (get-text-property 0 'consult-kmacro--annotation cand))
             :lookup #'consult--lookup-candidate)))
    (unless km (user-error "No kmacro selected"))
    (funcall
     ;; Kmacros are lambdas (oclosures) on Emacs 29
     (if (eval-when-compile (> emacs-major-version 28))
         km
       (kmacro-lambda-form km))
     arg)))

(provide 'consult-kmacro)
;;; consult-kmacro.el ends here
