;;; consult-register.el --- Consult commands for registers -*- lexical-binding: t -*-

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

;; Provides register-related Consult commands.

;;; Code:

(require 'consult)

(defcustom consult-register-narrow
  `((?n "Number" ,#'numberp)
    (?s "String" ,#'stringp)
    (?r "Rectangle" ,(lambda (x) (stringp (car-safe x))))
    ;; frameset-register-p and kmacro-register-p exists since 27.1
    (?f "Frameset" ,(lambda (x) (eq (type-of x) 'frameset-register)))
    (?k "Kmacro" ,(lambda (x) (eq (type-of x) 'kmacro-register)))
    (?p "Point" ,(lambda (x) (or (markerp x) (eq (car-safe x) 'file-query))))
    (?w "Window" ,(lambda (x) (window-configuration-p (car-safe x)))))
  "Register narrowing configuration.

Each element of the list must have the form '(char name predicate)."
  :type '(repeat (list character string function))
  :group 'consult)

;;;###autoload
(defun consult-register-window (buffer &optional show-empty)
  "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list."
  (let ((regs (seq-filter #'cdr register-alist))
        (separator
         (and (display-graphic-p)
              (propertize (concat (propertize " " 'display '(space :align-to right)) "\n")
                          'face '(:inherit consult-separator :height 1 :underline t)))))
    (when (or show-empty regs)
      (with-current-buffer-window buffer
          (cons 'display-buffer-below-selected
                '((window-height . fit-window-to-buffer)
	          (preserve-size . (nil . t))))
          nil
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local truncate-lines t)
        (setq-local window-min-height 1)
        (setq-local window-resize-pixelwise t)
        (insert (mapconcat
                 (lambda (reg)
                   (concat (funcall register-preview-function reg) separator))
                 (seq-sort #'car-less-than-car regs) nil))))))

;;;###autoload
(defun consult-register-format (reg)
  "Enhanced preview of register REG.

This function can be used as `register-preview-function'."
  (concat (consult-register--format reg) "\n"))

(defun consult-register--format (reg)
  "Format register REG for preview."
  (pcase-let ((`(,key . ,val) reg))
    (let* ((key-str (propertize (single-key-description key) 'face 'consult-key))
           (len (max 3 (length key-str))))
      (concat
       key-str
       (make-string (- len (length key-str)) ?\s)
       ;; Special printing for certain register types
       (cond
        ;; Display full string
        ((or (stringp val) (stringp (car-safe val)))
         (when (consp val)
           (setq val (mapconcat #'identity val "\n")))
         (mapconcat #'identity
                    (seq-take (split-string (string-trim val) "\n") 3)
                    (concat "\n" (make-string len ?\s))))
        ;; Display 'file-query
        ((eq (car-safe val) 'file-query)
         (format "%s at position %d"
                 (propertize (abbreviate-file-name (cadr val)) 'face 'consult-file)
                 (caddr val)))
        ;; Display full line of buffer
        ((and (markerp val) (marker-buffer val))
         (with-current-buffer (marker-buffer val)
           (save-restriction
             (save-excursion
               (widen)
               (goto-char val)
               (consult--format-location (buffer-name) (line-number-at-pos)
                                         (consult--line-with-cursor val))))))
        ;; Default printing for the other types
        (t (register-describe-oneline key)))))))

(defun consult-register--alist ()
  "Return register list or raise an error if the list is empty."
  ;; Sometimes, registers are made without a `cdr'.
  ;; Such registers don't do anything, and can be ignored.
  (or (seq-filter #'cdr register-alist) (user-error "All registers are empty")))

(defun consult-register--candidates ()
  "Return list of formatted register candidates."
  (mapcar (lambda (reg)
            (propertize
             (consult-register--format reg)
             'consult--candidate (car reg)
             'consult--type
             (car (seq-find (lambda (x) (funcall (caddr x) (cdr reg)))
                            consult-register-narrow))))
          (sort (consult-register--alist) #'car-less-than-car)))

;;;###autoload
(defun consult-register (&optional arg)
  "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick access to
registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in built-in
register access functions. The command supports narrowing, see
`consult-register-narrow'. Marker positions are previewed. See
`jump-to-register' and `insert-register' for the meaning of prefix ARG."
  (interactive "P")
  (let ((narrow (mapcar (lambda (x) (cons (car x) (cadr x)))
                        consult-register-narrow)))
    (consult-register-load
     (consult--read
      (consult-register--candidates)
      :prompt "Register: "
      :category 'consult-register
      :state
      (let ((preview (consult--jump-preview)))
        (lambda (cand restore)
          ;; Preview only markers
          (funcall preview
                   (when-let (reg (get-register cand))
                     (and (markerp reg) reg))
                   restore)))
      :group (consult--type-group narrow)
      :narrow (consult--type-narrow narrow)
      :sort nil
      :require-match t
      :history t ;; disable history
      :lookup #'consult--lookup-candidate)
     arg)))

;;;###autoload
(defun consult-register-load (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it. For a number or text, insert it. For a
location, jump to it. See `jump-to-register' and `insert-register' for the
meaning of prefix ARG."
  (interactive
   (list
    (and (consult-register--alist)
         (register-read-with-preview "Load register: "))
    current-prefix-arg))
  (condition-case nil
      (jump-to-register reg arg)
    (user-error (insert-register reg (not arg)))))

(defun consult-register--action (action-list)
  "Read register key and execute action from ACTION-LIST.

This function is derived from `register-read-with-preview'."
  (let* ((buffer "*Register Preview*")
         (prefix (car action-list))
         (action-list (cdr action-list))
         (action (car (nth 0 action-list)))
         (reg)
         (preview
          (lambda ()
	    (unless (get-buffer-window buffer)
	      (register-preview buffer 'show-empty)
              (when-let (win (get-buffer-window buffer))
                (with-selected-window win
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert
                     (propertize (concat prefix ":  ") 'face 'consult-help)
                     (mapconcat
                      (lambda (x)
                        (concat (propertize (format "M-%c" (car x)) 'face 'consult-key)
                                " " (propertize (cadr x) 'face 'consult-help)))
                      action-list "  "))
                    (fit-window-to-buffer)))))))
	 (timer (when (numberp register-preview-delay)
	          (run-at-time register-preview-delay nil preview)))
	 (help-chars (seq-remove #'get-register (cons help-char help-event-list))))
    (unwind-protect
        (while (not reg)
	  (while (memq (read-key (propertize (caddr (assq action action-list))
                                             'face 'minibuffer-prompt))
		       help-chars)
            (funcall preview))
          (cond
           ((or (eq ?\C-g last-input-event)
                (eq 'escape last-input-event)
                (eq ?\C-\[ last-input-event))
            (keyboard-quit))
           ((and (numberp last-input-event) (assq (logxor #x8000000 last-input-event) action-list))
            (setq action (logxor #x8000000 last-input-event)))
	   ((characterp last-input-event)
            (setq reg last-input-event))
           (t (error "Non-character input-event"))))
      (when (timerp timer)
        (cancel-timer timer))
      (let ((w (get-buffer-window buffer)))
        (when (window-live-p w)
          (delete-window w)))
      (when (get-buffer buffer)
        (kill-buffer buffer)))
    (when reg
      (funcall (cadddr (assq action action-list)) reg))))

;;;###autoload
(defun consult-register-store (arg)
  "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally deleting
the region when a prefix ARG is given. With a numeric prefix ARG, store/add the
number. Otherwise store point, frameset, window or kmacro."
  (interactive "P")
  (consult-register--action
   (cond
    ((use-region-p)
     (let ((beg (region-beginning))
           (end (region-end)))
       `("Region"
         (?c "copy" "Copy region to register: " ,(lambda (r) (copy-to-register r beg end arg t)))
         (?a "append" "Append region to register: " ,(lambda (r) (append-to-register r beg end arg)))
         (?p "prepend" "Prepend region to register: " ,(lambda (r) (prepend-to-register r beg end arg))))))
    ((numberp arg)
     `(,(format "Number %s" arg)
       (?s "store" ,(format "Store %s in register: " arg) ,(lambda (r) (number-to-register arg r)))
       (?a "add" ,(format "Add %s to register: " arg) ,(lambda (r) (increment-register arg r)))))
    (t
     `("Store"
       (?p "point" "Point to register: " ,#'point-to-register)
       (?f "frameset" "Frameset to register: " ,#'frameset-to-register)
       (?w "window" "Window to register: " ,#'window-configuration-to-register)
       ,@(and last-kbd-macro `((?k "kmacro" "Kmacro to register: " ,#'kmacro-to-register))))))))

(provide 'consult-register)
;;; consult-register.el ends here
