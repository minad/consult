;;; consult-register.el --- Consult commands for registers -*- lexical-binding: t -*-

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

;; Provides register-related Consult commands.

;;; Code:

(require 'consult)
(require 'kmacro)

(defcustom consult-register-prefix #("#" 0 1 (face consult-key))
  "Prepend prefix in front of register keys during completion."
  :type '(choice (const nil) string)
  :group 'consult)

(defvar consult-register--narrow
  '((?n . "Number")
    (?s . "String")
    (?p . "Point")
    (?r . "Rectangle")
    (?t . "Frameset")
    (?k . "Kmacro")
    (?f . "File")
    (?b . "Buffer")
    (?w . "Window"))
  "Register type names.
Each element of the list must have the form (char . name).")

(cl-defun consult-register--format-value (val)
  "Format generic register VAL as string."
  (with-output-to-string (register-val-describe val nil)))

(cl-defgeneric consult-register--describe (val)
  "Describe generic register VAL."
  (list (consult-register--format-value val)))

(cl-defmethod consult-register--describe ((val number))
  "Describe numeric register VAL."
  (list (consult-register--format-value val) 'consult--type ?n))

(cl-defmethod consult-register--describe ((val string))
  "Describe string register VAL."
  (list val 'consult--type
        (if (eq (car (get-text-property 0 'yank-handler val))
                'rectangle--insert-for-yank)
            ?r ?s)))

(cl-defmethod consult-register--describe ((val marker))
  "Describe marker register VAL."
  (with-current-buffer (marker-buffer val)
    (save-excursion
      (without-restriction
        (goto-char val)
        (let* ((line (line-number-at-pos))
               (str (propertize (consult--line-with-mark val)
                                'consult-location (cons val line))))
          (list (consult--format-file-line-match (buffer-name) line str)
                'multi-category `(consult-location . ,str)
                'consult--type ?p))))))

(defmacro consult-register--describe-kmacro ()
  "Generate method which describes kmacro register."
  `(cl-defmethod consult-register--describe ((val ,(if (< emacs-major-version 30) 'kmacro-register 'kmacro)))
     (list (consult-register--format-value val) 'consult--type ?k)))
(consult-register--describe-kmacro)

(cl-defmethod consult-register--describe ((val (head file)))
  "Describe file register VAL."
  (list (propertize (abbreviate-file-name (cdr val)) 'face 'consult-file)
        'consult--type ?f 'multi-category `(file . ,(cdr val))))

(cl-defmethod consult-register--describe ((val (head buffer)))
  "Describe buffer register VAL."
  (list (propertize (cdr val) 'face 'consult-buffer)
        'consult--type ?f 'multi-category `(buffer . ,(cdr val))))

(cl-defmethod consult-register--describe ((val (head file-query)))
  "Describe file-query register VAL."
  (list (format "%s at position %d"
                (propertize (abbreviate-file-name (cadr val))
                            'face 'consult-file)
                (caddr val))
        'consult--type ?f 'multi-category `(file . ,(cadr val))))

(cl-defmethod consult-register--describe ((val cons))
  "Describe rectangle or window-configuration register VAL."
  (cond
   ((stringp (car val))
    (list (string-join val "\n") 'consult--type ?r))
   ((window-configuration-p (car val))
    (list (consult-register--format-value val)
          'consult--type ?w))
   (t (list (consult-register--format-value val)))))

(with-eval-after-load 'frameset
  (cl-defmethod consult-register--describe ((val frameset-register))
    "Describe frameset register VAL."
    (list (consult-register--format-value val) 'consult--type ?t)))

;;;###autoload
(defun consult-register-window (buffer &optional show-empty)
  "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list."
  (let ((regs (consult-register--alist 'noerror))
        (separator
         (and (display-graphic-p)
              (propertize #(" \n" 0 1 (display (space :align-to right)))
                          'face '(:inherit consult-separator :height 1 :underline t)))))
    (when (or show-empty regs)
      (with-current-buffer-window buffer
          (cons 'display-buffer-at-bottom
                '((window-height . fit-window-to-buffer)
                  (preserve-size . (nil . t))))
          nil
        (setq-local cursor-in-non-selected-windows nil
                    mode-line-format nil
                    truncate-lines t
                    window-min-height 1
                    window-resize-pixelwise t)
        (insert (mapconcat
                 (lambda (reg)
                   (concat (funcall register-preview-function reg) separator))
                 regs nil))))))

;;;###autoload
(defun consult-register-format (reg &optional completion)
  "Enhanced preview of register REG.
This function can be used as `register-preview-function'.
If COMPLETION is non-nil format the register for completion."
  (pcase-let* ((`(,key . ,val) reg)
               (key-str (propertize (single-key-description key) 'face 'consult-key))
               (key-len (max 3 (length key-str)))
               (`(,str . ,props) (consult-register--describe val)))
    (when (string-search "\n" str)
      (let* ((lines (seq-take (seq-remove #'string-blank-p (split-string str "\n")) 3))
             (space (apply #'min most-positive-fixnum
                           (mapcar (lambda (x) (string-match-p "[^ ]" x)) lines))))
        (setq str (mapconcat (lambda (x) (substring x space))
                             lines (concat "\n" (make-string (1+ key-len) ?\s))))))
    (setq str (concat
               (and completion consult-register-prefix)
               key-str (make-string (- key-len (length key-str)) ?\s) " "
               str (and (not completion) "\n")))
    (when completion
      (add-text-properties
       0 (length str)
       `(consult--candidate ,(car reg) ,@props)
       str))
    str))

(defun consult-register--alist (&optional noerror filter)
  "Return register list, sorted and filtered with FILTER.
Raise an error if the list is empty and NOERROR is nil."
  (or (sort (seq-filter
             ;; Sometimes, registers are made without a `cdr'.
             ;; Such registers don't do anything, and can be ignored.
             (lambda (x) (and (cdr x) (or (not filter) (funcall filter x))))
             register-alist)
            #'car-less-than-car)
      (and (not noerror) (user-error "All registers are empty"))))

(defun consult-register--candidates (&optional filter)
  "Return formatted completion candidates, filtered with FILTER."
  (mapcar (lambda (reg) (consult-register-format reg 'completion))
          (consult-register--alist nil filter)))

;;;###autoload
(defun consult-register (&optional arg)
  "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents.  For quick access
to registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in
built-in register access functions.  The command supports narrowing, see
`consult-register--narrow'.  Marker positions are previewed.  See
`jump-to-register' and `insert-register' for the meaning of prefix ARG."
  (interactive "P")
  (consult-register-load
   (consult--read
    (consult-register--candidates)
    :prompt "Register: "
    :category 'multi-category
    :state
    (let ((preview (consult--jump-preview)))
      (lambda (action cand)
        ;; Preview only markers
        (funcall preview action
                 (when-let (reg (get-register cand))
                   (and (markerp reg) reg)))))
    :group (consult--type-group consult-register--narrow)
    :narrow (consult--type-narrow consult-register--narrow)
    :sort nil
    :require-match t
    :history t ;; disable history
    :lookup #'consult--lookup-candidate)
   arg))

;;;###autoload
(defun consult-register-load (reg &optional arg)
  "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG."
  (interactive
   (list
    (and (consult-register--alist)
         (register-read-with-preview "Load register: "))
    current-prefix-arg))
  (condition-case err
      (jump-to-register reg arg)
    (user-error
     (unless (string-search "access aborted" (error-message-string err))
       (insert-register reg (not arg))))))

(defun consult-register--action (action-list)
  "Read register key and execute action from ACTION-LIST.

This function is derived from `register-read-with-preview'."
  (let* ((buffer "*Register Preview*")
         (prefix (car action-list))
         (action-list (cdr action-list))
         (action (car (nth 0 action-list)))
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
         (help-chars (seq-remove #'get-register (cons help-char help-event-list)))
         key reg)
    (unwind-protect
        (while (not reg)
          (while (memq (setq key
                             (read-key (propertize (caddr (assq action action-list))
                                                   'face 'minibuffer-prompt)))
                       help-chars)
            (funcall preview))
          (setq key (if (and (eql key ?\e) (characterp last-input-event))
                        ;; in terminal Emacs M-letter is read as two keys, ESC and the letter,
                        ;; use what would have been read in graphical Emacs
                        (logior #x8000000 last-input-event)
                      last-input-event))
          (cond
           ((or (eq ?\C-g key)
                (eq 'escape key)
                (eq ?\C-\[ key))
            (keyboard-quit))
           ((and (numberp key) (assq (logxor #x8000000 key) action-list))
            (setq action (logxor #x8000000 key)))
           ((characterp key)
            (setq reg key))
           (t (error "Non-character input"))))
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

With an active region, store/append/prepend the contents, optionally
deleting the region when a prefix ARG is given.  With a numeric prefix
ARG, store or add the number.  Otherwise store point, frameset, window or
kmacro."
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
       (?f "file" "File to register: " ,(lambda (r) (set-register r `(file . ,(buffer-file-name)))))
       (?t "frameset" "Frameset to register: " ,#'frameset-to-register)
       (?w "window" "Window to register: " ,#'window-configuration-to-register)
       ,@(and last-kbd-macro `((?k "kmacro" "Kmacro to register: " ,#'kmacro-to-register))))))))

(provide 'consult-register)
;;; consult-register.el ends here
