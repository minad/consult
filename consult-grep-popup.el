;;; consult-grep-popup.el --- Popup for grep-consult -*- lexical-binding: t -*-
(require 'consult)

(defun consult-grep-popup-update (args)
  "Update current search with ARGS."
  (if args
      (insert (concat " -- " (mapconcat 'identity args " ")))))

(setq grep-popup-function #'consult-grep-popup-update)

(defun consult-grep-args ()
  "Edit `consult-grep' args with a popup."
  (interactive)
  (if (minibufferp)
      (let* ((input (consult-grep-args--split (minibuffer-contents-no-properties)))
             (grep-popup-args (cdr input)))
        (kill-region (minibuffer-prompt-end) (point-max))
        (insert (car input))
        (grep-popup))
    (message "Not in minibuffer!")))

(defun consult-grep-args--split (input)
  "Split INPUT into cmd and args."
  (let* ((parts (split-string input " +-- +"))
         (cmd (or (car parts) ""))
         (args (split-string (or (cadr parts) "") " ")))
    (cons cmd args)))

(define-key consult-async-map (kbd "M-h") #'consult-grep-args)
(provide 'consult-consult-grep-popup)
;;; consult-consult-grep-popup.el ends here
