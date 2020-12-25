;;; -*- lexical-binding: t -*-

(defun consult--async-example ()
  (thread-first (consult--async-sink)
    (consult--async-map (lambda (x) (concat "<" x ">")))
    (consult--async-map (lambda (x) (concat "#" x "#")))
    (consult--async-refresh)
    (consult--async-indicator)
    (consult--async-filter (lambda (x) (not (string-match-p "1" x))))
    (consult--async-process '("sh" "/home/user/elisp-snippets/test.sh"))))

(consult--read
 "Test: " (consult--async-example)
 :sort nil)
