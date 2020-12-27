;;; -*- lexical-binding: t -*-

(consult--read
 "Test: "
  (thread-first (consult--async-sink)
    (consult--async-map (lambda (x) (concat "<" x ">")))
    (consult--async-map (lambda (x) (concat "#" x "#")))
    (consult--async-refresh)
    (consult--async-indicator)
    (consult--async-filter (lambda (x) (not (string-match-p "1" x))))
    (consult--async-process '("sh" "/home/user/elisp-snippets/test.sh")))
 :sort nil)

(defun consult--async-input (async)
  (let ((candidates))
    (lambda (action)
      (when (stringp action)
        (unless (or (string= action "") (member action candidates))
          (push action candidates))
        (funcall async 'flush)
        (funcall async (mapcar
                        (lambda (x)
                          (concat (propertize (concat action " ") 'display "") x))
                        candidates)))
      (funcall async action))))

(consult--read
 "Test: "
 (thread-first (consult--async-sink)
   (consult--async-map (lambda (x) (concat "<" x ">")))
   (consult--async-refresh)
   (consult--async-input)))
