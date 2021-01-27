;;; consult.el --- Consulting completing-read -*- lexical-binding: t -*-

(defun consult--async-process-filter (async cmd)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
CMD is the command argument list."
  (let ((proc) (flush) (rest "") (last-input ""))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (or (string= action "") (string= action last-input))
           (process-send-string proc (concat action "\n"))
           (setq last-input action)))
        ('destroy
         (ignore-errors (delete-process proc))
         (funcall async 'destroy))
        ('setup
         (funcall async 'setup)
         (consult--async-log "consult--async-process started %S\n" cmd)
         (setq
          proc
          (make-process
           :name (car cmd)
           :stderr consult--async-log
           :noquery t
           :command cmd
           :filter
           (lambda (_ out)
             (when-let (pos (seq-position out 3)) ;; ETX
               (when flush
                 (setq flush nil)
                 (funcall async 'flush))
               (setq out (concat (substring out 0 pos) (substring out (1+ pos)))))
             (when-let (pos (seq-position out 2)) ;; STX
               (setq flush t rest "" out (substring out (1+ pos))))
             (let ((lines (split-string out "\n")))
               (if (not (cdr lines))
                   (setq rest (concat rest (car lines)))
                 (setcar lines (concat rest (car lines)))
                 (setq rest (car (last lines)))
                 (when flush
                   (setq flush nil)
                   (funcall async 'flush))
                 (funcall async (nbutlast lines)))))
           :sentinel
           (lambda (_ event)
             (consult--async-log "consult--async-process sentinel: %s\n" event)
             (when (and (string-prefix-p "finished" event) (not (string= rest "")))
               (when flush
                 (setq flush nil)
                 (funcall async 'flush))
               (funcall async (list rest)))))))
        (_ (funcall async action))))))

(defun consult-async-find (&optional initial)
  (interactive)
  (find-file
   (consult--read
    "Find file: "
    (thread-first (consult--async-sink)
      (consult--async-refresh-immediate)
      ;;(consult--async-refresh-timer)
      (consult--async-process-filter '("consult-filter"

                                       ;; Candidate generation
                                       "find -type f"
                                       ;;"grep --exclude-dir=.git --line-buffered --color=never -r -v \"^$\" *"

                                       ;; Filtering
                                       ;;"rg --line-buffered -m 100 -i \"$0\""
                                       ;;"grep --line-buffered -m 100 -i -E \"$0\""
                                       ;;"fzy -e \"$0\" | head -n 100"
                                       "fzf -f \"$0\" | head -n 100"

                                       ))
      ;;(consult--async-throttle)
      (consult--async-split))
    :sort nil
    :require-match t
    :initial (concat consult-async-default-split initial)
    :add-history (concat consult-async-default-split (thing-at-point 'filename))
    :category 'file
    :history '(:input consult--find-history))))
