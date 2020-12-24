;; -*- lexical-binding: t -*-

(defvar consult-preview-xref t)

(defun consult--xref-candidates (xrefs)
  "Return candidate list from XREFS."
  (let* ((candidates
          (mapcar (lambda (xref)
                    (let ((loc (xref-item-location xref))
                          (xref-file-name-display 'nondirectory))
                      (list (xref-location-group loc)
                            (xref-location-line loc)
                            (xref-item-summary xref)
                            xref)))
                  xrefs))
          (max-name (apply #'max (mapcar (lambda (x) (length (car x))) candidates)))
          (max-line (apply #'max (mapcar (lambda (x) (cadr x)) candidates)))
          (fmt (format "%%%ds:%%-%dd" max-name (length (number-to-string max-line)))))
      (mapcar (pcase-lambda (`(,name ,line ,str ,xref))
                (cons (concat (propertize (format fmt name line) 'face 'consult-location)
                              "   " str)
                      xref))
              candidates)))

(defun consult--xref (prompt xrefs &optional display)
  "Select from XREFS and jump.

PROMPT is the `completing-read' prompt.
DISPLAY is the display action according to `xref-pop-to-location'."
  (xref-pop-to-location
   (consult--read
    prompt
    (consult--xref-candidates xrefs)
    :preview (when consult-preview-xref
               (let ((preview (consult--preview-position)))
                 (lambda (cand restore)
                   (cond
                    (restore (funcall preview cand t))
                    (cand (funcall preview (xref-location-marker (xref-item-location cand)) nil))))))
    :require-match t
    :sort nil
    :lookup #'consult--lookup-cdr)
   display))

(defun consult-show-xrefs (fetcher &optional alist)
  "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments."
  (consult--xref "Go to xref: "
                  (funcall fetcher)
                  (cdr (assoc 'display-action alist))))
