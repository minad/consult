;;; consult-info.el --- Search through the info manuals -*- lexical-binding: t -*-

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

;; Provides the command `consult-info'.  This is an extra package,
;; to allow lazy loading of info.el.  The `consult-info' command
;; is autoloaded.

;;; Code:

(require 'consult)
(require 'info)

(defvar consult-info--history nil)

(defun consult-info--candidates (manuals input)
  "Dynamically find lines in MANUALS matching INPUT."
  (pcase-let ((`(,regexps . ,hl)
               (funcall consult--regexp-compiler input 'emacs t))
              (candidates nil)
              (buf-idx 0))
    (pcase-dolist (`(,manual . ,buf) manuals)
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        ;; TODO subfile support?!
        (while (and (not (eobp)) (re-search-forward (car regexps) nil t))
          (let ((bol (pos-bol))
                (eol (pos-eol)))
            (goto-char bol)
            (when (and
                   (not (looking-at "^\\s-*$"))
                   ;; Information separator character
                   (>= (- (point) 2) (point-min))
                   (not (eq (char-after (- (point) 2)) ?\^_))
                   ;; Only printable characters on the line, [:cntrl:] does
                   ;; not work?!
                   (not (re-search-forward "[^[:print:]]" eol t))
                   ;; Matches all regexps
                   (seq-every-p (lambda (r)
                                  (goto-char bol)
                                  (re-search-forward r eol t))
                                (cdr regexps))
                   ;; Find node beginning
                   (goto-char bol)
                   (if (search-backward "\n\^_" nil 'move)
                       (forward-line 2)
                     (when (looking-at "\^_")
                       (forward-line 1)))
                   ;; Node name
                   (re-search-forward "Node:[ \t]*" nil t))
              (let ((node (buffer-substring-no-properties
                           (point)
                           (progn
                             (skip-chars-forward "^,\t\n")
                             (point))))
                    (cand (concat
                           (funcall hl (buffer-substring-no-properties bol eol))
                           ;; Buffer index and bol for disambiguation
                           (consult--tofu-encode (logior (ash bol 8) buf-idx)))))
                (put-text-property 0 1 'consult--info
                                   (list (format "(%s)%s" manual node) bol buf) cand)
                (push cand candidates)))
            (goto-char (1+ eol)))))
      (cl-incf buf-idx))
    (nreverse candidates)))

(defun consult-info--position (cand)
  "Return position information for CAND."
  (when-let ((pos (and cand (get-text-property 0 'consult--info cand)))
             (matches (consult--point-placement cand 0))
             (dest (+ (cadr pos) (car matches))))
    `( ,(cdr matches) ,dest . ,pos)))

(defun consult-info--action (cand)
  "Jump to info CAND."
  (pcase (consult-info--position cand)
    (`( ,_matches ,pos ,node ,_bol ,_buf)
     (info node)
     (widen)
     (goto-char pos)
     (Info-select-node)
     (run-hooks 'consult-after-jump-hook))))

(defun consult-info--state ()
  "Info manual preview state."
  (let ((preview (consult--jump-preview)))
    (lambda (action cand)
      (pcase action
        ('preview
         (setq cand (consult-info--position cand))
         (funcall preview 'preview
                  (pcase cand
                    (`(,matches ,pos ,_node ,_bol ,buf)
                     (cons (set-marker (make-marker) pos buf) matches))))
         (let (Info-history Info-history-list Info-history-forward)
           (when cand (ignore-errors (Info-select-node)))))
        ('return
         (consult-info--action cand))))))

(defun consult-info--group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (if transform cand
    (car (get-text-property 0 'consult--info cand))))

;;;###autoload
(defun consult-info (&rest manuals)
  "Full text search through info MANUALS."
  (interactive
   (progn
     (info-initialize)
     (completing-read-multiple
      "Info Manuals: "
      (info--manual-names current-prefix-arg)
      nil t)))
  (let (buffers)
    (unwind-protect
        (progn
          (dolist (manual manuals)
            (with-current-buffer (generate-new-buffer (format "*info-preview-%s*" manual))
              (let (Info-history Info-history-list Info-history-forward)
                (Info-mode)
                (Info-find-node manual "Top"))
              (push (cons manual (current-buffer)) buffers)))
          (consult--read
           (consult--dynamic-collection
            (apply-partially #'consult-info--candidates (reverse buffers)))
           :state (consult-info--state)
           :prompt
           (format "Info (%s): "
                   (string-join (if (length> manuals 3)
                                    `(,@(seq-take manuals 3) ,"…")
                                  manuals)
                                ", "))
           :require-match t
           :sort nil
           :category 'consult-info
           :history '(:input consult-info--history)
           :group #'consult-info--group
           :initial (consult--async-split-initial "")
           :lookup #'consult--lookup-member))
      (dolist (buf buffers)
        (kill-buffer (cdr buf))))))

(provide 'consult-info)
;;; consult-info.el ends here
