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
  (pcase-let* ((`(,regexps . ,hl)
                (funcall consult--regexp-compiler input 'emacs t))
               (re (concat "\\(\^_\n\\(?:.*Node:[ \t]*\\([^,\t\n]+\\)\\)?.*\n\\)\\|" (car regexps)))
               (candidates nil)
               (cand-idx 0)
               (last-node nil)
               (full-node nil))
    (pcase-dolist (`(,manual . ,buf) manuals)
      (with-current-buffer buf
        (setq last-node nil full-node nil)
        (widen)
        (goto-char (point-min))
        ;; TODO Info has support for subfiles, which is currently not supported
        ;; by the `consult-info' search routine.  Fortunately most (or all?)
        ;; Emacs info files are generated with the --no-split option.  See the
        ;; comment in doc/emacs/Makefile.in.  Given the computing powers these
        ;; days split info files are probably also not necessary anymore.
        ;; However it could happen that info files installed as part of the
        ;; Linux distribution are split.
        (while (and (not (eobp)) (re-search-forward re nil t))
          (if (match-end 1)
              (progn
                (if-let ((node (match-string 2)))
                    (unless (equal node last-node)
                      (setq full-node (concat "(" manual ")" node)
                            last-node node))
                  (setq last-node nil full-node nil))
                (goto-char (1+ (pos-eol))))
            (let ((bol (pos-bol))
                  (eol (pos-eol)))
              (goto-char bol)
              (when (and
                     full-node
                     ;; Information separator character
                     (>= (- (point) 2) (point-min))
                     (not (eq (char-after (- (point) 2)) ?\^_))
                     ;; Non-blank line, only printable characters on the line.
                     (not (looking-at-p "^\\s-*$"))
                     (looking-at-p "^[[:print:]]*$")
                     ;; Matches all regexps
                     (seq-every-p (lambda (r)
                                    (goto-char bol)
                                    (re-search-forward r eol t))
                                  (cdr regexps)))
                (let ((cand (concat
                             (funcall hl (buffer-substring-no-properties bol eol))
                             (consult--tofu-encode cand-idx))))
                  (put-text-property 0 1 'consult--info (list full-node bol buf) cand)
                  (cl-incf cand-idx)
                  (push cand candidates)))
              (goto-char (1+ eol)))))))
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

(defun consult-info--prepare-buffers (manuals fun)
  "Prepare buffers for MANUALS and call FUN with buffers."
  (declare (indent 1))
  (let (buffers)
    (unwind-protect
        (let ((reporter (make-progress-reporter "Preparing" 0 (length manuals))))
          (consult--with-increased-gc
           (seq-do-indexed
            (lambda (manual idx)
              (push (cons manual (generate-new-buffer (format "*info-preview-%s*" manual)))
                    buffers)
              (with-current-buffer (cdar buffers)
                (let (Info-history Info-history-list Info-history-forward)
                  (Info-mode)
                  (Info-find-node manual "Top")))
              (progress-reporter-update reporter (1+ idx) manual))
            manuals))
          (progress-reporter-done reporter)
          (funcall fun (reverse buffers)))
      (dolist (buf buffers)
        (kill-buffer (cdr buf))))))

;;;###autoload
(defun consult-info (&rest manuals)
  "Full text search through info MANUALS."
  (interactive
   (if Info-current-file
       (list (file-name-base Info-current-file))
     (info-initialize)
     (completing-read-multiple
      "Info Manuals: "
      (info--manual-names current-prefix-arg)
      nil t)))
  (consult-info--prepare-buffers manuals
    (lambda (buffers)
      (consult--read
       (consult--dynamic-collection
        (apply-partially #'consult-info--candidates buffers))
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
       :add-history (consult--async-split-thingatpt 'symbol)
       :lookup #'consult--lookup-member))))

(provide 'consult-info)
;;; consult-info.el ends here
