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
  (let (candidates)
    (pcase-dolist (`(,manual . ,buffer) manuals)
      (with-current-buffer buffer
        (widen)
        (goto-char (point-min))
        (pcase-let ((`(,regexps . ,hl)
                     (funcall consult--regexp-compiler input 'emacs t)))
          ;; TODO subfile support?!
          (while (ignore-errors (re-search-forward (car regexps) nil t))
            (let ((bol (pos-bol))
                  (eol (pos-eol))
                  (current-node nil))
              (when
                  (save-excursion
                    (goto-char bol)
                    (and
                     (>= (- (point) 2) (point-min))
                     ;; Information separator character
                     (not (eq (char-after (- (point) 2)) ?\^_))
                     ;; Only printable characters on the line, [:cntrl:] does
                     ;; not work?!
                     (not (re-search-forward "[^[:print:]]" eol t))
                     ;; Matches all regexps
                     (seq-every-p
                      (lambda (r)
                        (goto-char bol)
                        (ignore-errors (re-search-forward r eol t)))
                      (cdr regexps))
                     ;; Find node beginning
                     (progn
                       (goto-char bol)
                       (if (search-backward "\n\^_" nil 'move)
                           (forward-line 2)
                         (when (looking-at "\^_")
                           (forward-line 1))))
                     ;; Node name
                     (re-search-forward "Node:[ \t]*" nil t)
                     (setq current-node
                           (buffer-substring-no-properties
                            (point)
                            (progn
                              (skip-chars-forward "^,\t\n")
                              (point))))))
                (let* ((node (format "(%s)%s" manual current-node))
                       (cand (concat
                              node ":"
                              (funcall hl (buffer-substring-no-properties bol eol)))))
                  (add-text-properties 0 (length node)
                                       (list 'consult--info-position (cons buffer bol)
                                             'face 'consult-file
                                             'consult--file-group node)
                                       cand)
                  (push cand candidates))))))))
    (nreverse candidates)))

(defun consult-info--lookup (selected candidates &rest _)
  "Lookup info position marker given SELECTED candidate from CANDIDATES list."
  (when-let ((cand (car (member selected candidates)))
             (pos (get-text-property 0 'consult--info-position cand))
             (node (get-text-property 0 'consult--file-group cand))
             (matches (consult--point-placement cand (1+ (length node)))))
    (save-restriction
      (widen)
      (cons node
            (cons
             (set-marker (make-marker) (+ (cdr pos) (car matches)) (car pos))
             (cdr matches))))))

(defun consult-info--state ()
  "Info manual preview state."
  (let ((preview (consult--jump-preview)))
    (lambda (action cand)
      (if (not cand)
          (funcall preview action nil)
        (let* ((pos (get-text-property 0 'consult--info-position cand))
               (node (get-text-property 0 'consult--file-group cand))
               (matches (consult--point-placement cand (1+ (length node))))
               (dest (+ (cdr pos) (car matches))))
        (funcall preview action
                 (cons
                  (set-marker (make-marker) dest (car pos))
                  (cdr matches)))
        (pcase action
          ('preview
           (let (Info-history Info-history-list Info-history-forward)
             (ignore-errors (Info-select-node))))
          ('return
           (info node)
           (widen)
           (goto-char dest)
           (Info-select-node)
           (run-hooks 'consult-after-jump-hook))))))))

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
            (with-current-buffer (generate-new-buffer (format "*info-preview: %s*" manual))
              (let (Info-history Info-history-list Info-history-forward)
                (Info-mode)
                (Info-find-node manual "Top")) ;; TODO noerror?
              (push (cons manual (current-buffer)) buffers)))
          (consult--read
           (consult--dynamic-collection
            (apply-partially #'consult-info--candidates buffers))
           :state (consult-info--state)
           :prompt (format "Info (%s): " (string-join manuals ", "))
           :require-match t
           :sort nil
           :history '(:input consult-info--history)
           :group #'consult--file-group
           ;; TODO fix consult-man and consult-info embark integration
           ;; We have to set (alist-get '(general . consult-man) embark-default-action-overrides)
           ;; and (alist-get '(general . consult-info) embark-default-action-overrides)
           :initial (consult--async-split-initial "")
           :lookup #'consult--lookup-member))
      (dolist (buf buffers)
        (kill-buffer (cdr buf))))))

(provide 'consult-info)
;;; consult-info.el ends here
