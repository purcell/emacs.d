;;; haskell-repl.el --- REPL evaluation -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'haskell-interactive-mode)
(require 'haskell-collapse)

(defun haskell-interactive-handle-expr ()
  "Handle an inputted expression at the REPL."
  (let ((expr (haskell-interactive-mode-input)))
    (if (string= "" (replace-regexp-in-string " " "" expr))
        ;; Just make a new prompt on space-only input
        (progn
          (goto-char (point-max))
          (insert "\n")
          (haskell-interactive-mode-prompt))
      (when (haskell-interactive-at-prompt)
        (cond
         ;; If already evaluating, then the user is trying to send
         ;; input to the REPL during evaluation. Most likely in
         ;; response to a getLine-like function.
         ((and (haskell-process-evaluating-p (haskell-interactive-process))
               (= (line-end-position) (point-max)))
          (goto-char (point-max))
          (let ((process (haskell-interactive-process))
                (string (buffer-substring-no-properties
                         haskell-interactive-mode-result-end
                         (point))))
            ;; here we need to go to end of line again as evil-mode
            ;; might have managed to put us one char back
            (goto-char (point-max))
            (insert "\n")
            ;; Bring the marker forward
            (setq haskell-interactive-mode-result-end
                  (point-max))
            (haskell-process-set-sent-stdin process t)
            (haskell-process-send-string process string)))
         ;; Otherwise we start a normal evaluation call.
         (t (setq haskell-interactive-mode-old-prompt-start
                  (copy-marker haskell-interactive-mode-prompt-start))
            (set-marker haskell-interactive-mode-prompt-start (point-max))
            (haskell-interactive-mode-history-add expr)
            (haskell-interactive-mode-do-expr expr)))))))

(defun haskell-interactive-mode-do-expr (expr)
  (cond
   ((string-match "^:present " expr)
    (haskell-interactive-mode-do-presentation (replace-regexp-in-string "^:present " "" expr)))
   (t
    (haskell-interactive-mode-run-expr expr))))

(defun haskell-interactive-mode-run-expr (expr)
  "Run the given expression."
  (let ((session (haskell-interactive-session))
        (process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list session process expr 0)
      :go (lambda (state)
            (goto-char (point-max))
            (insert "\n")
            (setq haskell-interactive-mode-result-end
                  (point-max))
            (haskell-process-send-string (cadr state)
                                         (haskell-interactive-mode-multi-line (cl-caddr state)))
            (haskell-process-set-evaluating (cadr state) t))
      :live (lambda (state buffer)
              (unless (and (string-prefix-p ":q" (cl-caddr state))
                           (string-prefix-p (cl-caddr state) ":quit"))
                (let* ((cursor (cl-cadddr state))
                       (next (replace-regexp-in-string
                              haskell-process-prompt-regex
                              ""
                              (substring buffer cursor))))
                  (haskell-interactive-mode-eval-result (car state) next)
                  (setf (cl-cdddr state) (list (length buffer)))
                  nil)))
      :complete
      (lambda (state response)
        (haskell-process-set-evaluating (cadr state) nil)
        (unless (haskell-interactive-mode-trigger-compile-error state response)
          (haskell-interactive-mode-expr-result state response)))))))

(defun haskell-interactive-mode-expr-result (state response)
  "Print the result of evaluating the expression."
  (let ((response
         (with-temp-buffer
           (insert response)
           (haskell-interactive-mode-handle-h)
           (buffer-string))))
    (when haskell-interactive-mode-eval-mode
      (unless (haskell-process-sent-stdin-p (cadr state))
        (haskell-interactive-mode-eval-as-mode (car state) response))))
  (haskell-interactive-mode-prompt (car state)))

(defun haskell-interactive-mode-eval-as-mode (session text)
  "Insert TEXT font-locked according to `haskell-interactive-mode-eval-mode'."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (let ((inhibit-read-only t))
      (delete-region (1+ haskell-interactive-mode-prompt-start) (point))
      (goto-char (point-max))
      (insert (haskell-fontify-as-mode text
                                       haskell-interactive-mode-eval-mode))
      (when haskell-interactive-mode-collapse
        (haskell-hide-toggle)))))

(provide 'haskell-repl)
