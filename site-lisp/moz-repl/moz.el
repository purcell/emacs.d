;;; moz.el --- Lets current buffer interact with inferior mozilla.

;; Copyright (C) 2006 by Massimiliano Mirra
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Author: Massimiliano Mirra, <bard [at] hyperstruct [dot] net>

;;; Code:

(require 'comint)

(define-minor-mode moz-minor-mode
  "Toggle Mozilla mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Mozilla mode is enabled, some commands become available to
send current code area (as understood by c-mark-function) or
region or buffer to an inferior mozilla process (which will be
started as needed)."
  nil
  " Moz"
  '(("\C-c\C-s" . run-mozilla)
    ("\C-\M-x"  . moz-send-defun)
    ("\C-c\C-c" . moz-send-defun-and-go)
    ("\C-c\C-r" . moz-send-region)
    ("\C-c\C-l" . moz-save-buffer-and-send)))

(defalias 'run-mozilla 'inferior-moz-switch-to-mozilla)

(defvar moz-repl-name "repl"
  "The current name of the repl.")

(defvar moz-input-separator "\n--end-remote-input\n")

(defvar moz-repl-host "localhost")

(defvar moz-repl-port 4242)

(defun moz-temporary-file ()
  (if (and moz-temporary-file
           (file-readable-p moz-temporary-file))
      moz-temporary-file
    (setq moz-temporary-file (make-temp-file "emacs-mozrepl"))))

(defun moz-send-region (start end)
  (interactive "r")
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                              moz-repl-name ".setenv('printPrompt', false); "
                              moz-repl-name ".setenv('inputMode', 'multiline'); "
                              "undefined; \n"))
  ;; Give the previous line a chance to be evaluated on its own.  If
  ;; it gets concatenated to the following ones, we are doomed.
  (sleep-for 0 1)
  (comint-send-region (inferior-moz-process)
                      start end)
  (comint-send-string (inferior-moz-process)
                      "\n--end-remote-input\n")
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".popenv('inputMode', 'printPrompt'); "
                              "undefined; \n"))
  (comint-send-string (inferior-moz-process)
                      "\n--end-remote-input\n")
  (display-buffer (process-buffer (inferior-moz-process))))

(defun moz-send-defun ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (moz-send-region (point) (mark))))

(defun moz-send-defun-and-go ()
  (interactive)
  (moz-send-defun)
  (inferior-moz-switch-to-mozilla))

(defun moz-save-buffer-and-send ()
  (interactive)
  (save-buffer)
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                              moz-repl-name ".setenv('inputMode', 'line'); "
                              moz-repl-name ".setenv('printPrompt', false); undefined; "))
  (comint-send-string (inferior-moz-process)
                      (concat moz-repl-name ".load('file://localhost/" (buffer-file-name) "');\n"
                              moz-repl-name ".popenv('inputMode', 'printPrompt'); undefined;\n"))
  (display-buffer (process-buffer (inferior-moz-process))))

;;; Inferior Mode

(defvar inferior-moz-buffer nil
  "The buffer in which the inferior process is running.")

(define-derived-mode inferior-moz-mode comint-mode "Inf-Mozilla"
  "Major mode for interacting with a Mozilla browser."
  (setq comint-input-sender 'inferior-moz-input-sender)
  (define-key inferior-moz-mode-map "\C-cc" (lambda () (interactive) (insert moz-repl-name ".")))
  (add-hook 'comint-output-filter-functions 'inferior-moz-track-repl-name nil t))
            
(defun inferior-moz-track-repl-name (comint-output)
  (when (string-match "\\(\\w+\\)> $" comint-output)
    (setq moz-repl-name (match-string 1 comint-output))))

(defun inferior-moz-self-insert-or-repl-name ()
  (interactive)
  (if (looking-back "\\(\\w+\\)> $")
      (insert moz-repl-name ".")
    (insert last-command-char)))

(defun inferior-moz-input-sender (proc string)
  "Custom function to send input with comint-send-input.
Instead of sending input and newline separately like in
comint-simple-send, here we *first* concatenate input and
newline, then send it all together.  This prevents newline to be
interpreted on its own."
  (comint-send-string proc (concat string "\n")))
    
(defun inferior-moz-switch-to-mozilla (arg)
  "Show the inferior mozilla buffer.  Start the process if needed."
  (interactive "P")
  (when arg
    (setq moz-repl-host (read-string "Host: " "localhost"))
    (setq moz-repl-port (read-number "Port: " 4242)))
  (pop-to-buffer (process-buffer (inferior-moz-process)))
  (goto-char (process-mark (inferior-moz-process))))

(defun inferior-moz-process ()
  "Return inferior mozilla process.  Start it if necessary."
  (or (if (buffer-live-p inferior-moz-buffer)
          (get-buffer-process inferior-moz-buffer))
      (progn
        (inferior-moz-start-process)
        (inferior-moz-process))))

(defun inferior-moz-start-process ()
  "Start an inferior mozilla process.
It runs the hook `inferior-moz-hook' after starting the process
and setting up the inferior-mozilla buffer."
  (interactive)
  (setq inferior-moz-buffer
        (apply 'make-comint "Moz" (cons moz-repl-host moz-repl-port) nil nil))
  (sleep-for 0 100)
  (with-current-buffer inferior-moz-buffer
    (inferior-moz-mode)
    (run-hooks 'inferior-moz-hook)))

(provide 'moz)

;;; moz.el ends here