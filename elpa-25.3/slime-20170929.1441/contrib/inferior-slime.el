;;; inferior-slime.el --- Minor mode with Slime keys for comint buffers
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'inferior-slime)))
;;   (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode 1)))
(require 'slime)
(require 'cl-lib)

(define-minor-mode inferior-slime-mode
  "\\<slime-mode-map>\
Inferior SLIME mode: The Inferior Superior Lisp Mode for Emacs.

This mode is intended for use with `inferior-lisp-mode'. It provides a
subset of the bindings from `slime-mode'.

\\{inferior-slime-mode-map}"
  :keymap
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined))

  (slime-setup-completion)
  (setq-local tab-always-indent 'complete))

(defun inferior-slime-return ()
  "Handle the return key in the inferior-lisp buffer.
The current input should only be sent if a whole expression has been
entered, i.e. the parenthesis are matched.

A prefix argument disables this behaviour."
  (interactive)
  (if (or current-prefix-arg (inferior-slime-input-complete-p))
      (comint-send-input)
    (insert "\n")
    (inferior-slime-indent-line)))

(defun inferior-slime-indent-line ()
  "Indent the current line, ignoring everything before the prompt."
  (interactive)
  (save-restriction
    (let ((indent-start
           (save-excursion
             (goto-char (process-mark (get-buffer-process (current-buffer))))
             (let ((inhibit-field-text-motion t))
               (beginning-of-line 1))
             (point))))
      (narrow-to-region indent-start (point-max)))
    (lisp-indent-line)))

(defun inferior-slime-input-complete-p ()
  "Return true if the input is complete in the inferior lisp buffer."
  (slime-input-complete-p (process-mark (get-buffer-process (current-buffer)))
                          (point-max)))

(defun inferior-slime-closing-return ()
  "Send the current expression to Lisp after closing any open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                      (point-max))
    (while (ignore-errors (save-excursion (backward-up-list 1) t))
      (insert ")")))
  (comint-send-input))

(defun inferior-slime-change-directory (directory)
  "Set default-directory in the *inferior-lisp* buffer to DIRECTORY."
  (let* ((proc (slime-process))
	 (buffer (and proc (process-buffer proc))))
    (when buffer 
      (with-current-buffer buffer
	(cd-absolute directory)))))

(defun inferior-slime-init-keymap ()
  (let ((map inferior-slime-mode-map))
    (set-keymap-parent map slime-parent-map)
    (slime-define-keys map
      ([return]			'inferior-slime-return)
      ([(control return)]	'inferior-slime-closing-return)
      ([(meta control ?m)]	'inferior-slime-closing-return)
      ;;("\t"			'slime-indent-and-complete-symbol)
      (" "			'slime-space))))

(inferior-slime-init-keymap)

(defun inferior-slime-hook-function ()
  (inferior-slime-mode 1))

(defun inferior-slime-switch-to-repl-buffer ()
  (switch-to-buffer (process-buffer (slime-inferior-process))))

(defun inferior-slime-show-transcript (string)
  (remove-hook 'comint-output-filter-functions
	       'inferior-slime-show-transcript t)
  (with-current-buffer (process-buffer (slime-inferior-process))
    (let ((window (display-buffer (current-buffer) t)))
      (set-window-point window (point-max)))))

(defun inferior-slime-start-transcript ()
  (let ((proc (slime-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer proc)
	(add-hook 'comint-output-filter-functions 
		  'inferior-slime-show-transcript
		  nil t)))))

(defun inferior-slime-stop-transcript ()
  (let ((proc (slime-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer (slime-inferior-process))
	(run-with-timer 0.2 nil 
			(lambda (buffer) 
			  (with-current-buffer buffer
			    (remove-hook 'comint-output-filter-functions
					 'inferior-slime-show-transcript t)))
			(current-buffer))))))

(defun inferior-slime-init ()
  (add-hook 'slime-inferior-process-start-hook 'inferior-slime-hook-function)
  (add-hook 'slime-change-directory-hooks 'inferior-slime-change-directory)
  (add-hook 'slime-transcript-start-hook 'inferior-slime-start-transcript)
  (add-hook 'slime-transcript-stop-hook 'inferior-slime-stop-transcript)
  (def-slime-selector-method ?r
    "SLIME Read-Eval-Print-Loop."
    (process-buffer (slime-inferior-process))))

(provide 'inferior-slime)
