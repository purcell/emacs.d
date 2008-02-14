;;; Flymake Shell mode

(require 'flymake)

(defcustom flymake-shell-of-choice
  "/bin/bash"
  "Path of shell.")

(defcustom flymake-shell-arguments
  (list "-n")
  "Shell arguments to invoke syntax checking.")

(defconst flymake-allowed-shell-file-name-masks
  '(("\\.sh$" flymake-shell-init))
  "Filename extensions that switch on flymake-shell mode syntax checks.")

(defcustom flymake-shell-err-line-pattern-re
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
  "Regexp matching JavaScript error messages.")

(defun flymake-shell-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list flymake-shell-of-choice (append flymake-shell-arguments (list local-file)))))

(defun flymake-shell-load ()
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-shell-file-name-masks))
  (setq flymake-err-line-patterns (append flymake-err-line-patterns flymake-shell-err-line-pattern-re))
  (flymake-mode t)
  (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line))

(provide 'flymake-shell)

;; flymake-shell.el ends here
