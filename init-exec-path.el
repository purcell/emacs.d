(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when *is-a-mac*
  (eval-after-load "woman"
    '(setq woman-manpath (append (list "/opt/local/man") woman-manpath)))

  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (if window-system (set-exec-path-from-shell-PATH)))


(provide 'init-exec-path)
