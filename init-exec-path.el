(eval-after-load 'exec-path-from-shell
  '(progn
     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
       (add-to-list 'exec-path-from-shell-variables var))))


(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
