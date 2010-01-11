;;(setq load-path (cons (expand-file-name "/usr/local/share/emacs/site-lisp/distel") load-path))
;;(defun my-erlang-load-hook ()
;; (setq erlang-root-dir "/opt/otp/lib/erlang"))
;;(add-hook 'erlang-load-hook 'my-erlang-load-hook)
(setq erlang-root-dir "/opt/local/lib/erlang")
(require 'erlang-start)
;;(require 'distel)
;;(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook))


(provide 'init-erlang)
