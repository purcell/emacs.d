(require 'ibuffer)



;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Org" ;; all org-related buffers
;;                 (mode . org-mode))
;;                ("Mail"
;;                 (or ;; mail-related buffers
;;                  (mode . message-mode)
;;                  (mode . mail-mode)
;;                  ;; etc.; all your mail related modes
;;                  ))
;;                ("MyProject1"
;;                 (filename . "src/myproject1/"))
;;                ("MyProject2"
;;                 (filename . "src/myproject2/"))
;;                ("Programming" ;; prog stuff not already in MyProjectX
;;                 (or
;;                  (mode . c-mode)
;;                  (mode . perl-mode)
;;                  (mode . python-mode)
;;                  (mode . emacs-lisp-mode)
;;                  ;; etc
;;                  ))
;;                ("ERC"   (mode . erc-mode))))))

;; (add-hook 'ibuffer-mode-hook
;;   (lambda ()
;;     (ibuffer-switch-to-saved-filter-groups "default")))


(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)