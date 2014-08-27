;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Enable line number.
(global-linum-mode t)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "GOPATH" "/usr/local/go")
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin" ":" "/usr/local/go/bin" ":" "/Users/justin/bin"))

;; use Meta-Return keys on Mac OS X to toggle maxmize full screen.
(when *is-a-mac*
  (global-set-key (kbd "<M-RET>") 'toggle-frame-fullscreen))
;; use C-tab to do auto complete
(global-set-key [C-tab] 'auto-complete)

;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook
  (lambda () (setq truncate-lines nil)))

;; Enable yasnippet globally.
(yas-global-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

(provide 'init-local)
;;; init-local.el ends here
