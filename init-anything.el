(require 'anything-config)

(setq anything-sources
      '(anything-c-source-ctags
        anything-c-source-org-headline
        anything-c-source-buffers
        anything-c-source-recentf
        anything-c-source-files-in-current-dir
        anything-c-source-lacarte
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-minibuffer-history))

(setq anything-samewindow nil)
(setq anything-input-idle-delay 0.2)

(add-to-list 'anything-c-ctags-modes 'ruby-mode)

(global-set-key [\M-f10] 'anything-at-point) ;; With C-u prefix, starts with symbol at point


(provide 'init-anything)
