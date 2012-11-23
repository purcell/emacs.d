(require 'ctags)
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
(setq tags-revert-without-query t)
(global-set-key (kbd "M-.")  'ctags-search)
(when *is-a-mac*
  ; Mac's default ctags does not support -e option
  ; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  (setq ctags-command "/usr/local/bin/ctags -e -R ") ;; the best option is to install latest ctags from sf.net
  )

(provide 'init-ctags)
