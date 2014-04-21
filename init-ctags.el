;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when *is-a-mac*
  ; Mac's default ctags does not support -e option
  ; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  (setq ctags-command "/usr/local/bin/ctags -e -R ") ;; the best option is to install latest ctags from sf.net
  )

(provide 'init-ctags)
