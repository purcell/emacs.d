(if (or *linux* *cygwin*)
  (if (file-exists-p "/usr/share/emacs/site-lisp/gtags.el")
    (load-file "/usr/share/emacs/site-lisp/gtags.el")
    )
  (add-hook 'gtags-mode-hook
       (lambda()
         (local-set-key "\M-t" 'gtags-find-tag) ; find a tag, definition
         (local-set-key "\M-r" 'gtags-find-rtag) ; find a tag reference
         (local-set-key "\C-t" 'gtags-pop-stack)
         (local-set-key "\M-." 'gtags-find-symbol)
         ;\M-, is used by search in dired
         (local-set-key "\M-m" 'gtags-find-file)
         (local-set-key "\M-/" 'gtags-find-pattern)
         (local-set-key "\M-;" 'gtags-find-with-grep)))
  )

(provide 'init-gtags)
