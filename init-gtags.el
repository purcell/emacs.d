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
  ;; @see http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
  (defun djcb-gtags-create-or-update ()
    "create or update the gnu global tag file"
    (interactive)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
            (topdir (read-directory-name
                      "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)) ; restore
      ;;  tagfile already exists; update it
      (shell-command "global -u && echo 'updated tagfile'")))
  )

(provide 'init-gtags)
