(defun gtags-ext-produce-tags-if-needed (dir)
   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory))
        (cd dir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'"))
  )

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun gtags-ext-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (gtags-ext-produce-tags-if-needed (read-directory-name
                            "gtags: top of source tree:" default-directory)))

(defun gtags-ext-add-gtagslibpath (libdir &optional del)
  "add external library directory to environment variable GTAGSLIBPATH.\ngtags will can that directory if needed.\nC-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
  (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
      ;; create tags
      (let ((olddir default-directory))
        (cd libdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)
        )
    )
  (setq libdir (directory-file-name libdir)) ;remove final slash
  (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
  (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
  (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":")))
  )

(defun gtags-ext-print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH"))
  )

(provide 'init-gtags)
