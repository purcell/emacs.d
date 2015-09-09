;; Settings for currently logged in user
;;      (concat user-emacs-directory "users/" user-login-name))
(setq user-settings-init-dir
      (concat user-emacs-directory "users/" user-login-name))
;;(add-to-list 'load-path (concat user-settings-dir "/init.el"))

(setq user-settings-dir
      (expand-file-name "settings/" user-settings-init-dir))

(setq user-defuns-dir
      (expand-file-name "defuns/" user-settings-init-dir))
      
(setq user-lisp-dir
      (expand-file-name "lisp/" user-settings-init-dir))      


(add-to-list 'load-path user-settings-init-dir)
(add-to-list 'load-path user-settings-dir)
(add-to-list 'load-path user-defuns-dir)
(add-to-list 'load-path user-lisp-dir)


  ;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-lisp-dir)
  (mapc 'load (directory-files user-lisp-dir nil "^[^#].*el$")))

  ;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-init-dir)
  (mapc 'load (directory-files user-settings-init-dir nil "^[^#].*el$")))


;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-defuns-dir)
  (mapc 'load (directory-files user-defuns-dir nil "^[^#].*el$")))
  

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
  




(provide 'init-local)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
