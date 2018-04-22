;;; coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "coffee-mode" "coffee-mode.el" (23009 23272
;;;;;;  0 0))
;;; Generated autoloads from coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.cson\\'" . coffee-mode))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; coffee-mode-autoloads.el ends here
