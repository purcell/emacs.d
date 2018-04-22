;;; hcl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hcl-mode" "hcl-mode.el" (23009 25346 0 0))
;;; Generated autoloads from hcl-mode.el

(autoload 'hcl-mode "hcl-mode" "\
Major mode for editing hcl configuration file

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hcl\\'" . hcl-mode))

(add-to-list 'auto-mode-alist '("\\.nomad\\'" . hcl-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hcl-mode-autoloads.el ends here
