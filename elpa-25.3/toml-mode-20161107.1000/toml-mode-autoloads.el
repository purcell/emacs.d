;;; toml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "toml-mode" "toml-mode.el" (23009 25341 0 0))
;;; Generated autoloads from toml-mode.el

(autoload 'toml-mode "toml-mode" "\
Major mode for TOML files.

Comments start with `#'.

For details see `https://github.com/toml-lang/toml'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; toml-mode-autoloads.el ends here
