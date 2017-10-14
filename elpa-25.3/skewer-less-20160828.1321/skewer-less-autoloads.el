;;; skewer-less-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "skewer-less" "skewer-less.el" (23009 23615
;;;;;;  0 0))
;;; Generated autoloads from skewer-less.el

(autoload 'skewer-less-mode "skewer-less" "\
Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

For this to work properly, the lessc command must be available on
`exec-path', and `skewer' must be running.

\(fn &optional ARG)" t nil)

(autoload 'skewer-less-eval-buffer "skewer-less" "\
When skewer appears to be active, ask for a reload.

\(fn)" t nil)

(autoload 'skewer-less-eval-region "skewer-less" "\
Process the region from BEG to END with \"lessc\", and pass it to `skewer-css'.

\(fn BEG END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; skewer-less-autoloads.el ends here
