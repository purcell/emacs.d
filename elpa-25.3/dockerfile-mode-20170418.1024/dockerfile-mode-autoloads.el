;;; dockerfile-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dockerfile-mode" "dockerfile-mode.el" (23009
;;;;;;  25345 0 0))
;;; Generated autoloads from dockerfile-mode.el

(autoload 'dockerfile-build-buffer "dockerfile-mode" "\
Build an image based upon the buffer

\(fn IMAGE-NAME)" t nil)

(autoload 'dockerfile-build-no-cache-buffer "dockerfile-mode" "\
Build an image based upon the buffer without cache

\(fn IMAGE-NAME)" t nil)

(autoload 'dockerfile-mode "dockerfile-mode" "\
A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dockerfile-mode-autoloads.el ends here
