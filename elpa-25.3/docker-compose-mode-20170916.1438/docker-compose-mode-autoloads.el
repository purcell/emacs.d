;;; docker-compose-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "docker-compose-mode" "docker-compose-mode.el"
;;;;;;  (23009 25346 0 0))
;;; Generated autoloads from docker-compose-mode.el

(autoload 'docker-compose-mode "docker-compose-mode" "\
Major mode to edit docker-compose files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("docker-compose.*.yml\\'" . docker-compose-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; docker-compose-mode-autoloads.el ends here
