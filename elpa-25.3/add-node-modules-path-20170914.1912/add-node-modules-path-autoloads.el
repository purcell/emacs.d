;;; add-node-modules-path-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "add-node-modules-path" "add-node-modules-path.el"
;;;;;;  (23009 23281 0 0))
;;; Generated autoloads from add-node-modules-path.el

(defvar add-node-modules-path-debug nil "\
Enable verbose output when non nil.")

(autoload 'add-node-modules-path "add-node-modules-path" "\
Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; add-node-modules-path-autoloads.el ends here
