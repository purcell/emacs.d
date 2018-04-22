;;; hippie-expand-slime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hippie-expand-slime" "hippie-expand-slime.el"
;;;;;;  (23009 25363 0 0))
;;; Generated autoloads from hippie-expand-slime.el

(autoload 'try-expand-slime "hippie-expand-slime" "\
Simple slime completion function for `hippie-expand'.

\(fn OLD)" nil nil)

(autoload 'try-expand-slime-fuzzy "hippie-expand-slime" "\
Fuzzy slime completion function for `hippie-expand'.

\(fn OLD)" nil nil)

(autoload 'set-up-slime-hippie-expand "hippie-expand-slime" "\
Add an optionally-fuzzy slime completion function to the front of
`hippie-expand-try-functions-list' for the current buffer.

\(fn &optional FUZZY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hippie-expand-slime-autoloads.el ends here
