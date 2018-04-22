;;; marshal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "marshal" "marshal.el" (23009 21884 0 0))
;;; Generated autoloads from marshal.el

(autoload 'marshal "marshal" "\


\(fn OBJ TYPE)" nil nil)

(autoload 'unmarshal "marshal" "\


\(fn OBJ BLOB TYPE)" nil nil)

(autoload 'marshal-defclass "marshal" "\


\(fn NAME SUPERCLASS SLOTS &rest OPTIONS-AND-DOC)" nil t)

(function-put 'marshal-defclass 'lisp-indent-function '2)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; marshal-autoloads.el ends here
