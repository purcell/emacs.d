(load-file "lua-mode.el")

(defmacro add-trace-for (fn)
  (let ((trace-fn-name (intern (concat "trace--" (symbol-name fn)))))
    `(progn
       (defun ,trace-fn-name (&rest args)
	 (message "%s was called with: %S" #',fn args))
       (add-function :before (symbol-function #',fn) #',trace-fn-name))))


;; (add-trace-for font-lock-fontify-region)
;; (add-trace-for font-lock-unfontify-region)
;; (add-trace-for lua--propertize-multiline-bounds)
