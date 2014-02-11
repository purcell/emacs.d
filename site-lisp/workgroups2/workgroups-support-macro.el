;;; workgroups-support-macro.el --- Generates (de)serialize functions
;;; Commentary:
;; Copyright (C) Sergey Pashinin
;; Author: Sergey Pashinin <sergey@pashinin.com>
;;; Code:

(defun wg-get-value (arg)
  "Get a value of ARG if it exists."
  (if (boundp `,arg) (eval arg)))

(defmacro wg-support (mode pkg params)
  "Macro to create (de)serialization functions for a buffer.
You need to save/restore a specific MODE which is loaded from a
package PKG.  In PARAMS you give local variables to save and a
deserialization function."
  `(let ((mode-str (symbol-name ,mode))
         (args ,params))

     (eval `(defun ,(intern (format "wg-deserialize-%s-buffer" mode-str)) (buffer)
              "DeSerialization function created with `wg-support'.
Gets saved variables and runs code to restore a BUFFER."
              (when (require ',,pkg nil 'noerror)
                (wg-dbind (this-function variables) (wg-buf-special-data buffer)
                  (let ((default-directory (car variables))
                        (df (cdr (assoc 'deserialize ',,params)))
                        (user-vars (car (cdr variables))))
                    (if df (funcall df buffer user-vars))
                    (current-buffer)
                    )))))

     (eval `(defun ,(intern (format "wg-serialize-%s-buffer" mode-str)) (buffer)
              "Serialization function created with `wg-support'.
Saves some variables to restore a BUFFER later."
              (when (get-buffer buffer)
                (with-current-buffer buffer
                  (when (eq major-mode ',,mode)
                    (let ((sf (cdr (assoc 'serialize ',,params)))
                          (save (cdr (assoc 'save ',,params))))
                      (list ',(intern (format "wg-deserialize-%s-buffer" mode-str))
                            (wg-take-until-unreadable (list default-directory
                                                            (if sf (funcall sf buffer)
                                                              (if save (mapcar 'wg-get-value save)))
                                                            )))))))))
     ;; Maybe change a docstring for functions
     ;;(put (intern (format "wg-serialize-%s-buffer" (symbol-name mode)))
     ;;     'function-documentation
     ;;     (format "A function created by `wg-support'."))

     ;; Add function to `wg-special-buffer-serdes-functions' variable
     (eval `(add-to-list 'wg-special-buffer-serdes-functions
                         ',(intern (format "wg-serialize-%s-buffer" mode-str)) t))
     ))

(provide 'workgroups-support-macro)
;;; workgroups-support-macro.el ends here
