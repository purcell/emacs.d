;;; Workaround for the deprecation of ob-ledger
;;; https://github.com/jwiegley/org-mode/blob/master/lisp/ob-ledger.el

(require 'ob)

(defvar org-babel-default-header-args:ledger
  '((:results . "output") (:cmdline . "bal"))
  "Default arguments to use when evaluating a ledger source block.")

(defun org-babel-execute:ledger (body params)
  "Execute a block of Ledger entries with org-babel.  This function is
   called by `org-babel-execute-src-block'."
  (message "executing Ledger source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (cmdline (cdr (assoc :cmdline params)))
        (in-file (org-babel-temp-file "ledger-"))
        (out-file (org-babel-temp-file "ledger-output-")))
    (with-temp-file in-file (insert body))
    (message "%s" (concat "ledger"
                          " -f " (org-babel-process-file-name in-file)
                          " " cmdline))
    (with-output-to-string
      (shell-command (concat "ledger"
                             " -f " (org-babel-process-file-name in-file)
                             " " cmdline
                             " > " (org-babel-process-file-name out-file))))
    (with-temp-buffer (insert-file-contents out-file) (buffer-string))))

(defun org-babel-prep-session:ledger (session params)
  (error "Ledger does not support sessions"))

(provide 'ob-ledger)
