;;----------------------------------------------------------------------------
;; Pick up http_proxy & https_proxy from Mac system config using proxy-config
;; tool available from
;; http://www.cs.usyd.edu.au/~massad/project-proxy-config.html
;;----------------------------------------------------------------------------
(when (and *is-a-mac* (executable-find "proxy-config"))
  (defun mac-configured-proxy (proto)
    (string-rtrim (shell-command-to-string
                   (concat "proxy-config " (cdr (assoc-string proto '(("http" . "-h") ("https" . "-s"))))))))

  (defun extract-host-and-port (url-string)
    (if (string-match "^[a-z]+://\\([^/]+\\)" url-string)
      (match-string 1 url-string)
      url-string))

  (defun assq-delete-all-with-test (k l &optional test)
    (let ((test-func (or test #'eq)))
      (loop for entry in l
            unless (funcall test-func k (car entry))
            collect entry)))

  (defun mac-set-proxy-vars ()
    (interactive)
    (require 'url)
    (loop for proto in '("http" "https")
          for proxy = (mac-configured-proxy proto)
          do
          (setenv (concat proto "_proxy" proxy))
          (setq url-proxy-services
                (append (assq-delete-all-with-test proto url-proxy-services #'equal)
                        (if (not (equal "" proxy)) (list (cons proto (extract-host-and-port proxy)))))))
    (message "proxy variables updated")))


(provide 'init-proxies)
