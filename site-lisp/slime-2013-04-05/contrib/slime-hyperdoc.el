(defvar slime-old-documentation-lookup-function 
  slime-documentation-lookup-function)

(define-slime-contrib slime-hyperdoc
  "Extensible C-c C-d h."
  (:authors "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies url-http browse-url)
  (:swank-dependencies swank-hyperdoc)
  (:on-load
   (setq slime-documentation-lookup-function 'slime-hyperdoc-lookup))
  (:on-unload
   (setq slime-documentation-lookup-function 
         slime-old-documentation-lookup-function)))

;;; TODO: `url-http-file-exists-p' is slow, make it optional behaviour.

(defun slime-hyperdoc-lookup-rpc (symbol-name)
  (slime-eval-async `(swank:hyperdoc ,symbol-name)
    (lexical-let ((symbol-name symbol-name))
      #'(lambda (result)
          (slime-log-event result)
          (loop with foundp = nil
                for (doc-type . url) in result do
                (when (and url (stringp url)
                           (let ((url-show-status nil))
                             (url-http-file-exists-p url)))
                  (message "Visiting documentation for %s `%s'..."
                           (substring (symbol-name doc-type) 1)
                           symbol-name)
                  (browse-url url)
                  (setq foundp t))
                finally
                (unless foundp
                  (error "Could not find documentation for `%s'." 
                         symbol-name)))))))

(defun slime-hyperdoc-lookup (symbol-name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (if (memq :hyperdoc (slime-lisp-features))
      (slime-hyperdoc-lookup-rpc symbol-name)
      (slime-hyperspec-lookup symbol-name)))

(provide 'slime-hyperdoc)
