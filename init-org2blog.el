; @see http://blog.nethazard.net/post-to-wordpress-blogs-with-emacs-org-mode/
(require 'org2blog-autoloads)
; setup proxy
;(setq url-proxy-services '(("http" . "127.0.0.1:8580"))) ; fr*egate
;(setq url-proxy-services '(("http" . "127.0.0.1:8000"))) ; gae
;(setq url-proxy-services '(("http" . "127.0.0.1:8118"))) ; privoxy');
(setq org2blog/wp-blog-alist
      '(("wp"
         :url "http://emacsguru.wordpress.com/xmlrpc.php"
         :username "emacsguru"
         :default-title ""
         :default-categories ("Linux")
         :tags-as-categories nil
         )
        ("my"
         :url "http://blog.binchen.org/xmlrpc.php"
         :username "chenbin0"
         :default-title ""
         :default-categories ("Linux")
         :tags-as-categories nil
         )
        ))
(provide 'init-org2blog)
