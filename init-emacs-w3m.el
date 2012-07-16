(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-home-page
      (if (file-readable-p "~/html/home.html")
        (concat "file://" (expand-file-name "~/html/home.html"))
        "http://www.google.com.au"))

(setq w3m-use-toolbar t
      ;w3m-use-tab     nil
      w3m-key-binding 'info
      )

(setq w3m-search-default-engine "g")
(eval-after-load "w3m-search" '(progn
                                 ; C-u S g RET <search term> RET
                                 (add-to-list 'w3m-search-engine-alist '("g"
                                                                         "http://www.google.com.au/search?hl=zh-CN&q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("wz"
                                                                         "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("q"
                                                                         "http://www.google.com.au/search?hl=en&q=%s+site:stackoverflow.com" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("s"
                                                                         "http://code.google.com/codesearch?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("b"
                                                                         "http://blogsearch.google.com.au/blogsearch?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("w"
                                                                         "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("d"
                                                                         "http://dictionary.reference.com/search?q=%s" utf-8))
                                 ))

(setq w3m-command-arguments       '("-F" "-cookie")
      w3m-mailto-url-function     'compose-mail
      browse-url-browser-function 'w3m
      mm-text-html-renderer       'w3m)

;bind this function to ‘a’, which is the normal w3m bookmark binding:
(eval-after-load "w3m" '(progn
                          (define-key w3m-info-like-map "A" 'delicious-post)
                          (w3m-lnum-mode 1)
                          ))

(provide 'init-emacs-w3m)
