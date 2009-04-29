(require 'twit)

(add-hook 'twit-new-tweet-hook 'twit-todochiku)
(setq twit-base-url "https://twitter.com") ;; use https to keep password secure
(setq twit-show-user-images t)

(provide 'init-twitter)
