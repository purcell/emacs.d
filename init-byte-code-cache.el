(setq byte-compile-warnings t)
(setq byte-cache-directory nil)
(setq bcc-enabled-on-save t)
(setq bcc-blacklist '("/\\.recentf$" "/history$" "/\\.ecb-user-layouts\\.el$" "/\\.session$"
                      "/\\.emacs-project$" "/\\.emacs\\.desktop$" "/custom\\.el$" "/init\\.el$"
                      "/\\.ido\\.last$" "/\\.ecb-tip-of-day\\.el$" "/\\.viper$" "/\\.recentf$"))
(require 'byte-code-cache)


(provide 'init-byte-code-cache)
