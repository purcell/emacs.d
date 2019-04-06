(add-to-list 'exec-path "/usr/local/bin")

(getenv "PATH")
(setenv "PATH"
        (concat
         "/Library/TeX/texbin/" ":"

         (getenv "PATH")))

(provide 'init-local)
