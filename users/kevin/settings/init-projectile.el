(setq projectile-other-file-alist '(("cpp" "h" "hpp" "ipp")
                                    ("ipp" "h" "hpp" "cpp")
                                    ("hpp" "h" "ipp" "cpp")
                                    ("cxx" "hxx" "ixx")
                                    ("ixx" "cxx" "hxx")
                                    ("hxx" "ixx" "cxx")
                                    ("c" "h")
                                    ("m" "h")
                                    ("mm" "h")
                                    ("h" "c" "cpp" "ipp" "hpp" "m" "mm")
                                    ("cc" "hh")
                                    ("hh" "cc")
                                    ("vert" "frag")
                                    ("frag" "vert")
                                    (nil "lock" "gpg")
                                    ("lock" "")
                                    ("gpg" "")
                                    ("js" "css" "html")))

(add-to-list 'projectile-other-file-alist '("css" "js")) ;; switch from css -> js
(add-to-list 'projectile-other-file-alist '("js" "css")) ;; switch from js -> css

(provide 'init-projectile)
