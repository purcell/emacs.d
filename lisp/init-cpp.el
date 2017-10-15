(require-package 'ggtags)
(require-package 'ycmd)
(require-package 'company-ycmd)
(require-package 'flycheck-ycmd)
(require 'ycmd-next-error)
(require-package 'cpputils-cmake)

(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'company-mode)
(set-variable 'ycmd-server-command '("python" "../vender/YCMD/ycmd"))
(set-variable 'ycmd-global-config "../vender/YCMD/cpp/ycm/.ycm_extra_conf.py")
(add-hook 'c++-mode-hook
          (lambda ()
            ;; quick compilation
                                        ;(set (make-local-variable 'compile-command)
                                        ;    (concat "g++ -std=c++11 -Wall " buffer-file-name " && ./a.out"))
            ;; (push 'company-semantic company-backends)
            (setq company-clang-arguments "-std=c++11")
            (setq flycheck-clang-language-standard "c++11")
            (setq flycheck-select-checker "c/c++-clang")
            ))
(company-ycmd-setup)
(flycheck-ycmd-setup)
(setq company-backends-c-mode-common '((company-c-headers
                                        company-ycmd
                                        company-dabbrev :with company-yasnippet)))

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
                                        ;(gtags-mode t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode 'javascript-mode 'shell-mode 'web-mode)
              (ggtags-mode 1))))
(provide 'init-cpp)
