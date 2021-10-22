;;; my-python.el --- My Python configs
;;; Commentary:
;;; Code:


;; Python3 as default interpreter
(setq py-python-command "/usr/bin/python3")

;; Python LSP
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook 'display-line-numbers-mode)
(require 'dap-python)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(require 'dap-python)

(dap-register-debug-template
 "DevAutomator Testing"
 (list :type "python"
       :args ""
       :cwd nil
       :program "/home/nzsn/WorkSpaces/Codes/DevAutomator/src/lang/TestCases"
       :module "pytest"
       :request "launch"
       :name "DevAutomator Testing"))

(require-package 'flycheck-mypy)
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)

(custom-set-variables
 '(flycheck-python-pycompile-executable "python3"))
(custom-set-variables
 '(flycheck-python-flake8-executable "python3"))
(custom-set-variables
 '(flycheck-python-pylint-executable "python3"))

(flycheck-add-next-checker 'python-pycompile 'python-mypy)
(flycheck-add-next-checker 'python-flake8 'python-mypy)

;;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
;;(add-to-list 'flycheck-disabled-checkers 'python-pylint)


(provide 'my-python)
;;; my-python.el ends here
