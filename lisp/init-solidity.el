;;; init-solidity.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'solidity-mode)
(maybe-require-package 'solidity-flycheck)
(maybe-require-package 'company-solidity)

(add-to-list 'auto-mode-alist '("\\(\\.sol\\)'" . solidity-mode))

(setq solidity-flycheck-solc-checker-active t)
(setq solidity-solc-path "/Users/marshall/anaconda3/envs/common_py3/bin/solc")

(provide 'init-solidity)
