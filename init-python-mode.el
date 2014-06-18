(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))


;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(eval-after-load 'python
  '(require 'flymake-python-pyflakes))

(add-hook 'python-mode-hook '(lambda () (flymake-python-pyflakes-load)))

(provide 'init-python-mode)
