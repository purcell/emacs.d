(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq auto-mode-alist
      (append '(("\\.py$" . python-mode)
		("SConstruct$" . python-mode)
		("SConscript$" . python-mode))
              auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


;;----------------------------------------------------------------------------
;; Pymacs and Rope for Python
;;----------------------------------------------------------------------------
;; See http://edward.oconnor.cx/2008/02/ropemacs
(setq pymacs-load-path
      (list (concat el-get-dir "ropemacs")))
(add-hook 'python-mode-hook
          (lambda ()
            (require 'pymacs)
            (pymacs-load "ropemacs" "rope-")))


;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(require 'flymake-python)

(add-hook 'python-mode-hook 'flymake-python-load)



(provide 'init-python-mode)
