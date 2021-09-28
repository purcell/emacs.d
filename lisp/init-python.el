;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)
(require-package 'py-autopep8)
(require-package 'python-black)

(when (maybe-require-package 'anaconda-mode)
  (with-eval-after-load 'python
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                           (anaconda-mode 1))))
    (add-hook 'anaconda-mode-hook
              (lambda ()
                (anaconda-eldoc-mode (if anaconda-mode 1 0)))))
  (with-eval-after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))
  (when (maybe-require-package 'company-anaconda)
    (with-eval-after-load 'company
      (with-eval-after-load 'python
        (add-to-list 'company-backends 'company-anaconda)))))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'elpy)
  (after-load 'python
    (add-hook 'python-mode-hook 'elpy-enable)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (add-hook 'python-mode-hook 'python-black-on-save-mode)
    (add-hook 'python-mode-hook 'python-black-on-save-mode-enable-dwim)
    ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    ;; (setq py-autopep8-options '("--max-line-length=120"))
    (setq python-black-extra-args '("--line-length=120"))))
(define-coding-system-alias 'UTF-8 'utf-8)

(provide 'init-python)
;;; init-python.el ends here
