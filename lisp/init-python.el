;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240

;;; Plus: Great article to initialize any virtualenv with lsp
;;; https://www.mattduck.com/lsp-python-getting-started.html

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))


;;; Pyvenv to support multiple venvs using conda and pyenv.
;;; Is recommend to set up only one of them to automatically switch envs
;;; without having to worry about setting the envs before hand.
(require-package 'pyvenv)
(require 'pyvenv)
(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")          ; Default venv
  (pyvenv-workon pyvenv-workon)

  (when (fboundp 'pyvenv-track-virtualenv)
    (fmakunbound 'pyvenv-track-virtualenv))

  (defun pyvenv-track-virtualenv ()
    "Set a virtualenv as specified for the current buffer.

This is originally provided by pyvenv, but I've added a couple
of features. The most important one is that this invokes lsp
/after/ all the pyvenv activate logic has been done, which means
lsp can properly jump to definitions."
    (when (string= major-mode "python-mode")
      (cond
       (pyvenv-activate
        (when (and (not (equal (file-name-as-directory pyvenv-activate)
                               pyvenv-virtual-env))
                   (or (not pyvenv-tracking-ask-before-change)
                       (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                         pyvenv-activate pyvenv-virtual-env))))
          (pyvenv-activate pyvenv-activate)))
       (pyvenv-workon
        (when (and (not (equal pyvenv-workon pyvenv-virtual-env-name))
                   (or (not pyvenv-tracking-ask-before-change)
                       (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                         pyvenv-workon pyvenv-virtual-env-name))))
          (message "pyvenv switching from %s to %s" pyvenv-virtual-env-name pyvenv-workon)
          (pyvenv-workon pyvenv-workon))
        ;; lsp needs to run after pyvenv-workon, so we make sure it's running here rather than
        ;; in the python-mode-hook.
        (when (not lsp-mode)
          (lsp))))))

  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

(provide 'init-python)
;;; init-python.el ends here
