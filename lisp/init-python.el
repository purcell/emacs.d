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

;;; lsp-mode setup
(use-package lsp-ui
  :disabled)
(require-package 'lsp-mode)
(use-package lsp-mode
  :demand t
  :config
  (defun md/lsp-setup()
    ;; recommended by LSP docs for performance
    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    (lsp-enable-imenu)
    (setq
     lsp-auto-configure t
     lsp-enable-dap-auto-configure nil ; Don't try to auto-enable dap: this creates a lot of binding clashes
     lsp-auto-guess-root t ; Uses projectile to guess the project root.
     lsp-before-save-edits t
     lsp-eldoc-enable-hover t
     lsp-eldoc-render-all nil
     lsp-completion-enable t
     lsp-completion-show-detail t
     lsp-completion-show-kind t
     lsp-enable-file-watchers t
     lsp-file-watch-threshold 100
     lsp-enable-folding t
     lsp-enable-imenu t
     lsp-enable-indentation t
     lsp-enable-links t
     lsp-clients-python-library-directories `("/usr/" ,(expand-file-name "~/.virtualenvs")) ; This seems appropriate
     lsp-enable-on-type-formatting nil
     lsp-enable-snippet nil ;; Not supported by company capf, which is the recommended company backend
     lsp-enable-symbol-highlighting nil
     lsp-enable-text-document-color nil
     lsp-enable-xref t
     lsp-flycheck-live-reporting nil
     lsp-idle-delay 0.5
     lsp-imenu-show-container-name t
     lsp-imenu-sort-methods '(position kind name)
     lsp-pyls-plugins-flake8-enabled t
     lsp-signature-auto-activate t
     lsp-signature-render-documentation t
     lsp-signature-doc-lines 10)
    (lsp-register-custom-settings
     '(("pyls.plugins.pyls_mypy.enabled" t t)
       ("pyls.plugins.pyls_mypy.live_mode" nil t)
       ("pyls.plugins.pyls_black.enabled" t t)
       ("pyls.plugins.pyls_isort.enabled" t t)

       ;; Disable these as they're duplicated by flake8
       ("pyls.plugins.pycodestyle.enabled" nil t)
       ("pyls.plugins.mccabe.enabled" nil t)
       ("pyls.plugins.pyflakes.enabled" nil t))))
  :hook
  ;; NOTE: we don't have a python-mode hook - it gets handled by pyvenv-track-virtualenv
  (;;(js-mode . lsp)
   ;;(Web-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-before-initialize . md/lsp-setup)))


;;; Pyvenv to support multiple venvs using conda and pyenv.
;;; Is recommend to set up only one of them to automatically switch envs
;;; without having to worry about setting the envs before hand.
(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
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
