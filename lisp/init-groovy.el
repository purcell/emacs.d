;;; init-groovy.el --- Groovy editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Setup based on the following entry https://emacs-lsp.github.io/lsp-mode/page/lsp-groovy/
;;; Instructions:
;;; 1. Install groovy: `brew install groovy'
;;; 1.5 Set Groovy home `GROOVY_HOME=/opt/homebrew/opt/groovy/libexec`
;;; 2. Compile The groovy language server
;;; https://github.com/GroovyLanguageServer/groovy-language-server
;;; 3. Move the result jar to: `~/.emacs.d/.cache/lsp/groovy-language-server-all.jar'


(when (maybe-require-package 'groovy-mode)
  (require-package 'groovy-imports)
  (add-hook 'groovy-mode-hook #'lsp)
  (add-auto-mode 'groovy-mode "\\.groovy\\'")

  ((string-equal system-type "darwin")  ; macOS
   (setq lsp-groovy-classpath "/opt/homebrew/opt/groovy/libexec/lib")))
