;;; Package --- Cscope for source code cross-reference tool
(require-package 'xcscope)
(require 'xcscope)
(cscope-setup)

;; See https://github.com/dkogan/xcscope.el
;; xcscope.el can be used as a front-end to GNU Global.
;(setq cscope-program “gtags-cscope”)

;; emacs will call C-c s I update Index will mismatch cscope -q
;(setq cscope-option-do-not-update-database "t")

(provide 'init-xcscope)
