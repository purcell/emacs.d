;load cedet now, emacs23' embedded cedet is now powerful enough
;@see http://appsintheopen.com/articles/1-setting-up-emacs- \
;for-rails-development/part/6-setting-up-the-emacs-code-browser
;TIPs:
;  Having used ecb for a few year now, the only commands I ever use are:
;   * Jump to the directory window CTRL-c . gd (ctrl and c together,
;     release and press '.', release and press 'g' then 'd')
;   * Jump to the history window CTRL-c . gh
;   * Jump to the last window you were in CTRL-c . gl
;   * Jump to the method window CTRL-c . gm
;   * Jump to the first editor window CTRL-c . g1
;   The directory browser can be controlled without using the mouse too ¨C
;   just use the arrow keys and enter ¨C give it a go!
; NO embedded cedet becasue 'ecb' ask for latest cedet
(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
(setq semantic-load-turn-everything-on t)
;  * This turns on which-func support (Plus all other code helpers)
; This is a minor mode which starts semantic-complete-analyze-inline during
; idle time.
; This has additional effect of showing list of completions in tooltip
; if you leave Emacs alone for too long.
; (global-semantic-idle-completions-mode 1)
; (global-semantic-idle-summary-mode 1)
(require 'semantic-sb nil t)
(require 'semanticdb nil t)

;; customisation of modes
(defun my-cedet-hook ()
  (local-set-key "\C-c," 'semantic-ia-complete-symbol)
  (local-set-key "\C-c." 'senator-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  ;(local-set-key "\C-ct" 'eassist-switch-h-cpp)
  ;(local-set-key "\C-xt" 'eassist-switch-h-cpp)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(setq-default semanticdb-default-save-directory "~/tmp/semantic")

(custom-set-variables '(semantic-idle-scheduler-idle-time 10))

(provide 'init-cedet)

