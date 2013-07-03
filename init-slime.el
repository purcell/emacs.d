(require-package 'slime)

;; There are 2 versions of Slime available as packages. The 2010* version
;; is for Clojure compatibility, and uses separate packages for slime-fuzzy
;; and slime-repl. The other version is the latest available, which
;; contains a complete "contrib" dir.
(let ((slime-contrib-dir (concat (directory-of-library "slime") "/contrib")))
  (if (file-directory-p slime-contrib-dir)
      ;; Ensure contrib dir is ahead of any slime-{fuzzy,repl} package
      (add-to-list 'load-path slime-contrib-dir)
    (require-package 'slime-fuzzy)
    (require-package 'slime-repl)))

(require-package 'ac-slime)
(require-package 'hippie-expand-slime)


(autoload 'slime-fuzzy-init "slime-fuzzy" "" nil)
(after-load 'slime-fuzzy
  (require 'slime-repl))

(defun sanityinc/set-up-slime-repl-auto-complete ()
  "Bind TAB to `indent-for-tab-command', as in regular Slime buffers."
  (local-set-key (kbd "TAB") 'indent-for-tab-command))

(after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl slime-fuzzy))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
  (add-hook 'slime-mode-hook 'set-up-slime-ac))

(after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  (add-hook 'slime-repl-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)
  (add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'sanityinc/set-up-slime-repl-auto-complete)
  (after-load 'auto-complete
    (add-to-list 'ac-modes 'slime-repl-mode)))


(provide 'init-slime)
