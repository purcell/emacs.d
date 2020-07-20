;;; init-slime.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'slime)
;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there. See
;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
(mapc #'delete-file
      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

(require-package 'hippie-expand-slime)
(when (maybe-require-package 'slime-company)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space)
  (with-eval-after-load 'slime-company
    (add-to-list 'company-backends 'company-slime)))


;;; Lisp buffers

(defun sanityinc/slime-setup ()
  "Mode setup function for slime lisp buffers."
  (set-up-slime-hippie-expand))

(with-eval-after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((features '(slime-fancy slime-repl slime-fuzzy)))
    (when (require 'slime-company nil t)
      (push 'slime-company features))
    (slime-setup features))
  (add-hook 'slime-mode-hook 'sanityinc/slime-setup))


;;; REPL

(defun sanityinc/slime-repl-setup ()
  "Mode setup function for slime REPL."
  (sanityinc/lisp-setup)
  (set-up-slime-hippie-expand))

(with-eval-after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))


(provide 'init-slime)
;;; init-slime.el ends here
