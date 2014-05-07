;;;Package --- complete anything” and is a modular in-buffer completion mechanism.
(require-package 'company)
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; does not matter, I never use this hotkey
;(global-set-key (kbd "C-c o") 'company-complete)

(setq company-require-match nil)

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            )))

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-cmake)
     ;; I donot like the downcase code in company-dabbrev
     (setq company-backends (delete 'company-dabbrev company-backends))
     (setq company-begin-commands '(self-insert-command))
     (setq company-idle-delay 0.2)
     ))

;; Yasnippet integration
;; Company interferes with Yasnippet’s native behaviour
;; @Here’s a quick fix: http://gist.github.com/265010
(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
  (first (yas/current-key)))

(define-key company-active-map "\t" 'company-yasnippet-or-completion)

(provide 'init-company)
