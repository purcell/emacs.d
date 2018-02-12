`ac-etags.el' is etags/ctags completion source for auto-complete.

Sample configuration

If you change `requires' auto-complete source attribute

  (custom-set-variables
    '(ac-etags-requires 1))

  (eval-after-load "etags"
    '(progn
        (ac-etags-setup)))

  (defun my/c-mode-common-hook ()
    (add-to-list 'ac-sources 'ac-source-etags))

  (add-hook 'c-mode-common-hook 'my/c-mode-common-hook)
