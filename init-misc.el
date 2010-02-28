;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile$")
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'find-file-hooks 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(autoload 'mwe:log-keyboard-commands "mwe-log-commands"
  "Log commands executed in the current buffer" t)

(show-paren-mode 1)
(setq show-paren-delay 0)

(provide 'init-misc)