
;;;### (autoloads (ecb-byte-compile ecb-minor-mode ecb-activate)
;;;;;;  "ecb" "ecb.el" (17425 12192))
;;; Generated autoloads from ecb.el

(autoload (quote ecb-activate) "ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument." t nil)

(autoload (quote ecb-minor-mode) "ecb" "\
Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}" t nil)

(autoload (quote ecb-byte-compile) "ecb" "\
Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist." t nil)

;;;***

;;;### (autoloads (ecb-show-help) "ecb-help" "ecb-help.el" (17383
;;;;;;  35172))
;;; Generated autoloads from ecb-help.el

(autoload (quote ecb-show-help) "ecb-help" "\
Shows the online help of ECB in Info or HTML-format.
The format depends on the setting in `ecb-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help (Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `ecb-help-info-start-file' and `ecb-help-html-start-file'!

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included." t nil)

;;;***

;;;### (autoloads nil "ecb-util" "ecb-util.el" (17423 2734))
;;; Generated autoloads from ecb-util.el

(defconst ecb-running-xemacs (featurep (quote xemacs)))

;;;***

(provide 'ecb-autoloads)
