;;; speedbar-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (rpm) "rpm" "rpm.el" (18857 63843))
;;; Generated autoloads from rpm.el

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs.

\(fn)" t nil)

;;;***

;;;### (autoloads (gud-speedbar-buttons) "sb-gud" "sb-gud.el" (18857
;;;;;;  63843))
;;; Generated autoloads from sb-gud.el

(autoload 'gud-speedbar-buttons "sb-gud" "\
Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info"
;;;;;;  "sb-info.el" (18857 63843))
;;; Generated autoloads from sb-info.el

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode.

\(fn)" t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for.

\(fn BUFFER)" nil nil)

(eval-after-load "info" '(require 'sb-info))

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "sb-rmail.el"
;;;;;;  (18857 63843))
;;; Generated autoloads from sb-rmail.el

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (w3-speedbar-buttons) "sb-w3" "sb-w3.el" (18857
;;;;;;  63843))
;;; Generated autoloads from sb-w3.el

(autoload 'w3-speedbar-buttons "sb-w3" "\
Create speedbar buttons for the current web BUFFER displayed in w3 mode.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "speedbar.el" (18857 63843))
;;; Generated autoloads from speedbar.el

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
A nil ARG means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted.

\(fn &optional ARG)" t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("bigclock.el" "dframe.el" "sb-ant.el"
;;;;;;  "sb-html.el" "sb-image.el" "sb-texinfo.el" "speedbar-load.el")
;;;;;;  (19926 52502 25450))

;;;***

(provide 'speedbar-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; speedbar-loaddefs.el ends here
