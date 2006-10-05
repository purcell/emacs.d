;;; speedbar-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (rpm) "rpm" "rpm.el" (17091 25106))
;;; Generated autoloads from rpm.el

(autoload (quote rpm) "rpm" "\
Red Hat Package Management in Emacs." t nil)

;;;***

;;;### (autoloads (gud-speedbar-buttons) "sb-gud" "sb-gud.el" (17091
;;;;;;  25106))
;;; Generated autoloads from sb-gud.el

(autoload (quote gud-speedbar-buttons) "sb-gud" "\
Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode." nil nil)

;;;***

;;;### (autoloads (Info-speedbar-buttons Info-speedbar-browser) "sb-info"
;;;;;;  "sb-info.el" (17091 25106))
;;; Generated autoloads from sb-info.el

(autoload (quote Info-speedbar-browser) "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode." t nil)

(autoload (quote Info-speedbar-buttons) "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for." nil nil)

(eval-after-load "info" (quote (require (quote sb-info))))

;;;***

;;;### (autoloads (rmail-speedbar-buttons) "sb-rmail" "sb-rmail.el"
;;;;;;  (17091 25106))
;;; Generated autoloads from sb-rmail.el

(autoload (quote rmail-speedbar-buttons) "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder." nil nil)

;;;***

;;;### (autoloads (w3-speedbar-buttons) "sb-w3" "sb-w3.el" (17091
;;;;;;  25106))
;;; Generated autoloads from sb-w3.el

(autoload (quote w3-speedbar-buttons) "sb-w3" "\
Create speedbar buttons for the current web BUFFER displayed in w3 mode." nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "speedbar.el" (17091 25106))
;;; Generated autoloads from speedbar.el

(defalias (quote speedbar) (quote speedbar-frame-mode))

(autoload (quote speedbar-frame-mode) "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted." t nil)

(autoload (quote speedbar-get-focus) "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame." t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; speedbar-loaddefs.el ends here
