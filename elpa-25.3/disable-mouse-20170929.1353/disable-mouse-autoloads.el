;;; disable-mouse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "disable-mouse" "disable-mouse.el" (23009 21805
;;;;;;  0 0))
;;; Generated autoloads from disable-mouse.el

(autoload 'disable-mouse-mode "disable-mouse" "\
Disable the mouse in the current buffer.
You can still use the mouse to click into other buffers or
interact with GUI elements such as divider lines.

\(fn &optional ARG)" t nil)

(defvar global-disable-mouse-mode nil "\
Non-nil if Global Disable-Mouse mode is enabled.
See the `global-disable-mouse-mode' command
for a description of this minor mode.")

(custom-autoload 'global-disable-mouse-mode "disable-mouse" nil)

(autoload 'global-disable-mouse-mode "disable-mouse" "\
Disable the mouse globally.
Interact with GUI elements such as divider lines will also be prevented.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; disable-mouse-autoloads.el ends here
