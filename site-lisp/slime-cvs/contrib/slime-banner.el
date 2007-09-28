;;; slime-banner.el -- Persistent header line and startup animation
;;
;; Authors: Helmut Eller  <heller@common-lisp.net>
;;          Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slime/contrib")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-banner)))

(defcustom slime-startup-animation (fboundp 'animate-string)
   "Enable the startup animation."
   :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
   :group 'slime-ui)

(defcustom slime-header-line-p (boundp 'header-line-format)
  "If non-nil, display a header line in Slime buffers."
  :type 'boolean
  :group 'slime-repl)

(defun slime-startup-message ()
  (when slime-header-line-p
    (setq header-line-format 
          (format "%s  Port: %s  Pid: %s"
                  (slime-lisp-implementation-type)
                  (slime-connection-port (slime-connection))
                  (slime-pid))))
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLIME " (or (slime-changelog-date) 
                                          "- ChangeLog file not found"))))
      (if slime-startup-animation
          (animate-string welcome 0 0) 
        (insert welcome)))))

(defun slime-banner-init ()
  (setq slime-repl-banner-function 'slime-startup-message))

(defun slime-banner-unload ()
  (setq slime-repl-banner-function 'slime-repl-insert-banner))

(provide 'slime-banner)
