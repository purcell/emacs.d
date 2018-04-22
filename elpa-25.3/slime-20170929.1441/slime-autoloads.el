;;; slime-autoloads.el --- autoload definitions for SLIME

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.
;;
;; JT@14/01/09: FIXME: This file should be auto-generated with autoload cookies.

;;; Code:

(autoload 'slime "slime"
  "Start a Lisp subprocess and connect to its Swank server." t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'slime-connect "slime"
  "Connect to a running Swank server." t)

(autoload 'slime-selector "slime"
  "Select a new by type, indicated by a single character." t)

(autoload 'hyperspec-lookup "lib/hyperspec" nil t)

(autoload 'slime-lisp-mode-hook "slime")

(autoload 'slime-scheme-mode-hook "slime")

(defvar slime-contribs nil
  "A list of contrib packages to load with SLIME.")

(autoload 'slime-setup "slime"
  "Setup some SLIME contribs.")

(define-obsolete-variable-alias 'slime-setup-contribs
  'slime-contribs "2.3.2")

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(provide 'slime-autoloads)

;;; slime-autoloads.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:

;;;### (autoloads nil nil ("slime-pkg.el" "slime-tests.el" "slime.el")
;;;;;;  (23009 25358 0 0))

;;;***
