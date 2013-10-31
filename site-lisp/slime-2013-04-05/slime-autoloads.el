;;; slime-autoloads.el --- autoload definitions for SLIME

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.

;;; Code:

(unless (featurep 'slime)
  (autoload 'slime "slime"
            "Start a Lisp subprocess and connect to its Swank server." t) 

  (autoload 'slime-mode "slime"
            "SLIME: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

  (autoload 'slime-connect "slime"
            "Connect to a running Swank server." t)

  (autoload 'hyperspec-lookup "hyperspec" nil t)

  (autoload 'slime-lisp-mode-hook "slime")
  (autoload 'slime-scheme-mode-hook "slime")

  (defvar slime-lisp-modes '(lisp-mode))
  (defvar slime-setup-contribs nil
    "List of contribst to load.
Modified my slime-setup.")

  (defun slime-setup (&optional contribs)
    "Setup Emacs so that lisp-mode buffers always use SLIME.
CONTRIBS is a list of contrib packages to load."
    (when (member 'lisp-mode slime-lisp-modes)
      (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))
    (setq slime-setup-contribs contribs)
    (add-hook 'slime-load-hook 'slime-setup-contribs))

  (provide 'slime-autoloads))

;;; slime-autoloads.el ends here
