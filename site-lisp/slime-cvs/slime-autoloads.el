;;; slime-autoloads.el --- autoload definitions for SLIME

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.

;;; Code:

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

(defun slime-setup (&rest options)
  "Setup Emacs so that lisp-mode buffers always use SLIME.
OPTIONS is a keyword list (&key AUTODOC TYPEOUT-FRAME HIGHLIGHT-EDITS):
AUTODOC and HIGHLIGHT-EDITS enable `slime-autodoc-mode' resp.
`slime-highlight-edits-mode'.
If TYPEOUT-FRAME is true, the SLIME will use the typeout window."
  (when (member 'lisp-mode slime-lisp-modes)
    (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))
  (when (member 'scheme-mode slime-lisp-modes)
    (add-hook 'scheme-mode-hook 'slime-scheme-mode-hook))
  (when (plist-get options :typeout-frame)
    (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame))
  (setq slime-use-autodoc-mode (plist-get options :autodoc))
  (setq slime-use-highlight-edits-mode (plist-get options :highlight-edits)))

(provide 'slime-autoloads)

;;; slime-autoloads.el ends here
