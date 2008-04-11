;;; slime-fancy.el --- Load and init some fancy SLIME contribs
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
;;
;; We load all SLIME contribs that are currently working,
;; and which only "upgrade" the behavior of SLIME in some way.
;; This includes:
;;   * Adding new commands, keybindings, menu items
;;   * Making things clickable that would otherwise be just plain text

;; Better arglist display, can be turned off by customization.
(require 'slime-autodoc)
(slime-autodoc-init)

;; Adds new commands and installs compound-prefix-completion as
;; default completion command.  Behaves similar to standard Emacs
;; completion, unless dashes are present. --mkoeppe
(require 'slime-c-p-c)
(slime-c-p-c-init)

;; Just adds commands.  (Well, shadows commands in lisp-mode-map)
(require 'slime-editing-commands)
(slime-editing-commands-init)

;; Makes the inspector fancier.  (Once loaded, can't be turned off.)
(require 'slime-fancy-inspector)

;; Just adds the command C-c M-i.  We do not make fuzzy completion the
;; default completion invoked by TAB. --mkoeppe
(require 'slime-fuzzy)
(slime-fuzzy-init)

;; Do not activate slime-highlighting-edits by default, as it's easier
;; to explictly activate it (if a user really wants it) than to explictly
;; deactivate it once it got globally enabled. -TCR.
(require 'slime-highlight-edits)
;(slime-highlight-edits-init)

;; Load slime-presentations even though they seem to be a
;; controversial feature, as they can be easily turned off by
;; customizing swank:*record-repl-results*. --mkoeppe
(require 'slime-presentations)
(slime-presentations-init)

;;; Do not load slime-presentation-streams, as this is an experimental
;;; feature that installs patches into some Lisps. --mkoeppe
;;(require 'slime-presentation-streams)

(require 'slime-scratch)
(slime-scratch-init)

;;; Do not load slime-typeout-frame, as simply loading causes display of a
;;; typeout frame, which cannot be turned off. --mkoeppe
;;(require 'slime-typeout-frame)

;; Just adds commands.
(when (locate-library "tree-widget")
  (require 'slime-xref-browser))

;; Puts clickable references to documentation into SBCL errors.
(require 'slime-references)
(slime-references-init)

(provide 'slime-fancy)
