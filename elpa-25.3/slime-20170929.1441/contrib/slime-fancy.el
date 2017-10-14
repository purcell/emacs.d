(require 'slime)

(define-slime-contrib slime-fancy
  "Make SLIME fancy."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-repl
                       slime-autodoc
                       slime-c-p-c
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       slime-fuzzy
                       slime-mdot-fu
                       slime-macrostep
                       slime-presentations
                       slime-scratch
                       slime-references
                       slime-package-fu
                       slime-fontifying-fu
                       slime-trace-dialog)
  (:on-load
   (slime-trace-dialog-init)
   (slime-repl-init)
   (slime-autodoc-init)
   (slime-c-p-c-init)
   (slime-editing-commands-init)
   (slime-fancy-inspector-init)
   (slime-fancy-trace-init)
   (slime-fuzzy-init)
   (slime-presentations-init)
   (slime-scratch-init)
   (slime-references-init)
   (slime-package-fu-init)
   (slime-fontifying-fu-init)))

(provide 'slime-fancy)
