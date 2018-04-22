(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-presentation-streams
  "Streams that allow attaching object identities to portions of
   output."
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Helmut Eller  <heller@common-lisp.net>")
  (:license "GPL")
  (:on-load
   (add-hook 'slime-connected-hook 'slime-presentation-streams-on-connected))
  (:swank-dependencies swank-presentation-streams))

(defun slime-presentation-streams-on-connected ()
  (slime-eval `(swank:init-presentation-streams)))

(provide 'slime-presentation-streams)
