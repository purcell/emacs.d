(require 'slime)
(require 'cl-lib)

(define-slime-contrib slime-listener-hooks
  "Enable slime integration in an application'w event loop"
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>, R. Mattes <rm@seid-online.de>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-listener-hooks))

(provide 'slime-listener-hooks)
