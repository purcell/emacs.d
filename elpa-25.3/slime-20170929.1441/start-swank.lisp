;;; This file is intended to be loaded by an implementation to
;;; get a running swank server
;;; e.g. sbcl --load start-swank.lisp
;;;
;;; Default port is 4005

;;; For additional swank-side configurations see
;;; 6.2 section of the Slime user manual.

(load (merge-pathnames "swank-loader.lisp" *load-truename*))

(swank-loader:init
 :delete nil         ; delete any existing SWANK packages
 :reload nil         ; reload SWANK, even if the SWANK package already exists
 :load-contribs nil) ; load all contribs

(swank:create-server :port 4005
                     ;; if non-nil the connection won't be closed
                     ;; after connecting
                     :dont-close nil)
