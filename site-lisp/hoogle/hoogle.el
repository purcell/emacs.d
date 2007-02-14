;; This code comes from hyperclim.el and was originally written by
;; Andy Hefner (andy.hefner@verizon.net) 
;; modified for Hoogle -- clemens@endorphin.org

(require 'cl)
(require 'browse-url)
(require 'thingatpt)

(defvar hoogle-url-base "http://haskell.org/hoogle/?q=")
(defvar hoogle-history nil)

(defun hoogle-lookup (p)
  (interactive "p")  
  (let ((symbol-name (thing-at-point 'symbol)))
    (unless (and (= 1 p) (stringp symbol-name))
      (setq symbol-name (read-from-minibuffer "Hoogle lookup name: " "" nil nil 'hoogle-history)))
    (browse-url (concat hoogle-url-base
		       symbol-name))))

(provide 'hoogle)
