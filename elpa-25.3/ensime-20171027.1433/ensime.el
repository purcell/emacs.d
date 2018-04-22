;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs

;; Copyright (C) 2003 - 2015 the SLIME and ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;; Homepage: https://github.com/ensime/ensime-emacs
;; Keywords: languages
;; Package-Version: 2.0.0
;; Package-Requires: ((scala-mode "0.23") (sbt-mode "0.2") (yasnippet "0.10.0") (company "0.9.0") (dash "2.12.1") (s "1.11.0") (popup "0.5.3"))

;;; Commentary:
;;
;;  ENSIME has a server component which can read the AST of your
;;  project and its dependencies, providing features that are simply
;;  not possible with emacs-lisp pattern matching.
;;
;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'ensime-mode)

(defvar ensime-prefer-noninteractive t
  "State variable used for regression testing, and for skipping prompt in conjunction with sbt.")

(defvar ensime-popup-in-other-frame nil)


;;;###autoload
(defun ensime ()
  "Read config file for settings then start an ensime-server and connect."
  (interactive)
  (ensime-startup-notifications)
  (let ((orig-bfn (buffer-file-name-with-indirect)))
    (if ensime-auto-generate-config
        (ensime--maybe-refresh-config
         nil
         `(lambda () (ensime--maybe-update-and-start-noninteractive ,orig-bfn))
         `(lambda (reason) (ensime--maybe-update-and-start-noninteractive ,orig-bfn)))
      (ensime--maybe-update-and-start orig-bfn))))

;;;###autoload
(defun ensime-remote (host port)
  "Read config file for settings. Then connect to an existing ENSIME server."
  (interactive "shost: \nnport: ")

  (let ((orig-buffer-file-name buffer-file-name))
    (if ensime-auto-generate-config
        (ensime--maybe-refresh-config
         nil
         `(lambda () (ensime--maybe-update-and-start orig-buffer-file-name (url-gateway-nslookup-host ,host) ,port))
         `(lambda (reason) (ensime--maybe-update-and-start orig-buffer-file-name (url-gateway-nslookup-host ,host) ,port))))))

(provide 'ensime)

;;; ensime.el ends here
