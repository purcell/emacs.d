;;; Package --- Edit server is a chrome plugin let you to edit text areas with emacs
;;; Commentary:
;;; Why need this package because the chrome can not directly start a new program
;;; Code:
(require-package 'edit-server)
(require 'edit-server)

(edit-server-start)

(provide 'init-edit-server)
;;; init-edit-server.el ends here
