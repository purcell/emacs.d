;;; package --- Summary:
;;; Commentary:
;;; Code:
;; offlineimap
(require-package 'offlineimap)
(add-hook 'gnus-before-startup-hook 'offlineimap)

;; bbdb
(require-package 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb-file "~/.bbdb-contacts")
(setq bbdb-send-mail-style 'gnus)
(setq bbdb-complete-name-full-completion t)
(setq bbdb-completion-type 'primary-or-name)
(setq bbdb-complete-name-allow-cycling t)
(setq
 bbdb-offer-save 1
 bbdb-use-pop-up t
 bbdb-electric-p t
 bbdb-popup-target-lines  1)

;; bbdb-vard
(require-package 'bbdb-vcard)

(require 'gnus)
(require 'nnir)
(setq gnus-fetch-old-headers t) ; do not hide already read messages
(setq gnus-ignored-newsgroups "")
; set the place where MIME stuff will go
(defvar mime-download-folder "~/Downloads")
(setq mm-default-directory (if (file-readable-p mime-download-folder)
                               mime-download-folder
                             "~/"))

(provide 'init-gnus)
;;; init-gnus ends here
