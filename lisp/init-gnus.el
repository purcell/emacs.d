;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offlineimap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offlineimap
(require-package 'offlineimap)
(add-hook 'gnus-before-startup-hook 'offlineimap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bbdb set up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnus set up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gnus)
(require 'nnir)
(setq gnus-fetch-old-headers t) ; do not hide already read messages
(setq gnus-ignored-newsgroups "")
; set the place where MIME stuff will go
(defvar mime-download-folder "~/Downloads")
(setq mm-default-directory (if (file-readable-p mime-download-folder)
                               mime-download-folder
                             "~/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smtpmail mutli
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-auth-supported (list mech)
                                              smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password &optional key
                            cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-starttls-credentials (list (list
                                                                                              server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL
enabled.)" server port user))

; This function will complain if you fill the from field with
; an account not present in smtp-accounts.
(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from) do (cond
                                               ((memq auth-mech '(cram-md5 plain login))
                                                (return (apply 'set-smtp (cons auth-mech auth-spec))))
                                               ((eql auth-mech 'ssl)
                                                (return (apply 'set-smtp-ssl auth-spec)))
                                               (t (error "Unrecognized SMTP auth. mechanism:
`%s'." auth-mech))) finally (error "Cannot infer SMTP
information."))))

(defvar %smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (with-current-buffer smtpmail-text-buffer
    (change-smtp))
  (funcall (symbol-value '%smtpmail-via-smtp) recipient
           smtpmail-text-buffer))


(provide 'init-gnus)
;;; init-gnus ends here
