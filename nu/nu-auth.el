;; nu-auth.el --- Nubank auth functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank auth functions

;;; Code:

(require 'nu-common)
(require 'nu-country)
(require 'nu-environment)
(require 's)

(defun nu--auth-token-directory (environment country)
  "Return the token directory for `ENVIRONMENT` and `COUNTRY`."
  (file-name-as-directory
   (concat (nu-home-directory) ".nu/tokens/"
           (nu-country-id country) "/"
           (nu-environment-id environment))))

(defun nu--auth-certificate-directory (environment country)
  "Return the certificate directory for `ENVIRONMENT` and `COUNTRY`."
  (file-name-as-directory
   (concat (nu-home-directory) ".nu/certificates/"
           (nu-country-id country) "/"
           (nu-environment-id environment))))

(defun nu-auth-access-token-filename (environment country)
  "Return the filename of the access token for `ENVIRONMENT` and `COUNTRY`."
  (concat (nu--auth-token-directory environment country) "access"))

(defun nu-auth-access-token (environment country)
  "Return the access token for `ENVIRONMENT` and `COUNTRY`."
  (let ((filename (nu-auth-access-token-filename environment country)))
    (if (file-exists-p filename)
        (s-trim (nu-slurp-file (nu-auth-access-token-filename environment country)))
      (user-error "Access token file does not exists: %s" filename))))

(defun nu-auth-authorization-header (environment country)
  "Return the Authorization HTTP header for `ENVIRONMENT` and `COUNTRY`."
  (let ((token (nu-auth-access-token environment country)))
    `("Authorization" . ,(concat "Bearer " token))))

(defun nu-auth-certificate-key-file (environment country)
  "Return the certificate key file for `ENVIRONMENT` and `COUNTRY`."
  (concat (nu--auth-certificate-directory environment country) "key.pem"))

(defun nu-auth-certificate-cert-file (environment country)
  "Return the certificate file for `ENVIRONMENT` and `COUNTRY`."
  (concat (nu--auth-certificate-directory environment country) "cert.pem"))

(defun nu-auth-request-curl-options (environment country)
  "Return the request.el curl options for `ENVIRONMENT` and `COUNTRY`."
  (list "--key" (nu-auth-certificate-key-file environment country)
        "--cert" (nu-auth-certificate-cert-file environment country)))

(provide 'nu-auth)

;;; nu-auth.el ends here
