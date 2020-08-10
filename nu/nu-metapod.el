;; nu-metapod.el --- Nubank Metapod functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank Metapod functions

;;; Code:

(require 'nu-country)
(require 'nu-environment)
(require 'nu-graphql)

(defun nu-metapod-url (environment)
  "Return the Metapod GraphQL URL for `ENVIRONMENT`."
  (format "https://%s-metapod.nubank.com.br/api/graphql/query"
          (nu-environment-id environment)))

(defun nu-metapod-customize (environment country)
  "Customize graphql-mode to use Metapod in `ENVIRONMENT` and `COUNTRY`."
  (nu-graphql-customize environment country (nu-metapod-url environment)))

(defun nu-metapod-switch ()
  "Switch environment and country and customize graphql-mode."
  (interactive)
  (let ((environment (nu-environment-switch))
        (country (nu-country-switch)))
    (nu-metapod-customize environment country)
    (message "You are now using Metapod in %s on %s."
             (propertize (nu-country-name country) 'face 'bold)
             (propertize (nu-environment-name environment) 'face 'bold))))

(provide 'nu-metapod)

;;; nu-metapod.el ends here
