;; nu-graphql.el --- Nubank graphql functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank graphql functions

;;; Code:

(require 'graphql-mode)
(require 'nu-auth)
(require 'request)

(defun nu-graphql-extra-headers (environment country)
  "Return the graphql-mode extra headers for `ENVIRONMENT` and `COUNTRY`."
  (list (nu-auth-authorization-header environment country)
        '("Accept" . "application/json; charset=utf-8")
        '("Content-Type" . "application/json")
        '("X-Correlation-ID" . "NUMACS")
        '("jaeger-debug-id" . "NUMACS")))

(defun nu-graphql-customize (environment country url)
  "Customize graphql-mode to use `ENVIRONMENT`, `COUNTRY` and `URL`."
  (setq graphql-extra-headers (nu-graphql-extra-headers environment country))
  (setq graphql-url url)
  (setq request-backend 'curl)
  (setq request-curl-options (nu-auth-request-curl-options environment country)))

(provide 'nu-graphql)

;;; nu-graphql.el ends here
