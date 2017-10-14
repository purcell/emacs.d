;;; ledger-fonts.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.



;;; Commentary:
;; All of the faces for ledger mode are defined here.

;;; Code:

(require 'ledger-regex)

(defgroup ledger-faces nil "Ledger mode highlighting" :group 'ledger)

(defface ledger-font-default-face
  `((t :inherit default))
  "Default face"
  :group 'ledger-faces)

(defface ledger-font-auto-xact-face
  `((t :foreground "orange" :weight normal))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-periodic-xact-face
  `((t :foreground "green" :weight normal))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-xact-cleared-face
  `((t :foreground "#AAAAAA" :weight normal))
  "Default face for cleared transaction"
  :group 'ledger-faces)

(defface ledger-font-xact-pending-face
  `((t :foreground "#444444" :weight normal))
  "Default face for pending transaction"
  :group 'ledger-faces)

(defface ledger-font-payee-uncleared-face
  `((t :foreground "#dc322f" :weight bold ))
  "Default face for Ledger"
  :group 'ledger-faces)

(defface ledger-font-payee-cleared-face
  `((t :inherit ledger-font-other-face))
  "Default face for cleared (*) payees"
  :group 'ledger-faces)

(defface ledger-font-payee-pending-face
  `((t :foreground "#F24B61" :weight normal))
  "Default face for pending (!) payees"
  :group 'ledger-faces)

(defface ledger-font-xact-highlight-face
  `((t :inherit ledger-occur-xact-face))
  "Default face for transaction under point"
  :group 'ledger-faces)

(defface ledger-font-pending-face
  `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions"
  :group 'ledger-faces)

(defface ledger-font-other-face
  `((t :foreground "#657b83" :weight normal))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-directive-face
  `((t :inherit font-lock-preprocessor-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-account-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-price-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-apply-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-alias-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-assert-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-bucket-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-C-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for C directive"
  :group 'ledger-faces)

(defface ledger-font-capture-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-check-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-commodity-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-D-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for D directive"
  :group 'ledger-faces)

(defface ledger-font-define-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-end-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-expr-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-fixed-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-include-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-N-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for N directive"
  :group 'ledger-faces)

(defface ledger-font-payee-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-tag-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-timeclock-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for timeclock I,i,O,o,b,h directives"
  :group 'ledger-faces)

(defface ledger-font-year-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-posting-account-face
  `((t :foreground "#268bd2" ))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-cleared-face
  `((t :inherit ledger-font-other-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-cleared-face
  `((t :inherit ledger-font-posting-account-cleared-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-pending-face
  `((t :inherit ledger-font-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-pending-face
  `((t :inherit ledger-font-posting-account-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-face
  `((t :foreground "#cb4b16" ))
  "Face for Ledger amounts"
  :group 'ledger-faces)

(defface ledger-font-posting-date-face
  `((t :foreground "#cb4b16" ))
  "Face for Ledger dates"
  :group 'ledger-faces)

(defface ledger-occur-narrowed-face
  `((t :inherit font-lock-comment-face :invisible t))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face
  `((t :inherit highlight))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defface ledger-font-comment-face
  `((t :inherit font-lock-comment-face))
  "Face for Ledger comments"
  :group 'ledger-faces)

(defface ledger-font-reconciler-uncleared-face
  `((t :inherit ledger-font-payee-uncleared-face))
  "Default face for uncleared transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-cleared-face
  `((t :inherit ledger-font-other-face))
  "Default face for cleared (*) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-report-clickable-face
  `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-code-face
  `((t :inherit ledger-font-default-face))
  "Face for Ledger codes"
  :group 'ledger-faces)

(defvar ledger-font-lock-keywords
  `(("account" . ledger-font-account-directive-face)
    ("apply" . ledger-font-apply-directive-face)
    ("alias" . ledger-font-alias-directive-face)
    ("assert" . ledger-font-assert-directive-face)
    ("bucket" . ledger-font-bucket-directive-face)
    ("capture" . ledger-font-capture-directive-face)
    ("check" . ledger-font-check-directive-face)
    ("comment" . ledger-font-comment-face)
    ("commodity" . ledger-font-commodity-directive-face)
    ("define" . ledger-font-define-directive-face)
    ("end" . ledger-font-end-directive-face)
    ("expr" . ledger-font-expr-directive-face)
    ("fixed" . ledger-font-fixed-directive-face)
    ("include" . ledger-font-include-directive-face)
    ("payee" . ledger-font-payee-directive-face)
    ("tag" . ledger-font-tag-directive-face)
    ("test" . ledger-font-comment-face)
    ("year" . ledger-font-year-directive-face))
  "Expressions to highlight in Ledger mode.")



(provide 'ledger-fonts)

;;; ledger-fonts.el ends here
