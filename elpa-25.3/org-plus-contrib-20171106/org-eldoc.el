;;; org-eldoc.el --- display org header and src block info using eldoc

;; Copyright (c) 2014-2017 Free Software Foundation, Inc.

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 6
;; Package-Requires: ((org "8"))
;; URL: https://bitbucket.org/ukaszg/org-eldoc
;; Created: 25/05/2014
;; Keywords: eldoc, outline, breadcrumb, org, babel, minibuffer

;; This file is not part of Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Changelog:

;; As of 01/11/14 switching license to GPL3 to allow submission to org-mode.
;; 08/11/14 switch code to automatically define eldoc-documentation-function, but don't autostart eldoc-mode.

;;; Code:

(require 'org)
(require 'ob-core)
(require 'eldoc)

(declare-function org-element-at-point "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(defgroup org-eldoc nil "" :group 'org)

(defcustom org-eldoc-breadcrumb-separator "/"
  "Breadcrumb separator."
  :group 'org-eldoc
  :type 'string)

(defcustom org-eldoc-test-buffer-name " *Org-eldoc test buffer*"
  "Name of the buffer used while testing for mode-local variable values."
  :group 'org-eldoc
  :type 'string)

(defun org-eldoc-get-breadcrumb ()
  "Return breadcrumb if on a headline or nil."
  (let ((case-fold-search t) cur)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at org-complex-heading-regexp)
          (setq cur (match-string 4))
          (org-format-outline-path
           (append (org-get-outline-path) (list cur))
           (frame-width) "" org-eldoc-breadcrumb-separator))))))

(defun org-eldoc-get-src-header ()
  "Returns lang and list of header properties if on src definition line and nil otherwise."
  (let ((case-fold-search t) info lang hdr-args)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (setq info (org-babel-get-src-block-info 'light)
                lang (propertize (or (nth 0 info) "no lang") 'face 'font-lock-string-face)
                hdr-args (nth 2 info))
          (concat
           lang
           ": "
           (mapconcat
            (lambda (elem)
              (when (and (cdr elem) (not (string= "" (cdr elem))))
                (concat
                 (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                 " "
                 (propertize (cdr elem) 'face 'org-verbatim)
                 " ")))
            hdr-args " ")))))))

(defun org-eldoc-get-src-lang ()
  "Return value of lang for the current block if in block body and nil otherwise."
  (let ((element (save-match-data (org-element-at-point))))
    (and (eq (org-element-type element) 'src-block)
	 (>= (line-beginning-position)
	     (org-element-property :post-affiliated element))
	 (<=
	  (line-end-position)
	  (org-with-wide-buffer
	   (goto-char (org-element-property :end element))
	   (skip-chars-backward " \t\n")
	   (line-end-position)))
	 (org-element-property :language element))))

(defvar org-eldoc-local-functions-cache (make-hash-table :size 40 :test 'equal)
  "Cache of major-mode's eldoc-documentation-functions,
 used by \\[org-eldoc-get-mode-local-documentation-function].")

(defun org-eldoc-get-mode-local-documentation-function (lang)
  "Check if LANG-mode sets eldoc-documentation-function and return its value."
  (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
        (mode-func (intern-soft (format "%s-mode" lang)))
        doc-func)
    (if (eq 'empty cached-func)
        (when (fboundp mode-func)
          (with-temp-buffer
            (funcall mode-func)
            (setq doc-func (and eldoc-documentation-function
                                (symbol-value 'eldoc-documentation-function)))
            (puthash lang doc-func org-eldoc-local-functions-cache))
          doc-func)
      cached-func)))

(declare-function c-eldoc-print-current-symbol-info "c-eldoc" ())
(declare-function css-eldoc-function "css-eldoc" ())
(declare-function php-eldoc-function "php-eldoc" ())
(declare-function go-eldoc--documentation-function "go-eldoc" ())

(defun org-eldoc-documentation-function ()
  "Return breadcrumbs when on a headline, args for src block header-line,
  calls other documentation functions depending on lang when inside src body."
  (or
   (org-eldoc-get-breadcrumb)
   (org-eldoc-get-src-header)
   (let ((lang (org-eldoc-get-src-lang)))
     (cond ((or
             (string= lang "emacs-lisp")
             (string= lang "elisp")) (if (fboundp 'elisp-eldoc-documentation-function)
                                         (elisp-eldoc-documentation-function)
                                       (let (eldoc-documentation-function)
                                         (eldoc-print-current-symbol-info))))
           ((or
             (string= lang "c") ;; http://github.com/nflath/c-eldoc
             (string= lang "C")) (when (require 'c-eldoc nil t)
                                   (c-eldoc-print-current-symbol-info)))
           ;; https://github.com/zenozeng/css-eldoc
           ((string= lang "css") (when (require 'css-eldoc nil t)
                                   (css-eldoc-function)))
           ;; https://github.com/zenozeng/php-eldoc
           ((string= lang "php") (when (require 'php-eldoc nil t)
                                   (php-eldoc-function)))
           ((or
             (string= lang "go")
             (string= lang "golang")) (when (require 'go-eldoc nil t)
                                        (go-eldoc--documentation-function)))
           (t (let ((doc-fun (org-eldoc-get-mode-local-documentation-function lang)))
                (when (functionp doc-fun) (funcall doc-fun))))))))

;;;###autoload
(defun org-eldoc-load ()
  "Set up org-eldoc documentation function."
  (interactive)
  (setq-local eldoc-documentation-function #'org-eldoc-documentation-function))

;;;###autoload
(add-hook 'org-mode-hook #'org-eldoc-load)

(provide 'org-eldoc)

;; -*- coding: utf-8-emacs; -*-

;;; org-eldoc.el ends here
