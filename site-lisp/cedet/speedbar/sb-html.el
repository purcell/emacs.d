;;; sb-html.el --- provide hierarchical speedbar menu's for HTML files

;; Copyright (c) 2001, 2002 Richard Y. Kim

;; Author: Richard Y. Kim, <ryk@dspwiz.com>
;; Maintainer: Richard Y. Kim, <ryk@dspwiz.com>
;; Created: Mon Apr 09 09:44:06 2001
;; Version: $Id: sb-html.el,v 1.4 2005/09/30 20:25:56 zappo Exp $
;; Keywords: speedbar, html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA

;;; Commentary:
;;
;; This small add-on to speedbar provides an alternate way to view HTML
;; documents which shows the natural hierarchy of the document based
;; on H1, H2, ... H6 tags.
;;
;; This code is a simple modification of sb-texinfo.el which comes
;; with speedbar.

;; Installation procedure:
;;
;;   Install speedbar 0.12 or later.
;;   Add the following to your ~/.emacs file:
;;   (eval-after-load "speedbar" '(load-library "sb-html"))
;;
;; Known Problems:
;;
;;   Perhaps <HEAD> and <BODY> should be treated as H0 tag?

;;; Change Log:
;;;
;;; 1.1 - modified regexp based on Eric's suggestion.
;;;
;;; 1.0 - sent to Eric Ludlam.

;;; Code:

(require 'speedbar)
(require 'sb-texinfo)	; for speedbar-format-texinfo-list

;; Attach these new functions to handle texinfo-mode.
(add-to-list 'speedbar-dynamic-tags-function-list
	     '(speedbar-fetch-dynamic-html . speedbar-insert-html-list))

;; This returns t if the major mode of the current buffer is not
;; 'html-helper-mode. If it is 'html-helper-mode, then this returns a
;; list where each element is (LEVEL NAME . MARKER). LEVEL is 0, 1, 2,
;; 3, 4, or 5 corresponding to H1, H2, H3, H4, H5 an H6
;; tags. respectively. NAME is the name of the section. MARKER is
;; emacs marker that points to the beginning of the section. The
;; elements in the list returned are in ascending order of the
;; MARKER. This function along with it's parter,
;; speedbar-insert-html-list, are designed to be added to the
;; speedbar-dynamic-tags-function-list list.
;;
;; This function is based on `speedbar-fetch-dynamic-texinfo'.
(defun speedbar-fetch-dynamic-html ( filename )
  (set-buffer (find-file-noselect filename))
  (if (not (or (eq major-mode 'html-helper-mode)
	       (eq major-mode 'html-mode)))
      t
    (condition-case nil
	(save-excursion

	  ;; Set speedbar-tag-hierarchy-method to nil so that
	  ;; speedbar-create-tag-hierarchy won't reorder the list.
	  ;; Make it buffer local so that the global value is not touched.
	  (make-local-variable 'speedbar-tag-hierarchy-method)
	  (setq speedbar-tag-hierarchy-method nil)

	  (set (make-local-variable
		'speedbar-generic-list-group-expand-button-type)
	       'expandtag)
	  (set (make-local-variable
		'speedbar-generic-list-tag-button-type)
	       'statictag)

	  (let ((case-fold-search t)	; HTML tags are case insensitive
		pos-beg title level alist beg)
	    (goto-char (point-min))
	    ;; regexp below will not work if `>' appears within an attribute
	    ;; value, e.g., <h1 foo="I'm a weird value >"> ... </h1>
	    (while (re-search-forward "<h\\([1-6]\\)\\b[^>]*>\\s-*" nil t)
	      (setq beg (match-end 0))
	      (goto-char (match-beginning 0))
	      (setq pos-beg (point-marker))
	      (setq level (1- (read (match-string 1))))
	      (re-search-forward (concat "\\s-*</h" (match-string 1) "\\b"))
	      (setq title (buffer-substring beg (match-beginning 0)))
	      (setq alist (cons (cons level (cons title pos-beg)) alist)))
	    (nreverse alist)))
      (error t))))

(fset 'speedbar-format-html-list 'speedbar-format-texinfo-list)

(defun speedbar-insert-html-list (indent lst)
  (speedbar-insert-generic-list indent (speedbar-format-html-list lst 0)
				'speedbar-tag-expand
				'speedbar-tag-find))

(provide 'sb-html)

;;; sb-html.el ends here
