;;; sb-ant.el --- provide speedbar menus for ANT Project files

;; Copyright (c) 2001, 2003 Ole Arndt
;; Author: Ole Arndt <arndt@tivano.com>
;; based on sb-html.el by:
;; Created: Tue Sep 09 09:45:00 2001

;; Copyright (c) 2001 Richard Y. Kim
;; Author: Richard Y. Kim, <ryk@dspwiz.com>
;; Maintainer: Richard Y. Kim, <ryk@dspwiz.com>
;; Created: Mon Apr 09 09:44:06 2001
;; Version: $Id: sb-ant.el,v 1.3 2005/09/30 20:25:50 zappo Exp $
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

;; This small add-on to speedbar provides an alternate way to view ANT
;; project files. It simply lists all the available targets.
;;
;; This code is a simple modification of sb-html.el which comes
;; with speedbar.

;; Installation procedure:
;;
;;   Install speedbar 0.12 or later.
;;   Add the following to your ~/.emacs file:
;;   (eval-after-load "speedbar" '(load-library "sb-ant"))
;;

;;; Code:

(require 'speedbar)
(require 'sb-texinfo)	; for speedbar-format-texinfo-list

;; Attach these new functions to handle texinfo-mode.
(add-to-list 'speedbar-dynamic-tags-function-list
	     '(speedbar-fetch-dynamic-ant-project . speedbar-insert-ant-project-list))

;; This returns t if the major mode of the current buffer is not
;; 'xml-mode. If it is 'xml-mode, then this returns a
;; list where each element is (LEVEL TARGETNAME . MARKER). 
;; LEVLE is always 0,  NAME is the name of the target. MARKER is
;; emacs marker that points to the beginning of the section. The
;; elements in the list returned are in ascending order of the
;; MARKER. This function along with it's parter,
;; speedbar-insert-ant-project-list, are designed to be added to the
;; speedbar-dynamic-tags-function-list list.
;;
;; This function is based on `speedbar-fetch-dynamic-html'.
(defun speedbar-fetch-dynamic-ant-project ( filename )
  (set-buffer (find-file-noselect filename))
  (if (not (or (eq major-mode 'xml-mode)
	       (eq major-mode 'sgml-mode)
	       (eq major-mode 'ant-mode)))
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

	  (let ((case-fold-search t)	; ant tags are case insensitive
		pos-beg title level alist beg)
	    (goto-char (point-min))
	    ;; regexp below will not work if `>' appears within an attribute
	    ;; value, e.g., <h1 foo="I'm a weird value >"> ... </h1>
	    (while (re-search-forward "<target[^>]*name\\s-*=\\s-*\"\\([^\"]*\\)\"[^>]*>" nil t)
	      (setq beg (match-end 0))
	      (goto-char (match-beginning 0))
	      (setq pos-beg (point-marker))
	      (setq level 0)
	      (setq title (match-string 1))
	      (re-search-forward (concat "</target" ""))
	      (setq alist (cons (cons level (cons title pos-beg)) alist)))
	    (nreverse alist)))
      (error t))))

(fset 'speedbar-format-ant-project-list 'speedbar-format-texinfo-list)

(defun speedbar-insert-ant-project-list (indent lst)
  (speedbar-insert-generic-list indent (speedbar-format-ant-project-list lst indent)
				'speedbar-tag-expand
				'speedbar-tag-find))

(provide 'sb-ant)

;;; sb-ant.el ends here
