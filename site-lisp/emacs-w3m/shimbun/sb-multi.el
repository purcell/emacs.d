;;; sb-multi.el --- Virtual shimbun class to retrieve multiple pages.

;; Copyright (C) 2006, 2007, 2008, 2009 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; `multiple-value-bind' requires the 2nd argument to be multiple-value,
  ;; not a list, in particular for XEmacs 21.5.  `values-list' does it,
  ;; but is a run-time cl function in XEmacs 21.4 and Emacs 21.
  (when (eq 'identity (symbol-function 'values-list))
    (define-compiler-macro values-list (arg)
      arg)))

(require 'shimbun)

(autoload 'shimbun-shallow-rendering "sb-text")

(luna-define-class shimbun-multi () ())

(luna-define-generic shimbun-multi-next-url (shimbun header url)
  "Return a URL of the next page if it exists in this current buffer.")

(luna-define-generic shimbun-multi-clear-contents (shimbun
						   header
						   has-previous-page
						   has-next-page)
  "Clear a content in this current buffer for an article of SHIMBUN.
Return nil, unless a content is cleared successfully.")

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-multi)
						  header
						  has-previous-page
						  has-next-page)
  (shimbun-clear-contents shimbun header))

(defun shimbun-multi-retrieve-next-pages (shimbun header base-cid url
						  &optional images cont)
  (let ((prefer-text-plain (shimbun-prefer-text-plain-internal shimbun))
	(case-fold-search t) base-url next-url)
    (setq base-url (or (shimbun-current-base-url) url)
	  next-url (shimbun-multi-next-url shimbun header base-url))
    (when (shimbun-multi-clear-contents shimbun header cont next-url)
      (goto-char (point-min))
      (insert "<html>\n<head>\n<base href=\""
	      base-url
	      "\">\n</head>\n<body>\n")
      (goto-char (point-max))
      (if next-url
	  (insert "\n</body>\n</html>\n")
	(if prefer-text-plain
	    (shimbun-insert-footer shimbun header)
	  (shimbun-insert-footer shimbun header t "</body>\n</html>\n"))))
    (if prefer-text-plain
	(shimbun-shallow-rendering)
      (when shimbun-encapsulate-images
	(setq images (shimbun-mime-replace-image-tags shimbun
						      base-cid
						      base-url
						      images))))
    (let ((body (shimbun-make-text-entity (if prefer-text-plain
					      "text/plain"
					    "text/html")
					  (buffer-string)))
	  (result
	   (when next-url
	     (with-temp-buffer
	       (shimbun-fetch-url shimbun next-url nil nil url)
	       (shimbun-multi-retrieve-next-pages shimbun
						  header
						  base-cid
						  next-url
						  images t)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(defun shimbun-multi-make-contents (shimbun header)
  (let ((base-cid (shimbun-header-id header))
	(body))
    (if (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
	(setq base-cid (match-string 1 base-cid))
      (error "Cannot extract base CID from %s for %s"
	     base-cid (shimbun-article-url shimbun header)))
    (multiple-value-bind (texts images)
	(values-list
	 (shimbun-multi-retrieve-next-pages shimbun header base-cid
					    (shimbun-article-url shimbun
								 header)))
      (if (= (length texts) 1)
	  (setq body (car texts))
	(setq body (shimbun-make-multipart-entity))
	(let ((i 0))
	  (dolist (text texts)
	    (setf (shimbun-entity-cid text)
		  (format "shimbun.%d.%s" (incf i) base-cid))))
	(apply 'shimbun-entity-add-child body texts))
      (when images
	(setf (shimbun-entity-cid body) (concat "shimbun.0." base-cid))
	(let ((new (shimbun-make-multipart-entity)))
	  (shimbun-entity-add-child new body)
	  (apply 'shimbun-entity-add-child new (mapcar 'cdr (nreverse images)))
	  (setq body new))))
    (erase-buffer)
    (shimbun-header-insert shimbun header)
    (insert "MIME-Version: 1.0\n")
    (shimbun-entity-insert body))
  (buffer-string))

(luna-define-method shimbun-make-contents ((shimbun shimbun-multi) header)
  (shimbun-multi-make-contents shimbun header))

(provide 'sb-multi)

;;; sb-multi.el ends here
