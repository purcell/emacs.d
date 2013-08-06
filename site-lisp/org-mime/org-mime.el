;;; org-mime.el --- org html export for text/html MIME emails

;; Copyright (C) 2010-2013 Eric Schulte

;; Author: Eric Schulte
;; Keywords: mime, mail, email, html
;; Homepage: http://orgmode.org/worg/org-contrib/org-mime.php
;; Version: 0.01

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WYSWYG, html mime composition using org-mode
;;
;; For mail composed using the orgstruct-mode minor mode, this
;; provides a function for converting all or part of your mail buffer
;; to embedded html as exported by org-mode.  Call `org-mime-htmlize'
;; in a message buffer to convert either the active region or the
;; entire buffer to html.
;;
;; Similarly the `org-mime-org-buffer-htmlize' function can be called
;; from within an org-mode buffer to convert the buffer to html, and
;; package the results into an email handling with appropriate MIME
;; encoding.
;;
;; you might want to bind this to a key with something like the
;; following message-mode binding
;;
;;   (add-hook 'message-mode-hook
;;             (lambda ()
;;               (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
;;
;; and the following org-mode binding
;;
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

;;; Code:
(require 'cl)

(declare-function org-export-string-as "ox"
		  (string backend &optional body-only ext-plist))

(defcustom org-mime-use-property-inheritance nil
  "Non-nil means al MAIL_ properties apply also for sublevels."
  :group 'org-mime
  :type 'boolean)

(defcustom org-mime-default-header
  "#+OPTIONS: latex:t\n"
  "Default header to control html export options, and ensure
  first line isn't assumed to be a title line."
  :group 'org-mime
  :type 'string)

(defcustom org-mime-library 'mml
  "Library to use for marking up MIME elements."
  :group 'org-mime
  :type '(choice 'mml 'semi 'vm))

(defcustom org-mime-preserve-breaks t
  "Used as temporary value of `org-export-preserve-breaks' during
  mime encoding."
  :group 'org-mime
  :type 'boolean)

(defcustom org-mime-fixedwith-wrap
  "<pre style=\"font-family: courier, monospace;\">\n%s</pre>\n"
  "Format string used to wrap a fixedwidth HTML email."
  :group 'org-mime
  :type 'string)

(defcustom org-mime-html-hook nil
  "Hook to run over the html buffer before attachment to email.
  This could be used for example to post-process html elements."
  :group 'org-mime
  :type 'hook)

(mapc (lambda (fmt)
	(eval `(defcustom
		 ,(intern (concat "org-mime-pre-" fmt "-hook"))
		 nil
		 (concat "Hook to run before " fmt " export.\nFunctions "
			 "should take no arguments and will be run in a "
			 "buffer holding\nthe text to be exported."))))
      '("ascii" "org" "html"))

(defcustom org-mime-send-subtree-hook nil
  "Hook to run in the subtree in the Org-mode file before export.")

(defcustom org-mime-send-buffer-hook nil
  "Hook to run in the Org-mode file before export.")

;; example hook, for setting a dark background in <pre style="background-color: #EEE;"> elements
(defun org-mime-change-element-style (element style)
  "Set new default htlm style for <ELEMENT> elements in exported html."
  (while (re-search-forward (format "<%s" element) nil t)
    (replace-match (format "<%s style=\"%s\"" element style))))

(defun org-mime-change-class-style (class style)
  "Set new default htlm style for objects with classs=CLASS in
exported html."
  (while (re-search-forward (format "class=\"%s\"" class) nil t)
    (replace-match (format "class=\"%s\" style=\"%s\"" class style))))

;; ;; example addition to `org-mime-html-hook' adding a dark background
;; ;; color to <pre> elements
;; (add-hook 'org-mime-html-hook
;;           (lambda ()
;;             (org-mime-change-element-style
;;              "pre" (format "color: %s; background-color: %s;"
;;                            "#E6E1DC" "#232323"))
;; 	    (org-mime-change-class-style
;;              "verse" "border-left: 2px solid gray; padding-left: 4px;")))

(defun org-mime-file (ext path id)
  "Markup a file for attachment."
  (case org-mime-library
    ('mml (format (concat "<#part type=\"%s\" filename=\"%s\" "
			  "disposition=inline id=\"<%s>\">\n<#/part>\n")
		  ext path id))
    ('semi (concat
            (format (concat "--[[%s\nContent-Disposition: "
			    "inline;\nContent-ID: <%s>][base64]]\n")
		    ext id)
            (base64-encode-string
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (binary-insert-encoded-file path)
               (buffer-string)))))
    ('vm "?")))

(defun org-mime-multipart (plain html &optional images)
  "Markup a multipart/alternative with text/plain and text/html alternatives.
If the html portion of the message includes images wrap the html
and images in a multipart/related part."
  (case org-mime-library
    ('mml (concat "<#multipart type=alternative><#part type=text/plain>"
		  plain
		  (when images "<#multipart type=related>")
		  "<#part type=text/html>"
		  html
		  images
		  (when images "<#/multipart>\n")
		  "<#/multipart>\n"))
    ('semi (concat
            "--" "<<alternative>>-{\n"
            "--" "[[text/plain]]\n" plain
	    (when images (concat "--" "<<alternative>>-{\n"))
            "--" "[[text/html]]\n"  html
	    images
	    (when images (concat "--" "}-<<alternative>>\n"))
            "--" "}-<<alternative>>\n"))
    ('vm "?")))

(defun org-mime-replace-images (str current-file)
  "Replace images in html files with cid links."
  (let (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"cid:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (expand-file-name
                       url (file-name-directory current-file)))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
           (add-to-list 'html-images
                        (org-mime-file (concat "image/" ext) path id))
           id)))
      str)
     html-images)))

(defun org-mime-htmlize (arg)
  "Export a portion of an email body composed using `mml-mode' to
html using `org-mode'.  If called with an active region only
export that region, otherwise export the entire body."
  (interactive "P")
  (require 'ox-org)
  (require 'ox-html)
  (let* ((region-p (org-region-active-p))
         (html-start (or (and region-p (region-beginning))
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward mail-header-separator)
                           (+ (point) 1))))
         (html-end (or (and region-p (region-end))
                       ;; TODO: should catch signature...
                       (point-max)))
         (raw-body (concat org-mime-default-header
			   (buffer-substring html-start html-end)))
         (tmp-file (make-temp-name (expand-file-name
				    "mail" temporary-file-directory)))
         (body (org-export-string-as raw-body 'org t))
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks org-mime-preserve-breaks)
	 ;; dvipng for inline latex because MathJax doesn't work in mail
	 (org-html-with-latex 'dvipng)
         ;; to hold attachments for inline html images
         (html-and-images
          (org-mime-replace-images
	   (org-export-string-as raw-body 'html t) tmp-file))
         (html-images (unless arg (cdr html-and-images)))
         (html (org-mime-apply-html-hook
                (if arg
                    (format org-mime-fixedwith-wrap body)
                  (car html-and-images)))))
    (delete-region html-start html-end)
    (save-excursion
      (goto-char html-start)
      (insert (org-mime-multipart
	       body html (mapconcat 'identity html-images "\n"))))))

(defun org-mime-apply-html-hook (html)
  (if org-mime-html-hook
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (run-hooks 'org-mime-html-hook)
        (buffer-string))
    html))

(defmacro org-mime-try (&rest body)
  `(condition-case nil ,@body (error nil)))

(defun org-mime-send-subtree (&optional fmt)
  (save-restriction
    (org-narrow-to-subtree)
    (run-hooks 'org-mime-send-subtree-hook)
    (flet ((mp (p) (org-entry-get nil p org-mime-use-property-inheritance)))
      (let* ((file (buffer-file-name (current-buffer)))
	     (subject (or (mp "MAIL_SUBJECT") (nth 4 (org-heading-components))))
	     (to (mp "MAIL_TO"))
	     (cc (mp "MAIL_CC"))
	     (bcc (mp "MAIL_BCC"))
	     (body (buffer-substring
		    (save-excursion (goto-char (point-min))
				    (forward-line 1)
				    (when (looking-at "[ \t]*:PROPERTIES:")
				      (re-search-forward ":END:" nil)
				      (forward-char))
				    (point))
		    (point-max))))
	(org-mime-compose body (or fmt 'org) file to subject
			  `((cc . ,cc) (bcc . ,bcc)))))))

(defun org-mime-send-buffer (&optional fmt)
  (run-hooks 'org-mime-send-buffer-hook)
  (let* ((region-p (org-region-active-p))
	 (subject (org-export-grab-title-from-buffer))
         (file (buffer-file-name (current-buffer)))
         (body-start (or (and region-p (region-beginning))
                         (save-excursion (goto-char (point-min)))))
         (body-end (or (and region-p (region-end)) (point-max)))
	 (temp-body-file (make-temp-file "org-mime-export"))
	 (body (buffer-substring body-start body-end)))
    (org-mime-compose body (or fmt 'org) file nil subject)))

(defun org-mime-compose (body fmt file &optional to subject headers)
  (require 'message)
  (message-mail to subject headers nil)
  (message-goto-body)
  (flet ((bhook (body fmt)
		(let ((hook (intern (concat "org-mime-pre-"
					    (symbol-name fmt)
					    "-hook"))))
		  (if (> (eval `(length ,hook)) 0)
		      (with-temp-buffer
			(insert body)
			(goto-char (point-min))
			(eval `(run-hooks ',hook))
			(buffer-string))
		    body))))
    (let ((fmt (if (symbolp fmt) fmt (intern fmt))))
      (cond
       ((eq fmt 'org)
	(require 'ox-org)
	(insert (org-export-string-as
		 (org-babel-trim (bhook body 'org)) 'org t)))
       ((eq fmt 'ascii)
	(require 'ox-ascii)
	(insert (org-export-string-as
		 (concat "#+Title:\n" (bhook body 'ascii)) 'ascii t)))
       ((or (eq fmt 'html) (eq fmt 'html-ascii))
	(require 'ox-ascii)
	(require 'ox-org)
	(let* ((org-link-file-path-type 'absolute)
	       ;; we probably don't want to export a huge style file
	       (org-export-htmlize-output-type 'inline-css)
	       (html-and-images
		(org-mime-replace-images
		 (org-export-string-as (bhook body 'html) 'html t) file))
	       (images (cdr html-and-images))
	       (html (org-mime-apply-html-hook (car html-and-images))))
	  (insert (org-mime-multipart
		   (org-export-string-as
		    (org-babel-trim
		     (bhook body (if (eq fmt 'html) 'org 'ascii)))
		    (if (eq fmt 'html) 'org 'ascii) t)
		   html)
		  (mapconcat 'identity images "\n"))))))))

(defun org-mime-org-buffer-htmlize ()
  "Create an email buffer containing the current org-mode file
  exported to html and encoded in both html and in org formats as
  mime alternatives."
  (interactive)
  (org-mime-send-buffer 'html))

(defun org-mime-subtree ()
  "Create an email buffer containing the current org-mode subtree
  exported to a org format or to the format specified by the
  MAIL_FMT property of the subtree."
  (interactive)
  (org-mime-send-subtree
   (or (org-entry-get nil "MAIL_FMT" org-mime-use-property-inheritance) 'org)))

(provide 'org-mime)
