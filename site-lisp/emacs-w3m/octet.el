;;; octet.el --- An octet stream viewer.

;; Copyright (C) 2000, 2002, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Created: 2000/05/19
;; Keywords: octet-stream, broken document

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Display application/octet-stream inline on the emacs buffer.
;;
;; This program requires:
;;
;; emacs-w3m for HTML rendereing.
;; (http://emacs-w3m.namazu.org/)
;; Mule-UCS  for UTF-8 decoding.
;; (ftp://ftp.m17n.org/pub/mule/Mule-UCS/)
;; wvHtml for MS Word document.
;; (http://www.wvware.com/)
;; xlHtml for MS Excel document.
;; (http://chicago.sourceforge.net/xlhtml/)
;; pptHtml for MS PowerPoint document.
;; (http://chicago.sourceforge.net/xlhtml/)
;; gunzip for decoding gzipped file.
;; bunzip2 for decoding bzip2ed file.

;; Put follwing line in your setting file:
;;
;; (require 'octet)
;;
;; To display octet data file, execute following command.
;;
;; M-x octet-find-file
;;
;; If you use SEMI, put following lines in your setting file:
;;
;; (require 'octet)
;; (octet-mime-setup)
;;
;; Then you can toggle displaying application/octet-stream messages.

;;; History:
;;
;; This file is created in 2000/05/19.
;; All part was rewrote in 2002/01/28.
;; Added to emacs-w3m repository in 2002/01/29.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'poe)     ; for compatibility
(require 'pces)    ; as-binary-process
(require 'mime)    ; SEMI
(require 'static)
(require 'w3m-util); w3m-insert-string

(defvar octet-temp-directory temporary-file-directory
  "A directory to create temporal files.")

(defvar octet-html-render-function 'octet-w3m-region
  "A function for HTML rendering.")

(defvar octet-suffix-type-alist
  '(("xls"  . msexcel)
    ("ppt"  . msppt)
    ("doc"  . msword)
    ("gz"   . gzip)
    ("bz2"  . bzip2)
    ("html" . html)
    ("jpg"  . jpeg)
    ("jpeg" . jpeg)
    ("gif"  . gif)
    ("png"  . png)
    ("tif"  . tiff)
    ("tiff" . tiff)
    ("txt"  . text)
    ("lzh"  . lzh)
    ("tar"  . tar)
    ("pdf"  . pdf))
  "Alist of suffix-to-octet-type.")

(defvar octet-content-type-alist
  '(("application/vnd\\.ms-excel"	. msexcel)
    ("application/vnd\\.ms-powerpoint"	. msppt)
    ("application/x-msexcel"		. msexcel)
    ("application/msword"		. msword)
    ("image/jpeg"			. jpeg)
    ("image/gif"			. gif)
    ("image/png"			. png)
    ("image/tiff"			. tiff)
    ("audio/midi"			. ignore)
    ("video/mpeg"			. ignore)
    ("text/html"			. html-un)
    ("application/x-tar"		. tar)
    ("application/pdf"			. pdf))
  "Alist of content-type-regexp-to-octet-type.")

(defvar octet-magic-type-alist
  '(("^\377\330\377[\340\356]..JFIF"	image jpeg)
    ("^\211PNG"				image png)
    ("^GIF8[79]"			image gif)
    ("^II\\*\000"			image tiff)
    ("^MM\000\\*"			image tiff)
    ("^MThd"				audio midi)
    ("^\000\000\001\263"		video mpeg)
    ("^<!doctype html"			text  html)
    ("^<head"				text  html)
    ("^<title"				text  html)
    ("^<html"				text  html))
  "*Alist of regexp about magic-number vs. corresponding content-types.
Each element looks like (REGEXP TYPE SUBTYPE).
REGEXP is a regular expression to match against the beginning of the
content of entity.
TYPE is symbol to indicate primary type of content-type.
SUBTYPE is symbol to indicate subtype of content-type.")

(defvar octet-type-filter-alist
  `((msexcel octet-filter-call1	      "xlhtml" ("-te")	html-u8)
    (msppt   octet-filter-call1	      "ppthtml" nil	html-u8)
    (msword  octet-filter-call2-extra "wvHtml"	nil	html-u8)
    (html    octet-render-html	      nil	nil	nil)
    (html-u8 octet-decode-u8-text     nil	nil	html)
    (html-un octet-decode-text	      nil	nil	html)
    (gzip    octet-filter-call1	      "gunzip"	("-c")	guess)
    (bzip2   octet-filter-call1	      "bunzip2"	("-c")	guess)
    (text    octet-decode-text	      nil	nil	nil)
    (ignore  ignore		      nil	nil	nil)
    (jpeg    octet-decode-image	      nil	jpeg	nil)
    (gif     octet-decode-image	      nil	gif	nil)
    (png     octet-decode-image	      nil	png	nil)
    (tiff    octet-decode-image	      nil	tiff	nil)
    (guess   octet-filter-guess	      nil	nil	nil)
    (lzh     octet-filter-call1	      "lha"	("-v")	text)
    (tar     octet-tar-mode	      nil	nil	nil)
    (pdf     octet-filter-call2	     "pdftotext" ("-q" "-eucjp" "-raw") text))
  "Alist of type-to-filter-program.
Each element should have the form like:
\(TYPE FUNCTION FILTER_PROGRAM ARGUMENT NEW-TYPE\)
nil in NEW-TYPE means filtering is completed.")

(defvar octet-find-file-hook nil)

(defvar octet-attachments nil)
(make-variable-buffer-local 'octet-attachments)

(defun octet-render-html (&rest args)
  (funcall octet-html-render-function (point-min) (point-max))
  0)

(defun octet-decode-text (&rest args)
  (let ((string (buffer-string)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (decode-coding-string string 'undecided)))
  0)

;;; HTML rendering by w3m.el
(defun w3m-about-octet-attachments (url &optional no-decode no-cache
					&rest args)
  (let (buffer attachments pair)
    (set-buffer-multibyte nil)
    (when (string-match "\\`about://octet-attachments/\\([^/]+\\)/" url)
      (setq buffer (get-buffer (base64-decode-string (match-string 1 url)))
	    url (substring url (match-end 0))
	    attachments (with-current-buffer buffer
			  octet-attachments))
      (when (and buffer attachments
		 (setq pair (assoc url attachments)))
	(insert (cdr pair)))))
  (car (funcall (symbol-function 'w3m-local-file-type) url)))

(defun octet-w3m-region (beg end)
  (let ((w3m-display-inline-images t)
	(w3m-url-hierarchical-schemes '("about")))
    (funcall (symbol-function 'w3m-region)
	     beg end (concat "about://octet-attachments/"
			     (base64-encode-string
			      (string-as-unibyte
			       (buffer-name (current-buffer))) "/")))
    (setq octet-attachments nil))
  0)

;; Decode image
(static-cond
 ((featurep 'xemacs)
  (defun octet-decode-image (ignore &rest args)
    (let (glyph)
      (if (memq (car args) (image-instantiator-format-list))
	  (progn
	    (setq glyph (make-glyph (vector (car args) :data (buffer-string))))
	    (if glyph
		(progn (erase-buffer)
		       (set-extent-end-glyph
			(make-extent (point-min)(point-min))
			glyph)
		       0)
	      1))
	1))))
 (t
  (defun octet-decode-image (ignore &rest args)
    (let (image)
      (if (image-type-available-p (car args))
	  (progn
	    (setq image (create-image (buffer-string) (car args) 'data))
	    (if image
		(progn (erase-buffer)
		       (insert-image image) 0)
	      1))
	1)))))

(defun octet-decode-u8-text (&rest args)
  (let ((string (buffer-string)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (decode-coding-string string 'utf-8)))
  0)

(defun octet-filter-call2 (filter &optional args)
  "Call octed filter with two arguments (infile, outfile).
Current buffer content is replaced.
Returns 0 if succeed."
  (let ((infile (file-name-nondirectory
		 (make-temp-file (expand-file-name "octet"
						   octet-temp-directory))))
	(outfile (file-name-nondirectory
		  (make-temp-file (expand-file-name "octet"
						    octet-temp-directory))))
	(last-dir default-directory)
	result)
    (cd octet-temp-directory)
    (write-region-as-binary (point-min) (point-max) infile nil 'no-msg)
    (unwind-protect
	(progn
	  (as-binary-process
	   (setq result (apply 'call-process filter nil nil nil
			       (append args (list infile outfile)))))
	  (when (and (numberp result)
		     (zerop result))
	    (erase-buffer)
	    (insert-file-contents-as-binary outfile))
	  0)
      (if (file-exists-p infile) (delete-file infile))
      (if (file-exists-p outfile) (delete-file outfile))
      (cd last-dir))))

(defun octet-filter-call2-extra (filter &optional args)
  "Call octed filter with two arguments (infile, outfile).
Current buffer content is replaced.
Also, exta attachments are collected to `octet-attachments'.
Returns 0 if succeed."
  (let ((infile (file-name-nondirectory
		 (make-temp-file (expand-file-name "octet"
						   octet-temp-directory))))
	(outfile (file-name-nondirectory
		  (make-temp-file (expand-file-name "octet"
						    octet-temp-directory))))
	(last-dir default-directory)
	result)
    (cd octet-temp-directory)
    (write-region-as-binary (point-min) (point-max) infile nil 'no-msg)
    (unwind-protect
	(progn
	  (as-binary-process
	   (setq result (apply 'call-process filter nil nil nil
			       (append args (list infile outfile)))))
	  (when (and (numberp result)
		     (zerop result))
	    (erase-buffer)
	    (insert-file-contents-as-binary outfile)
	    (dolist (attach (directory-files "." nil (concat
						      (regexp-quote outfile)
						      ".*\\..*")))
	      (setq octet-attachments
		    (cons (cons
			   attach
			   (with-temp-buffer
			     (insert-file-contents-as-binary attach)
			     (buffer-string)))
			  octet-attachments))
	      (if (file-exists-p attach) (delete-file attach))
	      ))
	  0)
      (if (file-exists-p infile) (delete-file infile))
      (if (file-exists-p outfile) (delete-file outfile))
      (cd last-dir))))

(defun octet-filter-call1 (filter &optional args)
  "Call external octed filter with two arguments (infile) and obtain stdout.
Current buffer content is replaced.
Returns 0 if succeed."
  (let ((infile (file-name-nondirectory
		 (make-temp-file (expand-file-name "octet"
						   octet-temp-directory))))
	(last-dir default-directory)
	result)
    (cd octet-temp-directory)
    (write-region-as-binary (point-min) (point-max) infile nil 'no-msg)
    (unwind-protect
	(progn
	  (erase-buffer)
	  (as-binary-process
	   (setq result (apply 'call-process filter nil t nil
			       (append args (list infile)))))
	  (if (numberp result) result 1))
      (if (file-exists-p infile) (delete-file infile))
      (cd last-dir))))

(defun octet-filter-guess (&rest args)
  (let (buffer-file-name)
    (octet-buffer)
    0))

(defun octet-tar-mode (&rest args)
  (funcall (symbol-function 'tar-mode))
  0)

(defun octet-guess-type-from-name (name)
  (when (string-match "\\.\\([a-z0-9]+\\)\\'" name)
    (cdr (assoc (downcase (match-string 1 name))
		octet-suffix-type-alist))))

(defun octet-guess-type-from-content-type (content-type)
  (let ((alist octet-content-type-alist)
	type)
    (while alist
      (when (string-match (car (car alist)) content-type)
	(setq type (cdr (car alist))
	      alist nil))
      (setq alist (cdr alist)))
    type))

(defun octet-guess-type-from-magic ()
  (let ((rest octet-magic-type-alist)
	type subtype)
    (goto-char (point-min))
    (while (not (let ((cell (car rest)))
		  (if cell
		      (if (looking-at (car cell))
			  (setq type (nth 1 cell)
				subtype (nth 2 cell)))
		    t)))
      (setq rest (cdr rest)))
    (if type
	(octet-guess-type-from-content-type
	 (concat (symbol-name type) "/" (symbol-name subtype))))))

(defun octet-filter-buffer (type)
  "Call a filter function in `octet-type-filter-alist'.
TYPE is the symbol of type.
Returns NEW-TYPE."
  (let ((elem (assq type octet-type-filter-alist)))
    (if (zerop (apply (nth 1 elem) (list (nth 2 elem) (nth 3 elem))))
	(nth 4 elem))))

;;;###autoload
(defun octet-buffer (&optional name content-type)
  "View octet-stream content according to `octet-type-filter-alist'.
Optional NAME is the filename.
If optional CONTENT-TYPE is specified, it is used for type guess."
  (interactive)
  (let ((type (or (and content-type
		       (octet-guess-type-from-content-type
			content-type))
		  (octet-guess-type-from-magic)
		  (and (or name buffer-file-name)
		       (octet-guess-type-from-name
			(or name buffer-file-name)))
		  (intern (condition-case nil
			      (completing-read "Octet Type(text): "
					       (mapcar
						(lambda (pair)
						  (list (symbol-name
							 (cdr pair))))
						octet-suffix-type-alist)
					       nil 'require-match nil nil
					       "text")
			    (quit "text"))))))
    (while (setq type (octet-filter-buffer type)))))

(static-if (featurep 'xemacs)
    (defun octet-insert-buffer (from)
      "Insert after point the contents of BUFFER and the image."
      (let (extent glyph)
	(with-current-buffer from
	  (if (setq extent (extent-at (point-min) nil nil nil 'at))
	      (setq glyph (extent-end-glyph extent))))
	(insert-buffer-substring from)
	(if glyph
	    (set-extent-end-glyph (make-extent (point) (point))
				  glyph))))
  (defalias 'octet-insert-buffer 'insert-buffer))

;;;###autoload
(defun octet-find-file (file)
  "Find FILE with octet-stream decoding."
  (interactive "fFilename: ")
  (as-binary-input-file	(find-file file))
  (unwind-protect
      (let (buffer-read-only)
	(octet-buffer))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (auto-save-mode -1)
    (setq buffer-read-only t
	  truncate-lines t)
    (run-hooks 'octet-find-file-hook)))

;;;
;; Functions for SEMI.
;;

(defvar mime-preview-octet-hook nil)
(defvar mime-view-octet-hook nil)

;;;###autoload
(defun mime-preview-octet (entity situation)
  "A method for mime-view to preview octet message."
  (goto-char (point-max))
  (let ((p (point))
	(name (mime-entity-filename entity))
	from-buf to-buf)
    (insert "\n")
    (goto-char p)
    (save-restriction
      (narrow-to-region p p)
      (setq to-buf (current-buffer))
      (with-temp-buffer
	(setq from-buf (current-buffer))
	(w3m-insert-string (mime-entity-content entity))
	(octet-buffer name (mime-entity-type/subtype entity))
	(with-current-buffer to-buf
	  (octet-insert-buffer from-buf)
	  (run-hooks 'mime-preview-octet-hook))))))

;;;###autoload
(defun mime-view-octet (entity situation)
  "A method for mime-view to display octet message."
  (let (type subtype)
    (let ((mdata (mime-entity-content entity))
	  (rest octet-magic-type-alist))
      (while (not (let ((cell (car rest)))
		    (if cell
			(if (string-match (car cell) mdata)
			    (setq type (nth 1 cell)
				  subtype (nth 2 cell)))
		      t)))
	(setq rest (cdr rest)))
      (if type
	  (progn
	    (setq situation (del-alist 'method (copy-alist situation)))
	    (funcall (symbol-function 'mime-play-entity)
		     entity
		     (put-alist 'type type
				(put-alist 'subtype subtype
					   situation))
		     'mime-view-octet))
	(let ((buf (get-buffer-create
		    (format "%s-%s" (buffer-name) (mime-entity-number entity))))
	      (name (mime-entity-filename entity)))
	  (with-current-buffer buf
	    (set-buffer-multibyte nil)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (w3m-insert-string mdata)
	    (octet-buffer name (mime-entity-type/subtype entity))
	    (setq buffer-read-only t
		  truncate-lines t)
	    (set-buffer-multibyte t)
	    (set-buffer-modified-p nil))
	  (let ((win (get-buffer-window (current-buffer))))
	    (or (eq (selected-window) win)
		(select-window (or win (get-largest-window)))))
	  (view-buffer buf)
	  (run-hooks 'mime-view-octet-hook)
	  (goto-char (point-min)))))))

;;;###autoload
(defun octet-mime-setup ()
  "Octet setting for MIME module."
  (eval-after-load "mime-view"
    '(progn
       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . msword)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . excel)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . x-msexcel)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . vnd.ms-excel)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . vnd.ms-powerpoint)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . octet-stream)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-preview-condition
	'((type . application)(subtype . t)
	  (encoding . t)
	  (body . invisible)
	  (body-presentation-method . mime-preview-octet)))
       ;; another condition?
       )))

(provide 'octet)

;;; octet.el ends here
