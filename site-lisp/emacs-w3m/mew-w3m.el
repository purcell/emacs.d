;; mew-w3m.el --- View Text/Html content with w3m in Mew

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Shun-ichi GOTO  <gotoh@taiyo.co.jp>,
;;         Hideyuki SHIRAI <shirai@meadowy.org>
;; Created: Wed Feb 28 03:31:00 2001
;; Version: $Revision: 1.69 $
;; Keywords: Mew, mail, w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is for viewing formatted (rendered) Text/Html content
;; in Mew's message buffer.

;;; Installation:

;; (1) Simply load this file and add followings in your ~/.mew file.
;;
;; (require 'mew-w3m)
;;
;; (2) And you can use keymap of w3m-mode as mew-w3m-minor-mode.
;; To activate this feaeture, add followings also:
;;
;; (setq mew-use-w3m-minor-mode t)
;; (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
;;
;; (3) If you use mew-1.95b118 or later on which Emacs 21, 22 or XEmacs,
;; can display the images in the Text/Html message.
;; To activate this feaeture, add following in your ~/.mew file.
;;
;; (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)
;;
;; Press "T":    Toggle the visibility of the images included its message only.
;; Press "C-uT": Display the all images included its Text/Html part."
;;
;; (4) You can use emacs-w3m to fetch and/or browse
;; `external-body with URL access'. To activate this feaeture,
;; add followings also:
;;
;; (setq mew-ext-url-alist
;;      '(("^application/" "Fetch by emacs-w3m" mew-w3m-ext-url-fetch nil)
;;        (t "Browse by emacs-w3m" mew-w3m-ext-url-show nil)))
;;  or
;; (setq mew-ext-url-alist
;;      '((t "Browse by emacs-w3m" mew-w3m-ext-url-show nil)))
;;

;;; Usage:

;; There's nothing special. Browse messages in usual way.
;; On viewing Text/Html file, rendered text is appeared in message
;; buffer instead of usual "HTML" banner.
;; C-c C-e operation is also allowed to view with external browser.
;;
;; If mew-use-w3m-minor-mode is t, key operations of w3m-mode is
;; allowed (as minor-mode-map) and jump links in message buffer.
;; NOTE: This feature is not complete. You may confuse.
;;
;;

;;; Code:

(require 'mew)
(require 'w3m)
(eval-when-compile (require 'cl))

;;; initializer for mew
(defgroup mew-w3m nil
  "mew-w3m - Inline HTML rendering extension of Mew"
  :group 'w3m)

(defcustom mew-use-w3m-minor-mode nil
  "*Use w3m minor mode in message buffer.
Non-nil means that the minor mode whose keymap contains keys binded to
some emacs-w3m commands are activated in message buffer, when viewing
Text/Html contents."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-auto-insert-image nil
  "*If non-nil, images are inserted automatically in Multipart/Related message.
This variable is effective only in XEmacs, Emacs 21 and Emacs 22."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-cid-retrieve-hook nil
  "*Hook run after cid retrieved"
  :group 'mew-w3m
  :type 'hook)

(defcustom mew-w3m-region-cite-mark "&gt;&nbsp;"
  "*Method of converting `blockquote'."
  :group 'mew-w3m
  :type '(choice (const :tag "Use Indent" nil)
		 (const :tag "Use Cite Mark \"> \"" "&gt;&nbsp;")
		 (string :tag "Use Other Mark")))

(defconst mew-w3m-safe-url-regexp "\\`cid:")

;; Avoid bytecompile error and warnings.
(eval-when-compile
  (defvar mew-use-text/html)
  (unless (fboundp 'mew-current-get-fld)
    (autoload 'mew-coding-system-p "mew")
    (autoload 'mew-current-get-fld "mew")
    (autoload 'mew-current-get-msg "mew")
    (autoload 'mew-syntax-get-entry-by-cid "mew")
    (defun mew-cache-hit (&rest args) ())))

(defmacro mew-w3m-add-text-properties (props)
  `(add-text-properties (point-min)
			(min (1+ (point-min)) (point-max))
			,props))

(defun mew-w3m-minor-mode-setter ()
  "Check message buffer and activate w3m-minor-mode."
  (w3m-minor-mode (or (and (get-text-property (point-min) 'w3m)
			   mew-use-w3m-minor-mode)
		      0)))

(defvar mew-w3m-use-safe-url-regexp t)

(defun mew-w3m-view-inline-image (&optional allimage)
  "Display the images of Text/Html part.
\\<mew-summary-mode-map>
'\\[mew-w3m-view-inline-image]'	Toggle display the images included its message only.
'\\[universal-argument]\\[mew-w3m-view-inline-image]'	Display the all images included its Text/Html part."
  (interactive "P")
  (mew-summary-msg-or-part
   (if allimage
       (let ((mew-use-text/html t)
	     (mew-w3m-auto-insert-image t)
	     (mew-w3m-use-safe-url-regexp nil))
	 (mew-summary-display 'force))
     (with-current-buffer (mew-buffer-message)
       (let* ((image (get-text-property (point-min) 'w3m-images))
	      (w3m-display-inline-images image)
	      (w3m-safe-url-regexp (when mew-w3m-use-safe-url-regexp
				     mew-w3m-safe-url-regexp)))
	 (w3m-toggle-inline-images)
	 (mew-elet
	  (mew-w3m-add-text-properties `(w3m-images ,(not image)))
	  (set-buffer-modified-p nil)))))))

(defun mew-w3m-region (start end &optional url charset)
  "w3m-region with inserting the cite mark."
  (if (null mew-w3m-region-cite-mark)
      (w3m-region start end url charset)
    (save-restriction
      (narrow-to-region start end)
      (let ((case-fold-search t)
	    pos lines tagbeg0 tagend0 tagbeg1 tagend1)
	(goto-char (point-min))
	(while (w3m-search-tag "blockquote")
	  (setq tagbeg0 (match-beginning 0))
	  (setq tagend0 (match-end 0))
	  (when (w3m-search-tag "/blockquote")
	    (setq tagbeg1 (match-beginning 0))
	    (setq tagend1 (match-end 0))
	    (setq lines (buffer-substring tagend0 tagbeg1))
	    (delete-region tagbeg0 tagend1)
	    (insert (with-temp-buffer
		      (insert lines)
		      (goto-char (point-min))
		      (if (and (w3m-search-tag "pre")
			       (setq tagbeg0 (match-beginning 0))
			       (setq tagend0 (match-end 0))
			       (w3m-search-tag "/pre")
			       (setq tagbeg1 (match-beginning 0))
			       (setq tagend1 (match-end 0)))
			  (progn
			    (delete-region tagbeg1 tagend1)
			    (delete-region tagbeg0 tagend0))
			;; delete <br>
			(goto-char (point-min))
			(while (w3m-search-tag "br")
			  (delete-region (match-beginning 0) (match-end 0))
			  (unless (looking-at "[\n\r]") (insert "\n"))))
		      (goto-char (point-max))
		      (skip-chars-backward " \t\n\f\r")
		      (delete-region (point) (point-max))
		      (goto-char (point-min))
		      (skip-chars-forward " \t\n\f\r")
		      (delete-region (point-min) (point))
		      (goto-char (point-min))
		      (while (not (eobp))
			(insert mew-w3m-region-cite-mark)
			(forward-line 1))
		      (goto-char (point-min))
		      (insert "<pre>\n")
		      (goto-char (point-max))
		      (insert "\n</pre>\n")
		      (buffer-substring (point-min) (point-max)))))))
      (w3m-region (point-min) (point-max) url charset))))

;; processing Text/Html contents with w3m.
(defun mew-mime-text/html-w3m (&rest args)
  "View Text/Html contents with w3m rendering output."
  (let ((w3m-display-inline-images mew-w3m-auto-insert-image)
	(w3m-safe-url-regexp (when mew-w3m-use-safe-url-regexp
			       mew-w3m-safe-url-regexp))
	w3m-force-redisplay	;; don't redraw
	charset wcs xref
	cache begin end params execute)
    (if (= (length args) 2)
	;; Mew-2
	(setq begin (nth 0 args) end (nth 1 args))
      ;; Old Mew
      (setq cache (nth 0 args))
      (setq begin (nth 1 args))
      (setq end (nth 2 args))
      (setq params (nth 3 args))
      (setq execute (nth 4 args)))
    (if (and cache (or execute (<= end begin)))
	;; 'C-cC-e' + Old Mew
	(apply 'mew-mime-text/html (list cache begin end params execute))
      (save-excursion
	;; search Xref: Header in SHIMBUN article
	(when cache (set-buffer cache))
	(goto-char (point-min))
	(when (re-search-forward mew-eoh nil t)
	  (let ((eoh (point))
		(case-fold-search t))
	    (goto-char (point-min))
	    (when (and (re-search-forward "^X-Shimbun-Id: " eoh t)
		       (goto-char (point-min))
		       (re-search-forward "^Xref: \\(.+\\)\n" eoh t))
	      (setq xref (match-string 1))
	      (w3m-static-if (fboundp 'match-string-no-properties)
		  (setq xref (match-string-no-properties 1))
		(setq xref (match-string 1))
		(set-text-properties 0 (length xref) nil xref))))))
      (mew-elet
       (cond
	((and (null cache) (eq w3m-type 'w3m-m17n))
	 ;; Mew-2 + w3m-m17n.
	 ;; Coding-system and charset are decided by Mew.
	 (let ((w3m-input-coding-system w3m-input-coding-system)
	       (w3m-output-coding-system w3m-output-coding-system)
	       (w3m-halfdump-command-arguments w3m-halfdump-command-arguments))
	   (when (setq charset (mew-charset-guess-region begin end))
	     (setq wcs (mew-charset-to-cs charset)))
	   (when (and charset wcs (mew-coding-system-p wcs))
	     ;; guess correctly and not us-ascii
	     (setq w3m-input-coding-system wcs)
	     (setq w3m-output-coding-system wcs)
	     (setq w3m-halfdump-command-arguments
		   (list "-halfdump"
			 "-I" charset "-O" charset
			 "-o" "ext_halfdump=1"
			 "-o" "pre_conv=1"
			 "-o" "strict_iso2022=0")))
	   (mew-w3m-region begin end xref)))
	((null cache)	;; Mew-2 + w3m, w3mmee
	 (mew-w3m-region begin end xref (mew-charset-guess-region begin end)))
	(t		;; Old Mew
	 (setq charset (or (mew-syntax-get-param params "charset")
			   (with-current-buffer cache
			     (mew-charset-guess-region begin end))))
	 (if charset
	     (setq wcs (mew-charset-to-cs charset))
	   (setq wcs mew-cs-text-for-write))
	 (mew-frwlet
	     mew-cs-dummy wcs
	   (mew-w3m-region (point)
			   (progn (insert-buffer-substring cache begin end)
				  (point))
			   xref))))
       (mew-w3m-add-text-properties `(w3m t w3m-images ,mew-w3m-auto-insert-image))))))

(defvar w3m-mew-support-cid (and (boundp 'mew-version-number)
				 (fboundp 'mew-syntax-get-entry-by-cid)))

(defun mew-w3m-cid-retrieve (url &rest args)
  (let ((output-buffer (current-buffer)))
    (with-current-buffer w3m-current-buffer
      (when (and w3m-mew-support-cid
		 (string-match "^cid:\\(.+\\)" url))
	(setq url (match-string 1 url))
	(let* ((fld (mew-current-get-fld (mew-frame-id)))
	       (msg (mew-current-get-msg (mew-frame-id)))
	       (cache (mew-cache-hit fld msg 'must-hit))
	       (syntax (mew-cache-decode-syntax cache))
	       cidstx beg end)
	  (if (string< "4.0.53" mew-version-number)
	      (setq cidstx (mew-syntax-get-entry-by-cid syntax (concat "<" url ">")))
	    (setq cidstx (mew-syntax-get-entry-by-cid syntax url)))
	  (when cidstx
	    (setq beg (mew-syntax-get-begin cidstx))
	    (setq end (mew-syntax-get-end cidstx))
	    (prog1
		(with-current-buffer output-buffer
		  (set-buffer-multibyte t)
		  (insert-buffer-substring cache beg end)
		  (set-buffer-multibyte nil)
		  (downcase (car (mew-syntax-get-ct cidstx))))
	      (run-hooks 'mew-w3m-cid-retrieve-hook))))))))

(when w3m-mew-support-cid
  (push (cons 'mew-message-mode 'mew-w3m-cid-retrieve)
	w3m-cid-retrieve-function-alist))

(defun mew-w3m-ext-url-show (dummy url)
  (pop-to-buffer (mew-buffer-message))
  (w3m url))

(defun mew-w3m-ext-url-fetch (dummy url)
  (lexical-let ((url url)
		(name (file-name-nondirectory url))
		handler)
    (w3m-process-do
	(success (prog1
		     (w3m-download url nil nil handler)
		   (message "Download: %s..." name)))
      (if success
	  (message "Download: %s...done" name)
	(message "Download: %s...failed" name))
      (sit-for 1))))

(defun w3m-mail-compose-with-mew (source url charset content-type
					 to subject other-headers)
  "Compose a mail using Mew."
  (when (one-window-p)
    (split-window))
  (select-window (next-window))
  (condition-case nil
      (unless (and (boundp 'mew-init-p) mew-init-p
		   (progn
		     (mew-summary-jump-to-draft-buffer)
		     (and (eq major-mode 'mew-draft-mode)
			  (y-or-n-p "Attatch this draft? "))))
	(mew-user-agent-compose to subject other-headers))
    (quit
     (if (y-or-n-p "Create new draft? ")
	 (mew-user-agent-compose to subject other-headers)
       (delete-window)
       (error "Abort mail composing"))))
  (let* ((basename (file-name-nondirectory (w3m-url-strip-query url)))
	 (ct (downcase  content-type))
	 (mew-attach-move-next-after-copy nil)
	 (i 1)
	 (pos -1)
	 (csorig (mew-charset-to-cs (symbol-name charset)))
	 last filename cs)
    (unless (mew-attach-p)
      (mew-draft-prepare-attachments))
    ;; goto last attachment
    (setq last (catch 'last
		 (while (not (= pos (point)))
		   (setq i (1+ i))
		   (mew-attach-goto-number 'here `(,i))
		   (when (mew-attach-line-lastp)
		     (throw 'last t)))))
    (when (eq csorig mew-cs-unknown)
      (setq csorig nil))
    (if (or (not last) (not (mew-attach-not-line012-1)))
	(message "Can not attach from emacs-w3m here!")
      ;; Application/.*xml is not inline view with Mew.
      (cond
       ((string= "application/xhtml+xml" ct)
	(setq ct "text/html"))
       ((string-match "^application/.*xml$" ct)
	(setq ct "text/xml")))
      (setq filename (expand-file-name (cond
					((and (string-match "^[\t ]*$" basename)
					      (string= ct "text/html"))
					 "index.html")
					((and (string-match "^[\t ]*$" basename)
					      (string= ct "text/xml"))
					 "index.xml")
					((string-match "^[\t ]*$" basename)
					 "dummy")
					(t
					 basename))
				       mew-temp-dir))
      (with-temp-buffer
	(cond
	 ((string= "text/html" ct)
	  (insert source)
	  (setq cs (w3m-static-if (fboundp 'mew-text/html-detect-cs)
		       (mew-text/html-detect-cs (point-min) (point-max))))
	  (when (or (eq cs mew-cs-unknown) (not cs))
	    (cond
	     (csorig
	      (setq cs csorig))
	     (t
	      (setq cs mew-cs-autoconv)))))
	 ((string= "text/xml" ct)
	  (insert source)
	  (setq cs (w3m-static-if (fboundp 'mew-text/html-detect-cs)
		       (mew-text/html-detect-cs (point-min) (point-max))))
	  (when (or (eq cs mew-cs-unknown) (not cs))
	    (cond
	     (csorig
	      (setq cs csorig))
	     ((mew-coding-system-p 'utf-8)
	      (setq cs 'utf-8))
	     (t
	      (setq cs mew-cs-autoconv)))))
	 ((string-match "^text/" ct)
	  (insert source)
	  (setq cs mew-cs-autoconv))
	 (t
	  (mew-set-buffer-multibyte nil)
	  (insert source)
	  (setq cs mew-cs-binary)))
	(setq charset (cond
		       ((eq cs mew-cs-autoconv)
			(mew-charset-guess-region (point-min) (point-max)))
		       ((eq cs mew-cs-binary)
			nil)
		       (t
			(mew-cs-to-charset cs))))
	(mew-frwlet
	    mew-cs-text-for-read cs
	  (write-region (point-min) (point-max) filename nil 'nomsg)))
      (when ct
	(setq ct (mew-capitalize ct)))
      (mew-attach-copy filename (file-name-nondirectory filename))
      ;; content-type check & set
      (let* ((nums (mew-syntax-nums))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (file (mew-syntax-get-file syntax))
	     (ctl (mew-syntax-get-ct syntax))
	     (ct-orig (mew-syntax-get-value ctl 'cap))
	     cte)
	(unless (string= ct ct-orig)
	  (setq ctl (list ct))
	  (mew-syntax-set-ct syntax ctl)
	  (setq cte (mew-ctdb-cte (mew-ctdb-by-ct ct)))
	  (mew-syntax-set-cte syntax cte)
	  (mew-syntax-set-cdp syntax (mew-syntax-cdp-format ct file))
	  (mew-encode-syntax-print mew-encode-syntax)))
      ;; charset set
      (let* ((nums (mew-syntax-nums))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (file (mew-syntax-get-file syntax))
	     (ctl (mew-syntax-get-ct syntax))
	     (ct (mew-syntax-get-value ctl 'cap))
	     (params (mew-syntax-get-params ctl))
	     (ocharset "charset"))
	(when (and (string-match "^Text" ct) charset)
	  (setq params (mew-delete ocharset params))
	  (setq ctl (cons ct (cons (list ocharset charset) params)))
	  (mew-syntax-set-ct syntax ctl))
	(mew-syntax-set-cd syntax url)
	(mew-encode-syntax-print mew-encode-syntax))
      (message "Compose a mail using Mew with %s...done" url)
      (when (and (file-exists-p filename) (file-writable-p filename))
	(delete-file filename)))))

;;;
(provide 'mew-w3m)

;; mew-w3m.el ends here
