;;; shimbun.el --- interfacing with web newspapers -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: news

;; This file is the main part of shimbun.

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;; Shimbun API:
;;
;; shimbun-open
;; shimbun-server
;; shimbun-groups
;; shimbun-current-group
;; shimbun-open-group
;; shimbun-close-group
;; shimbun-headers
;; shimbun-reply-to
;; shimbun-x-face
;; shimbun-header-insert
;; shimbun-search-id
;; shimbun-article-expiration-days
;; shimbun-article
;; shimbun-close

;; Shimbun Header API:
;;
;; shimbun-header-subject
;; shimbun-header-set-subject
;; shimbun-header-from
;; shimbun-header-set-from
;; shimbun-header-date
;; shimbun-header-set-date
;; shimbun-header-id
;; shimbun-header-set-id
;; shimbun-header-references
;; shimbun-header-set-references
;; shimbun-header-chars
;; shimbun-header-set-chars
;; shimbun-header-lines
;; shimbun-header-set-lines
;; shimbun-header-xref
;; shimbun-header-set-xref
;; shimbun-header-extra
;; shimbun-header-set-extra

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'mcharset)
(require 'eword-encode)
(require 'luna)
(require 'std11)
(require 'w3m)

(eval-and-compile
  (luna-define-class shimbun ()
		     (mua server current-group groups
			  x-face x-face-alist
			  url coding-system from-address
			  content-start content-end
			  expiration-days server-name
			  ;; Say whether to prefer text/plain articles.
			  prefer-text-plain
			  ;; Similar to `content-start' and `content-end'
			  ;; but are used to extract html contents for
			  ;; text/plain articles.
			  text-content-start text-content-end
			  ;; Say whether to convert Japanese zenkaku
			  ;; ASCII chars into hankaku.
			  japanese-hankaku
			  ;; Coding system for encoding URIs when
			  ;; accessing the server.
			  url-coding-system
			  ;; Number of times to retry fetching contents.
			  retry-fetching))
  (luna-define-internal-accessors 'shimbun))

(defgroup shimbun nil
  "shimbun - the backend library to read web newspapers."
  :group 'w3m
  :group 'hypermedia)

(defcustom shimbun-x-face
  "X-Face: @Q+y!*#)K`rvKfSQnCK.Q\\{T0qs@?plqxVu<=@H-y\
22NlKSprlFiND7{\"{]&Ddg1=P6{Ze|\n xbW2L1p5ofS\\&u~28A\
dJrT4Cd<Ls?U!G4}0S%FA~KegR;YZWieoc%`|$4M\\\"i*2avWm?"
  "*Default X-Face field for shimbun."
  :group 'shimbun
  :type '(string :format "%{%t%}:\n%v" :size 0))

(defcustom shimbun-server-additional-path nil
  "*List of additional directories to search for shimbun servers."
  :group 'shimbun
  :type '(repeat (directory :format "%t: %v\n" :size 0)))

(defcustom shimbun-checking-new-news-format "Checking new news on #S for #g"
  "*Format string used to show a progress message while chacking new news.
See `shimbun-message' for the special format specifiers."
  :group 'shimbun
  :type '(string :format "%{%t%}:\n%v" :size 0))

(defcustom shimbun-verbose t
  "*Flag controls whether shimbun should be verbose.
If it is non-nil, the `w3m-verbose' variable will be bound to nil
while shimbun is waiting for a server's response."
  :group 'shimbun
  :type 'boolean)

(defcustom shimbun-message-enable-logging nil
  "*Non-nil means preserve echo messages in the *Message* buffer."
  :group 'shimbun
  :type 'boolean)

(defcustom shimbun-japanese-hankaku nil
  "Non-nil means convert Japanese zenkaku ASCII chars into hankaku.
A non-nil value of this variable affects all shimbun articles except
ones fetched by shimbun modules that override the `shimbun-headers'
method or the `shimbun-clear-contents' method.  Valid values include:

`header' or `subject':
    Perform the hankaku conversion on only subjects.
`body':
    Perform the hankaku conversion on only bodies.
non-nil values excluding `header', `subject', `body', and `never':
    Perform the hankaku conversion on both subjects and bodies.
nil:
    Don't perform the hankaku conversion.
`never':
    Never perform the hankaku conversion.

Another way is to set `shimbun-SERVER-japanese-hankaku' to non-nil per
SERVER.  If you want to perform the hankaku conversion on articles
except ones fetched from SERVER for example, set this variable to t
and set `shimbun-SERVER-japanese-hankaku' to `never'."
  :group 'shimbun
  :type '(radio
	  (sexp :format "Header only\n" :value header
		:match
		(lambda (widget value)
		  (memq value '(header subject))))
	  (const :tag "Body only" body)
	  (sexp :format "Header and Body\n" :value t
		:match
		(lambda (widget value)
		  (and value (not (memq value '(header subject body never))))))
	  (const :tag "Don't convert" nil)
	  (const :tag "Never convert" never)))

(defcustom shimbun-retry-fetching nil
  "Number of times to retry fetching the web contents of a url.
If it is a positive number and the fetching of the contents of a url
fails, it will be retried until it is successful or until the number
of times to retry reaches to that number.  Note that a non-nil value
of `shimbun-SERVER-retry-fetching' overrides this variable."
  :group 'shimbun
  :type '(radio (const :format "Don't retry " nil)
		(integer :tag "Number of retries"
			 :match (lambda (widget value) (natnump value))
			 :value 1)))

(defcustom shimbun-use-local nil
  "Specifies if local files should be used (\"offline\" mode).
This way, you can use an external script to retrieve the
necessary HTML/XML files.  For an example, see
`nnshimbun-generate-download-script'.  If a local file for an URL
cannot be found, it will silently be retrieved as usual."
  :group 'shimbun
  :type 'boolean)

(defcustom shimbun-local-path w3m-default-save-directory
  "Directory where local shimbun files are stored.
Default is the value of `w3m-default-save-directory'."
  :group 'shimbun
  :type 'directory)

(defun shimbun-servers-list ()
  "Return a list of shimbun servers."
  (let (servers)
    (dolist (dir (cons (file-name-directory (locate-library "shimbun"))
		       shimbun-server-additional-path))
      (when (file-directory-p dir)
	(dolist (file (directory-files dir nil nil t))
	  (and (string-match "\\`sb-\\(.*\\)\\.elc?\\'" file)
	       (not (member (setq file (match-string 1 file))
			    '("fml" "glimpse" "lump" "mailarc"
			      "mailman" "mhonarc" "text" "hash"
			      "rss" "atom" "multi")))
	       (not (member file servers))
	       (push file servers)))))
    (sort servers 'string-lessp)))

(defun shimbun-servers-alist ()
  "Return an associative list of shimbun servers."
  (mapcar 'list (shimbun-servers-list)))

;;; Shimbun MUA
(eval-and-compile
  (luna-define-class shimbun-mua () (shimbun))
  (luna-define-internal-accessors 'shimbun-mua))

(luna-define-generic shimbun-mua-search-id (mua id)
  "Return non-nil when MUA found a message structure which corresponds to ID.")

(defun shimbun-mua-shimbun (mua)
  "Return the shimbun object created by MUA."
  (shimbun-mua-shimbun-internal mua))

;;; emacs-w3m implementation of url retrieval and entity decoding.
(defun shimbun-retrieve-url (url &optional no-cache no-decode
				 referer url-coding-system)
  "Rertrieve URL contents and insert to current buffer.
Return content-type of URL as string when retrieval succeeded.
Non-ASCII characters `url' are escaped based on `url-coding-system'."
  (let (type charset fname)
    (if (and url
	     shimbun-use-local
	     shimbun-local-path
	     (file-regular-p
	      (setq fname (concat (file-name-as-directory
				   (expand-file-name shimbun-local-path))
				  (substring (md5 url) 0 10)
				  "_shimbun"))))
	;; get local file contents
	(progn
	  (let ((coding-system-for-read 'no-conversion))
	    (insert-file-contents fname))
	  (when (re-search-forward "^$" nil t)
	    (let ((pos (match-beginning 0)))
	      (re-search-backward
	       "^Content-Type: \\(.*?\\)\\(?:[ ;]+\\|$\\)\\(charset=\\(.*\\)\\)?"
	       nil t)
	      (setq type (match-string 1)
		    charset (match-string 3))
	      (delete-region (point-min) pos))))
      ;; retrieve URL
      (when url
	(setq type (w3m-retrieve
		    (w3m-url-transfer-encode-string url url-coding-system)
		    nil no-cache nil referer))))
    (if type
	(progn
	  (unless no-decode
	    (if charset
		(w3m-decode-buffer url charset type)
	      (w3m-decode-buffer url))
	    (goto-char (point-min)))
	  type)
      (unless no-decode
	(set-buffer-multibyte t)
	nil))))

(luna-define-generic shimbun-retry-fetching (shimbun)
  "Return the number of times to retry fetching the web contents of a url.")

(luna-define-method shimbun-retry-fetching ((shimbun shimbun))
  (or (shimbun-retry-fetching-internal shimbun)
      shimbun-retry-fetching))

(defun shimbun-fetch-url (shimbun url &optional no-cache no-decode referer)
  "Retrieve contents specified by URL for SHIMBUN.
This function is exacly similar to `shimbun-retrieve-url', but
considers the `coding-system' slot of SHIMBUN when estimating a
coding system of retrieved contents and the `url-coding-system'
slot of SHIMBUN to encode URL."
  (let* ((coding (shimbun-coding-system-internal shimbun))
	 (w3m-coding-system-priority-list
	  (if coding
	      (cons coding w3m-coding-system-priority-list)
	    w3m-coding-system-priority-list))
	 (retry (shimbun-retry-fetching shimbun)))
    (setq coding (or (shimbun-url-coding-system-internal shimbun) coding))
    (save-restriction
      (narrow-to-region (point) (point))
      (or (shimbun-retrieve-url url no-cache no-decode referer coding)
	  (and retry
	       (let (retval)
		 (shimbun-message
		  shimbun "shimbun: Retrying to fetch contents...")
		 (while (and (> retry 0) (not retval))
		   (delete-region (point-min) (point-max))
		   (setq retval (shimbun-retrieve-url
				 url no-cache no-decode referer coding)
			 retry (1- retry)))
		 (shimbun-message shimbun
				  "shimbun: Retrying to fetch contents...%s"
				  (if retval "done" "failed"))
		 retval))))))

(defun shimbun-real-url (url &optional no-cache)
  "Return a real URL."
  (w3m-real-url url no-cache))

(defalias 'shimbun-decode-anchor-string 'w3m-decode-anchor-string)
(defalias 'shimbun-decode-entities 'w3m-decode-entities)
(defalias 'shimbun-decode-entities-string 'w3m-decode-entities-string)
(defalias 'shimbun-expand-url 'w3m-expand-url)
(defalias 'shimbun-find-coding-system 'w3m-find-coding-system)
(defalias 'shimbun-replace-in-string 'w3m-replace-in-string)
(defalias 'shimbun-url-encode-string 'w3m-url-encode-string)

;;; Implementation of Header API.
(eval-and-compile
  (luna-define-class shimbun-header ()
		     (number subject from date id references
			     chars lines xref extra))
  (luna-define-internal-accessors 'shimbun-header))

(defun shimbun-header-number (header)
  (shimbun-header-number-internal header))

(defun shimbun-header-set-number (header number)
  (shimbun-header-set-number-internal header number))

(defun shimbun-header-subject (header &optional no-encode)
  (if no-encode
      (shimbun-header-subject-internal header)
    (shimbun-mime-encode-string
     (shimbun-header-subject-internal header))))

(defun shimbun-header-normalize (string &optional keep-angle-brackets)
  (when string
    (save-match-data
      ;; This is a trick to keep backward compatibility for
      ;; `shimbun-header-set-subject' and `shimbun-header-set-from'.
      (if (string-match eword-encoded-word-regexp string)
	  (eword-decode-string string)
	(with-temp-buffer
	  (insert string)
	  (unless keep-angle-brackets
	    (shimbun-remove-markup))
	  (shimbun-decode-entities)
	  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\r ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\f ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\n ?\  t)
	  (goto-char (point-min))
	  (skip-chars-forward " ")
	  (buffer-substring (point)
			    (progn
			      (goto-char (point-max))
			      (skip-chars-backward " ")
			      (point))))))))

(defun shimbun-header-set-subject (header subject &optional asis)
  (shimbun-header-set-subject-internal header
				       (if asis
					   subject
					 (shimbun-header-normalize subject))))

(defun shimbun-header-from (header &optional no-encode)
  (if no-encode
      (shimbun-header-from-internal header)
    (shimbun-mime-encode-string
     (shimbun-header-from-internal header))))

(defun shimbun-header-set-from (header from &optional asis)
  (shimbun-header-set-from-internal header
				    (if asis
					from
				      (shimbun-header-normalize from t))))

(defun shimbun-header-date (header)
  (shimbun-header-date-internal header))

(defun shimbun-header-set-date (header date &optional asis)
  (shimbun-header-set-date-internal header (if asis
					       date
					     (shimbun-header-normalize date))))

(defun shimbun-header-id (header)
  (shimbun-header-id-internal header))

(defun shimbun-header-set-id (header id &optional asis)
  (shimbun-header-set-id-internal header
				  (if asis
				      id
				    (shimbun-header-normalize id t))))

(defun shimbun-header-references (header)
  (shimbun-header-references-internal header))

(defun shimbun-header-set-references (header references &optional asis)
  (shimbun-header-set-references-internal
   header
   (if asis
       references
     (shimbun-header-normalize references t))))

(defun shimbun-header-chars (header)
  (shimbun-header-chars-internal header))

(defun shimbun-header-set-chars (header chars)
  (shimbun-header-set-chars-internal header chars))

(defun shimbun-header-lines (header)
  (shimbun-header-lines-internal header))

(defun shimbun-header-set-lines (header lines)
  (shimbun-header-set-lines-internal header lines))

(defun shimbun-header-xref (header)
  (shimbun-header-xref-internal header))

(defun shimbun-header-set-xref (header xref)
  (shimbun-header-set-xref-internal header xref))

(defun shimbun-header-extra (header)
  (shimbun-header-extra-internal header))

(defun shimbun-header-set-extra (header extra)
  (shimbun-header-set-extra-internal header extra))

(defun shimbun-create-header (&optional number subject from date id
					references chars lines xref
					extra asis)
  "Return a new header for a shimbun article.
Because `shimbun-create-header' normalizes arguments with
`shimbun-header-normalize' before creating new header object,
following operations are unnecessary:

  * MIME-encoding of subjects and from addresses.
  * Removal of HTML tags.
  * Decode of HTML entities.
  * Replacement of space characteres, such as tab, newline, and
    linefeed.

If optional 11th argument ASIS is non-nil, normalization of header
values is suppressed."
  (let ((new (luna-make-entity 'shimbun-header :number number)))
    (shimbun-header-set-subject new subject asis)
    (shimbun-header-set-from new from asis)
    (shimbun-header-set-date new date asis)
    (shimbun-header-set-id new id asis)
    (shimbun-header-set-references new references asis)
    (shimbun-header-set-chars new chars)
    (shimbun-header-set-lines new lines)
    (shimbun-header-set-xref new xref)
    (shimbun-header-set-extra new extra)
    new))

(defun shimbun-make-header (&optional number subject from date id
				      references chars lines xref
				      extra)
  "Return a new header for a shimbun article.
This function is obsolete.  You should use `shimbun-create-header'
instead of this function."
  (shimbun-create-header number
			 (and subject (eword-decode-string subject))
			 (and from (eword-decode-string from))
			 date id references chars lines xref extra t))

;; Functions for the internal use.
(defun shimbun-article-base-url (shimbun header)
  "Return URL which points the original page specified by HEADER for SHIMBUN."
  (let ((xref (shimbun-header-xref header)))
    (if (and xref (eq (aref xref 0) ?/))
	(concat (shimbun-url-internal shimbun) xref)
      xref)))

(luna-define-generic shimbun-article-url (shimbun header)
  "Return URL which points the printable page specified by HEADER for SHIMBUN.")
(luna-define-method shimbun-article-url ((shimbun shimbun) header)
  (shimbun-article-base-url shimbun header))

(defcustom shimbun-encapsulate-images t
  "*If non-nil, inline images will be encapsulated in the articles.
Generated article have a multipart/related content-type."
  :group 'shimbun
  :type 'boolean)

;;; Base class for shimbun message entities:
(eval-and-compile
  (luna-define-class shimbun-entity () (type cid data))
  (luna-define-internal-accessors 'shimbun-entity))

(luna-define-generic shimbun-entity-type (entity) "Return TYPE of ENTITY.")
(luna-define-method shimbun-entity-type ((entity shimbun-entity))
  (shimbun-entity-type-internal entity))

(luna-define-generic shimbun-entity-cid (entity) "Return CID of ENTITY.")
(luna-define-method shimbun-entity-cid ((entity shimbun-entity))
  (shimbun-entity-cid-internal entity))

(defun shimbun-entity-set-cid (entity cid)
  (shimbun-entity-set-cid-internal entity cid))
(defsetf shimbun-entity-cid shimbun-entity-set-cid)

(luna-define-generic shimbun-entity-insert (entity)
  "Insert ENTITY as a MIME part.")
(luna-define-method shimbun-entity-insert ((entity shimbun-entity))
  (insert "Content-Type: " (shimbun-entity-type entity) "\n")
  (when (shimbun-entity-cid entity)
    (insert "Content-ID: <" (shimbun-entity-cid entity) ">\n"))
  (insert "\n"))

;;; Class for multipart entities:
(eval-and-compile
  (luna-define-class shimbun-multipart-entity (shimbun-entity) (boundary))
  (luna-define-internal-accessors 'shimbun-multipart-entity))

(defvar shimbun-multipart-entity-counter 0)

(luna-define-method initialize-instance :before ((entity
						  shimbun-multipart-entity)
						 &rest init-args)
  (shimbun-multipart-entity-set-boundary-internal
   entity
   (apply 'format "===shimbun_%d_%d_%d_%d==="
	  (incf shimbun-multipart-entity-counter)
	  (current-time))))

(defun shimbun-make-multipart-entity (&optional type cid)
  (luna-make-entity 'shimbun-multipart-entity :type type :cid cid))

(luna-define-method shimbun-entity-type ((entity shimbun-multipart-entity))
  (concat
   (or (shimbun-entity-type-internal entity)
       (shimbun-entity-set-type-internal
	entity
	(catch 'type
	  (dolist (child (shimbun-entity-data-internal entity))
	    (unless (string-match "\\`text/" (shimbun-entity-type child))
	      (throw 'type "multipart/related")))
	  "multipart/mixed")))
   "; boundary=\"" (shimbun-multipart-entity-boundary-internal entity) "\""
   (when (string= "multipart/related" (shimbun-entity-type-internal entity))
     (catch 'start
       (dolist (child (shimbun-entity-data-internal entity))
	 (when (string-match "\\`\\(?:text/\\|multipart/mixed\\)"
			     (shimbun-entity-type child))
	   (throw 'start
		  (concat "; type=\""
			  (shimbun-entity-type-internal child)
			  "\"; start=\"<"
			  (shimbun-entity-cid child)
			  ">\""))))))))

(luna-define-method shimbun-entity-insert :after ((entity
						   shimbun-multipart-entity))
  (let ((boundary (shimbun-multipart-entity-boundary-internal entity)))
    (dolist (child (shimbun-entity-data-internal entity))
      (insert "--" boundary "\n")
      (shimbun-entity-insert child)
      (insert (if (bolp)
		  "\n"
		"\n\n")))
    (insert "--" boundary "--\n")))

(defun shimbun-entity-add-child (entity &rest children)
  (shimbun-entity-set-data-internal entity
				    (nconc
				     (shimbun-entity-data-internal entity)
				     children)))

;;; Class of text entities:
(eval-and-compile
  (luna-define-class shimbun-text-entity (shimbun-entity) (charset))
  (luna-define-internal-accessors 'shimbun-text-entity))

(defun shimbun-make-text-entity (type data &optional cid)
  (luna-make-entity 'shimbun-text-entity :type type :data data :cid cid))

(luna-define-generic shimbun-text-entity-charset (entity &optional begin end)
  "Return MIME charset of ENTITY.")
(luna-define-method shimbun-text-entity-charset ((entity shimbun-text-entity)
						 &optional begin end)
  (let (cur tmp)
    (unless (and begin end)
      (setq cur (current-buffer))
      (set-buffer (generate-new-buffer " *temp*"))
      (insert (shimbun-entity-data-internal entity))
      (setq begin (point-min)
	    end (point-max)
	    tmp (current-buffer)))
    (prog1
	;; Prefer meta charset.
	(or (let ((charset (progn
			     (goto-char begin)
			     (and (re-search-forward "\
<meta\\s-+http-equiv=[\"']?content-type[\"']?\\s-+content=[\"']\
text/\\sw+\\(?:\;\\s-*charset=\\(.+\\)\\)?[\"'][^>]*>" end t)
				  (match-string 1)))))
	      (when (and charset
			 (w3m-find-coding-system (intern (downcase charset))))
		(shimbun-text-entity-set-charset-internal entity
							  (upcase charset))))
	    (shimbun-text-entity-charset-internal entity)
	    (shimbun-text-entity-set-charset-internal
	     entity (upcase (symbol-name
			     (detect-mime-charset-region begin end)))))
      (when cur
	(set-buffer cur)
	(kill-buffer tmp)))))

(luna-define-method shimbun-entity-type ((entity shimbun-text-entity))
  (concat (shimbun-entity-type-internal entity)
	  "; charset=" (shimbun-text-entity-charset entity)))

(luna-define-method shimbun-entity-insert :around ((entity
						    shimbun-text-entity))
  (save-restriction
    (narrow-to-region (point) (point))
    (insert (shimbun-entity-data-internal entity))
    (encode-coding-region (point-min) (point-max)
			  (mime-charset-to-coding-system
			   (shimbun-text-entity-charset entity
							(point-min)
							(point-max))))
    (goto-char (point-min))
    (luna-call-next-method)
    (goto-char (point-max))))

;;; Class for image entities:
(eval-and-compile
  (luna-define-class shimbun-image-entity (shimbun-entity) (disposition))
  (luna-define-internal-accessors 'shimbun-image-entity))

(luna-define-method initialize-instance :before ((entity shimbun-image-entity)
						 &rest init-args)
  (shimbun-image-entity-set-disposition-internal entity "inline"))

(defun shimbun-make-image-entity (type data &optional cid)
  (luna-make-entity 'shimbun-image-entity :type type :data data :cid cid))

(luna-define-method shimbun-entity-insert :around ((entity
						    shimbun-image-entity))
  (insert "Content-Transfer-Encoding: base64\n"
	  "Content-Disposition: "
	  (shimbun-image-entity-disposition-internal entity) "\n")
  (luna-call-next-method)
  (insert (base64-encode-string (shimbun-entity-data-internal entity))))

(defun shimbun-mime-replace-image-tags (shimbun base-cid
						&optional base-url images)
  "Replace all IMG tags with references to inlined image parts.
This function takes a BASE-CID as a base string for CIDs of inlined
image parts, and returns an alist of URLs and image entities."
  (goto-char (point-min))
  (let ((case-fold-search t)
	start end url img type)
    (while (and (re-search-forward "\\(<[\t\n\f\r ]*img\\)[\t\n\f\r ]" nil t)
		(progn
		  (setq start (match-end 1))
		  (search-forward ">" nil 'move))
		(progn
		  (setq end (match-beginning 0))
		  (goto-char start)
		  (re-search-forward
		   (eval-when-compile
		     (let ((spc "\t\n\f\r "))
		       (concat "[" spc "]+"
			       ;; 1. replaceable part
			       "\\(src[" spc "]*=[" spc "]*"
			       "\\(?:\""
			       ;; 2. url quoted with \"
			       "\\([^\"]+\\)"
			       "\"\\|'"
			       ;; 3. url quoted with '
			       "\\([^']+\\)"
			       "'\\|"
			       ;; 4. url unquoted
			       "\\([^" spc "\"']+\\)"
			       "\\)\\)")))
		   end t)))
      (setq start (match-beginning 1)
	    end (match-end 1)
	    url (or (match-string 2) (match-string 3) (match-string 4)))
      (setq url (shimbun-expand-url
		 ;; See the FIXME comment within the function definition
		 ;; of `w3m-decode-anchor-string' in w3m.el.
		 (shimbun-decode-anchor-string
		  (if (string-match "[\t\n\f\r ]+\\'" url)
		      (substring url 0 (match-beginning 0))
		    url))
		 base-url))
      (unless (or (setq img (assoc url images))
		  (and w3m-ignored-image-url-regexp
		       (string-match w3m-ignored-image-url-regexp url)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (setq case-fold-search nil
		type (shimbun-fetch-url shimbun url nil t base-url))
	  (goto-char (point-min))
	  (when (or (and (looking-at
			  "\\(GIF8\\)\\|\\(\377\330\\)\\|\211PNG\r\n")
			 (setq type (concat "image/"
					    (cond ((match-beginning 1) "gif")
						  ((match-beginning 2) "jpeg")
						  (t "png")))))
		    (and type
			 (string-match "\\`image/" type)))
	    (push (setq img (cons url
				  (shimbun-make-image-entity
				   type
				   (buffer-string)
				   (format "shimbun.inline.%d.%s"
					   (length images) base-cid))))
		  images))))
      (when img
	;; Only when an image is successfully retrieved, its
	;; source URI should be rewritten.
	(goto-char start)
	(delete-region start end)
	(insert "src=\"cid:" (shimbun-entity-cid (cdr img)) "\""))))
  images)

(defun shimbun-make-mime-article (shimbun header &optional base-url)
  "Make a MIME article according to SHIMBUN and HEADER.
If article have inline images, generated article have a multipart/related
content-type if `shimbun-encapsulate-images' is non-nil."
  (let ((base-cid (shimbun-header-id header)) images)
    (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
      (setq base-cid (match-string 1 base-cid)))
    (when shimbun-encapsulate-images
      (setq images
	    (shimbun-mime-replace-image-tags shimbun
					     base-cid
					     (or base-url
						 (shimbun-article-url
						  shimbun header)))))
    (let ((body (shimbun-make-text-entity "text/html" (buffer-string))))
      (erase-buffer)
      (when images
	(let ((new (shimbun-make-multipart-entity)))
	  (setf (shimbun-entity-cid body) (concat "shimbun.0." base-cid))
	  (shimbun-entity-add-child new body)
	  (apply 'shimbun-entity-add-child new (mapcar 'cdr (nreverse images)))
	  (setq body new)))
      (shimbun-header-insert shimbun header)
      (insert "MIME-Version: 1.0\n")
      (shimbun-entity-insert body))))

(defcustom shimbun-x-face-database-function
  (if (boundp 'shimbun-use-bbdb-for-x-face)
      (cdr (assq (symbol-value 'shimbun-use-bbdb-for-x-face)
		 '((t . shimbun-bbdb-get-x-face)
		   (never . never)))))
  "*Function to get faces from a favorite database.
When its initial value is nil and BBDB or LSDB is loaded, it will be
set to an appropriate default value.  You can set this to `never' if
you want to use no database."
  :group 'shimbun
  :type '(radio
	  (const :tag "Default" nil)
	  (const :tag "Use no database" never)
	  (const :tag "Use BBDB" shimbun-bbdb-get-x-face)
	  (const :tag "Use LSDB" shimbun-lsdb-get-x-face)
	  (function :format "User defined function: %v\n" :size 0)))

(defun shimbun-header-insert (shimbun header)
  (let ((from (shimbun-header-from header))
	(refs (shimbun-header-references header))
	(reply-to (shimbun-reply-to shimbun))
	x-face)
    (insert
     (with-temp-buffer
       ;; Make sure the temp buffer's multibyteness is true.  It is
       ;; needed to make `encode-mime-charset-string' (which is
       ;; employed by `eword-encode-string') encode non-ascii text.
       (static-unless (featurep 'xemacs)
	 (set-buffer-multibyte t))
       (insert "Subject: " (or (eword-encode-string
				(shimbun-header-subject header t))
			       "(none)")
	       "\nFrom: " (or (eword-encode-string
			       (shimbun-header-from header t))
			      "(nobody)")
	       "\nDate: " (or (shimbun-header-date header) "")
	       "\nMessage-ID: " (shimbun-header-id header) "\n")
       (when reply-to
	 (insert "Reply-To: " reply-to "\n"))
       (when (and refs
		  (string< "" refs))
	 (insert "References: " refs "\n"))
       (insert "Lines: " (number-to-string (or (shimbun-header-lines header)
					       0))
	       "\n"
	       "Xref: " (or (shimbun-article-base-url shimbun header) "") "\n")
       (unless shimbun-x-face-database-function
	 (when (and (fboundp 'bbdb-get-field)
		    (not (eq 'autoload
			     (car-safe (symbol-function 'bbdb-get-field))))
		    (boundp 'bbdb-file)
		    (stringp (symbol-value 'bbdb-file))
		    (file-exists-p (symbol-value 'bbdb-file)))
	   (setq shimbun-x-face-database-function 'shimbun-bbdb-get-x-face)))
       (unless shimbun-x-face-database-function
	 (when (and
		(fboundp 'lsdb-lookup-records)
		(not (eq 'autoload
			 (car-safe (symbol-function 'lsdb-lookup-records)))))
	   (setq shimbun-x-face-database-function 'shimbun-lsdb-get-x-face)))
       (when (setq x-face
		   (or (and from
			    (functionp shimbun-x-face-database-function)
			    (funcall shimbun-x-face-database-function from))
		       (shimbun-x-face shimbun)))
	 (insert x-face)
	 (unless (bolp)
	   (insert "\n")))
       (buffer-string)))))

(eval-when-compile
  ;; Attempt to pick up the inline function `bbdb-search-simple'.
  (condition-case nil
      (require 'bbdb)
    (error
     (autoload 'bbdb-search-simple "bbdb")
     (autoload 'bbdb-get-field "bbdb"))))

(defun shimbun-bbdb-get-x-face (person)
  "Search a face of a PERSON from BBDB.  When missing it, return nil."
  (let (x)
    (and (setq x (cadr (mail-extract-address-components person)))
	 (setq x (bbdb-search-simple nil x))
	 (setq x (bbdb-get-field x 'face))
	 (not (zerop (length x)))
	 (concat "X-Face: "
		 (mapconcat 'identity
			    (split-string x)
			    "\nX-Face: ")))))

(eval-when-compile
  (condition-case nil
      (require 'lsdb)
    (error
     (autoload 'lsdb-maybe-load-hash-tables "lsdb")
     (autoload 'lsdb-lookup-records "lsdb"))))

(defun shimbun-lsdb-get-x-face (person)
  "Return a face of a PERSON from LSDB.  When missing it, return nil."
  (lsdb-maybe-load-hash-tables)
  (let (x)
    (and (setq x (car (mail-extract-address-components person)))
	 (setq x (car (lsdb-lookup-records x)))
	 (setq x (cdr (assq 'x-face x)))
	 (not (zerop (length x)))
	 (mapconcat (lambda (x-face)
		      (concat "X-Face: "
			      (mapconcat 'identity
					 (split-string x-face)
					 "\n ")))
		    x
		    "\n"))))

;;; Implementation of Shimbun API.

(defconst shimbun-attributes
  '(url groups coding-system server-name from-address
	content-start content-end x-face-alist expiration-days
	prefer-text-plain text-content-start text-content-end
	japanese-hankaku url-coding-system retry-fetching))

(defun shimbun-open (server &optional mua)
  "Open a shimbun for SERVER.
Optional MUA is a `shimbun-mua' instance."
  (let ((load-path (append shimbun-server-additional-path load-path))
	rest subst shimbun)
    (require (intern (concat "sb-" server)))
    (setq shimbun
	  (apply
	   'luna-make-entity (intern (concat "shimbun-" server))
	   :mua mua :server server
	   (dolist (attr shimbun-attributes (nreverse rest))
	     (push (intern (format ":%s" attr)) rest)
	     (push (if (setq subst (assq attr
					 '((content-start . text-content-start)
					   (content-end . text-content-end)
					   (text-content-start . content-start)
					   (text-content-end . content-end))))
		       (or (symbol-value (intern-soft (format "shimbun-%s-%s"
							      server attr)))
			   (symbol-value (intern-soft (format "shimbun-%s-%s"
							      server
							      (cdr subst)))))
		     (symbol-value (intern-soft (format "shimbun-%s-%s"
							server attr))))
		   rest))))
    (when mua
      (shimbun-mua-set-shimbun-internal mua shimbun))
    shimbun))

(defun shimbun-server (shimbun)
  "Return the server name of SHIMBUN."
  (shimbun-server-internal shimbun))

(luna-define-generic shimbun-server-name (shimbun)
  "Return the server name of SHIMBUN in human-readable style.")

(luna-define-method shimbun-server-name ((shimbun shimbun))
  (or (shimbun-server-name-internal shimbun)
      (shimbun-server-internal shimbun)))

(luna-define-generic shimbun-groups (shimbun)
  "Return a list of groups which are available in the SHIMBUN.")

(luna-define-method shimbun-groups ((shimbun shimbun))
  (shimbun-groups-internal shimbun))

(luna-define-generic shimbun-group-p (shimbun group)
  "Return non-nil if group is available in the SHIMBUN.")

(luna-define-method shimbun-group-p ((shimbun shimbun) group)
  (member group (shimbun-groups shimbun)))

(defun shimbun-current-group (shimbun)
  "Return the current group of SHIMBUN."
  (shimbun-current-group-internal shimbun))

(luna-define-generic shimbun-current-group-name (shimbun)
  "Return the current group name of SHIMBUN in human-readable style.")

(luna-define-method shimbun-current-group-name ((shimbun shimbun))
  (shimbun-current-group-internal shimbun))

(defun shimbun-open-group (shimbun group)
  "Open a SHIMBUN GROUP."
  (if (shimbun-group-p shimbun group)
      (shimbun-set-current-group-internal shimbun group)
    (error "No such group %s" group)))

(defun shimbun-close-group (shimbun)
  "Close opened group of SHIMBUN."
  (when (shimbun-current-group-internal shimbun)
    (shimbun-set-current-group-internal shimbun nil)))

(luna-define-generic shimbun-japanese-hankaku (shimbun)
  "Say whether to convert Japanese zenkaku ASCII chars into hankaku.")

(luna-define-method shimbun-japanese-hankaku ((shimbun shimbun))
  (let ((hankaku (or (shimbun-japanese-hankaku-internal shimbun)
		     shimbun-japanese-hankaku)))
    (unless (eq hankaku 'never)
      hankaku)))

(luna-define-generic shimbun-headers (shimbun &optional range)
  "Return a SHIMBUN header list.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(defmacro shimbun-header-index-pages (range)
  "Return number of pages to retrieve according to RANGE.
Return nil if all pages should be retrieved."
  (if (consp range)
      `(let ((range ,range))
	 (if (eq 'last range) 1
	   (if (eq 'all range) nil
	     range)))
    `(if (eq 'last ,range) 1
       (if (eq 'all ,range) nil
	 ,range))))

(defvar shimbun-use-refresh t
  "Non-nil means honor the REFRESH attribute in META tags.
Bind it to nil per shimbun if the refresh brings unwanted page.")

;; FIXME: It seems better that `shimbun-fetch-url' provides the redirection
;; support, whereas it is currently done by `shimbun-headers-1' for headers
;; and `shimbun-article-1' for articles separately.  The reason doing so is
;; that `shimbun-article-1' needs to replace urls in XREFs with the redirected
;; ones.
(defun shimbun-headers-1 (shimbun url)
  "Run `shimbun-fetch-url' and refresh the contents if necessary."
  (let (;; The default url used when it is not specified for refresh.
	(w3m-current-url url)
	(w3m-use-refresh shimbun-use-refresh))
    (when (and (shimbun-fetch-url shimbun url 'reload)
	       (progn
		 (w3m-check-refresh-attribute)
		 (and w3m-current-refresh
		      (not (string-equal url (cdr w3m-current-refresh))))))
      (erase-buffer)
      (shimbun-headers-1 shimbun (cdr w3m-current-refresh)))))

(luna-define-method shimbun-headers ((shimbun shimbun) &optional range)
  (shimbun-message shimbun (concat shimbun-checking-new-news-format "..."))
  (prog1
      (with-temp-buffer
	(let ((w3m-verbose (if shimbun-verbose nil w3m-verbose))
	      headers)
	  (shimbun-headers-1 shimbun (shimbun-index-url shimbun))
	  (setq headers (shimbun-get-headers shimbun range))
	  (if (memq (shimbun-japanese-hankaku shimbun) '(body nil))
	      headers
	    (dolist (header headers headers)
	      (erase-buffer)
	      (insert (shimbun-header-subject-internal header))
	      (shimbun-japanese-hankaku-buffer)
	      (shimbun-header-set-subject-internal header (buffer-string))))))
    (shimbun-message shimbun (concat shimbun-checking-new-news-format
				     "...done"))))

(luna-define-generic shimbun-reply-to (shimbun)
  "Return a reply-to field body for SHIMBUN.")

(luna-define-method shimbun-reply-to ((shimbun shimbun))
  nil)

(luna-define-generic shimbun-x-face (shimbun)
  "Return a X-Face field string for SHIMBUN.")

(luna-define-method shimbun-x-face ((shimbun shimbun))
  (shimbun-set-x-face-internal
   shimbun
   (let ((group (shimbun-current-group-internal shimbun))
	 (alist (shimbun-x-face-alist-internal shimbun)))
     (or (cdr (assoc group alist))
	 (catch 'face
	   (dolist (elem alist)
	     (when (and (string-match "[]$*+\\^[]" (car elem))
			(string-match (car elem) group))
	       (throw 'face (cdr elem)))))
	 (cdr (assoc "default" alist))
	 shimbun-x-face))))

(defun shimbun-search-id (shimbun id)
  "Return non-nil when MUA found a message structure which corresponds to ID."
  (when (shimbun-mua-internal shimbun)
    (shimbun-mua-search-id (shimbun-mua-internal shimbun) id)))

(defun shimbun-article-expiration-days (shimbun)
  "Return an expiration day number of SHIMBUN.
Return nil when articles are not expired."
  (shimbun-expiration-days-internal shimbun))

(defun shimbun-content-start (shimbun)
  "Return the `content-start' value according to SHIMBUN."
  (if (shimbun-prefer-text-plain-internal shimbun)
      (or (shimbun-text-content-start-internal shimbun)
	  (shimbun-content-start-internal shimbun))
    (or (shimbun-content-start-internal shimbun)
	(shimbun-text-content-start-internal shimbun))))

(defun shimbun-content-end (shimbun)
  "Return the `content-end' value according to SHIMBUN."
  (if (shimbun-prefer-text-plain-internal shimbun)
      (or (shimbun-text-content-end-internal shimbun)
	  (shimbun-content-end-internal shimbun))
    (or (shimbun-content-end-internal shimbun)
	(shimbun-text-content-end-internal shimbun))))

(luna-define-generic shimbun-from-address (shimbun)
  "Make a From address like \"SERVER (GROUP) <ADDRESS>\".")

(luna-define-method shimbun-from-address ((shimbun shimbun))
  (let ((addr (or (shimbun-from-address-internal shimbun)
		  (shimbun-reply-to shimbun))))
    (if addr
	(format "%s (%s) <%s>"
		(shimbun-server-name shimbun)
		(shimbun-current-group-name shimbun)
		addr)
      (format "%s (%s)"
	      (shimbun-server-name shimbun)
	      (shimbun-current-group-name shimbun)))))

(luna-define-generic shimbun-article (shimbun header &optional outbuf)
  "Retrieve a SHIMBUN article which corresponds to HEADER to the OUTBUF.
HEADER is a shimbun-header which is obtained by `shimbun-headers'.
If OUTBUF is not specified, article is retrieved to the current buffer.")

;; See also the FIXME comment around `shimbun-headers-1'.
(defun shimbun-article-1 (shimbun header)
  "Run `shimbun-fetch-url' and refresh the contents if necessary."
  (let* ((url (shimbun-article-url shimbun header))
	 ;; The default url used when it is not specified for refresh.
	 (w3m-current-url url)
	 (w3m-use-refresh shimbun-use-refresh)
	 real)
    (when (shimbun-fetch-url shimbun url nil nil
			     (shimbun-article-base-url shimbun header))
      (w3m-check-refresh-attribute)
      (if (and w3m-current-refresh
	       ;; The page may specify to refresh itself.
	       (not (string-equal url (cdr w3m-current-refresh))))
	  (progn
	    (shimbun-header-set-xref header (cdr w3m-current-refresh))
	    (erase-buffer)
	    (shimbun-article-1 shimbun header))
	(unless (string-equal url (setq real (w3m-real-url url)))
	  (shimbun-header-set-xref header real))))))

(luna-define-method shimbun-article ((shimbun shimbun) header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (shimbun-article-1 shimbun header)
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(luna-define-generic shimbun-make-contents (shimbun header)
  "Return a content string of SHIMBUN article using current buffer content.
HEADER is a header structure obtained via `shimbun-headers'.")

(defun shimbun-insert-footer (shimbun header &optional html &rest args)
  "Insert the footer and ARGS."
  (goto-char (point-min))
  (when (re-search-forward
	 "[\t\n ]*\\(?:<\\(?:![^>]+\\|br\\|/?div\\|/?p\\)>[\t\n ]*\\)*\\'"
	 nil 'move)
    (delete-region (match-beginning 0) (point-max)))
  (apply 'insert "\n" (shimbun-footer shimbun header html) args))

(defun shimbun-current-base-url ()
  "Process BASE tag in the current buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (re-search-forward "</head\\(?:[ \t\r\f\n][^>]*\\)?>" nil t)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(when (re-search-forward "<base[ \t\r\f\n]+" nil t)
	  (w3m-parse-attributes (href)
	    (when (< 0 (length href))
	      href)))))))

(defun shimbun-make-html-contents (shimbun header)
  (let ((base-url (or (shimbun-current-base-url)
		      (shimbun-article-url shimbun header))))
    (when (shimbun-clear-contents shimbun header)
      (goto-char (point-min))
      (insert "<html>\n<head>\n<base href=\""
	      base-url
	      "\">\n</head>\n<body>\n")
      (shimbun-insert-footer shimbun header t "</body>\n</html>\n"))
    (shimbun-make-mime-article shimbun header base-url)
    (buffer-string)))

(eval-and-compile
  (autoload 'shimbun-make-text-contents "sb-text"))

(luna-define-method shimbun-make-contents ((shimbun shimbun) header)
  (if (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-make-text-contents shimbun header)
    (shimbun-make-html-contents shimbun header)))

(luna-define-generic shimbun-clear-contents (shimbun header)
  "Clear a content in this current buffer for an article of SHIMBUN.
Return nil, unless a content is cleared successfully.")

(luna-define-method shimbun-clear-contents ((shimbun shimbun) header)
  (let ((start (shimbun-content-start shimbun))
	(end (shimbun-content-end shimbun))
	(case-fold-search t))
    (goto-char (point-min))
    (when (and (stringp start)
	       (re-search-forward start nil t)
	       (progn
		 (setq start (point))
		 (stringp end))
	       (re-search-forward end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      (unless (memq (shimbun-japanese-hankaku shimbun) '(header subject nil))
	(shimbun-japanese-hankaku-buffer t)
	(goto-char (point-min)))
      t)))

(luna-define-generic shimbun-footer (shimbun header &optional html)
  "Make a footer string for SHIMBUN and HEADER.")

(luna-define-method shimbun-footer ((shimbun shimbun) header &optional html)
  "Return a null string for servers that have no footer."
  "")

(luna-define-generic shimbun-index-url (shimbun)
  "Return a index URL of SHIMBUN.")

(luna-define-method shimbun-index-url ((shimbun shimbun))
  (shimbun-url-internal shimbun))

(luna-define-generic shimbun-get-headers (shimbun &optional range)
  "Return a shimbun header list of SHIMBUN.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(luna-define-generic shimbun-close (shimbun)
  "Close a SHIMBUN.")

(luna-define-method shimbun-close ((shimbun shimbun))
  (shimbun-close-group shimbun))

;;; Virtual class for Newspapers:
(luna-define-class shimbun-newspaper () ())
(luna-define-method shimbun-footer ((shimbun shimbun-newspaper) header
				    &optional html)
  (if html
      (concat "<div align=\"left\">\n--&nbsp;<br>\n<i>"
	      (shimbun-server-name shimbun)
	      " holds the copyright of this article.<br>\n"
	      "The original article is <a href=\""
	      (shimbun-article-base-url shimbun header) "\">"
	      "here</a>.\n</i></div>\n")
    (concat "-- \n"
	    (shimbun-server-name shimbun)
	    " holds the the copyright of this article.\n"
	    "The original article is in:\n"
	    (shimbun-article-base-url shimbun header)
	    "\n")))

;;; Virtual class for Japanese Newspapers:
(luna-define-class shimbun-japanese-newspaper () ())
(luna-define-method shimbun-footer ((shimbun shimbun-japanese-newspaper) header
				    &optional html)
  (if html
      (concat "<div align=\"left\">\n--&nbsp;<br>\nこの記事の著作権は、"
	      (shimbun-server-name shimbun)
	      "社に帰属します。<br>\n原物は <a href=\""
	      (shimbun-article-base-url shimbun header) "\">"
	      (shimbun-article-base-url shimbun header)
	      "</a> で公開されています。\n</div>\n")
    (concat "-- \nこの記事の著作権は、"
	    (shimbun-server-name shimbun)
	    "社に帰属します。\n原物は "
	    (shimbun-article-base-url shimbun header)
	    " で公開されています。\n")))

;;; Misc Functions
(defun shimbun-header-insert-and-buffer-string (shimbun header
							&optional charset html)
  "Insert headers and footer in the current buffer and return the contents.
SHIMBUN and HEADER specify what headers and footer are.  CHARSET, which
defaults to the one that the `detect-mime-charset-region' function
determines, specifies the content charset and is used to encode
the return value of this function.  A non-nil value of HTML specifies
that the content type is text/html, otherwise text/plain."
  (unless charset
    (setq charset (upcase (symbol-name (detect-mime-charset-region
					(point-min) (point-max))))))
  (goto-char (point-min))
  (shimbun-header-insert shimbun header)
  (insert "Content-Type: text/" (if html "html" "plain") "; charset=" charset
	  "\nMIME-Version: 1.0\n\n")
  (if html
      (progn
	(insert "<html>\n<head>\n<base href=\""
		(shimbun-article-url shimbun header)
		"\">\n</head>\n<body>\n")
	(shimbun-insert-footer shimbun header t "</body>\n</html>\n"))
    (shimbun-insert-footer shimbun header))
  (encode-mime-charset-string (buffer-string) charset))

(defun shimbun-mime-encode-string (string)
  (condition-case nil
      (save-match-data
	(with-temp-buffer
	  ;; Make sure the temp buffer's multibyteness is true.  It is
	  ;; needed to make `encode-mime-charset-string' (which is
	  ;; employed by `eword-encode-string') encode non-ascii text.
	  (static-unless (featurep 'xemacs)
	    (set-buffer-multibyte t))
	  (mapconcat
	   #'identity
	   (split-string (or (eword-encode-string
			      (shimbun-decode-entities-string string)) ""))
	   " ")))
    (error string)))

(defun shimbun-make-date-string (year month day &optional time timezone)
  "Make a date string which will be used as shimbun headers.

YEAR is a 4-digit number, and MONTH and DAY are also numbers.  TIME is
a string in the \"HH:MM\" form, where HH is a number of hours and MM is
a number of minutes.  It defaults to \"00:00\".

TIMEZONE defaults to \"+0900\" by the historical reason.  You should
specify this if a time is represented based on other than the \"+0900\"
zone."
  (setq year (cond ((< year 69)
		    (+ year 2000))
		   ((< year 100)
		    (+ year 1900))
		   ((< year 1000)	; possible 3-digit years.
		    (+ year 1900))	; why isn't it 1000?
		   (t year)))
  (let ((cts (current-time-string (encode-time 0 0 0 day month year))))
    (format "%s, %02d %s %04d %s %s"
	    (substring cts 0 3)
	    day
	    (substring cts 4 7)
	    year
	    (or time "00:00")
	    (or timezone "+0900"))))

(autoload 'timezone-fix-time "timezone")

(defun shimbun-time-parse-string (string)
  "Parse the time-string STRING into the Emacs style (HIGH LOW) time."
  (let ((x (nreverse (append (timezone-fix-time string nil nil) nil))))
    (apply 'encode-time (nconc (cdr x) (list (car x))))))

(defun shimbun-decode-time (&optional specified-time specified-zone)
  "Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE).
This function behaves as well as `decode-time' if the optional 2nd arg
SPECIFIED-ZONE is nil.  If it is an integer indicating the number of
seconds ahead of UTC (east of Greenwich), the return value expresses
the local time of the zone that the value indicates.  For instance,
the following form returns the present time of Japan, wherever you are.

\(shimbun-decode-time nil 32400)"
  (if specified-zone
      (let* ((tz (- (car (current-time-zone)) specified-zone))
	     (ct (or specified-time (current-time)))
	     (ms (car ct))
	     (ls (- (cadr ct) tz)))
	(cond ((< ls 0)
	       (setq ls (+ ls 65536)
		     ms (1- ms)))
	      ((>= ls 65536)
	       (setq ls (- ls 65536)
		     ms (1+ ms))))
	(nconc (nbutlast (decode-time (list ms ls))) (list specified-zone)))
    (decode-time specified-time)))

(defun shimbun-sort-headers (headers)
  "Return a list of sorted HEADERS by date in increasing order."
  (sort headers
	(lambda (a b)
	  (setq a (shimbun-time-parse-string (shimbun-header-date a))
		b (shimbun-time-parse-string (shimbun-header-date b)))
	  (or (< (car a) (car b))
	      (and (= (car a) (car b))
		   (< (cadr a) (cadr b)))))))

(defun shimbun-remove-tags (begin-tag &optional end-tag)
  "Remove all occurrences of regions surrounded by BEGIN-TAG and END-TAG."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (if end-tag
	(let (pos)
	  (while (and (re-search-forward begin-tag nil t)
		      (setq pos (match-beginning 0))
		      (re-search-forward end-tag nil t))
	    (delete-region pos (point))))
      (while (re-search-forward begin-tag nil t)
	(delete-region (match-beginning 0) (match-end 0))))))

(defun shimbun-end-of-tag (&optional tag include-whitespace)
  "Move point to the end of tag.  Inner nested tags are skipped.
If TAG, which is a name of the tag, is given, this function moves point
from the open-tag <TAG ...> (point should exist in front of or within
it initially) to the end-point of the closing-tag </TAG>.  For example,
in the following two situations, point moves from the leftmost tag to
the end-point of the rightmost tag:

<TAG ...>...<TAG ...>...<TAG ...>...</TAG>...</TAG>...</TAG>
<TAG ...>...<TAG ...>...</TAG>...<TAG ...>...</TAG>...</TAG>

If TAG is omitted or nil, this function moves point to the end-point of
the tag in which point exists.  In this case, point should initially
exist within the start position of the tag and the next tag as follows:

<!-- foo <bar ...<baz ...>...> -->
 ^^^^^^^^
If INCLUDE-WHITESPACE is non-nil, include leading and trailing
whitespace.  Return the end-point and set the match-data #0, #1, #2,
and #3 as follows (\"___\" shows whitespace):

The case where TAG is spefified:
___<TAG ...>___...___</TAG>___
   0        1  2  2  1     0     INCLUDE-WHITESPACE=nil
0  1        2  3  3  2     1  0  INCLUDE-WHITESPACE=non-nil

The case where TAG is nil:
___<TAG ...>___
   0        0     INCLUDE-WHITESPACE=nil
0  1        1  0  INCLUDE-WHITESPACE=non-nil"
  (let ((init (point))
	(num 1)
	(md (match-data))
	(case-fold-search t)
	regexp st1 st2 st3 nd1 nd2 nd3 nd0 st0)
    (condition-case nil
	(progn
	  (if tag
	      (progn
		(setq tag (regexp-quote tag))
		(if (looking-at (concat "\
\[\t\n\r ]*\\(<[\t\n\r ]*" tag "\\(?:[\t\n\r ]*\\|[\t\n\r ]+[^>]+\\)>\\)\
\[\t\n\r ]*"))
		    (setq st1 (nth 2 (match-data)) ;; (match-beginning 1)
			  st2 (nth 3 (match-data)) ;; (match-end 1)
			  st3 (nth 1 (match-data))) ;; (match-end 0)
		  (search-backward "<")
		  (if (looking-at (concat "\
\\(<[\t\n\r ]*" tag "\\(?:[\t\n\r ]*\\|[\t\n\r ]+[^>]+\\)>\\)[\t\n\r ]*"))
		      (setq st1 (car (match-data)) ;; (match-beginning 0)
			    st2 (nth 3 (match-data)) ;; (match-end 1))
			    st3 (nth 1 (match-data))) ;; (match-end 0)
		    (error "")))
		(goto-char (1+ st1))
		(setq regexp (concat "\
\[\t\n\r ]*\\(<\\(/\\)?" tag "\\(?:[\t\n\r ]*\\|[\t\n\r ]+[^>]+\\)>\\)"))
		(while (and (> num 0)
			    (re-search-forward regexp))
		  (setq num (if (match-beginning 2)
				(1- num)
			      (1+ num))))
		(setq nd1 (nth 3 (match-data)) ;; (match-end 1)
		      nd2 (nth 2 (match-data)) ;; (match-beginning 1)
		      nd3 (car (match-data)))) ;; (match-beginning 0)
	    (search-backward "<")
	    (setq st1 (car (match-data))) ;; (match-beginning 0)
	    (goto-char init)
	    (while (and (> num 0)
			(re-search-forward "\\(>\\)\\|<"))
	      (setq num (if (match-beginning 1)
			    (1- num)
			  (1+ num))))
	    (setq nd1 (nth 3 (match-data)))) ;; (match-end 1)
	  (if include-whitespace
	      (progn
		(skip-chars-forward "\t\n\r ")
		(setq nd0 (point-marker))
		(goto-char st1)
		(skip-chars-backward "\t\n\r ")
		(setq st0 (point-marker))
		(goto-char nd0)
		(set-match-data (if tag
				    (list st0 nd0 st1 nd1 st2 nd2 st3 nd3)
				  (list st0 nd0 st1 nd1))))
	    (set-match-data (if tag
				(list st1 nd1 st2 nd2 st3 nd3)
			      (list st1 nd1))))
	  (point))
      (error
       (set-match-data md)
       (goto-char init)
       nil))))

(defun shimbun-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<!--" nil t)
      (delete-region (match-beginning 0)
		     (or (search-forward "-->" nil t)
			 (point-max))))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t t))))

(defun shimbun-strip-cr ()
  "Strip ^M from the end of all lines."
  (goto-char (point-max))
  (while (search-backward "\r\n" nil t)
    (delete-char 1)))

(if (fboundp 'subst-char-in-string)
    (defalias 'shimbun-subst-char-in-string 'subst-char-in-string)
  (defun shimbun-subst-char-in-string (fromchar tochar string
						&optional inplace)
    "Replace characters in STRING from FROMCHAR to TOCHAR.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((string (if inplace string (copy-sequence string)))
	  (len (length string))
	  (idx 0))
      ;; Replace all occurrences of FROMCHAR with TOCHAR.
      (while (< idx len)
	(when (= (aref string idx) fromchar)
	  (aset string idx tochar))
	(setq idx (1+ idx)))
      string)))

(defun shimbun-message (shimbun fmt &rest args)
  "Function equivalent to `message' enabling to handle special formats.
SHIMBUN is a shimbun entity object.  FMT and ARGS are the same as the
arguments of `message'.  This function allows the following special
format specifiers:

#g means print a group name.
#s means print a server name.
#S means print a human-readable server name.

Use ## to put a single # into the output.  If `shimbun-verbose' is nil,
it will run silently.  The `shimbun-message-enable-logging' variable
controls whether this function should preserve a message in the
*Messages* buffer."
  (let (specifier)
    (with-temp-buffer
      (static-unless (featurep 'xemacs)
	(set-buffer-multibyte t))
      (insert fmt)
      (goto-char (point-min))
      (while (search-forward "#" nil t)
	(setq specifier (char-after))
	(delete-region (1- (point)) (1+ (point)))
	(cond ((eq specifier ?#)
	       (insert "#"))
	      ((eq specifier ?g)
	       (insert (shimbun-current-group-internal shimbun)))
	      ((eq specifier ?s)
	       (insert (shimbun-server-internal shimbun)))
	      ((eq specifier ?S)
	       (insert (or (shimbun-server-name-internal shimbun)
			   (shimbun-server-internal shimbun))))))
      (setq fmt (buffer-string))))
  (if shimbun-verbose
      (static-if (featurep 'xemacs)
	  (let ((string (apply 'format fmt args)))
	    (if shimbun-message-enable-logging
		(display-message 'message string)
	      (display-message 'no-log string))
	    string)
	(if shimbun-message-enable-logging
	    (apply 'message fmt args)
	  (let (message-log-max)
	    (apply 'message fmt args))))
    (apply 'format fmt args)))

(defun shimbun-break-long-japanese-lines (&optional shimbun)
  "Break long Japanese lines in an article.
Article should be charset decoded html data.  If SHIMBUN is given,
this function will narrow the buffer to just an article using the
shimbun class variables `content-start' and `content-end'.  Otherwise,
it considers the buffer has already been narrowed to an article."
  (save-restriction
    (when shimbun
      (goto-char (point-min))
      (let ((case-fold-search t)
	    start)
	(when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		   (setq start (point))
		   (re-search-forward (shimbun-content-end shimbun) nil t))
	  (narrow-to-region start (match-beginning 0)))))
    (goto-char (point-min))
    (while (re-search-forward
	    "<p[\t\n ]+[^>]*>\\|</p>\\|\\([、。）」]+\\)\\(\\cj\\)?"
	    nil t)
      (if (match-beginning 2)
	  (replace-match "\\1\n\\2")
	(unless (eolp)
	  (insert "\n")))))
  (goto-char (point-min)))

(defmacro shimbun-with-narrowed-article (shimbun &rest forms)
  "Narrow to the article in the buffer and evaluate FORMS."
  `(progn
     (shimbun-strip-cr)
     (goto-char (point-min))
     (let ((case-fold-search t)
	   start)
       (when (re-search-forward (shimbun-content-start ,shimbun) nil t)
	 (setq start (match-end 0))
	 (when (re-search-forward (shimbun-content-end ,shimbun) nil t)
	   (narrow-to-region start (match-beginning 0))
	   (goto-char (point-min))
	   ;; Remove trailing whitespace.
	   (while (re-search-forward "[\t ]+$" nil t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char (point-min))
	   ,@forms
	   (widen))
	 (goto-char (point-min))))))

(static-if (featurep 'xemacs)
    (defalias 'shimbun-char-category-list 'char-category-list)
  (defun shimbun-char-category-list (char)
    "Return a list of category mnemonics for CHAR."
    (append (category-set-mnemonics (char-category-set char)) nil)))

(eval-when-compile
  (defsubst shimbun-japanese-hankaku-region-1 (start end quote)
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (when quote
	(while (re-search-forward "＜\\(?:[ 　]\\|&nbsp;\\)?" nil t)
	  (replace-match "&lt;"))
	(goto-char start)
	(while (re-search-forward "\\(?:[ 　]\\|&nbsp;\\)?＞" nil t)
	  (replace-match "&gt;"))
	(goto-char start)
	(while (search-forward "＆" nil t)
	  (replace-match "&amp;"))
	(goto-char start))
      (while (re-search-forward "\
\\(?:[Ｆｆ][Ｉｉ][Ｌｌ][Ｅｅ]\\|[Ｆｆ][Ｔｔ][Ｐｐ]\
\\|[Ｈｈ][Ｔｔ][Ｔｔ][Ｐｐ][Ｓｓ]?\\|[Ｍｍ][Ａａ][Ｉｉ][Ｌｌ][Ｔｔ][Ｏｏ]\\)\
：\\cj+"
				nil t)
	(japanese-hankaku-region (match-beginning 0) (match-end 0) t))
      (goto-char start)
      (while (re-search-forward "\\([^0-9０-９]\\)：\\|：\\([^ 0-9　０-９]\\)"
				nil t)
	(if (match-beginning 1)
	    (replace-match "\\1:")
	  (replace-match ":\\2")
	  (backward-char 1))
	(unless (looking-at "&nbsp;")
	  (insert " ")))
      (goto-char start)
      (while (search-forward "；" nil t)
	(replace-match ";")
	(unless (looking-at "[ 　]\\|&nbsp;")
	  (insert " ")))
      (goto-char start)
      ;; Ｚ＠Ｚ -> Ｚ@Ｚ
      ;; where Ｚ is a zenkaku alphanumeric, ＠ is a zenkaku symbol.
      (while (re-search-forward "\\cA[．´｀＾＿―‐／＼｜’＠]\\cA" nil t)
	(backward-char 2)
	(insert (prog1
		    (cdr (assq (char-after)
			       '((?． . ?.) (?´ . ?') (?｀ . ?`)
				 (?＾ . ?^) (?＿ . ?_) (?― . ?-)
				 (?‐ . ?-) (?／ . ?/) (?＼ . ?\\)
				 (?｜ . ?|) (?’ . ?') (?＠ . ?@))))
		  (delete-char 1))))
      (goto-char start)
      ;; Replace Chinese hyphen with "−".
      (condition-case nil
	  (let ((regexp (concat "[" (list (make-char 'chinese-gb2312 35 45)
					  (make-char 'chinese-big5-1 34 49))
				"]")))
	    (while (re-search-forward regexp nil t)
	      (replace-match "−")))
	(error))
      (goto-char start)
      (while (re-search-forward
	      "[^　、。，．＿ー―‐〜‘’“”（）［］｛｝〈〉＝′″￥]+"
	      nil t)
	(japanese-hankaku-region (match-beginning 0) (match-end 0) t))
      (goto-char start)
      ;; Exclude ">　" in order not to break paragraph start.
      (while (re-search-forward "\\([!-=?-~]\\)　\\|　\\([!-~]\\)" nil t)
	(if (match-beginning 1)
	    (replace-match "\\1 ")
	  (unless (memq (char-before (match-beginning 0)) '(nil ?\n ?>))
	    (replace-match " \\2"))
	  (backward-char 1)))
      (goto-char start)
      (while (re-search-forward "\\([!-~]\\)、[ 　]*\\([!-~]\\)" nil t)
	(replace-match "\\1, \\2")
	(backward-char 1))
      (goto-char start)
      (while (re-search-forward "，\\(\\cj\\)" nil t)
	(replace-match "、\\1")
	(backward-char 1))
      (goto-char start)
      (while (re-search-forward "\\(\\cj\\)，" nil t)
	(replace-match "\\1、"))
      (goto-char start)
      (while (re-search-forward "\\([0-9]\\)，\\([0-9][0-9][0-9][^0-9]\\)"
				nil t)
	(replace-match "\\1,\\2")
	(backward-char 2))
      (goto-char start)
      (while (re-search-forward "\
\\([0-9]\\)\\(?:\\(．\\)\\|\\(＿\\)\\|\\(―\\)\\|\\(‐\\)\\)\\([0-9]\\)"
				nil t)
	(replace-match (cond ((match-beginning 2)
			      "\\1.\\6")
			     ((match-beginning 3)
			      "\\1_\\6")
			     ((or (match-beginning 4) (match-beginning 5))
			      "\\1-\\6")))
	(backward-char 1))
      (when (eq w3m-output-coding-system 'utf-8)
	(goto-char start)
	(while (re-search-forward "\\([‘“‘“]\\)\\|[°’”°′″]" nil t)
	  (if (match-beginning 1)
	      (or (memq (char-before (match-beginning 1)) '(?  ?　))
		  (string-equal (buffer-substring
				 (match-beginning 1)
				 (max (- (match-beginning 1) 6)
				      start))
				"&nbsp;")
		  (progn
		    (backward-char 1)
		    (insert " ")
		    (forward-char 1)))
	    (unless (looking-at "?:[ 　]\\|&nbsp;")
	      (insert " ")))))

      ;; Do wakachi-gaki.
      ;; FIXME:“花の中 3トリオ”“ベスト 8進出”
      (goto-char start)
      (while (re-search-forward
	      "\\(\\cj\\)\\(?:[ 　]\\|&nbsp;\\)\\([])>}]\
\\|&#\\(?:62\\|187\\|8217\\|8221\\|8250\\|8969\\|8971\\|9002\\);\
\\|&\\(?:gt\\|raquo\\|rsquo\\|rdquo\\|rsaquo\\|rceil\\|rfloor\\|rang\\);\\)\
\\|\\([(<[{]\\|&#\\(?:60\\|171\\|8216\\|8220\\|8249\\|8968\\|8970\\|9001\\);\
\\|&\\(?:lt\\|laquo\\|lsquo\\|ldquo\\|lsaquo\\|lceil\\|lfloor\\|lang\\);\\)\
\\(?:[ 　]\\|&nbsp;\\)\\(\\cj\\)"
	      nil t)
	(replace-match (if (match-beginning 1) "\\1\\2" "\\3\\4"))
	(backward-char 1))
      (goto-char start)
      (while (re-search-forward "\
\\(\\(?:[^0-9]\\|\\`\\)\\cj\\)\\([0-9]+\\(?:[,.][0-9]+\\)*[^0-9]\\)\
\\|\\(\\(?:[^0-9]\\|\\`[0-9]*\\)[!-/:-=?-~][0-9]+\\(?:[,.][0-9]+\\)*\\)\
\\(\\cj\\)\
\\|\\([0-9]\\)\\(\\cH[^0-9]\\)\
\\|\\(\\cj\\)\\([(<A-Z[a-z{]\
\\|&#\\(?:60\\|171\\|8216\\|8220\\|8249\\|8968\\|8970\\|9001\\);\
\\|&\\(?:lt\\|laquo\\|lsquo\\|ldquo\\|lsaquo\\|lceil\\|lfloor\\|lang\\);\\)\
\\|\\([]%)>A-Za-z}]\
\\|&#\\(?:62\\|187\\|8217\\|8221\\|8250\\|8969\\|8971\\|9002\\);\
\\|&\\(?:gt\\|raquo\\|rsquo\\|rdquo\\|rsaquo\\|rceil\\|rfloor\\|rang\\);\\)\
\\(\\cj\\)"
				nil t)
	(cond
	 ((match-beginning 1)
	  (unless (or
		   (and (member (match-string 1)
				'("明治" "大正" "昭和" "平成"))
			(eq (char-before) ?年))
		   (and (member (match-string 1) '("午前" "午後"))
			(eq (char-before) ?時))
		   (memq (char-before (match-end 1))
			 '(?　 ?＋ ?− ?± ?× ?÷ ?＝ ?≠ ?≦ ?≧ ?≒
			       ?≪ ?≫))
		   (and (memq (char-before (match-end 1)) '(?第 ?約))
			(memq ?j
			      (shimbun-char-category-list (char-before)))))
	    (replace-match "\\1 \\2"))
	  (goto-char (match-end 1)))
	 ((match-beginning 3)
	  (replace-match "\\3 \\4")
	  (goto-char (match-end 3)))
	 ((match-beginning 5)
	  (unless (memq (char-after (match-beginning 6)) '(?つ))
	    (replace-match "\\5 \\6"))
	  (goto-char (match-end 5)))
	 ((match-beginning 7)
	  (unless (eq (char-after (match-beginning 7)) ?　)
	    (replace-match "\\7 \\8"))
	  (goto-char (match-end 7)))
	 (t
	  (unless (string-equal (buffer-substring
				 (max (- (match-beginning 10) 3)
				      (point-min))
				 (match-beginning 10))
				"<p>")
	    (replace-match (concat "\\9 " (match-string 10))))
	  (goto-char (match-end 9)))))
      (goto-char start)
      (let ((regexp
	     (if (eq w3m-output-coding-system 'utf-8)
		 "\\(\\cG\\|\\cg\\)\\(\\cj\\)\\|\\(\\cj\\)\\(\\cG\\|\\cg\\)"
	       "\\(\\cg\\)\\(\\cj\\)\\|\\(\\cj\\)\\(\\cg\\)")))
	(while (re-search-forward regexp nil t)
	  (if (match-beginning 1)
	      (unless (eq (char-before) ?　)
		(replace-match "\\1 \\2"))
	    (unless (eq (char-after (match-beginning 3)) ?　)
	      (replace-match "\\3 \\4")))
	  (backward-char 1)))

      ;; Finally strip useless space.
      (goto-char start)
      (while (re-search-forward "\\(※\\) \\([0-9]\\)" nil t)
	(replace-match "\\1\\2"))
      (goto-char start)
      (let ((regexp
	     (if (eq w3m-output-coding-system 'utf-8)
		 (eval-when-compile
		   (let ((chars "〔〈《「『〖【＂（、。，．・゛゜￣ヽヾゝゞ〃\
〜（）〔〕［］｛｝〈〉《》「」『』【】"))
		     (concat "\\(?:[ 　]\\|&nbsp;\\)\\([" chars "℃々℃]\\)"
			     "\\|\\([" chars "]\\)\\(?:[ 　]\\|&nbsp;\\)")))
	       (eval-when-compile
		 (let ((chars "‘“〔〈《「『〖【°′″§＂（、。，．・゛゜¨\
￣ヽヾゝゞ〃〜‖…‥‘’“”（）〔〕［］｛｝〈〉《》「」『』【】°′″§"))
		   (concat "\\(?:[ 　]\\|&nbsp;\\)\\([" chars "℃々℃]\\)"
			   "\\|\\([" chars "]\\)\\(?:[ 　]\\|&nbsp;\\)"))))))
	(while (re-search-forward regexp nil t)
	  (goto-char (match-beginning 0))
	  (if (match-beginning 1)
	      (if (or (bobp)
		      (eq (save-match-data
			    (when (re-search-backward ">[\t\n ]*" nil t)
			      (match-end 0)))
			  (match-beginning 0)))
		  ;; Don't break paragraph start.
		  (goto-char (match-beginning 1))
		(delete-region (goto-char (match-beginning 0))
			       (match-beginning 1)))
	    (delete-region (match-end 2) (match-end 0)))))
      (goto-char (point-max)))))

(defun shimbun-japanese-hankaku-region (start end &optional quote)
  "Convert Japanese zenkaku ASCII chars between START and END into hankaku.
There are exceptions; some chars and the ones in links aren't converted,
and \"＜\", \"＞\" and \"＆\" are quoted if QUOTE is non-nil."
  (setq end (set-marker (make-marker) end))
  (while start
    (goto-char start)
    (set-match-data nil)
    (re-search-forward "<a[\t\n\r ]+\\(?:[^\t\n\r ]+[\t\n\r ]+\\)*\
\\(?:href=\"\\([^\"]+\\)\\|href=\'\\([^']+\\)\\)\
\\|<img[\t\n\r ]+\\(?:[^\t\n\r ]+[\t\n\r ]+\\)*\
\\(?:src=\"\\([^\"]+\\)\\|src=\'\\([^']+\\)\\)"
		       end t)
    (shimbun-japanese-hankaku-region-1
     (prog1
	 start
       (setq start (cadr (match-data)))) ;; marker of (match-end 0)
     (or (match-beginning 1) (match-beginning 2) (match-beginning 3)
	 (match-beginning 4) end)
     quote))
  (set-marker end nil))

(defun shimbun-japanese-hankaku-buffer (&optional quote)
  "Convert Japanese zenkaku ASCII chars in the current buffer into hankaku.
Sections surrounded by the <pre>...</pre> tags are not processed.
There are exceptions; some chars aren't converted, and \"＜\", \"＞\" and
\"＆\" are quoted if QUOTE is non-nil."
  (let ((start (goto-char (point-min))))
    (while (search-forward "<pre>" nil t)
      (when (> (match-beginning 0) start)
	(shimbun-japanese-hankaku-region start (match-beginning 0) quote)
	(forward-char 5))
      (search-forward "</pre>" nil 'move)
      (setq start (point)))
    (unless (eobp)
      (shimbun-japanese-hankaku-region start (point-max) quote))))

(provide 'shimbun)

;;; shimbun.el ends here
