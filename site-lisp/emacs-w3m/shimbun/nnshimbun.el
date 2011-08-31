;;; nnshimbun.el --- interfacing with web newspapers

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          ARISAWA Akihiro    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: news

;; This file is a part of emacs-w3m.

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

;; This is a Gnus back end to read web contents all over the world.
;; This back end requires one or more shimbun modules to make it work.
;; There are many pre-made shimbun modules designed for various web
;; sites, moreover, you can even make the modules for the specific
;; sites by yourself.
;;
;; Note that you need to have APEL and FLIM packages installed before
;; installing this back end together with emacs-w3m.  See the emacs-w3m
;; Info manual for further informations.

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'nnoo)
(require 'nnheader)
(require 'nnmail)

;; In some platforms, to load gnus-bcklg.el might fail because of the
;; reason `shell-file-name' has been set mistakenly or the reason the
;; uncompface program is not available.  That is due to the code which
;; determines the default value of `gnus-article-compface-xbm'.  See
;; gnus-ems.el which has been distributed with Emacs 21.1-21.4.  So,
;; we take the following countermeasure to load gnus-bcklg.el safely.
(eval-and-compile
  (condition-case err
      (require 'gnus-bcklg)
    (error
     (if (fboundp 'shell-command-to-string)
	 (let ((fn (symbol-function 'shell-command-to-string)))
	   (fset 'shell-command-to-string (lambda (command) ""))
	   (unwind-protect
	       (require 'gnus-bcklg)
	     (fset 'shell-command-to-string fn)))
       (signal (car err) (cdr err))))))

(require 'shimbun)

(eval-and-compile
  (autoload 'gnus-declare-backend "gnus-start")
  (autoload 'gnus-ephemeral-group-p "gnus")
  (autoload 'gnus-group-change-level "gnus-start")
  (autoload 'gnus-group-goto-group "gnus-group")
  (autoload 'gnus-group-group-level "gnus-group")
  (autoload 'gnus-group-insert-group-line-info "gnus-group")
  (autoload 'gnus-group-make-group "gnus-group")
  (autoload 'gnus-group-prefixed-name "gnus")
  (autoload 'gnus-group-read-ephemeral-group "gnus-group")
  (autoload 'gnus-group-remove-mark "gnus-group")
  (autoload 'gnus-group-short-name "gnus")
  (autoload 'gnus-group-update-group-line "gnus-group")
  (autoload 'gnus-kill-ephemeral-group "gnus")
  (autoload 'gnus-summary-refer-article "gnus-sum")
  (autoload 'message-make-date "message")
  (autoload 'parse-time-string "parse-time"))

(eval-when-compile
  (defvar gnus-level-default-subscribed)
  (defvar gnus-level-killed)
  (defvar gnus-level-subscribed))

(defgroup nnshimbun nil
  "Reading web contents with Gnus."
  :group 'gnus)


;; Customizable variables:

(defcustom nnshimbun-keep-backlog 300
  "*If non-nil, nnshimbun will keep read articles for later re-retrieval.
If it is a number N, then nnshimbun will keep only the last N articles
read.  If it is neither nil nor a number, nnshimbun will keep all read
articles.  That is not a good idea, though.

Note that smaller values may spoil the `prefetch-articles' feature,
since nnshimbun uses the backlog to keep the prefetched articles."
  :group 'nnshimbun
  :type '(choice (const :tag "Off" nil)
		 (integer :format "%t: %v\n"
			  :size 0)
		 (sexp :format "All\n"
		       :match (lambda (widget value)
				(and value (not (numberp value))))
		       :value t)))

(defcustom nnshimbun-keep-unparsable-dated-articles t "\
*If non-nil, nnshimbun won't expire the articles of which the date is unknown."
  :group 'nnshimbun
  :type 'boolean)

(defcustom nnshimbun-default-group-level nil
  "Integer specifies the default nnshimbun group level or nil.
It is applied when an nnshimbun group is newly created.  If it is nil,
the value of `gnus-level-default-subscribed' will be used."
  :group 'nnshimbun
  :type '(radio (const :format "%v  " nil) (integer :value 3)))


;; The nnshimbun group parameter:

(defvar nnshimbun-group-parameters-custom
  '(list :format "Nnshimbun group parameters.\
  Check the leftmost button for customizing.\n%v"
	 (checklist :inline t
		    (list :inline t :format "%v"
			  (const :format "" index-range)
			  (choice :tag "Index range"
				  :value all
				  (const all)
				  (const last)
				  (integer :tag "pages")))
		    (list :inline t :format "%v"
			  (const :format "" prefetch-articles)
			  (choice :tag "Prefetch articles"
				  :value off
				  (const on)
				  (const off)))
		    (list :inline t :format "%v"
			  (const :format "" encapsulate-images)
			  (choice :tag "Encapsulate article"
				  :value on
				  (const on)
				  (const off)))
		    (list :inline t :format "%v"
			  (const :format "" expiry-wait)
			  (choice :tag "Expire wait"
				  :value never
				  (const never)
				  (const immediate)
				  (integer :tag "days"))))
	 (repeat :inline t :tag "Others"
		 (list :inline t :format "%v"
		       (symbol :tag "Keyword")
		       (sexp :tag "Value"))))
  "A type definition for customizing the nnshimbun group parameters.")

(eval-and-compile
  (defconst nnshimbun-is-compiled-for-modern-gnus
    (eval-when-compile
      ;; The `gnus-define-group-parameter' macro isn't available in old Gnusae,
      ;; e.g. installed Emacs 21 may contain Gnus v5.9 which is the old Gnus.
      (and (fboundp 'gnus-define-group-parameter)
	   (condition-case nil
	       (macroexpand '(gnus-define-group-parameter PARAM))
	     (error nil))
	   t))
    "Non-nil means the nnshimbun.elc file is compiled for the modern Gnus.
Users should never modify the value."))

(eval-and-compile
  (let ((flag (if nnshimbun-is-compiled-for-modern-gnus
		  (if (fboundp 'gnus-define-group-parameter)
		      nil
		    (message "\
Warning: nnshimbun.elc is compiled for the newer Gnus,\
 you should recompile it")
		    (sit-for 1)
		    nil)
		(if (fboundp 'gnus-define-group-parameter)
		    (progn
		      (message "\
Warning: nnshimbun.elc is compiled for the old Gnus,\
 you should recompile it")
		      (sit-for 1)
		      '(nil . t))
		  '(t . t)))))
    (when (car flag)
      (defmacro gnus-define-group-parameter (&rest args) nil))
    (when (cdr flag)
      (defun nnshimbun-find-group-parameters (name)
	"Return an nnshimbun GROUP's group parameters."
	(when name
	  (or (gnus-group-find-parameter name 'nnshimbun-group-parameters t)
	      (assoc-default
	       name
	       (when (boundp 'nnshimbun-group-parameters-alist)
		 (symbol-value 'nnshimbun-group-parameters-alist))
	       (function string-match))))))))

(gnus-define-group-parameter
 ;; This definition provides the `nnshimbun-group-parameters' group
 ;; parameter, the `nnshimbun-group-parameters-alist' variable and the
 ;; `nnshimbun-find-group-parameters' function.
 nnshimbun-group-parameters
 :type list
 :function nnshimbun-find-group-parameters
 :function-document "\
Return nnshimbun GROUP's group parameters as a plist."
 :variable nnshimbun-group-parameters-alist
 :variable-default nil
 :variable-document "\
Alist of nnshimbun group parameters.
Each element should be a cons of a regexp matching group names and a
plist which contains keyword-value pairs, like the following:

'(\"^nnshimbun\\\\+asahi:\" index-range all prefetch-articles off
  encapsulate-images on expiry-wait 6)

`index-range' specifies the number of indices that should be checked
to detect new articles.  The valid values include:

      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`prefetch-articles' specifies whether to pre-fetch articles when
scanning the group.

`encapsulate-images' specifies whether to embed inline images into
shimbun articles.

`expiry-wait' overrides the parameter of the same name in Gnus."
 :variable-group nnshimbun
 :variable-type `(repeat (cons :format "%v" (regexp :tag "Group name regexp"
						    :value "^nnshimbun\\+")
			       ,nnshimbun-group-parameters-custom))
 :parameter-type nnshimbun-group-parameters-custom
 :parameter-document "\
Group parameters for the nnshimbun groups.

`Index range' specifies the number of indices that should be checked
to detect new articles.  The valid values include:

      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`Prefetch articles' specifies whether to pre-fetch articles when
scanning the group.

`Encapsulate article' specifies whether to embed inline images into
shimbun articles.

`Expire wait' overrides the parameter of the same name in Gnus.")


;; The back end definitions:

(gnus-declare-backend "nnshimbun" 'address)
(nnoo-declare nnshimbun)

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "*Directory where nnshimbun will store NOV and marks files.
Actually, you can find those files in the SERVER/GROUP/ subdirectory.")

(defvoo nnshimbun-nov-is-evil nil
  "*If non-nil, nnshimbun won't use the NOV databases to retrieve headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-marks-is-evil nil
  "If non-nil, Gnus will never generate and use marks file for shimbun spools.
Using marks files makes it possible to backup and restore shimbun groups
separately from `.newsrc.eld'.  If you have, for some reason, set this
to t, and want to set it to nil again, you should always remove the
corresponding marks file (usually named `.marks' in the shimbun group
directory, but see `nnshimbun-marks-file-name') for the group.  Then the
marks file will be regenerated properly by Gnus.")

(defvoo nnshimbun-marks-file-name ".marks")

(defvoo nnshimbun-marks nil)

(defvar nnshimbun-marks-modtime (gnus-make-hashtable))

(defvoo nnshimbun-pre-fetch-article 'off
  "*If it is neither `off' nor nil, nnshimbun will pre-fetch articles.
It is done when scanning the group.  This simply provides a default
value for all the nnshimbun groups.  You can use the
`prefecth-articles' nnshimbun group parameter for each nnshimbun group.")

(defvoo nnshimbun-encapsulate-images shimbun-encapsulate-images
  "*If neither `off' or nil, nnshimbun will embed inline images in articles.
This simply provides a default value for all the nnshimbun groups.
You can use the `encapsulate-images' nnshimbun group parameter for
each nnshimbun group.")

(defvoo nnshimbun-index-range 2
  "*The number of indices that should be checked to detect new articles.
`all' or nil is for all indices, `last' is for the last index, and an
integer N is for the last N pages of indices.  This simply provides a
default value for all the nnshimbun groups.  You can use the
`index-range' nnshimbun group parameter for each nnshimbun group.")


;; This value will be set by the `nnshimbun-open-server' function.
(defvoo nnshimbun-shimbun nil)

(defvoo nnshimbun-status-string "")
(defvoo nnshimbun-backlog-articles nil)
(defvoo nnshimbun-backlog-hashtb nil)

(defmacro nnshimbun-current-server ()
  '(nnoo-current-server 'nnshimbun))

(defmacro nnshimbun-current-group ()
  '(shimbun-current-group nnshimbun-shimbun))

(defun nnshimbun-group-prefixed-name (group &optional server)
  (gnus-group-prefixed-name (or group (nnshimbun-current-group))
			    (list 'nnshimbun
				  (or server (nnshimbun-current-server)))))

(defun nnshimbun-group-ephemeral-p (group)
  (gnus-ephemeral-group-p (nnshimbun-group-prefixed-name
			   group (shimbun-server nnshimbun-shimbun))))

(defmacro nnshimbun-backlog (&rest form)
  `(let ((gnus-keep-backlog nnshimbun-keep-backlog)
	 (gnus-backlog-buffer
	  (format " *nnshimbun backlog %s*" (nnshimbun-current-server)))
	 (gnus-backlog-articles nnshimbun-backlog-articles)
	 (gnus-backlog-hashtb nnshimbun-backlog-hashtb))
     (unwind-protect
	 (progn ,@form)
       (setq nnshimbun-backlog-articles gnus-backlog-articles
	     nnshimbun-backlog-hashtb gnus-backlog-hashtb))))
(put 'nnshimbun-backlog 'lisp-indent-function 0)
(put 'nnshimbun-backlog 'edebug-form-spec t)

(defmacro nnshimbun-find-parameter (group symbol &optional full-name-p)
  "Return GROUP's nnshimbun group parameter corresponding to SYMBOL.
If FULL-NAME-P is non-nil, it assumes that GROUP is a full name."
  (let ((name (if full-name-p
		  group
		`(nnshimbun-group-prefixed-name ,group))))
    (cond ((eq 'index-range (eval symbol))
	   `(or (plist-get (nnshimbun-find-group-parameters ,name)
			   'index-range)
		nnshimbun-index-range))
	  ((eq 'prefetch-articles (eval symbol))
	   `(let ((val (or (plist-get (nnshimbun-find-group-parameters ,name)
				      'prefetch-articles)
			   nnshimbun-pre-fetch-article)))
	      (if (eq 'off val)
		  nil
		val)))
	  ((eq 'encapsulate-images (eval symbol))
	   `(let ((val (or (plist-get (nnshimbun-find-group-parameters ,name)
				      'encapsulate-images)
			   nnshimbun-encapsulate-images)))
	      (if (eq 'off val)
		  nil
		val)))
	  ((eq 'expiry-wait (eval symbol))
	   (if full-name-p
	       `(or (plist-get (nnshimbun-find-group-parameters ,group)
			       'expiry-wait)
		    (gnus-group-find-parameter ,group 'expiry-wait))
	     `(let ((name ,name))
		(or (plist-get (nnshimbun-find-group-parameters name)
			       'expiry-wait)
		    (gnus-group-find-parameter name 'expiry-wait)))))
	  (t
	   `(plist-get (nnshimbun-find-group-parameters ,name) ,symbol)))))

(defun nnshimbun-decode-group-name (group)
  (if (and group (mm-coding-system-p 'utf-8))
      (mm-decode-coding-string group 'utf-8)
    group))

(defun nnshimbun-nov-buffer-name (&optional group)
  (format " *nnshimbun overview %s %s*"
	  (nnshimbun-current-server)
	  (or group (nnshimbun-current-group))))

(defun nnshimbun-group-pathname (&optional group file)
  "Return an absolute file name of FILE for GROUP."
  (nnmail-group-pathname (or group (nnshimbun-current-group))
			 (expand-file-name (nnshimbun-current-server)
					   nnshimbun-directory)
			 file))


;; Interface functions:

(nnoo-define-basics nnshimbun)

(defun nnshimbun-possibly-change-group (group &optional server)
  (when (if server
	    (nnshimbun-open-server server)
	  nnshimbun-shimbun)
    (or (not group)
	(condition-case err
	    (shimbun-open-group nnshimbun-shimbun group)
	  (error
	   (nnheader-report 'nnshimbun "%s" (error-message-string err)))))))

(deffoo nnshimbun-open-server (server &optional defs)
  (or (nnshimbun-server-opened server)
      (let ((file-name-coding-system nnmail-pathname-coding-system)
	    shimbun)
	(when (condition-case err
		  (setq shimbun
			(shimbun-open (or (cadr (assq 'nnshimbun-address defs))
					  server)
				      (luna-make-entity 'shimbun-gnus-mua)))
		(error
		 (nnheader-report 'nnshimbun "%s" (error-message-string err))))
	  (nnoo-change-server 'nnshimbun server
			      (cons (list 'nnshimbun-shimbun shimbun) defs))
	  t))))

(deffoo nnshimbun-close-server (&optional server)
  (when (nnshimbun-server-opened server)
    (when nnshimbun-shimbun
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(when (buffer-live-p (nnshimbun-nov-buffer-name group))
	  (nnshimbun-write-nov group t)))
      (shimbun-close nnshimbun-shimbun)))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnoo-close-server 'nnshimbun server)
  t)

(defun nnshimbun-replace-date-header (article header)
  ;; This function definition should be replaced with the proper one
  ;; when it is called at the first time.
  (require 'gnus-sum)
  (require 'bytecomp)
  (defalias 'nnshimbun-replace-date-header
    (byte-compile
     '(lambda (article header)
	"Replace ARTICLE's date header with HEADER."
	(let ((x (gnus-summary-article-header article)))
	  (when x
	    (mail-header-set-date x (shimbun-header-date header)))))))
  (funcall 'nnshimbun-replace-date-header article header))

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let ((header (with-current-buffer (nnshimbun-open-nov group)
		    (and (nnheader-find-nov-line article)
			 (nnshimbun-parse-nov))))
	  original-id)
      (when header
	(setq original-id (shimbun-header-id header))
	(with-current-buffer (or to-buffer nntp-server-buffer)
	  (erase-buffer)
	  (let ((shimbun-encapsulate-images
		 (nnshimbun-find-parameter group 'encapsulate-images)))
	    (shimbun-article nnshimbun-shimbun header))
	  (when (> (buffer-size) 0)
	    ;; Replace the date string in the `gnus-newsgroup-data' variable
	    ;; with the newly retrieved article's one.  It may be kludge.
	    (nnshimbun-replace-date-header article header)
	    (nnshimbun-replace-nov-entry group article header original-id)
	    (nnshimbun-backlog
	      (gnus-backlog-enter-article group article (current-buffer)))
	    (nnheader-report 'nnshimbun "Article %s retrieved"
			     (shimbun-header-id header))
	    (cons group article)))))))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (setq group (nnshimbun-decode-group-name group))
  (when (nnshimbun-possibly-change-group group server)
    (if (or (integerp article)
	    (when (stringp article)
	      (setq article
		    (or (when (or group
				  (setq group (nnshimbun-current-group)))
			  (nnshimbun-search-id group article))
			(catch 'found
			  (dolist (x (shimbun-groups nnshimbun-shimbun))
			    (and (nnshimbun-possibly-change-group x)
				 (setq x (nnshimbun-search-id x article))
				 (throw 'found x))))))))
	(nnshimbun-request-article-1 article group server to-buffer)
      (nnheader-report 'nnshimbun "Couldn't retrieve article: %s"
		       (prin1-to-string article)))))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (setq group (nnshimbun-decode-group-name group))
  (if (not (nnshimbun-possibly-change-group group server))
      (nnheader-report 'nnshimbun "Invalid group")
    (let (beg end lines)
      (with-current-buffer (nnshimbun-open-nov group)
	(when (and (nnshimbun-group-ephemeral-p group)
		   (zerop (buffer-size)))
	  (nnshimbun-generate-nov-database group))
	(goto-char (point-min))
	(setq beg (ignore-errors (read (current-buffer))))
	(goto-char (point-max))
	(forward-line -1)
	(setq end (ignore-errors (read (current-buffer)))
	      lines (count-lines (point-min) (point-max))))
      (nnheader-report 'nnshimbun "Selected group %s" group)
      (nnheader-insert "211 %d %d %d %s\n"
		       lines (or beg 0) (or end 0) group))))

(deffoo nnshimbun-request-scan (&optional group server)
  (setq group (nnshimbun-decode-group-name group))
  (when (nnshimbun-possibly-change-group nil server)
    (if group
	(nnshimbun-generate-nov-database group)
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(nnshimbun-generate-nov-database group)))))

(deffoo nnshimbun-close-group (group &optional server)
  (setq group (nnshimbun-decode-group-name group))
  (nnshimbun-write-nov group (nnshimbun-group-ephemeral-p group)))

(deffoo nnshimbun-request-list (&optional server)
  (when (nnshimbun-possibly-change-group nil server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(when (nnshimbun-possibly-change-group group)
	  (let (beg end)
	    (with-current-buffer (nnshimbun-open-nov group)
	      (goto-char (point-min))
	      (setq beg (ignore-errors (read (current-buffer))))
	      (goto-char (point-max))
	      (forward-line -1)
	      (setq end (ignore-errors (read (current-buffer)))))
	    (insert (format "%s %d %d n\n" group (or end 0) (or beg 0)))))))
    t)) ;; return value

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (setq group (nnshimbun-decode-group-name group))
  (when (nnshimbun-possibly-change-group group server)
    (if (nnshimbun-retrieve-headers-with-nov articles group fetch-old)
	'nov
      (with-current-buffer nntp-server-buffer
	(erase-buffer)
	(let (header)
	  (dolist (art articles)
	    (when (and (if (stringp art)
			   (setq art (nnshimbun-search-id group art))
			 (integerp art))
		       (setq header
			     (with-current-buffer (nnshimbun-open-nov group)
			       (and (nnheader-find-nov-line art)
				    (nnshimbun-parse-nov)))))
	      (insert (format "220 %d Article retrieved.\n" art))
	      (shimbun-header-insert nnshimbun-shimbun header)
	      (insert ".\n")
	      (delete-region (point) (point-max)))))
	'header))))

(defun nnshimbun-retrieve-headers-with-nov (articles &optional group fetch-old)
  (unless (or gnus-nov-is-evil nnshimbun-nov-is-evil)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert-buffer-substring (nnshimbun-open-nov group))
      (unless (and fetch-old (not (numberp fetch-old)))
	(nnheader-nov-delete-outside-range
	 (if fetch-old
	     (max 1 (- (car articles) fetch-old))
	   (car articles))
	 (nth (1- (length articles)) articles)))
      t)))


;; Functions to manage the NOV databases:

(defvar nnshimbun-tmp-string nil
  "Internal variable used as a rest to keep a temporary string.
The macro `nnshimbun-string-or' uses it exclusively.")

(defmacro nnshimbun-string-or (&rest strings)
  "Return the first element of STRINGS that is a non-blank string.
It should run fast, especially if two strings are given.  nil is
allowed for each string."
  (cond ((null strings)
	 nil)
	((= 1 (length strings))
	 ;; Return irregularly nil if one blank string is given.
	 `(unless (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	    nnshimbun-tmp-string))
	((= 2 (length strings))
	 ;; Return the second string when the first string is blank.
	 `(if (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	      ,(cadr strings)
	    nnshimbun-tmp-string))
	(t
	 `(let ((strings (list ,@strings)))
	    (while strings
	      (setq strings (if (zerop (length (setq nnshimbun-tmp-string
						     (car strings))))
				(cdr strings))))
	    nnshimbun-tmp-string))))

(defun nnshimbun-insert-nov (number header &optional id)
  (insert "\n")
  (backward-char 1)
  (let ((header-id (nnshimbun-string-or (shimbun-header-id header)))
	;; Make `princ' print string in the current buffer.
	(standard-output (current-buffer))
	(xref (nnshimbun-string-or (shimbun-header-xref header)))
	(extra (shimbun-header-extra header))
	(start (point)))
    (and (stringp id)
	 header-id
	 (string-equal id header-id)
	 (setq id nil))
    (princ number)
    (insert
     "\t"
     (nnshimbun-string-or (shimbun-header-subject header) "(none)") "\t"
     (nnshimbun-string-or (shimbun-header-from header) "(nobody)") "\t"
     (nnshimbun-string-or (shimbun-header-date header) (message-make-date))
     "\t"
     (or header-id (nnmail-message-id)) "\t"
     (or (shimbun-header-references header) "") "\t")
    (princ (or (shimbun-header-chars header) 0))
    (insert "\t")
    (princ (or (shimbun-header-lines header) 0))
    (insert "\t")
    (when xref
      (insert "Xref: " xref))
    (insert "\t")
    (when id
      (insert "X-Nnshimbun-Id: " id "\t"))
    (while extra
      (insert (format "%s: %s\t" (symbol-name (caar extra))
		      (shimbun-mime-encode-string (cdar extra))))
      (setq extra (cdr extra)))
    ;; Replace newlines with spaces in the current NOV line.
    (while (progn
	     (forward-line 0)
	     (> (point) start))
      (backward-delete-char 1)
      (insert " "))
    (forward-line 1)))

(defun nnshimbun-generate-nov-database (group)
  (when (nnshimbun-possibly-change-group group)
    (with-current-buffer (nnshimbun-open-nov group)
      (goto-char (point-max))
      (forward-line -1)
      (let* ((i (or (ignore-errors (read (current-buffer))) 0))
	     (name (unless (nnshimbun-group-ephemeral-p group)
		     (nnshimbun-group-prefixed-name group)))
	     (pre-fetch (when name
			  (nnshimbun-find-parameter name 'prefetch-articles t))))
	(dolist (header
		 (shimbun-headers nnshimbun-shimbun
				  (if name
				      (nnshimbun-find-parameter name
								'index-range t)
				    'last)))
	  (let ((article
		 (nnshimbun-search-id group (shimbun-header-id header))))
	    (if article
		(nnshimbun-replace-nov-entry group article header)
	      (goto-char (point-max))
	      (nnshimbun-insert-nov (setq i (1+ i)) header)
	      (when pre-fetch
		(with-temp-buffer
		  (nnshimbun-request-article-1 i group nil
					       (current-buffer))))))))
      (nnshimbun-write-nov group))))

(defun nnshimbun-replace-nov-entry (group article header &optional id)
  (with-current-buffer (nnshimbun-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (nnshimbun-insert-nov article header id))))

(defun nnshimbun-search-id (group id)
  (with-current-buffer (nnshimbun-open-nov group)
    (goto-char (point-min))
    (let (found case-fold-search)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (point-at-bol) t))))
	    (forward-line 1)
	  (forward-line 0)
	  (setq found t)))
      (unless found
	(goto-char (point-min))
	(setq id (concat "X-Nnshimbun-Id: " id))
	(while (and (not found)
		    (search-forward id nil t))
	  (if (not (search-backward "\t" (point-at-bol) t 8))
	      (forward-line 1)
	    (forward-line 0)
	    (setq found t))))
      (when found
	(ignore-errors (read (current-buffer)))))))

;; This function is defined as the alternative of `nnheader-parse-nov'
;; in order to keep the compatibility between T-gnus and Gnus.
(defun nnshimbun-parse-nov ()
  (let ((eol (point-at-eol)))
    (let ((number  (nnheader-nov-read-integer))
	  (subject (nnheader-nov-field))
	  (from    (nnheader-nov-field))
	  (date    (nnheader-nov-field))
	  (id      (nnheader-nov-read-message-id))
	  (refs    (nnheader-nov-field))
	  (chars   (nnheader-nov-read-integer))
	  (lines   (nnheader-nov-read-integer))
	  (xref    (unless (eq (char-after) ?\n)
		     (when (looking-at "Xref: ")
		       (goto-char (match-end 0)))
		     (nnheader-nov-field)))
	  (extra   (nnheader-nov-parse-extra)))
      (mapc (lambda (elem) (setcdr elem (eword-decode-string (cdr elem))))
	    extra)
      (shimbun-make-header number subject from date
			   (or (cdr (assq 'X-Nnshimbun-Id extra)) id)
			   refs chars lines xref extra))))

(defun nnshimbun-open-nov (group)
  (let ((buffer (nnshimbun-nov-buffer-name group)))
    (unless (gnus-buffer-live-p buffer)
      (with-current-buffer (gnus-get-buffer-create buffer)
	(erase-buffer)
	(let ((file-name-coding-system nnmail-pathname-coding-system)
	      (nov (nnshimbun-group-pathname group nnshimbun-nov-file-name)))
	  (when (file-exists-p nov)
	    (nnheader-insert-file-contents nov)))
	(set-buffer-modified-p nil)))
    buffer))

(defun nnshimbun-write-nov (group &optional close)
  (let ((buffer (nnshimbun-nov-buffer-name group)))
    (prog1 (or (nnshimbun-group-ephemeral-p group)
	       (not (gnus-buffer-live-p buffer))
	       (let ((file-name-coding-system nnmail-pathname-coding-system))
		 (when (let ((dir (nnshimbun-group-pathname group)))
			 (or (file-directory-p dir)
			     (ignore-errors
			      (make-directory dir t)
			      (file-directory-p dir))))
		   (let ((nov (nnshimbun-group-pathname
			       group nnshimbun-nov-file-name)))
		     (with-current-buffer buffer
		       (when (and (buffer-modified-p)
				  (or (> (buffer-size) 0)
				      (file-exists-p nov)))
			 (nnmail-write-region 1 (point-max) nov nil 'nomesg)
			 (set-buffer-modified-p nil)
			 t))))))
      (when close
	(kill-buffer buffer)))))

(deffoo nnshimbun-request-expire-articles (articles group
						    &optional server force)
  "Do the expiration for the specified ARTICLES in the nnshimbun GROUP.
Note that nnshimbun does not actually delete any articles, it simply
deletes the entry in your own NOV database corresponding to the
article to be expired.  The optional fourth argument FORCE is ignored."
  (setq group (nnshimbun-decode-group-name group))
  (when (nnshimbun-possibly-change-group group server)
    (let* ((expirable (copy-sequence articles))
	   (name (nnshimbun-group-prefixed-name group))
	   ;; If the group's `expiry-wait' parameter is non-nil, the value
	   ;; of `nnmail-expiry-wait' will be bound to that value, and the
	   ;; value of `nnmail-expiry-wait-function' will be bound to nil.
	   ;; See the `gnus-summary-expire-articles' function definition to
	   ;; understand how it works.  If the group's parameter is not
	   ;; specified by the user, the shimbun's default value will be
	   ;; used instead.
	   (expiry-wait
	    (or (nnshimbun-find-parameter name 'expiry-wait t)
		(shimbun-article-expiration-days nnshimbun-shimbun)))
	   (nnmail-expiry-wait (or expiry-wait nnmail-expiry-wait))
	   (nnmail-expiry-wait-function (if expiry-wait
					    nil
					  nnmail-expiry-wait-function))
	   article end time)
      (with-current-buffer (nnshimbun-open-nov group)
	(while expirable
	  (setq article (pop expirable))
	  (when (and (nnheader-find-nov-line article)
		     (setq end (point-at-eol))
		     (not (= (point-max) (1+ end))))
	    (setq time (and (search-forward "\t" end t)
			    (search-forward "\t" end t)
			    (search-forward "\t" end t)
			    (parse-time-string
			     (buffer-substring
			      (point)
			      (if (search-forward "\t" end t)
				  (1- (point))
				end)))))
	    (when (if (setq time (condition-case nil
				     (apply 'encode-time time)
				   (error nil)))
		      (nnmail-expired-article-p name time nil)
		    ;; Inhibit expiration if there's no parsable date
		    ;; and the following option is non-nil.
		    (not nnshimbun-keep-unparsable-dated-articles))
	      (forward-line 0)
	      (delete-region (point) (1+ end))
	      (setq articles (delq article articles)))))
	(nnshimbun-write-nov group))
      articles)))

(deffoo nnshimbun-request-delete-group (group &optional force server)
  "Delete NOV and marks files used for GROUP and the parent directories.
Other files in the directory are also deleted."
  (setq group (nnshimbun-decode-group-name group))
  (when (nnshimbun-possibly-change-group group server)
    (let ((dir (nnshimbun-group-pathname group))
	  (nov (nnshimbun-nov-buffer-name group))
	  files file subdir)
      (when (file-directory-p dir)
	(setq files (directory-files
		     dir  t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	(while files
	  (setq file (pop files))
	  (if (eq t (car (file-attributes file)))
	      ;; `file' is a subdirectory.
	      (setq subdir t)
	    ;; `file' is a file or a symlink.
	    (delete-file file)))
	(unless subdir
	  (delete-directory dir)))
      (setq dir (expand-file-name ".." dir))
      (when (and (file-directory-p dir)
		 (not (directory-files
		       dir t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
	(delete-directory dir))
      (when (gnus-buffer-live-p nov)
	(kill-buffer nov)))
    t))

(deffoo nnshimbun-request-set-mark (group actions &optional server)
  (setq group (nnshimbun-decode-group-name group))
  (when (and (not nnshimbun-marks-is-evil)
	     (nnshimbun-possibly-change-group group server))
    (nnshimbun-open-marks group server)
    (dolist (action actions)
      (let ((range (nth 0 action))
	    (what  (nth 1 action))
	    (marks (nth 2 action)))
	(assert (or (eq what 'add) (eq what 'del)) nil
		"Unknown request-set-mark action: %s" what)
	(dolist (mark marks)
	  (setq nnshimbun-marks
		(gnus-update-alist-soft
		 mark
		 (funcall (if (eq what 'add) 'gnus-range-add
			    'gnus-remove-from-range)
			  (cdr (assoc mark nnshimbun-marks)) range)
		 nnshimbun-marks)))))
    (nnshimbun-save-marks group))
  nil)

(defun nnshimbun-marks-changed-p (group)
  (let ((file (nnshimbun-group-pathname group nnshimbun-marks-file-name)))
    (if (null (gnus-gethash file nnshimbun-marks-modtime))
	t ;; never looked at marks file, assume it has changed
      (not (equal (gnus-gethash file nnshimbun-marks-modtime)
		  (nth 5 (file-attributes file)))))))

(deffoo nnshimbun-request-update-info (group info &optional server)
  (setq group (nnshimbun-decode-group-name group))
  (when (and (not nnshimbun-marks-is-evil)
	     (nnshimbun-possibly-change-group group server)
	     (nnshimbun-marks-changed-p group))
    (nnheader-message 8 "Updating marks for %s..." group)
    (nnshimbun-open-marks group server)
    ;; Update info using `nnshimbun-marks'.
    (mapc (lambda (pred)
	    (unless (memq (cdr pred) gnus-article-unpropagated-mark-lists)
	      (gnus-info-set-marks
	       info
	       (gnus-update-alist-soft
		(cdr pred)
		(cdr (assq (cdr pred) nnshimbun-marks))
		(gnus-info-marks info))
	       t)))
	  gnus-article-mark-lists)
    (let ((seen (cdr (assq 'read nnshimbun-marks))))
      (gnus-info-set-read info
			  (if (and (integerp (car seen))
				   (null (cdr seen)))
			      (list (cons (car seen) (car seen)))
			    seen)))
    (nnheader-message 8 "Updating marks for %s...done" group))
  info)

(defun nnshimbun-possibly-create-directory (group)
  (let ((dir (nnshimbun-group-pathname group))
	(file-name-coding-system nnmail-pathname-coding-system))
    (unless (file-exists-p dir)
      (make-directory (directory-file-name dir) t)
      (nnheader-message 5 "Creating shimbun directory %s" dir))))

(defun nnshimbun-save-marks (group)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(file (nnshimbun-group-pathname group nnshimbun-marks-file-name)))
    (condition-case err
	(progn
	  (nnshimbun-possibly-create-directory group)
	  (with-temp-file file
	    (erase-buffer)
	    (gnus-prin1 nnshimbun-marks)
	    (insert "\n"))
	  (gnus-sethash file
			(nth 5 (file-attributes file))
			nnshimbun-marks-modtime))
      (error (or (gnus-yes-or-no-p
		  (format "Could not write to %s (%s).  Continue? " file err))
		 (error "Cannot write to %s (%s)" file err))))))

(defun nnshimbun-open-marks (group server)
  (let* ((decoded (nnshimbun-decode-group-name group))
	 (file (nnshimbun-group-pathname decoded nnshimbun-marks-file-name))
	 (file-name-coding-system nnmail-pathname-coding-system))
    (if (file-exists-p file)
	(condition-case err
	    (with-temp-buffer
	      (gnus-sethash file (nth 5 (file-attributes file))
			    nnshimbun-marks-modtime)
	      (nnheader-insert-file-contents file)
	      (setq nnshimbun-marks (read (current-buffer)))
	      (dolist (el gnus-article-unpropagated-mark-lists)
		(setq nnshimbun-marks (gnus-remassoc el nnshimbun-marks))))
	  (error (or (gnus-yes-or-no-p
		      (format "Error reading nnshimbun marks file %s (%s).\
  Continuing will use marks from .newsrc.eld.  Continue? " file err))
		     (error "Cannot read nnshimbun marks file %s (%s)"
			    file err))))
      ;; User didn't have a .marks file.  Probably first time
      ;; user of the .marks stuff.  Bootstrap it from .newsrc.eld.
      (let ((info (gnus-get-info
		   (gnus-group-prefixed-name
		    group
		    (gnus-server-to-method
		     (format "nnshimbun:%s" (or server "")))))))
	(setq decoded (if (member server '(nil ""))
			  (concat "nnshimbun:" decoded)
			(format "nnshimbun+%s:%s" server decoded)))
	(nnheader-message 7 "Bootstrapping marks for %s..." decoded)
	(setq nnshimbun-marks (gnus-info-marks info))
	(push (cons 'read (gnus-info-read info)) nnshimbun-marks)
	(dolist (el gnus-article-unpropagated-mark-lists)
	  (setq nnshimbun-marks (gnus-remassoc el nnshimbun-marks)))
	(nnshimbun-save-marks group)
	(nnheader-message 7 "Bootstrapping marks for %s...done" decoded)))))


;; Defining the `shimbun-gnus-mua':

(luna-define-class shimbun-gnus-mua (shimbun-mua) ())

(luna-define-method shimbun-mua-search-id ((mua shimbun-gnus-mua) id)
  (nnshimbun-search-id (shimbun-current-group (shimbun-mua-shimbun mua))
		       id))


;; Functions to follow anchors that point previous articles:

(defun nnshimbun-search-xref (group url)
  "Return the header of the article corresponding to URL in GROUP."
  (let ((buffer (nnshimbun-nov-buffer-name group)))
    (when (gnus-buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-min))
	(let (found)
	  (unless found
	    (goto-char (point-min))
	    (setq url (concat "Xref: " url))
	    (while (and (not found)
			(search-forward url nil t)
			(looking-at "[?\t]"))
	      (if (not (search-backward "\t" (point-at-bol) t 7))
		  (forward-line 1)
		(forward-line 0)
		(setq found t))))
	  (when found
	    (nnshimbun-parse-nov)))))))

;;;###autoload
(defun gnus-summary-refer-shimbun-article (url)
  "Show a shimbun article pointed to by the given URL."
  (interactive "sURL: ")
  (let ((method (gnus-find-method-for-group gnus-newsgroup-name))
	(header))
    (or (and (eq 'nnshimbun (car method))
	     (nnshimbun-possibly-change-group nil (nth 1 method))
	     (setq header (nnshimbun-search-xref
			   (gnus-group-short-name gnus-newsgroup-name) url))
	     (with-current-buffer gnus-summary-buffer
	       (gnus-summary-refer-article (shimbun-header-id header))))
	'w3m-goto-url)))

(defun nnshimbun-setup-article-mode ()
  (set (make-local-variable 'w3m-goto-article-function)
       'gnus-summary-refer-shimbun-article))


;; Command to create an nnshimbun group:

(defvar nnshimbun-server-history nil)

;;;###autoload
(defun gnus-group-make-shimbun-group (server group &optional ephemeral)
  "Create a new nnshimbun group.
The user will be prompted for a SERVER name and a GROUP name.  When
this command is called with a prefix argument, it makes an ephemeral
shimbun group."
  (interactive
   (let ((minibuffer-setup-hook
	  (append minibuffer-setup-hook '(beginning-of-line)))
	 (alist (shimbun-servers-alist))
	 server groups group)
     (unless (eq major-mode 'gnus-group-mode)
       (error "Command invoked outside of a Gnus group buffer"))
     (setq server (completing-read
		   "Shimbun server address [Hit TAB to see candidates]: "
		   alist nil t
		   (car (delete "" nnshimbun-server-history))
		   'nnshimbun-server-history))
     (static-unless (featurep 'xemacs)
       (setq server (string-as-unibyte server)))
     (if (assoc server alist)
	 (let ((shimbun (shimbun-open server)))
	   (setq group (completing-read
			"Group name [Hit TAB to see candidates]: "
			(mapcar 'list (shimbun-groups shimbun))))
	   ;; Unify non-ASCII text.
	   (when (mm-coding-system-p 'utf-8)
	     (setq group (mm-decode-coding-string
			  (mm-encode-coding-string group 'utf-8) 'utf-8)))
	   (unless (shimbun-group-p shimbun group)
	     (setq group nil)))
       (setq server nil))
     (list server group current-prefix-arg)))
  (if (and server group)
      (let (nname)
	(setq server (list 'nnshimbun server)
	      nname (gnus-group-prefixed-name
		     (if (mm-coding-system-p 'utf-8)
			 (mm-encode-coding-string group 'utf-8)
		       group)
		     server))
	(if ephemeral
	    (gnus-group-read-ephemeral-group nname server t
					     (cons (current-buffer)
						   (if (eq major-mode
							   'gnus-summary-mode)
						       'summary 'group)))
	  (when (gnus-ephemeral-group-p nname)
	    (gnus-kill-ephemeral-group nname))
	  (let ((gnus-level-default-subscribed
		 (or nnshimbun-default-group-level
		     gnus-level-default-subscribed)))
	    (gnus-group-make-group group server))))
    (error "Can't find group")))

;;;###autoload
(defun gnus-group-make-shimbun-groups (server)
  "Create all nnshimbun groups prepared for SERVER."
  (interactive
   (let ((minibuffer-setup-hook (append minibuffer-setup-hook
					'(beginning-of-line))))
     (unless (eq major-mode 'gnus-group-mode)
       (error "Command invoked outside of a Gnus group buffer"))
     (list (completing-read
	    "Shimbun server address [Hit TAB to see candidates]: "
	    (shimbun-servers-alist) nil t))))
  (unless (string-equal server "")
    (let ((gnus-level-default-subscribed (or nnshimbun-default-group-level
					     gnus-level-default-subscribed))
	  (gnus-verbose 0)
	  (grps (reverse (shimbun-groups (shimbun-open server))))
	  (inhibit-read-only t)
	  grp group)
      (if grps
	  (when (or (= (length grps) 1)
		    (prog1 (yes-or-no-p (format "\
Are you sure you want to make %d groups for nnshimbun+%s:? "
						(length grps) server))
		      (message "")))
	    (while grps
	      (setq grp (pop grps)
		    group (format "nnshimbun+%s:%s" server grp))
	      (if (gnus-group-entry group)
		  (progn
		    (save-excursion
		      (unless (gnus-group-goto-group group)
			(gnus-group-update-group group)
			(gnus-group-goto-group group))
		      (gnus-group-remove-mark group)
		      (gnus-group-change-level
		       group
		       gnus-level-default-subscribed
		       (or (gnus-group-group-level) gnus-level-killed))
		      (gnus-group-update-group-line)
		      (delete-region (point-at-bol)
				     (progn (forward-line 1) (point))))
		    (gnus-group-insert-group-line-info group)
		    (forward-line -1))
		(gnus-group-make-group grp (list 'nnshimbun server)))))
	(message "No group is found in nnshimbun+%s:" server)))))

(defun nnshimbun-generate-download-script (&optional async)
  "Generate download script for all subscribed schimbuns.
Output will be put in a new buffer.  If called with a prefix,
puts a '&' after each w3m command."
  (interactive "P")
  (switch-to-buffer
   (get-buffer-create "*shimbun download script*"))
  (erase-buffer)
  (insert
   (concat "#!/bin/sh\n# shimbun download script\n\n"
	   "W3M=" (if w3m-command w3m-command "/usr/bin/w3m")
	   "\nOPTS=\"-no-cookie -o accept_encoding=identity -dump_both\"\n\n"))
  (let ((path (file-name-as-directory
	       (expand-file-name shimbun-local-path)))
	url fname)
    ;; get all subscribed shimbun groups
    (dolist (cur gnus-newsrc-alist)
      (when (and (eq (car-safe (nth 4 cur)) 'nnshimbun)
		 (<= (nth 1 cur) gnus-level-subscribed))
	(when (string-match "nnshimbun\\+\\(.+\\):\\(.+\\)" (car cur))
	  (nnshimbun-possibly-change-group (match-string 2 (car cur))
					   (match-string 1 (car cur)))
	  (when (setq url (shimbun-index-url nnshimbun-shimbun))
	    (setq fname (concat path (substring (md5 url) 0 10) "_shimbun"))
	    (insert
	     (concat "$W3M $OPTS " (shell-quote-argument url) " > " fname
		     (if async " &\n" "\n")))))))))

(provide 'nnshimbun)

;;; nnshimbun.el ends here
