;;; elmo-archive.el --- Archive folder of ELMO. -*- coding: euc-japan -*-

;; Copyright (C) 1998,1999,2000 OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;	Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news
;; Created: Sep 13, 1998

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;
;; TODO:
;; Info-Zip 専用エージェントを用いた日本語検索（OS/2 専用）。

;;; Code:
;;
(eval-when-compile (require 'cl))

(require 'elmo)
(require 'elmo-msgdb)

;;; User vars.
(defvar elmo-archive-lha-dos-compatible
  (memq system-type '(OS/2 emx windows-nt))
  "*If non-nil, regard your LHA as compatible to DOS version.")

(defvar elmo-archive-use-izip-agent (memq system-type '(OS/2 emx))
  "*If non-nil, use the special agent in fetching headers.")

(defvar elmo-archive-folder-path "~/Mail"
  "*Base directory for archive folders.")

(defvar elmo-archive-basename "elmo-archive"
  "*Common basename of archive folder file, w/o suffix.")

(defvar elmo-archive-cmdstr-max-length 8000 ; SASAKI Osamu's suggestion
  "*Command line string limitation under OS/2, exactly 8190 bytes.")

(defvar elmo-archive-fetch-headers-volume 50
  "*Quantity of article headers to fetch per once.")

(defvar elmo-archive-dummy-file ".elmo-archive"
  "*Name of dummy file that will be appended when the folder is null.")

(defvar elmo-archive-check-existance-strict t
  "*Check existance of archive contents if non-nil.")

(defvar elmo-archive-load-hook nil
  "*Hook called after loading elmo-archive.el.")

(defvar elmo-archive-treat-file nil
  "*Treat archive folder as a file if non-nil.")

;;; User variables for elmo-archive.
(defvar elmo-archive-default-type 'zip
  "*Default archiver type.  The value must be a symbol.")

(defvar elmo-archive-use-cache nil
  "Use cache in archive folder.")

;;; ELMO Local directory folder
(eval-and-compile
  (luna-define-class elmo-archive-folder (elmo-folder)
		     (archive-name archive-type archive-prefix dir-name))
  (luna-define-internal-accessors 'elmo-archive-folder))

(luna-define-generic elmo-archive-folder-path (folder)
  "Return local directory path of the FOLDER.")

(luna-define-method elmo-archive-folder-path ((folder elmo-archive-folder))
  elmo-archive-folder-path)

(defun elmo-intern-soft (str)
  (if (eq str "") nil (intern-soft str)))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-archive-folder)
					    name)
  (elmo-archive-folder-set-dir-name-internal folder name)
  (when (string-match
	 "^\\([^;]*\\);?\\([^;]*\\);?\\([^;]*\\)$"
	 name)
    ;; Drive letter is OK!
    (or (elmo-archive-folder-set-archive-name-internal
	 folder (match-string 1 name))
	(elmo-archive-folder-set-archive-name-internal
	 folder ""))
    (or (elmo-archive-folder-set-archive-type-internal
	 folder (elmo-intern-soft (match-string 2 name)))
	(elmo-archive-folder-set-archive-type-internal
	 folder elmo-archive-default-type))
    (or (elmo-archive-folder-set-archive-prefix-internal
	 folder (match-string 3 name))
	(elmo-archive-folder-set-archive-prefix-internal
	 folder "")))
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-archive-folder))
  ;; For compatibility
  (expand-file-name
   (elmo-replace-string-as-filename
    (elmo-folder-name-internal folder))
   (expand-file-name (concat (symbol-name (elmo-folder-type-internal folder))
			     "/"
			     (symbol-name
			      (elmo-archive-folder-archive-type-internal
			       folder)))
		     elmo-msgdb-directory)))

;;; MMDF parser -- info-zip agent w/ REXX
(defvar elmo-mmdf-delimiter "^\01\01\01\01$"
  "*Regular expression of MMDF delimiter.")

(defvar elmo-unixmail-delimiter "^From \\([^ \t]+\\) \\(.+\\)"
  "*Regular expression of UNIX Mail delimiter.")

(defvar elmo-archive-header-regexp "^[ \t]*[-=][-=][-=][-=]"
  "*Common regexp of the delimiter in listing archive.") ; marche

(defvar elmo-archive-file-regexp-alist
  (append
   (if elmo-archive-lha-dos-compatible
       '((lha . "^%s\\([0-9]+\\)$"))	; OS/2,DOS w/  "-x"
     '((lha . "^.*[ \t]%s\\([0-9]+\\)$")))
   '((zip . "^.*[ \t]%s\\([0-9]+\\)$")
     (zoo . "^.*[ \t]%s\\([0-9]+\\)$")
     (tar . "^%s\\([0-9]+\\)$")		; ok
     (tgz . "^%s\\([0-9]+\\)$")		; ok
     (rar . "^[ \t]%s\\([0-9]+\\)$"))))

(defvar elmo-archive-suffix-alist
  '((lha . ".lzh")			; default
;;;     (lha . ".lzs")
    (zip . ".zip")
    (zoo . ".zoo")
;;;     (arc . ".arc")
;;;     (arj . ".arj")
    (rar . ".rar")
    (tar . ".tar")
    (tgz . ".tar.gz")))

;;; lha
(defvar elmo-archive-lha-method-alist
  (if elmo-archive-lha-dos-compatible
      ;; OS/2
      '((cp  . ("lha" "u" "-x"))
	(mv  . ("lha" "m" "-x"))
	(rm  . ("lha" "d"))
	(ls  . ("lha" "l" "-x"))
	(cat . ("lha" "p" "-n"))
	(ext . ("lha" "x"))		; "-x"
	)
    ;; some UN|X
    '((cp  . ("lha" "u"))
      (mv  . ("lha" "m"))
      (rm  . ("lha" "d"))
      (ls  . ("lha" "l"))
      (cat . ("lha" "pq"))
      (ext . ("lha" "x")))))

;;; info-zip/unzip
(defvar elmo-archive-zip-method-alist
  '((cp       . ("zip" "-9q"))
    (cp-pipe  . ("zip" "-9q@"))
    (mv       . ("zip" "-mDq9"))
    (mv-pipe  . ("zip" "-mDq9@"))
    (rm       . ("zip" "-dq"))
    (rm-pipe  . ("zip" "-dq@"))
    (ls       . ("unzip" "-lq"))
    (cat      . ("unzip" "-pq"))
    (ext      . ("unzip"))
    (cat-headers . ("izwlagent" "--cat"))))

;;; zoo
(defvar elmo-archive-zoo-method-alist
  '((cp       . ("zoo" "aq"))
    (cp-pipe  . ("zoo" "aqI"))
    (mv       . ("zoo" "aMq"))
    (mv-pipe  . ("zoo" "aMqI"))
    (rm       . ("zoo" "Dq"))
    (ls       . ("zoo" "l"))		; normal
    (cat      . ("zoo" "xpq"))
    (ext      . ("zoo" "xq"))))

;;; rar
(defvar elmo-archive-rar-method-alist
  '((cp       . ("rar" "u" "-m5"))
    (mv       . ("rar" "m" "-m5"))
    (rm       . ("rar" "d"))
    (ls       . ("rar" "v"))
    (cat      . ("rar" "p" "-inul"))
    (ext      . ("rar" "x"))))

;;; GNU tar (*.tar)
(defvar elmo-archive-tar-method-alist
  (if elmo-archive-lha-dos-compatible
      '((ls   . ("gtar" "-tf"))
	(cat  . ("gtar" "--posix Oxf"))
	(ext  . ("gtar" "-xf"))
;;;	(rm   . ("gtar" "--posix" "--delete" "-f")) ; well not work
	)
    '((ls    . ("gtar" "-tf"))
      (cat   . ("gtar" "-Oxf"))
      (ext   . ("gtar" "-xf"))
;;;      (rm    . ("gtar" "--delete" "-f")) ; well not work
      )))

;;; GNU tar (*.tar.gz, *.tar.Z, *.tar.bz2)
(defvar elmo-archive-tgz-method-alist
  '((ls         . ("gtar" "-ztf"))
    (cat        . ("gtar" "-Ozxf"))
    (create     . ("gtar" "-zcf"))
;;;    (rm         . elmo-archive-tgz-rm-func)
    (cp         . elmo-archive-tgz-cp-func)
    (mv         . elmo-archive-tgz-mv-func)
    (ext        . ("gtar" "-zxf"))
    ;; tgz special method
    (decompress . ("gzip" "-d"))
    (compress   . ("gzip"))
    (append     . ("gtar" "-uf"))
;;;    (delete     . ("gtar" "--delete" "-f")) ; well not work
    ))

(defvar elmo-archive-method-list
  '(elmo-archive-lha-method-alist
    elmo-archive-zip-method-alist
    elmo-archive-zoo-method-alist
;;;    elmo-archive-tar-method-alist
    elmo-archive-tgz-method-alist
;;;    elmo-archive-arc-method-alist
;;;    elmo-archive-arj-method-alist
    elmo-archive-rar-method-alist))

;;; Internal vars.
(defvar elmo-archive-method-alist nil)
(defvar elmo-archive-suffixes nil)


;;; Macro
(defmacro elmo-archive-get-method (type action)
  `(cdr (assq ,action (cdr (assq ,type elmo-archive-method-alist)))))

(defmacro elmo-archive-get-suffix (type)
  `(cdr (assq ,type elmo-archive-suffix-alist)))

(defmacro elmo-archive-get-regexp (type)
  `(cdr (assq ,type elmo-archive-file-regexp-alist)))

(defsubst elmo-archive-call-process (prog args &optional output)
  (= (apply 'call-process prog nil output nil args) 0))

(defsubst elmo-archive-call-method (method args &optional output)
  (cond
   ((functionp method)
    (funcall method args output))
   (t
    (elmo-archive-call-process
     (car method) (append (cdr method) args) output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scan Folder

(defsubst elmo-archive-list-folder-subr (folder &optional nonsort)
  "*Returns list of number-file(int, not string) in archive FILE.
TYPE specifies the archiver's symbol."
  (let* ((type (elmo-archive-folder-archive-type-internal folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (file (elmo-archive-get-archive-name folder))
	 (method (elmo-archive-get-method type 'ls))
	 (args (list file))
	 (file-regexp (format (elmo-archive-get-regexp type)
			      (elmo-concat-path (regexp-quote prefix) "")))
	 (killed (elmo-folder-killed-list-internal folder))
	 numbers buf file-list header-end)
    (if (file-exists-p file)
	(with-temp-buffer
	  (unless (elmo-archive-call-method method args t)
	    (error "%s exited abnormally!" method))
	  (goto-char (point-min))
	  (when (re-search-forward elmo-archive-header-regexp nil t)
	    (forward-line)
	    (setq header-end (point))
	    (when (re-search-forward elmo-archive-header-regexp nil t)
	      (beginning-of-line)
	      (narrow-to-region header-end (point))
	      (goto-char (point-min))))
	  (while (and (re-search-forward file-regexp nil t)
		      (not (eobp)))  ; for GNU tar 981010
	    (setq file-list (nconc file-list (list (string-to-number
						    (match-string 1)))))))
      (error "%s does not exist" file))
    (if nonsort
	(cons (elmo-max-of-list file-list)
	      (if killed
		  (- (length file-list)
		     (elmo-msgdb-killed-list-length killed))
		(length file-list)))
      (setq numbers (sort file-list '<))
      (elmo-living-messages numbers killed))))

(luna-define-method elmo-folder-list-messages-internal ((folder
							 elmo-archive-folder)
							&optional nohide)
  (elmo-archive-list-folder-subr folder))

(luna-define-method elmo-folder-status ((folder elmo-archive-folder))
  (elmo-archive-list-folder-subr folder t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Folder related functions

(defsubst elmo-archive-get-archive-directory (folder)
  ;; allow fullpath. return format is "/foo/bar/".
  (if (file-name-absolute-p (elmo-archive-folder-archive-name-internal folder))
      (if (find-file-name-handler
	   (elmo-archive-folder-archive-name-internal folder)
	   'copy-file)
	  (elmo-archive-folder-archive-name-internal folder)
	(expand-file-name (elmo-archive-folder-archive-name-internal folder)))
    (expand-file-name (elmo-archive-folder-archive-name-internal folder)
		      elmo-archive-folder-path)))

(defun elmo-archive-get-archive-name (folder)
  (let ((dir (elmo-archive-get-archive-directory folder))
	(suffix (elmo-archive-get-suffix
		 (elmo-archive-folder-archive-type-internal
		  folder)))
	filename dbdir)
    (unless suffix
      (error "Unknown archiver type: %s"
	     (elmo-archive-folder-archive-type-internal folder)))
    (if elmo-archive-treat-file
	(if (string-match (concat (regexp-quote suffix) "$")
			  (elmo-archive-folder-archive-name-internal folder))
	    (expand-file-name (elmo-archive-folder-archive-name-internal
			       folder)
			      elmo-archive-folder-path)
	  (expand-file-name (concat (elmo-archive-folder-archive-name-internal
				     folder)
				    suffix)
			    elmo-archive-folder-path))
      (if (string-match
	   "^\\(ange-ftp\\|efs\\)-"
	   (symbol-name (find-file-name-handler dir 'copy-file)))
	  ;; ange-ftp, efs
	  (progn
	    (setq filename (expand-file-name
			    (concat elmo-archive-basename suffix)
			    (setq dbdir
				  (elmo-folder-msgdb-path folder))))
	    (if (file-directory-p dbdir)
		(); ok.
	      (if (file-exists-p dbdir)
		  (error "File %s already exists" dbdir)
		(elmo-make-directory dbdir)))
	    (if (not (file-exists-p filename))
		(copy-file
		 (if (file-directory-p dir)
		     (expand-file-name
		      (concat elmo-archive-basename suffix)
		      dir)
		   dir)
		 filename))
	    filename)
	(if (or (not (file-exists-p dir))
		(file-directory-p dir))
	    (expand-file-name
	     (concat elmo-archive-basename suffix)
	     dir)
	  dir)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-archive-folder))
  (file-exists-p (elmo-archive-get-archive-name folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-archive-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-archive-folder))
  t)

(luna-define-method elmo-folder-create ((folder elmo-archive-folder))
  (let* ((dir (directory-file-name	; remove tail slash.
	       (elmo-archive-get-archive-directory folder)))
	 (type (elmo-archive-folder-archive-type-internal folder))
	 (arc (elmo-archive-get-archive-name folder)))
    (if elmo-archive-treat-file
	(setq dir (directory-file-name (file-name-directory dir))))
    (cond ((and (file-exists-p dir)
		(not (file-directory-p dir)))
	   ;; file exists
	   (error "Create folder failed; File \"%s\" exists" dir))
	  ((file-directory-p dir)
	   (if (file-exists-p arc)
	       t			; return value
	     (elmo-archive-create-file arc type folder)))
	  (t
	   (elmo-make-directory dir)
	   (elmo-archive-create-file arc type folder)
	   t))))

(defun elmo-archive-create-file (archive type folder)
  (save-excursion
    (let* ((tmp-dir (directory-file-name
		     (elmo-folder-msgdb-path folder)))
	   (dummy elmo-archive-dummy-file)
	   (method (or (elmo-archive-get-method type 'create)
		       (elmo-archive-get-method type 'mv)))
	   (args (list archive dummy)))
      (when (null method)
	(ding)
	(error "WARNING: read-only mode: %s (method undefined)" type))
      (cond
       ((file-directory-p tmp-dir)
	())				; nop
       ((file-exists-p tmp-dir)
	;; file exists
	(error "Create directory failed; File \"%s\" exists" tmp-dir))
       (t
	(elmo-make-directory tmp-dir)))
      (elmo-bind-directory tmp-dir
	(write-region (point) (point) dummy nil 'no-msg)
	(prog1
	    (elmo-archive-call-method method args)
	  (if (file-exists-p dummy)
	      (delete-file dummy)))
	))))

(luna-define-method elmo-folder-delete ((folder elmo-archive-folder))
  (let ((msgs (and (elmo-folder-exists-p folder)
		   (elmo-folder-list-messages folder))))
    (when (yes-or-no-p (format "%sDelete msgdb and substance of \"%s\"? "
			       (if (> (length msgs) 0)
				   (format "%d msg(s) exists. " (length msgs))
				 "")
			       (elmo-folder-name-internal folder)))
      (let ((arc (elmo-archive-get-archive-name folder)))
	(if (not (file-exists-p arc))
	    (error "No such file: %s" arc)
	  (delete-file arc))
	(elmo-msgdb-delete-path folder)
	t))))

(luna-define-method elmo-folder-rename-internal ((folder elmo-archive-folder)
						 new-folder)
  (let* ((old-arc (elmo-archive-get-archive-name folder))
	 (new-arc (elmo-archive-get-archive-name new-folder))
	 (new-dir (directory-file-name
		   (elmo-archive-get-archive-directory new-folder))))
    (if elmo-archive-treat-file
	(setq new-dir (directory-file-name (file-name-directory new-dir))))
    (unless (and (eq (elmo-archive-folder-archive-type-internal folder)
		     (elmo-archive-folder-archive-type-internal new-folder))
		 (equal (elmo-archive-folder-archive-prefix-internal
			 folder)
			(elmo-archive-folder-archive-prefix-internal
			 new-folder)))
      (error "Not same archive type and prefix"))
    (unless (file-exists-p old-arc)
      (error "No such file: %s" old-arc))
    (when (file-exists-p new-arc)
      (error "Already exists: %s" new-arc))
    (unless (file-directory-p new-dir)
      (elmo-make-directory new-dir))
    (rename-file old-arc new-arc)
    t))

(defun elmo-archive-folder-list-subfolders (folder one-level)
  (if elmo-archive-treat-file
      (let* ((path (elmo-archive-get-archive-directory folder))
	     (base-folder (or (elmo-archive-folder-archive-name-internal
			       folder)
			      ""))
	     (suffix (elmo-archive-folder-archive-type-internal folder))
	     (prefix (if (string=
			  (elmo-archive-folder-archive-prefix-internal folder)
			  "")
			 ""
		       (concat ";"
			       (elmo-archive-folder-archive-prefix-internal
				folder))))
	     (dir (if (file-directory-p path)
		      path (file-name-directory path)))
	     (name (if (file-directory-p path)
		       "" (file-name-nondirectory path)))
	     (flist (and (file-directory-p dir)
			 (directory-files dir nil
					  (if (> (length name) 0)
					      (concat "^" name "[^A-z][^A-z]")
					    name)
					  nil)))
	     (regexp (format "^\\(.*\\)\\(%s\\)$"
			     (mapconcat
			      (lambda (x) (regexp-quote (cdr x)))
			      elmo-archive-suffix-alist
			      "\\|"))))
	(if (string-match "\\(.*\\)/$" base-folder) ; ends with '/'.
	    (setq base-folder (match-string 1 base-folder))
	  (unless (file-directory-p path)
	    (setq base-folder (or (file-name-directory base-folder) ""))))
	(delq
	 nil
	 (mapcar
	  (lambda (x)
	    (when (and (string-match regexp x)
		       (eq suffix
			   (car
			    (rassoc (match-string 2 x)
				    elmo-archive-suffix-alist))))
	      (format "%s%s;%s%s"
		      (elmo-folder-prefix-internal folder)
		      (elmo-concat-path base-folder (match-string 1 x))
		      suffix prefix)))
	  flist)))
    (elmo-mapcar-list-of-list
     (lambda (x)
       (if (file-exists-p
	    (expand-file-name
	     (concat elmo-archive-basename
		     (elmo-archive-get-suffix
		      (elmo-archive-folder-archive-type-internal
		       folder)))
	     (expand-file-name
	      x
	      (elmo-archive-folder-path folder))))
	   (concat (elmo-folder-prefix-internal folder) x)))
     (elmo-list-subdirectories
      (elmo-archive-folder-path folder)
      (or (elmo-archive-folder-dir-name-internal folder) "")
      one-level))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-archive-folder)
						 &optional one-level)
  (elmo-archive-folder-list-subfolders folder one-level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Article file related functions
;;; read(extract) / append(move) / delete(delete) / query(list)

(defsubst elmo-archive-message-fetch-internal (folder number)
  (let* ((type (elmo-archive-folder-archive-type-internal folder))
	 (arc (elmo-archive-get-archive-name folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (method (elmo-archive-get-method type 'cat))
	 (args (list arc (elmo-concat-path
			  prefix (number-to-string number)))))
    (and (file-exists-p arc)
	 (as-binary-process
	  (elmo-archive-call-method method args t))
	 (progn
	   (elmo-delete-cr-buffer)
	   t))))

(luna-define-method elmo-message-fetch-internal ((folder elmo-archive-folder)
						 number strategy
						 &optional section unseen)
  (elmo-archive-message-fetch-internal folder number))

(luna-define-method elmo-folder-append-buffer ((folder elmo-archive-folder)
					       &optional flags number
					       return-number)
  (elmo-archive-folder-append-buffer folder flags number return-number))

;; verrrrrry slow!!
(defun elmo-archive-folder-append-buffer (folder flags number return-number)
  (let* ((type (elmo-archive-folder-archive-type-internal folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (arc (elmo-archive-get-archive-name folder))
	 (method (elmo-archive-get-method type 'mv))
	 (next-num (or number
		       (1+ (if (file-exists-p arc)
			       (car
				(elmo-folder-status folder)) 0))))
	 (tmp-dir (elmo-folder-msgdb-path folder))
	 (src-buffer (current-buffer))
	 dst-buffer
	 newfile)
    (when (null method)
      (ding)
      (error "WARNING: read-only mode: %s (method undefined)" type))
    (with-temp-buffer
      (let ((tmp-dir (expand-file-name prefix tmp-dir)))
	(when (not (file-directory-p tmp-dir))
	  (elmo-make-directory (directory-file-name tmp-dir))))
      (setq newfile (elmo-concat-path
		     prefix
		     (number-to-string next-num)))
      (elmo-bind-directory tmp-dir
	(if (and (or (functionp method) (car method))
		 (file-writable-p newfile))
	    (progn
	      (setq dst-buffer (current-buffer))
	      (with-current-buffer src-buffer
		(copy-to-buffer dst-buffer (point-min) (point-max)))
	      (as-binary-output-file
	       (write-region (point-min) (point-max) newfile nil 'no-msg))
	      (when (elmo-archive-call-method method (list arc newfile))
		(elmo-folder-preserve-flags
		 folder
		 (with-current-buffer src-buffer
		   (elmo-msgdb-get-message-id-from-buffer))
		 flags)
		(if return-number
		    next-num
		  t)))
	  nil)))))

(defun elmo-folder-append-messages-*-archive (folder
					      src-folder
					      numbers
					      same-number)
  (let ((prefix (elmo-archive-folder-archive-prefix-internal folder)))
    (cond
     ((and same-number
	   (null prefix)
	   (elmo-folder-message-file-p src-folder)
	   (elmo-folder-message-file-number-p src-folder))
      ;; same-number(localdir, localnews) -> archive
      (unless (elmo-archive-append-files
	       folder
	       (elmo-folder-message-file-directory src-folder)
	       numbers)
	(setq numbers nil))
      (elmo-progress-notify 'elmo-folder-move-messages (length numbers))
      numbers)
     ((elmo-folder-message-make-temp-file-p src-folder)
      ;; not-same-number (localdir, localnews), (archive maildir) -> archive
      (let ((temp-dir (elmo-folder-message-make-temp-files
		       src-folder
		       numbers
		       (unless same-number
			 (1+ (if (file-exists-p (elmo-archive-get-archive-name
						 folder))
				 (car (elmo-folder-status folder)) 0)))))
	    new-dir base-dir files)
	(unwind-protect
	    (progn
	      (setq base-dir temp-dir)
	      (when (> (length prefix) 0)
		(when (file-name-directory prefix)
		  (elmo-make-directory (file-name-directory prefix)))
		(rename-file
		 temp-dir
		 (setq new-dir
		       (expand-file-name
			prefix
			;; parent of temp-dir..(works in windows?)
			(expand-file-name ".." temp-dir))))
		;; now temp-dir has name prefix.
		(setq temp-dir new-dir)
		;; parent of prefix becomes base-dir.
		(setq base-dir (expand-file-name ".." temp-dir)))
	      (setq files
		    (mapcar
		     (lambda (x) (elmo-concat-path prefix x))
		     (directory-files temp-dir nil "^[^\\.]")))
	      (unless (elmo-archive-append-files folder
						 base-dir
						 files)
		(setq numbers nil)))
	  (elmo-delete-directory temp-dir)))
      (elmo-progress-notify 'elmo-folder-move-messages (length numbers))
      numbers)
     (t
      (elmo-folder-append-messages folder src-folder numbers same-number
				   'elmo-folder-append-messages-*-archive)))))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-archive-folder))
  (let ((type (elmo-archive-folder-archive-type-internal folder)))
    (or (elmo-archive-get-method type 'ext-pipe)
	(elmo-archive-get-method type 'ext))))

(luna-define-method elmo-folder-message-make-temp-files
  ((folder elmo-archive-folder) numbers
   &optional start-number)
  (elmo-archive-folder-message-make-temp-files folder numbers start-number))

(defun elmo-archive-folder-message-make-temp-files (folder
						    numbers
						    start-number)
  (let* ((tmp-dir-src (elmo-folder-make-temporary-directory folder))
	 (tmp-dir-dst (elmo-folder-make-temporary-directory folder))
	 (arc     (elmo-archive-get-archive-name folder))
	 (type    (elmo-archive-folder-archive-type-internal folder))
	 (prefix  (elmo-archive-folder-archive-prefix-internal folder))
	 (p-method (elmo-archive-get-method type 'ext-pipe))
	 (n-method (elmo-archive-get-method type 'ext))
	 (tmp-msgs (mapcar (lambda (x) (elmo-concat-path
					prefix
					(number-to-string x))) numbers))
	 number)
    ;; Expand files in the tmp-dir-src.
    (elmo-bind-directory tmp-dir-src
      (cond
       ((functionp n-method)
	(funcall n-method (cons arc tmp-msgs)))
       (p-method
	(let ((p-prog (car p-method))
	      (p-prog-arg (cdr p-method)))
	  (elmo-archive-exec-msgs-subr1
	   p-prog (append p-prog-arg (list arc)) tmp-msgs)))
       (t
	(let ((n-prog (car n-method))
	      (n-prog-arg (cdr n-method)))
	  (elmo-archive-exec-msgs-subr2
	   n-prog (append n-prog-arg (list arc)) tmp-msgs
	   (length arc))))))
    ;; Move files to the tmp-dir-dst.
    (setq number start-number)
    (dolist (tmp-file tmp-msgs)
      (rename-file (expand-file-name
		    tmp-file
		    tmp-dir-src)
		   (expand-file-name
		    (if start-number
			(number-to-string number)
		      (file-name-nondirectory tmp-file))
		    tmp-dir-dst))
      (if start-number (incf number)))
    ;; Remove tmp-dir-src.
    (elmo-delete-directory tmp-dir-src)
    ;; tmp-dir-dst is the return directory.
    tmp-dir-dst))

(defun elmo-archive-append-files (folder dir &optional files)
  (let* ((dst-type (elmo-archive-folder-archive-type-internal folder))
	 (arc (elmo-archive-get-archive-name folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (p-method (elmo-archive-get-method dst-type 'cp-pipe))
	 (n-method (elmo-archive-get-method dst-type 'cp))
	 src tmp newfile)
    (unless (elmo-folder-exists-p folder) (elmo-folder-create folder))
    (unless files (setq files (directory-files dir nil "^[^\\.]")))
    (when (null (or p-method n-method))
      (ding)
      (error "WARNING: read-only mode: %s (method undefined)" dst-type))
    (save-excursion
      (elmo-bind-directory dir
	(cond
	 ((functionp n-method)
	  (funcall n-method (cons arc files)))
	 (p-method
	  (let ((p-prog (car p-method))
		(p-prog-arg (cdr p-method)))
	    (elmo-archive-exec-msgs-subr1
	     p-prog (append p-prog-arg (list arc)) files)))
	 (t
	  (let ((n-prog (car n-method))
		(n-prog-arg (cdr n-method)))
	    (elmo-archive-exec-msgs-subr2
	     n-prog (append n-prog-arg (list arc)) files (length arc)))))))))

(luna-define-method elmo-folder-delete-messages-internal ((folder
							   elmo-archive-folder)
							  numbers)
  (let* ((type (elmo-archive-folder-archive-type-internal folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (arc (elmo-archive-get-archive-name folder))
	 (p-method (elmo-archive-get-method type 'rm-pipe))
	 (n-method (elmo-archive-get-method type 'rm))
	 (numbers (mapcar (lambda (x) (elmo-concat-path
				       prefix
				       (number-to-string x)))
			  numbers)))
    (cond ((functionp n-method)
	   (funcall n-method (cons arc numbers)))
	  (p-method
	   (let ((p-prog (car p-method))
		 (p-prog-arg (cdr p-method)))
	     (elmo-archive-exec-msgs-subr1
	      p-prog (append p-prog-arg (list arc)) numbers)))
	  (n-method
	   (let ((n-prog (car n-method))
		 (n-prog-arg (cdr n-method)))
	     (elmo-archive-exec-msgs-subr2
	      n-prog (append n-prog-arg (list arc)) numbers (length arc))))
	  (t
	   (ding)
	   (error "WARNING: not delete: %s (method undefined)" type)))))

(defun elmo-archive-exec-msgs-subr1 (prog args msgs)
  (with-temp-buffer
    (insert (mapconcat 'concat msgs "\n")) ;string
    (= 0 (apply 'call-process-region (point-min) (point-max)
		prog nil nil nil args))))

(defun elmo-archive-exec-msgs-subr2 (prog args msgs arc-length)
  (let ((max-len (- elmo-archive-cmdstr-max-length arc-length))
	(n (length msgs))
	rest i sum)
    (setq rest msgs) ;string
    (setq i 1)
    (setq sum 0)
    (catch 'done
      (while (and rest (<= i n))
	(mapc
	 (lambda (x)
	   (let* ((len (length x))
		  (files (member x (reverse rest))))
	     ;; total(previous) + current + white space
	     (if (<= max-len (+ sum len 1))
		 (progn
		   (unless
		       (elmo-archive-call-process
			prog (append args files))
		     (throw 'done nil))
		   (setq sum 0) ;; reset
		   (setq rest (nthcdr i rest)))
	       (setq sum (+ sum len 1)))
	     (setq i (1+ i))))
	 msgs))
      (throw 'done
	     (or (not rest)
		 (elmo-archive-call-process prog (append args rest))))
      )))

(defsubst elmo-archive-article-exists-p (arc msg type)
  (if (not elmo-archive-check-existance-strict)
      t ; nop
    (save-excursion ; added 980915
      (let* ((method (elmo-archive-get-method type 'ls))
	     (args (list arc msg))
	     (buf (get-buffer-create " *ELMO ARCHIVE query*"))
	     (error-msg "\\(no file\\|0 files\\)")
	     ret-val)
	(set-buffer buf)
	(erase-buffer)
	(elmo-archive-call-method method args t)
	;; pointer: point-max
	(setq ret-val (not (re-search-backward error-msg nil t)))
	(kill-buffer buf)
	ret-val))))

(defun elmo-archive-tgz-common-func (args exec-type &optional copy)
  (let* ((arc (car args))
	 (tmp-msgs (cdr args))
	 (decompress (elmo-archive-get-method 'tgz 'decompress))
	 (compress (elmo-archive-get-method 'tgz 'compress))
	 (exec (elmo-archive-get-method 'tgz exec-type))
	 (suffix (elmo-archive-get-suffix 'tgz))
	 (tar-suffix (elmo-archive-get-suffix 'tar))
	 arc-tar ret-val
	 )
    (when (null (and decompress compress exec))
      (ding)
      (error "WARNING: special method undefined: %s of %s"
	     (or (if (null decompress) 'decompress)
		 (if (null compress) 'compress)
		 (if (null exec) exec-type))
	     'tgz))
    (unless tar-suffix
      (ding)
      (error "WARNING: `tar' suffix undefined"))
    (if (string-match (concat (regexp-quote suffix) "$") arc)
	(setq arc-tar
	      (concat (substring arc 0 (match-beginning 0)) tar-suffix))
      (error "%s: not match suffix [%s]" arc suffix))
    (and
     ;; decompress
     (elmo-archive-call-process
      (car decompress) (append (cdr decompress) (list arc)))
     ;; append (or delete)
     (elmo-archive-exec-msgs-subr2
      (car exec) (append (cdr exec) (list arc-tar)) tmp-msgs (length arc-tar))
     ;; compress
     (setq ret-val
	   (elmo-archive-call-process
	    (car compress) (append (cdr compress) (list arc-tar)))))
    ;; delete temporary messages
    (if (and (not copy)
	     (eq exec-type 'append))
	(while tmp-msgs
	  (if (file-exists-p (car tmp-msgs))
	      (delete-file (car tmp-msgs)))
	  (setq tmp-msgs (cdr tmp-msgs))))
    ret-val))

(defun elmo-archive-tgz-cp-func (args &optional output)
  (elmo-archive-tgz-common-func args 'append t))

(defun elmo-archive-tgz-mv-func (args &optional output)
  (elmo-archive-tgz-common-func args 'append))

(defun elmo-archive-tgz-rm-func (args &optional output)
  (elmo-archive-tgz-common-func args 'delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MessageDB functions (from elmo-localdir.el)

(defsubst elmo-archive-msgdb-create-entity-subr (msgdb number)
  (let (header-end)
    (set-buffer-multibyte default-enable-multibyte-characters)
    (goto-char (point-min))
    (if (re-search-forward "\\(^--.*$\\)\\|\\(\n\n\\)" nil t)
	(setq header-end (point))
      (setq header-end (point-max)))
    (narrow-to-region (point-min) header-end)
    (elmo-msgdb-create-message-entity-from-header
     (elmo-msgdb-message-entity-handler msgdb) number)))

;; verrrry slow!!
(defsubst elmo-archive-msgdb-create-entity (msgdb
					    method
					    archive number type
					    &optional prefix)
  (let* ((msg (elmo-concat-path prefix (number-to-string number)))
	 (arg-list (list archive msg)))
    (when (elmo-archive-article-exists-p archive msg type)
      ;; insert article.
      (as-binary-process
       (elmo-archive-call-method method arg-list t))
      (elmo-archive-msgdb-create-entity-subr msgdb number))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-archive-folder)
					      numbers flag-table)
  (when numbers
    (save-excursion ;; 981005
      (elmo-with-progress-display (elmo-folder-create-msgdb (length numbers))
	  "Creating msgdb"
	(if (and elmo-archive-use-izip-agent
		 (elmo-archive-get-method
		  (elmo-archive-folder-archive-type-internal folder)
		  'cat-headers))
	    (elmo-archive-msgdb-create-as-numlist-subr2
	     folder numbers flag-table)
	  (elmo-archive-msgdb-create-as-numlist-subr1
	   folder numbers flag-table))))))

(defun elmo-archive-msgdb-create-as-numlist-subr1 (folder numlist flag-table)
  (let* ((type (elmo-archive-folder-archive-type-internal folder))
	 (file (elmo-archive-get-archive-name folder))
	 (method (elmo-archive-get-method type 'cat))
	 (new-msgdb (elmo-make-msgdb))
	 entity message-id flags)
    (with-temp-buffer
      (while numlist
	(erase-buffer)
	(setq entity
	      (elmo-archive-msgdb-create-entity
	       new-msgdb
	       method file (car numlist) type
	       (elmo-archive-folder-archive-prefix-internal folder)))
	(when entity
	  (setq message-id (elmo-message-entity-field entity 'message-id)
		flags (elmo-flag-table-get flag-table message-id))
	  (elmo-global-flags-set flags folder (car numlist) message-id)
	  (elmo-msgdb-append-entity new-msgdb entity flags))
	(elmo-progress-notify 'elmo-folder-msgdb-create)
	(setq numlist (cdr numlist)))
      new-msgdb)))

;;; info-zip agent
(defun elmo-archive-msgdb-create-as-numlist-subr2 (folder
						   numlist
						   flag-table)
  (let* ((delim1 elmo-mmdf-delimiter)		;; MMDF
	 (delim2 elmo-unixmail-delimiter)	;; UNIX Mail
	 (type (elmo-archive-folder-archive-type-internal folder))
	 (prefix (elmo-archive-folder-archive-prefix-internal folder))
	 (method (elmo-archive-get-method type 'cat-headers))
	 (prog (car method))
	 (args (cdr method))
	 (arc (elmo-archive-get-archive-name folder))
	 (new-msgdb (elmo-make-msgdb))
	 n msgs case-fold-search)
    (with-temp-buffer
      (while numlist
	(setq n (min (1- elmo-archive-fetch-headers-volume)
		     (1- (length numlist))))
	(setq msgs (reverse (memq (nth n numlist) (reverse numlist))))
	(setq numlist (nthcdr (1+ n) numlist))
	(erase-buffer)
	(insert
	 (mapconcat
	  'concat
	  (mapcar (lambda (x) (elmo-concat-path prefix (number-to-string x)))
		  msgs)
	  "\n"))
	(as-binary-process (apply 'call-process-region
				  (point-min) (point-max)
				  prog t t nil (append args (list arc))))
	(goto-char (point-min))
	(cond
	 ((looking-at delim1)	;; MMDF
	  (elmo-msgdb-append
	   new-msgdb
	   (elmo-archive-parse-mmdf folder msgs flag-table)))
;;; 	 ((looking-at delim2)		; UNIX MAIL
;;; 	  (elmo-msgdb-append
;;; 	   new-msgdb
;;; 	   (elmo-archive-parse-unixmail msgs flag-table)))
	 (t			;; unknown format
	  (error "Unknown format!")))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(defun elmo-archive-parse-mmdf (folder msgs flag-table)
  (let ((delim elmo-mmdf-delimiter)
	(new-msgdb (elmo-make-msgdb))
	number sp ep rest entity
	message-id flags)
    (goto-char (point-min))
    (setq rest msgs)
    (while (and rest (re-search-forward delim nil t)
		(not (eobp)))
      (setq number (car rest))
      (setq sp (1+ (point)))
      (setq ep (prog2 (re-search-forward delim)
		   (1+ (- (point) (length delim)))))
      (if (>= sp ep)			; no article!
	  ()				; nop
	(save-excursion
	  (narrow-to-region sp ep)
	  (setq entity (elmo-archive-msgdb-create-entity-subr new-msgdb number)
		message-id (elmo-message-entity-field entity 'message-id)
		flags (elmo-flag-table-get flag-table message-id))
	  (elmo-global-flags-set flags folder number message-id)
	  (elmo-msgdb-append-entity new-msgdb entity flags)
	  (widen)))
      (forward-line)
      (setq rest (cdr rest)))
    new-msgdb))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search functions

(defsubst elmo-archive-field-condition-match (folder number number-list
						     condition prefix)
  (save-excursion
    (let* ((type (elmo-archive-folder-archive-type-internal folder))
	   (arc (elmo-archive-get-archive-name folder))
	   (method (elmo-archive-get-method type 'cat))
	   (args (list arc (elmo-concat-path prefix (number-to-string number)))))
      (elmo-set-work-buf
	(when (file-exists-p arc)
	  (as-binary-process
	   (elmo-archive-call-method method args t))
	  (set-buffer-multibyte default-enable-multibyte-characters)
	  (decode-mime-charset-region (point-min)(point-max) elmo-mime-charset)
	  (elmo-message-buffer-match-condition condition number))))))

(luna-define-method elmo-folder-search ((folder elmo-archive-folder)
					condition &optional from-msgs)
  (let ((case-fold-search nil)
;;;	 (args (elmo-string-to-list key))
;;; XXX: I don't know whether `elmo-archive-list-folder' updates match-data.
;;;	 (msgs (or from-msgs (elmo-archive-list-folder spec)))
	(msgs (cond ((null from-msgs)
		     (elmo-folder-list-messages folder))
		    ((listp from-msgs)
		     from-msgs)
		    (t
		     (elmo-folder-list-messages folder 'visible 'in-msgdb))))
	ret-val)
    (elmo-with-progress-display (elmo-folder-search (length msgs)) "Searching"
      (dolist (number msgs)
	(when (elmo-archive-field-condition-match
	       folder number msgs
	       condition
	       (elmo-archive-folder-archive-prefix-internal folder))
	  (setq ret-val (cons number ret-val)))
	(elmo-progress-notify 'elmo-folder-search)))
    (nreverse ret-val)))

;;; method(alist)
(if (null elmo-archive-method-alist)
    (let ((mlist elmo-archive-method-list) ; from mew-highlight.el
	  method type str)
      (while mlist
	(setq method (car mlist))
	(setq mlist (cdr mlist))
	(setq str (symbol-name method))
	(string-match "elmo-archive-\\([^-].*\\)-method-alist$" str)
	(setq type (intern-soft (match-string 1 str)))
	(setq elmo-archive-method-alist
	      (cons (cons type
			  (symbol-value method))
		    elmo-archive-method-alist)))))

;;; valid suffix(list)
(if (null elmo-archive-suffixes)
    (let ((slist elmo-archive-suffix-alist)
	  tmp)
      (while slist
	(setq tmp (car slist))
	(setq elmo-archive-suffixes
	      (nconc elmo-archive-suffixes (list (cdr tmp))))
	(setq slist (cdr slist)))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-archive-folder)
					      number)
  elmo-archive-use-cache)

;;; End
(run-hooks 'elmo-archive-load-hook)

(require 'product)
(product-provide (provide 'elmo-archive) (require 'elmo-version))

;;; elmo-archive.el ends here
