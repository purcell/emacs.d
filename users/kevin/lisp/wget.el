;;; wget.el --- Interface program of wget on Emacs

;; Copyright (C) 2001-2004, 2015 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; URL: https://github.com/ataka/emacs-wget
;; Keywords: hypermedia, WWW


;; This file is the main part of emacs-wget.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; emacs-wget is an interface program of GNU wget on Emacs.  For more
;; details about emacs-wget, see:
;;
;;    https://github.com/ataka/emacs-wget


;;; How to install:

;; See README.

;;; Usage:

;; See USAGE.


;;; Code:
(require 'wget-sysdep)
(require 'thingatpt)

(defgroup wget nil
  "wget program interface."
  :group 'hypermedia
  :prefix "wget-")


;;;; User Options

(defcustom wget-command "wget"
  "*Program name of `wget'."
  :group 'wget
  :type 'string)

(defvar wget-basic-options
  '("-v"				;--verbose: Be verbose.
    "--progress=dot") 			;--progress=dot: progress bar (wget => 1.8).
  "*List of default emacs/wget system options.")

(defcustom wget-default-options nil
  ;; '("-nc" "-c")                      ; <- example.
  "*List of default options of wget."
  :group 'wget
  :type '(repeat
	  (restricted-sexp :tag "Option")))

(defcustom wget-web-page-options
  `(,@wget-default-options
    "-r"				;--recursive: Turn on recursive retrieving.
    "-np")				;--no-parent: Do not ever ascend to the parent directory.
  "*List of options to download all Web pages."
  :group 'wget
  :type '(repeat
	  (restricted-sexp :tag "Web page Option")))

(defcustom wget-ftp-default-options nil
  "*List of default options when download from ftp.
If nil, use `wget-default-options' instead."
  :group 'wget
  :type '(repeat
	  (restricted-sexp :tag "FTP Option")))

;; Download dir

(defcustom wget-download-directory
  "~/download"
  "*Default directory name that retrieved files go.
If nil, always ask download directory."
  :group 'wget
  :type '(choice directory (sexp :tag "Lisp object")))

(defcustom wget-download-directory-filter nil
  "*Function that defines the filtering of download directory."
  :group 'wget
  :type '(radio (const :tag "No filter" nil)
		(const :tag "Filter by regexp"
		       wget-download-dir-filter-regexp)
		(const :tag "Aliases"
		       wget-download-dir-filter-alias)
		(const :tag "Filter by regexp and alias"
		       wget-download-dir-filter-regexp-and-alias)
		(const :tag "Check current dir"
		       wget-download-dir-filter-current-dir)
		(function :tag "Other Function")))

;; Download Log

(defcustom wget-download-log-file nil
  "*Default file name for download log.
If emacs-wget find this file in the download directory,
append download log in format `wget-download-log-format'.

If log file is not found, create or ask creating.
See the variable `wget-create-download-log'."
  :group 'wget
  :type 'file)

(defcustom wget-create-download-log 'always
  "*If non-nil, create or ask creating the log file
when the log file is not found in the download directory.

'always  Always create log file
'ask     Ask creating log file
nil      Do not create log file.

The log file name is defined by variable `wget-download-log-file'."
  :group 'wget
  :type '(choice (const always) (const ask) (const nil)))

(defcustom wget-download-log-format
  "%T\t%U\n"
  "*Format string for `wget-download-log-file'.

%T	Time format string replaced by `wget-download-log-time-format'
%t	Title
%U	URI"
  :group 'wget
  :type 'string)

(defcustom wget-download-log-time-format
  "%Y-%m-%d %H:%M:%S"
  "*Time format string for `wget-download-log-file'.
See the function `format-time-string' for format-string."
  :group 'wget
  :type 'string)

(defcustom wget-add-download-log-eof t
  "*If non-nil, download log is added at the end of file,
else it is added at the beginning of file.
Download log is added to the file `wget-download-log-file'
in the format `wget-download-log-format'."
  :group 'wget
  :type 'boolean)

;; misc

(defcustom wget-executable-file-extension-list nil
  ; '("exe" "sh" "csh" "pl" "rb") 	; <- example
  "*List of file extension that change file permission executable after downloading."
  :group 'wget
  :type '(repeat
	  (restricted-sexp :tag "File extension")))

(defcustom wget-truncate-partial-width-windows t
  "*Non-nil means truncate lines in *wget* buffer less than full frame wide."
  :group 'wget
  :type 'boolean)

(defcustom wget-max-window-height (/ (frame-height) 2)
  "*Max height of *wget* buffer."
  :group 'wget
  :type 'integer)

;; Hooks

(defcustom wget-hook nil
  "*Hook run after calling `wget-uri'."
  :group 'wget
  :type 'hook)

(defcustom wget-after-hook nil
  "*Hook run after finishing downloading file."
  :group 'wget
  :type 'hook)

(defcustom wget-load-hook nil
  "*Hook run after loading Emacs-wget."
  :group 'wget
  :type 'hook)

;; debug etc...

(defcustom wget-debug nil
  "*Non nil means save wget log message in buffer `wget-debug-buffer'."
  :group 'wget
  :type 'boolean)

(defvar wget-process-buffer "*wget*"
  "*Name of wget process buffer.
If nil, do not show the wget buffer.")


;;; System Variables

(defvar wget-debug-buffer " *wget-log*"
  "Name of wget log buffer.")

(defconst wget-long-option-alist
  (eval-when-compile
    (let ((options
	   '(;; Download Options
	     "bind-address=ADDRESS" "tries=NUMBER" "no-clobber" "continue"
	     "timestamping" "timeout=SECONDS" "limit-rate=AMOUNT" "wait=SECONDS"
	     "waitretry=SECONDS" "random-wait" "proxy=on/off" "quota=QUOTA"
	     ; (NEW in 1.9)
	     "dns-timeout=SECONDS" "connect-timeout=SECONDS" "read-timeout=SECONDS"
	     "dns-cache=off" "restrict-file-names=MODE"
	     ;; Directory Options
	     "no-directories" "force-directories" "no-host-directories"
	     "cut-dirs=NUMBER" "directory-prefix=PREFIX"
	     ;; HTTP Options
	     "html-extension" "http-user=USER" "http-passwd=PASSWORD"
	     "cache=on/off" "cookies=on/off" "load-cookies FILE" "save-cookies FILE"
	     "ignore-length" "header=ADDITIONAL-HEADER" "proxy-user=USER"
	     "proxy-passwd=PASSWORD" "referer=URL" "save-headers" "user-agent=AGENT-STRING"
	     ; (NEW in 1.9)
	     "post-data=STRING" "post-file=FILE"
	     ;; FTP Options
	     "dont-remove-listing" "glob=on/off" "passive-ftp" "retr-symlinks"
	     ;; Recursive Retrieval Options
	     "recursive" "level=DEPTH" "delete-after" "convert-links"
	     "backup-converted" "mirror" "page-requisites"
	     ; (New in 1.9)
	     "strict-comments"
	     ;; Recursive Accept/Reject Options
	     "accept ACCLIST" "reject REJLIST" "domains=DOMAIN-LIST"
	     "exclude-domains DOMAIN-LIST" "follow-ftp" "follow-tags=LIST"
	     "ignore-tags=LIST" "span-hosts" "relative" "include-directories=LIST"
	     "exclude-directories=LIST" "no-parent")))
      (sort
       (mapcar
	(lambda (wrd)
	  (let* ((eql (or (string-match "=" wrd) (string-match " " wrd)))
		 (opt (concat "--" (substring wrd 0 (and eql (1+ eql))))))
	    (cons wrd opt)))
	options)
       (lambda (x y) (string< (car x) (car y))))))
  "Alist of wget long options")


(defvar wget-process-percent-alist nil
  "Alist of (PROC . PERCENT) of each wget process.
PROC is process of wget and PERCENT is number of download percentage.

See also `wget-process-byte-alist', `wget-process-length-alist',
`wget-process-saved-alist', `wget-process-dir-alist',
and `wget-process-messg-alist'.")

(defvar wget-process-byte-alist nil
  "Alist of (PROC . BYTE) of each wget process.
PROC is process of wget and BYTE is number of download byte.

See `wget-process-percent-alist' for more information.")

(defvar wget-process-length-alist nil
  "Alist of (PROC . LENGTH) of each wget process.
PROC is process of wget and BYTE is number of download byte.

See `wget-process-percent-alist' for more information.")

(defvar wget-process-mime-alist nil
  "Alist of (PROC . MIME) of each wget process.
PROC is process of wget and MIME is stirng of MIME code.

See `wget-process-percent-alist' for more information.")

(defvar wget-process-saved-alist nil
  "Alist of (PROC . SAVED) of each wget process.
PROC is process of wget and SAVED is string of saved file name.

See `wget-process-percent-alist' for more information.")

(defvar wget-process-dir-alist nil
  "Alist of (PROC . DIR) of each wget process.
PROC is process of wget and DIR is string of directory name where to download.

See `wget-process-percent-alist' for more information.")

(defvar wget-process-messg-alist nil
  "Alist of (PROC . MESSG) of each wget process.
PROC is process of wget and MESSG is message string.

See `wget-process-percent-alist' for more information.")

(defvar wget-ftp-regexp "^ftp://"
  "Regexp for FTP.")

(defvar wget-current-title nil)

(defconst wget-download-line-format (format "  %s%11s" "[100%]" " "))



;;;; User Interface

;;;###autoload
(defun wget (uri &optional arg)
  "Wget interface to download URI asynchronously.
If argument ARG is non-nil, ask some options.
Called with prefix argument, turn argument ARG t.

If you are in dired mode which is seeing ftp directory,
`wget' regard current line file name as URI."
  (interactive (list
		(if (string= major-mode "dired-mode")
		    (dired-wget)
		  (read-string "URI: " (thing-at-point-url-at-point)))
		(when current-prefix-arg t)))
  (let ((options (if (and wget-ftp-default-options
			  (string-match wget-ftp-regexp uri))
		     wget-ftp-default-options
		   wget-default-options))
	(dir (wget-cd-download-dir arg uri)))
    (when dir
      (setq options
	    (append wget-basic-options
		    (if arg
			(wget-read-options "Wget options: "
					   options wget-long-option-alist)
		      options)))
      (if (string= uri "")
	  (error "There is no uri")
	(wget-uri uri dir options)))))

;;;###autoload
(defun wget-web-page (uri &optional arg)
  "Wget interface to download whole Web page.
If argument ARG is non-nil, ask options.
Called with prefix argument, turn argument ARG t.

Second argument URI is string.
wget-web-page downlod whole Web page from it following relative link."
  (interactive (list
		(read-string "Web Page URI: " (thing-at-point-url-at-point))
		(if current-prefix-arg t)))
  (let ((options wget-web-page-options)
	(dir (wget-cd-download-dir arg uri)))
    (when dir
      (setq options
	    (append wget-basic-options
		    (if arg
			(wget-read-options "Wget options: "
					   options wget-long-option-alist)
		      options)))
      (if (string= uri "")
	  (error "There is no uri")
	(wget-uri uri dir options)))))


;;; Internal Code (Not including process)

(defun dired-wget ()
  "Wget interface for dired-mode in ftp directory.
Return string of URI at current line."
  (save-excursion
    (let ((max (progn (end-of-line 1) (point)))
	  uri base)
      (forward-line 0)
      (unless (re-search-forward " \\([^ ]+\\)$" max t)
	(error "No file at point"))
      (setq uri (match-string 1))
      (goto-char (point-min))
      (re-search-forward "@\\(.+\\):" nil t)
      (setq base (match-string 1))
      (read-string "URI: " (concat base "/" uri)))))

(defun wget-api (uri current-uri &optional arg)
  "Application Program Interface for wget"
  (if uri
      (progn
	(when arg
	  (setq uri (read-string "URI: " uri)))
	(wget uri arg))
    ;;No URI at point.
    (let ((char (read-char "Download:  [c]urrent page;  [w]eb pages;  [q]uit")))
      (cond
       ;; Current Page.
       ((equal ?c char)
	(if arg
	    (setq uri (read-string "URI: " current-uri))
	  (setq uri current-uri))
	(wget uri arg))
       ;; Web Pages.
       ((equal ?w char)
	(if arg
	    (setq uri (read-string "Web Page URI: " current-uri))
	  (setq uri current-uri))
	(wget-web-page uri arg))
       ;; Quit.
       ((equal ?q char)
	nil)
       (t (wget-api uri current-uri arg))))))

(defun wget-cd-download-dir (arg uri)
  "Change directory to wget download dir.
If ARG is non-nil, download dir is `wget-download-directory'.
Otherwise, Ask download dir.

When download dir is not directory or unwritable, get error"
  (let ((dir wget-download-directory))
    (cond
     ((functionp wget-download-directory-filter)
      (setq dir (funcall wget-download-directory-filter arg uri dir)))
     (arg
      (setq dir (wget-read-download-dir dir))))
    (unless dir
      (setq dir (wget-read-download-dir)))
    ;; Check directory exist and writable
    (catch 'directory
      (while t
	(cond
	 ((file-exists-p dir) (throw 'directory t))
	 ((y-or-n-p (format "%s does not exist. Create it? " dir))
	  (make-directory dir t)
	  (throw 'directory t))
	 (t (if (functionp wget-download-directory-filter)
		(setq dir (funcall wget-download-directory-filter arg uri dir))
	      (setq dir (wget-read-download-dir dir)))))))
    (unless (file-directory-p dir)
      (error "%s is not directory" dir))
    (unless (file-writable-p dir)
      (error "Can't open download directory: %s" dir))
    ;; Change Directory
    (setq dir (expand-file-name dir))
    (save-excursion
      (set-buffer (get-buffer-create (or wget-process-buffer " *wget*")))
      (cd dir))))

;; filter functions

(defun wget-download-dir-filter-current-dir (arg uri dir)
  "Ask download dir if current dir is not under DIR'.
If current dir is under DIR, then use current dir for download dir.

If first argument ARG is non-nil, always ask download dir.
Third argument DIR should be a string, not a list."
  (if (stringp dir)
      (if (and (not arg)
	       (string-match (concat "^" (expand-file-name dir)) default-directory))
	  default-directory
	(wget-read-download-dir dir))
    dir))

(defun wget-download-dir-filter-regexp (arg uri dir)
  "Change download directory by regexp.

`wget-download-directory' should be an alist of (REGEXP . DIR).
FILE that matches REGEXP goes to DIR.

Example of `wget-download-directory':
  ((\"\\\\.\\\\(jpe?g\\\\|png\\\\)$\" . \"~/pictures\")
   (\"\\\\.el$\" . \"~/site-lisp\")
   (\".\" . \"~/download\"))"
  (if (consp dir)
      (assoc-default uri dir #'string-match)
    dir))

(defun wget-download-dir-filter-alias (arg uri dir)
  "Ask alias of download directory.

`wget-download-directory' should be an alist of (ALIAS . DIR).

The alias \"default\" is special.  If it is found in
`wget-download-directory', use it by default.
When prefix argument C-u is specified, ask aliases.

Example of `wget-download-directory':
  ((\"pics\" . \"~/pictures\")
   (\"elisp\" . \"~/site-lisp\")
   (\"default\" . \"~/download\"))"
  (when (consp dir)
    (let ((default (cdr (assoc "default" dir))))
      (if arg ; C-u wget
	  (setq dir (wget-read-download-dir
		     (cdr (assoc (completing-read "Directory alias: " dir) dir))))
	;; Without C-u
	(if default
	    (setq dir default)
	  (setq dir (cdr (assoc (completing-read "Directory alias: " dir) dir)))))))
  dir)

(defun wget-download-dir-filter-regexp-and-alias (arg uri dir)
  "Change download directory by regexp and alias.
First call function `wget-download-dir-filter-regexp', and then
call function `wget-download-dir-filter-alias'.

So, `wget-download-directory' should be an alist and its value
should be one of them:
 (REGEXP . DIR) or (REGEXP . ((ALIAS . DIR)...)).

Example of `wget-download-directory':
  ((\"\\\\.\\\\(jpe?g\\\\|png\\\\)$\" . 
    ((\"dog\" . \"~/dogs/picture\")
     (\"cat\" . \"~/cats/picture\")
     (\"default\" . \"~/pictures\")))
   (\"\\\\.el$\" . \"~/site-lisp\")
   (\".\" . \"~/download\"))"
  (setq dir (wget-download-dir-filter-regexp arg uri dir))
  (wget-download-dir-filter-alias arg uri dir))

;; misc

(defun wget-read-download-dir (&optional dir)
  "Ask download dir.
The optional rgument DIR is default directory name."
  (wget-read-directory-name "Download directory: "
			    (and dir (file-name-as-directory dir))))

;;; Completion

(defvar wget-read-options-map
  (copy-keymap minibuffer-local-completion-map)
  "Local keymap for minibuffer to read wget long options.")

(define-key wget-read-options-map " "    'self-insert-command)
(define-key wget-read-options-map "\C-i" 'wget-option-completion)


(defun wget-read-options (prompt init-list table)
  "Read a string of wget option with completion."
  (let ((minibuffer-completion-table table)
	(init (mapconcat (lambda (x) x) init-list " ")))
    (split-string
     (read-from-minibuffer prompt init
			   wget-read-options-map))))

(defun wget-option-completion ()
  "Complete wget long options, stop to space."
  (interactive)
  (goto-char (point-max))
  (let* ((pos    (point))
	 (start  (progn
		   (skip-chars-backward "^ \n\t")
		   (point)))
	 (word   (prog1
		     (buffer-substring (if (looking-at "--") (+ 2 start) start)
				       pos)
		   (goto-char pos)))
	 (result (try-completion word minibuffer-completion-table))
	 (tmpmesg '(lambda (x)
		     (save-excursion
		       (goto-char (point-max))
		       (save-excursion (insert " " x))
		       (sit-for 1)
		       (delete-region (point) (point-max))))))
    (cond
     ((eq result t)
      (funcall tmpmesg "[Sole Completion]"))
     ((eq result nil)
      (ding)
      (funcall tmpmesg "[No Match!]"))
     ((string= result word)
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions word minibuffer-completion-table))))
     ((= (preceding-char) ?=)
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions word minibuffer-completion-table)))
      (funcall tmpmesg "[Sole Completion]"))
     (t
      (delete-region start pos)
      (insert (cdr (or (assoc result minibuffer-completion-table)
		       (cons result (concat "--" result)))))
      (unless (eq t (try-completion result minibuffer-completion-table))
	(funcall tmpmesg "[Complete, but not unique]"))))))


;;;; Wget Process

(defmacro wget-delete-property (proc alist)
  "Delete a member whose car is PROC from ALIST."
  `(when (assoc ,proc ,alist)
     (setq ,alist (delete (assoc ,proc ,alist) ,alist))))

(defmacro wget-change-property (proc value alist)
  "Change a property of process PROC in ALIST with cons-cell of (PROC . VALUE)."
  `(progn
     (wget-delete-property ,proc ,alist)
     (setq ,alist (cons (cons ,proc ,value) ,alist))))

(defun wget-uri (uri dir &optional options)
  "Wget URI asynchronously.
Optional argument OPTIONS is a list of options to pass wget
process."
  (let* ((args `(,@options ,uri))
	 (buf  wget-process-buffer)
	 (lang (concat "LANG=" (getenv "LANG")))
	 (process-environment (cons "LANG=C"
				    (delete lang (copy-sequence process-environment))))
	 (proc (save-excursion
		 (set-buffer (or buf " *wget*"))
		 (wget-write-download-log uri)
		 (apply 'start-process "wget" buf wget-command args)))
	 (win  (selected-window)))
    ;; Initialize property.
    (wget-change-property proc nil wget-process-percent-alist)
    (wget-change-property proc nil wget-process-byte-alist)
    (wget-change-property proc nil wget-process-length-alist)
    (wget-change-property proc nil wget-process-saved-alist)
    (wget-change-property proc dir wget-process-dir-alist)
    (wget-change-property proc nil wget-process-messg-alist)
    ;; Set process functions.
    (when buf
      (set-process-filter proc 'wget-process-filter))
    (set-process-sentinel proc 'wget-process-sentinel)
    (message "Downloading %s..."
	     (wget-process-file-name proc))
    (wget-state-of-progress proc)
    (run-hooks 'wget-hook)))

;;; Debug Code
;;
;; (apply 'start-process "WGET" "*Wget*" "wget" ;wget-command
;;        '("https://github.com/ataka/emacs-wget"))
;;


(defun wget-process-filter (proc string)
  "Process filter function for wget.
Argument PROC is process of wget and argument STRING is an output string from wget."
  (when wget-debug
    (save-excursion
      (set-buffer (get-buffer-create wget-debug-buffer))
      (insert string "")))
  (when (string-match "[0-9a-Z]" string) ; Ignore wget output that contains only `.'
    (let ((proc-cell (wget-get-wget-process proc))
	  length mime percent byte saved messg)
      ;; Connecting
      (when (string-match "Connecting to" string)
	(wget-progress-update proc-cell "connecting"))
      ;; Connected
      (when (string-match "connected[!.]" string)
	(wget-progress-update proc-cell "connected"))
      ;; Reusing connection
      (when (string-match "Reusing connection to" string)
	(wget-change-property proc 0 wget-process-percent-alist)
	(wget-progress-update proc-cell 0))
      ;; Length
      (when (string-match "Length: \\([0-9,]+\\)" string)
	(setq length (match-string 1 string))
	(wget-change-property proc length wget-process-length-alist))
      (when (string-match
	     "Length: \\([0-9,]+\\) \\(\\[.+\\]\\|(.+)\\)" string)
	(setq mime (match-string 2 string))
	(wget-change-property proc mime wget-process-mime-alist))
      ;; Saving
      (when (string-match "=> `\\(.+\\)'" string)
	(wget-change-property proc (match-string 1 string)
			      wget-process-saved-alist))
      ;; Retrieved
      (when (string-match "The file is already fully retrieved; nothing to do\\." string)
	(wget-change-property proc "retrieved" wget-process-messg-alist))
      (when (string-match "no newer than local file" string)
	(wget-change-property proc "retrieved" wget-process-messg-alist))
      ;; Percent
      (when (string-match "\\([ 1][ 0-9][0-9]\\)%" string)
	(setq percent (string-to-number (match-string 1 string)))
	(wget-change-property proc percent wget-process-percent-alist)
	(wget-progress-update proc-cell percent))
      ;; Byte
      (when (string-match "\\([0-9]+\\)K" string)
	(setq byte (string-to-number (match-string 1 string)))
	(wget-change-property proc byte wget-process-byte-alist))
      ;; Saved
      (when (string-match "`\\(.+\\)' saved" string)
	(setq saved (match-string 1 string))
	(wget-change-property proc saved wget-process-saved-alist))
      ))
  (if (and (> (point) (length "  --- Wget Process ---"))
	   (string= (buffer-name (current-buffer)) wget-process-buffer))
      (move-to-column (length wget-download-line-format))))

(defun wget-reset-property (proc)
  "Reset property of process PROC in `wget-process-*-alist'.
See also `wget-change-property'."
  (wget-delete-property proc wget-process-percent-alist)
  (wget-delete-property proc wget-process-byte-alist)
  (wget-delete-property proc wget-process-length-alist)
  (wget-delete-property proc wget-process-saved-alist)
  (wget-delete-property proc wget-process-dir-alist)
  (wget-delete-property proc wget-process-messg-alist))

(defun wget-process-sentinel (proc state)
  "Process setinel function for wget.
Argument PROC is process of wget and STATE is state."
  (let ((ps  (process-status proc))
	(status (process-exit-status proc))
	(win (when (process-buffer proc)
	       (get-buffer-window (process-buffer proc)))))
    (cond
     ((eq ps 'exit)
      (unwind-protect
	  (if (and (eq status 0) (wget-get-wget-process proc))
	      (progn
		(ding)
		(message "Downloading %s...done"
			 (or (cdr (assoc proc wget-process-saved-alist))
			     (wget-process-file-name proc)))
		(if (string= (cdr (assoc proc wget-process-messg-alist))
			     "retrieved")
		    (wget-progress-update (wget-get-wget-process proc) "retrieved")
		  (wget-progress-update (wget-get-wget-process proc) "downloaded"))
		(wget-close-wget-output-window win)
		(when wget-executable-file-extension-list
		  (wget-set-file-executable proc))
		(run-hooks 'wget-after-hook))
	    (ding)
	    (message "Downloading %s...failed"
		     (wget-process-file-name proc))
	    (wget-revert-buffer)
	    (wget-close-wget-output-window win))
	(wget-reset-property proc)))
     ((eq ps 'signal)
      (unwind-protect
	  (if (eq status 9)
	      (progn
		(wget-revert-buffer)
		(wget-close-wget-output-window win))
	    (error "Downloading %s...failed and exit"
		   (wget-process-file-name proc)))
	(wget-reset-property proc)))
     (t nil))))

(defun wget-quit ()
  "Kill wget process."
  (interactive)
  (let (proc (proc-alist (wget-get-process-alist)))
    (cond
     ;; No wget process.
     ((null proc-alist)
      (error "No wget process"))
     ;; Only one wget process.
     ((null (cdr proc-alist))
      (setq proc (cdr (car proc-alist))))
     ;; Called in *wget* buffer.
     ((equal wget-process-buffer (buffer-name))
      (setq proc
	    (let ((file (progn (forward-line 0)
			       (buffer-substring-no-properties
				(progn
				  (move-to-column (length wget-download-line-format))
				  (point))
				(progn (end-of-line 1) (point))))))
	      (or (cdr (assoc file proc-alist))
		  (car (rassoc file wget-process-saved-alist)))))
      (unless proc
	(error "No wget process")))
     ;; Many wget processes.
     (t (setq proc
	      (cdr (assoc (completing-read
			   "Kill: " proc-alist nil t)
			  proc-alist)))))
    (if (rassoc proc proc-alist)
	(if (wget-kill-process proc t t)
	    (message "Killed process and removed %s"
		     (wget-process-file-name proc))
	  (message "Killed process"))
      (message "Wget process is finised"))))

(defun wget-quit-and-exit ()
  "Kill all wget processes."
  (interactive)
  (while (wget-get-process-alist)
    (wget-kill-process (cdr (car (wget-get-process-alist))) t nil)
    (sit-for 1)	;;Quitting Process too fast lead to Emacs panic??
    ))

(defun wget-kill-process (proc del &optional query)
  "Kill wget process PROC.

If argument DEL is non-nil, remove downloaded file.
However, if process download recursively, do not remove files and directory.
If optional argument QUERY is non-nil, ask remove or not."
  (wget-reset-property proc)
  (kill-process proc)
  (let ((file (expand-file-name (wget-process-file-name proc)
				(cdr (assoc proc wget-process-dir-alist)))))
    (if (and del
	     (not (wget-recursive-p proc))
	     (file-exists-p file)
	     (or (not query) (y-or-n-p "Remove downloaded file? ")))
	(delete-file file))))

(defun wget-get-process-alist ()
  "Return alist of (URI . PROCESS).
If no process for wget, just return nil."
  (let ((proc-alist (mapcar 'wget-get-wget-process (process-list))))
    (delete nil proc-alist)))

(defun wget-get-wget-process (proc)
  "Return cons cell of (URI . PROCESS) if PROC is `wget-command' process.
If not, return nil."
  (let ((command (process-command proc)))
    (when (string= wget-command (car command))
      (cons (nth (1- (length command)) command) proc))))

(defun wget-process-file-name (proc)
  "Return file or directory name of URI under wget process PROC."
  (let ((uri (car (wget-get-wget-process proc))))
    (wget-replace-regexp-in-string ".+/" "" uri)))

(defun wget-close-wget-output-window (win)
  "Close *wget* window.
Argument WIN is window."
  (when (and (windowp win) (not (wget-get-process-alist)))
    (delete-window win)))

(defun wget-recursive-p (proc)
  "Return t if wget option contains \"-r\" or \"--recursive\"."
  (let ((proc-com (process-command proc)))
    (or (member "-r" proc-com)
	(member "--recursive" proc-com))))


;;;; Wget mode

(defvar wget-mode-map nil)
(if wget-mode-map
    nil
  (let ((map (make-keymap)))
        (suppress-keymap map)
	(define-key map "q" 'quit-window)
	(define-key map "Q" 'wget-quit-and-exit)
	(define-key map "d" 'wget-quit)
	(define-key map "g" 'wget-revert-buffer)
	(define-key map "i" 'wget-info)
	(define-key map "n" 'wget-next-line)
	(define-key map "p" 'wget-previous-line)
	(setq wget-mode-map map)))

(defun wget-mode ()
  "Major mode for operating wget process.

State of wget downloading rogress is described as follows:
  [STATE]: [PROGRESS]  [URI]

STATE show percent of downloading.
PROGRESS show bar graph of download.

Keybindings:
\\{wget-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines wget-truncate-partial-width-windows)
  (use-local-map wget-mode-map)
  (setq major-mode 'wget-mode
	mode-name "wget"
	buffer-read-only t))

(defun wget-state-of-progress (proc)
  "Show state of wget downloading progress."
  (interactive (list
		(cdr (cdr (wget-get-process-alist)))))
  (let ((win (selected-window))
	(buf wget-process-buffer)
	(window-min-height 3))
    (when buf
      (if (get-buffer-window buf)
	  (progn
	    (select-window (get-buffer-window buf))
	    (if (< (window-height) wget-max-window-height)
		(enlarge-window 1)))
	(select-window
	 (split-window-vertically
	  (- (if (< (+ 3 (length (wget-get-process-alist)))
		    wget-max-window-height)
		 (+ 3 (length (wget-get-process-alist)))
	       wget-max-window-height))))
	(switch-to-buffer buf))
      (wget-revert-buffer)
      (wget-mode)
      (select-window win))))

(defun wget-revert-buffer ()
  "Revert wget buffer."
  (interactive)
  (when wget-process-buffer
    (save-excursion
      (set-buffer wget-process-buffer)
      (let ((proc-alist (wget-get-process-alist))
	    (buffer-read-only nil)
	    (height (wget-window-height)))
	(erase-buffer)
	(goto-char (point-min))
	(insert "  --- Wget Process ---")
	(if proc-alist
	    (progn
	      (mapcar 'wget-progress-update proc-alist)
	      (when (and
		     (not (one-window-p))
		     (> height (+ 2 (length proc-alist))))
		(shrink-window (- height (+ 2 (length proc-alist))))))
	  ;; if no processes.
	  (insert "\n ** No Wget Process **"))
	(set-buffer-modified-p nil)))))

(defun wget-progress-update (proc-cell &optional progress)
  "Update wget progress and return uri.
Argument PROC-CELL is cons cell of (URI . PROCESS)."
  (when (process-buffer (cdr proc-cell))
    (save-excursion
      (let* ((status "[  0%]")
	     (bar  (format "%11c" ? ))
	     (uri  (wget-replace-regexp-in-string "~" "%7E" (car proc-cell)))
	     (proc (cdr proc-cell))
	     (buffer-read-only (prog1 nil
				 (set-buffer (process-buffer proc)))))
	(if progress
	    (let ((file (cdr (assoc proc wget-process-saved-alist))))
	      (goto-char (point-min))
	      (when uri
		(if (not (and file (wget-recursive-p proc)))
		    (search-forward uri nil t)
		  (if file
		      (unless (search-forward file nil t)
			(goto-char (point-max))
			(search-forward uri nil t))))
		(delete-region (progn (forward-line 0) (point))
			       (progn (end-of-line 1) (point)))
		(forward-line 0))
	      (if (numberp progress)
		  (setq status (format "[%3d%%]" progress)
			bar    (format "%-11s"
				       (make-string (/ progress 10) ?*)))
		;; if PROGRESS is not a number but a string.
		(setq bar "")
		(cond
		 ((string= progress "connecting")
		  (setq status "*Connecting...   "))
		 ((string= progress "connected")
		  (setq status "=*=Connected=*=  "))
		 ((string= progress "retrieved")
		  (setq status "=*=Up-To-Date=*= "
			uri (wget-process-file-name proc)))
		 ((string= progress "downloaded")
		  (setq status "=*=DOWNLOADED=*= "
			uri (concat (file-name-as-directory
				     (cdr (assoc proc wget-process-dir-alist)))
				    file)))))
	      (when (and file (wget-recursive-p proc))
		(setq uri file)))
	  ;; If no progress
	  (if (cdr (assoc proc wget-process-percent-alist))
	      (setq progress (cdr (assoc proc wget-process-percent-alist))
		    status   (format "[%3d%%]" progress)
		    bar      (format "%-11s"
				     (make-string (/ progress 10) ?*))))
	  (end-of-line 1)
	  (insert "\n"))
	(when uri
	  (insert "  " status bar uri)
	  (set-buffer-modified-p nil))))))

(defun wget-info ()
  "Show information about the retrieving file."
  (interactive)
  (let* ((file (save-excursion
		 (buffer-substring
		  (progn (forward-line 0)
			 (move-to-column (length wget-download-line-format))
			 (point))
		  (progn (end-of-line) (point)))))
	 (proc (cdr (assoc file (wget-get-process-alist)))))
    (when proc
      (let ((byte    (cdr (assoc proc wget-process-byte-alist)))
	    (length  (cdr (assoc proc wget-process-length-alist)))
	    (percent (cdr (assoc proc wget-process-percent-alist)))
	    (mime    (cdr (assoc proc wget-process-mime-alist)))
	    (dir     (cdr (assoc proc wget-process-dir-alist))))
	(princ (concat (if byte (number-to-string  byte))
		       (if length (format " / %s byte " length))
		       (if percent (format "(%d%%)  " percent) " ")
		       mime " => " dir))))))

(defun wget-next-line (arg)
  "Move cursor vertically down ARG lines at beginning of URI."
  (interactive "p")
  (forward-line arg)
  (move-to-column (length wget-download-line-format))
  (wget-info))

(defun wget-previous-line (arg)
  "Move cursor vertically up ARG lines at beginning of URI."
  (interactive "p")
  (forward-line (- arg))
  (unless (bobp)
    (move-to-column (length wget-download-line-format))
    (wget-info)))


;;;; misc

(defun wget-write-download-log (uri)
  "Write download log into `wget-download-log-file'."
  (let ((log wget-download-log-file)
	(case-fold-search nil)
	str)
    (when log
      (setq log (expand-file-name log))
      (when (or (file-exists-p log)
		(eq wget-create-download-log 'always)
		(and (eq wget-create-download-log 'ask)
		     (y-or-n-p "Create download log file? ")))
	;; format log str
	(setq str (wget-replace-regexp-in-string
		   "%T" (format-time-string wget-download-log-time-format)
		   wget-download-log-format t))
	(when (string-match "%t" str)
	  (setq str (wget-replace-regexp-in-string
		     "%t" (read-string "Title: " wget-current-title)
		     str t)))
	(setq str (wget-replace-regexp-in-string
		   "%U" uri str t))
	(save-excursion
	  (set-buffer (or (get-file-buffer log) (find-file-noselect log)))
	  (if wget-add-download-log-eof
	      (goto-char (point-max))
	    (goto-char (point-min)))
	  (unless (bolp)
	    (insert "\n"))
	  (insert str)
	  (basic-save-buffer))))))

(defun wget-set-file-executable (proc)
  "Change file permission executable if file extension matches `wget-executable-file-extension-list'."
  (let ((dir  (cdr (assoc proc wget-process-dir-alist)))
	(file (or (cdr (assoc proc wget-process-saved-alist))
		  (wget-process-file-name proc)))
	(ext  (concat "\\." (regexp-opt wget-executable-file-extension-list) "\\'"))
	modes)
    (when (string-match ext file)
      (setq file (expand-file-name file dir))
      (if (eq system-type 'windows-nt)
	  (call-process "chmod" nil nil nil "+x" file)
	(setq modes (logior (file-modes file) ?\111))
	(set-file-modes file modes)))))


(provide 'wget)
(unless noninteractive
  (run-hooks 'wget-load-hook))

;;; wget.el ends here
