;;; w3m-session.el --- Functions to operate session of w3m -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Hideyuki SHIRAI <shirai@meadowy.org>
;; Keywords: w3m, WWW, hypermedia

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

;; w3m-session.el is the add-on program of emacs-w3m to save and load
;; sessions.   For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:
(eval-when-compile (require 'cl))
(require 'w3m-util)

(eval-when-compile
  (defvar w3m-async-exec)
  (defvar w3m-async-exec-with-many-urls)
  (defvar w3m-current-title)
  (defvar w3m-current-url)
  (defvar w3m-history)
  (defvar w3m-history-flat)
  (defvar w3m-language)
  (defvar w3m-mode-map)
  (defvar w3m-profile-directory)
  (autoload 'w3m-goto-url-new-session "w3m")
  (autoload 'w3m-history-tree "w3m-hist")
  (autoload 'w3m-load-list "w3m")
  (autoload 'w3m-save-list "w3m"))

(defcustom w3m-session-file
  (expand-file-name ".sessions" w3m-profile-directory)
  "*File name to keep sessions."
  :group 'w3m
  :type '(file :size 0))

(defcustom w3m-session-autosave t
  "*Non-nil means save automatically when w3m quit."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-deleted-save t
  "*Non-nil means save deleted sessions."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-crash-recovery t
  "*Non-nil means emacs-w3m save session set automatically, and recover it when emacs-w3m crash."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-time-format
  (if (and (equal "Japanese" w3m-language)
	   (not (featurep 'xemacs)))
      "%Y年%m月%d日(%a) %H:%M"
    "%Y-%m-%d (%a) %H:%M")
  "*Format of saved time."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-automatic-title
  (if (equal "Japanese" w3m-language)
      "自動保存"
    "Automatic saved sessions")
  "*String of title to save session automatically."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-deleted-title
  (if (equal "Japanese" w3m-language)
      "削除セッション"
    "Removed sessions")
  "*String of title to save session when buffer delete."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-crash-recovery-title
  (if (equal "Japanese" w3m-language)
      "クラッシュ回復"
    "Crash recovery sessions")
  "*String of title to save session to use for crash recovering."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-deleted-keep-number 5
  "*Number to keep sessions when buffers delete."
  :group 'w3m
  :type '(integer :size 0))

(defcustom w3m-session-automatic-keep-number 5
  "*Number to keep sessions automatically."
  :group 'w3m
  :type '(integer :size 0))

(defcustom w3m-session-unknown-title "<Unknown Title>"
  "*String of title to use when title is not specified."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-load-last-sessions nil
  "*Whether to load the last sessions when emacs-w3m starts."
  :group 'w3m
  :type '(radio (const :format "Load the last sessions automatically." t)
		(const :format "Ask whether to load the last sessions." ask)
		(const :format "Never load the last sessions automatically." nil)))

(defcustom w3m-session-load-crashed-sessions 'ask
  "*Whether to load the crashed sessions when emacs-w3m starts."
  :group 'w3m
  :type '(radio (const :format "Load the crashed sessions automatically." t)
		(const :format "Ask whether to load the crashed sessions." ask)
		(const :format "Never load the crashed sessions automatically." nil)))

(defface w3m-session-select
  `((((class color) (background light) (type tty))
     (:foreground "black"))
    (((class color) (background dark) (type tty))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (((class color) (background dark))
     (:foreground "white"))
    (t nil))
  "Face of w3m-session."
  :group 'w3m)
;; backward-compatibility alias
(put 'w3m-session-select-face 'face-alias 'w3m-session-select)

(defface w3m-session-selected
  `((((class color) (background light) (type tty))
     (:foreground "blue" :bold t :underline t))
    (((class color) (background dark) (type tty))
     (:foreground "cyan" :bold t :underline t))
    (((class color) (background light))
     (:foreground "dark blue" :bold t :underline t))
    (((class color) (background dark))
     (:foreground "white" :bold t :underline t))
    (t (:bold t :underline t)))
  "Face of selected w3m-session."
  :group 'w3m)
;; backward-compatibility alias
(put 'w3m-session-selected-face 'face-alias 'w3m-session-selected)

(defun w3m-session-history-to-save ()
  "Return a copy of `w3m-history-flat' without current page data."
  (let ((pos (cadar w3m-history)))
    (apply
     'append
     (mapcar (lambda (x)
	       (unless (equal (nth 2 x) pos)
		 (list x)))
	     (copy-sequence (w3m-history-slimmed-history-flat))))))

;; format of sessin file.
;; '((sessiontitle1 time1 ((url11 pos11 hflat11 urltitle11)
;;                         (url12 pos12 hflat12 urltitle12) ...) current1)
;;   ...

(defmacro w3m-session-ignore-errors (&rest forms)
  "Run FORMS.  Remove `w3m-session-file' and quit if any error happens."
  `(condition-case err
       (progn ,@forms)
     (error
      (if (and (file-exists-p w3m-session-file)
	       (yes-or-no-p (format
			     "\
Sorry, an error found in \"%s\"; may we remove it? "
			     ,(if (featurep 'xemacs)
				  '(abbreviate-file-name w3m-session-file t)
				'(abbreviate-file-name w3m-session-file)))))
	  (progn
	    (delete-file w3m-session-file)
	    (run-at-time 0.1 nil #'message
			 "\"%s\" has been removed; try again"
			 (abbreviate-file-name w3m-session-file))
	    (keyboard-quit))
	(signal (car err) (cdr err))))))

;;;###autoload
(defun w3m-session-save ()
  "Save list of displayed session."
  (interactive)
  (w3m-session-ignore-errors
   (let ((sessions (w3m-load-list w3m-session-file))
	 (bufs (w3m-list-buffers))
	 (prompt "New session title: ")
	 (cnum 0)
	 (i 0)
	 title titles urls len buf cbuf)
     (mapc (lambda (x)
	     (setq titles (cons (cons (car x) (car x)) titles)))
	   sessions)
     (setq title (or w3m-current-title
		     (with-current-buffer (car bufs)
		       w3m-current-title)))
     (setq titles (cons (cons title title) titles))
     (catch 'loop
       (while t
	 (setq title (completing-read prompt titles nil nil title))
	 (if (or (string= title "")
		 (and (assoc title sessions)
		      (not (y-or-n-p (format "\"%s\" is exist. Overwrite? "
					     title)))))
	     (setq prompt "Again New session title: ")
	   (throw 'loop t))))
     (setq cbuf (current-buffer))
     (save-current-buffer
       (while (setq buf (car bufs))
	 (setq bufs (cdr bufs))
	 (set-buffer buf)
	 (when w3m-current-url
	   (when (eq cbuf (current-buffer))
	     (setq cnum i))
	   (setq i (1+ i))
	   (setq urls (cons (list w3m-current-url
				  (copy-sequence (caar w3m-history))
				  (w3m-session-history-to-save)
				  w3m-current-title)
			    urls)))))
     (if (not urls)
	 (message "%s: no session save...done" title)
       (setq len (length urls))
       (setq urls (nreverse urls))
       (when (assoc title sessions)
	 (setq sessions (delete (assoc title sessions) sessions)))
       (setq sessions (cons (list title (current-time) urls cnum) sessions))
       (w3m-save-list w3m-session-file sessions)
       (if (= len 1)
	   (message "%s: 1 session save...done" title)
	 (message "%s: %d sessions save...done" title len))))))

(defun w3m-session-automatic-save ()
  "Save list of displayed session automatically."
  (when w3m-session-autosave
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (bufs (w3m-list-buffers))
	   (title (concat w3m-session-automatic-title "-1"))
	   (titleregex (concat "^"
			       (regexp-quote w3m-session-automatic-title)
			       "-[0-9]+$"))
	   (cnum 0)
	   (i 0)
	   urls buf cbuf session
	   tmp tmptitle tmptime tmpurls)
       (when bufs
	 (setq cbuf (current-buffer))
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (when (eq cbuf (current-buffer))
		 (setq cnum i))
	       (setq i (1+ i))
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (setq i 2)
	   (while (setq session (car sessions))
	     (setq sessions (cdr sessions))
	     (if (string-match titleregex (nth 0 session))
		 (when (<= i w3m-session-automatic-keep-number)
		   (setq tmptitle (format (concat w3m-session-automatic-title
						  "-%d") i))
		   (setq tmptime (nth 1 session))
		   (setq tmpurls (nth 2 session))
		   (setq tmp (cons (list tmptitle tmptime tmpurls nil) tmp))
		   (setq i (1+ i)))
	       (setq tmp (cons session tmp))))
	   (setq sessions (nreverse tmp))
	   (setq urls (nreverse urls))
	   (setq sessions (cons (list title (current-time) urls cnum)
				sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

(defun w3m-session-deleted-save (buffers)
  "Save list of deleted session."
  (when w3m-session-deleted-save
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (title (concat w3m-session-deleted-title "-1"))
	   (titleregex (concat "^"
			       (regexp-quote w3m-session-deleted-title)
			       "-[0-9]+$"))
	   (bufs (copy-sequence buffers))
	   (i 2)
	   urls buf session
	   tmp tmptitle tmptime tmpurls)
       (when bufs
	 (setq bufs (sort bufs 'w3m-buffer-name-lessp))
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (while (setq session (car sessions))
	     (setq sessions (cdr sessions))
	     (if (string-match titleregex (nth 0 session))
		 (when (<= i w3m-session-deleted-keep-number)
		   (setq tmptitle (format (concat w3m-session-deleted-title
						  "-%d") i))
		   (setq tmptime (nth 1 session))
		   (setq tmpurls (nth 2 session))
		   (setq tmp (cons (list tmptitle tmptime tmpurls nil) tmp))
		   (setq i (1+ i)))
	       (setq tmp (cons session tmp))))
	   (setq sessions (nreverse tmp))
	   (setq urls (nreverse urls))
	   (setq sessions (cons (list title (current-time) urls nil) sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

(defun w3m-session-crash-recovery-save ()
  "Save list of displayed session."
  (when w3m-session-crash-recovery
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (bufs (w3m-list-buffers))
	   (title w3m-session-crash-recovery-title)
	   urls buf tmp)
       (when bufs
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (setq urls (nreverse urls))
	   (setq tmp (assoc title sessions))
	   (when tmp (setq sessions (delete tmp sessions)))
	   (setq sessions (cons (list title (current-time) urls nil) sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

(defun w3m-session-crash-recovery-remove ()
  "Remove crash recovery session set."
  (when w3m-session-crash-recovery
    (w3m-session-ignore-errors
     (let* ((sessions (w3m-load-list w3m-session-file))
	    (item (assoc w3m-session-crash-recovery-title sessions)))
       (when item
	 (setq sessions (delete item sessions))
	 (w3m-save-list w3m-session-file sessions))))))

(defvar w3m-session-select-mode-map nil)
(unless w3m-session-select-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'w3m-session-select-quit)
    (define-key map "Q" 'w3m-session-select-quit)
    (define-key map "\C-g" 'w3m-session-select-quit)
    (define-key map "\C-m" 'w3m-session-select-select)
    (define-key map "\M-s" 'w3m-session-select-open-session-group)
    (define-key map "d" 'w3m-session-select-delete)
    (define-key map "D" 'w3m-session-select-delete)
    (define-key map "s" 'w3m-session-select-save)
    (define-key map "S" 'w3m-session-select-save)
    (define-key map "r" 'w3m-session-select-rename)
    (define-key map "R" 'w3m-session-select-rename)
    (define-key map "n" 'w3m-session-select-next)
    (define-key map "j" 'w3m-session-select-next)
    (define-key map "\C-n" 'w3m-session-select-next)
    (define-key map [down] 'w3m-session-select-next)
    (define-key map "p" 'w3m-session-select-previous)
    (define-key map "k" 'w3m-session-select-previous)
    (define-key map "\C-p" 'w3m-session-select-previous)
    (define-key map [up] 'w3m-session-select-previous)
    (setq w3m-session-select-mode-map map)))

;;; Local variables
(defvar w3m-session-select-wincfg nil)
(defvar w3m-session-select-sessions nil)
(make-variable-buffer-local 'w3m-session-select-wincfg)
(make-variable-buffer-local 'w3m-session-select-sessions)

(defun w3m-session-select-mode (&optional sessions)
  "Major mode for selecting emacs-w3m session.

\\<w3m-session-select-mode-map>
\\[w3m-session-select-select]	Select the session.
\\[w3m-session-select-open-session-group]	Open the session group.
\\[w3m-session-select-delete]	Delete the session.
\\[w3m-session-select-rename]	Rename the session.
\\[w3m-session-select-save]	Save the session.
\\[w3m-session-select-next]	Move the point to the next session.
\\[w3m-session-select-previous]	Move the point to the previous session.
\\[w3m-session-select-quit]	Exit selecting session.
"
  (w3m-session-ignore-errors
   (let ((sessions (or sessions
		       (w3m-load-list w3m-session-file))))
     (buffer-disable-undo)
     (setq mode-name "w3m session"
	   truncate-lines t
	   buffer-read-only nil
	   major-mode 'w3m-session-select-mode
	   w3m-session-select-sessions sessions
	   buffer-read-only t)
     (use-local-map w3m-session-select-mode-map)
     (w3m-session-select-list-all-sessions))))

(defun w3m-session-select-list-all-sessions ()
  "List up all saved sessions."
  (let* ((sessions w3m-session-select-sessions)
	 (num 0)
	 (max 0)
	 (buffer-read-only nil)
	 title titles time times url urls wid pos)
    (if (not sessions)
	(progn
	  (message "No saved session")
	  (w3m-session-select-quit))
      (mapc (lambda (x)
	      (setq title (format "%s[%d]" (nth 0 x) (length (nth 2 x))))
	      (setq wid (string-width title))
	      (when (> wid max)
		(setq max wid))
	      (setq titles (cons title titles))
	      (setq times (cons (format-time-string w3m-session-time-format
						    (nth 1 x))
				times))
	      (setq urls (cons (mapconcat (lambda (url)
					    (if (stringp url)
						url
					      (car url)))
					  (nth 2 x) ", ")
			       urls)))
	    sessions)
      (setq titles (nreverse titles))
      (setq times (nreverse times))
      (setq urls (nreverse urls))
      (setq max (+ max 2))
      (erase-buffer)
      (insert "Select session:\n\n")
      (while (and (setq title (car titles))
		  (setq time (car times))
		  (setq url (car urls)))
	(setq titles (cdr titles))
	(setq times (cdr times))
	(setq urls (cdr urls))
	(setq pos (point))
	(insert title)
	(add-text-properties pos (point)
			     `(face w3m-session-select
				    w3m-session-number ,num))
	(setq num (1+ num))
	(insert (make-string (- max (string-width title)) ?\ ))
	(insert time "  " url "\n"))
      (goto-char (point-min))
      (goto-char (next-single-property-change
		  (point) 'w3m-session-number))
      (put-text-property (point)
			 (next-single-property-change
			  (point) 'w3m-session-number)
			 'face 'w3m-session-selected)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(defun w3m-session-select-list-session-group (arg)
  (let ((session (nth 2 (nth arg w3m-session-select-sessions)))
	(num 0)
	(max 0)
	(buffer-read-only nil)
	title url wid
	titles urls pos)
    (when session
      (mapc (lambda (x)
	      (setq title (format "%s" (or (nth 3 x) w3m-session-unknown-title)))
	      (setq wid (string-width title))
	      (when (> wid max)
		(setq max wid))
	      (setq titles (cons title titles))
	      (setq urls (cons (nth 0 x)
			       urls)))
	    session)
      (setq titles (nreverse titles))
      (setq urls (nreverse urls))
      (setq max (+ max 2))
      (erase-buffer)
      (insert "Select session:\n\n")
      (setq pos (point))
      (insert "Open all sessions")
      (add-text-properties pos (point)
			   `(face w3m-session-selected
				  w3m-session-number ,arg))
      (insert "\n")
      (while (and (setq title (car titles))
		  (setq url (car urls)))
	(setq titles (cdr titles))
	(setq urls (cdr urls))
	(setq pos (point))
	(insert title)
	(add-text-properties pos (point)
			     `(face w3m-session-select
				    w3m-session-number ,(cons arg num)))
	(setq num (1+ num))
	(insert (make-string (- max (string-width title)) ?\ ))
	(insert url "\n"))
      (goto-char (point-min))
      (goto-char (next-single-property-change
		  (point) 'w3m-session-number)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun w3m-session-select-next (&optional arg)
  "Move the point to the next session."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((positive (< 0 arg))
	(buffer-read-only nil))
    (beginning-of-line)
    (put-text-property (point)
		       (next-single-property-change
			(point) 'w3m-session-number)
		       'face 'w3m-session-select)
    (while (not (zerop arg))
      (forward-line (if positive 1 -1))
      (unless (get-text-property (point) 'w3m-session-number)
	(if positive
	    (goto-char (next-single-property-change
			(point-min) 'w3m-session-number))
	  (goto-char (previous-single-property-change
		      (point-max) 'w3m-session-number))))
      (setq arg (if positive
		    (1- arg)
		  (1+ arg))))
    (beginning-of-line)
    (put-text-property (point)
		       (next-single-property-change
			(point) 'w3m-session-number)
		       'face 'w3m-session-selected)
    (set-buffer-modified-p nil)))

(defun w3m-session-select-previous (&optional arg)
  "the point to the previous session."
  (interactive "p")
  (w3m-session-select-next (- arg)))

(defun w3m-session-select-quit ()
  "Exit from w3m session select mode."
  (interactive)
  (let ((buffer (current-buffer))
	(wincfg w3m-session-select-wincfg))
    (or (one-window-p) (delete-window))
    (kill-buffer buffer)
    (set-window-configuration wincfg)))

(defun w3m-session-select-select ()
  "Select the session."
  (interactive)
  (beginning-of-line)
  (let* ((num (get-text-property
	       (point) 'w3m-session-number))
	 (item (if (consp num) 
		   (nth (cdr num)
			(caddr (nth (car num) 
				    w3m-session-select-sessions)))
		 (nth num w3m-session-select-sessions)))
	 (session (if (consp num)
		      (list (or (cadddr item) w3m-session-unknown-title)
			    nil
			    (list item) 
			    nil)
		    item)))
    (w3m-session-select-quit)
    (w3m-session-goto-session session)))

(defun w3m-session-select-open-session-group ()
  "Open the session group."
  (interactive)
  (beginning-of-line)
  (let ((num (get-text-property
	      (point) 'w3m-session-number))
	wheight)
    (if (consp num)
	(message "There is no session group.")
      (setq wheight 
	    (max (+ (length (caddr (nth num w3m-session-select-sessions))) 6)
		 window-min-height))
      (condition-case nil
	  (enlarge-window (- wheight (window-height)))
	(error nil))
      (w3m-session-select-list-session-group num))))

(defun w3m-session-select-save ()
  "Save the session."
  (interactive)
  (when (y-or-n-p "Save this sessions? ")
    (w3m-session-select-quit)
    (w3m-session-save)
    (w3m-session-select)))

(defun w3m-session-select-rename ()
  "Rename this session."
  (interactive)
  (beginning-of-line)
  (let ((num (get-text-property
	      (point) 'w3m-session-number))
	(sessions w3m-session-select-sessions))
    (w3m-session-select-quit)
    (w3m-session-rename sessions num)
    (w3m-session-select)))

(defun w3m-session-select-delete ()
  "Delete the session."
  (interactive)
  (when (y-or-n-p "Delete this session? ")
    (beginning-of-line)
    (let ((num (get-text-property
		(point) 'w3m-session-number))
	  (sessions w3m-session-select-sessions))
      (w3m-session-select-quit)
      (w3m-session-delete sessions num)
      (w3m-session-select))))

;;;###autoload
(defun w3m-session-select ()
  "Select session from session list."
  (interactive)
  (w3m-session-ignore-errors
   (let* ((sessions (w3m-load-list w3m-session-file))
	  (showbuf (w3m-get-buffer-create " *w3m-session select*"))
	  (wheight (max (+ (length sessions) 5) window-min-height))
	  (wincfg (current-window-configuration))
	  window last-window)
     (setq last-window (previous-window
			(w3m-static-if (fboundp 'frame-highest-window)
			    (frame-highest-window)
			  (frame-first-window))))
     (while (minibuffer-window-active-p last-window)
       (setq last-window (previous-window last-window)))
     (while (and
	     (not (one-window-p))
	     (or (< (window-width last-window)
		    (frame-width))
		 (< (window-height last-window)
		    (+ wheight window-min-height))))
       (setq window last-window)
       (setq last-window (previous-window window))
       (delete-window window))
     (select-window (split-window last-window))
     (condition-case nil
	 (shrink-window (- (window-height) wheight))
       (error nil))
     (switch-to-buffer showbuf)
     (setq w3m-session-select-wincfg wincfg)
     (w3m-session-select-mode sessions))))

(defun w3m-session-goto-session (session)
  "Goto URLs."
  (let ((title (nth 0 session))
	(urls (nth 2 session))
	(cnum (nth 3 session))
	(i 0)
	(w3m-async-exec (and w3m-async-exec-with-many-urls w3m-async-exec))
	url cbuf buf pos history)
    (message "Session goto(%s)..." title)
    (while (setq url (car urls))
      (setq urls (cdr urls))
      (unless (stringp url)
	(setq pos     (nth 1 url)
	      history (nth 2 url)
	      url     (nth 0 url)))
      (w3m-goto-url-new-session url)
      (setq buf (car (nreverse (w3m-list-buffers))))
      (when (or (and (numberp cnum) (= cnum i))
		(and (not cnum) (= i 0)))
	(setq cbuf buf))
      (when (and buf pos history)
	(set-buffer buf)
	(setq w3m-history-flat history)
	(w3m-history-tree pos))
      (setq i (1+ i)))
    (when (and cbuf (eq major-mode 'w3m-mode))
      (set-window-buffer (selected-window) cbuf))
    (message "Session goto(%s)...done" title)))

(defun w3m-session-rename (sessions num)
    (if (consp num)
	(message "This command can execute in Main session area")
      (let ((prompt "New session title: ")
	    (overwrite nil)
	    tmp title otitle)
	(setq tmp (nth num sessions))
	(setq otitle (car tmp))
	(setq title otitle)
	(catch 'loop
	  (while t
	    (setq title (read-from-minibuffer prompt otitle))
	    (cond
	     ((string= title "")
	      nil)
	     ((string= title otitle)
	      (when (y-or-n-p
		     (format "\"%s\" is same as original title. Do not rename? "
			     title))
		(throw 'loop t)))
	     ((assoc title sessions)
	      (when (y-or-n-p (format "\"%s\" is exist. Overwrite? " title))
		(setq overwrite t)
		(throw 'loop t))))
	    (setq prompt "Again New session title: ")))
	(when overwrite
	  (setq sessions (delete (assoc title sessions) sessions)))
	(unless (string= title otitle)
	  (setq sessions (delete tmp sessions))
	  (setcar tmp title)
	  (setq sessions (cons tmp sessions))
	  (w3m-save-list w3m-session-file sessions)))))

(defun w3m-session-delete (sessions num)
  (let (tmp)
    (if (consp num)
	(let ((item (nth 2 (nth (car num) sessions))))
	  (setq tmp (delete (nth (cdr num) item)
			    item))
	  (setf (nth 2 (nth (car num) sessions))
		tmp))
      (setq tmp (nth num sessions))
      (setq sessions (delete tmp sessions)))
    (if sessions
	(w3m-save-list w3m-session-file sessions)
      (let ((file (expand-file-name w3m-session-file)))
	(when (and (file-exists-p file)
		   (file-writable-p file))
	  (delete-file file))))))

(defvar w3m-session-menu-items
  `([,(w3m-make-menu-item "新しいセッションを作る..."
			  "Create New Session...")
     w3m-goto-new-session-url t]
    [,(w3m-make-menu-item "このセッションを複製する" "Copy This Session")
     w3m-copy-buffer w3m-current-url]
    "----" ;; separator
    [,(w3m-make-menu-item "前のセッションに移動する"
			  "Move Previous Session")
     w3m-previous-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "次のセッションに移動する" "Move Next Session")
     w3m-next-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    "----" ;; separator
    [,(w3m-make-menu-item "このセッションを閉じる" "Close This Session")
     w3m-delete-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "他のセッションを閉じる" "Close Other Sessions")
     w3m-delete-other-buffers
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "現在のセッションを保存する"
			  "Save Displayed Sessions")
     w3m-session-save t]
    [,(w3m-make-menu-item "セッションを選択する" "Select Sessions")
     w3m-session-select t])
    "*List of the session menu items.")

;;;###autoload
(defun w3m-setup-session-menu ()
  "Setup w3m session items in menubar."
  (w3m-static-if (featurep 'xemacs)
      (unless (car (find-menu-item current-menubar '("Session")))
	(easy-menu-define w3m-session-menu w3m-mode-map
	  "" '("Session" ["(empty)" ignore nil]))
	(easy-menu-add w3m-session-menu)
	(add-hook 'activate-menubar-hook 'w3m-session-menubar-update))
    (unless (lookup-key w3m-mode-map [menu-bar Session])
      (easy-menu-define w3m-session-menu w3m-mode-map "" '("Session"))
      (easy-menu-add w3m-session-menu)
      (add-hook 'menu-bar-update-hook 'w3m-session-menubar-update))))

(defvar w3m-session-menu-items-pre nil)
(defvar w3m-session-menu-items-time nil)

(defun w3m-session-menubar-update ()
  "Update w3m session menubar."
  (when (and (eq major-mode 'w3m-mode)
	     (w3m-static-if (featurep 'xemacs)
		 (frame-property (selected-frame) 'menubar-visible-p)
	       menu-bar-mode))
    (let ((items w3m-session-menu-items)
	  (pages (w3m-session-make-menu-items)))
      (easy-menu-define w3m-session-menu w3m-mode-map
	"The menu kepmap for the emacs-w3m session."
	(cons "Session" (if pages
			     (append items '("----") pages)
			   items)))
      (w3m-static-when (featurep 'xemacs)
	(when (setq items (car (find-menu-item current-menubar '("Session"))))
	  (setcdr items (cdr w3m-session-menu))
	  (set-buffer-menubar current-menubar))))))

(defun w3m-session-file-modtime ()
  "Return the modification time of the session file `w3m-session-file'.
The value is a list of two time values `(HIGH LOW)' if the session
file exists, otherwise nil."
  (nth 5 (file-attributes w3m-session-file)))

(defvar w3m-session-make-item-xmas
  (and (equal "Japanese" w3m-language) (featurep 'xemacs)))

(defun w3m-session-make-item (item)
  (if w3m-session-make-item-xmas
      (concat item "%_ ")
    item))

(defun w3m-session-make-menu-items ()
  "Create w3m session menu items."
  (if (and w3m-session-menu-items-pre
	   w3m-session-menu-items-time
	   (equal w3m-session-menu-items-time
		  (w3m-session-file-modtime)))
      w3m-session-menu-items-pre
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file)))
       (setq w3m-session-menu-items-time (w3m-session-file-modtime))
       (setq w3m-session-menu-items-pre
	     (and sessions
		  (mapcar
		   (lambda (entry)
		     (cons (w3m-session-make-item (car entry))
			   (cons (vector "Open all sessions"
					 `(w3m-session-goto-session
					   (quote ,entry)))
				 (mapcar
				  (lambda (item)
				    (let ((title
					   (w3m-session-make-item
					    (or (nth 3 item)
						w3m-session-unknown-title))))
				      (vector
				       title
				       `(w3m-session-goto-session
					 (quote
					  ,(list title
						 nil
						 (list item)
						 nil))))))
				  (nth 2 entry)))))
		   sessions)))))))

;;;###autoload
(defun w3m-session-last-autosave-session ()
  (when w3m-session-load-last-sessions
    (w3m-session-ignore-errors
     (let ((item
	    (let ((sessions (w3m-load-list w3m-session-file))
		  (n 1) x)
	      (catch 'loop
		(while t
		  (if (< w3m-session-automatic-keep-number n)
		      (throw 'loop nil)
		    (setq x (assoc (format "%s-%d"
					   w3m-session-automatic-title n)
				   sessions))
		    (when x (throw 'loop x)))
		  (setq n (1+ n)))))))
       (when (and item
		  (or (and (eq w3m-session-load-last-sessions 'ask)
			   (y-or-n-p "Load the last sessions? "))
		      w3m-session-load-last-sessions))
	 item)))))

;;;###autoload
(defun w3m-session-last-crashed-session ()
  (when (and w3m-session-crash-recovery w3m-session-load-crashed-sessions)
    (w3m-session-ignore-errors
     (let ((item (assoc w3m-session-crash-recovery-title
			(w3m-load-list w3m-session-file))))
       (when (and item
		  (or (and (eq w3m-session-load-crashed-sessions 'ask)
			   (y-or-n-p "Load the crashed sessions? "))
		      (eq w3m-session-load-crashed-sessions t)))
	 item)))))

(provide 'w3m-session)

;;; w3m-session.el ends here
