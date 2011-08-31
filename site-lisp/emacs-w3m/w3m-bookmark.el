;;; w3m-bookmark.el --- Functions to operate bookmark file of w3m

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

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

;; w3m-bookmark.el is the add-on program of emacs-w3m to operate
;; bookmark file.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m-util)
(require 'w3m)
(require 'easymenu)

(defcustom w3m-bookmark-file
  (expand-file-name "bookmark.html" w3m-profile-directory)
  "Bookmark file of w3m."
  :group 'w3m
  :type '(file :size 0))

(defcustom w3m-bookmark-file-coding-system 'euc-japan
  "Coding system for a created bookmark file.
This option is used when a new bookmark file is created, or when an
existing bookmark file includes ASCII characters only.  If the coding
system which is used to encode your using bookmark file is different
from the value of this option, emacs-w3m does not change the encoding
of your bookmark file."
  :group 'w3m
  :type '(coding-system :size 0))

(defcustom w3m-bookmark-default-section
  nil
  "Default section to add new entry."
  :group 'w3m
  :type '(radio (const :tag "Not specified" nil)
		(string :format "Default section name: %v\n" :size 0)))

(defcustom w3m-bookmark-mode-hook nil
  "*Hook run at the end of function `w3m-bookmark-mode'."
  :group 'w3m
  :type 'hook)

(defcustom w3m-bookmark-menu-open-new-session nil
  "If non-nil, \"Bookmark\" menu item open new session."
  :group 'w3m
  :type 'boolean)

(eval-and-compile
  (defconst w3m-bookmark-section-delimiter
    "<!--End of section (do not delete this comment)-->\n"))

(eval-and-compile
  (defconst w3m-bookmark-section-format
    (eval-when-compile
      (concat "<h2>%s</h2>\n<ul>\n"
	      "<li><a href=\"%s\">%s</a>\n"
	      w3m-bookmark-section-delimiter
	      "</ul>\n"))))

(defconst w3m-bookmark-initial-format
  (eval-when-compile
    (concat "<html><head><title>Bookmarks</title></head>\n"
	    "<body>\n<h1>Bookmarks</h1>\n"
	    w3m-bookmark-section-format
	    "</body>\n</html>\n")))

(defvar w3m-bookmark-section-history nil)
(defvar w3m-bookmark-title-history nil)

(defvar w3m-bookmark-buffer-file-name nil
  "Non-nil means that `w3m-bookmark-file' has been loaded to this buffer.")
(make-variable-buffer-local 'w3m-bookmark-buffer-file-name)

(defvar w3m-bookmark-mode-map
  (let ((map (make-sparse-keymap))
	(table '((kill-line . w3m-bookmark-kill-entry)
		 (undo . w3m-bookmark-undo))))
    (dolist (pair table)
      (substitute-key-definition (car pair) (cdr pair) map global-map))
    (substitute-key-definition 'w3m-edit-current-url 'w3m-bookmark-edit
			       map w3m-mode-map)
    map)
  "*Keymap for `w3m-bookmark-mode'.")

(defvar w3m-bookmark-mode nil "Non-nil if w3m bookmark mode is enabled.")
(make-variable-buffer-local 'w3m-bookmark-mode)
(unless (assq 'w3m-bookmark-mode minor-mode-alist)
  (push (list 'w3m-bookmark-mode " bookmark") minor-mode-alist))
(unless (assq 'w3m-bookmark-mode minor-mode-map-alist)
  (push (cons 'w3m-bookmark-mode w3m-bookmark-mode-map) minor-mode-map-alist))

(defun w3m-bookmark-mode (&optional arg)
  "\\<w3m-bookmark-mode-map>
Minor mode to edit bookmark.

\\[w3m-bookmark-kill-entry]	Kill the current entry of this bookmark.
\\[w3m-bookmark-undo]	Undo some previous changes on this bookmark.
\\[w3m-bookmark-edit]	Open `w3m-bookmark-file'.
"
  (interactive "P")
  (when (setq w3m-bookmark-mode
	      (if arg
		  (> (prefix-numeric-value arg) 0)
		(not w3m-bookmark-mode)))
    (run-hooks 'w3m-bookmark-mode-hook)))

(defun w3m-bookmark-mode-setter (url)
  "Activate `w3m-bookmark-mode', when visiting page shows bookmark."
  (w3m-bookmark-mode (if (string-match "\\`about://bookmark/" url)
			 (progn
			   (setq default-directory
				 (file-name-directory w3m-bookmark-file))
			   1)
		       0)))
(add-hook 'w3m-display-functions 'w3m-bookmark-mode-setter)

(defun w3m-bookmark-file-modtime ()
  "Return the modification time of the bookmark file `w3m-bookmark-file'.
The value is a list of two time values `(HIGH LOW)' if the bookmark
file exists, otherwise nil."
  (nth 5 (file-attributes w3m-bookmark-file)))

(defun w3m-bookmark-buffer (&optional no-verify-modtime)
  "Return the buffer reading `w3m-bookmark-file' current."
  (let ((buffer (get-file-buffer w3m-bookmark-file)))
    (if buffer
	;; When a buffer visiting `w3m-bookmark-file' is found, return
	;; it instead of a working buffer.  In this case, kill the
	;; working buffer which was generated in the past.
	(progn (w3m-kill-buffer " *w3m bookmark*") buffer)
      ;; Generate a working buffer.
      (with-current-buffer (w3m-get-buffer-create " *w3m bookmark*")
	(unless (and w3m-bookmark-buffer-file-name
		     (or no-verify-modtime
			 (equal (w3m-visited-file-modtime)
				(w3m-bookmark-file-modtime))))
	  (when (file-readable-p w3m-bookmark-file)
	    (buffer-disable-undo)
	    (erase-buffer)
	    (let ((coding-system-for-read 'binary))
	      (insert-file-contents w3m-bookmark-file))
	    (w3m-decode-buffer
	     (w3m-expand-file-name-as-url w3m-bookmark-file))
	    (set-buffer-file-coding-system
	     (if (memq w3m-current-coding-system
		       '(undecided undecided-dos undecided-mac undecided-unix))
		 w3m-bookmark-file-coding-system
	       w3m-current-coding-system))
	    (set-buffer-modified-p nil))
	  (setq w3m-bookmark-buffer-file-name w3m-bookmark-file)
	  (set-visited-file-modtime (or (w3m-bookmark-file-modtime)
					;; No bookmark file.
					(with-temp-buffer
					  ;; 0 in Emacs; (0 . 0) in XEmacs
					  (visited-file-modtime))))
	  (buffer-enable-undo))
	(current-buffer)))))

(defun w3m-bookmark-verify-modtime ()
  (unless (equal (w3m-visited-file-modtime)
		 (w3m-bookmark-file-modtime))
    (if (buffer-file-name)
	(ask-user-about-supersession-threat w3m-bookmark-file)
      (let ((modified (buffer-modified-p))
	    (name (buffer-name)))
	(unwind-protect
	    (progn
	      (set-visited-file-name w3m-bookmark-file)
	      (ask-user-about-supersession-threat w3m-bookmark-file))
	  (set-visited-file-name nil)
	  (rename-buffer name)
	  (set-buffer-modified-p modified))))))

(defun w3m-bookmark-sections ()
  "Return collection of registered sections."
  (let (sections)
    (with-current-buffer (w3m-bookmark-buffer)
      (goto-char (point-min))
      (while (search-forward "<h2>" nil t)
	(push (cons (buffer-substring-no-properties
		     (point)
		     (if (search-forward "</h2>" nil t)
			 (match-beginning 0)
		       (point-at-eol)))
		    nil)
	      sections)))
    (nreverse sections)))

(defun w3m-bookmark-save-buffer ()
  "Save this current buffer to `w3m-bookmark-file'."
  (cond
   ((buffer-file-name)
    (basic-save-buffer))
   ((buffer-modified-p)
    (let ((backup-info (find-backup-file-name w3m-bookmark-file))
	  (modes (when (file-exists-p w3m-bookmark-file)
		   (file-modes w3m-bookmark-file))))
      (when (and modes ; means that `w3m-bookmark-file' exists.
		 make-backup-files
		 (funcall backup-enable-predicate w3m-bookmark-file))
	(rename-file w3m-bookmark-file (car backup-info) t))
      (write-region (point-min) (point-max) w3m-bookmark-file)
      (when modes
	(set-file-modes w3m-bookmark-file modes))
      (set-visited-file-modtime (w3m-bookmark-file-modtime))
      (set-buffer-modified-p nil)
      (dolist (file (cdr backup-info))
	(condition-case ()
	    (delete-file file)
	  (file-error nil)))))))

(defun w3m-bookmark-safe-string (string format)
  (labels ((filter (s c) (decode-coding-string (encode-coding-string s c) c)))
    (if (let ((encoding (w3m-static-when (featurep 'mule)
			  buffer-file-coding-system)))
	  (or (string= string (filter string encoding))
	      (when w3m-use-mule-ucs
		(string= (setq string
			       (filter string
				       (if w3m-accept-japanese-characters
					   'w3m-euc-japan
					 'w3m-iso-latin-1)))
			 (filter string encoding)))))
	string
      (error format string))))

(defun w3m-bookmark-write-file (url title section)
  "Make new bookmark with specified spec, and save it."
  (with-current-buffer (w3m-bookmark-buffer)
    (setq title (w3m-bookmark-safe-string
		 title
		 "Specified title includes unsafe character(s): %s")
	  section (w3m-bookmark-safe-string
		   section
		   "Specified section includes unsafe character(s): %s"))
    (if (zerop (buffer-size))
	;; New bookmark file.
	(progn
	  (insert (format w3m-bookmark-initial-format section url title))
	  (set-buffer-file-coding-system w3m-bookmark-file-coding-system))
      (goto-char (point-min))
      (if (search-forward (format "<h2>%s</h2>" section) nil t)
	  (progn
	    (unless (search-forward w3m-bookmark-section-delimiter nil t)
	      (error "Can't find section delimiter: %s" section))
	    (goto-char (match-beginning 0))
	    (insert (format "<li><a href=\"%s\">%s</a>\n" url title)))
	;; New section.
	(unless (search-forward "</body>\n" nil t)
	  (error "%s" "Can't find terminator of bookmark"))
	(goto-char (match-beginning 0))
	(insert (format w3m-bookmark-section-format
			section url title))))
    (w3m-bookmark-save-buffer)))

(defun w3m-bookmark-add (url &optional title)
  "Add URL to bookmark.
Optional argument TITLE is title of link."
  (let ((section (completing-read
		  (if w3m-bookmark-default-section
		      (format "Section (default %s): "
			      w3m-bookmark-default-section)
		    "Section: ")
		  (w3m-bookmark-sections) nil nil nil
		  'w3m-bookmark-section-history)))
    (and (string= section "")
	 (setq section w3m-bookmark-default-section))
    (when (or (not section)
	      (string-match section "^ *$"))
      (error "%s" "You must specify section name"))
    (setq title (read-string "Title: " title 'w3m-bookmark-title-history))
    (when (or (not title)
	      (string-match title "^ *$"))
      (error "%s" "You must specify title"))
    (w3m-bookmark-write-file url
			     (w3m-encode-specials-string title)
			     (w3m-encode-specials-string section))))

;;;###autoload
(defun w3m-bookmark-add-this-url ()
  "Add link under cursor to bookmark."
  (interactive)
  (if (null (w3m-anchor))
      (message "No anchor")		; nothing to do
    (let ((url (w3m-anchor))
	  (title (buffer-substring-no-properties
		  (previous-single-property-change (1+ (point))
						   'w3m-href-anchor)
		  (next-single-property-change (point) 'w3m-href-anchor))))
      (w3m-bookmark-add url title))
    (message "Added")))

;;;###autoload
(defun w3m-bookmark-add-current-url (&optional arg)
  "Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one."
  (interactive "P")
  (w3m-bookmark-add (if arg (w3m-input-url) w3m-current-url)
		    w3m-current-title)
  (message "Added"))

;;;###autoload
(defun w3m-bookmark-add-all-urls ()
  "Add urls of all pages being visited to the bookmark."
  (interactive)
  (let ((buffers (w3m-list-buffers)))
    (if (and w3m-use-tab
	     (>= (length buffers) 2))
	(while buffers
	  (switch-to-buffer (pop buffers))
	  (condition-case nil
	      (w3m-bookmark-add-current-url)
	    (quit)))
      (message
       "Use the `%s' command instead"
       (key-description (car (where-is-internal 'w3m-bookmark-add-current-url
						w3m-mode-map)))))))

;;;###autoload
(defun w3m-bookmark-add-current-url-group ()
  "Add link of the group of current urls to the bookmark."
  (interactive)
  (w3m-bookmark-add
   (concat "group:"
	   (mapconcat
	    'w3m-url-encode-string
	    (mapcar (lambda (buffer)
		      (with-current-buffer buffer w3m-current-url))
		    (w3m-list-buffers))
	    "&")))
  (message "Added as URL group"))

;;;###autoload
(defun w3m-bookmark-view (&optional reload)
  "Display the bookmark."
  (interactive "P")
  (if (file-exists-p w3m-bookmark-file)
      (progn
	;; Store the current position in the history structure.
	(w3m-history-store-position)
	(w3m-goto-url "about://bookmark/" reload))
    (message "No bookmark file is available")))

;;;###autoload
(defun w3m-bookmark-view-new-session (&optional reload)
  "Display the bookmark on a new session."
  (interactive "P")
  (if (not (eq major-mode 'w3m-mode))
      (message "This command can be used in w3m mode only")
    (if (file-exists-p w3m-bookmark-file)
	(w3m-view-this-url-1 "about://bookmark/" reload 'new-session)
      (message "No bookmark file is available"))))

;;;###autoload
(defun w3m-about-bookmark (&rest args)
  (insert-buffer-substring (w3m-bookmark-buffer))
  (let ((ident) (i 0) (j 0))
    (goto-char (point-min))
    (while (search-forward (setq ident (format "w3mbk%d." i)) nil t)
      (incf i))
    (setq i 0)
    (goto-char (point-min))
    (while (re-search-forward "\n<\\(?:h2\\|\\(li\\)\\)>" nil t)
      (forward-char -1)
      (insert (if (match-beginning 1)
		  (format " id=\"%s%d.%d\"" ident i (incf j))
		(format " id=\"%s%d\"" ident (incf i))))))
  "text/html")

(defun w3m-bookmark-current-number ()
  "Return the ordinal number of the current bookmark entry."
  (let ((x (car (get-text-property (point-at-eol) 'w3m-name-anchor))))
    (and x
	 (string-match "\\`w3mbk[0-9]+\\.[0-9]+\\.\\([0-9]+\\)\\'" x)
	 (string-to-number (match-string 1 x)))))

(defun w3m-bookmark-kill-entry (num)
  "Kill the bookmark entry of the current line.
With prefix argument, kill that many entries from point."
  (interactive "p")
  (let ((entries (w3m-bookmark-current-number)))
    (when entries
      (setq entries (list entries))
      (while (> (decf num) 0)
	(push (1+ (car entries)) entries))
      (condition-case nil
	  (w3m-bookmark-kill-entries entries)
	(file-supersession nil))
      (w3m-bookmark-view t))))

(defun w3m-bookmark-kill-entries (entries)
  (with-current-buffer (w3m-bookmark-buffer t)
    (w3m-bookmark-verify-modtime)
    (goto-char (point-min))
    (let ((i 0))
      (while (search-forward "\n<li>" nil t)
	(when (memq (incf i) entries)
	  (let ((beg (point-at-bol))
		(end (progn
		       (search-forward w3m-bookmark-section-delimiter)
		       (match-beginning 0))))
	    (delete-region (goto-char beg)
			   (if (search-forward "\n<li>" end t)
			       (point-at-bol)
			     end))
	    (goto-char (1- beg))))))
    (w3m-bookmark-save-buffer)))

(defun w3m-bookmark-undo (&optional arg)
  "Undo some previous changes on bookmark."
  (interactive "p")
  (condition-case nil
      (with-current-buffer (w3m-bookmark-buffer t)
	(w3m-bookmark-verify-modtime)
	(undo arg)
	(w3m-bookmark-save-buffer))
    (file-supersession nil))
  (w3m-bookmark-view t))

(defun w3m-bookmark-edit ()
  "Edit the bookmark file."
  (interactive)
  (w3m-edit-url (w3m-expand-file-name-as-url w3m-bookmark-file)))

;; Bookmark menu
(defvar w3m-bookmark-menu-items
  (let ((etsu (when w3m-use-japanese-menu
		(decode-coding-string "\e$B1\\\e(B" 'iso-2022-jp)))) ;; 閲
    `(([,(w3m-make-menu-item (concat "ブックマークの" etsu "覧") "View Bookmark")
	w3m-bookmark-view t]
       [,(w3m-make-menu-item (concat "新セッションでブックマークの" etsu "覧")
			     "View Bookmark in a New Session")
	w3m-bookmark-view-new-session t]
       [,(w3m-make-menu-item "ブックマークの編集" "Edit Bookmark")
	w3m-bookmark-edit t]
       "----"
       [,(w3m-make-menu-item "このページをブックマーク" "Add Current URL to Bookmark")
	w3m-bookmark-add-current-url t]
       [,(w3m-make-menu-item "すべての URL をブックマーク" "Add These URLs to Bookmark")
	w3m-bookmark-add-current-url-group t]
       [,(w3m-make-menu-item "この URL をブックマーク" "Add This URL to Bookmark")
	w3m-bookmark-add-this-url (w3m-anchor)])
      .
      ([,(w3m-make-menu-item "このエントリを消去" "Kill Current Entry")
	w3m-bookmark-kill-entry
	(text-property-not-all (point-at-bol) (point-at-eol)
			       'w3m-href-anchor nil)]
       [,(w3m-make-menu-item "もとに戻す" "Undo")
	w3m-bookmark-undo t]
       [,(w3m-make-menu-item "ブックマークの編集" "Edit Bookmark")
	w3m-bookmark-edit t])))
  "*List of the bookmark menu items.
The car is used if `w3m-bookmark-mode' is nil, otherwise the cdr is used.")

;;;###autoload
(defun w3m-setup-bookmark-menu ()
  "Setup w3m bookmark items in menubar."
  (w3m-static-if (featurep 'xemacs)
      (unless (car (find-menu-item current-menubar '("Bookmark")))
	(easy-menu-define w3m-bookmark-menu w3m-mode-map
	  "" '("Bookmark" ["(empty)" ignore nil]))
	(easy-menu-add w3m-bookmark-menu)
	(add-hook 'activate-menubar-hook 'w3m-bookmark-menubar-update))
    (unless (lookup-key w3m-mode-map [menu-bar Bookmark])
      (easy-menu-define w3m-bookmark-menu w3m-mode-map "" '("Bookmark"))
      (easy-menu-add w3m-bookmark-menu)
      (add-hook 'menu-bar-update-hook 'w3m-bookmark-menubar-update))))

(defun w3m-bookmark-menubar-update ()
  "Update w3m bookmark menubar."
  (when (and (eq major-mode 'w3m-mode)
	     (w3m-static-if (featurep 'xemacs)
		 (frame-property (selected-frame) 'menubar-visible-p)
	       menu-bar-mode))
    (let ((items (if w3m-bookmark-mode
		     (cdr w3m-bookmark-menu-items)
		   (car w3m-bookmark-menu-items)))
	  (pages (w3m-bookmark-make-menu-items)))
      (easy-menu-define w3m-bookmark-menu w3m-mode-map
	"The menu kepmap for the emacs-w3m bookmark."
	(cons "Bookmark" (if pages
			     (append items '("----") pages)
			   items)))
      (w3m-static-when (featurep 'xemacs)
	(when (setq items (car (find-menu-item current-menubar '("Bookmark"))))
	  (setcdr items (cdr w3m-bookmark-menu))
	  (set-buffer-menubar current-menubar))))))

(defun w3m-bookmark-iterator ()
  "Iteration bookmark groups/entries.
Format as (list (\"Group name\" . (\"Entry URL\" . \"Entry name\")* )* )."
  (let ((entries nil))
    (with-current-buffer (w3m-bookmark-buffer)
      (goto-char (point-min))
      (let (group entry beg end)
	(while (re-search-forward "<h2>\\([^<]+\\)</h2>" nil t)
	  (setq group (match-string-no-properties 1))
	  (setq beg (match-beginning 0))
	  (setq end (re-search-forward "</ul>" nil t))
	  (save-excursion
	    (let (urls)
	      (narrow-to-region beg end)
	      (goto-char (point-min))
	      (while (re-search-forward
		      "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>"
		      nil t)
		(push (cons (match-string-no-properties 1)
			    (match-string-no-properties 2))
		      urls))
	      (setq entry (cons group (nreverse urls)))
	      (push entry entries)
	      (widen)))
	  (goto-char (match-end 0))))
      (nreverse entries))))

(defun w3m-bookmark-menu-open-item (url)
  "Open URL at current/new buffer"
  (if w3m-bookmark-menu-open-new-session
      (w3m-goto-url-new-session url)
    (w3m-goto-url url)))

(defvar w3m-bookmark-menu-items-pre nil)
(defvar w3m-bookmark-menu-items-time nil)

(defvar w3m-bookmark-make-item-xmas
  (and (equal "Japanese" w3m-language) (featurep 'xemacs)))

(defun w3m-bookmark-make-item (item)
  (if w3m-bookmark-make-item-xmas
      (concat item "%_ ")
    item))

(defun w3m-bookmark-make-menu-items (&optional nomenu)
  "Create w3m bookmark menu items."
  (when (not nomenu)
    (if (and w3m-bookmark-menu-items-pre
	     w3m-bookmark-menu-items-time
	     (equal w3m-bookmark-menu-items-time
		    (w3m-bookmark-file-modtime)))
	w3m-bookmark-menu-items-pre
      (setq w3m-bookmark-menu-items-time (w3m-bookmark-file-modtime))
      (let ((entries (when (file-exists-p w3m-bookmark-file)
		       (w3m-bookmark-iterator))))
	(setq w3m-bookmark-menu-items-pre
	      (and entries
		   (mapcar
		    (lambda (entry)
		      (let ((group (car entry))
			    (items (cdr entry)))
			(cons (w3m-bookmark-make-item group)
			      (and items
				   (mapcar
				    (lambda (item)
				      (vector
				       (w3m-bookmark-make-item (cdr item))
				       `(w3m-bookmark-menu-open-item
					 ,(car item))))
				    items)))))
		    entries)))))))

(provide 'w3m-bookmark)

;;; w3m-bookmark.el ends here
