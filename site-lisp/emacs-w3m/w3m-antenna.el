;;; w3m-antenna.el --- Utility to detect changes of WEB

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; w3m-antenna.el is the add-on utility of emacs-w3m to detect changes
;; of WEB pages.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m-util)
(require 'w3m-rss)
(require 'w3m)

(defgroup w3m-antenna nil
  "w3m-antenna - Utility to detect changes of WEB."
  :group 'w3m
  :prefix "w3m-antenna-")

(define-widget 'w3m-antenna-string 'string
  "String widget with default value.
When creating a new widget, its value is given by an expression specified
with :value-from."
  :tag "URL"
  :value-from nil
  :create 'w3m-antenna-string-create)

(defun w3m-antenna-string-create (widget)
  (if (string= "" (widget-get widget :value))
      ;; No value is given.
      (widget-put widget :value
		  (let* ((symbol (widget-get widget :value-from))
			 (value  (eval symbol)))
		    (if value
			(set symbol nil)
		      (setq value ""))
		    value)))
  (widget-default-create widget))

(eval-when-compile
  ;; Compiler warning in Emacs 19.
  (autoload 'widget-default-get "wid-edit"))

(apply 'define-widget 'w3m-antenna-function 'function
       "Bug-fixed version of the `function' widget.
In Emacs 20.7 through 21.4 and XEmacs, it doesn't represent a value as
a string internally, converts it into a string in the customization
buffer, and provides the default value as `ignore'."
       (if (and (fboundp 'widget-default-get)
		(widget-default-get
		 '(function :value-to-external ignore :value foo)))
	   '(:value-create
	     (lambda (widget)
	       (widget-put widget :value
			   (widget-sexp-value-to-internal widget value))
	       (widget-field-value-create widget))
	     :value-to-internal
	     (lambda (widget value) value)
	     :value ignore)))

(defvar w3m-antenna-alist nil
  "A list of site information (internal variable).  nil means that
antenna database is not initialized.  Each site information is a list
that consists of:
 0. Format string of URL.
 1. Title.
 2. Class (Normal, HNS or TIME).
 3. Real URL.
 4. Last modification time.
 5. Size in bytes.
 6. Time when size modification is detected.
")

(defmacro w3m-antenna-site-key (site)
  `(car ,site))
(defmacro w3m-antenna-site-title (site)
  `(nth 1 ,site))
(defmacro w3m-antenna-site-class (site)
  `(nth 2 ,site))
(defmacro w3m-antenna-site-url (site)
  `(nth 3 ,site))
(defmacro w3m-antenna-site-last-modified (site)
  `(nth 4 ,site))
(defmacro w3m-antenna-site-size (site)
  `(nth 5 ,site))
(defmacro w3m-antenna-site-size-detected (site)
  `(nth 6 ,site))

(defcustom w3m-antenna-file
  (expand-file-name ".antenna" w3m-profile-directory)
  "File which has list of antenna URLs."
  :group 'w3m-antenna
  :type '(file :size 0))

(defcustom w3m-antenna-refresh-interval nil
  "Interval time to update (to refresh) the antenna page automatically.
The value should be a positive integer in seconds, or nil which means
not to update the page."
  :group 'w3m-antenna
  :type '(choice
	  (const :tag "Not reload." nil)
	  (integer :tag "Interval second.")))

(defcustom w3m-antenna-sites
  (unless noninteractive
    (mapcar (lambda (site)
	      (list (w3m-antenna-site-key site)
		    (w3m-antenna-site-title site)
		    (w3m-antenna-site-class site)))
	    (w3m-load-list w3m-antenna-file)))
  "List of WEB sites, watched by `w3m-antenna'."
  :group 'w3m-antenna
  :type `(repeat
	  (group
	   :indent 7
	   (w3m-antenna-string :format "URL: %v\n" :size 0
			       :value-from w3m-antenna-tmp-url)
	   (w3m-antenna-string :format "Title: %v\n" :size 0
			       :value-from w3m-antenna-tmp-title)
	   (choice
	    :tag "Procedure"
	    (const :tag "Check either its last modified time or its size" nil)
	    (const :tag "Check its last modified time only" time)
	    (const :tag "Check its current date provided by Hyper Nikki System"
		   hns)
	    (list :tag "Check RSS"
		  (function-item :format "" w3m-antenna-check-rss)
		  (string :format "URL: %v\n" :value ""))
	    (list :tag "Check the another changelog page"
		  (function-item :format "" w3m-antenna-check-another-page)
		  (string :format "URL: %v\n" :value ""))
	    (list :tag "Check the page linked by the anchor that matches"
		  (function-item :format "" w3m-antenna-check-anchor)
		  (regexp :value "")
		  (integer :value 0))
	    (cons :tag "Check with a user defined function"
		  (w3m-antenna-function
		   :match (lambda (widget value)
			    (and (functionp value)
				 (not (memq value
					    '(w3m-antenna-check-rss
					      w3m-antenna-check-another-page
					      w3m-antenna-check-anchor))))))
		  (repeat :tag "Arguments" sexp))))))

(defcustom w3m-antenna-html-skelton
  (eval-when-compile
    (concat "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	    "<html>\n<head>\n<title>Antenna</title>\n%R</head>\n<body>\n"
	    "<h1>Antenna</h1>\n<p align=\"right\">Checked at %D.</p>\n"
	    "<h2>Updated</h2>\n<ul>\n%C</ul>\n"
	    "<h2>Visited</h2>\n<ul>\n%U</ul>\n"
	    "</body>\n</html>\n"))
  "HTML skelton of antenna."
  :group 'w3m-antenna
  :type 'string)

(defcustom w3m-antenna-make-summary-function
  'w3m-antenna-make-summary-like-natsumican
  "Function to make summary of site information."
  :group 'w3m-antenna
  :type '(choice
	  :format "%{%t%}:\n %[Value Menu%] %v"
	  (function-item :tag "Simple style." w3m-antenna-make-summary)
	  (function-item :tag "Natsumican style."
			 w3m-antenna-make-summary-like-natsumican)
	  (function :format "User function: %v\n" :size 0)))

(defcustom w3m-antenna-sort-changed-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of changed sites."
  :group 'w3m-antenna
  :type '(choice
	  :format "%{%t%}:\n %[Value Menu%] %v"
	  (function-item :tag "Sort by last modification time."
			 w3m-antenna-sort-sites-by-time)
	  (function-item :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (function-item :tag "Do nothing." identity)
	  (function :format "User function: %v\n" :size 0)))

(defcustom w3m-antenna-sort-unchanged-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of unchanged sites."
  :group 'w3m-antenna
  :type '(choice
	  :format "%{%t%}:\n %[Value Menu%] %v"
	  (function-item :tag "Sort by last modification time."
			 w3m-antenna-sort-sites-by-time)
	  (function-item :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (function-item :tag "Do nothing." identity)
	  (function :format "User function: %v\n" :size 0)))

(defun w3m-antenna-alist ()
  (let ((alist (w3m-load-list w3m-antenna-file)))
    (mapcar (lambda (site)
	      (let ((l (assoc (w3m-antenna-site-key site) alist)))
		(if l
		    (progn
		      (setf (w3m-antenna-site-class l)
			    (w3m-antenna-site-class site))
		      l)
		  (append site (list nil nil nil nil)))))
	    w3m-antenna-sites)))

(defun w3m-antenna-hns-last-modified (url handler)
  (w3m-process-do-with-temp-buffer
      (type (w3m-retrieve (w3m-expand-url "di.cgi" url) nil t nil nil handler))
    (when type
      (or (let (start str)
	    ;; Process a line such as "Tue, 27 Mar 2001 12:43:16 GMT<br>".
	    (goto-char (point-min))
	    (and
	     (search-forward "\nLast-Modified: " nil t)
	     (setq start (match-end 0))
	     (search-forward "<br>" nil t)
	     (setq str (buffer-substring start (match-beginning 0)))
	     ;; Ignore format such as "2001, 27 03 GMT", which is used
	     ;; by old HNS.
	     (not (string-match
		   " *[0-9][0-9][0-9][0-9], +[0-9][0-9] +[0-9][0-9] +" str))
	     (w3m-time-parse-string str)))
	  (progn
	    ;; Process a line such as "newest day is 2001/03/15".
	    (goto-char (point-min))
	    (and
	     (re-search-forward "\
^newest day is \\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)$"
				nil t)
	     (encode-time 0 0 0
			  (string-to-number (match-string 3))
			  (string-to-number (match-string 2))
			  (string-to-number (match-string 1))
			  32400)))))))

(defun w3m-antenna-check-hns (site handler)
  "Check the page served by HNS (Hyper Nikki System) asynchronously."
  (lexical-let ((site site))
    (w3m-process-do
	(time
	 (w3m-antenna-hns-last-modified (w3m-antenna-site-key site) handler))
      (if time
	  (w3m-antenna-site-update site (w3m-antenna-site-key site) time nil)
	(w3m-antenna-check-page site handler)))))

(defun w3m-antenna-check-rss (site handler url)
  "Check RSS to detect change of SITE asynchronously.
In order to use this function, `xml.el' is required."
  (lexical-let ((url url)
		(site site))
    (w3m-process-do-with-temp-buffer
	(type (w3m-retrieve url nil t nil nil handler))
      (let (link date dates)
	(when type
	  (w3m-decode-buffer url)
	  (let* ((xml (ignore-errors
			(xml-parse-region (point-min) (point-max))))
		 (dc-ns (w3m-rss-get-namespace-prefix
			 xml "http://purl.org/dc/elements/1.1/"))
		 (rss-ns (w3m-rss-get-namespace-prefix
			  xml "http://purl.org/rss/1.0/"))
		 (channel (car (w3m-rss-find-el
				(intern (concat rss-ns "channel"))
				xml)))
		 (items (w3m-rss-find-el
			 (intern (concat rss-ns "item"))
			 xml)))
	    (setq link (nth 2 (car (w3m-rss-find-el
				    (intern (concat rss-ns "link"))
				    channel))))
	    (setq dates (append
			 (w3m-rss-find-el
			  (intern (concat dc-ns "date"))
			  channel)
			 (w3m-rss-find-el
			  (intern (concat dc-ns "date"))
			  items)
			 (w3m-rss-find-el 'pubDate channel)
			 (w3m-rss-find-el 'pubDate items)))
	    (when dates
	      ;; Ignore future entries to display site announcements.
	      (let ((now (current-time)))
		(let ((low (+ (nth 1 now) 3600))) ; 3600 = clock skew margin
		  (setq now
			(if (>= low 65536)
			    (list (1+ (car now))
				  (- low 65536)
				  (nth 2 now))
			  (list (car now)
				low
				(nth 2 now)))))
		(setq date '(0 0))
		(dolist (tmp dates)
		  (setq tmp (w3m-rss-parse-date-string (nth 2 tmp)))
		  (and (w3m-time-newer-p tmp date)
		       (w3m-time-newer-p now tmp)
		       (setq date tmp)))))))
	(if (and link date)
	    (w3m-antenna-site-update site link date nil)
	  (w3m-antenna-check-page site handler))))))

(defun w3m-antenna-check-another-page (site handler url)
  "Check the another page to detect change of SITE asynchronously.
This function checks the another page specified by the URL before
checking the SITE itself.  This function is useful when the SITE's
owner either maintains the page which describes the change of the
SITE."
  (lexical-let ((site site))
    (w3m-process-do-with-temp-buffer
	(time (w3m-last-modified url t handler))
      (if time
	  (w3m-antenna-site-update site (w3m-antenna-site-key site) time nil)
	(w3m-antenna-check-page site handler)))))

(defun w3m-antenna-check-anchor (site handler regexp number)
  "Check the page linked from SITE asynchronously.
This function checks the page linked by an anchor that matches REGEXP
from the page that is specified by SITE's key attribute."
  (lexical-let ((site site)
		(regexp regexp)
		(number (or number 0)))
    (w3m-process-do-with-temp-buffer
	(type (w3m-retrieve (w3m-antenna-site-key site)
			    nil nil nil nil handler))
      (w3m-antenna-check-page site
			      handler
			      (when type
				(w3m-decode-buffer (w3m-antenna-site-key site))
				(goto-char (point-min))
				(when (re-search-forward regexp nil t)
				  (w3m-expand-url
				   (match-string number)
				   (w3m-antenna-site-key site))))))))

;; To avoid byte-compile warning.
(eval-and-compile
  (autoload 'w3m-filter "w3m-filter"))

(defun w3m-antenna-check-page (site handler &optional url)
  "Check SITE with the generic procedure.
It consists of 3 steps:
\(1\) Check the time when the SITE was last modified with HEAD request.
\(2\) Check the size of the SITE with HEAD request.
\(3\) Get the real content of the SITE, and check its size.
"
  (lexical-let ((site site)
		(url (or url
			 (w3m-antenna-site-url site)
			 (w3m-antenna-site-key site))))
    (w3m-process-do
	(attr (w3m-attributes url t handler))
      (when attr
	(if (nth 4 attr)	; Use the value of Last-modified header.
	    (w3m-antenna-site-update site url (nth 4 attr) (nth 2 attr))
	  (unless (eq 'time (w3m-antenna-site-class site))
	    (if (nth 2 attr)	; Use the value of Content-Length header.
		(w3m-antenna-site-update site url nil (nth 2 attr))
	      ;; Get the real content of the SITE, and calculate its size.
	      (w3m-process-do-with-temp-buffer
		  (type (w3m-retrieve url nil t nil nil handler))
		(when type
		  (w3m-decode-buffer url nil type)
		  (w3m-remove-comments)
		  (when w3m-use-filter
		    (w3m-filter url))
		  (w3m-antenna-site-update site url nil (buffer-size)))))))))))

(defun w3m-antenna-site-update (site url time size)
  "Update SITE's status information with specified TIME and SIZE."
  ;; (w3m-antenna-site-size-detected site) keeps the time when SITE's
  ;; size attribute is checked.
  (setf (w3m-antenna-site-size-detected site)
	(when size
	  (or (when (and url
			 (w3m-antenna-site-url site)
			 (string= url (w3m-antenna-site-url site))
			 (w3m-antenna-site-size site)
			 (= size (w3m-antenna-site-size site)))
		(w3m-antenna-site-size-detected site))
	      (current-time))))
  (setf (w3m-antenna-site-url site) url)
  (setf (w3m-antenna-site-last-modified site) time)
  (setf (w3m-antenna-site-size site) size)
  site)

(defun w3m-antenna-check-site (site handler)
  "Check SITE asynchronously.
If a class attribute of the SITE is a list that consists of a function
to check SITE and its options, call it.  When a class attribute of the
SITE is equal to the symbol `hns', call `w3m-antenna-check-hns'.
Otherwise, call `w3m-antenna-check-page'."
  (if (and (listp (w3m-antenna-site-class site))
	   (functionp (car (w3m-antenna-site-class site))))
      (apply (car (w3m-antenna-site-class site))
	     site handler (cdr (w3m-antenna-site-class site)))
    (if (eq 'hns (w3m-antenna-site-class site))
	(w3m-antenna-check-hns site handler)
      (w3m-antenna-check-page site
			      handler
			      (format-time-string (w3m-antenna-site-key site)
						  (current-time))))))

(defun w3m-antenna-mapcar (function sequence handler)
  "Apply FUNCTION to each element of SEQUENCE asynchronously, and make
a list of the results."
  (let ((index -1)
	(table (make-symbol "table"))
	(buffer (make-symbol "buffer")))
    (set table (make-vector (length sequence) nil))
    (set buffer (current-buffer))
    (dolist (element sequence)
      (aset (symbol-value table)
	    (incf index)
	    (funcall function
		     element
		     (cons `(lambda (x)
			      (aset ,table ,index x)
			      (w3m-antenna-mapcar-after ,table ,buffer))
			   handler))))
    (w3m-antenna-mapcar-after (symbol-value table) (symbol-value buffer))))

(defun w3m-antenna-mapcar-after (result buffer)
  "Handler function of `w3m-antenna-mapcar'.
If all asynchronous processes have finished, return a list of the
results for the further handler functions.  Otherwise, return an
asynchronous process that has not finished yet."
  (or (catch 'found-proces
	(let ((index -1))
	  (while (< (incf index) (length result))
	    (when (w3m-process-p (aref result index))
	      (throw 'found-proces (aref result index))))))
      (progn
	(set-buffer buffer)
	(append result nil))))

(defun w3m-antenna-check-all-sites (&optional handler)
  "Check all sites specified in `w3m-antenna-sites'."
  (unless w3m-antenna-alist
    (setq w3m-antenna-alist (w3m-antenna-alist)))
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-antenna-check-all-sites handler))
    (w3m-process-do
	(result
	 (w3m-antenna-mapcar 'w3m-antenna-check-site
			     w3m-antenna-alist
			     handler))
      (prog1 w3m-antenna-alist
	(w3m-save-list w3m-antenna-file w3m-antenna-alist)
	(setq w3m-antenna-alist nil)))))

(defun w3m-antenna-make-summary (site)
  (format "<li><a href=\"%s\">%s</a> %s"
	  (or (w3m-antenna-site-url site)
	      (w3m-antenna-site-key site))
	  (w3m-antenna-site-title site)
	  (cond
	   ((w3m-antenna-site-last-modified site)
	    (current-time-string (w3m-antenna-site-last-modified site)))
	   ((w3m-antenna-site-size site) "Size")
	   (t ""))))

(defun w3m-antenna-make-summary-like-natsumican (site)
  (let ((t1 (w3m-antenna-site-last-modified site))
	(t2 (w3m-antenna-site-size-detected site)))
    (format "<li>%20s&nbsp;&nbsp;(%s)&nbsp;&nbsp;<a href=\"%s\">%s</a>"
	    (if (or t1 t2)
		(format-time-string "%Y/%m/%d %R" (or t1 t2))
	      "----/--/-- --:--")
	    (cond
	     (t1 "T")
	     (t2 "S")
	     (t "?"))
	    (or (w3m-antenna-site-url site)
		(w3m-antenna-site-key site))
	    (w3m-antenna-site-title site))))

(defun w3m-antenna-sort-sites-by-time (sites)
  (sort sites
	(lambda (a b)
	  (w3m-time-newer-p
	   (or (w3m-antenna-site-last-modified a)
	       (w3m-antenna-site-size-detected a))
	   (or (w3m-antenna-site-last-modified b)
	       (w3m-antenna-site-size-detected b))))))

(defun w3m-antenna-sort-sites-by-title (sites)
  (sort sites
	(lambda (a b)
	  (string< (w3m-antenna-site-title a)
		   (w3m-antenna-site-title b)))))

(defun w3m-antenna-make-contents (changed-sites unchanged-sites)
  (insert w3m-antenna-html-skelton)
  (goto-char (point-min))
  (while (re-search-forward "%\\(.\\)" nil t)
    (let ((c (char-after (match-beginning 1))))
      (cond
       ((memq c '(?C ?U))
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (delete-region (point-min) (point-max))
	  (goto-char (point-min))
	  (dolist (site (if (eq c ?C)
			    changed-sites
			  unchanged-sites))
	    (insert (funcall w3m-antenna-make-summary-function site)
		    "\n"))
	  (goto-char (point-max))))
       ((eq c '?D)
	(goto-char (match-beginning 0))
	(delete-region (match-beginning 0) (match-end 0))
	(insert (let ((time (nth 5 (file-attributes w3m-antenna-file))))
		  (if time
		      (current-time-string time)
		    "(unknown)"))))
       ((eq c '?R)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (delete-region (point-min) (point-max))
	  (when (and w3m-antenna-refresh-interval
		     (integerp w3m-antenna-refresh-interval)
		     (< 0 w3m-antenna-refresh-interval))
	    (insert (format "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"%d\">\n"
			    w3m-antenna-refresh-interval)))))))))

;;;###autoload
(defun w3m-about-antenna (url &optional no-decode no-cache
			      post-data referer handler)
  (w3m-process-do
      (alist (if no-cache
		 (w3m-antenna-check-all-sites handler)
	       (or w3m-antenna-alist (w3m-antenna-alist))))
    (let (changed unchanged)
      (dolist (site alist)
	(if (w3m-time-newer-p (or (w3m-antenna-site-last-modified site)
				  (w3m-antenna-site-size-detected site))
			      (or (w3m-arrived-last-modified
				   (w3m-antenna-site-url site))
				  (w3m-arrived-time
				   (w3m-antenna-site-url site))))
	    (progn
	      (w3m-cache-remove (w3m-antenna-site-url site))
	      (push site changed))
	  (push site unchanged)))
      (w3m-antenna-make-contents
       (funcall w3m-antenna-sort-changed-sites-function (nreverse changed))
       (funcall w3m-antenna-sort-unchanged-sites-function (nreverse unchanged)))
      "text/html")))

;;;###autoload
(defun w3m-antenna (&optional no-cache)
  "Report changes of WEB sites, which is specified in `w3m-antenna-sites'."
  (interactive "P")
  (w3m-goto-url "about://antenna/" no-cache))

(defvar w3m-antenna-tmp-url nil)
(defvar w3m-antenna-tmp-title nil)

(defun w3m-antenna-add-current-url (&optional arg)
  "Add link of current page to antenna.
With prefix, ask new url to add instead of current page."
  (interactive "P")
  (w3m-antenna-add (if arg (w3m-input-url) w3m-current-url)
		   (w3m-encode-specials-string w3m-current-title)))

(defun w3m-antenna-add (url &optional title)
  "Add URL to antenna.
Optional argument TITLE is title of link."
  (setq w3m-antenna-tmp-url url)
  (setq w3m-antenna-tmp-title title)
  (customize-variable 'w3m-antenna-sites)
  ;; dirty...
  (goto-char (point-max))
  (re-search-backward "INS")
  (widget-button-press (point))
  (re-search-forward "State:\\|\\(\\[State\\]:\\)")
  (backward-char (if (match-beginning 1) 3 2)))

(defvar w3m-antenna-mode-map
  (let ((map (make-sparse-keymap)))
    (substitute-key-definition 'w3m-edit-current-url 'w3m-antenna-edit
			       map w3m-mode-map)
    map)
  "*Keymap for `w3m-antenna-mode'.")

(defvar w3m-antenna-mode nil "Non-nil if w3m antenna mode is enabled.")
(make-variable-buffer-local 'w3m-antenna-mode)
(unless (assq 'w3m-antenna-mode minor-mode-alist)
  (push (list 'w3m-antenna-mode " antenna") minor-mode-alist))
(unless (assq 'w3m-antenna-mode minor-mode-map-alist)
  (push (cons 'w3m-antenna-mode w3m-antenna-mode-map) minor-mode-map-alist))

(defun w3m-antenna-mode (&optional arg)
  "\\<w3m-antenna-mode-map>
Minor mode to edit antenna.

\\[w3m-antenna-edit]	Customize `w3m-antenna-sites'.
"
  (interactive "P")
  (when (setq w3m-antenna-mode
	      (if arg
		  (> (prefix-numeric-value arg) 0)
		(not w3m-antenna-mode)))
    (run-hooks 'w3m-antenna-mode-hook)))

(defun w3m-antenna-mode-setter (url)
  "Activate `w3m-antenna-mode', when visiting page shows antenna."
  (w3m-antenna-mode (if (string-match "\\`about://antenna/" url)
			 (progn
			   (setq default-directory
				 (file-name-directory w3m-antenna-file))
			   1)
		       0)))
(add-hook 'w3m-display-functions 'w3m-antenna-mode-setter)

(defun w3m-antenna-edit ()
  "Start customize of `w3m-antenna-sites'."
  (interactive)
  (customize-variable 'w3m-antenna-sites))

(provide 'w3m-antenna)

;;; w3m-antenna.el ends here
