;;; org-protocol.el --- Intercept Calls from Emacsclient to Trigger Custom Actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2008-2017 Free Software Foundation, Inc.
;;
;; Authors: Bastien Guerry <bzg@gnu.org>
;;       Daniel M German <dmg AT uvic DOT org>
;;       Sebastian Rose <sebastian_rose AT gmx DOT de>
;;       Ross Patterson <me AT rpatterson DOT net>
;; Maintainer: Sebastian Rose <sebastian_rose AT gmx DOT de>
;; Keywords: org, emacsclient, wp

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; Intercept calls from emacsclient to trigger custom actions.
;;
;; This is done by advising `server-visit-files' to scan the list of filenames
;; for `org-protocol-the-protocol' and sub-protocols defined in
;; `org-protocol-protocol-alist' and `org-protocol-protocol-alist-default'.
;;
;; Any application that supports calling external programs with an URL
;; as argument may be used with this functionality.
;;
;;
;; Usage:
;; ------
;;
;;   1.) Add this to your init file (.emacs probably):
;;
;;       (add-to-list 'load-path "/path/to/org-protocol/")
;;       (require 'org-protocol)
;;
;;   3.) Ensure emacs-server is up and running.
;;   4.) Try this from the command line (adjust the URL as needed):
;;
;;       $ emacsclient \
;;         org-protocol://store-link?url=http:%2F%2Flocalhost%2Findex.html&title=The%20title
;;
;;   5.) Optionally add custom sub-protocols and handlers:
;;
;;       (setq org-protocol-protocol-alist
;;             '(("my-protocol"
;;                :protocol "my-protocol"
;;                :function my-protocol-handler-function)))
;;
;;       A "sub-protocol" will be found in URLs like this:
;;
;;           org-protocol://sub-protocol?key=val&key2=val2
;;
;; If it works, you can now setup other applications for using this feature.
;;
;;
;; As of March 2009 Firefox users follow the steps documented on
;; http://kb.mozillazine.org/Register_protocol, Opera setup is described here:
;; http://www.opera.com/support/kb/view/535/
;;
;;
;; Documentation
;; -------------
;;
;; org-protocol.el comes with and installs handlers to open sources of published
;; online content, store and insert the browser's URLs and cite online content
;; by clicking on a bookmark in Firefox, Opera and probably other browsers and
;; applications:
;;
;;   * `org-protocol-open-source' uses the sub-protocol \"open-source\" and maps
;;     URLs to local filenames defined in `org-protocol-project-alist'.
;;
;;   * `org-protocol-store-link' stores an Org link (if Org is present) and
;;     pushes the browsers URL to the `kill-ring' for yanking.  This handler is
;;     triggered through the sub-protocol \"store-link\".
;;
;;   * Call `org-protocol-capture' by using the sub-protocol \"capture\".  If
;;     Org is loaded, Emacs will pop-up a capture buffer and fill the
;;     template with the data provided.  I.e. the browser's URL is inserted as an
;;     Org-link of which the page title will be the description part.  If text
;;     was select in the browser, that text will be the body of the entry.
;;
;; You may use the same bookmark URL for all those standard handlers and just
;; adjust the sub-protocol used:
;;
;;     location.href='org-protocol://sub-protocol?url='+
;;           encodeURIComponent(location.href)+'&title='+
;;           encodeURIComponent(document.title)+'&body='+
;;           encodeURIComponent(window.getSelection())
;;
;; The handler for the sub-protocol \"capture\" detects an optional template
;; char that, if present, triggers the use of a special template.
;; Example:
;;
;;     location.href='org-protocol://capture?template=x'+ ...
;;
;;  uses template ?x.
;;
;; Note that using double slashes is optional from org-protocol.el's point of
;; view because emacsclient squashes the slashes to one.
;;
;;
;; provides: 'org-protocol
;;
;;; Code:

(require 'org)

(declare-function org-publish-get-project-from-filename "ox-publish"
		  (filename &optional up))
(declare-function server-edit "server" (&optional arg))

(defvar org-capture-link-is-already-stored)

(defgroup org-protocol nil
  "Intercept calls from emacsclient to trigger custom actions.

This is done by advising `server-visit-files' to scan the list of filenames
for `org-protocol-the-protocol' and sub-protocols defined in
`org-protocol-protocol-alist' and `org-protocol-protocol-alist-default'."
  :version "22.1"
  :group 'convenience
  :group 'org)


;;; Variables:

(defconst org-protocol-protocol-alist-default
  '(("org-capture"     :protocol "capture"     :function org-protocol-capture  :kill-client t)
    ("org-store-link"  :protocol "store-link"  :function org-protocol-store-link)
    ("org-open-source" :protocol "open-source" :function org-protocol-open-source))
  "Default protocols to use.
See `org-protocol-protocol-alist' for a description of this variable.")

(defconst org-protocol-the-protocol "org-protocol"
  "This is the protocol to detect if org-protocol.el is loaded.
`org-protocol-protocol-alist-default' and `org-protocol-protocol-alist' hold
the sub-protocols that trigger the required action.  You will have to define
just one protocol handler OS-wide (MS-Windows) or per application (Linux).
That protocol handler should call emacsclient.")

;;; User variables:

(defcustom org-protocol-reverse-list-of-files t
  "Non-nil means re-reverse the list of filenames passed on the command line.
The filenames passed on the command line are passed to the emacs-server in
reverse order.  Set to t (default) to re-reverse the list, i.e. use the
sequence on the command line.  If nil, the sequence of the filenames is
unchanged."
  :group 'org-protocol
  :type 'boolean)

(defcustom org-protocol-project-alist nil
  "Map URLs to local filenames for `org-protocol-open-source' (open-source).

Each element of this list must be of the form:

  (module-name :property value property: value ...)

where module-name is an arbitrary name.  All the values are strings.

Possible properties are:

  :online-suffix     - the suffix to strip from the published URLs
  :working-suffix    - the replacement for online-suffix
  :base-url          - the base URL, e.g. http://www.example.com/project/
                       Last slash required.
  :working-directory - the local working directory.  This is, what base-url will
                       be replaced with.
  :redirects         - A list of cons cells, each of which maps a regular
                       expression to match to a path relative to :working-directory.

Example:

   (setq org-protocol-project-alist
       \\='((\"http://orgmode.org/worg/\"
          :online-suffix \".php\"
          :working-suffix \".org\"
          :base-url \"http://orgmode.org/worg/\"
          :working-directory \"/home/user/org/Worg/\")
         (\"http://localhost/org-notes/\"
          :online-suffix \".html\"
          :working-suffix \".org\"
          :base-url \"http://localhost/org/\"
          :working-directory \"/home/user/org/\"
          :rewrites ((\"org/?$\" . \"index.php\")))
         (\"Hugo based blog\"
          :base-url \"https://www.site.com/\"
          :working-directory \"~/site/content/post/\"
          :online-suffix \".html\"
          :working-suffix \".md\"
          :rewrites ((\"\\(https://site.com/[0-9]+/[0-9]+/[0-9]+/\\)\" . \".md\")))))


   The last line tells `org-protocol-open-source' to open
   /home/user/org/index.php, if the URL cannot be mapped to an existing
   file, and ends with either \"org\" or \"org/\".

Consider using the interactive functions `org-protocol-create' and
`org-protocol-create-for-org' to help you filling this variable with valid contents."
  :group 'org-protocol
  :type 'alist)

(defcustom org-protocol-protocol-alist nil
  "Register custom handlers for org-protocol.

Each element of this list must be of the form:

  (module-name :protocol protocol :function func :kill-client nil)

protocol - protocol to detect in a filename without trailing
           colon and slashes.  See rfc1738 section 2.1 for more
           on this.  If you define a protocol \"my-protocol\",
           `org-protocol-check-filename-for-protocol' will search
           filenames for \"org-protocol:/my-protocol\" and
           trigger your action for every match.  `org-protocol'
           is defined in `org-protocol-the-protocol'.  Double and
           triple slashes are compressed to one by emacsclient.

function - function that handles requests with protocol and takes
           one argument.  If a new-style link (key=val&key2=val2)
           is given, the argument will be a property list with
           the values from the link.  If an old-style link is
           given (val1/val2), the argument will be the filename
           with all protocols stripped.

           If the function returns nil, emacsclient and -server
           do nothing.  Any non-nil return value is considered a
           valid filename and thus passed to the server.

           `org-protocol.el' provides some support for handling
           old-style filenames, if you follow the conventions
           used for the standard handlers in
           `org-protocol-protocol-alist-default'.  See
           `org-protocol-parse-parameters'.

kill-client - If t, kill the client immediately, once the sub-protocol is
           detected.  This is necessary for actions that can be interrupted by
           `C-g' to avoid dangling emacsclients.  Note that all other command
           line arguments but the this one will be discarded.  Greedy handlers
           still receive the whole list of arguments though.

Here is an example:

  (setq org-protocol-protocol-alist
      \\='((\"my-protocol\"
         :protocol \"my-protocol\"
         :function my-protocol-handler-function)
        (\"your-protocol\"
         :protocol \"your-protocol\"
         :function your-protocol-handler-function)))"
  :group 'org-protocol
  :type '(alist))

(defcustom org-protocol-default-template-key nil
  "The default template key to use.
This is usually a single character string but can also be a
string with two characters."
  :group 'org-protocol
  :type '(choice (const nil) (string)))

(defcustom org-protocol-data-separator "/+\\|\\?"
  "The default data separator to use.
This should be a single regexp string."
  :group 'org-protocol
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;; Helper functions:

(defun org-protocol-sanitize-uri (uri)
  "Sanitize slashes to double-slashes in URI.
Emacsclient compresses double and triple slashes."
  (when (string-match "^\\([a-z]+\\):/" uri)
    (let* ((splitparts (split-string uri "/+")))
      (setq uri (concat (car splitparts) "//" (mapconcat 'identity (cdr splitparts) "/")))))
  uri)

(defun org-protocol-split-data (data &optional unhexify separator)
  "Split the DATA argument for an org-protocol handler function.
If UNHEXIFY is non-nil, hex-decode each split part.  If UNHEXIFY
is a function, use that function to decode each split part.  The
string is split at each occurrence of SEPARATOR (regexp).  If no
SEPARATOR is specified or SEPARATOR is nil, assume \"/+\".  The
results of that splitting are returned as a list."
  (let* ((sep (or separator "/+\\|\\?"))
         (split-parts (split-string data sep)))
    (if unhexify
	(if (fboundp unhexify)
	    (mapcar unhexify split-parts)
	  (mapcar 'org-link-unescape split-parts))
      split-parts)))

(defun org-protocol-flatten-greedy (param-list &optional strip-path replacement)
  "Transform PARAM-LIST into a flat list for greedy handlers.

Greedy handlers might receive a list like this from emacsclient:
\((\"/dir/org-protocol:/greedy:/~/path1\" (23 . 12)) (\"/dir/param\"))
where \"/dir/\" is the absolute path to emacsclient's working directory.  This
function transforms it into a flat list using `org-protocol-flatten' and
transforms the elements of that list as follows:

If STRIP-PATH is non-nil, remove the \"/dir/\" prefix from all members of
param-list.

If REPLACEMENT is string, replace the \"/dir/\" prefix with it.

The first parameter, the one that contains the protocols, is always changed.
Everything up to the end of the protocols is stripped.

Note, that this function will always behave as if
`org-protocol-reverse-list-of-files' was set to t and the returned list will
reflect that.  emacsclient's first parameter will be the first one in the
returned list."
  (let* ((l (org-protocol-flatten (if org-protocol-reverse-list-of-files
				      param-list
				    (reverse param-list))))
	 (trigger (car l))
	 (len 0)
	 dir
	 ret)
    (when (string-match "^\\(.*\\)\\(org-protocol:/+[a-zA-z0-9][-_a-zA-z0-9]*:/+\\)\\(.*\\)" trigger)
      (setq dir (match-string 1 trigger))
      (setq len (length dir))
      (setcar l (concat dir (match-string 3 trigger))))
    (if strip-path
	(progn
	  (dolist (e l ret)
	    (setq ret
		  (append ret
			  (list
			   (if (stringp e)
			       (if (stringp replacement)
				   (setq e (concat replacement (substring e len)))
				 (setq e (substring e len)))
			     e)))))
	  ret)
      l)))

(defun org-protocol-flatten (list)
  "Transform LIST into a flat list.

Greedy handlers might receive a list like this from emacsclient:
\((\"/dir/org-protocol:/greedy:/~/path1\" (23 . 12)) (\"/dir/param\"))
where \"/dir/\" is the absolute path to emacsclients working directory.
This function transforms it into a flat list."
  (if (null list) ()
    (if (listp list)
	(append (org-protocol-flatten (car list)) (org-protocol-flatten (cdr list)))
      (list list))))

(defun org-protocol-parse-parameters (info &optional new-style default-order)
  "Return a property list of parameters from INFO.
If NEW-STYLE is non-nil, treat INFO as a query string (ex:
url=URL&title=TITLE).  If old-style links are used (ex:
org-protocol://store-link/url/title), assign them to attributes
following DEFAULT-ORDER.

If no DEFAULT-ORDER is specified, return the list of values.

If INFO is already a property list, return it unchanged."
  (if (listp info)
      info
    (if new-style
	(let ((data (org-protocol-convert-query-to-plist info))
	      result)
	  (while data
	    (setq result
		  (append
		   result
		   (list
		    (pop data)
		    (org-link-unescape (pop data))))))
	  result)
      (let ((data (org-protocol-split-data info t org-protocol-data-separator)))
	(if default-order
	    (org-protocol-assign-parameters data default-order)
	  data)))))

(defun org-protocol-assign-parameters (data default-order)
  "Return a property list of parameters from DATA.
Key names are taken from DEFAULT-ORDER, which should be a list of
symbols.  If DEFAULT-ORDER is shorter than the number of values
specified, the rest of the values are treated as :key value pairs."
  (let (result)
    (while default-order
      (setq result
	    (append result
		    (list (pop default-order)
			  (pop data)))))
    (while data
      (setq result
	    (append result
		    (list (intern (concat ":" (pop data)))
			  (pop data)))))
    result))

;;; Standard protocol handlers:

(defun org-protocol-store-link (fname)
  "Process an org-protocol://store-link style url.
Additionally store a browser URL as an org link.  Also pushes the
link's URL to the `kill-ring'.

Parameters: url, title (optional), body (optional)

Old-style links such as org-protocol://store-link://URL/TITLE are
also recognized.

The location for a browser's bookmark has to look like this:

  javascript:location.href = \\
      \\='org-protocol://store-link?url=\\=' + \\
      encodeURIComponent(location.href) + \\='&title=\\=' + \\
      encodeURIComponent(document.title);

Don't use `escape()'!  Use `encodeURIComponent()' instead.  The
title of the page could contain slashes and the location
definitely will.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

FNAME should be a property list.  If not, an old-style link of the
form URL/TITLE can also be used."
  (let* ((splitparts (org-protocol-parse-parameters fname nil '(:url :title)))
         (uri (org-protocol-sanitize-uri (plist-get splitparts :url)))
         (title (plist-get splitparts :title)))
    (when (boundp 'org-stored-links)
      (push (list uri title) org-stored-links))
    (kill-new uri)
    (message "`%s' to insert new org-link, `%s' to insert `%s'"
             (substitute-command-keys "`\\[org-insert-link]'")
             (substitute-command-keys "`\\[yank]'")
             uri))
  nil)

(defun org-protocol-capture (info)
  "Process an org-protocol://capture style url with INFO.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects an URL, title and optional text, separated
by `/'.  The location for a browser's bookmark looks like this:

  javascript:location.href = \\='org-protocol://capture?url=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())

By default, it uses the character `org-protocol-default-template-key',
which should be associated with a template in `org-capture-templates'.
You may specify the template with a template= query parameter, like this:

  javascript:location.href = \\='org-protocol://capture?template=b\\='+ ...

Now template ?b will be used."
  (if (and (boundp 'org-stored-links)
	   (org-protocol-do-capture info))
      (message "Item captured."))
  nil)

(defun org-protocol-convert-query-to-plist (query)
  "Convert QUERY key=value pairs in the URL to a property list."
  (if query
      (apply 'append (mapcar (lambda (x)
			       (let ((c (split-string x "=")))
				 (list (intern (concat ":" (car c))) (cadr c))))
			     (split-string query "&")))))

(defun org-protocol-do-capture (info)
  "Perform the actual capture based on INFO."
  (let* ((temp-parts (org-protocol-parse-parameters info))
	 (parts
	  (cond
	   ((and (listp info) (symbolp (car info))) info)
	   ((= (length (car temp-parts)) 1) ;; First parameter is exactly one character long
	    (org-protocol-assign-parameters temp-parts '(:template :url :title :body)))
	   (t
	    (org-protocol-assign-parameters temp-parts '(:url :title :body)))))
	 (template (or (plist-get parts :template)
		       org-protocol-default-template-key))
	 (url (and (plist-get parts :url) (org-protocol-sanitize-uri (plist-get parts :url))))
	 (type (and url (if (string-match "^\\([a-z]+\\):" url)
			    (match-string 1 url))))
	 (title (or (plist-get parts :title) ""))
	 (region (or (plist-get parts :body) ""))
	 (orglink (if url
		      (org-make-link-string
		       url (if (string-match "[^[:space:]]" title) title url))
		    title))
	 (org-capture-link-is-already-stored t)) ;; avoid call to org-store-link
    (setq org-stored-links
	  (cons (list url title) org-stored-links))
    (org-store-link-props :type type
			  :link url
			  :description title
			  :annotation orglink
			  :initial region
			  :query parts)
    (raise-frame)
    (funcall 'org-capture nil template)))

(defun org-protocol-open-source (fname)
  "Process an org-protocol://open-source?url= style URL with FNAME.

Change a filename by mapping URLs to local filenames as set
in `org-protocol-project-alist'.

The location for a browser's bookmark should look like this:

  javascript:location.href = \\='org-protocol://open-source?url=\\=' + \\
        encodeURIComponent(location.href)"
  ;; As we enter this function for a match on our protocol, the return value
  ;; defaults to nil.
  (let ((result nil)
	(f (org-protocol-sanitize-uri
	    (plist-get (org-protocol-parse-parameters fname nil '(:url))
		       :url))))
    (catch 'result
      (dolist (prolist org-protocol-project-alist)
        (let* ((base-url (plist-get (cdr prolist) :base-url))
               (wsearch (regexp-quote base-url)))

          (when (string-match wsearch f)
            (let* ((wdir (plist-get (cdr prolist) :working-directory))
                   (strip-suffix (plist-get (cdr prolist) :online-suffix))
                   (add-suffix (plist-get (cdr prolist) :working-suffix))
		   ;; Strip "[?#].*$" if `f' is a redirect with another
		   ;; ending than strip-suffix here:
		   (f1 (substring f 0 (string-match "\\([\\?#].*\\)?$" f)))
                   (start-pos (+ (string-match wsearch f1) (length base-url)))
                   (end-pos (string-match
			     (regexp-quote strip-suffix) f1))
		   ;; We have to compare redirects without suffix below:
		   (f2 (concat wdir (substring f1 start-pos end-pos)))
                   (the-file (concat f2 add-suffix)))

	      ;; Note: the-file may still contain `%C3' et al here because browsers
	      ;; tend to encode `&auml;' in URLs to `%25C3' - `%25' being `%'.
	      ;; So the results may vary.

	      ;; -- start redirects --
	      (unless (file-exists-p the-file)
		(message "File %s does not exist.\nTesting for rewritten URLs." the-file)
		(let ((rewrites (plist-get (cdr prolist) :rewrites)))
		  (when rewrites
		    (message "Rewrites found: %S" rewrites)
		    (dolist (rewrite rewrites)
		      ;; Try to match a rewritten URL and map it to
		      ;; a real file.  Compare redirects without
		      ;; suffix.
		      (when (string-match (car rewrite) f1)
			(let ((replacement
			       (concat (directory-file-name
					(replace-match "" nil nil f1 1))
				       (cdr rewrite))))
			  (throw 'result (concat wdir replacement))))))))
	      ;; -- end of redirects --

              (if (file-readable-p the-file)
                  (throw 'result the-file))
              (if (file-exists-p the-file)
                  (message "%s: permission denied!" the-file)
                (message "%s: no such file or directory." the-file))))))
      result)))


;;; Core functions:

(defun org-protocol-check-filename-for-protocol (fname restoffiles _client)
  "Check if `org-protocol-the-protocol' and a valid protocol are used in FNAME.
Sub-protocols are registered in `org-protocol-protocol-alist' and
`org-protocol-protocol-alist-default'.  This is how the matching is done:

  (string-match \"protocol:/+sub-protocol\\\\(://\\\\|\\\\?\\\\)\" ...)

protocol and sub-protocol are regexp-quoted.

Old-style links such as \"protocol://sub-protocol://param1/param2\" are
also recognized.

If a matching protocol is found, the protocol is stripped from
fname and the result is passed to the protocol function as the
first parameter.  The second parameter will be non-nil if FNAME
uses key=val&key2=val2-type arguments, or nil if FNAME uses
val/val2-type arguments.  If the function returns nil, the
filename is removed from the list of filenames passed from
emacsclient to the server.  If the function returns a non-nil
value, that value is passed to the server as filename.

If the handler function is greedy, RESTOFFILES will also be passed to it.

CLIENT is ignored."
  (let ((sub-protocols (append org-protocol-protocol-alist
			       org-protocol-protocol-alist-default)))
    (catch 'fname
      (let ((the-protocol (concat (regexp-quote org-protocol-the-protocol)
				  ":/+")))
        (when (string-match the-protocol fname)
          (dolist (prolist sub-protocols)
            (let ((proto
		   (concat the-protocol
			   (regexp-quote (plist-get (cdr prolist) :protocol))
			   "\\(:/+\\|\\?\\)")))
              (when (string-match proto fname)
                (let* ((func (plist-get (cdr prolist) :function))
                       (greedy (plist-get (cdr prolist) :greedy))
                       (split (split-string fname proto))
                       (result (if greedy restoffiles (cadr split)))
		       (new-style (string= (match-string 1 fname) "?")))
                  (when (plist-get (cdr prolist) :kill-client)
		    (message "Greedy org-protocol handler.  Killing client.")
		    (server-edit))
                  (when (fboundp func)
                    (unless greedy
                      (throw 'fname
			     (if new-style
				 (funcall func (org-protocol-parse-parameters
						result new-style))
			       (warn "Please update your Org Protocol handler \
to deal with new-style links.")
			       (funcall func result))))
		    ;; Greedy protocol handlers are responsible for
		    ;; parsing their own filenames.
		    (funcall func result)
                    (throw 'fname t))))))))
      fname)))

(defadvice server-visit-files (before org-protocol-detect-protocol-server activate)
  "Advice server-visit-flist to call `org-protocol-modify-filename-for-protocol'."
  (let ((flist (if org-protocol-reverse-list-of-files
                   (reverse  (ad-get-arg 0))
                 (ad-get-arg 0)))
        (client (ad-get-arg 1)))
    (catch 'greedy
      (dolist (var flist)
	;; `\' to `/' on windows.  FIXME: could this be done any better?
        (let ((fname  (expand-file-name (car var))))
          (setq fname (org-protocol-check-filename-for-protocol
		       fname (member var flist)  client))
          (if (eq fname t) ;; greedy? We need the t return value.
              (progn
                (ad-set-arg 0 nil)
                (throw 'greedy t))
            (if (stringp fname) ;; probably filename
                (setcar var fname)
              (ad-set-arg 0 (delq var (ad-get-arg 0))))))))))

;;; Org specific functions:

(defun org-protocol-create-for-org ()
  "Create a Org protocol project for the current file's project.
The visited file needs to be part of a publishing project in
`org-publish-project-alist' for this to work.  The function
delegates most of the work to `org-protocol-create'."
  (interactive)
  (require 'ox-publish)
  (let ((all (or (org-publish-get-project-from-filename buffer-file-name))))
    (if all (org-protocol-create (cdr all))
      (message "%s"
	       (substitute-command-keys
		"Not in an Org project.  \
Did you mean `\\[org-protocol-create]'?")))))

(defun org-protocol-create (&optional project-plist)
  "Create a new org-protocol project interactively.
An org-protocol project is an entry in
`org-protocol-project-alist' which is used by
`org-protocol-open-source'.  Optionally use PROJECT-PLIST to
initialize the defaults for this project.  If PROJECT-PLIST is
the cdr of an element in `org-publish-project-alist', reuse
:base-directory, :html-extension and :base-extension."
  (interactive)
  (let ((working-dir (expand-file-name
		      (or (plist-get project-plist :base-directory)
			  default-directory)))
        (base-url "http://orgmode.org/worg/")
        (strip-suffix (or (plist-get project-plist :html-extension) ".html"))
        (working-suffix (if (plist-get project-plist :base-extension)
                            (concat "." (plist-get project-plist :base-extension))
                          ".org"))
        (insert-default-directory t)
        (minibuffer-allow-text-properties nil))

    (setq base-url (read-string "Base URL of published content: " base-url nil base-url t))
    (or (string-suffix-p "/" base-url)
	(setq base-url (concat base-url "/")))

    (setq working-dir
          (expand-file-name
           (read-directory-name "Local working directory: " working-dir working-dir t)))
    (or (string-suffix-p "/" working-dir)
	(setq working-dir (concat working-dir "/")))

    (setq strip-suffix
          (read-string
           (concat "Extension to strip from published URLs (" strip-suffix "): ")
	   strip-suffix nil strip-suffix t))

    (setq working-suffix
          (read-string
           (concat "Extension of editable files (" working-suffix "): ")
	   working-suffix nil working-suffix t))

    (when (yes-or-no-p "Save the new org-protocol-project to your init file? ")
      (setq org-protocol-project-alist
            (cons `(,base-url . (:base-url ,base-url
					   :working-directory ,working-dir
					   :online-suffix ,strip-suffix
					   :working-suffix ,working-suffix))
                  org-protocol-project-alist))
      (customize-save-variable 'org-protocol-project-alist org-protocol-project-alist))))

(provide 'org-protocol)

;;; org-protocol.el ends here
