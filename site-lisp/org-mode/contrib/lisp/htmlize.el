;; htmlize.el -- Convert buffer text and decorations to HTML.

;; Copyright (C) 1997-2012 Hrvoje Niksic

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: hypermedia, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package converts the buffer text and the associated
;; decorations to HTML.  Mail to <hniksic@xemacs.org> to discuss
;; features and additions.  All suggestions are more than welcome.

;; To use this, just switch to the buffer you want HTML-ized and type
;; `M-x htmlize-buffer'.  You will be switched to a new buffer that
;; contains the resulting HTML code.  You can edit and inspect this
;; buffer, or you can just save it with C-x C-w.  `M-x htmlize-file'
;; will find a file, fontify it, and save the HTML version in
;; FILE.html, without any additional intervention.  `M-x
;; htmlize-many-files' allows you to htmlize any number of files in
;; the same manner.  `M-x htmlize-many-files-dired' does the same for
;; files marked in a dired buffer.

;; htmlize supports three types of HTML output, selected by setting
;; `htmlize-output-type': `css', `inline-css', and `font'.  In `css'
;; mode, htmlize uses cascading style sheets to specify colors; it
;; generates classes that correspond to Emacs faces and uses <span
;; class=FACE>...</span> to color parts of text.  In this mode, the
;; produced HTML is valid under the 4.01 strict DTD, as confirmed by
;; the W3C validator.  `inline-css' is like `css', except the CSS is
;; put directly in the STYLE attribute of the SPAN element, making it
;; possible to paste the generated HTML to other documents.  In `font'
;; mode, htmlize uses <font color="...">...</font> to colorize HTML,
;; which is not standard-compliant, but works better in older
;; browsers.  `css' mode is the default.

;; You can also use htmlize from your Emacs Lisp code.  When called
;; non-interactively, `htmlize-buffer' and `htmlize-region' will
;; return the resulting HTML buffer, but will not change current
;; buffer or move the point.

;; I tried to make the package elisp-compatible with multiple Emacsen,
;; specifically aiming for XEmacs 19.14+ and GNU Emacs 19.34+.  Please
;; let me know if it doesn't work on some of those, and I'll try to
;; fix it.  I relied heavily on the presence of CL extensions,
;; especially for cross-emacs compatibility; please don't try to
;; remove that particular dependency.  When byte-compiling under GNU
;; Emacs, you're likely to get some warnings; just ignore them.

;; The latest version should be available at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el>
;;
;; You can find a sample of htmlize's output (possibly generated with
;; an older version) at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.html>

;; Thanks go to the multitudes of people who have sent reports and
;; contributed comments, suggestions, and fixes.  They include Ron
;; Gut, Bob Weiner, Toni Drabik, Peter Breton, Thomas Vogels, Juri
;; Linkov, Maciek Pasternacki, and many others.

;; User quotes: "You sir, are a sick, sick, _sick_ person. :)"
;;                  -- Bill Perry, author of Emacs/W3


;;; Code:

(require 'cl)
(eval-when-compile
  (if (string-match "XEmacs" emacs-version)
      (byte-compiler-options
	(warnings (- unresolved))))
  (defvar font-lock-auto-fontify)
  (defvar font-lock-support-mode)
  (defvar global-font-lock-mode)
  (when (and (eq emacs-major-version 19)
	     (not (string-match "XEmacs" emacs-version)))
    ;; Older versions of GNU Emacs fail to autoload cl-extra even when
    ;; `cl' is loaded.
    (load "cl-extra")))

(defconst htmlize-version "1.36")

;; Incantations to make custom stuff work without customize, e.g. on
;; XEmacs 19.14 or GNU Emacs 19.34.
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil				; we've got what we needed
    ;; No custom or obsolete custom, define surrogates.  Define all
    ;; three macros, so we don't hose another library that expects
    ;; e.g. `defface' to work after (fboundp 'defcustom) succeeds.
    (defmacro defgroup (&rest ignored) nil)
    (defmacro defcustom (var value doc &rest ignored)
      `(defvar ,var ,value ,doc))
    (defmacro defface (face value doc &rest stuff)
      `(make-face ,face))))

(defgroup htmlize nil
  "Convert buffer text and faces to HTML."
  :group 'hypermedia)

(defcustom htmlize-head-tags ""
  "*Additional tags to insert within HEAD of the generated document."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-output-type 'css
  "*Output type of generated HTML, one of `css', `inline-css', or `font'.
When set to `css' (the default), htmlize will generate a style sheet
with description of faces, and use it in the HTML document, specifying
the faces in the actual text with <span class=\"FACE\">.

When set to `inline-css', the style will be generated as above, but
placed directly in the STYLE attribute of the span ELEMENT: <span
style=\"STYLE\">.  This makes it easier to paste the resulting HTML to
other documents.

When set to `font', the properties will be set using layout tags
<font>, <b>, <i>, <u>, and <strike>.

`css' output is normally preferred, but `font' is still useful for
supporting old, pre-CSS browsers, and both `inline-css' and `font' for
easier embedding of colorized text in foreign HTML documents (no style
sheet to carry around)."
  :type '(choice (const css) (const inline-css) (const font))
  :group 'htmlize)

(defcustom htmlize-generate-hyperlinks t
  "*Non-nil means generate the hyperlinks for URLs and mail addresses.
This is on by default; set it to nil if you don't want htmlize to
insert hyperlinks in the resulting HTML.  (In which case you can still
do your own hyperlinkification from htmlize-after-hook.)"
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-hyperlink-style "
      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
"
  "*The CSS style used for hyperlinks when in CSS mode."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-replace-form-feeds t
  "*Non-nil means replace form feeds in source code with HTML separators.
Form feeds are the ^L characters at line beginnings that are sometimes
used to separate sections of source code.  If this variable is set to
`t', form feed characters are replaced with the <hr> separator.  If this
is a string, it specifies the replacement to use.  Note that <pre> is
temporarily closed before the separator is inserted, so the default
replacement is effectively \"</pre><hr /><pre>\".  If you specify
another replacement, don't forget to close and reopen the <pre> if you
want the output to remain valid HTML.

If you need more elaborate processing, set this to nil and use
htmlize-after-hook."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-charset nil
  "*The charset declared by the resulting HTML documents.
When non-nil, causes htmlize to insert the following in the HEAD section
of the generated HTML:

  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=CHARSET\">

where CHARSET is the value you've set for htmlize-html-charset.  Valid
charsets are defined by MIME and include strings like \"iso-8859-1\",
\"iso-8859-15\", \"utf-8\", etc.

If you are using non-Latin-1 charsets, you might need to set this for
your documents to render correctly.  Also, the W3C validator requires
submitted HTML documents to declare a charset.  So if you care about
validation, you can use this to prevent the validator from bitching.

Needless to say, if you set this, you should actually make sure that
the buffer is in the encoding you're claiming it is in.  (Under Mule
that is done by ensuring the correct \"file coding system\" for the
buffer.)  If you don't understand what that means, this option is
probably not for you."
  :type '(choice (const :tag "Unset" nil)
		 string)
  :group 'htmlize)

(defcustom htmlize-convert-nonascii-to-entities (featurep 'mule)
  "*Whether non-ASCII characters should be converted to HTML entities.

When this is non-nil, characters with codes in the 128-255 range will be
considered Latin 1 and rewritten as \"&#CODE;\".  Characters with codes
above 255 will be converted to \"&#UCS;\", where UCS denotes the Unicode
code point of the character.  If the code point cannot be determined,
the character will be copied unchanged, as would be the case if the
option were nil.

When the option is nil, the non-ASCII characters are copied to HTML
without modification.  In that case, the web server and/or the browser
must be set to understand the encoding that was used when saving the
buffer.  (You might also want to specify it by setting
`htmlize-html-charset'.)

Note that in an HTML entity \"&#CODE;\", CODE is always a UCS code point,
which has nothing to do with the charset the page is in.  For example,
\"&#169;\" *always* refers to the copyright symbol, regardless of charset
specified by the META tag or the charset sent by the HTTP server.  In
other words, \"&#169;\" is exactly equivalent to \"&copy;\".

By default, entity conversion is turned on for Mule-enabled Emacsen and
turned off otherwise.  This is because Mule knows the charset of
non-ASCII characters in the buffer.  A non-Mule Emacs cannot tell
whether a character with code 0xA9 represents Latin 1 copyright symbol,
Latin 2 \"S with caron\", or something else altogether.  Setting this to
t without Mule means asserting that 128-255 characters always mean Latin
1.

For most people htmlize will work fine with this option left at the
default setting; don't change it unless you know what you're doing."
  :type 'sexp
  :group 'htmlize)

(defcustom htmlize-ignore-face-size 'absolute
  "*Whether face size should be ignored when generating HTML.
If this is nil, face sizes are used.  If set to t, sizes are ignored
If set to `absolute', only absolute size specifications are ignored.
Please note that font sizes only work with CSS-based output types."
  :type '(choice (const :tag "Don't ignore" nil)
		 (const :tag "Ignore all" t)
		 (const :tag "Ignore absolute" absolute))
  :group 'htmlize)

(defcustom htmlize-css-name-prefix ""
  "*The prefix used for CSS names.
The CSS names that htmlize generates from face names are often too
generic for CSS files; for example, `font-lock-type-face' is transformed
to `type'.  Use this variable to add a prefix to the generated names.
The string \"htmlize-\" is an example of a reasonable prefix."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-use-rgb-txt t
  "*Whether `rgb.txt' should be used to convert color names to RGB.

This conversion means determining, for instance, that the color
\"IndianRed\" corresponds to the (205, 92, 92) RGB triple.  `rgb.txt'
is the X color database that maps hundreds of color names to such RGB
triples.  When this variable is non-nil, `htmlize' uses `rgb.txt' to
look up color names.

If this variable is nil, htmlize queries Emacs for RGB components of
colors using `color-instance-rgb-components' and `x-color-values'.
This can yield incorrect results on non-true-color displays.

If the `rgb.txt' file is not found (which will be the case if you're
running Emacs on non-X11 systems), this option is ignored."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-major-mode nil
  "The mode the newly created HTML buffer will be put in.
Set this to nil if you prefer the default (fundamental) mode."
  :type '(radio (const :tag "No mode (fundamental)" nil)
		 (function-item html-mode)
		 (function :tag "User-defined major mode"))
  :group 'htmlize)

(defvar htmlize-before-hook nil
  "Hook run before htmlizing a buffer.
The hook functions are run in the source buffer (not the resulting HTML
buffer).")

(defvar htmlize-after-hook nil
  "Hook run after htmlizing a buffer.
Unlike `htmlize-before-hook', these functions are run in the generated
HTML buffer.  You may use them to modify the outlook of the final HTML
output.")

(defvar htmlize-file-hook nil
  "Hook run by `htmlize-file' after htmlizing a file, but before saving it.")

(defvar htmlize-buffer-places)

;;; Some cross-Emacs compatibility.

;; I try to conditionalize on features rather than Emacs version, but
;; in some cases checking against the version *is* necessary.
(defconst htmlize-running-xemacs (string-match "XEmacs" emacs-version))

(eval-and-compile
  ;; save-current-buffer, with-current-buffer, and with-temp-buffer
  ;; are not available in 19.34 and in older XEmacsen.  Strictly
  ;; speaking, we should stick to our own namespace and define and use
  ;; htmlize-save-current-buffer, etc.  But non-standard special forms
  ;; are a pain because they're not properly fontified or indented and
  ;; because they look weird and ugly.  So I'll just go ahead and
  ;; define the real ones if they're not available.  If someone
  ;; convinces me that this breaks something, I'll switch to the
  ;; "htmlize-" namespace.
  (unless (fboundp 'save-current-buffer)
    (defmacro save-current-buffer (&rest forms)
      `(let ((__scb_current (current-buffer)))
	 (unwind-protect
	     (progn ,@forms)
	   (set-buffer __scb_current)))))
  (unless (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest forms)
      `(save-current-buffer (set-buffer ,buffer) ,@forms)))
  (unless (fboundp 'with-temp-buffer)
    (defmacro with-temp-buffer (&rest forms)
      (let ((temp-buffer (gensym "tb-")))
	`(let ((,temp-buffer
		(get-buffer-create (generate-new-buffer-name " *temp*"))))
	   (unwind-protect
	       (with-current-buffer ,temp-buffer
		 ,@forms)
	     (and (buffer-live-p ,temp-buffer)
		  (kill-buffer ,temp-buffer))))))))

;; We need a function that efficiently finds the next change of a
;; property (usually `face'), preferably regardless of whether the
;; change occurred because of a text property or an extent/overlay.
;; As it turns out, it is not easy to do that compatibly.
;;
;; Under XEmacs, `next-single-property-change' does that.  Under GNU
;; Emacs beginning with version 21, `next-single-char-property-change'
;; is available and does the same.  GNU Emacs 20 had
;; `next-char-property-change', which we can use.  GNU Emacs 19 didn't
;; provide any means for simultaneously examining overlays and text
;; properties, so when using Emacs 19.34, we punt and fall back to
;; `next-single-property-change', thus ignoring overlays altogether.

(cond
 (htmlize-running-xemacs
  ;; XEmacs: good.
  (defun htmlize-next-change (pos prop &optional limit)
    (next-single-property-change pos prop nil (or limit (point-max)))))
 ((fboundp 'next-single-char-property-change)
  ;; GNU Emacs 21: good.
  (defun htmlize-next-change (pos prop &optional limit)
    (next-single-char-property-change pos prop nil limit)))
 ((fboundp 'next-char-property-change)
  ;; GNU Emacs 20: bad, but fixable.
  (defun htmlize-next-change (pos prop &optional limit)
    (let ((done nil)
	  (current-value (get-char-property pos prop))
	  newpos next-value)
      ;; Loop over positions returned by next-char-property-change
      ;; until the value of PROP changes or we've hit EOB.
      (while (not done)
	(setq newpos (next-char-property-change pos limit)
	      next-value (get-char-property newpos prop))
	(cond ((eq newpos pos)
	       ;; Possibly at EOB?  Whatever, just don't infloop.
	       (setq done t))
	      ((eq next-value current-value)
	       ;; PROP hasn't changed -- keep looping.
	       )
	      (t
	       (setq done t)))
	(setq pos newpos))
      pos)))
 (t
  ;; GNU Emacs 19.34: hopeless, cannot properly support overlays.
  (defun htmlize-next-change (pos prop &optional limit)
    (unless limit
      (setq limit (point-max)))
    (let ((res (next-single-property-change pos prop)))
      (if (or (null res)
	      (> res limit))
	  limit
	res)))))

;;; Transformation of buffer text: HTML escapes, untabification, etc.

(defvar htmlize-basic-character-table
  ;; Map characters in the 0-127 range to either one-character strings
  ;; or to numeric entities.
  (let ((table (make-vector 128 ?\0)))
    ;; Map characters in the 32-126 range to themselves, others to
    ;; &#CODE entities;
    (dotimes (i 128)
      (setf (aref table i) (if (and (>= i 32) (<= i 126))
			       (char-to-string i)
			     (format "&#%d;" i))))
    ;; Set exceptions manually.
    (setf
     ;; Don't escape newline, carriage return, and TAB.
     (aref table ?\n) "\n"
     (aref table ?\r) "\r"
     (aref table ?\t) "\t"
     ;; Escape &, <, and >.
     (aref table ?&) "&amp;"
     (aref table ?<) "&lt;"
     (aref table ?>) "&gt;"
     ;; Not escaping '"' buys us a measurable speedup.  It's only
     ;; necessary to quote it for strings used in attribute values,
     ;; which htmlize doesn't do.
     ;(aref table ?\") "&quot;"
     )
    table))

;; A cache of HTML representation of non-ASCII characters.  Depending
;; on availability of `encode-char' and the setting of
;; `htmlize-convert-nonascii-to-entities', this maps non-ASCII
;; characters to either "&#<code>;" or "<char>" (mapconcat's mapper
;; must always return strings).  It's only filled as characters are
;; encountered, so that in a buffer with e.g. French text, it will
;; only ever contain French accented characters as keys.  It's cleared
;; on each entry to htmlize-buffer-1 to allow modifications of
;; `htmlize-convert-nonascii-to-entities' to take effect.
(defvar htmlize-extended-character-cache (make-hash-table :test 'eq))

(defun htmlize-protect-string (string)
  "HTML-protect string, escaping HTML metacharacters and I18N chars."
  ;; Only protecting strings that actually contain unsafe or non-ASCII
  ;; chars removes a lot of unnecessary funcalls and consing.
  (if (not (string-match "[^\r\n\t -%'-;=?-~]" string))
      string
    (mapconcat (lambda (char)
		 (cond
		  ((< char 128)
		   ;; ASCII: use htmlize-basic-character-table.
		   (aref htmlize-basic-character-table char))
		  ((gethash char htmlize-extended-character-cache)
		   ;; We've already seen this char; return the cached
		   ;; string.
		   )
		  ((not htmlize-convert-nonascii-to-entities)
		   ;; If conversion to entities is not desired, always
		   ;; copy the char literally.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))
		  ((< char 256)
		   ;; Latin 1: no need to call encode-char.
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" char)))
		  ((and (fboundp 'encode-char)
			;; Must check if encode-char works for CHAR;
			;; it fails for Arabic and possibly elsewhere.
			(encode-char char 'ucs))
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" (encode-char char 'ucs))))
		  (t
		   ;; encode-char doesn't work for this char.  Copy it
		   ;; unchanged and hope for the best.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))))
	       string "")))

(defconst htmlize-ellipsis "...")
(put-text-property 0 (length htmlize-ellipsis) 'htmlize-ellipsis t htmlize-ellipsis)

(defun htmlize-buffer-substring-no-invisible (beg end)
  ;; Like buffer-substring-no-properties, but don't copy invisible
  ;; parts of the region.  Where buffer-substring-no-properties
  ;; mandates an ellipsis to be shown, htmlize-ellipsis is inserted.
  (let ((pos beg)
	visible-list invisible show next-change)
    ;; Iterate over the changes in the `invisible' property and filter
    ;; out the portions where it's non-nil, i.e. where the text is
    ;; invisible.
    (while (< pos end)
      (setq invisible (get-char-property pos 'invisible)
	    next-change (htmlize-next-change pos 'invisible end))
      (if (not (listp buffer-invisibility-spec))
	  ;; If buffer-invisibility-spec is not a list, then all
	  ;; characters with non-nil `invisible' property are visible.
	  (setq show (not invisible))
	;; Otherwise, the value of a non-nil `invisible' property can be:
	;; 1. a symbol -- make the text invisible if it matches
	;;    buffer-invisibility-spec.
	;; 2. a list of symbols -- make the text invisible if
	;;    any symbol in the list matches
	;;    buffer-invisibility-spec.
	;; If the match of buffer-invisibility-spec has a non-nil
	;; CDR, replace the invisible text with an ellipsis.
	(let (match)
	  (if (symbolp invisible)
	      (setq match (member* invisible buffer-invisibility-spec
				   :key (lambda (i)
					  (if (symbolp i) i (car i)))))
	    (setq match (block nil
			  (dolist (elem invisible)
			    (let ((m (member*
				      elem buffer-invisibility-spec
				      :key (lambda (i)
					     (if (symbolp i) i (car i))))))
			      (when m (return m))))
			  nil)))
	  (setq show (cond ((null match) t)
			   ((and (cdr-safe (car match))
				 ;; Conflate successive ellipses.
				 (not (eq show htmlize-ellipsis)))
			    htmlize-ellipsis)
			   (t nil)))))
      (cond ((eq show t)
	     (push (buffer-substring-no-properties pos next-change) visible-list))
	    ((stringp show)
	     (push show visible-list)))
      (setq pos next-change))
    (if (= (length visible-list) 1)
	;; If VISIBLE-LIST consists of only one element, return it
	;; without concatenation.  This avoids additional consing in
	;; regions without any invisible text.
	(car visible-list)
      (apply #'concat (nreverse visible-list)))))

(defun htmlize-trim-ellipsis (text)
  ;; Remove htmlize-ellipses ("...") from the beginning of TEXT if it
  ;; starts with it.  It checks for the special property of the
  ;; ellipsis so it doesn't work on ordinary text that begins with
  ;; "...".
  (if (get-text-property 0 'htmlize-ellipsis text)
      (substring text (length htmlize-ellipsis))
    text))

(defconst htmlize-tab-spaces
  ;; A table of strings with spaces.  (aref htmlize-tab-spaces 5) is
  ;; like (make-string 5 ?\ ), except it doesn't cons.
  (let ((v (make-vector 32 nil)))
    (dotimes (i (length v))
      (setf (aref v i) (make-string i ?\ )))
    v))

(defun htmlize-untabify (text start-column)
  "Untabify TEXT, assuming it starts at START-COLUMN."
  (let ((column start-column)
	(last-match 0)
	(chunk-start 0)
	chunks match-pos tab-size)
    (while (string-match "[\t\n]" text last-match)
      (setq match-pos (match-beginning 0))
      (cond ((eq (aref text match-pos) ?\t)
	     ;; Encountered a tab: create a chunk of text followed by
	     ;; the expanded tab.
	     (push (substring text chunk-start match-pos) chunks)
	     ;; Increase COLUMN by the length of the text we've
	     ;; skipped since last tab or newline.  (Encountering
	     ;; newline resets it.)
	     (incf column (- match-pos last-match))
	     ;; Calculate tab size based on tab-width and COLUMN.
	     (setq tab-size (- tab-width (% column tab-width)))
	     ;; Expand the tab.
	     (push (aref htmlize-tab-spaces tab-size) chunks)
	     (incf column tab-size)
	     (setq chunk-start (1+ match-pos)))
	    (t
	     ;; Reset COLUMN at beginning of line.
	     (setq column 0)))
      (setq last-match (1+ match-pos)))
    ;; If no chunks have been allocated, it means there have been no
    ;; tabs to expand.  Return TEXT unmodified.
    (if (null chunks)
	text
      (when (< chunk-start (length text))
	;; Push the remaining chunk.
	(push (substring text chunk-start) chunks))
      ;; Generate the output from the available chunks.
      (apply #'concat (nreverse chunks)))))

(defun htmlize-despam-address (string)
  "Replace every occurrence of '@' in STRING with &#64;.
`htmlize-make-hyperlinks' uses this to spam-protect mailto links
without modifying their meaning."
  ;; Suggested by Ville Skytta.
  (while (string-match "@" string)
    (setq string (replace-match "&#64;" nil t string)))
  string)

(defun htmlize-make-hyperlinks ()
  "Make hyperlinks in HTML."
  ;; Function originally submitted by Ville Skytta.  Rewritten by
  ;; Hrvoje Niksic, then modified by Ville Skytta and Hrvoje Niksic.
  (goto-char (point-min))
  (while (re-search-forward
	  "&lt;\\(\\(mailto:\\)?\\([-=+_.a-zA-Z0-9]+@[-_.a-zA-Z0-9]+\\)\\)&gt;"
	  nil t)
    (let ((address (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"mailto:"
	      (htmlize-despam-address address)
	      "\">"
	      (htmlize-despam-address link-text)
	      "</a>&gt;")))
  (goto-char (point-min))
  (while (re-search-forward "&lt;\\(\\(URL:\\)?\\([a-zA-Z]+://[^;]+\\)\\)&gt;"
			    nil t)
    (let ((url (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"" url "\">" link-text "</a>&gt;"))))

;; Tests for htmlize-make-hyperlinks:

;; <mailto:hniksic@xemacs.org>
;; <http://fly.srk.fer.hr>
;; <URL:http://www.xemacs.org>
;; <http://www.mail-archive.com/bbdb-info@xemacs.org/>
;; <hniksic@xemacs.org>
;; <xalan-dev-sc.10148567319.hacuhiucknfgmpfnjcpg-john=doe.com@xml.apache.org>

(defun htmlize-defang-local-variables ()
  ;; Juri Linkov reports that an HTML-ized "Local variables" can lead
  ;; visiting the HTML to fail with "Local variables list is not
  ;; properly terminated".  He suggested changing the phrase to
  ;; syntactically equivalent HTML that Emacs doesn't recognize.
  (goto-char (point-min))
  (while (search-forward "Local Variables:" nil t)
    (replace-match "Local Variables&#58;" nil t)))


;;; Color handling.

(if (fboundp 'locate-file)
    (defalias 'htmlize-locate-file 'locate-file)
  (defun htmlize-locate-file (file path)
    (dolist (dir path nil)
      (when (file-exists-p (expand-file-name file dir))
	(return (expand-file-name file dir))))))

(defvar htmlize-x-library-search-path
  '("/usr/X11R6/lib/X11/"
    "/usr/X11R5/lib/X11/"
    "/usr/lib/X11R6/X11/"
    "/usr/lib/X11R5/X11/"
    "/usr/local/X11R6/lib/X11/"
    "/usr/local/X11R5/lib/X11/"
    "/usr/local/lib/X11R6/X11/"
    "/usr/local/lib/X11R5/X11/"
    "/usr/X11/lib/X11/"
    "/usr/lib/X11/"
    "/usr/local/lib/X11/"
    "/usr/X386/lib/X11/"
    "/usr/x386/lib/X11/"
    "/usr/XFree86/lib/X11/"
    "/usr/unsupported/lib/X11/"
    "/usr/athena/lib/X11/"
    "/usr/local/x11r5/lib/X11/"
    "/usr/lpp/Xamples/lib/X11/"
    "/usr/openwin/lib/X11/"
    "/usr/openwin/share/lib/X11/"))

(defun htmlize-get-color-rgb-hash (&optional rgb-file)
  "Return a hash table mapping X color names to RGB values.
The keys in the hash table are X11 color names, and the values are the
#rrggbb RGB specifications, extracted from `rgb.txt'.

If RGB-FILE is nil, the function will try hard to find a suitable file
in the system directories.

If no rgb.txt file is found, return nil."
  (let ((rgb-file (or rgb-file (htmlize-locate-file
				"rgb.txt"
				htmlize-x-library-search-path)))
	(hash nil))
    (when rgb-file
      (with-temp-buffer
	(insert-file-contents rgb-file)
	(setq hash (make-hash-table :test 'equal))
	(while (not (eobp))
	  (cond ((looking-at "^\\s-*\\([!#]\\|$\\)")
		 ;; Skip comments and empty lines.
		 )
		((looking-at
		  "[ \t]*\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)")
		 (setf (gethash (downcase (match-string 4)) hash)
		       (format "#%02x%02x%02x"
			       (string-to-number (match-string 1))
			       (string-to-number (match-string 2))
			       (string-to-number (match-string 3)))))
		(t
		 (error
		  "Unrecognized line in %s: %s"
		  rgb-file
		  (buffer-substring (point) (progn (end-of-line) (point))))))
	  (forward-line 1))))
    hash))

;; Compile the RGB map when loaded.  On systems where rgb.txt is
;; missing, the value of the variable will be nil, and rgb.txt will
;; not be used.
(defvar htmlize-color-rgb-hash (htmlize-get-color-rgb-hash))

;;; Face handling.

(defun htmlize-face-specifies-property (face prop)
  ;; Return t if face specifies PROP, as opposed to it being inherited
  ;; from the default face.  The problem with e.g.
  ;; `face-foreground-instance' is that it returns an instance for
  ;; EVERY face because every face inherits from the default face.
  ;; However, we'd like htmlize-face-{fore,back}ground to return nil
  ;; when called with a face that doesn't specify its own foreground
  ;; or background.
  (or (eq face 'default)
      (assq 'global (specifier-spec-list (face-property face prop)))))

(defun htmlize-face-color-internal (face fg)
  ;; Used only under GNU Emacs.  Return the color of FACE, but don't
  ;; return "unspecified-fg" or "unspecified-bg".  If the face is
  ;; `default' and the color is unspecified, look up the color in
  ;; frame parameters.
  (let* ((function (if fg #'face-foreground #'face-background))
	 color)
    (if (>= emacs-major-version 22)
	;; For GNU Emacs 22+ set INHERIT to get the inherited values.
	(setq color (funcall function face nil t))
      (setq color (funcall function face))
      ;; For GNU Emacs 21 (which has `face-attribute'): if the color
      ;; is nil, recursively check for the face's parent.
      (when (and (null color)
		 (fboundp 'face-attribute)
		 (face-attribute face :inherit)
		 (not (eq (face-attribute face :inherit) 'unspecified)))
	(setq color (htmlize-face-color-internal
		     (face-attribute face :inherit) fg))))
    (when (and (eq face 'default) (null color))
      (setq color (cdr (assq (if fg 'foreground-color 'background-color)
			     (frame-parameters)))))
    (when (or (eq color 'unspecified)
	      (equal color "unspecified-fg")
	      (equal color "unspecified-bg"))
      (setq color nil))
    (when (and (eq face 'default)
	       (null color))
      ;; Assuming black on white doesn't seem right, but I can't think
      ;; of anything better to do.
      (setq color (if fg "black" "white")))
    color))

(defun htmlize-face-foreground (face)
  ;; Return the name of the foreground color of FACE.  If FACE does
  ;; not specify a foreground color, return nil.
  (cond (htmlize-running-xemacs
	 ;; XEmacs.
	 (and (htmlize-face-specifies-property face 'foreground)
	      (color-instance-name (face-foreground-instance face))))
	(t
	 ;; GNU Emacs.
	 (htmlize-face-color-internal face t))))

(defun htmlize-face-background (face)
  ;; Return the name of the background color of FACE.  If FACE does
  ;; not specify a background color, return nil.
  (cond (htmlize-running-xemacs
	 ;; XEmacs.
	 (and (htmlize-face-specifies-property face 'background)
	      (color-instance-name (face-background-instance face))))
	(t
	 ;; GNU Emacs.
	 (htmlize-face-color-internal face nil))))

;; Convert COLOR to the #RRGGBB string.  If COLOR is already in that
;; format, it's left unchanged.

(defun htmlize-color-to-rgb (color)
  (let ((rgb-string nil))
    (cond ((null color)
	   ;; Ignore nil COLOR because it means that the face is not
	   ;; specifying any color.  Hence (htmlize-color-to-rgb nil)
	   ;; returns nil.
	   )
	  ((string-match "\\`#" color)
	   ;; The color is already in #rrggbb format.
	   (setq rgb-string color))
	  ((and htmlize-use-rgb-txt
		htmlize-color-rgb-hash)
	   ;; Use of rgb.txt is requested, and it's available on the
	   ;; system.  Use it.
	   (setq rgb-string (gethash (downcase color) htmlize-color-rgb-hash)))
	  (t
	   ;; We're getting the RGB components from Emacs.
	   (let ((rgb
		  ;; Here I cannot conditionalize on (fboundp ...)
		  ;; because ps-print under some versions of GNU Emacs
		  ;; defines its own dummy version of
		  ;; `color-instance-rgb-components'.
		  (if htmlize-running-xemacs
		      (mapcar (lambda (arg)
				(/ arg 256))
			      (color-instance-rgb-components
			       (make-color-instance color)))
		    (mapcar (lambda (arg)
			      (/ arg 256))
			    (x-color-values color)))))
	     (when rgb
	       (setq rgb-string (apply #'format "#%02x%02x%02x" rgb))))))
    ;; If RGB-STRING is still nil, it means the color cannot be found,
    ;; for whatever reason.  In that case just punt and return COLOR.
    ;; Most browsers support a decent set of color names anyway.
    (or rgb-string color)))

;; We store the face properties we care about into an
;; `htmlize-fstruct' type.  That way we only have to analyze face
;; properties, which can be time consuming, once per each face.  The
;; mapping between Emacs faces and htmlize-fstructs is established by
;; htmlize-make-face-map.  The name "fstruct" refers to variables of
;; type `htmlize-fstruct', while the term "face" is reserved for Emacs
;; faces.

(defstruct htmlize-fstruct
  foreground				; foreground color, #rrggbb
  background				; background color, #rrggbb
  size					; size
  boldp					; whether face is bold
  italicp				; whether face is italic
  underlinep				; whether face is underlined
  overlinep				; whether face is overlined
  strikep				; whether face is struck through
  css-name				; CSS name of face
  )

(defun htmlize-face-emacs21-attr (fstruct attr value)
  ;; For ATTR and VALUE, set the equivalent value in FSTRUCT.
  (case attr
    (:foreground
     (setf (htmlize-fstruct-foreground fstruct) (htmlize-color-to-rgb value)))
    (:background
     (setf (htmlize-fstruct-background fstruct) (htmlize-color-to-rgb value)))
    (:height
     (setf (htmlize-fstruct-size fstruct) value))
    (:weight
     (when (string-match (symbol-name value) "bold")
       (setf (htmlize-fstruct-boldp fstruct) t)))
    (:slant
     (setf (htmlize-fstruct-italicp fstruct) (or (eq value 'italic)
						 (eq value 'oblique))))
    (:bold
     (setf (htmlize-fstruct-boldp fstruct) value))
    (:italic
     (setf (htmlize-fstruct-italicp fstruct) value))
    (:underline
     (setf (htmlize-fstruct-underlinep fstruct) value))
    (:overline
     (setf (htmlize-fstruct-overlinep fstruct) value))
    (:strike-through
     (setf (htmlize-fstruct-strikep fstruct) value))))

(defun htmlize-face-size (face)
  ;; The size (height) of FACE, taking inheritance into account.
  ;; Only works in Emacs 21 and later.
  (let ((size-list
	 (loop
	  for f = face then (ignore-errors (face-attribute f :inherit)) ;?????
	  until (or (not f) (eq f 'unspecified))
	  for h = (ignore-errors (face-attribute f :height)) ;???????
	  collect (if (eq h 'unspecified) nil h))))
    (reduce 'htmlize-merge-size (cons nil size-list))))

(defun htmlize-face-to-fstruct (face)
  "Convert Emacs face FACE to fstruct."
  (let ((fstruct (make-htmlize-fstruct
		  :foreground (htmlize-color-to-rgb
			       (htmlize-face-foreground face))
		  :background (htmlize-color-to-rgb
			       (htmlize-face-background face)))))
    (cond (htmlize-running-xemacs
	   ;; XEmacs doesn't provide a way to detect whether a face is
	   ;; bold or italic, so we need to examine the font instance.
	   ;; #### This probably doesn't work under MS Windows and/or
	   ;; GTK devices.  I'll need help with those.
	   (let* ((font-instance (face-font-instance face))
		  (props (font-instance-properties font-instance)))
	     (when (equalp (cdr (assq 'WEIGHT_NAME props)) "bold")
	       (setf (htmlize-fstruct-boldp fstruct) t))
	     (when (or (equalp (cdr (assq 'SLANT props)) "i")
		       (equalp (cdr (assq 'SLANT props)) "o"))
	       (setf (htmlize-fstruct-italicp fstruct) t))
	     (setf (htmlize-fstruct-strikep fstruct)
		   (face-strikethru-p face))
	     (setf (htmlize-fstruct-underlinep fstruct)
		   (face-underline-p face))))
	  ((fboundp 'face-attribute)
	   ;; GNU Emacs 21 and further.
	   (dolist (attr '(:weight :slant :underline :overline :strike-through))
	     (let ((value (if (>= emacs-major-version 22)
			      ;; Use the INHERIT arg in GNU Emacs 22.
			      (face-attribute face attr nil t)
			    ;; Otherwise, fake it.
			    (let ((face face))
			      (while (and (eq (face-attribute face attr)
					      'unspecified)
					  (not (eq (face-attribute face :inherit)
						   'unspecified)))
				(setq face (face-attribute face :inherit)))
			      (face-attribute face attr)))))
	       (when (and value (not (eq value 'unspecified)))
		 (htmlize-face-emacs21-attr fstruct attr value))))
	   (let ((size (htmlize-face-size face)))
	     (unless (eql size 1.0) 	; ignore non-spec
	       (setf (htmlize-fstruct-size fstruct) size))))
	  (t
	   ;; Older GNU Emacs.  Some of these functions are only
	   ;; available under Emacs 20+, hence the guards.
	   (when (fboundp 'face-bold-p)
	     (setf (htmlize-fstruct-boldp fstruct) (face-bold-p face)))
	   (when (fboundp 'face-italic-p)
	     (setf (htmlize-fstruct-italicp fstruct) (face-italic-p face)))
	   (setf (htmlize-fstruct-underlinep fstruct)
		 (face-underline-p face))))
    ;; Generate the css-name property.  Emacs places no restrictions
    ;; on the names of symbols that represent faces -- any characters
    ;; may be in the name, even ^@.  We try hard to beat the face name
    ;; into shape, both esthetically and according to CSS1 specs.
    (setf (htmlize-fstruct-css-name fstruct)
	  (let ((name (downcase (symbol-name face))))
	    (when (string-match "\\`font-lock-" name)
	      ;; Change font-lock-FOO-face to FOO.
	      (setq name (replace-match "" t t name)))
	    (when (string-match "-face\\'" name)
	      ;; Drop the redundant "-face" suffix.
	      (setq name (replace-match "" t t name)))
	    (while (string-match "[^-a-zA-Z0-9]" name)
	      ;; Drop the non-alphanumerics.
	      (setq name (replace-match "X" t t name)))
	    (when (string-match "\\`[-0-9]" name)
	      ;; CSS identifiers may not start with a digit.
	      (setq name (concat "X" name)))
	    ;; After these transformations, the face could come
	    ;; out empty.
	    (when (equal name "")
	      (setq name "face"))
	    ;; Apply the prefix.
	    (setq name (concat htmlize-css-name-prefix name))
	    name))
    fstruct))

(defmacro htmlize-copy-attr-if-set (attr-list dest source)
  ;; Expand the code of the type
  ;; (and (htmlize-fstruct-ATTR source)
  ;;      (setf (htmlize-fstruct-ATTR dest) (htmlize-fstruct-ATTR source)))
  ;; for the given list of boolean attributes.
  (cons 'progn
	(loop for attr in attr-list
	      for attr-sym = (intern (format "htmlize-fstruct-%s" attr))
	      collect `(and (,attr-sym ,source)
			    (setf (,attr-sym ,dest) (,attr-sym ,source))))))

(defun htmlize-merge-size (merged next)
  ;; Calculate the size of the merge of MERGED and NEXT.
  (cond ((null merged)     next)
	((integerp next)   next)
	((null next)       merged)
	((floatp merged)   (* merged next))
	((integerp merged) (round (* merged next)))))

(defun htmlize-merge-two-faces (merged next)
  (htmlize-copy-attr-if-set
   (foreground background boldp italicp underlinep overlinep strikep)
   merged next)
  (setf (htmlize-fstruct-size merged)
	(htmlize-merge-size (htmlize-fstruct-size merged)
			    (htmlize-fstruct-size next)))
  merged)

(defun htmlize-merge-faces (fstruct-list)
  (cond ((null fstruct-list)
	 ;; Nothing to do, return a dummy face.
	 (make-htmlize-fstruct))
	((null (cdr fstruct-list))
	 ;; Optimize for the common case of a single face, simply
	 ;; return it.
	 (car fstruct-list))
	(t
	 (reduce #'htmlize-merge-two-faces
		 (cons (make-htmlize-fstruct) fstruct-list)))))

;; GNU Emacs 20+ supports attribute lists in `face' properties.  For
;; example, you can use `(:foreground "red" :weight bold)' as an
;; overlay's "face", or you can even use a list of such lists, etc.
;; We call those "attrlists".
;;
;; htmlize supports attrlist by converting them to fstructs, the same
;; as with regular faces.

(defun htmlize-attrlist-to-fstruct (attrlist)
  ;; Like htmlize-face-to-fstruct, but accepts an ATTRLIST as input.
  (let ((fstruct (make-htmlize-fstruct)))
    (cond ((eq (car attrlist) 'foreground-color)
	   ;; ATTRLIST is (foreground-color . COLOR)
	   (setf (htmlize-fstruct-foreground fstruct)
		 (htmlize-color-to-rgb (cdr attrlist))))
	  ((eq (car attrlist) 'background-color)
	   ;; ATTRLIST is (background-color . COLOR)
	   (setf (htmlize-fstruct-background fstruct)
		 (htmlize-color-to-rgb (cdr attrlist))))
	  (t
	   ;; ATTRLIST is a plist.
	   (while attrlist
	     (let ((attr (pop attrlist))
		   (value (pop attrlist)))
	       (when (and value (not (eq value 'unspecified)))
		 (htmlize-face-emacs21-attr fstruct attr value))))))
    (setf (htmlize-fstruct-css-name fstruct) "ATTRLIST")
    fstruct))

(defun htmlize-face-list-p (face-prop)
  "Return non-nil if FACE-PROP is a list of faces, nil otherwise."
  ;; If not for attrlists, this would return (listp face-prop).  This
  ;; way we have to be more careful because attrlist is also a list!
  (cond
   ((eq face-prop nil)
    ;; FACE-PROP being nil means empty list (no face), so return t.
    t)
   ((symbolp face-prop)
    ;; A symbol other than nil means that it's only one face, so return
    ;; nil.
    nil)
   ((not (consp face-prop))
    ;; Huh?  Not a symbol or cons -- treat it as a single element.
    nil)
   (t
    ;; We know that FACE-PROP is a cons: check whether it looks like an
    ;; ATTRLIST.
    (let* ((car (car face-prop))
	   (attrlist-p (and (symbolp car)
			    (or (eq car 'foreground-color)
				(eq car 'background-color)
				(eq (aref (symbol-name car) 0) ?:)))))
      ;; If FACE-PROP is not an ATTRLIST, it means it's a list of
      ;; faces.
      (not attrlist-p)))))

(defun htmlize-make-face-map (faces)
  ;; Return a hash table mapping Emacs faces to htmlize's fstructs.
  ;; The keys are either face symbols or attrlists, so the test
  ;; function must be `equal'.
  (let ((face-map (make-hash-table :test 'equal))
	css-names)
    (dolist (face faces)
      (unless (gethash face face-map)
	;; Haven't seen FACE yet; convert it to an fstruct and cache
	;; it.
	(let ((fstruct (if (symbolp face)
			   (htmlize-face-to-fstruct face)
			 (htmlize-attrlist-to-fstruct face))))
	  (setf (gethash face face-map) fstruct)
	  (let* ((css-name (htmlize-fstruct-css-name fstruct))
		 (new-name css-name)
		 (i 0))
	    ;; Uniquify the face's css-name by using NAME-1, NAME-2,
	    ;; etc.
	    (while (member new-name css-names)
	      (setq new-name (format "%s-%s" css-name (incf i))))
	    (unless (equal new-name css-name)
	      (setf (htmlize-fstruct-css-name fstruct) new-name))
	    (push new-name css-names)))))
    face-map))

(defun htmlize-unstringify-face (face)
  "If FACE is a string, return it interned, otherwise return it unchanged."
  (if (stringp face)
      (intern face)
    face))

(defun htmlize-faces-in-buffer ()
  "Return a list of faces used in the current buffer.
Under XEmacs, this returns the set of faces specified by the extents
with the `face' property.  (This covers text properties as well.)  Under
GNU Emacs, it returns the set of faces specified by the `face' text
property and by buffer overlays that specify `face'."
  (let (faces)
    ;; Testing for (fboundp 'map-extents) doesn't work because W3
    ;; defines `map-extents' under FSF.
    (if htmlize-running-xemacs
	(let (face-prop)
	  (map-extents (lambda (extent ignored)
			 (setq face-prop (extent-face extent)
			       ;; FACE-PROP can be a face or a list of
			       ;; faces.
			       faces (if (listp face-prop)
					 (union face-prop faces)
				       (adjoin face-prop faces)))
			 nil)
		       nil
		       ;; Specify endpoints explicitly to respect
		       ;; narrowing.
		       (point-min) (point-max) nil nil 'face))
      ;; FSF Emacs code.
      ;; Faces used by text properties.
      (let ((pos (point-min)) face-prop next)
	(while (< pos (point-max))
	  (setq face-prop (get-text-property pos 'face)
		next (or (next-single-property-change pos 'face) (point-max)))
	  ;; FACE-PROP can be a face/attrlist or a list thereof.
	  (setq faces (if (htmlize-face-list-p face-prop)
			  (nunion (mapcar #'htmlize-unstringify-face face-prop)
				  faces :test 'equal)
			(adjoin (htmlize-unstringify-face face-prop)
				faces :test 'equal)))
	  (setq pos next)))
      ;; Faces used by overlays.
      (dolist (overlay (overlays-in (point-min) (point-max)))
	(let ((face-prop (overlay-get overlay 'face)))
	  ;; FACE-PROP can be a face/attrlist or a list thereof.
	  (setq faces (if (htmlize-face-list-p face-prop)
			  (nunion (mapcar #'htmlize-unstringify-face face-prop)
				  faces :test 'equal)
			(adjoin (htmlize-unstringify-face face-prop)
				faces :test 'equal))))))
    faces))

;; htmlize-faces-at-point returns the faces in use at point.  The
;; faces are sorted by increasing priority, i.e. the last face takes
;; precedence.
;;
;; Under XEmacs, this returns all the faces in all the extents at
;; point.  Under GNU Emacs, this returns all the faces in the `face'
;; property and all the faces in the overlays at point.

(cond (htmlize-running-xemacs
       (defun htmlize-faces-at-point ()
	 (let (extent extent-list face-list face-prop)
	   (while (setq extent (extent-at (point) nil 'face extent))
	     (push extent extent-list))
	   ;; extent-list is in reverse display order, meaning that
	   ;; smallest ones come last.  That is the order we want,
	   ;; except it can be overridden by the `priority' property.
	   (setq extent-list (stable-sort extent-list #'<
					  :key #'extent-priority))
	   (dolist (extent extent-list)
	     (setq face-prop (extent-face extent))
	     ;; extent's face-list is in reverse order from what we
	     ;; want, but the `nreverse' below will take care of it.
	     (setq face-list (if (listp face-prop)
				 (append face-prop face-list)
			       (cons face-prop face-list))))
	   (nreverse face-list))))
      (t
       (defun htmlize-faces-at-point ()
	 (let (all-faces)
	   ;; Faces from text properties.
	   (let ((face-prop (get-text-property (point) 'face)))
	     (setq all-faces (if (htmlize-face-list-p face-prop)
				 (nreverse (mapcar #'htmlize-unstringify-face
						   face-prop))
			       (list (htmlize-unstringify-face face-prop)))))
	   ;; Faces from overlays.
	   (let ((overlays
		  ;; Collect overlays at point that specify `face'.
		  (delete-if-not (lambda (o)
				   (overlay-get o 'face))
				 (overlays-at (point))))
		 list face-prop)
	     ;; Sort the overlays so the smaller (more specific) ones
	     ;; come later.  The number of overlays at each one
	     ;; position should be very small, so the sort shouldn't
	     ;; slow things down.
	     (setq overlays (sort* overlays
				   ;; Sort by ascending...
				   #'<
				   ;; ...overlay size.
				   :key (lambda (o)
					  (- (overlay-end o)
					     (overlay-start o)))))
	     ;; Overlay priorities, if present, override the above
	     ;; established order.  Larger overlay priority takes
	     ;; precedence and therefore comes later in the list.
	     (setq overlays (stable-sort
			     overlays
			     ;; Reorder (stably) by acending...
			     #'<
			     ;; ...overlay priority.
			     :key (lambda (o)
				    (or (overlay-get o 'priority) 0))))
	     (dolist (overlay overlays)
	       (setq face-prop (overlay-get overlay 'face))
	       (setq list (if (htmlize-face-list-p face-prop)
			      (nconc (nreverse (mapcar
						#'htmlize-unstringify-face
						face-prop))
				     list)
			    (cons (htmlize-unstringify-face face-prop) list))))
	     ;; Under "Merging Faces" the manual explicitly states
	     ;; that faces specified by overlays take precedence over
	     ;; faces specified by text properties.
	     (setq all-faces (nconc all-faces list)))
	   all-faces))))

;; htmlize supports generating HTML in two several fundamentally
;; different ways, one with the use of CSS and nested <span> tags, and
;; the other with the use of the old <font> tags.  Rather than adding
;; a bunch of ifs to many places, we take a semi-OO approach.
;; `htmlize-buffer-1' calls a number of "methods", which indirect to
;; the functions that depend on `htmlize-output-type'.  The currently
;; used methods are `doctype', `insert-head', `body-tag', and
;; `insert-text'.  Not all output types define all methods.
;;
;; Methods are called either with (htmlize-method METHOD ARGS...)
;; special form, or by accessing the function with
;; (htmlize-method-function 'METHOD) and calling (funcall FUNCTION).
;; The latter form is useful in tight loops because `htmlize-method'
;; conses.
;;
;; Currently defined output types are `css' and `font'.

(defmacro htmlize-method (method &rest args)
  ;; Expand to (htmlize-TYPE-METHOD ...ARGS...).  TYPE is the value of
  ;; `htmlize-output-type' at run time.
  `(funcall (htmlize-method-function ',method) ,@args))

(defun htmlize-method-function (method)
  ;; Return METHOD's function definition for the current output type.
  ;; The returned object can be safely funcalled.
  (let ((sym (intern (format "htmlize-%s-%s" htmlize-output-type method))))
    (indirect-function (if (fboundp sym)
			   sym
			 (let ((default (intern (concat "htmlize-default-"
							(symbol-name method)))))
			   (if (fboundp default)
			       default
			     'ignore))))))

(defvar htmlize-memoization-table (make-hash-table :test 'equal))

(defmacro htmlize-memoize (key generator)
  "Return the value of GENERATOR, memoized as KEY.
That means that GENERATOR will be evaluated and returned the first time
it's called with the same value of KEY.  All other times, the cached
\(memoized) value will be returned."
  (let ((value (gensym)))
    `(let ((,value (gethash ,key htmlize-memoization-table)))
       (unless ,value
	 (setq ,value ,generator)
	 (setf (gethash ,key htmlize-memoization-table) ,value))
       ,value)))

;;; Default methods.

(defun htmlize-default-doctype ()
  nil					; no doc-string
  ;; According to DTDs published by the W3C, it is illegal to embed
  ;; <font> in <pre>.  This makes sense in general, but is bad for
  ;; htmlize's intended usage of <font> to specify the document color.

  ;; To make generated HTML legal, htmlize's `font' mode used to
  ;; specify the SGML declaration of "HTML Pro" DTD here.  HTML Pro
  ;; aka Silmaril DTD was a project whose goal was to produce a GPL'ed
  ;; DTD that would encompass all the incompatible HTML extensions
  ;; procured by Netscape, MSIE, and other players in the field.
  ;; Apparently the project got abandoned, the last available version
  ;; being "Draft 0 Revision 11" from January 1997, as documented at
  ;; <http://imbolc.ucc.ie/~pflynn/articles/htmlpro.html>.

  ;; Since by now HTML Pro is remembered by none but the most die-hard
  ;; early-web-days nostalgics and used by not even them, there is no
  ;; use in specifying it.  So we return the standard HTML 4.0
  ;; declaration, which makes generated HTML technically illegal.  If
  ;; you have a problem with that, use the `css' engine designed to
  ;; create fully conforming HTML.

  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"

  ;; Now-abandoned HTML Pro declaration.
  ;"<!DOCTYPE HTML PUBLIC \"+//Silmaril//DTD HTML Pro v0r11 19970101//EN\">"
  )

(defun htmlize-default-body-tag (face-map)
  nil					; no doc-string
  "<body>")

;;; CSS based output support.

;; Internal function; not a method.
(defun htmlize-css-specs (fstruct)
  (let (result)
    (when (htmlize-fstruct-foreground fstruct)
      (push (format "color: %s;" (htmlize-fstruct-foreground fstruct))
	    result))
    (when (htmlize-fstruct-background fstruct)
      (push (format "background-color: %s;"
		    (htmlize-fstruct-background fstruct))
	    result))
    (let ((size (htmlize-fstruct-size fstruct)))
      (when (and size (not (eq htmlize-ignore-face-size t)))
	(cond ((floatp size)
	       (push (format "font-size: %d%%;" (* 100 size)) result))
	      ((not (eq htmlize-ignore-face-size 'absolute))
	       (push (format "font-size: %spt;" (/ size 10.0)) result)))))
    (when (htmlize-fstruct-boldp fstruct)
      (push "font-weight: bold;" result))
    (when (htmlize-fstruct-italicp fstruct)
      (push "font-style: italic;" result))
    (when (htmlize-fstruct-underlinep fstruct)
      (push "text-decoration: underline;" result))
    (when (htmlize-fstruct-overlinep fstruct)
      (push "text-decoration: overline;" result))
    (when (htmlize-fstruct-strikep fstruct)
      (push "text-decoration: line-through;" result))
    (nreverse result)))

(defun htmlize-css-insert-head (buffer-faces face-map)
  (insert "    <style type=\"text/css\">\n    <!--\n")
  (insert "      body {\n        "
	  (mapconcat #'identity
		     (htmlize-css-specs (gethash 'default face-map))
		     "\n        ")
	  "\n      }\n")
  (dolist (face (sort* (copy-list buffer-faces) #'string-lessp
		       :key (lambda (f)
			      (htmlize-fstruct-css-name (gethash f face-map)))))
    (let* ((fstruct (gethash face face-map))
	   (cleaned-up-face-name
	    (let ((s
		   ;; Use `prin1-to-string' rather than `symbol-name'
		   ;; to get the face name because the "face" can also
		   ;; be an attrlist, which is not a symbol.
		   (prin1-to-string face)))
	      ;; If the name contains `--' or `*/', remove them.
	      (while (string-match "--" s)
		(setq s (replace-match "-" t t s)))
	      (while (string-match "\\*/" s)
		(setq s (replace-match "XX" t t s)))
	      s))
	   (specs (htmlize-css-specs fstruct)))
      (insert "      ." (htmlize-fstruct-css-name fstruct))
      (if (null specs)
	  (insert " {")
	(insert " {\n        /* " cleaned-up-face-name " */\n        "
		(mapconcat #'identity specs "\n        ")))
      (insert "\n      }\n")))
  (insert htmlize-hyperlink-style
	  "    -->\n    </style>\n"))

(defun htmlize-css-insert-text (text fstruct-list buffer)
  ;; Insert TEXT colored with FACES into BUFFER.  In CSS mode, this is
  ;; easy: just nest the text in one <span class=...> tag for each
  ;; face in FSTRUCT-LIST.
  (dolist (fstruct fstruct-list)
    (princ "<span class=\"" buffer)
    (princ (htmlize-fstruct-css-name fstruct) buffer)
    (princ "\">" buffer))
  (princ text buffer)
  (dolist (fstruct fstruct-list)
    (ignore fstruct)			; shut up the byte-compiler
    (princ "</span>" buffer)))

;; `inline-css' output support.

(defun htmlize-inline-css-body-tag (face-map)
  (format "<body style=\"%s\">"
	  (mapconcat #'identity (htmlize-css-specs (gethash 'default face-map))
		     " ")))

(defun htmlize-inline-css-insert-text (text fstruct-list buffer)
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (style (htmlize-memoize
		 merged
		 (let ((specs (htmlize-css-specs merged)))
		   (and specs
			(mapconcat #'identity (htmlize-css-specs merged) " "))))))
    (when style
      (princ "<span style=\"" buffer)
      (princ style buffer)
      (princ "\">" buffer))
    (princ text buffer)
    (when style
      (princ "</span>" buffer))))

;;; `font' tag based output support.

(defun htmlize-font-body-tag (face-map)
  (let ((fstruct (gethash 'default face-map)))
    (format "<body text=\"%s\" bgcolor=\"%s\">"
	    (htmlize-fstruct-foreground fstruct)
	    (htmlize-fstruct-background fstruct))))

(defun htmlize-font-insert-text (text fstruct-list buffer)
  ;; In `font' mode, we use the traditional HTML means of altering
  ;; presentation: <font> tag for colors, <b> for bold, <u> for
  ;; underline, and <strike> for strike-through.
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (markup (htmlize-memoize
		  merged
		  (cons (concat
			 (and (htmlize-fstruct-foreground merged)
			      (format "<font color=\"%s\">" (htmlize-fstruct-foreground merged)))
			 (and (htmlize-fstruct-boldp merged)      "<b>")
			 (and (htmlize-fstruct-italicp merged)    "<i>")
			 (and (htmlize-fstruct-underlinep merged) "<u>")
			 (and (htmlize-fstruct-strikep merged)    "<strike>"))
			(concat
			 (and (htmlize-fstruct-strikep merged)    "</strike>")
			 (and (htmlize-fstruct-underlinep merged) "</u>")
			 (and (htmlize-fstruct-italicp merged)    "</i>")
			 (and (htmlize-fstruct-boldp merged)      "</b>")
			 (and (htmlize-fstruct-foreground merged) "</font>"))))))
    (princ (car markup) buffer)
    (princ text buffer)
    (princ (cdr markup) buffer)))

(defun htmlize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Htmlize
  ;; current buffer, writing the resulting HTML to a new buffer, and
  ;; return it.  Unlike htmlize-buffer, this doesn't change current
  ;; buffer or use switch-to-buffer.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    (let* ((buffer-faces (htmlize-faces-in-buffer))
	   (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
	   ;; Generate the new buffer.  It's important that it inherits
	   ;; default-directory from the current buffer.
	   (htmlbuf (generate-new-buffer (if (buffer-file-name)
					     (htmlize-make-file-name
					      (file-name-nondirectory
					       (buffer-file-name)))
					   "*html*")))
	   ;; Having a dummy value in the plist allows writing simply
	   ;; (plist-put places foo bar).
	   (places '(nil nil))
	   (title (if (buffer-file-name)
		      (file-name-nondirectory (buffer-file-name))
		    (buffer-name))))
      ;; Initialize HTMLBUF and insert the HTML prolog.
      (with-current-buffer htmlbuf
	(buffer-disable-undo)
	(insert (htmlize-method doctype) ?\n
		(format "<!-- Created by htmlize-%s in %s mode. -->\n"
			htmlize-version htmlize-output-type)
		"<html>\n  ")
	(plist-put places 'head-start (point-marker))
	(insert "<head>\n"
		"    <title>" (htmlize-protect-string title) "</title>\n"
		(if htmlize-html-charset
		    (format (concat "    <meta http-equiv=\"Content-Type\" "
				    "content=\"text/html; charset=%s\">\n")
			    htmlize-html-charset)
		  "")
		htmlize-head-tags)
	(htmlize-method insert-head buffer-faces face-map)
	(insert "  </head>")
	(plist-put places 'head-end (point-marker))
	(insert "\n  ")
	(plist-put places 'body-start (point-marker))
	(insert (htmlize-method body-tag face-map)
		"\n    ")
	(plist-put places 'content-start (point-marker))
	(insert "<pre>\n"))
      (let ((insert-text-method
	     ;; Get the inserter method, so we can funcall it inside
	     ;; the loop.  Not calling `htmlize-method' in the loop
	     ;; body yields a measurable speed increase.
	     (htmlize-method-function 'insert-text))
	    ;; Declare variables used in loop body outside the loop
	    ;; because it's faster to establish `let' bindings only
	    ;; once.
	    next-change text face-list fstruct-list trailing-ellipsis)
	;; This loop traverses and reads the source buffer, appending
	;; the resulting HTML to HTMLBUF with `princ'.  This method is
	;; fast because: 1) it doesn't require examining the text
	;; properties char by char (htmlize-next-change is used to
	;; move between runs with the same face), and 2) it doesn't
	;; require buffer switches, which are slow in Emacs.
	(goto-char (point-min))
	(while (not (eobp))
	  (setq next-change (htmlize-next-change (point) 'face))
	  ;; Get faces in use between (point) and NEXT-CHANGE, and
	  ;; convert them to fstructs.
	  (setq face-list (htmlize-faces-at-point)
		fstruct-list (delq nil (mapcar (lambda (f)
						 (gethash f face-map))
					       face-list)))
	  ;; Extract buffer text, sans the invisible parts.  Then
	  ;; untabify it and escape the HTML metacharacters.
	  (setq text (htmlize-buffer-substring-no-invisible
		      (point) next-change))
	  (when trailing-ellipsis
	    (setq text (htmlize-trim-ellipsis text)))
	  ;; If TEXT ends up empty, don't change trailing-ellipsis.
	  (when (> (length text) 0)
	    (setq trailing-ellipsis
		  (get-text-property (1- (length text))
				     'htmlize-ellipsis text)))
	  (setq text (htmlize-untabify text (current-column)))
	  (setq text (htmlize-protect-string text))
	  ;; Don't bother writing anything if there's no text (this
	  ;; happens in invisible regions).
	  (when (> (length text) 0)
	    ;; Insert the text, along with the necessary markup to
	    ;; represent faces in FSTRUCT-LIST.
	    (funcall insert-text-method text fstruct-list htmlbuf))
	  (goto-char next-change)))

      ;; Insert the epilog and post-process the buffer.
      (with-current-buffer htmlbuf
	(insert "</pre>")
	(plist-put places 'content-end (point-marker))
	(insert "\n  </body>")
	(plist-put places 'body-end (point-marker))
	(insert "\n</html>\n")
	(when htmlize-generate-hyperlinks
	  (htmlize-make-hyperlinks))
	(htmlize-defang-local-variables)
	(when htmlize-replace-form-feeds
	  ;; Change each "\n^L" to "<hr />".
	  (goto-char (point-min))
	  (let ((source
		 ;; ^L has already been escaped, so search for that.
		 (htmlize-protect-string "\n\^L"))
		(replacement
		 (if (stringp htmlize-replace-form-feeds)
		     htmlize-replace-form-feeds
		   "</pre><hr /><pre>")))
	    (while (search-forward source nil t)
	      (replace-match replacement t t))))
	(goto-char (point-min))
	(when htmlize-html-major-mode
	  ;; What sucks about this is that the minor modes, most notably
	  ;; font-lock-mode, won't be initialized.  Oh well.
	  (funcall htmlize-html-major-mode))
	(set (make-local-variable 'htmlize-buffer-places) places)
	(run-hooks 'htmlize-after-hook)
	(buffer-enable-undo))
      htmlbuf)))

;; Utility functions.

(defmacro htmlize-with-fontify-message (&rest body)
  ;; When forcing fontification of large buffers in
  ;; htmlize-ensure-fontified, inform the user that he is waiting for
  ;; font-lock, not for htmlize to finish.
  `(progn
     (if (> (buffer-size) 65536)
	 (message "Forcing fontification of %s..."
		  (buffer-name (current-buffer))))
     ,@body
     (if (> (buffer-size) 65536)
	 (message "Forcing fontification of %s...done"
		  (buffer-name (current-buffer))))))

(defun htmlize-ensure-fontified ()
  ;; If font-lock is being used, ensure that the "support" modes
  ;; actually fontify the buffer.  If font-lock is not in use, we
  ;; don't care because, except in htmlize-file, we don't force
  ;; font-lock on the user.
  (when (and (boundp 'font-lock-mode)
	     font-lock-mode)
    ;; In part taken from ps-print-ensure-fontified in GNU Emacs 21.
    (cond
     ((and (boundp 'jit-lock-mode)
	   (symbol-value 'jit-lock-mode))
      (htmlize-with-fontify-message
       (jit-lock-fontify-now (point-min) (point-max))))
     ((and (boundp 'lazy-lock-mode)
	   (symbol-value 'lazy-lock-mode))
      (htmlize-with-fontify-message
       (lazy-lock-fontify-region (point-min) (point-max))))
     ((and (boundp 'lazy-shot-mode)
	   (symbol-value 'lazy-shot-mode))
      (htmlize-with-fontify-message
       ;; lazy-shot is amazing in that it must *refontify* the region,
       ;; even if the whole buffer has already been fontified.  <sigh>
       (lazy-shot-fontify-region (point-min) (point-max))))
     ;; There's also fast-lock, but we don't need to handle specially,
     ;; I think.  fast-lock doesn't really defer fontification, it
     ;; just saves it to an external cache so it's not done twice.
     )))


;;;###autoload
(defun htmlize-buffer (&optional buffer)
  "Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses."
  (interactive)
  (let ((htmlbuf (with-current-buffer (or buffer (current-buffer))
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

;;;###autoload
(defun htmlize-region (beg end)
  "Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details."
  (interactive "r")
  ;; Don't let zmacs region highlighting end up in HTML.
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (let ((htmlbuf (save-restriction
		   (narrow-to-region beg end)
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

(defun htmlize-region-for-paste (beg end)
  "Htmlize the region and return just the HTML as a string.
This forces the `inline-css' style and only returns the HTML body,
but without the BODY tag.  This should make it useful for inserting
the text to another HTML buffer."
  (let* ((htmlize-output-type 'inline-css)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

(defun htmlize-make-file-name (file)
  "Make an HTML file name from FILE.

In its default implementation, this simply appends `.html' to FILE.
This function is called by htmlize to create the buffer file name, and
by `htmlize-file' to create the target file name.

More elaborate transformations are conceivable, such as changing FILE's
extension to `.html' (\"file.c\" -> \"file.html\").  If you want them,
overload this function to do it and htmlize will comply."
  (concat file ".html"))

;; Older implementation of htmlize-make-file-name that changes FILE's
;; extension to ".html".
;(defun htmlize-make-file-name (file)
;  (let ((extension (file-name-extension file))
;	(sans-extension (file-name-sans-extension file)))
;    (if (or (equal extension "html")
;	    (equal extension "htm")
;	    (equal sans-extension ""))
;	(concat file ".html")
;      (concat sans-extension ".html"))))

;;;###autoload
(defun htmlize-file (file &optional target)
  "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
		      "HTML-ize file: "
		      nil nil nil (and (buffer-file-name)
				       (file-name-nondirectory
					(buffer-file-name))))))
  (let ((output-file (if (and target (not (file-directory-p target)))
			 target
		       (expand-file-name
			(htmlize-make-file-name (file-name-nondirectory file))
			(or target (file-name-directory file)))))
	;; Try to prevent `find-file-noselect' from triggering
	;; font-lock because we'll fontify explicitly below.
	(font-lock-mode nil)
	(font-lock-auto-fontify nil)
	(global-font-lock-mode nil)
	;; Ignore the size limit for the purposes of htmlization.
	(font-lock-maximum-size nil)
	;; Disable font-lock support modes.  This will only work in
	;; more recent Emacs versions, so htmlize-buffer-1 still needs
	;; to call htmlize-ensure-fontified.
	(font-lock-support-mode nil))
    (with-temp-buffer
      ;; Insert FILE into the temporary buffer.
      (insert-file-contents file)
      ;; Set the file name so normal-mode and htmlize-buffer-1 pick it
      ;; up.  Restore it afterwards so with-temp-buffer's kill-buffer
      ;; doesn't complain about killing a modified buffer.
      (let ((buffer-file-name file))
	;; Set the major mode for the sake of font-lock.
	(normal-mode)
	(font-lock-mode 1)
	(unless font-lock-mode
	  ;; In GNU Emacs (font-lock-mode 1) doesn't force font-lock,
	  ;; contrary to the documentation.  This seems to work.
	  (font-lock-fontify-buffer))
	;; htmlize the buffer and save the HTML.
	(with-current-buffer (htmlize-buffer-1)
	  (unwind-protect
	      (progn
		(run-hooks 'htmlize-file-hook)
		(write-region (point-min) (point-max) output-file))
	    (kill-buffer (current-buffer)))))))
  ;; I haven't decided on a useful return value yet, so just return
  ;; nil.
  nil)

;;;###autoload
(defun htmlize-many-files (files &optional target-directory)
  "Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file."
  (interactive
   (list
    (let (list file)
      ;; Use empty string as DEFAULT because setting DEFAULT to nil
      ;; defaults to the directory name, which is not what we want.
      (while (not (equal (setq file (read-file-name
				     "HTML-ize file (RET to finish): "
				     (and list (file-name-directory
						(car list)))
				     "" t))
			 ""))
	(push file list))
      (nreverse list))))
  ;; Verify that TARGET-DIRECTORY is indeed a directory.  If it's a
  ;; file, htmlize-file will use it as target, and that doesn't make
  ;; sense.
  (and target-directory
       (not (file-directory-p target-directory))
       (error "target-directory must name a directory: %s" target-directory))
  (dolist (file files)
    (htmlize-file file target-directory)))

;;;###autoload
(defun htmlize-many-files-dired (arg &optional target-directory)
  "HTMLize dired-marked files."
  (interactive "P")
  (htmlize-many-files (dired-get-marked-files nil arg) target-directory))

(provide 'htmlize)

;;; htmlize.el ends here
