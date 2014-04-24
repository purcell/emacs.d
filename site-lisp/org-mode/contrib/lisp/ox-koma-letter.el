;;; ox-koma-letter.el --- KOMA Scrlttr2 Back-End for Org Export Engine

;; Copyright (C) 2007-2012, 2014  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou AT gmail DOT com>
;;         Alan Schmitt <alan.schmitt AT polytechnique DOT org>
;;         Viktor Rosenfeld <listuser36 AT gmail DOT com>
;;         Rasmus Pank Roulund <emacs AT pank DOT eu>
;; Keywords: org, wp, tex

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a KOMA Scrlttr2 back-end, derived from the
;; LaTeX one.
;;
;; Depending on the desired output format, three commands are provided
;; for export: `org-koma-letter-export-as-latex' (temporary buffer),
;; `org-koma-letter-export-to-latex' ("tex" file) and
;; `org-koma-letter-export-to-pdf' ("pdf" file).
;;
;; On top of buffer keywords supported by `latex' back-end (see
;; `org-latex-options-alist'), this back-end introduces the following
;; keywords:
;;   - CLOSING: see `org-koma-letter-closing',
;;   - FROM_ADDRESS: see `org-koma-letter-from-address',
;;   - LCO: see `org-koma-letter-class-option-file',
;;   - OPENING: see `org-koma-letter-opening',
;;   - PHONE_NUMBER: see `org-koma-letter-phone-number',
;;   - SIGNATURE: see `org-koma-letter-signature',
;;   - PLACE: see `org-koma-letter-place',
;;   - TO_ADDRESS:  If unspecified this is set to "\mbox{}".
;;
;; TO_ADDRESS and FROM_ADDRESS can also be specified using heading
;; with the special tags specified in
;; `org-koma-letter-special-tags-in-letter', namely "to" and "from".
;; LaTeX line breaks are not necessary if using these headings.  If
;; both a headline and a keyword specify a to or from address the
;; value is determined in accordance with
;; `org-koma-letter-prefer-special-headings'.
;;
;; A number of OPTIONS settings can be set to change which contents is
;; exported.
;;   - backaddress (see `org-koma-letter-use-backaddress')
;;   - foldmarks (see `org-koma-letter-use-foldmarks')
;;   - phone (see `org-koma-letter-use-phone')
;;   - email (see `org-koma-letter-use-email')
;;   - place (see `org-koma-letter-use-place')
;;   - subject, a list of format options
;;     (see `org-koma-letter-subject-format')
;;   - after-closing-order, a list of the ordering of headings with
;;     special tags after closing (see
;;     `org-koma-letter-special-tags-after-closing')
;;   - after-letter-order, as above, but after the end of the letter
;;     (see `org-koma-letter-special-tags-after-letter').
;;
;; The following variables works differently from the main LaTeX class
;;   - AUTHOR: Default to user-full-name but may be disabled.
;;     (See also `org-koma-letter-author'),
;;   - EMAIL: Same as AUTHOR. (see also `org-koma-letter-email'),
;;
;; Headlines are in general ignored.  However, headlines with special
;; tags can be used for specified contents like postscript (ps),
;; carbon copy (cc), enclosures (encl) and code to be inserted after
;; \end{letter} (after_letter).  Specials tags are defined in
;; `org-koma-letter-special-tags-after-closing' and
;; `org-koma-letter-special-tags-after-letter'.  Currently members of
;; `org-koma-letter-special-tags-after-closing' used as macros and the
;; content of the headline is the argument.
;;
;; Headlines with two and from may also be used rather than the
;; keyword approach described above.  If both a keyword and a headline
;; with information is present precedence is determined by
;; `org-koma-letter-prefer-special-headings'.
;;
;; You will need to add an appropriate association in
;; `org-latex-classes' in order to use the KOMA Scrlttr2 class.
;; The easiest way to do this is by adding
;;
;;   (eval-after-load "ox-koma-letter"
;;   '(org-koma-letter-plug-into-ox))
;;
;; to your init file.  This will add a sparse scrlttr2 class and
;; set it as the default `org-koma-latex-default-class'.  You can also
;; add you own letter class.  For instace:
;;
;;   (add-to-list 'org-latex-classes
;;                '("my-letter"
;;                  "\\documentclass\[%
;;   DIV=14,
;;   fontsize=12pt,
;;   parskip=half,
;;   subject=titled,
;;   backaddress=false,
;;   fromalign=left,
;;   fromemail=true,
;;   fromphone=true\]\{scrlttr2\}
;;   \[DEFAULT-PACKAGES]
;;   \[PACKAGES]
;;   \[EXTRA]"))
;;
;; Then, in your Org document, be sure to require the proper class
;; with:
;;
;;    #+LATEX_CLASS: my-letter
;;
;; Or by setting `org-koma-letter-default-class'.
;;
;; You may have to load (LaTeX) Babel as well, e.g., by adding
;; it to `org-latex-packages-alist',
;;
;;    (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))

;;; Code:

(require 'ox-latex)


;;; User-Configurable Variables

(defgroup org-export-koma-letter nil
  "Options for exporting to KOMA scrlttr2 class in LaTeX export."
  :tag "Org Koma-Letter"
  :group 'org-export)

(defcustom org-koma-letter-class-option-file "NF"
  "Letter Class Option File."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-author 'user-full-name
  "The sender's name.

This variable defaults to calling the function `user-full-name'
which just returns the current function `user-full-name'.  Alternatively a
string, nil or a function may be given.  Functions must return a
string."
  :group 'org-export-koma-letter
  :type '(radio (function-item user-full-name)
		(string)
		(function)
		(const :tag "Do not export author" nil)))

(defcustom org-koma-letter-email 'org-koma-letter-email
  "The sender's email address.

This variable defaults to the value `org-koma-letter-email' which
returns `user-mail-address'.  Alternatively a string, nil or a
function may be given.  Functions must return a string."
  :group 'org-export-koma-letter
  :type '(radio (function-item org-koma-letter-email)
		(string)
		(function)
		(const :tag "Do not export email" nil)))

(defcustom org-koma-letter-from-address nil
  "Sender's address, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-phone-number nil
  "Sender's phone number, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-place nil
  "Place from which the letter is sent."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-opening nil
  "Letter's opening, as a string.

If (1) this value is nil; (2) the letter is started with a
headline; and (3) `org-koma-letter-headline-is-opening-maybe' is
t the value opening will be implicit set as the headline title."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-closing nil
  "Koma-Letter's closing, as a string."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-prefer-special-headings nil
  "If TO and/or FROM is specified using both a heading and a keyword the heading value will be preferred if the variable is t."
  :group 'org-export-koma-letter
  :type 'boolean)

(defcustom org-koma-letter-signature nil
  "String used as the signature."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-subject-format t
  "Use the title as the subject of the letter.

At this time the following values are allowed:

 - afteropening: subject after opening.
 - beforeopening: subject before opening.
 - centered: subject centered.
 - left:subject left-justified.
 - right: subject right-justified.
 - titled: add title/description to subject.
 - underlined: set subject underlined.
 - untitled: do not add title/description to subject.
 - No-export: do no insert a subject even if present.

Please refer to the KOMA-script manual (Table 4.16. in the
English manual of 2012-07-22)."
  :type '(radio
	  (const :tag "No export" nil)
	  (const :tag "Default options" t)
	  (set :tag "selection"
	   (const  'afteropening)
	   (const  'beforeopening)
	   (const  'centered)
	   (const  'left)
	   (const  'right)
	   (const  'underlined)
	   (const  'titled)
	   (const  'untitled))
	  (string))
  :group 'org-export-koma-letter)

(defcustom org-koma-letter-use-backaddress nil
  "Print return address in line above to address."
  :group 'org-export-koma-letter
  :type 'boolean)

(defcustom org-koma-letter-use-foldmarks "true"
  "Configure appearence of fold marks.

Accepts any valid value for the KOMA-Script `foldmarks' option.

Use `foldmarks:true' to activate default fold marks or
`foldmarks:nil' to deactivate fold marks."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-use-phone nil
  "Print sender's phone number."
  :group 'org-export-koma-letter
  :type 'boolean)

(defcustom org-koma-letter-use-email nil
  "Print sender's email address."
  :group 'org-export-koma-letter
  :type 'boolean)

(defcustom org-koma-letter-use-place t
  "Print the letter's place next to the date."
  :group 'org-export-koma-letter
  :type 'boolean)

(defcustom org-koma-letter-default-class nil
  "Default class for `org-koma-letter'.

The value must be a member of `org-latex-classes'."
  :group 'org-export-koma-letter
  :type 'string)

(defcustom org-koma-letter-headline-is-opening-maybe t
  "Whether a headline may be used as an opening.
A headline is only used if #+OPENING is not set.  See also
`org-koma-letter-opening'."
  :group 'org-export-koma-letter
  :type 'boolean)

(defconst org-koma-letter-special-tags-in-letter '(to from)
  "Header tags related to the letter itself.")

(defconst org-koma-letter-special-tags-after-closing '(ps encl cc)
  "Header tags to be inserted after closing.")

(defconst org-koma-letter-special-tags-after-letter '(after_letter)
  "Header tags to be inserted after closing.")

(defvar org-koma-letter-special-contents nil
  "Holds special content temporarily.")



;;; Define Back-End

(org-export-define-derived-backend 'koma-letter 'latex
  :options-alist
  '((:lco "LCO" nil org-koma-letter-class-option-file)
    (:latex-class "LATEX_CLASS" nil (if org-koma-letter-default-class
					org-koma-letter-default-class
					org-latex-default-class) t)
    (:author "AUTHOR" nil (org-koma-letter--get-value org-koma-letter-author) t)
    (:author-changed-in-buffer-p "AUTHOR" nil nil t)
    (:from-address "FROM_ADDRESS" nil nil newline)
    (:phone-number "PHONE_NUMBER" nil org-koma-letter-phone-number)
    (:email "EMAIL" nil (org-koma-letter--get-value org-koma-letter-email) t)
    (:email-changed-in-buffer-p "EMAIL" nil nil t)
    (:to-address "TO_ADDRESS" nil nil newline)
    (:place "PLACE" nil org-koma-letter-place)
    (:opening "OPENING" nil org-koma-letter-opening)
    (:closing "CLOSING" nil org-koma-letter-closing)
    (:signature "SIGNATURE" nil org-koma-letter-signature newline)
    (:special-tags nil nil (append
			    org-koma-letter-special-tags-in-letter
			    org-koma-letter-special-tags-after-closing
			    org-koma-letter-special-tags-after-letter))
    (:special-headings nil "special-headings"
		       org-koma-letter-prefer-special-headings)
    (:with-after-closing nil "after-closing-order"
			 org-koma-letter-special-tags-after-closing)
    (:with-after-letter nil "after-letter-order"
			org-koma-letter-special-tags-after-letter)
    (:with-backaddress nil "backaddress" org-koma-letter-use-backaddress)
    (:with-backaddress-changed-in-buffer-p nil "backaddress" nil)
    (:with-foldmarks nil "foldmarks" org-koma-letter-use-foldmarks)
    (:with-foldmarks-changed-in-buffer-p nil "foldmarks" "foldmarks-not-set")
    (:with-phone nil "phone" org-koma-letter-use-phone)
    (:with-phone-changed-in-buffer-p nil "phone" nil)
    (:with-email nil "email" org-koma-letter-use-email)
    (:with-email-changed-in-buffer-p nil "email" nil)
    (:with-place nil "place" org-koma-letter-use-place)
    (:with-subject nil "subject" org-koma-letter-subject-format))
  :translate-alist '((export-block . org-koma-letter-export-block)
		     (export-snippet . org-koma-letter-export-snippet)
		     (headline . org-koma-letter-headline)
		     (keyword . org-koma-letter-keyword)
		     (template . org-koma-letter-template))
  :menu-entry
  '(?k "Export with KOMA Scrlttr2"
       ((?L "As LaTeX buffer" org-koma-letter-export-as-latex)
	(?l "As LaTeX file" org-koma-letter-export-to-latex)
	(?p "As PDF file" org-koma-letter-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-koma-letter-export-to-pdf t s v b)
		(org-open-file (org-koma-letter-export-to-pdf nil s v b))))))))


;;; Initialize class function

(defun org-koma-letter-plug-into-ox ()
  "Add a sparse `default-koma-letter' to `org-latex-classes' and set `org-koma-letter-default-class' to `default-koma-letter'."
  (let ((class "default-koma-letter"))
    (eval-after-load "ox-latex"
      `(unless (member ,class 'org-latex-classes)
	 (add-to-list 'org-latex-classes
		      `(,class
			"\\documentclass[11pt]{scrlttr2}") ())
	 (setq org-koma-letter-default-class class)))))

;;; Helper functions

(defun org-koma-letter-email ()
  "Return the current `user-mail-address'."
  user-mail-address)

;; The following is taken from/inspired by ox-grof.el
;; Thanks, Luis!

(defun org-koma-letter--get-tagged-contents (key)
  "Get contents from a headline tagged with KEY.
Technically, the contents is stored in `org-koma-letter-special-contents'."
  (cdr (assoc (org-koma-letter--get-value key)
	      org-koma-letter-special-contents)))

(defun org-koma-letter--get-value (value)
  "Determines if VALUE is nil, a string, a function or a symbol and return a string or nil."
  (when value
    (cond ((stringp value) value)
	  ((functionp value) (funcall value))
	  ((symbolp value) (symbol-name value))
	  (t value))))


(defun org-koma-letter--special-contents-as-macro (a-list &optional keep-newlines no-tag)
  "Find members of `org-koma-letter-special-contents' corresponding to A-LIST.
Return them as a string to be formatted.

The function is used for inserting content of speciall headings
such as PS.

If KEEP-NEWLINES is t newlines will not be removed.  If NO-TAG is
is t the content in `org-koma-letter-special-contents' will not
be wrapped in a macro named whatever the members of A-LIST are
called."
  (let (output)
    (dolist (ac* a-list output)
      (let*
	  ((ac (org-koma-letter--get-value ac*))
	   (x (org-koma-letter--get-tagged-contents ac)))
	(when x
	  (setq output
		(concat
		 output "\n"
		 ;; sometimes LaTeX complains about newlines
		 ;; at the end or beginning of macros.  Remove them.
		 (org-koma-letter--format-string-as-macro
		  (if keep-newlines x (org-koma-letter--normalize-string x))
		  (unless no-tag  ac)))))))))

(defun org-koma-letter--format-string-as-macro (string &optional macro)
  "Format STRING as  \"\\macro{string}\" if MACRO is given else as \"string\"."
  (if macro
      (format "\\%s{%s}" macro string)
    (format "%s" string)))

(defun org-koma-letter--normalize-string (string)
  "Remove new lines in the beginning and end of `STRING'."
  (replace-regexp-in-string "\\`[ \n\t]+\\|[\n\t ]*\\'" "" string))

(defun org-koma-letter--determine-to-and-from (info key)
  "Given INFO determine KEY for the letter.
KEY should be `to' or `from'.

`ox-koma-letter' allows two ways to specify to and from.  If both
are present return the preferred one as determined by
`org-koma-letter-prefer-special-headings'."
  (let* ((plist-alist '((from . :from-address)
		       (to . :to-address)))
	 (default-alist  `((from  ,org-koma-letter-from-address)
			   (to  "\\mbox{}")))
	 (option-value (plist-get info (cdr-safe (assoc key plist-alist))))
	 (head-value (org-koma-letter--get-tagged-contents key))
	 (order (append
		 (funcall
		  (if (plist-get info :special-headings)
		      'reverse 'identity)
		  `(,option-value ,head-value))
		 (cdr-safe (assoc key default-alist))))
	 tmp
	 (adr (dolist (x order tmp)
		(when (and (not tmp) x)
		  (setq tmp x)))))
  (when adr
    (replace-regexp-in-string
     "\n" "\\\\\\\\\n"
     (org-koma-letter--normalize-string adr)))))

;;; Transcode Functions

;;;; Export Block

(defun org-koma-letter-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :type export-block) '("KOMA-LETTER" "LATEX"))
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Export Snippet

(defun org-koma-letter-export-snippet (export-snippet contents info)
  "Transcode an EXPORT-SNIPPET object into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (memq (org-export-snippet-backend export-snippet) '(latex koma-letter))
    (org-element-property :value export-snippet)))

;;;; Keyword

(defun org-koma-letter-keyword (keyword contents info)
  "Transcode a KEYWORD element into KOMA Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    ;; Handle specifically BEAMER and TOC (headlines only) keywords.
    ;; Otherwise, fallback to `latex' back-end.
    (if (equal key "KOMA-LETTER") value
      (org-export-with-backend 'latex keyword contents info))))


;; Headline

(defun org-koma-letter-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information.

Note that if a headline is tagged with a tag from
`org-koma-letter-special-tags' it will not be exported, but
stored in `org-koma-letter-special-contents' and included at the
appropriate place."
  (let*
      ((tags (org-export-get-tags headline info))
       (tag* (car tags))
       (tag  (when tag*
	       (car (member-ignore-case
		     tag*
		     (mapcar 'symbol-name (plist-get info :special-tags)))))))
    (if tag
	(progn
	  (push (cons tag contents)
		org-koma-letter-special-contents)
	  nil)
      (unless (or (plist-get info :opening)
		  (not org-koma-letter-headline-is-opening-maybe))
	(plist-put info :opening
		   (org-export-data (org-element-property :title headline) info)))
      contents)))


;;;; Template

(defun org-koma-letter-template (contents info)
  "Return complete document string after KOMA Scrlttr2 conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; FIXME: instead of setq'ing org-koma-letter-special-contents and
  ;; callying varioues stuff it might be nice to put a big let* around the templace
  ;; as in org-groff...
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
   ;; Document class and packages.
   (let* ((class (plist-get info :latex-class))
	  (class-options (plist-get info :latex-class-options))
	  (header (nth 1 (assoc class org-latex-classes)))
	  (document-class-string
	   (and (stringp header)
		(if (not class-options) header
		  (replace-regexp-in-string
		   "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		   class-options header t nil 1)))))
     (if (not document-class-string)
	 (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
	(org-latex-guess-inputenc
	 (org-element-normalize-string
	  (org-splice-latex-header
	   document-class-string
	   org-latex-default-packages-alist ; Defined in org.el.
	   org-latex-packages-alist nil     ; Defined in org.el.
	   (concat (org-element-normalize-string (plist-get info :latex-header))
		   (plist-get info :latex-header-extra)))))
	info)))
   (let ((lco (plist-get info :lco))
	 (author (plist-get info :author))
	 (author-set (plist-get info :author-changed-in-buffer-p))
	 (from-address (org-koma-letter--determine-to-and-from info 'from))
	 (phone-number (plist-get info :phone-number))
	 (email (plist-get info :email))
	 (email-set (plist-get info :email-changed-in-buffer-p))
	 (signature (plist-get info :signature)))
     (concat
      ;; author or email not set in file: may be overridden by lco
      (unless author-set
	(when author (format "\\setkomavar{fromname}{%s}\n"
			     (org-export-data author info))))
      (unless email-set
	(when email (format "\\setkomavar{fromemail}{%s}\n" email)))
      ;; Letter Class Option File
      (when lco
	(let ((lco-files (split-string lco " "))
	      (lco-def ""))
	  (dolist (lco-file lco-files lco-def)
	    (setq lco-def (format "%s\\LoadLetterOption{%s}\n" lco-def lco-file)))
	  lco-def))
      ;; Define "From" data.
      (when (and author author-set) (format "\\setkomavar{fromname}{%s}\n"
					    (org-export-data author info)))
      (when from-address (format "\\setkomavar{fromaddress}{%s}\n" from-address))
      (when phone-number
	(format "\\setkomavar{fromphone}{%s}\n" phone-number))
      (when (and email email-set) (format "\\setkomavar{fromemail}{%s}\n" email))
      (when signature (format "\\setkomavar{signature}{%s}\n" signature))))
   ;; Date.
   (format "\\date{%s}\n" (org-export-data (org-export-get-date info) info))
   ;; Place
   (let ((with-place (plist-get info :with-place))
	 (place (plist-get info :place)))
     (when (or place (not with-place))
       (format "\\setkomavar{place}{%s}\n" (if with-place place ""))))
   ;; KOMA options
   (let ((with-backaddress (plist-get info :with-backaddress))
	 (with-backaddress-set (plist-get info :with-backaddress-changed-in-buffer-p))
	 (with-foldmarks (plist-get info :with-foldmarks))
	 (with-foldmarks-set 
	  (not (string-equal (plist-get info :with-foldmarks-changed-in-buffer-p)
			     "foldmarks-not-set")))
	 (with-phone (plist-get info :with-phone))
	 (with-phone-set (plist-get info :with-phone-changed-in-buffer-p))
	 (with-email (plist-get info :with-email))
	 (with-email-set (plist-get info :with-email-changed-in-buffer-p)))
     (concat
      (when with-backaddress-set
	(format "\\KOMAoption{backaddress}{%s}\n" (if with-backaddress "true" "false")))
      (when with-foldmarks-set
	(format "\\KOMAoption{foldmarks}{%s}\n" (if with-foldmarks with-foldmarks "false")))
      (when with-phone-set
	(format "\\KOMAoption{fromphone}{%s}\n" (if with-phone "true" "false")))
      (when with-email-set
	(format "\\KOMAoption{fromemail}{%s}\n" (if with-email "true" "false")))))
   ;; Document start
   "\\begin{document}\n\n"
   ;; Subject
   (let* ((with-subject (plist-get info :with-subject))
	  (subject-format (cond ((member with-subject '("true" "t" t)) nil)
				((stringp with-subject) (list with-subject))
				((symbolp with-subject)
				 (list (symbol-name with-subject)))
				(t with-subject)))
	  (subject (org-export-data (plist-get info :title) info))
	  (l (length subject-format))
	  (y ""))
     (concat
      (when (and with-subject subject-format)
	(concat
	 "\\KOMAoption{subject}{"
	 (apply 'format
		(dotimes (x l y)
		  (setq y (concat (if (> x 0) "%s," "%s") y)))
		subject-format) "}\n"))
      (when (and subject with-subject)
	(format "\\setkomavar{subject}{%s}\n\n" subject))))
   ;; Letter start
   (format "\\begin{letter}{%%\n%s}\n\n"
	   (org-koma-letter--determine-to-and-from info 'to))
   ;; Opening.
   (format "\\opening{%s}\n\n" (or (plist-get info :opening) ""))
   ;; Letter body.
   contents
   ;; Closing.
   (format "\n\\closing{%s}\n" (or (plist-get info :closing) ""))
   (org-koma-letter--special-contents-as-macro
    (plist-get info :with-after-closing))
   ;; Letter end.
   "\n\\end{letter}\n"
   (org-koma-letter--special-contents-as-macro
    (plist-get info :with-after-letter) t t)
   ;; Document end.
   "\n\\end{document}"
   ))


;;; Commands

;;;###autoload
(defun org-koma-letter-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (org-koma-letter-special-contents)
    (org-export-to-buffer 'koma-letter "*Org KOMA-LETTER Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun org-koma-letter-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
	(org-koma-letter-special-contents))
    (org-export-to-file 'koma-letter outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-koma-letter-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep))
	(org-koma-letter-special-contents))
    (org-export-to-file 'koma-letter file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))


(provide 'ox-koma-letter)
;;; ox-koma-letter.el ends here
