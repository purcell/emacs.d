;; org-export-generic.el --- Export frameworg with custom backends

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author:   Wes Hardaker <hardaker at users dot sourceforge dot net>
;; Keywords: outlines, hypermedia, calendar, wp, export
;; Homepage: http://orgmode.org
;; Version:  6.25trans
;; Acks:     Much of this code was stolen form the ascii export from Carsten
;;
;; This file is not yet part of GNU Emacs.
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;; ----------------------------------------------------------------------
;;
;; OVERVIEW
;;
;; org-export-generic is basically a simple translation system that
;; knows how to parse at least most of a .org buffer and then add
;; various formatting prefixes before and after each section type.  It
;; does this by examining a property list stored in org-generic-alist.
;; You can dynamically add propety lists of your own using the
;; org-set-generic-type function:
;;
;;    (org-set-generic-type
;;     "really-basic-text"
;;     '(:file-suffix  ".txt"
;;       :key-binding  ?R
;;
;;       :title-format "=== %s ===\n"
;;       :body-header-section-numbers t
;;       :body-header-section-number-format "%s) "
;;       :body-section-header-prefix  "\n"
;;       :body-section-header-suffix "\n"
;;       :body-line-format "  %s\n"
;;       :body-line-wrap   75
;;       ))
;;
;; Note: Upper case key-bindings are reserved for your use.  Lower
;; case key bindings may conflict with future export-generic
;; publications.
;;
;; Then run org-export (ctrl-c ctrl-e) and select generic or run
;; org-export-generic.  You'll then be prompted with a list of export
;; types to choose from which will include your new type assigned to
;; the key "r".
;;
;; ----------------------------------------------------------------------
;;
;; TODO (non-ordered)
;;   * handle function references
;;   * handle other types of multi-complex-listy-things to do
;;     ideas:  (t ?- "%s" ?-)
;;   * handle indent specifiers better
;;     ideas:  (4 ?\  "%s")
;;   * need flag to remove indents from body text
;;   * handle links
;;   * handle internationalization strings better
;;   * date/author/etc needs improvment (internationalization too)
;;   * allow specifying of section ordering
;;     ideas:  :ordering  ("header" "toc" "body" "footer")
;;                         ^ matches current hard coded ordering
;;   * err, actually *do* a footer
;;   * deal with usage of org globals
;;   *** should we even consider them, or let the per-section specifiers do it
;;   *** answer: remove; mostly removed now
;;   * deal with interactive support for picking a export specifier label
;;   * char specifiers that need extra length because of formatting
;;     idea: (?- 4)  for 4-longer
;;   * centering specifier
;;     idea: ('center " -- %s -- ")
;;   * remove more of the unneeded export-to-ascii copy code
;;   * tags
;;     *** supported now, but need separate format per tag
;;     *** allow different open/closing prefixes
;;   * properties
;;   * drawers
;;   * Escape camel-case for wiki exporters.
;;   * Adjust to depth limits on headers --- need to roll-over from headers
;;     to lists, as per other exporters
;;   * optmization (many plist extracts should be in let vars)
;;   * define defcustom spec for the specifier list
;;   * fonts:  at least monospace is not handled at all here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org-exp)
(require 'assoc)

(defgroup org-export-generic nil
  "Options specific for ASCII export of Org-mode files."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-export-generic-links-to-notes t
  "Non-nil means convert links to notes before the next headline.
When nil, the link will be exported in place.  If the line becomes long
in this way, it will be wrapped."
  :group 'org-export-generic
  :type 'boolean)


(defvar org-generic-current-indentation nil) ; For communication

(defvar org-generic-alist
  '(
    ;;
    ;; generic DEMO exporter
    ;;
    ;; (this tries to use every specifier for demo purposes)
    ;;
    ("demo"
     :file-suffix        	    ".txt"
     :key-binding        	    ?d

     :header-prefix             "<header>\n"
     :header-suffix             "</header>\n"

     :author-export             t
     :tags-export               t

     :drawers-export            t


     :title-prefix              ?=
     :title-format              "<h1>%s</h1>\n"
     :title-suffix              ?=

     :date-export        	    t
     :date-prefix               "<date>"
     :date-format               "<br /><b>Date:</b> <i>%s</i><br />"
     :date-suffix               "</date>\n\n"

     :toc-export                t
     :toc-header-prefix         "<tocname>\n"
     :toc-header-format         "__%s__\n"
     :toc-header-suffix         "</tocname>\n"

     :toc-prefix                "<toc>\n"
     :toc-suffix                "</toc>\n"

     :toc-section-numbers       t
     :toc-section-number-format "\#(%s) "
     :toc-format                "--%s--"
     :toc-format-with-todo      "!!%s!!\n"
     :toc-indent-char           ?\
     :toc-indent-depth          4

     :toc-tags-export           t
     :toc-tags-prefix           "   <tags>"
     :toc-tags-format           "*%s*"
     :toc-tags-suffix           "</tags>\n"
     :toc-tags-none-string      "\n"

     :body-header-section-numbers 3	; t = all, nil = none

					; lists indicate different things per level
					; list contents or straight value can either be a
					;    ?x char reference for printing strings that match the header len
					;    "" string to print directly
     :body-section-header-prefix  ("<h1>" "<h2>" "<h3>"
				   "<h4>" "<h5>" "<h6>")
     :body-section-header-format  "%s"
     :body-section-header-suffix  ("</h1>\n" "</h2>\n" "</h3>\n"
				   "</h4>\n" "</h5>\n" "</h6>\n")

     :timestamps-export           t
     :priorities-export           t
     :todo-keywords-export        t

     :body-tags-export 		  t
     :body-tags-prefix 		  "  <tags>"
     :body-tags-suffix 		  "</tags>\n"

					; section prefixes/suffixes can be direct strings or lists as well
     :body-section-prefix         "<secprefix>\n"
     :body-section-suffix         "</secsuffix>\n"
					;	 :body-section-prefix         ("<sec1>\n" "<sec2>\n" "<sec3>\n")
					;	 :body-section-suffix         ("</sec1>\n" "</sec2>\n" "</sec3>\n")


					; if preformated text should be included (eg, : prefixed)
     :body-line-export-preformated t
     :body-line-fixed-prefix       "<pre>\n"
     :body-line-fixed-suffix       "\n</pre>\n"
     :body-line-fixed-format       "%s\n"


     :body-list-prefix             "<list>\n"
     :body-list-suffix             "</list>\n"
     :body-list-format             "<li>%s</li>\n"

     :body-number-list-prefix       "<ol>\n"
     :body-number-list-suffix       "</ol>\n"
     :body-number-list-format       "<li>%s</li>\n"
     :body-number-list-leave-number t

     :body-list-checkbox-todo      "<checkbox type=\"todo\">"
     :body-list-checkbox-todo-end  "</checkbox (todo)>"
     :body-list-checkbox-done      "<checkbox type=\"done\">"
     :body-list-checkbox-done-end  "</checkbox (done)>"
     :body-list-checkbox-half      "<checkbox type=\"half\">"
     :body-list-checkbox-half-end  "</checkbox (half)>"




					; other body lines
     :body-line-format             "%s"
     :body-line-wrap               60	; wrap at 60 chars

					; print above and below all body parts
     :body-text-prefix 	       "<p>\n"
     :body-text-suffix 	       "</p>\n"

     )

    ;;
    ;; ascii exporter
    ;;
    ;; (close to the original ascii specifier)
    ;;
    ("ascii"
     :file-suffix        	    ".txt"
     :key-binding        	    ?a

     :header-prefix             ""
     :header-suffix             ""

     :title-prefix ?=
     :title-format "%s\n"
     :title-suffix ?=

     :date-export        	    t
     :date-prefix               ""
     :date-format               "Date: %s\n"
     :date-suffix               ""

     :toc-header-prefix ""
     :toc-header-format "%s\n"
     :toc-header-suffix ?=

     :toc-export                t
     :toc-section-numbers       t
     :toc-section-number-format "%s "
     :toc-format                "%s\n"
     :toc-format-with-todo      "%s (*)\n"
     :toc-indent-char           ?\
     :toc-indent-depth          4

     :body-header-section-numbers 3
     :body-section-prefix         "\n"

					;	 :body-section-header-prefix  "\n"
					;	 :body-section-header-format  "%s\n"
					;	 :body-section-header-suffix  (?\$ ?\# ?^ ?\~ ?\= ?\-)

     :body-section-header-prefix  ("" "" "" "* " "  + " "    - ")
     :body-section-header-format  "%s\n"
     :body-section-header-suffix  (?~ ?= ?- "\n" "\n" "\n")

					;	 :body-section-marker-prefix  ""
					;	 :body-section-marker-chars   (?\$ ?\# ?^ ?\~ ?\= ?\-)
					;	 :body-section-marker-suffix  "\n"

     :body-line-export-preformated t
     :body-line-format             "%s\n"
     :body-line-wrap               75

					;	 :body-text-prefix "<t>\n"
					;	 :body-text-suffix "</t>\n"


     :body-bullet-list-prefix      (?* ?+ ?-)
					;	 :body-bullet-list-suffix      (?* ?+ ?-)
     )

    ;;
    ;; wikipedia
    ;;
    ("wikipedia"
     :file-suffix        	    ".txt"
     :key-binding                   ?w

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "= %s =\n"

     :date-export        	    nil

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("= " "== " "=== "
				     "==== " "===== " "====== ")
     :body-section-header-suffix    (" =\n\n" " ==\n\n" " ===\n\n"
				     " ====\n\n" " =====\n\n" " ======\n\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-format       " %s\n"

     :body-list-format              "* %s\n"
     :body-number-list-format       "# %s\n"

     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
     )
    ;;
    ;; internet-draft .xml for xml2rfc exporter
    ;;
    ("ietfid"
     ;; this tries to use every specifier for demo purposes
     :file-suffix        	    ".xml"
     :key-binding                   ?i

     :title-prefix              "<?xml version=\"1.0\"\?>
<!DOCTYPE rfc SYSTEM \"rfc2629.dtd\" [
<!ENTITY rfcs PUBLIC '' 'blah'>
<?rfc strict=\"yes\" ?>
<?rfc toc=\"yes\" ?>
<?rfc tocdepth=\"4\" ?>
<?rfc symrefs=\"yes\" ?>
<?rfc compact=\"yes\" ?>
<?rfc subcompact=\"no\" ?>
<rfc category=\"std\" ipr=\"pre5378Trust200902\" docName=\"FILLME.txt\">
    <front>
"
     :title-format              "<title abbrev=\"ABBREV HERE\">\n%s\n</title>\n"
     :title-suffix              "<author initials=\"A.A\" surname=\"LASTNAME\" fullname=\"FULL NAME\">
      <organization>Comany, Inc..</organization>
      <address>
        <postal>
          <street></street>
          <city></city>
          <region></region>
          <code></code>
          <country></country>
        </postal>
        <phone></phone>
        <email></email>
      </address>
    </author>
    <date month=\"FILLMONTH\" year=\"FILLYEAR\"/>
    <area>Operations and Management</area>
    <workgroup>FIXME</workgroup>
<abstract>\n"
     :date-export        	    nil

     :toc-export                nil

     :body-header-section-numbers nil

     :body-section-header-format  "<section title=\"%s\">\n"
     :body-section-suffix         "</section>\n"

					; if preformated text should be included (eg, : prefixed)
     :body-line-export-preformated t
     :body-line-fixed-prefix       "<figure>\n<artwork>\n"
     :body-line-fixed-suffix       "\n</artwork>\n</figure>\n"

					; other body lines
     :body-line-format             "%s"
     :body-line-wrap               75

					; print above and below all body parts
     :body-text-prefix 	       "<t>\n"
     :body-text-suffix 	       "</t>\n"

     :body-list-prefix 	       "<list style=\"symbols\">\n"
     :body-list-suffix 	       "</list>\n"
     :body-list-format 	       "<t>%s</t>\n"

     )
    ("trac-wiki"
     :file-suffix     ".txt"
     :key-binding     ?T

     ;; lifted from wikipedia exporter
     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "= %s =\n"

     :date-export        	    nil

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    (" == " " === " " ==== "
				     " ===== " )
     :body-section-header-suffix    (" ==\n\n" " ===\n\n" " ====\n\n"
				     " =====\n\n" " ======\n\n" " =======\n\n")

     :body-line-export-preformated  t ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-format       " %s\n"

     :body-list-format              " * %s\n"
     :body-number-list-format       " # %s\n"
     ;;    :body-list-prefix              "LISTSTART"
     ;;    :body-list-suffix              "LISTEND"

     ;; this is ignored! [2010/02/02:rpg]
     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
     )
    ("tikiwiki"
     :file-suffix     ".txt"
     :key-binding     ?U

     ;; lifted from wikipedia exporter
     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "-= %s =-\n"

     :date-export        	    nil

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("! " "!! " "!!! " "!!!! "
				     "!!!!! " "!!!!!! " "!!!!!!! ")
     :body-section-header-suffix    (" \n" " \n" " \n"
				     " \n" " \n" " \n")


     :body-line-export-preformated  t ;; yes/no/maybe???
     :body-line-format              "%s "
     :body-line-wrap                nil

     :body-line-fixed-format       " %s\n"

     :body-list-format              "* %s\n"
     :body-number-list-format       "# %s\n"
     ;;    :body-list-prefix              "LISTSTART"
     ;;    :body-list-suffix              "LISTEND"
     :blockquote-start              "\n^\n"
     :blockquote-end                "^\n\n"
     :body-newline-paragraph        "\n"
     :bold-format                   "__%s__"
     :italic-format                 "''%s''"
     :underline-format              "===%s==="
     :strikethrough-format          "--%s--"
     :code-format                   "-+%s+-"
     :verbatim-format               "~pp~%s~/pp~"
     )
    )
  "A assoc list of property lists to specify export definitions"
)

(setq org-generic-export-type "demo")

(defvar org-export-generic-section-type "")
(defvar org-export-generic-section-suffix "")

;;;###autoload
(defun org-set-generic-type (type definition)
  "Adds a TYPE and DEFINITION to the existing list of defined generic
export definitions."
  (aput 'org-generic-alist type definition))

;;; helper functions for org-set-generic-type
(defvar org-export-generic-keywords nil)
(defmacro* def-org-export-generic-keyword (keyword
                                           &key documentation
                                                type)
  "Define KEYWORD as a legitimate element for inclusion in
the body of an org-set-generic-type definition."
  `(progn
     (pushnew ,keyword org-export-generic-keywords)
     ;; TODO: push the documentation and type information
     ;; somewhere where it will do us some good.
     ))

(def-org-export-generic-keyword :body-newline-paragraph
    :documentation "Bound either to NIL or to a pattern to be
inserted in the output for every blank line in the input.
  The intention is to handle formats where text is flowed, and
newlines are interpreted as significant \(e.g., as indicating
preformatted text\).  A common non-nil value for this keyword
is \"\\n\".  Should typically be combined with a value for
:body-line-format that does NOT end with a newline."
    :type string)

;;; fontification keywords
(def-org-export-generic-keyword :bold-format)
(def-org-export-generic-keyword :italic-format)
(def-org-export-generic-keyword :underline-format)
(def-org-export-generic-keyword :strikethrough-format)
(def-org-export-generic-keyword :code-format)
(def-org-export-generic-keyword :verbatim-format)




(defun org-export-generic-remember-section (type suffix &optional prefix)
  (setq org-export-generic-section-type type)
  (setq org-export-generic-section-suffix suffix)
  (if prefix
      (insert prefix))
)

(defun org-export-generic-check-section (type &optional prefix suffix)
  "checks to see if type is already in use, or we're switching parts
If we're switching, then insert a potentially previously remembered
suffix, and insert the current prefix immediately and then save the
suffix a later change time."

  (when (not (equal type org-export-generic-section-type))
    (if org-export-generic-section-suffix
      (insert org-export-generic-section-suffix))
    (setq org-export-generic-section-type type)
    (setq org-export-generic-section-suffix suffix)
    (if prefix
	(insert prefix))))

;;;###autoload
(defun org-export-generic (arg)
  "Export the outline as generic output.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char rbeg)
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) rend)))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))

	 helpstart
	 (bogus (mapc (lambda (x)
			(setq helpstart
			      (concat helpstart "\["
				      (char-to-string
				       (plist-get (cdr x) :key-binding))
				      "]  " (car x) "\n")))
		      org-generic-alist))

	 (help (concat helpstart "

\[ ] the current setting of the org-generic-export-type variable
"))

	 (cmds

	  (append
	   (mapcar (lambda (x)
		     (list
		      (plist-get (cdr x) :key-binding)
		      (car x)))
		      org-generic-alist)
	   (list (list ?  "default"))))

	 r1 r2 ass

	 ;; read in the type to use
	 (export-plist
	  (progn
	    (save-excursion
	      (save-window-excursion
		(delete-other-windows)
		(with-output-to-temp-buffer "*Org Export/Generic Styles Help*"
		  (princ help))
		(org-fit-window-to-buffer (get-buffer-window
					   "*Org Export/Generic Styles Help*"))
		(message "Select command: ")
		(setq r1 (read-char-exclusive))))
	    (setq r2 (if (< r1 27) (+ r1 96) r1))
	    (unless (setq ass (cadr (assq r2 cmds)))
	      (error "No command associated with key %c" r1))

	    (cdr (assoc
		  (if (equal ass "default") org-generic-export-type ass)
		  org-generic-alist))))

	 (custom-times org-display-custom-times)
	 (org-generic-current-indentation '(0 . 0))
	 (level 0) (old-level 0) line txt lastwastext
	 (umax nil)
	 (umax-toc nil)
	 (case-fold-search nil)
	 (bfname (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
	 (filesuffix (or (plist-get export-plist :file-suffix) ".foo"))
	 (filename (concat (file-name-as-directory
			    (org-export-directory :ascii opt-plist))
			   (file-name-sans-extension
			    (or (and subtree-p
				     (org-entry-get (region-beginning)
						    "EXPORT_FILE_NAME" t))
				(file-name-nondirectory bfname)))
			   filesuffix))
	 (filename (if (equal (file-truename filename)
			      (file-truename bfname))
		       (concat filename filesuffix)
		     filename))
	 (buffer (find-file-noselect filename))
	 (org-levels-open (make-vector org-level-max nil))
	 (odd org-odd-levels-only)
	 (date  (plist-get opt-plist :date))
	 (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (file-name-sans-extension
			   (file-name-nondirectory bfname))))
	 (email       (plist-get opt-plist :email))
	 (language    (plist-get opt-plist :language))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
;	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]*" org-quote-string "\\>\\)"))
	 (todo nil)
	 (lang-words nil)
	 (region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (org-export-current-backend 'org-export-generic)
	 (lines (org-split-string
		 (org-export-preprocess-string
		  region
		  :for-backend 'ascii
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :drawers (plist-get export-plist :drawers-export)
		  :tags (plist-get export-plist :tags-export)
		  :priority (plist-get export-plist :priority-export)
		  :footnotes (plist-get export-plist :footnotes-export)
		  :timestamps (plist-get export-plist :timestamps-export)
		  :todo-keywords (plist-get export-plist :todo-keywords-export)
		  :verbatim-multiline t
		  :select-tags (plist-get export-plist :select-tags-export)
		  :exclude-tags (plist-get export-plist :exclude-tags-export)
                  :emph-multiline t
		  :archived-trees
		  (plist-get export-plist :archived-trees-export)
		  :add-text (plist-get opt-plist :text))
		 "\n"))
	 ;; export-generic plist variables
	 (withtags 	(plist-get export-plist :tags-export))
	 (tagsintoc 	(plist-get export-plist :toc-tags-export))
	 (tocnotagsstr 	(or (plist-get export-plist :toc-tags-none-string) ""))
	 (tocdepth 	(plist-get export-plist :toc-indent-depth))
	 (tocindentchar (plist-get export-plist :toc-indent-char))
	 (tocsecnums    (plist-get export-plist :toc-section-numbers))
	 (tocsecnumform (plist-get export-plist :toc-section-number-format))
	 (tocformat     (plist-get export-plist :toc-format))
	 (tocformtodo   (plist-get export-plist :toc-format-with-todo))
	 (tocprefix     (plist-get export-plist :toc-prefix))
	 (tocsuffix     (plist-get export-plist :toc-suffix))
	 (bodyfixedpre  (plist-get export-plist :body-line-fixed-prefix))
	 (bodyfixedsuf  (plist-get export-plist :body-line-fixed-suffix))
	 (bodyfixedform (or (plist-get export-plist :body-line-fixed-format)
			    "%s"))
	 (listprefix  	(plist-get export-plist :body-list-prefix))
	 (listsuffix  	(plist-get export-plist :body-list-suffix))
	 (listformat 	(or (plist-get export-plist :body-list-format) "%s\n"))
	 (numlistleavenum
	  (plist-get export-plist :body-number-list-leave-number))
	 (numlistprefix	(plist-get export-plist :body-number-list-prefix))
	 (numlistsuffix	(plist-get export-plist :body-number-list-suffix))
	 (numlistformat
	  (or (plist-get export-plist :body-number-list-format) "%s\n"))
	 (listchecktodo
	  (or (plist-get export-plist :body-list-checkbox-todo) "\\1"))
	 (listcheckdone
	  (or (plist-get export-plist :body-list-checkbox-done) "\\1"))
	 (listcheckhalf
	  (or (plist-get export-plist :body-list-checkbox-half) "\\1"))
	 (listchecktodoend
	  (or (plist-get export-plist :body-list-checkbox-todo-end) ""))
	 (listcheckdoneend
	  (or (plist-get export-plist :body-list-checkbox-done-end) ""))
	 (listcheckhalfend
	  (or (plist-get export-plist :body-list-checkbox-half-end) ""))
         (bodynewline-paragraph   (plist-get export-plist :body-newline-paragraph))
	 (bodytextpre   (plist-get export-plist :body-text-prefix))
	 (bodytextsuf   (plist-get export-plist :body-text-suffix))
	 (bodylinewrap  (plist-get export-plist :body-line-wrap))
	 (bodylineform  (or (plist-get export-plist :body-line-format) "%s"))
         (blockquotestart (or (plist-get export-plist :blockquote-start) "\n\n\t"))
         (blockquoteend (or (plist-get export-plist :blockquote-end) "\n\n"))

         ;; dynamic variables used heinously in fontification
         ;; not referenced locally...
         (format-boldify (plist-get export-plist :bold-format))
         (format-italicize (plist-get export-plist :italic-format))
         (format-underline (plist-get export-plist :underline-format))
         (format-strikethrough (plist-get export-plist :strikethrough-format))
         (format-code (plist-get export-plist :code-format))
         (format-verbatim (plist-get export-plist :verbatim-format))



	 thetoc toctags have-headings first-heading-pos
	 table-open table-buffer link-buffer link desc desc0 rpl wrap)

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (find-file-noselect filename)

    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (fundamental-mode)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapc (lambda (x)
	    (set (make-local-variable (nth 2 x))
		 (plist-get opt-plist (car x))))
	  org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))
    (setq umax-toc umax)

    ;; File header
    (if title
	(insert
	 (org-export-generic-header title export-plist
				    :title-prefix
				    :title-format
				    :title-suffix)))

    (if (and (or author email)
	     (plist-get export-plist :author-export))
	(insert (concat (nth 1 lang-words) ": " (or author "")
			(if email (concat " <" email ">") "")
			"\n")))

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    (if (and date (plist-get export-plist :date-export))
	(insert
	 (org-export-generic-header date export-plist
				    :date-prefix
				    :date-format
				    :date-suffix)))

    ;; export the table of contents first
    (if (plist-get export-plist :toc-export)
	(progn
	  (push
	   (org-export-generic-header (nth 3 lang-words) export-plist
				      :toc-header-prefix
				      :toc-header-format
				      :toc-header-suffix)
	   thetoc)

	  (if tocprefix
	      (push tocprefix thetoc))

	  (mapc '(lambda (line)
		   (if (string-match org-todo-line-regexp line)
		       ;; This is a headline
		       (progn
			 (setq have-headings t)
			 (setq level (- (match-end 1) (match-beginning 1)
					level-offset)
			       level (org-tr-level level)
			       txt (match-string 3 line)
			       todo
			       (or (and org-export-mark-todo-in-toc
					(match-beginning 2)
					(not (member (match-string 2 line)
						     org-done-keywords)))
					; TODO, not DONE
				   (and org-export-mark-todo-in-toc
					(= level umax-toc)
					(org-search-todo-below
					 line lines level))))
			 (setq txt (org-html-expand-for-generic txt))

			 (while (string-match org-bracket-link-regexp txt)
			   (setq txt
				 (replace-match
				  (match-string (if (match-end 2) 3 1) txt)
				  t t txt)))

			 (if (and (not tagsintoc)
				  (string-match
				   (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				   txt))
			     (setq txt (replace-match "" t t txt))
			   ; include tags but formated
			   (if (string-match
				(org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$")
				txt)
			       (progn
				 (setq
				  toctags
				  (org-export-generic-header
				   (match-string 1 txt)
				   export-plist :toc-tags-prefix
				   :toc-tags-format :toc-tags-suffix))
				 (string-match
				  (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				  txt)
				 (setq txt (replace-match "" t t txt)))
			     (setq toctags tocnotagsstr)))

			 (if (string-match quote-re0 txt)
			     (setq txt (replace-match "" t t txt)))

			 (if (<= level umax-toc)
			     (progn
			       (push
				(concat

				 (make-string
				  (* (max 0 (- level org-min-level)) tocdepth)
				  tocindentchar)

				 (if tocsecnums
				     (format tocsecnumform
				      (org-section-number level))
				   "")

				 (format
				  (if todo tocformtodo tocformat)
				  txt)

				 toctags)

				thetoc)
			       (setq org-last-level level))
			   ))))
		lines)
	  (if tocsuffix
	      (push tocsuffix thetoc))
	  (setq thetoc (if have-headings (nreverse thetoc) nil))))

    (org-init-section-numbers)
    (org-export-generic-check-section "top")
    (while (setq line (pop lines))
      (when (and link-buffer (string-match org-outline-regexp-bol line))
	(org-export-generic-push-links (nreverse link-buffer))
	(setq link-buffer nil))
      (setq wrap nil)
      ;; Remove the quoted HTML tags.
      ;; XXX
      (setq line (org-html-expand-for-generic line))
      ;; Replace links with the description when possible
      ;; XXX
      (while (string-match org-bracket-link-regexp line)
	(setq link (match-string 1 line)
	      desc0 (match-string 3 line)
	      desc (or desc0 (match-string 1 line)))
	(if (and (> (length link) 8)
		 (equal (substring link 0 8) "coderef:"))
	    (setq line (replace-match
			(format (org-export-get-coderef-format (substring link 8) desc)
				(cdr (assoc
				      (substring link 8)
				      org-export-code-refs)))
			t t line))
	  (setq rpl (concat "["
			    (or (match-string 3 line) (match-string 1 line))
			    "]"))
	  (when (and desc0 (not (equal desc0 link)))
	    (if org-export-generic-links-to-notes
		(push (cons desc0 link) link-buffer)
	      (setq rpl (concat rpl " (" link ")")
		    wrap (+ (length line) (- (length (match-string 0 line)))
			    (length desc)))))
	  (setq line (replace-match rpl t t line))))
      (when custom-times
	(setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	;;
	;; a Headline
	;;
	(org-export-generic-check-section "headline")

	(setq first-heading-pos (or first-heading-pos (point)))
	(setq level (org-tr-level (- (match-end 1) (match-beginning 1)
				     level-offset))
	      txt (match-string 2 line))
	(org-generic-level-start level old-level txt umax export-plist lines)
	(setq old-level level))

       ((and org-export-with-tables
	     (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	;;
	;; a Table
	;;
	(org-export-generic-check-section "table")

	(if (not table-open)
	    ;; New table starts
	    (setq table-open t table-buffer nil))
	;; Accumulate table lines
	(setq table-buffer (cons line table-buffer))
	(when (or (not lines)
		  (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
				     (car lines))))
	  (setq table-open nil
		table-buffer (nreverse table-buffer))
	  (insert (mapconcat
		   (lambda (x)
		     (org-fix-indentation x org-generic-current-indentation))
		   (org-format-table-generic table-buffer)
		   "\n") "\n")))

       ((string-match "^\\([ \t]*\\)\\(:\\( \\|$\\)\\)" line)
	;;
	;; pre-formatted text
	;;
	(setq line (replace-match "\\1" nil nil line))

	(org-export-generic-check-section "preformat" bodyfixedpre bodyfixedsuf)

	(insert (format bodyfixedform line)))

       ((or (string-match "^\\([ \t]*\\)\\([\-\+][ \t]*\\)" line)
            ;; if the bullet list item is an asterisk, the leading space is /mandatory/
            ;; [2010/02/02:rpg]
            (string-match "^\\([ \t]+\\)\\(\\*[ \t]*\\)" line))
	;;
	;; plain list item
	;; TODO: nested lists
	;;
        ;; first add a line break between any previous paragraph or line item and this
        ;; one
        (when bodynewline-paragraph
          (insert bodynewline-paragraph))

        ;; I believe this gets rid of leading whitespace.
	(setq line (replace-match "" nil nil line))

        ;; won't this insert the suffix /before/ the last line of the list?
        ;; also isn't it spoofed by bulleted lists that have a line skip between the list items
        ;; unless 'org-empty-line-terminates-plain-lists' is true?
	(org-export-generic-check-section "liststart" listprefix listsuffix)

	;; deal with checkboxes
	(cond
	 ((string-match "^\\(\\[ \\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listchecktodo nil nil line)
			     listchecktodoend)))
	 ((string-match "^\\(\\[X\\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listcheckdone nil nil line)
			     listcheckdoneend)))
	 ((string-match "^\\(\\[/\\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listcheckhalf nil nil line)
			     listcheckhalfend)))
	 )

	(insert (format listformat (org-export-generic-fontify line))))
       ((string-match "^\\([ \t]+\\)\\([0-9]+\\.[ \t]*\\)" line)
	;;
	;; numbered list item
	;;
	;; TODO: nested lists
	;;
	(setq line (replace-match (if numlistleavenum "\\2" "") nil nil line))

	(org-export-generic-check-section "numliststart"
					  numlistprefix numlistsuffix)

	;; deal with checkboxes
	;; TODO: whoops; leaving the numbers is a problem for ^ matching
	(cond
	 ((string-match "\\(\\[ \\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listchecktodo nil nil line)
			     listchecktodoend)))
	 ((string-match "\\(\\[X\\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listcheckdone nil nil line)
			     listcheckdoneend)))
	 ((string-match "\\(\\[/\\]\\)[ \t]*" line)
	  (setq line (concat (replace-match listcheckhalf nil nil line)
			     listcheckhalfend)))
	 )

	(insert (format numlistformat (org-export-generic-fontify line))))

       ((equal line "ORG-BLOCKQUOTE-START")
        (setq line blockquotestart))
       ((equal line "ORG-BLOCKQUOTE-END")
        (setq line blockquoteend))
       ((string-match "^\\s-*$" line)
        ;; blank line
        (if bodynewline-paragraph
            (insert bodynewline-paragraph)))
       (t
	;;
	;; body
	;;
	(org-export-generic-check-section "body" bodytextpre bodytextsuf)

        (setq line
              (org-export-generic-fontify line))

	;; XXX: properties?  list?
	(if (string-match "^\\([ \t]*\\)\\([-+*][ \t]+\\)\\(.*?\\)\\( ::\\)" line)
	    (setq line (replace-match "\\1\\3:" t nil line)))

	(setq line (org-fix-indentation line org-generic-current-indentation))

	;; Remove forced line breaks
	(if (string-match "\\\\\\\\[ \t]*$" line)
	    (setq line (replace-match "" t t line)))

	(if bodylinewrap
	    ;; XXX: was dependent on wrap var which was calculated by???
	    (if (> (length line) bodylinewrap)
		(setq line
		      (org-export-generic-wrap line bodylinewrap))
	      (setq line line)))
	(insert (format bodylineform line)))))

    ;; if we're at a level > 0; insert the closing body level stuff
    (let ((counter 0))
      (while (> (- level counter) 0)
	(insert
	 (org-export-generic-format export-plist :body-section-suffix 0
				    (- level counter)))
	(setq counter (1+ counter))))

    (org-export-generic-check-section "bottom")

    (org-export-generic-push-links (nreverse link-buffer))

    (normal-mode)

    ;; insert the table of contents
    (when thetoc
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\[TABLE-OF-CONTENTS\\][ \t]*$" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (replace-match ""))
	(goto-char first-heading-pos))
      (mapc 'insert thetoc)
      (or (looking-at "[ \t]*\n[ \t]*\n")
	  (insert "\n\n")))

    ;; Convert whitespace place holders
    (goto-char (point-min))
    (let (beg end)
      (while (setq beg (next-single-property-change (point) 'org-whitespace))
	(setq end (next-single-property-change beg 'org-whitespace))
	(goto-char beg)
	(delete-region beg end)
	(insert (make-string (- end beg) ?\ ))))

    (save-buffer)

    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
	(setq end (next-single-property-change beg 'display))
	(delete-region beg end)
	(goto-char beg)
	(insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
	(setq end (next-single-property-change beg 'org-cwidth))
	(delete-region beg end)
	(goto-char beg)))
    (goto-char (point-min))))


(defun org-export-generic-format (export-plist prop &optional len n reverse)
  "converts a property specification to a string given types of properties

The EXPORT-PLIST should be defined as the lookup plist.
The PROP should be the property name to search for in it.
LEN is set to the length of multi-characters strings to generate (or 0)
N is the tree depth
REVERSE means to reverse the list if the plist match is a list
 "
  (let* ((prefixtype (plist-get export-plist prop))
	 subtype)
    (cond
     ((null prefixtype) "")
     ((and len (char-or-string-p prefixtype) (not (stringp prefixtype)))
      ;; sequence of chars
      (concat (make-string len prefixtype) "\n"))
     ((stringp prefixtype)
      prefixtype)
     ((and n (listp prefixtype))
      (if reverse
	  (setq prefixtype (reverse prefixtype)))
      (setq subtype (if (> n (length prefixtype))
			(car (last prefixtype))
		      (nth (1- n) prefixtype)))
      (if (stringp subtype)
	  subtype
	(concat (make-string len subtype) "\n")))
     (t ""))
    ))

(defun org-export-generic-header (header export-plist
				  prefixprop formatprop postfixprop
				  &optional n reverse)
  "convert a header to an output string given formatting property names"
  (let* ((formatspec (plist-get export-plist formatprop))
	 (len (length header)))
    (concat
     (org-export-generic-format export-plist prefixprop len n reverse)
     (format (or formatspec "%s") header)
     (org-export-generic-format export-plist postfixprop len n reverse))
    ))

(defun org-export-generic-preprocess (parameters)
  "Do extra work for ASCII export"
  ;; Put quotes around verbatim text
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (goto-char (match-end 2))
    (backward-delete-char 1) (insert "'")
    (goto-char (match-beginning 2))
    (delete-char 1) (insert "`")
    (goto-char (match-end 2)))
  ;; Remove target markers
  (goto-char (point-min))
  (while (re-search-forward  "<<<?\\([^<>]*\\)>>>?\\([ \t]*\\)" nil t)
    (replace-match "\\1\\2")))

(defun org-html-expand-for-generic (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-export-generic-wrap (line where)
  "Wrap LINE at or before WHERE."
  (let* ((ind (org-get-indentation line))
	 (indstr (make-string ind ?\ ))
	 (len (length line))
	 (result "")
	 pos didfirst)
    (while (> len where)
      (catch 'found
	(loop for i from where downto (/ where 2) do
	  (and (equal (aref line i) ?\ )
	       (setq pos i)
	       (throw 'found t))))
      (if pos
	  (progn
	    (setq result
		  (concat result
			  (if didfirst indstr "")
			  (substring line 0 pos)
			  "\n"))
	    (setq didfirst t)
	    (setq line (substring line (1+ pos)))
	    (setq len (length line)))
	(setq result (concat result line))
	(setq len 0)))
    (concat result indstr line)))

(defun org-export-generic-push-links (link-buffer)
  "Push out links in the buffer."
  (when link-buffer
    ;; We still have links to push out.
    (insert "\n")
    (let ((ind ""))
      (save-match-data
	(if (save-excursion
	      (re-search-backward
	       "^\\(\\([ \t]*\\)\\|\\(\\*+ \\)\\)[^ \t\n]" nil t))
	    (setq ind (or (match-string 2)
			  (make-string (length (match-string 3)) ?\ )))))
      (mapc (lambda (x) (insert ind "[" (car x) "]: " (cdr x) "\n"))
	    link-buffer))
    (insert "\n")))

(defun org-generic-level-start (level old-level title umax export-plist
				      &optional lines)
  "Insert a new level in a generic export."
  (let ((n (- level umax 1))
	(ind 0)
	(diff (- level old-level)) (counter 0)
	(secnums (plist-get export-plist :body-header-section-numbers))
	(secnumformat
	 (plist-get export-plist :body-header-section-number-format))
	char tagstring)
    (unless org-export-with-tags
      (if (string-match (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	  (setq title (replace-match "" t t title))))

    (cond
     ;; going deeper
     ((> level old-level)
      (while (< (+ old-level counter) (1- level))
	(insert
	 (org-export-generic-format export-plist :body-section-prefix 0
				    (+ old-level counter)))
	(setq counter (1+ counter))
	))
     ;; going up
     ((< level old-level)
      (while (> (- old-level counter) (1- level))
	(insert
	 (org-export-generic-format export-plist :body-section-suffix 0
				    (- old-level counter)))
	(setq counter (1+ counter))
	))
     ;; same level
     ((= level old-level)
      (insert
       (org-export-generic-format export-plist :body-section-suffix 0 level))
      )
     )
    (insert
     (org-export-generic-format export-plist :body-section-prefix 0 level))

    (if (and org-export-with-section-numbers
	     secnums
	     (or (not (numberp secnums))
		 (< level secnums)))
	(setq title
	      (concat (format (or secnumformat "%s ")
			      (org-section-number level)) title)))

    ;; handle tags and formatting
    (if (string-match
	 (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") title)
	(progn
	  (if (plist-get export-plist :body-tags-export)
	      (setq tagstring (org-export-generic-header (match-string 1 title)
							 export-plist
							 :body-tags-prefix
							 :body-tags-format
							 :body-tags-suffix)))
	  (string-match (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$") title)
	  (setq title (replace-match "" t t title)))
      (setq tagstring (plist-get export-plist :body-tags-none-string)))

    (insert
     (org-export-generic-header title export-plist
				:body-section-header-prefix
				:body-section-header-format
				:body-section-header-suffix
				level))
    (if tagstring
	(insert tagstring))

    (setq org-generic-current-indentation '(0 . 0))))

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- fill-column (string-width s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (string-width s) underline)
		"\n"))))

(defvar org-table-colgroup-info nil)
(defun org-format-table-generic (lines)
  "Format a table for ascii export."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (not (string-match "^[ \t]*|" (car lines)))
      ;; Table made by table.el - test for spanning
      lines

    ;; A normal org table
    ;; Get rid of hlines at beginning and end
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (when org-export-table-remove-special-lines
      ;; Check if the table has a marking column.  If yes remove the
      ;; column and the special lines
      (setq lines (org-table-clean-before-export lines)))
    ;; Get rid of the vertical lines except for grouping
    (let ((vl (org-colgroup-info-to-vline-list org-table-colgroup-info))
	  rtn line vl1 start)
      (while (setq line (pop lines))
	(if (string-match org-table-hline-regexp line)
	    (and (string-match "|\\(.*\\)|" line)
		 (setq line (replace-match " \\1" t nil line)))
	  (setq start 0 vl1 vl)
	  (while (string-match "|" line start)
	    (setq start (match-end 0))
	    (or (pop vl1) (setq line (replace-match " " t t line)))))
	(push line rtn))
      (nreverse rtn))))

(defun org-colgroup-info-to-vline-list (info)
  (let (vl new last)
    (while info
      (setq last new new (pop info))
      (if (or (memq last '(:end :startend))
	      (memq new  '(:start :startend)))
	  (push t vl)
	(push nil vl)))
    (setq vl (nreverse vl))
    (and vl (setcar vl nil))
    vl))


;;; FIXME: this should probably turn into a defconstant later [2010/05/20:rpg]
(defvar org-export-generic-emphasis-alist
  '(("*" format-boldify nil)
    ("/" format-italicize nil)
    ("_" format-underline nil)
    ("+" format-strikethrough nil)
    ("=" format-code t)
    ("~" format-verbatim t))
  "Alist of org format -> formatting variables for fontification.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a variable name, set in org-export-generic.  That
variable will be dereferenced to obtain a formatting string to wrap
fontified text with.
The third element decides whether to protect converted text from other
conversions.")

;;; Cargo-culted from the latex translation.  I couldn't figure out how
;;; to keep the structure since the generic export operates on lines, rather
;;; than on a buffer as in the latex export, meaning that none of the
;;; search forward code could be kept.  This led me to rewrite the
;;; whole thing recursively.  A huge lose for efficiency (potentially),
;;; but I couldn't figure out how to make the looping work.
;;; Worse, it's /doubly/ recursive, because this function calls
;;; org-export-generic-emph-format, which can call it recursively...
;;; [2010/05/20:rpg]
(defun org-export-generic-fontify (string)
  "Convert fontification according to generic rules."
  (if (string-match org-emph-re string)
        ;; The match goes one char after the *string*, except at the end of a line
        (let ((emph (assoc (match-string 3 string)
                           org-export-generic-emphasis-alist))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (unless emph
            (message "`org-export-generic-emphasis-alist' has no entry for formatting triggered by \"%s\""
                     (match-string 3 string)))
          ;; now we need to determine whether we have strikethrough or
          ;; a list, which is a bit nasty
          (if (and (equal (match-string 3 string) "+")
                   (save-match-data
                     (string-match "\\`-+\\'" (match-string 4 string))))
              ;; a list --- skip this match and recurse on the point after the
              ;; first emph char...
              (concat (substring string 0 (1+ (match-beginning 3)))
                      (org-export-generic-fontify (substring string (match-beginning 3))))
              (concat (substring string 0 beg) ;; part before the match
                      (match-string 1 string)
                      (org-export-generic-emph-format (second emph)
                                                      (match-string 4 string)
                                                      (third emph))
                      (or (match-string 5 string) "")
                      (org-export-generic-fontify (substring string end)))))
        string))

(defun org-export-generic-emph-format (format-varname string protect)
  "Return a string that results from applying the markup indicated by
FORMAT-VARNAME to STRING."
  (let ((format (symbol-value format-varname)))
    (let ((string-to-emphasize
           (if protect
               string
               (org-export-generic-fontify string))))
      (if format
          (format format string-to-emphasize)
          string-to-emphasize))))

(provide 'org-generic)
(provide 'org-export-generic)

;;; org-export-generic.el ends here
