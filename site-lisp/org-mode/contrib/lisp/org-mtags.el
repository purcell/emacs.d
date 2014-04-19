;;; org-mtags.el --- Muse-like tags in Org-mode

;; Copyright (C) 2008-2014 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.01
;;
;; This file is not yet part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This modules implements some of the formatting tags available in
;; Emacs Muse.  This is not a way if adding new functionality, but just
;; a different way to write some formatting directives.  The advantage is
;; that files written in this way can be read by Muse reasonably well,
;; and that this provides an alternative way of writing formatting
;; directives in Org, a way that some might find more pleasant to type
;; and look at that the Org's #+BEGIN..#+END notation.

;; The goal of this development is to make it easier for people to
;; move between both worlds as they see fit for different tasks.

;; The following muse tags will be translated during export into their
;; native Org equivalents:
;;
;;   <br>
;;        Needs to be at the end of a line.  Will be translated to "\\".
;;
;;   <example switches="-n -r">
;;        Needs to be on a line by itself, similarly the </example> tag.
;;        Will be translated into Org's #+BEGIN_EXAMPLE construct.
;;
;;   <quote>
;;        Needs to be on a line by itself, similarly the </quote> tag.
;;        Will be translated into Org's #+BEGIN_QUOTE construct.
;;
;;   <comment>
;;        Needs to be on a line by itself, similarly the </comment> tag.
;;        Will be translated into Org's #+BEGIN_COMMENT construct.
;;
;;   <verse>
;;        Needs to be on a line by itself, similarly the </verse> tag.
;;        Will be translated into Org's #+BEGIN_VERSE construct.
;;
;;   <contents>
;;        This gets translated into "[TABLE-OF-CONTENTS]".  It will not
;;        trigger the production of a table of contents - that is done
;;        in Org with the "#+OPTIONS: toc:t" setting.  But it will define
;;        the location where the TOC will be placed.
;;
;;   <literal style="STYLE">    ;; only latex, html, and docbook supported
;;        in Org.
;;        Needs to be on a line by itself, similarly the </literal> tag.
;;
;;   <src lang="LANG" switches="-n -r">
;;        Needs to be on a line by itself, similarly the </src> tag.
;;        Will be translated into Org's BEGIN_SRC construct.
;;
;;   <include file="FILE" markup="MARKUP" lang="LANG"
;;            prefix="str" prefix1="str" switches="-n -r">
;;        Needs to be on a line by itself.
;;        Will be translated into Org's #+INCLUDE construct.
;;
;; The lisp/perl/ruby/python tags can be implemented using the
;; `org-eval.el' module, which see.

(require 'org)

;;; Customization

(defgroup org-mtags nil
  "Options concerning Muse tags in Org mode."
  :tag "Org Muse Tags"
  :group 'org)

(defface org-mtags    ; similar to shadow
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for Muse-like tags in Org."
  :group 'org-mtags
  :group 'org-faces)

(defcustom org-mtags-prefer-muse-templates t
  "Non-nil means prefere Muse tags for structure elements.
This is relevane when expanding the templates defined in the variable
`org-structure-templates'."
  :group 'org-mtags
  :type 'boolean)

(defconst org-mtags-supported-tags
  '("example" "quote" "comment" "verse" "contents" "literal" "src" "include")
  "The tags that are supported by org-mtags.el for conversion.
In addition to this list, the <br> tag is supported as well.")

(defconst org-mtags-fontification-re
  (concat
   "^[ \t]*</?\\("
   (mapconcat 'identity org-mtags-supported-tags "\\|")
   "\\)\\>[^>]*>\\|<br>[ \t]*$")
  "Regular expression used for fontifying muse tags.")

(defun org-mtags-replace ()
  "Replace Muse-like tags with the appropriate Org constructs.
The is done in the entire buffer."
  (interactive) ;; FIXME
  (let ((re (concat "^[ \t]*\\(</?\\("
		    (mapconcat 'identity org-mtags-supported-tags "\\|")
		    "\\)\\>\\)"))
	info tag rpl style markup lang file prefix prefix1 switches)
    ;; First, do the <br> tag
    (goto-char (point-min))
    (while (re-search-forward "<br>[ \t]*$" nil t)
      (replace-match "\\\\" t t))
    ;; Now, all the other tags
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (goto-char (match-beginning 1))
      (setq info (org-mtags-get-tag-and-attributes))
      (if (not info)
	  (end-of-line 1)
	(setq tag (plist-get info :tag))
	(cond
	 ((equal tag "contents")
	  (setq rpl "[TABLE-OF-CONTENTS]")
	  ;; FIXME: also trigger TOC in options-plist?????
	  )
	 ((member tag '("quote" "comment" "verse"))
	  (if (plist-get info :closing)
	      (setq rpl (format "#+END_%s" (upcase tag)))
	    (setq rpl (format "#+BEGIN_%s" (upcase tag)))))
	 ((equal tag "literal")
	  (setq style (plist-get info :style))
	  (and style (setq style (downcase style)))
	  (if (plist-get info :closing)
	      (setq rpl (cond
			 ((member style '("latex"))
			  "#+END_LaTeX")
			 ((member style '("html"))
			  "#+END_HTML")
			 ((member style '("docbook"))
			  "#+END_DOCBOOK")
			 ((member style '("ascii"))
			  "#+END_ASCII")))
	    (setq rpl (cond
		       ((member style '("latex"))
			"#+BEGIN_LaTeX")
		       ((member style '("html"))
			"#+BEGIN_HTML")
		       ((member style '("ascii"))
			"#+BEGIN_ASCII")))))
	 ((equal tag "example")
	  (if (plist-get info :closing)
	      (setq rpl "#+END_EXAMPLE")
	    (setq rpl "#+BEGIN_EXAMPLE")
	    (when (setq switches (plist-get info :switches))
	      (setq rpl (concat rpl " " switches)))))
	 ((equal tag "src")
	  (if (plist-get info :closing)
	      (setq rpl "#+END_SRC")
	    (setq rpl "#+BEGIN_SRC")
	    (when (setq lang (plist-get info :lang))
	      (setq rpl (concat rpl " " lang))
	      (when (setq switches (plist-get info :switches))
		(setq rpl (concat rpl " " switches))))))
	 ((equal tag "include")
	  (setq file (plist-get info :file)
		markup (downcase (plist-get info :markup))
		lang (plist-get info :lang)
		prefix (plist-get info :prefix)
		prefix1 (plist-get info :prefix1)
		switches (plist-get info :switches))
	  (setq rpl "#+INCLUDE")
	  (setq rpl (concat rpl " " (prin1-to-string file)))
	  (when markup
	    (setq rpl (concat rpl " " markup))
	    (when (and (equal markup "src") lang)
	      (setq rpl (concat rpl " " lang))))
	  (when prefix
	    (setq rpl (concat rpl " :prefix " (prin1-to-string prefix))))
	  (when prefix1
	    (setq rpl (concat rpl " :prefix1 " (prin1-to-string prefix1))))
	  (when switches
	    (setq rpl (concat rpl " " switches)))))
	(when rpl
	  (goto-char (plist-get info :match-beginning))
	  (delete-region (point-at-bol) (plist-get info :match-end))
	  (insert rpl))))))

(defun org-mtags-get-tag-and-attributes ()
  "Parse a Muse-like tag at point ant rturn the information about it.
The return value is a property list which contains all the attributes
with string values.  In addition, it reutnrs the following properties:

:tag              The tag as a string.
:match-beginning  The beginning of the match, just before \"<\".
:match-end        The end of the match, just after \">\".
:closing          t when the tag starts with \"</\"."
  (when (looking-at "<\\(/\\)?\\([a-zA-Z]+\\>\\)\\([^>]*\\)>")
    (let ((start 0)
	  tag rest prop attributes endp val)
      (setq tag (org-match-string-no-properties 2)
	    endp (match-end 1)
	    rest (and (match-end 3)
		      (org-match-string-no-properties 3))
	    attributes (list :tag tag
			     :match-beginning (match-beginning 0)
			     :match-end (match-end 0)
			     :closing endp))
      (when rest
	(while (string-match "\\([a-zA-Z]+\\)=\\([^ \t\n>]+\\|\"[^>]+\"\\)"
			     rest start)
	  (setq start (match-end 0)
		prop (org-match-string-no-properties 1 rest)
		val (org-remove-double-quotes
		     (org-match-string-no-properties 2 rest)))
	  (setq attributes (plist-put attributes
				      (intern (concat ":" prop)) val))))
      attributes)))

(defun org-mtags-fontify-tags (limit)
  "Fontify the muse-like tags."
  (while (re-search-forward org-mtags-fontification-re limit t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(face org-mtags font-lock-multiline t
				font-lock-fontified t))))

(add-hook 'org-export-preprocess-hook 'org-mtags-replace)
(add-hook 'org-font-lock-hook 'org-mtags-fontify-tags)

(provide 'org-mtags)

;;; org-mtags.el ends here
