;;; rng-auto.el --- automatically extracted autoloads for RELAX NG

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(setq nxml-version "20041004")

(unless (and (fboundp 'make-hash-table)
	     (boundp 'fontification-functions))
  (error "FSF GNU Emacs version 21 or later required"))

(when (featurep 'mucs)
  (error "nxml-mode is not compatible with Mule-UCS"))

;; Add fix for Unicode-display bug in Emacs 21.1 on Windows (fixed in 21.2)
(when (and (fboundp 'w32-add-charset-info)
	   (boundp 'w32-charset-info-alist)
	   (not (assoc "iso10646-1" w32-charset-info-alist)))
  (w32-add-charset-info "iso10646-1" 'w32-charset-ansi t))

(defun rng-add-info-dir (dir)
  (cond ((and (boundp 'Info-directory-list) Info-directory-list)
	 (unless (member dir Info-directory-list)
	   (setq Info-directory-list
		 (append Info-directory-list (list dir)))
	   (when (boundp 'Info-dir-contents)
	     (setq Info-dir-contents nil))
	   (when (and (boundp 'Info-current-file)
		      (equal Info-current-file "dir")
		      (fboundp 'Info-find-node-2)
		      (get-buffer "*info*"))
	     (save-excursion
	       (set-buffer (get-buffer "*info*"))
	       (Info-find-node-2 t "Top")))))
	(t
	 (unless (member dir Info-default-directory-list)
	   (setq Info-default-directory-list
		 (append Info-default-directory-list (list dir)))))))

(let* ((dir (file-name-directory load-file-name))
       (schema-dir (concat dir "schema/")))
  (unless (member dir load-path)
    (setq load-path (cons dir load-path)))
  (setq rng-schema-locating-files-default
	(list "schemas.xml"
	      (abbreviate-file-name
	       (expand-file-name "schemas.xml" schema-dir))))
  (setq rng-schema-locating-file-schema-file
	(expand-file-name "locate.rnc" schema-dir))
  (rng-add-info-dir dir))

;; Users shouldn't edit this.
;; Instead they should customize `rng-preferred-prefix-alist'.

(setq rng-preferred-prefix-alist-default
      ;; For XHTML and RELAX NG, prefer no prefix
      '(("http://www.w3.org/1999/XSL/Transform" . "xsl")
	("http://www.w3.org/1999/02/22-rdf-syntax-ns#" . "rdf")))

(add-hook 'nxml-mode-hook 'rng-nxml-mode-init)
(add-hook 'nxml-mode-hook 'nxml-enable-unicode-char-name-sets)

(setq rng-schema-loader-alist '(("rnc" . rng-c-load-schema)))

;; This is needed for compatibility with versions before 21.4.
;; We need mode-line-process to be risky because otherwise we can't
;; :eval in its value.
(put 'mode-line-process 'risky-local-variable t)

(require 'nxml-enc)
;; Install our own `set-auto-coding-function'.
(nxml-start-auto-coding)


;;;### (autoloads (nxml-glyph-display-string) "nxml-glyph" "nxml-glyph.el"
;;;;;;  (16278 18099))
;;; Generated autoloads from nxml-glyph.el

(autoload (quote nxml-glyph-display-string) "nxml-glyph" "\
Return a string that can display a glyph for Unicode code-point N.
FACE gives the face that will be used for displaying the string.
Return nil if the face cannot display a glyph for N." nil nil)

;;;***

;;;### (autoloads (nxml-mode) "nxml-mode" "nxml-mode.el" (16702 54517))
;;; Generated autoloads from nxml-mode.el

(autoload (quote nxml-mode) "nxml-mode" "\
Major mode for editing XML.

Syntax highlighting is performed unless the variable
`nxml-syntax-highlight-flag' is nil.

\\[nxml-finish-element] finishes the current element by inserting an end-tag.
C-c C-i closes a start-tag with `>' and then inserts a balancing end-tag
leaving point between the start-tag and end-tag. 
\\[nxml-balanced-close-start-tag-block] is similar but for block rather than inline elements:
the start-tag, point, and end-tag are all left on separate lines.
If `nxml-slash-auto-complete-flag' is non-nil, then inserting a `</'
automatically inserts the rest of the end-tag.

\\[nxml-complete] performs completion on the symbol preceding point.

\\[nxml-dynamic-markup-word] uses the contents of the current buffer
to choose a tag to put around the word preceding point.

Validation is provided by the related minor-mode `rng-validate-mode'.
This also makes completion schema- and context- sensitive.  Element
names, attribute names, attribute values and namespace URIs can all be
completed. By default, `rng-validate-mode' is automatically enabled by
`rng-nxml-mode-init' which is normally added to `nxml-mode-hook'. You
can toggle it using \\[rng-validate-mode].

\\[indent-for-tab-command] indents the current line appropriately.
This can be customized using the variable `nxml-child-indent'
and the variable `nxml-attribute-indent'.

\\[nxml-insert-named-char] inserts a character reference using
the character's name (by default, the Unicode name). \\[universal-argument] \\[nxml-insert-named-char]
inserts the character directly.

The Emacs commands that normally operate on balanced expressions will
operate on XML markup items.  Thus \\[forward-sexp] will move forward
across one markup item; \\[backward-sexp] will move backward across
one markup item; \\[kill-sexp] will kill the following markup item;
\\[mark-sexp] will mark the following markup item.  By default, each
tag each treated as a single markup item; to make the complete element
be treated as a single markup item, set the variable
`nxml-sexp-element-flag' to t.  For more details, see the function
`nxml-forward-balanced-item'.

\\[nxml-backward-up-element] and \\[nxml-down-element] move up and
down the element structure.

Many aspects this mode can be customized using
\\[customize-group] nxml RET." t nil)

;;;***

;;;### (autoloads (nxml-enable-unicode-char-name-sets) "nxml-uchnm"
;;;;;;  "nxml-uchnm.el" (16270 38352))
;;; Generated autoloads from nxml-uchnm.el

(autoload (quote nxml-enable-unicode-char-name-sets) "nxml-uchnm" "\
Enable the use of Unicode standard names for characters.
The Unicode blocks for which names are enabled is controlled by
the variable `nxml-enabled-unicode-blocks'." t nil)

;;;***

;;;### (autoloads (rng-c-load-schema) "rng-cmpct" "rng-cmpct.el"
;;;;;;  (16280 36493))
;;; Generated autoloads from rng-cmpct.el

(autoload (quote rng-c-load-schema) "rng-cmpct" "\
Load a schema in RELAX NG compact syntax from FILENAME.
Return a pattern." nil nil)

;;;***

;;;### (autoloads (rng-write-version rng-format-manual rng-byte-compile-load
;;;;;;  rng-update-autoloads) "rng-maint" "rng-maint.el" (16279 23645))
;;; Generated autoloads from rng-maint.el

(autoload (quote rng-update-autoloads) "rng-maint" "\
Update the autoloads in rng-auto.el." t nil)

(autoload (quote rng-byte-compile-load) "rng-maint" "\
Byte-compile and load all of the RELAX NG library in an appropriate order." t nil)

(autoload (quote rng-format-manual) "rng-maint" "\
Create manual.texi from manual.xml." t nil)

(autoload (quote rng-write-version) "rng-maint" nil nil nil)

;;;***

;;;### (autoloads (rng-nxml-mode-init) "rng-nxml" "rng-nxml.el" (16294
;;;;;;  8571))
;;; Generated autoloads from rng-nxml.el

(autoload (quote rng-nxml-mode-init) "rng-nxml" "\
Initialize `nxml-mode' to take advantage of `rng-validate-mode'.
This is typically called from `nxml-mode-hook'.
Validation will be enabled if `rng-nxml-auto-validate-flag' is non-nil." t nil)

;;;***

;;;### (autoloads (rng-validate-mode) "rng-valid" "rng-valid.el"
;;;;;;  (16664 9855))
;;; Generated autoloads from rng-valid.el

(autoload (quote rng-validate-mode) "rng-valid" "\
Minor mode performing continual validation against a RELAX NG schema.

Checks whether the buffer is a well-formed XML 1.0 document,
conforming to the XML Namespaces Recommendation and valid against a
RELAX NG schema. The mode-line indicates whether it is or not.  Any
parts of the buffer that cause it not to be are considered errors and
are highlighted with `rng-error-face'. A description of each error is
available as a tooltip.  \\[rng-next-error] goes to the next error
after point. Clicking mouse-1 on the word `Invalid' in the mode-line
goes to the first error in the buffer. If the buffer changes, then it
will be automatically rechecked when Emacs becomes idle; the
rechecking will be paused whenever there is input pending..

By default, uses a vacuous schema that allows any well-formed XML
document. A schema can be specified explictly using
\\[rng-set-schema-file-and-validate], or implicitly based on the buffer's
file name or on the root element name.  In each case the schema must
be a RELAX NG schema using the compact schema (such schemas
conventionally have a suffix of `.rnc').  The variable
`rng-schema-locating-files' specifies files containing rules
to use for finding the schema." t nil)

;;;***

;;;### (autoloads (rng-xsd-compile) "rng-xsd" "rng-xsd.el" (16216
;;;;;;  26672))
;;; Generated autoloads from rng-xsd.el

(put (quote http://www\.w3\.org/2001/XMLSchema-datatypes) (quote rng-dt-compile) (quote rng-xsd-compile))

(autoload (quote rng-xsd-compile) "rng-xsd" "\
Provides W3C XML Schema as a RELAX NG datatypes library. NAME is a
symbol giving the local name of the datatype.  PARAMS is a list of
pairs (PARAM-NAME . PARAM-VALUE) where PARAM-NAME is a symbol giving
the name of the parameter and PARAM-VALUE is a string giving its
value.  If NAME or PARAMS are invalid, it calls rng-dt-error passing
it arguments in the same style as format; the value from rng-dt-error
will be returned.  Otherwise, it returns a list.  The first member of
the list is t if any string is a legal value for the datatype and nil
otherwise.  The second argument is a symbol; this symbol will be
called as a function passing it a string followed by the remaining
members of the list.  The function must return an object representing
the value of the datatype that was represented by the string, or nil
if the string is not a representation of any value. The object
returned can be any convenient non-nil value, provided that, if two
strings represent the same value, the returned objects must be equal." nil nil)

;;;***

;;;### (autoloads (xmltok-get-declared-encoding-position) "xmltok"
;;;;;;  "xmltok.el" (16664 8418))
;;; Generated autoloads from xmltok.el

(autoload (quote xmltok-get-declared-encoding-position) "xmltok" "\
Return the position of the encoding in the XML declaration at point.
If there is a well-formed XML declaration starting at point and it
contains an encoding declaration, then return (START . END)
where START and END are the positions of the start and the end
of the encoding name; if there is no encoding declaration return
the position where and encoding declaration could be inserted.
If there is XML that is not well-formed that looks like an XML declaration,
return nil.  Otherwise, return t.
If LIMIT is non-nil, then do not consider characters beyond LIMIT." nil nil)

;;;***

;;; rng-auto.el ends here
