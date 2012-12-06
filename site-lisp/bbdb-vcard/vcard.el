;;; vcard.el --- vcard parsing and display routines

;; Copyright (C) 1997, 1999, 2000 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: vcard, mail, news
;; Created: 1997-09-27

;; $Id: vcard.el,v 1.11 2000/06/29 17:07:55 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Unformatted vcards are just plain ugly.  But if you live in the MIME
;; world, they are a better way of exchanging contact information than
;; freeform signatures since the former can be automatically parsed and
;; stored in a searchable index.
;;
;; This library of routines provides the back end necessary for parsing
;; vcards so that they can eventually go into an address book like BBDB
;; (although this library does not implement that itself).  Also included
;; is a sample pretty-printer which MUAs can use which do not provide their
;; own vcard formatters.

;; This library does not interface directly with any mail user agents.  For
;; an example of bindings for the VM MUA, see vm-vcard.el available from
;;
;;    http://www.splode.com/~friedman/software/emacs-lisp/index.html#mail
;;
;; Updates to vcard.el should be available there too.

;; The main entry point to this package is `vcard-pretty-print' although
;; any documented variable or function is considered part of the API for
;; operating on vcard data.

;; The vcard 2.1 format is defined by the versit consortium.
;; See http://www.imc.org/pdi/vcard-21.ps
;;
;; RFC 2426 defines the vcard 3.0 format.
;; See ftp://ftp.rfc-editor.org/in-notes/rfc2426.txt

;; A parsed vcard is a list of attributes of the form
;;
;;     (proplist value1 value2 ...)
;;
;; Where proplist is a list of property names and parameters, e.g.
;;
;;     (property1 (property2 . parameter2) ...)
;;
;; Each property has an associated implicit or explicit parameter value
;; (not to be confused with attribute values; in general this API uses
;; `parameter' to refer to property values and `value' to refer to attribute
;; values to avoid confusion).  If a property has no explicit parameter value,
;; the parameter value is considered to be `t'.  Any property which does not
;; exist for an attribute is considered to have a nil parameter.

;; TODO:
;;   * Finish supporting the 3.0 extensions.
;;     Currently, only the 2.1 standard is supported.
;;   * Handle nested vcards and grouped attributes?
;;     (I've never actually seen one of these in use.)
;;   * Handle multibyte charsets.
;;   * Inverse of vcard-parse-string: write .VCF files from alist
;;   * Implement a vcard address book?  Or is using BBDB preferable?
;;   * Improve the sample formatter.

;;; Code:

(defgroup vcard nil
  "Support for the vCard electronic business card format."
  :group 'vcard
  :group 'mail
  :group 'news)

;;;###autoload
(defcustom vcard-pretty-print-function 'vcard-format-sample-box
  "*Formatting function used by `vcard-pretty-print'."
  :type 'function
  :group 'vcard)

;;;###autoload
(defcustom vcard-standard-filters
  '(vcard-filter-html
    vcard-filter-adr-newlines
    vcard-filter-tel-normalize
    vcard-filter-textprop-cr)
  "*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'."
  :type 'hook
  :group 'vcard)


;;; No user-settable options below.

;; XEmacs 21 ints and chars are disjoint types.
;; For all else, treat them as the same.
(defalias 'vcard-char-to-int
  (if (fboundp 'char-to-int) 'char-to-int 'identity))

;; This is just the version number for this package; it does not refer to
;; the vcard format specification.  Currently, this package does not yet
;; support the full vcard 3.0 specification.
;;
;; Whenever any part of the API defined in this package change in a way
;; that is not backward-compatible, the major version number here should be
;; incremented.  Backward-compatible additions to the API should be
;; indicated by increasing the minor version number.
(defconst vcard-api-version "2.0")

;; The vcard standards allow specifying the encoding for an attribute using
;; these values as immediate property names, rather than parameters of the
;; `encoding' property.  If these are encountered while parsing, associate
;; them as parameters of the `encoding' property in the returned structure.
(defvar vcard-encoding-tags
  '("quoted-printable" "base64" "8bit" "7bit"))

;; The vcard parser will auto-decode these encodings when they are
;; encountered.  These methods are invoked via vcard-parse-region-value.
(defvar vcard-region-decoder-methods
  '(("quoted-printable" . vcard-region-decode-quoted-printable)
    ("base64"           . vcard-region-decode-base64)))

;; This is used by vcard-region-decode-base64
(defvar vcard-region-decode-base64-table
  (let* ((a "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (len (length a))
         (tbl (make-vector 123 nil))
         (i 0))
    (while (< i len)
      (aset tbl (vcard-char-to-int (aref a i)) i)
      (setq i (1+ i)))
    tbl))


;;; This function can be used generically by applications to obtain
;;; a printable representation of a vcard.

;;;###autoload
(defun vcard-pretty-print (vcard)
  "Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist."
  (and (stringp vcard)
       (setq vcard (vcard-parse-string vcard)))
  (funcall vcard-pretty-print-function vcard))


;;; Parsing routines

;;;###autoload
(defun vcard-parse-string (raw &optional filter)
  "Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.\)
If supplied to this function an alist of the form

    \(\(\(\"prop1a\"\) \"value1a\"\)
     \(\(\"prop2a\" \"prop2b\" \(\"prop2c\" . \"param2c\"\)\) \"value2a\"\)
     \(\(\"prop3a\" \"prop3b\"\) \"value3a\" \"value3b\" \"value3c\"\)\)

would be returned."
  (let ((vcard nil)
        (buf (generate-new-buffer " *vcard parser work*")))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          ;; Make sure last line is newline-terminated.
          ;; An extra trailing newline is harmless.
          (insert raw "\n")
          (setq vcard (vcard-parse-region (point-min) (point-max) filter)))
      (kill-buffer buf))
    vcard))

;;;###autoload
(defun vcard-parse-region (beg end &optional filter)
  "Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!"
  (or filter
      (setq filter 'vcard-standard-filter))
  (let ((case-fold-search t)
        (vcard-data nil)
        (pos (make-marker))
        (newpos (make-marker))
        properties value)
    (save-restriction
      (narrow-to-region beg end)
      (save-match-data
        ;; Unfold folded lines and delete naked carriage returns
        (goto-char (point-min))
        (while (re-search-forward "\r$\\|\n[ \t]" nil t)
          (goto-char (match-beginning 0))
          (delete-char 1))

        (goto-char (point-min))
        (re-search-forward "^begin:[ \t]*vcard[ \t]*\n")
        (set-marker pos (point))
        (while (and (not (looking-at "^end[ \t]*:[ \t]*vcard[ \t]*$"))
                    (re-search-forward ":[ \t]*" nil t))
          (set-marker newpos (match-end 0))
          (setq properties
                (vcard-parse-region-properties pos (match-beginning 0)))
          (set-marker pos (marker-position newpos))
          (re-search-forward "[ \t]*\n")
          (set-marker newpos (match-end 0))
          (setq value
                (vcard-parse-region-value properties pos (match-beginning 0)))
          (set-marker pos (marker-position newpos))
          (goto-char pos)
          (funcall filter properties value)
          (setq vcard-data (cons (cons properties value) vcard-data)))))
    (nreverse vcard-data)))

(defun vcard-parse-region-properties (beg end)
  (downcase-region beg end)
  (let* ((proplist (vcard-split-string (buffer-substring beg end) ";"))
         (props proplist)
         split)
    (save-match-data
      (while props
        (cond ((string-match "=" (car props))
               (setq split (vcard-split-string (car props) "=" 2))
               (setcar props (cons (car split) (car (cdr split)))))
              ((member (car props) vcard-encoding-tags)
               (setcar props (cons "encoding" (car props)))))
        (setq props (cdr props))))
    proplist))

(defun vcard-parse-region-value (proplist beg end)
  (let* ((encoding (vcard-get-property proplist "encoding"))
         (decoder (cdr (assoc encoding vcard-region-decoder-methods)))
         result pos match-beg match-end)
    (save-restriction
      (narrow-to-region beg end)
      (cond (decoder
             ;; Each `;'-separated field needs to be decoded and saved
             ;; separately; if the entire region were decoded at once, we
             ;; would not be able to distinguish between the original `;'
             ;; chars and those which were encoded in order to quote them
             ;; against being treated as field separators.
             (goto-char beg)
             (setq pos (set-marker (make-marker) (point)))
             (setq match-beg (make-marker))
             (setq match-end (make-marker))
             (save-match-data
               (while (< pos (point-max))
                 (cond ((search-forward ";" nil t)
                        (set-marker match-beg (match-beginning 0))
                        (set-marker match-end (match-end 0)))
                       (t
                        (set-marker match-beg (point-max))
                        (set-marker match-end (point-max))))
                 (funcall decoder pos match-beg)
                 (setq result (cons (buffer-substring pos match-beg) result))
                 (set-marker pos (marker-position match-end))))
             (setq result (nreverse result))
             (vcard-set-property proplist "encoding" nil))
            (t
             (setq result (vcard-split-string (buffer-string) ";")))))
    (goto-char (point-max))
    result))


;;; Functions for retrieving property or value information from parsed
;;; vcard attributes.

(defun vcard-values (vcard have-props &optional non-props limit)
  "Return the values in VCARD.
This function is like `vcard-ref' and takes the same arguments, but return
only the values, not the associated property lists."
  (mapcar 'cdr (vcard-ref vcard have-props non-props limit)))

(defun vcard-ref (vcard have-props &optional non-props limit)
  "Return the attributes in VCARD with HAVE-PROPS properties.
Optional arg NON-PROPS is a list of properties which candidate attributes
must not have.
Optional arg LIMIT means return no more than that many attributes.

The attributes in VCARD which have all properties specified by HAVE-PROPS
but not having any specified by NON-PROPS are returned.  The first element
of each attribute is the actual property list; the remaining elements are
the values.

If a specific property has an associated parameter \(e.g. an encoding\),
use the syntax \(\"property\" . \"parameter\"\) to specify it.  If property
parameter is not important or it has no specific parameter, just specify
the property name as a string."
  (let ((attrs vcard)
        (result nil)
        (count 0))
    (while (and attrs (or (null limit) (< count limit)))
      (and (vcard-proplist-all-properties (car (car attrs)) have-props)
           (not (vcard-proplist-any-properties (car (car attrs)) non-props))
           (setq result (cons (car attrs) result)
                 count (1+ count)))
      (setq attrs (cdr attrs)))
    (nreverse result)))

(defun vcard-proplist-all-properties (proplist props)
  "Returns nil unless PROPLIST contains all properties specified in PROPS."
  (let ((result t))
    (while (and result props)
      (or (vcard-get-property proplist (car props))
          (setq result nil))
      (setq props (cdr props)))
    result))

(defun vcard-proplist-any-properties (proplist props)
  "Returns `t' if PROPLIST contains any of the properties specified in PROPS."
  (let ((result nil))
    (while (and (not result) props)
      (and (vcard-get-property proplist (car props))
           (setq result t))
      (setq props (cdr props)))
    result))

(defun vcard-get-property (proplist property)
  "Return the value from PROPLIST of PROPERTY.
PROPLIST is a vcard attribute property list, which is normally the first
element of each attribute entry in a vcard."
  (or (and (member property proplist) t)
      (cdr (assoc property proplist))))

(defun vcard-set-property (proplist property value)
  "In PROPLIST, set PROPERTY to VALUE.
PROPLIST is a vcard attribute property list.
If VALUE is nil, PROPERTY is deleted."
  (let (elt)
    (cond ((null value)
           (vcard-delete-property proplist property))
          ((setq elt (member property proplist))
           (and value (not (eq value t))
                (setcar elt (cons property value))))
          ((setq elt (assoc property proplist))
           (cond ((eq value t)
                  (setq elt (memq elt proplist))
                  (setcar elt property))
                 (t
                  (setcdr elt value))))
          ((eq value t)
           (nconc proplist (cons property nil)))
          (t
           (nconc proplist (cons (cons property value) nil))))))

(defun vcard-delete-property (proplist property)
  "Delete from PROPLIST the specified property PROPERTY.
This will not succeed in deleting the first member of the proplist, but
that element should never be deleted since it is the primary key."
  (let (elt)
    (cond ((setq elt (member property proplist))
           (delq (car elt) proplist))
          ((setq elt (assoc property proplist))
           (delq (car (memq elt proplist)) proplist)))))


;;; Vcard data filters.
;;;
;;; Filters receive both the property list and value list and may modify
;;; either in-place.  The return value from the filters are ignored.
;;;
;;; These filters can be used for purposes such as removing HTML tags or
;;; normalizing phone numbers into a standard form.

(defun vcard-standard-filter (proplist values)
  "Apply filters in `vcard-standard-filters' to attributes."
  (vcard-filter-apply-filter-list vcard-standard-filters proplist values))

;; This function could be used to dispatch other filter lists.
(defun vcard-filter-apply-filter-list (filter-list proplist values)
  (while filter-list
    (funcall (car filter-list) proplist values)
    (setq filter-list (cdr filter-list))))

;; Some lusers put HTML (or even javascript!) in their vcards under the
;; misguided notion that it's a standard feature of vcards just because
;; Netscape supports this feature.  That is wrong; the vcard specification
;; does not define any html content semantics and most MUAs cannot do
;; anything with html text except display them unparsed, which is ugly.
;;
;; Thank Netscape for abusing the standard and damned near rendering it
;; useless for interoperability between MUAs.
;;
;; This filter does a very rudimentary job.
(defun vcard-filter-html (proplist values)
  "Remove HTML tags from attribute values."
  (save-match-data
    (while values
      (while (string-match "<[^<>\n]+>" (car values))
        (setcar values (replace-match "" t t (car values))))
      (setq values (cdr values)))))

(defun vcard-filter-adr-newlines (proplist values)
  "Replace newlines with \"; \" in `adr' values."
  (and (vcard-get-property proplist "adr")
       (save-match-data
         (while values
           (while (string-match "[\r\n]+" (car values))
             (setcar values (replace-match "; " t t (car values))))
           (setq values (cdr values))))))

(defun vcard-filter-tel-normalize (proplist values)
  "Normalize telephone numbers in `tel' values.
Spaces and hyphens are replaced with `.'.
US domestic telephone numbers are replaced with international format."
  (and (vcard-get-property proplist "tel")
       (save-match-data
         (while values
           (while (string-match "[\t._-]+" (car values))
             (setcar values (replace-match " " t t (car values))))
           (and (string-match "^(?\\(\\S-\\S-\\S-\\))? ?\
\\(\\S-\\S-\\S- \\S-\\S-\\S-\\S-\\)"
                              (car values))
                (setcar values
                        (replace-match "+1 \\1 \\2" t nil (car values))))
           (setq values (cdr values))))))

(defun vcard-filter-textprop-cr (proplist values)
  "Strip carriage returns from text values."
  (and (vcard-proplist-any-properties
        proplist '("adr" "email" "fn" "label" "n" "org" "tel" "title" "url"))
       (save-match-data
         (while values
           (while (string-match "\r+" (car values))
             (setcar values (replace-match "" t t (car values))))
           (setq values (cdr values))))))


;;; Decoding methods.

(defmacro vcard-hexstring-to-ascii (s)
  (if (string-lessp emacs-version "20")
      `(format "%c" (car (read-from-string (format "?\\x%s" ,s))))
    `(format "%c" (string-to-number ,s 16))))

(defun vcard-hexstring-utf8-to-unicode (one two)
  (decode-coding-string (unibyte-string (string-to-number one 16) (string-to-number two 16)) 'utf-8))

(defun vcard-region-decode-quoted-printable (&optional beg end)
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-region (or beg (point-min)) (or end (point-max)))
        (goto-char (point-min))
        (while (re-search-forward "=\n" nil t)
          (delete-region (match-beginning 0) (match-end 0)))
        (goto-char (point-min))
        (while (re-search-forward "=\\([0-9A-Za-z][0-9A-Za-z]\\)=\\([0-9A-Fa-f][0-9A-fa-f]\\)" nil t)
            (replace-match (vcard-hexstring-utf8-to-unicode (match-string 1) (match-string 2)) t t))))))

(defun vcard-region-decode-base64 (&optional beg end)
  (save-restriction
    (narrow-to-region (or beg (point-min)) (or end (point-max)))
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward "[ \t\r\n]+" nil t)
        (delete-region (match-beginning 0) (match-end 0))))
    (goto-char (point-min))
    (let ((count 0)
          (n 0)
          (c nil))
      (while (not (eobp))
        (setq c (char-after (point)))
        (delete-char 1)
        (cond ((char-equal c ?=)
               (if (= count 2)
                   (insert (lsh n -10))
                 ;; count must be 3
                 (insert (lsh n -16) (logand 255 (lsh n -8))))
               (delete-region (point) (point-max)))
              (t
               (setq n (+ n (aref vcard-region-decode-base64-table
                                  (vcard-char-to-int c))))
               (setq count (1+ count))
               (cond ((= count 4)
                      (insert (logand 255 (lsh n -16))
                              (logand 255 (lsh n -8))
                              (logand 255 n))
                      (setq n 0 count 0))
                     (t
                      (setq n (lsh n 6))))))))))


(defun vcard-split-string (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

(defun vcard-copy-tree (tree)
  "Make a deep copy of nested conses."
  (cond
   ((consp tree)
    (cons (vcard-copy-tree (car tree))
          (vcard-copy-tree (cdr tree))))
   (t tree)))

(defun vcard-flatten (l)
  (if (consp l)
      (apply 'nconc (mapcar 'vcard-flatten l))
    (list l)))


;;; Sample formatting routines.

(defun vcard-format-sample-box (vcard)
  "Like `vcard-format-sample-string', but put an ascii box around text."
  (let* ((lines (vcard-format-sample-lines vcard))
         (len (vcard-format-sample-max-length lines))
         (edge (concat "\n+" (make-string (+ len 2) ?-) "+\n"))
         (line-fmt (format "| %%-%ds |" len))
         (formatted-lines
          (mapconcat (function (lambda (s) (format line-fmt s))) lines "\n")))
    (if (string= formatted-lines "")
        formatted-lines
      (concat edge formatted-lines edge))))

(defun vcard-format-sample-string (vcard)
  "Format VCARD into a string suitable for display to user.
VCARD should be a parsed vcard alist.  The result is a string
with formatted vcard information which can be inserted into a mime
presentation buffer."
  (mapconcat 'identity (vcard-format-sample-lines vcard) "\n"))

(defun vcard-format-sample-lines (vcard)
  (let* ((name  (vcard-format-sample-get-name vcard))
         (title (vcard-format-sample-values-concat vcard '("title") 1 "; "))
         (org   (vcard-format-sample-values-concat vcard '("org")   1 "; "))
         (addr  (vcard-format-sample-get-address vcard))
         (tel   (vcard-format-sample-get-telephone vcard))
         (lines (delete nil (vcard-flatten (list name title org addr))))
         (col-template (format "%%-%ds%%s"
                               (vcard-format-sample-offset lines tel)))
         (l lines))
    (while tel
      (setcar l (format col-template (car l) (car tel)))
      ;; If we stripped away too many nil slots from l, add empty strings
      ;; back in so setcar above will work on next iteration.
      (and (cdr tel)
           (null (cdr l))
           (setcdr l (cons "" nil)))
      (setq l (cdr l))
      (setq tel (cdr tel)))
    lines))

(defun vcard-format-sample-get-name (vcard)
  (let ((name (car (car (vcard-values vcard '("fn") nil 1))))
        (email (car (vcard-format-sample-values
                     vcard '((("email" "pref"))
                             (("email" "internet"))
                             (("email"))) 1))))
    (cond ((and name email)
           (format "%s <%s>" name email))
          (email)
          (name)
          (""))))

(defun vcard-format-sample-get-telephone (vcard)
  (let ((fields '(("Work: "
                   (("tel" "work" "pref")  . ("fax" "pager" "cell"))
                   (("tel" "work" "voice") . ("fax" "pager" "cell"))
                   (("tel" "work")         . ("fax" "pager" "cell")))
                  ("Home: "
                   (("tel" "home" "pref")  . ("fax" "pager" "cell"))
                   (("tel" "home" "voice") . ("fax" "pager" "cell"))
                   (("tel" "home")         . ("fax" "pager" "cell"))
                   (("tel")                . ("fax" "pager" "cell" "work")))
                  ("Cell: "
                   (("tel" "cell" "pref"))
                   (("tel" "cell")))
                  ("Fax:  "
                   (("tel" "pref" "fax"))
                   (("tel" "work" "fax"))
                   (("tel" "home" "fax"))
                   (("tel" "fax")))))
        (phones nil)
        result)
    (while fields
      (setq result (vcard-format-sample-values vcard (cdr (car fields))))
      (while result
        (setq phones
              (cons (concat (car (car fields)) (car (car result))) phones))
        (setq result (cdr result)))
      (setq fields (cdr fields)))
    (nreverse phones)))

(defun vcard-format-sample-get-address (vcard)
  (let* ((addr (vcard-format-sample-values vcard '((("adr" "pref" "work"))
                                                   (("adr" "pref"))
                                                   (("adr" "work"))
                                                   (("adr"))) 1))
         (street (delete "" (list (nth 0 addr) (nth 1 addr) (nth 2 addr))))
         (city-list (delete "" (nthcdr 3 addr)))
         (city (cond ((null (car city-list)) nil)
                     ((cdr city-list)
                      (format "%s, %s"
                              (car city-list)
                              (mapconcat 'identity (cdr city-list) " ")))
                     (t (car city-list)))))
    (delete nil (if city
                    (append street (list city))
                  street))))

(defun vcard-format-sample-values-concat (vcard have-props limit sep)
  (let ((l (car (vcard-values vcard have-props nil limit))))
    (and l (mapconcat 'identity (delete "" (vcard-copy-tree l)) sep))))

(defun vcard-format-sample-values (vcard proplists &optional limit)
  (let ((result (vcard-format-sample-ref vcard proplists limit)))
    (if (equal limit 1)
        (cdr result)
      (mapcar 'cdr result))))

(defun vcard-format-sample-ref (vcard proplists &optional limit)
  (let ((result nil))
    (while (and (null result) proplists)
      (setq result (vcard-ref vcard
                              (car (car proplists))
                              (cdr (car proplists))
                              limit))
      (setq proplists (cdr proplists)))
    (if (equal limit 1)
        (vcard-copy-tree (car result))
      (vcard-copy-tree result))))

(defun vcard-format-sample-offset (row1 row2 &optional maxwidth)
  (or maxwidth (setq maxwidth (frame-width)))
  (let ((max1 (vcard-format-sample-max-length row1))
        (max2 (vcard-format-sample-max-length row2)))
    (if (zerop max1)
        0
      (+ max1 (min 5 (max 1 (- maxwidth (+ max1 max2))))))))

(defun vcard-format-sample-max-length (strings)
  (let ((maxlen 0))
    (while strings
      (setq maxlen (max maxlen (length (car strings))))
      (setq strings (cdr strings)))
    maxlen))

(provide 'vcard)

;;; vcard.el ends here.
