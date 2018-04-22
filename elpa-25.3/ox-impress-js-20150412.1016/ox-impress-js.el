;;; ox-impress-js.el --- impress.js Back-End for Org Export Engine

;; Copyright (C) 2014, 2015 Takumi Kinjo.

;; Author: Takumi Kinjo <takumi dot kinjo at gmail dot org>
;; URL: https://github.com/kinjo/org-impress-js.el
;; Version: 0.1
;; Package-Requires: ((org "8"))
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a impress.js back-end for Org
;; generic exporter based on ox-html.el.

;; See http://orgmode.org/ about Org-mode and see 
;; http://bartaz.github.io/impress.js/ about impress.js.
;; I appreciate for their great works.

;; Original author: Carsten Dominik <carsten at orgmode dot org>
;;      Jambunathan K <kjambunathan at gmail dot com>

;;; Code:

;;; Dependencies

(require 'ox-html)

;;; Define Back-End

(org-export-define-derived-backend 'impress-js 'html
  :translate-alist 
  '((headline . org-impress-js-headline)
    (inner-template . org-impress-js-inner-template)
    (section . org-impress-js-section)
    (template . org-impress-js-template))  
  :menu-entry
  '(?j "Export to impress.js HTML"
       ((?J "As impress.js HTML buffer" org-impress-js-export-as-html)
	(?j "As impress.js HTML file" org-impress-js-export-to-html)
	(?o "As impress.js HTML file and open"
	    (lambda (a s v b)
	      (if a (org-impress-js-export-to-html t s v b)
		(org-open-file (org-impress-js-export-to-html nil s v b)))))))
  :options-alist
  '((:html-doctype "HTML_DOCTYPE" nil org-impress-js-doctype)
    (:html-description nil nil org-impress-js-description)
    (:html-fallback-message nil nil org-impress-js-fallback-message)
    (:html-hint-message nil nil org-impress-js-hint-message)
    (:html-hint-js nil nil org-impress-js-hint-js)
    (:html-impress-js-stylesheet "IMPRESSJS_STYLE" nil org-impress-js-stylesheet newline)
    (:html-impress-js-javascript "IMPRESSJS_SRC" nil org-impress-js-javascript newline)
    (:impress-js-toc "IMPRESSJS_TOC" nil nil newline)
    (:impress-js-title "IMPRESSJS_TITLE" nil nil newline)))


;;; Internal Variables

(defconst org-impress-js-data-props
  '(data-x data-y data-z data-scale data-rotate data-rotate-x data-rotate-y data-rotate-z
	   trans-x trans-y trans-z rotate rotate-x rotate-y rotate-z)
  "Org properties for data properties for impress.js.")

(defvar org-impress-js-slide-angles '(0 0 0 0)
  "Vector for the slide angles.")

(defvar org-impress-js-slide-position '(0 0 0 0)
  "Vector for the slide position.")

(defvar org-impress-js-toc-data-plist nil
  "Data properties of TOC slide.")

(defvar org-impress-js-title-data-plist nil
  "Data properties of title slide.")


;;; User Configuration Variables

;;;; For impress.js.

(defcustom org-impress-js-default-slide-class "step slide"
  "Default of the class attribute for the slides.
\"step\" or \"step slide\" are available.")

(defcustom org-impress-js-default-trans '(1000 0 0)
  "Default transfer vector for slide. List are corresponding to
X, Y and Z axis.")

(defcustom org-impress-js-default-rot '(0 0 0)
  "Default rotational vector for slide. List are angles by degrees
around X, Y and Z axis.")

(defcustom org-impress-js-description
  "impress.js is a presentation tool based on the power of CSS3 \
transforms and transitions in modern browsers and inspired by \
the idea behind prezi.com."
  "impress.js description."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-fallback-message
  "    <p>Your browser <b>doesn't support the features required</b> \
by impress.js, so you are presented with a simplified version of this \
presentation.</p>
<p>For the best experience please use the latest <b>Chrome</b>, \
<b>Safari</b> or <b>Firefox</b> browser.</p>
"
  "impress.js fallback-message."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-hint-message
  "    <p>Use a spacebar or arrow keys to navigate</p>\n"
  "impress.js hint message."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-hint-js
  "if (\"ontouchstart\" in document.documentElement) {
document.querySelector(\".hint\").innerHTML = \"\
<p>Tap on the left or right to navigate</p>\";
}
"
  "impress.js hint JavaScript."
  :group 'org-export-impress-js
  :type 'string)

(defcustom org-impress-js-divs
  '((preamble  "div" "preamble")
    (content   "div" "impress")
    (postamble "div" "postamble"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of 'preamble, 'content or 'postamble.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list :greedy t
	       (list :tag "Preamble"
		     (const :format "" preamble)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Postamble" (const :format "" postamble)
		     (string :tag "     id") (string :tag "element"))))

;;;; Template :: Generic

(defconst org-impress-js-doctype "html5"
  "Document type definition to use for exported impress.js HTML files.")

;;;; Template :: Styles

(defcustom org-impress-js-stylesheet "resources/css/impress-demo.css"
  "Path to the default CSS file for impress.js.

Use IMPRESSJS_STYLE option in your Org file is available too."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-impress-js-javascript "resources/js/impress.js"
  "Path to the JavaScript file for impress.js.

Use IMPRESSJS_SRC option in your Org file is available too."
  :group 'org-export-impress-js
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)


;;; Matrix calculation functions

(defmacro mnth (i j m)
  "Return i-j-th value on 4x4 matrix correspond as below.

  | m00 m01 m02 m03 |
  | m10 m11 m12 m13 |
  | m20 m21 m22 m23 |
  | m30 m31 m32 m33 |"
  (list 'nth j (list 'nth i m)))

(defmacro vnth (i v)
  "Return i-th value on 1x4 row vecotr."
  (list 'nth i v))

(defun make-matx (m)
  "Make new matrix from `m'. `m' is 4x4 matrix."
  (copy-tree m))

(defun unit-matx ()
  "Return 4x4 unit matrix."
  (make-matx '((1 0 0 0)
	       (0 1 0 0)
	       (0 0 1 0)
	       (0 0 0 1))))

(defun add-vec (v0 v1)
  "Add vectors `v0' and `v1'. `v0' and `v1' are 4-vectors."
  (list (+ (vnth 0 v0) (vnth 0 v1))
	(+ (vnth 1 v0) (vnth 1 v1))
	(+ (vnth 2 v0) (vnth 2 v1))
	(+ (vnth 3 v0) (vnth 3 v1))))

(defun matx-vec-prod (m v)
  "Product of `m' and `v'. `m' is 4x4 matrix and `v' is 4-vector."
  (list
   (+ (* (vnth 0 v) (mnth 0 0 m)) (* (vnth 1 v) (mnth 0 1 m))
      (* (vnth 2 v) (mnth 0 2 m)) (* (vnth 3 v) (mnth 0 3 m)))
   (+ (* (vnth 0 v) (mnth 1 0 m)) (* (vnth 1 v) (mnth 1 1 m))
      (* (vnth 2 v) (mnth 1 2 m)) (* (vnth 3 v) (mnth 1 3 m)))
   (+ (* (vnth 0 v) (mnth 2 0 m)) (* (vnth 1 v) (mnth 2 1 m))
      (* (vnth 2 v) (mnth 2 2 m)) (* (vnth 3 v) (mnth 2 3 m)))
   (+ (* (vnth 0 v) (mnth 3 0 m)) (* (vnth 1 v) (mnth 3 1 m))
      (* (vnth 2 v) (mnth 3 2 m)) (* (vnth 3 v) (mnth 3 3 m)))))

(defun vec-matx-prod (v m)
  "Product of `v' and `m'. `v' is 4-vector and `m' is 4x4 matrix."
  (list
   (+ (* (vnth 0 v) (mnth 0 0 m)) (* (vnth 1 v) (mnth 1 0 m))
      (* (vnth 2 v) (mnth 2 0 m)) (* (vnth 3 v) (mnth 3 0 m)))
   (+ (* (vnth 0 v) (mnth 0 1 m)) (* (vnth 1 v) (mnth 1 1 m))
      (* (vnth 2 v) (mnth 2 1 m)) (* (vnth 3 v) (mnth 3 1 m)))
   (+ (* (vnth 0 v) (mnth 0 2 m)) (* (vnth 1 v) (mnth 1 2 m))
      (* (vnth 2 v) (mnth 2 2 m)) (* (vnth 3 v) (mnth 3 2 m)))
   (+ (* (vnth 0 v) (mnth 0 3 m)) (* (vnth 1 v) (mnth 1 3 m))
      (* (vnth 2 v) (mnth 2 3 m)) (* (vnth 3 v) (mnth 3 3 m)))))

(defun matx-matx-prod (m0 m1)
  "Product of `m0' and `m1'. `m0' and `m1' are 4x4 matrices."
  (list
   (vec-matx-prod (nth 0 m0) m1)
   (vec-matx-prod (nth 1 m0) m1)
   (vec-matx-prod (nth 2 m0) m1)
   (vec-matx-prod (nth 3 m0) m1)))

(defun rot-matx-z (m r)
  "Return a matrix rotated around Z axis. `m' is 4x4 matrix and
`r' is a radian angle around Z axis."
  (let ((u (unit-matx)))
    (setf (mnth 0 0 u) (cos r))
    (setf (mnth 0 1 u) (- (sin r)))
    (setf (mnth 1 0 u) (sin r))
    (setf (mnth 1 1 u) (cos r))
    (matx-matx-prod u m)))

(defun rot-matx-x (m r)
  "Return a matrix rotated around X axis. `m' is 4x4 matrix and
`r' is a radian angle around X axis."
  (let ((u (unit-matx)))
    (setf (mnth 1 1 u) (cos r))
    (setf (mnth 1 2 u) (- (sin r)))
    (setf (mnth 2 1 u) (sin r))
    (setf (mnth 2 2 u) (cos r))
    (matx-matx-prod u m)))

(defun rot-matx-y (m r)
  "Return a matrix rotated around Y axis. `m' is 4x4 matrix and
`r' is a radian angle around Y axis."
  (let ((u (unit-matx)))
    (setf (mnth 0 0 u) (cos r))
    (setf (mnth 2 0 u) (sin r))
    (setf (mnth 0 2 u) (- (sin r)))
    (setf (mnth 2 2 u) (cos r))
    (matx-matx-prod u m)))

(defun rot-matx (m rx ry rz)
  "Return a matrix rotated around Z-Y-X. `m' is 4x4 matrix.
`rx', `ry' and `rz' are angles around each axies."
  (rot-matx-x (rot-matx-y (rot-matx-z m rz) ry) rx))

(defun matx-euler (m)
  "Return euler angles (rx ry rz) extracted from `M'. `M' is 4x4
rotation matrix calculated as Z-Y-X euler angles."
  (list (- (atan (mnth 1 2 m) (mnth 2 2 m)))
	(atan (- (mnth 0 2 m)) (sqrt (+ (* (mnth 1 2 m) (mnth 1 2 m)) (* (mnth 2 2 m) (mnth 2 2 m)))))
	(- (atan (mnth 0 1 m) (mnth 0 0 m)))))


;;; Internal Functions

(defun org-impress-js-xhtml-p (info) nil)

(defun org-impress-js-html5-p (info) t)

(defun org-impress-js-close-tag (tag attr info)
  (concat "<" tag " " attr " />"))

(defun org-impress-js-doctype (info) "Return correct html doctype tag." "<!DOCTYPE html>")

(defun org-impress-js-begin (property)
  "Initialize variables when exporting started.

This is called from org-export-before-processing-hook."
  (when (eq 'impress-js property)
    (setq org-impress-js-toc-data-plist nil
	  org-impress-js-title-data-plist nil
	  org-impress-js-slide-angles '(0 0 0 0)
	  org-impress-js-slide-position '(0 0 0 0))))

(defun org-impress-js-to-number (v)
  "Convert to number."
  (and v (string-to-number (format "%s" v))))
       
(defun org-impress-js-parse-keyword (keyword-value)
  "Parse keyword value."
  (let ((props (or keyword-value "")))
    (let ((plist)
	  (all (append org-impress-js-data-props '(class))))
      (dolist (p all)
	(when (and (string-match (concat "\\(\\`\\|[ \t]\\)"
					 (regexp-quote (symbol-name p))
					 ":\\(([^)\n]+)\\|[^ \t\n\r;,]*\\)")
				 props))
	  (setq plist (plist-put plist
				 p
				 (car (read-from-string
				       (match-string 2 props)))))))
      plist)))

(defun org-impress-js-set-default-data-plist (plist)
  "Set default properties for translation if no translation
values are given by `plist' and return plist.
`plist' has values for each property declared in `org-impress-js-data-props'."
  (and (not (or (plist-get plist 'data-scale)
		(plist-get plist 'data-x)
		(plist-get plist 'data-y)
		(plist-get plist 'data-z)
		(plist-get plist 'trans-x)
		(plist-get plist 'trans-y)
		(plist-get plist 'trans-z)
		(plist-get plist 'data-rotate-x)
		(plist-get plist 'data-rotate-y)
		(plist-get plist 'data-rotate-z)
		(plist-get plist 'data-rotate)
		(plist-get plist 'rotate-x)
		(plist-get plist 'rotate-y)
		(plist-get plist 'rotate-z)
		(plist-get plist 'rotate)))
       (plist-put plist 'trans-x (vnth 0 org-impress-js-default-trans))
       (plist-put plist 'trans-y (vnth 1 org-impress-js-default-trans))
       (plist-put plist 'trans-z (vnth 2 org-impress-js-default-trans))
       (plist-put plist 'rotate-x (vnth 0 org-impress-js-default-rot))
       (plist-put plist 'rotate-y (vnth 1 org-impress-js-default-rot))
       (plist-put plist 'rotate-z (vnth 2 org-impress-js-default-rot)))
  plist)


;;; Template

(defun org-impress-js--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-html-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :html-description))
	(keywords (plist-get info :keywords))
	(charset (or (and org-html-coding-system
			  (fboundp 'coding-system-get)
			  (coding-system-get org-html-coding-system
					     'mime-charset))
		     "iso-8859-1")))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
	 (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (format
      (if (org-impress-js-html5-p info)
	  (org-impress-js-close-tag "meta" " charset=\"%s\"" info)
	(org-impress-js-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
     (org-impress-js-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (org-impress-js-close-tag "meta" " name=\"viewport\" content=\"width=1024\"" info)
     "\n"
     (org-impress-js-close-tag "meta" " name=\"apple-mobile-web-app-capable\" content=\"yes\"" info) "\n"
     (and (org-string-nw-p author)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"author\" content=\"%s\""
				       (funcall protect-string author))
			       info)
	   "\n"))
     (and (org-string-nw-p description)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"description\" content=\"%s\"\n"
				       (funcall protect-string description))
			       info)
	   "\n"))
     (and (org-string-nw-p keywords)
	  (concat
	   (org-impress-js-close-tag "meta"
			       (format " name=\"keywords\" content=\"%s\""
				       (funcall protect-string keywords))
			       info)
	   "\n"))
      (org-impress-js-close-tag "link" " href=\"http://fonts.googleapis.com/css?family=Open+Sans:regular,semibold,italic,italicsemibold|PT+Sans:400,700,400italic,700italic|PT+Serif:400,700,400italic,700italic\" rel=\"stylesheet\"" info) "\n"
      (org-impress-js-close-tag "link" " rel=\"shortcut icon\" href=\"favicon.png\"" info) "\n"
      (org-impress-js-close-tag "link" " rel=\"apple-touch-icon\" href=\"apple-touch-icon.png\"" info) "\n")))

(defun org-impress-js--build-impress-js-stylesheet (info)
  "Return a link tag to load impress.js CSS file.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (when (plist-get info :html-impress-js-stylesheet)
      (org-impress-js-close-tag "link"
			  (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-impress-js-stylesheet))
			  info)))))

(defun org-impress-js--build-fallback-message (info)
  "Return impress.js fallback-message as a string.
INFO is a plist used as a communication channel."
  (concat "<div class=\"fallback-message\">\n"
	  (plist-get info :html-fallback-message)
	  "</div>\n"))

(defun org-impress-js--build-title (info)
  "Return a title slide.

Postamble will be embeded if available. See `org-html-postamble'."
  (org-element-normalize-string
   (concat
    (let* ((props (org-impress-js--global-slide-plist info :impress-js-title))
	   (class (plist-get props 'class))
	   (attrs org-impress-js-title-data-plist))
      (format "<div id=\"title\" class=\"%s\" %s>\n" class attrs))
    ;; Document title.
    (let ((title (plist-get info :title)))
      (format "<h1>%s</h1>\n" (org-export-data (or title "") info)))
    (org-html--build-pre/postamble 'postamble info)
    "</div>\n")))

(defun org-impress-js--build-hint-message (info)
  "Return impress.js hint message as a string.
INFO is a plist used as a communication channel."
  (concat "<div class=\"hint\">\n"
	  (plist-get info :html-hint-message)
	  "</div>\n"))

(defun org-impress-js--build-init-impress-js (info)
  "Return a init script for impress.js as a string.
INFO is a plist used as a communication channel."
  (concat "<script>\n"
	  (plist-get info :html-hint-js)
	  "</script>\n"
	  (format "<script src=\"%s\"></script>\n"
		  (plist-get info :html-impress-js-javascript))
	  "<script>impress().init();</script>\n"))

(defun org-impress-js--build-data-attrs (plist)
  "Return the data attributes for impress.js as a string.
`plist' has values for each property declared in `org-impress-js-data-props'."
  (let ((data-x (plist-get plist 'data-x))
	(data-y (plist-get plist 'data-y))
	(data-z (plist-get plist 'data-z))
	(data-scale (plist-get plist 'data-scale))
	(data-rotate (plist-get plist 'data-rotate))
	(data-rotate-x (plist-get plist 'data-rotate-x))
	(data-rotate-y (plist-get plist 'data-rotate-y))
	(data-rotate-z (plist-get plist 'data-rotate-z)))
    (concat (format " data-x=\"%0.8f\"" data-x)
	    (format " data-y=\"%0.8f\"" data-y)
	    (format " data-z=\"%0.8f\"" data-z)
	    (and data-scale (format " data-scale=\"%0.8f\"" data-scale))
	    (and data-rotate (format " data-rotate=\"%0.8f\"" data-rotate))
	    (format " data-rotate-x=\"%0.8f\"" data-rotate-x)
	    (format " data-rotate-y=\"%0.8f\"" data-rotate-y)
	    (format " data-rotate-z=\"%0.8f\"" data-rotate-z))))

(defun org-impress-js-inner-template (contents info)
  "Return body of document string after impress.js HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-impress-js-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun org-impress-js-template (contents info)
  "Return complete document string after impress.js HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-impress-js-html5-p info)) (org-impress-js-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
			      org-html-xml-declaration)
			 (cdr (assoc (plist-get info :html-extension)
				     org-html-xml-declaration))
			 (cdr (assoc "html" org-html-xml-declaration))

			 "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
		  (or (and org-html-coding-system
			   (fboundp 'coding-system-get)
			   (coding-system-get org-html-coding-system 'mime-charset))
		      "iso-8859-1"))))))
   (org-impress-js-doctype info)
   "\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (org-impress-js--build-meta-info info)
   (org-impress-js--build-impress-js-stylesheet info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body class=\"impress-not-supported\">\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   ;; (org-html--build-pre/postamble 'preamble info)
   ;; Fallback message.
   (org-impress-js--build-fallback-message info)
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-impress-js-divs))
	   (nth 2 (assq 'content org-impress-js-divs)))
   ;; Title.
   (org-impress-js--build-title info)
   contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-impress-js-divs)))
   ;; Postamble.
   ;; (org-html--build-pre/postamble 'postamble info)
   ;; Hint message.
   (org-impress-js--build-hint-message info)
   ;; impress.js init.
   (org-impress-js--build-init-impress-js info)
   ;; Closing document.
   "</body>\n</html>"))


;;; Tables of Contents

(defun org-impress-js-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let* ((toc-entries
	  (mapcar (lambda (headline)
		    (cons (org-impress-js--format-toc-headline headline info)
			  (org-export-get-relative-level headline info)))
		  (org-export-collect-headlines info depth)))
	 (outer-tag (if (and (org-html-html5-p info)
			     (plist-get info :html-html5-fancy))
			"nav"
		      "div"))
	 (props (org-impress-js--global-slide-plist info :impress-js-toc))
	 (class (plist-get props 'class))
	 (attrs org-impress-js-toc-data-plist))
    (when toc-entries
      (concat (format "<%s id=\"table-of-contents\" class=\"%s\" %s>\n"
		      outer-tag class attrs)
	      (format "<h1>%s</h1>\n"
		      (org-html--translate "Table of Contents" info))
	      "<div id=\"text-table-of-contents\">"
	      (org-html--toc-text toc-entries)
	      "</div>\n"
	      (format "</%s>\n" outer-tag)))))

(defun org-impress-js--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		;; Create an anonymous back-end that will ignore any
		;; footnote-reference, link, radio-target and target
		;; in table of contents.
		(org-export-create-backend
		 :parent 'html
		 :transcoders '((footnote-reference . ignore)
				(link . (lambda (object c i) c))
				(radio-target . (lambda (object c i) c))
				(target . ignore)))
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a href=\"#/%s\">%s</a>"
	    ;; Label.
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (concat "outline-container-"
			 (org-export-get-headline-id headline info))))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (plist-get info :html-format-headline-function)
		    todo todo-type priority text tags :section-number nil)))))

(defun org-impress-js--global-slide-plist (info option)
  "Read properties for position, translation and rotation from an export
option. These properties are used for title and TOC slide building.

INFO is a plist holding contextual information. OPTION is a string
represents an export option."
  (let ((plist (org-impress-js-parse-keyword (plist-get info option))))
    (setq plist
	  (plist-put plist 'class
		     (concat "step "
			     (and (plist-get plist 'class)
				  (format "%s" (plist-get plist 'class))))))
    (dolist (p org-impress-js-data-props)
      (plist-put plist p (org-impress-js-to-number (plist-get plist p))))
    (or (plist-get plist 'data-scale)
	(plist-put plist 'data-scale 1))
    (org-impress-js-set-default-data-plist plist)))


;;; Transcode Functions

;;;; Headline

(defun org-impress-js--angles (plist)
  "Return a vector for the slide angles and set it into
`org-impress-js-slide-angles'.
`plist' has values for each property declared in `org-impress-js-data-props'."
  (let* ((data-rotate (plist-get plist 'data-rotate))
	 (data-rotate-x (plist-get plist 'data-rotate-x))
	 (data-rotate-y (plist-get plist 'data-rotate-y))
	 (data-rotate-z (plist-get plist 'data-rotate-z))
	 (rotate (plist-get plist 'rotate))
	 (rotate-x (plist-get plist 'rotate-x))
	 (rotate-y (plist-get plist 'rotate-y))
	 (rotate-z (plist-get plist 'rotate-z))
	 (rot (let ((angles org-impress-js-slide-angles))
		(matx-matx-prod
		 (rot-matx (unit-matx)
			   (if data-rotate-x (degrees-to-radians data-rotate-x) (vnth 0 angles))
			   (if data-rotate-y (degrees-to-radians data-rotate-y) (vnth 1 angles))
			   ;; `data-rotate-z' is prioritized than `data-rotate'.
			   (if data-rotate-z (degrees-to-radians data-rotate-z)
			     (if data-rotate (degrees-to-radians data-rotate)
			       (vnth 2 angles))))
		 (rot-matx (unit-matx)
			   (degrees-to-radians (or rotate-x 0))
			   (- (degrees-to-radians (or rotate-y 0)))
			   ;; `rotate-z' is prioritized than `rotate'.
			   (degrees-to-radians (or rotate-z rotate 0)))))))
    (setq org-impress-js-slide-angles (matx-euler rot))))

(defun org-impress-js--trans (plist angles)
  "Return a vector for the slide position and set it into
`org-impress-js-slide-position'.
`plist' has values for each property declared in `org-impress-js-data-props'.
`angles' is a vector for the slide angles is return by `org-impress-js--angles'."
  (let ((data-x (plist-get plist 'data-x))
	(data-y (plist-get plist 'data-y))
	(data-z (plist-get plist 'data-z))
	(trans-x (plist-get plist 'trans-x))
	(trans-y (plist-get plist 'trans-y))
	(trans-z (plist-get plist 'trans-z)))
    (setq org-impress-js-slide-position
	  (let ((tran (add-vec
		       org-impress-js-slide-position
		       (matx-vec-prod (rot-matx (unit-matx)
						(vnth 0 angles)
						(vnth 1 angles)
						(- (vnth 2 angles) pi))
				      (list (- (or trans-x 0))
					    (or trans-y 0)
					    (or trans-z 0)
					    1)))))
	    ;; Reset coordinates if corresponding data are given.
	    (and data-x (setf (vnth 0 tran) data-x))
	    (and data-y (setf (vnth 1 tran) data-y))
	    (and data-z (setf (vnth 2 tran) data-z))
	    tran))))

(defun org-impress-js--node-plist (headline)
  "Return the property values declared in the current headline.
Properties can be handled here are declared in `org-impress-js-data-props'."
  (let ((plist))
    (dolist (p org-impress-js-data-props)
      (setq plist
	    (plist-put plist p
		       (org-impress-js-to-number
			(org-export-get-node-property
			 (intern (format ":%s" (upcase (symbol-name p))))
			 headline)))))
    plist))

(defun org-impress-js-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Empty contents?
  (setq contents (or contents ""))
  (when (null org-impress-js-title-data-plist)
    (setq org-impress-js-title-data-plist
	  (let* ((props (org-impress-js--global-slide-plist info :impress-js-title))
		 (angles (org-impress-js--angles props))
		 (trans (org-impress-js--trans props angles))
		 (degrees (list
			   (radians-to-degrees (vnth 0 angles))
			   (- (radians-to-degrees (vnth 1 angles)))
			   (radians-to-degrees (vnth 2 angles))))
		 (attrs
		  (org-impress-js--build-data-attrs
		   `(data-x ,(vnth 0 trans) data-y ,(vnth 1 trans) data-z ,(vnth 2 trans)
			    data-scale ,(plist-get props 'data-scale)
			    data-rotate-x ,(vnth 0 degrees)
			    data-rotate-y ,(vnth 1 degrees)
			    data-rotate-z ,(vnth 2 degrees)))))
	    attrs)))
  (when (and (plist-get info :with-toc) (null org-impress-js-toc-data-plist))
    (setq org-impress-js-toc-data-plist
	  (let* ((props (org-impress-js--global-slide-plist info :impress-js-toc))
		 (angles (org-impress-js--angles props))
		 (trans (org-impress-js--trans props angles))
		 (degrees (list
			   (radians-to-degrees (vnth 0 angles))
			   (- (radians-to-degrees (vnth 1 angles)))
			   (radians-to-degrees (vnth 2 angles))))
		 (attrs
		  (org-impress-js--build-data-attrs
		   `(data-x ,(vnth 0 trans) data-y ,(vnth 1 trans) data-z ,(vnth 2 trans)
			    data-scale ,(plist-get props 'data-scale)
			    data-rotate-x ,(vnth 0 degrees)
			    data-rotate-y ,(vnth 1 degrees)
			    data-rotate-z ,(vnth 2 degrees)))))
	    attrs)))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text
	  (if (plist-get info :html-format-headline-function)
	      (funcall (plist-get info :html-format-headline-function)
		       todo todo-type priority text tags info)
	    (full-text (org-html-format-headline--wrap headline info))))
	 ;; Attributes used to position presentation steps
	 (class (org-export-get-node-property :CLASS headline))
	 (props (org-impress-js-set-default-data-plist
		 (org-impress-js--node-plist headline))))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
	     (itemized-body (org-html-format-list-item
			     contents type nil info nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "-"))
	     (ids (delq nil
			(list (org-element-property :CUSTOM_ID headline)
			      (org-export-get-headline-id headline info)
			      (org-element-property :ID headline))))
	     (preferred-id (car ids))
	     (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
	     ;; Ignore the section indentations.
	     (level1 1)
	     (first-content (car (org-element-contents headline)))
	     (angles (org-impress-js--angles props))
	     (trans (org-impress-js--trans props angles))
	     (degrees (list
		       (radians-to-degrees (vnth 0 angles))
		       (- (radians-to-degrees (vnth 1 angles)))
		       (radians-to-degrees (vnth 2 angles)))))
	(format "<%s id=\"%s\" class=\"%s\" %s>%s%s\n"
		(org-html--container headline info)
		(if (null (cdr ids))
		    (concat "outline-container-" preferred-id)
		  preferred-id)
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class
			(concat " " (if class class org-impress-js-default-slide-class)))
		(org-impress-js--build-data-attrs
		 `(data-x ,(vnth 0 trans) data-y ,(vnth 1 trans) data-z ,(vnth 2 trans)
			  data-scale ,(plist-get props 'data-scale)
			  data-rotate-x ,(vnth 0 degrees)
			  data-rotate-y ,(vnth 1 degrees)
			  data-rotate-z ,(vnth 2 degrees)))
		(format "\n<h%d>%s</h%d>\n"
			level
			(concat
			 (and numberedp
			      (format
			       "<span class=\"section-number-%d\">%s</span> "
			       level
			       (mapconcat #'number-to-string numbers ".")))
			 full-text)
			level)
		;; When there is no section, pretend there is an empty
		;; one to get the correct <div class="outline- ...>
		;; which is needed by `org-info.js'.
		(if (not (eq (org-element-type first-content) 'section))
		    (concat (org-impress-js-section first-content "" info)
			    contents)
		  contents)))))))

;;;; Section

(defun org-impress-js-section (section contents info)
  "Transcode a SECTION element from Org to HTML for impress.js.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
			   (1- (plist-get info :html-toplevel-hlevel))))
	     (section-number
	      (and (org-export-numbered-headline-p parent info)
		   (mapconcat
		    #'number-to-string
		    (org-export-get-headline-number parent info) "-"))))
        ;; Build return value.
	(format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>\n</div>"
		class-num
		(or (org-element-property :CUSTOM_ID parent)
		    section-number
		    (org-export-get-headline-id parent info))
		(or contents ""))))))


;;; End-user functions

;;;###autoload
(defun org-impress-js-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'impress-js "*Org HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-impress-js-convert-region-to-html ()
  "Assume the current region has org-mode syntax, and convert it to
impress.js HTML.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an HTML buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'impress-js))

;;;###autoload
(defun org-impress-js-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a impress.js HTML file.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'impress-js file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-impress-js-publish-to-html (plist filename pub-dir)
  "Publish an org file to impress.js HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'impress-js filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))

(add-hook 'org-export-before-processing-hook 'org-impress-js-begin)

(provide 'ox-impress-js)
;;; ox-impress-js.el ends here
