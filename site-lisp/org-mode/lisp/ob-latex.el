;;; ob-latex.el --- org-babel functions for latex "evaluation"

;; Copyright (C) 2009-2014 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

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

;;; Commentary:

;; Org-Babel support for evaluating LaTeX source code.
;;
;; Currently on evaluation this returns raw LaTeX code, unless a :file
;; header argument is given in which case small png or pdf files will
;; be created directly form the latex source code.

;;; Code:
(require 'ob)

(declare-function org-create-formula-image "org" (string tofile options buffer))
(declare-function org-splice-latex-header "org"
		  (tpl def-pkg pkg snippets-p &optional extra))
(declare-function org-latex-guess-inputenc "ox-latex" (header))
(declare-function org-latex-compile "ox-latex" (file))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("latex" . "tex"))

(defvar org-format-latex-header)	  ; From org.el
(defvar org-format-latex-options)	  ; From org.el
(defvar org-latex-default-packages-alist) ; From org.el
(defvar org-latex-packages-alist)	  ; From org.el

(defvar org-babel-default-header-args:latex
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a LaTeX source block.")

(defcustom org-babel-latex-htlatex ""
  "The htlatex command to enable conversion of latex to SVG or HTML."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-latex-htlatex-packages
  '("[usenames]{color}" "{tikz}" "{color}" "{listings}" "{amsmath}")
  "Packages to use for htlatex export."
  :group 'org-babel
  :type '(repeat (string)))

(defun org-babel-expand-body:latex (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body))) (mapcar #'cdr (org-babel-get-header params :var)))
  (org-babel-trim body))

(defun org-babel-execute:latex (body params)
  "Execute a block of Latex code with Babel.
This function is called by `org-babel-execute-src-block'."
  (setq body (org-babel-expand-body:latex body params))
  (if (cdr (assoc :file params))
      (let* ((out-file (cdr (assoc :file params)))
	     (tex-file (org-babel-temp-file "latex-" ".tex"))
	     (border (cdr (assoc :border params)))
	     (imagemagick (cdr (assoc :imagemagick params)))
	     (im-in-options (cdr (assoc :iminoptions params)))
	     (im-out-options (cdr (assoc :imoutoptions params)))
	     (pdfpng (cdr (assoc :pdfpng params)))
	     (fit (or (cdr (assoc :fit params)) border))
	     (height (and fit (cdr (assoc :pdfheight params))))
	     (width (and fit (cdr (assoc :pdfwidth params))))
	     (headers (cdr (assoc :headers params)))
	     (in-buffer (not (string= "no" (cdr (assoc :buffer params)))))
	     (org-latex-packages-alist
	      (append (cdr (assoc :packages params)) org-latex-packages-alist)))
        (cond
         ((and (string-match "\\.png$" out-file) (not imagemagick))
          (org-create-formula-image
           body out-file org-format-latex-options in-buffer))
         ((string-match "\\.tikz$" out-file)
	  (when (file-exists-p out-file) (delete-file out-file))
	  (with-temp-file out-file
	    (insert body)))
	 ((or (string-match "\\.pdf$" out-file) imagemagick)
	  (with-temp-file tex-file
	    (require 'ox-latex)
	    (insert
	     (org-latex-guess-inputenc
	      (org-splice-latex-header
	       org-format-latex-header
	       (delq
		nil
		(mapcar
		 (lambda (el)
		   (unless (and (listp el) (string= "hyperref" (cadr el)))
		     el))
		 org-latex-default-packages-alist))
	       org-latex-packages-alist
	       nil))
	     (if fit "\n\\usepackage[active, tightpage]{preview}\n" "")
	     (if border (format "\\setlength{\\PreviewBorder}{%s}" border) "")
	     (if height (concat "\n" (format "\\pdfpageheight %s" height)) "")
	     (if width  (concat "\n" (format "\\pdfpagewidth %s" width))   "")
	     (if headers
		 (concat "\n"
			 (if (listp headers)
			     (mapconcat #'identity headers "\n")
			   headers) "\n")
	       "")
	     (if fit
		 (concat "\n\\begin{document}\n\\begin{preview}\n" body
			 "\n\\end{preview}\n\\end{document}\n")
	       (concat "\n\\begin{document}\n" body "\n\\end{document}\n"))))
          (when (file-exists-p out-file) (delete-file out-file))
	  (let ((transient-pdf-file (org-babel-latex-tex-to-pdf tex-file)))
	    (cond
	     ((string-match "\\.pdf$" out-file)
	      (rename-file transient-pdf-file out-file))
	     (imagemagick
	      (convert-pdf
	       transient-pdf-file out-file im-in-options im-out-options)
	      (when (file-exists-p transient-pdf-file)
		(delete-file transient-pdf-file))))))
	 ((and (or (string-match "\\.svg$" out-file)
		   (string-match "\\.html$" out-file))
	       (not (string= "" org-babel-latex-htlatex)))
	  (with-temp-file tex-file
	    (insert (concat
		     "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
"
		     (mapconcat (lambda (pkg)
				  (concat "\\usepackage" pkg))
				org-babel-latex-htlatex-packages
				"\n")
		     "\\begin{document}"
		     body
		     "\\end{document}")))
	  (when (file-exists-p out-file) (delete-file out-file))
	  (let ((default-directory (file-name-directory tex-file)))
	    (shell-command (format "%s %s" org-babel-latex-htlatex tex-file)))
	  (cond
	   ((file-exists-p (concat (file-name-sans-extension tex-file) "-1.svg"))
	    (if (string-match "\\.svg$" out-file)
		(progn
		  (shell-command "pwd")
		  (shell-command (format "mv %s %s"
					 (concat (file-name-sans-extension tex-file) "-1.svg")
					 out-file)))
	      (error "SVG file produced but HTML file requested.")))
	   ((file-exists-p (concat (file-name-sans-extension tex-file) ".html"))
	    (if (string-match "\\.html$" out-file)
		(shell-command "mv %s %s"
			       (concat (file-name-sans-extension tex-file)
				       ".html")
			       out-file)
	      (error "HTML file produced but SVG file requested.")))))
         ((string-match "\\.\\([^\\.]+\\)$" out-file)
          (error "Can not create %s files, please specify a .png or .pdf file or try the :imagemagick header argument"
		 (match-string 1 out-file))))
        nil) ;; signal that output has already been written to file
    body))

(defun convert-pdf (pdffile out-file im-in-options im-out-options)
  "Generate a file from a pdf file using imagemagick."
  (let ((cmd (concat "convert " im-in-options " " pdffile " "
		     im-out-options " " out-file)))
    (message (concat "Converting pdffile file " cmd  "..."))
    (shell-command cmd)))

(defun org-babel-latex-tex-to-pdf (file)
  "Generate a pdf file according to the contents FILE."
  (require 'ox-latex)
  (org-latex-compile file))

(defun org-babel-prep-session:latex (session params)
  "Return an error because LaTeX doesn't support sessions."
  (error "LaTeX does not support sessions"))


(provide 'ob-latex)
;;; ob-latex.el ends here
