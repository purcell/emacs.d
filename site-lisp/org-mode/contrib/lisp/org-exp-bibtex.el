;;; org-exp-bibtex.el --- Export bibtex fragments

;; Copyright (C) 2009-2012 Taru Karttunen

;; Author: Taru Karttunen <taruti@taruti.net>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is an utility to handle BibTeX export to both LaTeX and html
;; exports. It uses the bibtex2html software from
;; http://www.lri.fr/~filliatr/bibtex2html/
;;
;; The usage is as follows:
;; #+BIBLIOGRAPHY: bibfilebasename stylename optional-options
;; e.g. given foo.bib and using style plain:
;; #+BIBLIOGRAPHY: foo plain option:-d
;;
;; Optional options are of the form:
;;
;; option:-foobar pass '-foobar' to bibtex2html
;; e.g.
;; option:-d sort by date.
;; option:-a sort as BibTeX (usually by author) *default*
;; option:-u unsorted i.e. same order as in .bib file
;; option:-r reverse the sort.
;; see the bibtex2html man page for more. Multiple options can be combined like:
;; option:-d option:-r
;;
;; Limiting to only the entries cited in the document:
;; limit:t

;; For LaTeX export this simply inserts the lines
;; \bibliographystyle{plain}
;; \bibliography{foo}
;; into the tex-file when exporting.

;; For Html export it:
;; 1) converts all \cite{foo} to links to the bibliography
;; 2) creates a foo.html and foo_bib.html
;; 3) includes the contents of foo.html in the exported html file

(require 'org)
(require 'org-exp)

(defvar org-export-current-backend) ; dynamically bound in org-exp.el
(defun org-export-bibtex-preprocess ()
  "Export all BibTeX."
  (interactive)
  (save-window-excursion
    (setq oebp-cite-plist '())

    ;; Convert #+BIBLIOGRAPHY: name style
    (goto-char (point-min))
    (while (re-search-forward "^#\\+BIBLIOGRAPHY:[ \t]+\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\([^\r\n]*\\)" nil t)
      (let ((file  (match-string 1))
	    (style (match-string 2))
	    (opt   (org-exp-bibtex-options-to-plist (match-string 3))))
	(replace-match
	(cond
	 ((eq org-export-current-backend 'html) ;; We are exporting to HTML
	  (let (extra-args cite-list end-hook tmp-files)
	    (dolist (elt opt)
	      (when (equal "option" (car elt))
		(setq extra-args (cons (cdr elt) extra-args))))

	    (when (assoc "limit" opt) ;; Limit is true - collect references
	      (org-exp-bibtex-docites (lambda ()
					(dolist (c (org-split-string (match-string 1) ","))
					  (add-to-list 'cite-list c))))
;;	      (message "cites: %s" cite-list)
	      (let ((tmp (make-temp-file "org-exp-bibtex")))
		(with-temp-file tmp (dolist (i cite-list) (insert (concat i "\n"))))
		(setq tmp-files   (cons tmp tmp-files))
		(setq extra-args (append extra-args `("-citefile" ,tmp)))))

	    (when (not (eq 0 (apply 'call-process  (append '("bibtex2html" nil nil nil)
							   `("-a" "--nodoc"  "--style" ,style "--no-header")
							   extra-args
							   (list (concat file ".bib"))))))
	      (error "Executing bibtex2html failed"))

	    (dolist (f tmp-files) (delete-file f)))

	  (with-temp-buffer
	    (save-match-data
	      (insert-file-contents (concat file ".html"))
	      (goto-char (point-min))
	      (while (re-search-forward (org-re "a name=\"\\([-_[:word:]]+\\)\">\\([[:word:]]+\\)") nil t)
		(setq oebp-cite-plist (cons (cons (match-string 1) (match-string 2)) oebp-cite-plist)))
	      (goto-char (point-min))
	      (while (re-search-forward "<hr>" nil t)
		(replace-match "<hr/>" t t))
	      (concat "\n#+BEGIN_HTML\n<div id=\"bibliography\">\n<h2>References</h2>\n" (buffer-string) "\n</div>\n#+END_HTML\n"))))
	 ((eq org-export-current-backend 'latex) ;; Latex export
	  (concat "\n#+LATEX: \\bibliographystyle{" style "}"
		  "\n#+LATEX: \\bibliography{" file "}\n"))) t t)))

    ;; Convert cites to links in html
    (when (eq org-export-current-backend 'html)
      ;; Split citation commands with multiple keys
      (org-exp-bibtex-docites
       (lambda ()
	 (let ((keys (save-match-data (org-split-string (match-string 1) ","))))
	   (when (> (length keys) 1)
	     (replace-match (mapconcat (lambda (k) (format "\\cite{%s}" k)) keys "")
			    t t)))))
      ;; Replace the citation commands with links
      (org-exp-bibtex-docites
       (lambda () (let* ((cn (match-string 1))
			 (cv (assoc cn oebp-cite-plist)))
;;		    (message "L: %s" (concat "\[_{}[[" cn "][" (if cv (cdr cv) cn) "]]\]"))
		    (replace-match (concat "\[_{}[[#" cn "][" (if cv (cdr cv) cn) "]]\]")) t t))))))

(defun org-exp-bibtex-docites (fun)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (eq org-export-current-backend 'html)
	(while (re-search-forward "\\\\cite{\\([^}\n]+\\)}" nil t)
	  (apply fun nil))))))

(defun org-exp-bibtex-options-to-plist (options)
  (save-match-data
    (flet ((f (o) (let ((s (split-string o ":"))) (cons (nth 0 s) (nth 1 s)))))
      (mapcar 'f (split-string options nil t)))))

(add-hook 'org-export-preprocess-hook 'org-export-bibtex-preprocess)

(provide 'org-exp-bibtex)

;;; org-exp-bibtex.el ends here
