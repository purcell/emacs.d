;;; ox-bibtex.el --- Export bibtex fragments

;; Copyright (C) 2009-2014 Taru Karttunen

;; Author: Taru Karttunen <taruti@taruti.net>
;;      Nicolas Goaziou <n dot goaziou at gmail dot com>
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
;; exports.  It uses the bibtex2html software from:
;;
;;   http://www.lri.fr/~filliatr/bibtex2html/
;;
;; It also introduces "cite" syntax for Org links.
;;
;; The usage is as follows:
;;
;;   #+BIBLIOGRAPHY: bibfilebasename stylename optional-options
;;
;; e.g. given foo.bib and using style plain:
;;
;;   #+BIBLIOGRAPHY: foo plain option:-d
;;
;; Optional options are of the form:
;;
;;   option:-foobar pass '-foobar' to bibtex2html
;;
;; e.g.,
;;
;;   option:-d    sort by date
;;   option:-a    sort as BibTeX (usually by author) *default*
;;   option:-u    unsorted i.e. same order as in .bib file
;;   option:-r    reverse the sort
;;
;; See the bibtex2html man page for more.  Multiple options can be
;; combined like:
;;
;;   option:-d option:-r
;;
;; Limiting to only the entries cited in the document:
;;
;;   limit:t
;;
;; For LaTeX export this simply inserts the lines
;;
;;   \bibliographystyle{plain}
;;   \bibliography{foo}
;;
;; into the TeX file when exporting.
;;
;; For HTML export it:
;; 1) converts all \cite{foo} and [[cite:foo]] to links to the
;;    bibliography,
;; 2) creates a foo.html and foo_bib.html,
;; 3) includes the contents of foo.html in the exported HTML file.
;;
;; For LaTeX export it:
;; 1) converts all [[cite:foo]] to \cite{foo}.

;; Initialization

(eval-when-compile (require 'cl))
(org-add-link-type "cite" 'ebib)


;;; Internal Functions

(defun org-bibtex-get-file (keyword)
  "Return bibliography file as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no file is found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (match-string 1 value))))

(defun org-bibtex-get-style (keyword)
  "Return bibliography style as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no style is found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (match-string 2 value))))

(defun org-bibtex-get-arguments (keyword)
  "Return \"bibtex2html\" arguments specified by the user.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. Return value is a plist
containing `:options' and `:limit' properties. The former
contains a list of strings to be passed as options ot
\"bibtex2html\" process. The latter contains a boolean."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (let (options limit)
           (dolist (arg (org-split-string (match-string 3 value))
                        ;; Return value.
                        (list :options (nreverse options) :limit limit))
             (let* ((s (split-string arg ":"))
                    (key (car s))
                    (value (nth 1 s)))
               (cond ((equal "limit" key)
                      (setq limit (not (equal "nil" value))))
                     ((equal "option" key) (push value options)))))))))

(defun org-bibtex-citation-p (object)
  "Non-nil when OBJECT is a citation."
  (case (org-element-type object)
    (link (equal (org-element-property :type object) "cite"))
    (latex-fragment
     (string-match "\\`\\\\cite{" (org-element-property :value object)))))

(defun org-bibtex-get-citation-key (citation)
  "Return key for a given citation, as a string.
CITATION is a `latex-fragment' or `link' type object satisfying
to `org-bibtex-citation-p' predicate."
  (if (eq (org-element-type citation) 'link)
      (org-element-property :path citation)
    (let ((value (org-element-property :value citation)))
      (and (string-match "\\`\\\\cite{" value)
	   (substring value (match-end 0) -1)))))



;;; LaTeX Part

(defadvice org-latex-keyword (around bibtex-keyword)
  "Translate \"BIBLIOGRAPHY\" keywords into LaTeX syntax.
Fallback to `latex' back-end for other keywords."
  (let ((keyword (ad-get-arg 0)))
    (if (not (equal (org-element-property :key keyword) "BIBLIOGRAPHY"))
        ad-do-it
      (let ((file (org-bibtex-get-file keyword))
            (style (org-bibtex-get-style keyword)))
        (setq ad-return-value
              (when file
                (concat (and style (format "\\bibliographystyle{%s}\n" style))
                        (format "\\bibliography{%s}" file))))))))

(defadvice org-latex-link (around bibtex-link)
  "Translate \"cite\" type links into LaTeX syntax.
Fallback to `latex' back-end for other keywords."
  (let ((link (ad-get-arg 0)))
    (if (not (org-bibtex-citation-p link)) ad-do-it
      (setq ad-return-value
	    (format "\\cite{%s}" (org-bibtex-get-citation-key link))))))

(ad-activate 'org-latex-keyword)
(ad-activate 'org-latex-link)



;;; HTML Part

(defvar org-bibtex-html-entries-alist nil)  ; Dynamically scoped.
(defvar org-bibtex-html-keywords-alist nil) ; Dynamically scoped.


;;;; Advices

(defadvice org-html-keyword (around bibtex-keyword)
  "Translate \"BIBLIOGRAPHY\" keywords into HTML syntax.
Fallback to `html' back-end for other keywords."
  (let ((keyword (ad-get-arg 0)))
    (if (not (equal (org-element-property :key keyword) "BIBLIOGRAPHY"))
        ad-do-it
      (setq ad-return-value
            (cdr (assq keyword org-bibtex-html-keywords-alist))))))

(defadvice org-html-latex-fragment (around bibtex-citation)
  "Translate \"\\cite\" LaTeX fragments into HTML syntax.
Fallback to `html' back-end for other keywords."
  (let ((fragment (ad-get-arg 0)))
    (if (not (org-bibtex-citation-p fragment)) ad-do-it
      (setq ad-return-value
            (mapconcat
             (lambda (key)
               (let ((key (org-trim key)))
                 (format "[<a href=\"#%s\">%s</a>]"
                         key
                         (or (cdr (assoc key org-bibtex-html-entries-alist))
                             key))))
             (org-split-string (org-bibtex-get-citation-key fragment) ",")
             "")))))

(defadvice org-html-link (around bibtex-link)
  "Translate \"cite:\" type links into HTML syntax.
Fallback to `html' back-end for other types."
  (let ((link (ad-get-arg 0)))
    (if (not (org-bibtex-citation-p link)) ad-do-it
      (setq ad-return-value
	    (mapconcat
	     (lambda (key)
	       (format "[<a href=\"#%s\">%s</a>]"
		       key
		       (or (cdr (assoc key org-bibtex-html-entries-alist))
			   key)))
	     (org-split-string (org-bibtex-get-citation-key link)
			       "[ \t]*,[ \t]*")
	     "")))))

(ad-activate 'org-html-keyword)
(ad-activate 'org-html-latex-fragment)
(ad-activate 'org-html-link)


;;;; Filter

(defun org-bibtex-process-bib-files (tree backend info)
  "Send each bibliography in parse tree to \"bibtex2html\" process.
Return new parse tree.  This function assumes current back-end is HTML."
  ;; Initialize dynamically scoped variables.  The first one
  ;; contain an alist between keyword objects and their HTML
  ;; translation.  The second one will contain an alist between
  ;; citation keys and names in the output (according to style).
  (setq org-bibtex-html-entries-alist nil
        org-bibtex-html-keywords-alist nil)
  (org-element-map tree 'keyword
    (lambda (keyword)
      (when (equal (org-element-property :key keyword) "BIBLIOGRAPHY")
        (let ((arguments (org-bibtex-get-arguments keyword))
              (file (org-bibtex-get-file keyword))
              temp-file)
          ;; limit is set: collect citations throughout the document
          ;; in TEMP-FILE and pass it to "bibtex2html" as "-citefile"
          ;; argument.
          (when (plist-get arguments :limit)
            (let ((citations
                   (org-element-map tree '(latex-fragment link)
                     (lambda (object)
                       (and (org-bibtex-citation-p object)
			    (org-bibtex-get-citation-key object))))))
              (with-temp-file (setq temp-file (make-temp-file "ox-bibtex"))
                (insert (mapconcat 'identity citations "\n")))
              (setq arguments
                    (plist-put arguments
                               :options
                               (append (plist-get arguments :options)
                                       (list "-citefile" temp-file))))))
          ;; Call "bibtex2html" on specified file.
          (unless (eq 0 (apply 'call-process
                               (append '("bibtex2html" nil nil nil)
                                       '("-a" "-nodoc" "-noheader" "-nofooter")
                                       (list "--style"
                                             (org-bibtex-get-style keyword))
                                       (plist-get arguments :options)
                                       (list (concat file ".bib")))))
            (error "Executing bibtex2html failed"))
          (and temp-file (delete-file temp-file))
          ;; Open produced HTML file, wrap references within a block and
          ;; return it.
          (with-temp-buffer
            (insert "<div id=\"bibliography\">\n<h2>References</h2>\n")
            (insert-file-contents (concat file ".html"))
            (insert "\n</div>")
            ;; Update `org-bibtex-html-keywords-alist'.
            (push (cons keyword (buffer-string))
                  org-bibtex-html-keywords-alist)
            ;; Update `org-bibtex-html-entries-alist'.
            (goto-char (point-min))
            (while (re-search-forward
                    "a name=\"\\([-_a-zA-Z0-9:]+\\)\">\\(\\w+\\)" nil t)
              (push (cons (match-string 1) (match-string 2))
                    org-bibtex-html-entries-alist)))))))
  ;; Return parse tree unchanged.
  tree)

(eval-after-load 'ox
  '(add-to-list 'org-export-filter-parse-tree-functions
                'org-bibtex-process-bib-files))



(provide 'ox-bibtex)

;;; ox-bibtex.el ends here
