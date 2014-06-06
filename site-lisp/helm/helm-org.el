;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-plugin)
(require 'org)

(declare-function org-get-current-options "ext:org-exp.el")

;;; Org headlines
;;
;;
(defvar helm-source-org-headline
  `((name . "Org Headline")
    (headline
     ,@(mapcar
        (lambda (num)
          (format "^\\*\\{%d\\} \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
                  num))
        (number-sequence 1 8)))
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)
    (persistent-action . (lambda (elm)
                           (helm-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to line" . helm-action-line-goto)
           ("Refile to this headline" . helm-org-headline-refile)
           ("Insert link to this headline"
            . helm-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun helm-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (helm-goto-line (car lineno-and-content))
     (and (looking-at org-complex-heading-regexp)
          (org-make-link-string (concat "*" (match-string 4)))))))

(defun helm-org-headline-refile (lineno-and-content)
  "Refile current org entry to LINENO-AND-CONTENT."
  (with-helm-current-buffer
    (org-cut-subtree)
    (helm-goto-line (car lineno-and-content))
    (org-end-of-subtree t t)
    (let ((org-yank-adjusted-subtrees t))
      (org-yank))))


;;; Org keywords
;;
;;  Stop supporting this in emacs-24.3.50.1.
(when (boundp 'org-additional-option-like-keywords)
  (defvar helm-source-org-keywords
    '((name . "Org Keywords")
      (init . helm-org-keywords-init)
      (candidates . helm-org-keywords-candidates)
      (action . helm-org-keywords-insert)
      (persistent-action . helm-org-keywords-show-help)
      (persistent-help . "Show an example and info page to describe this keyword.")
      (keywords-examples)
      (keywords)))

  (defvar helm-org-keywords-info-location
    '(("#+TITLE:" . "(org)Export options")
      ("#+AUTHOR:" . "(org)Export options")
      ("#+DATE:" . "(org)Export options")
      ("#+EMAIL:" . "(org)Export options")
      ("#+DESCRIPTION:" . "(org)Export options")
      ("#+KEYWORDS:" . "(org)Export options")
      ("#+LANGUAGE:" . "(org)Export options")
      ("#+TEXT:" . "(org)Export options")
      ("#+TEXT:" . "(org)Export options")
      ("#+OPTIONS:" . "(org)Export options")
      ("#+BIND:" . "(org)Export options")
      ("#+LINK_UP:" . "(org)Export options")
      ("#+LINK_HOME:" . "(org)Export options")
      ("#+LATEX_HEADER:" . "(org)Export options")
      ("#+EXPORT_SELECT_TAGS:" . "(org)Export options")
      ("#+EXPORT_EXCLUDE_TAGS:" . "(org)Export options")
      ("#+INFOJS_OPT" . "(org)Javascript support")
      ("#+BEGIN_HTML" . "(org)Quoting HTML tags")
      ("#+BEGIN_LaTeX" . "(org)Quoting LaTeX code")
      ("#+ORGTBL" . "(org)Radio tables")
      ("#+HTML:" . "(org)Quoting HTML tags")
      ("#+LaTeX:" . "(org)Quoting LaTeX code")
      ("#+BEGIN:" . "(org)Dynamic blocks") ;clocktable columnview
      ("#+BEGIN_EXAMPLE" . "(org)Literal examples")
      ("#+BEGIN_QUOTE" . "(org)Paragraphs")
      ("#+BEGIN_VERSE" . "(org)Paragraphs")
      ("#+BEGIN_SRC" . "(org)Literal examples")
      ("#+CAPTION" . "(org)Tables in HTML export")
      ("#+LABEL" . "(org)Tables in LaTeX export")
      ("#+ATTR_HTML" . "(org)Links")
      ("#+ATTR_LaTeX" . "(org)Images in LaTeX export")))

  (defun helm-org-keywords-init ()
    (unless (helm-attr 'keywords-examples)
      (require 'org)
      (helm-attrset 'keywords-examples
                    (append
                     (mapcar
                      (lambda (x)
                        (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                        (cons (match-string 2 x) (match-string 1 x)))
                      (org-split-string (org-get-current-options) "\n"))
                     (mapcar 'list org-additional-option-like-keywords)))
      (helm-attrset 'keywords (mapcar 'car (helm-attr 'keywords-examples)))))

  (defun helm-org-keywords-candidates ()
    (and (or (eq (buffer-local-value 'major-mode helm-current-buffer) 'org-mode)
             (eq (buffer-local-value 'major-mode helm-current-buffer) 'message-mode))
         (helm-attr 'keywords)))

  (defun helm-org-keywords-insert (keyword)
    (cond ((and (string-match "BEGIN" keyword)
                (helm-region-active-p))
           (let ((beg (region-beginning))
                 (end (region-end)))
             (goto-char end)
             (insert "\n#+" (replace-regexp-in-string
                             "BEGIN" "END" keyword) "\n")
             (goto-char beg)
             (insert "#+" keyword " ")
             (save-excursion (insert "\n"))))
          ((string-match "BEGIN" keyword)
           (insert "#+" keyword " ")
           (save-excursion
             (insert "\n#+" (replace-regexp-in-string
                             "BEGIN" "END" keyword) "\n")))
          (t (insert "#+" keyword " "))))

  (defun helm-org-keywords-show-help (keyword)
    (info (or (assoc-default (concat "#+" keyword) helm-org-keywords-info-location)
              "(org)In-buffer settings"))
    (search-forward (concat "#+" keyword) nil t)
    (helm-highlight-current-line)
    (message "%s" (or (cdr (assoc keyword (helm-attr 'keywords-examples))) ""))))

;;;###autoload
(defun helm-org-keywords ()
  "Preconfigured `helm' for org keywords."
  (interactive)
  (cl-assert (boundp 'org-additional-option-like-keywords) nil
             "Helm-org-keyword not supported in %s" emacs-version)
  (helm-other-buffer 'helm-source-org-keywords "*org keywords*"))

;;;###autoload
(defun helm-org-headlines ()
  "Preconfigured helm to show org headlines."
  (interactive)
  (helm-other-buffer 'helm-source-org-headline "*org headlines*"))


(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
