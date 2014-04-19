;;; ox-confluence --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2012, 2014 Sébastien Delafond

;; Author: Sébastien Delafond <sdelafond at gmx dot net>
;; Keywords: outlines, confluence, wiki

;; This file is not part of GNU Emacs.

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
;; ox-confluence.el lets you convert Org files to confluence files
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence)
;;
;; Export Org files to confluence:
;; M-x org-confluence-export-as-confluence RET
;;
;;; Code:

(require 'ox)
(require 'ox-ascii)

;; Define the backend itself
(org-export-define-derived-backend 'confluence 'ascii
  :translate-alist '((bold . org-confluence-bold)
		     (example-block . org-confluence-example-block)
		     (fixed-width . org-confluence-fixed-width)
		     (footnote-definition . org-confluence-empty)
		     (footnote-reference . org-confluence-empty)
		     (headline . org-confluence-headline)
		     (italic . org-confluence-italic)
		     (link . org-confluence-link)
		     (section . org-confluence-section)
		     (src-block . org-confluence-src-block)
		     (strike-through . org-confluence-strike-through)
		     (table . org-confluence-table)
		     (table-cell . org-confluence-table-cell)
		     (table-row . org-confluence-table-row)
		     (template . org-confluence-template)
		     (underline . org-confluence-underline)))

;; All the functions we use
(defun org-confluence-bold (bold contents info)
  (format "*%s*" contents))

(defun org-confluence-empty (empty contents info)
  "")

(defun org-confluence-example-block (example-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let ((content (org-export-format-code-default example-block info)))
    (org-confluence--block "none" "Confluence" content)))

(defun org-confluence-italic (italic contents info)
  (format "_%s_" contents))

(defun org-confluence-fixed-width (fixed-width contents info)
  (format "\{\{%s\}\}" contents))

(defun org-confluence-headline (headline contents info)
  (let ((low-level-rank (org-export-low-level-p headline info))
        (text (org-export-data (org-element-property :title headline)
                               info))
        (level (org-export-get-relative-level headline info)))
    ;; Else: Standard headline.
    (format "h%s. %s\n%s" level text
            (if (org-string-nw-p contents) contents
              ""))))

(defun org-confluence-link (link desc info)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "["
            (when (org-string-nw-p desc) (format "%s|" desc))
            (cond
             ((string-match "^confluence:" raw-link)
              (replace-regexp-in-string "^confluence:" "" raw-link))
             (t
              raw-link))
            "]")))
(defun org-confluence-section (section contents info)
  contents)

(defun org-confluence-src-block (src-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let* ((lang (org-element-property :language src-block))
         (language (if (string= lang "sh") "bash" ;; FIXME: provide a mapping of some sort
                     lang))
         (content (org-export-format-code-default src-block info)))
    (org-confluence--block language "Emacs" content)))

(defun org-confluence-strike-through (strike-through contents info)
  (format "-%s-" contents))

(defun org-confluence-table (table contents info)
  contents)

(defun org-confluence-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-confluence-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     contents "|")))

(defun org-confluence-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "\{toc\}\n\n") contents)))

(defun org-confluence-underline (underline contents info)
  (format "+%s+" contents))

(defun org-confluence--block (language theme contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          "}\n"
          contents
          "\{code\}\n"))

;; main interactive entrypoint
(defun org-confluence-export-as-confluence
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

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

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*org CONFLUENCE Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-confluence)
