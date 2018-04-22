;;; cider-apropos.el --- Apropos functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2014-2017 Jeff Valk, Bozhidar Batsov and CIDER contributors
;;
;; Author: Jeff Valk <jv@jeffvalk.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Apropos functionality for Clojure.

;;; Code:

(require 'cider-doc)
(require 'cider-util)
(require 'subr-x)
(require 'cider-compat)

(require 'cider-client)
(require 'cider-popup)
(require 'nrepl-dict)

(require 'clojure-mode)
(require 'apropos)
(require 'button)

(defconst cider-apropos-buffer "*cider-apropos*")
(add-to-list 'cider-ancillary-buffers cider-apropos-buffer)

(defcustom cider-apropos-actions '(("display-doc" . cider-doc-lookup)
                                   ("find-def" . cider--find-var)
                                   ("lookup-on-grimoire" . cider-grimoire-lookup))
  "Controls the actions to be applied on the symbol found by an apropos search.
The first action key in the list will be selected as default.  If the list
contains only one action key, the associated action function will be
applied automatically.  An action function can be any function that receives
the symbol found by the apropos search as argument."
  :type '(alist :key-type string :value-type function)
  :group 'cider
  :package-version '(cider . "0.13.0"))

(define-button-type 'apropos-special-form
  'apropos-label "Special form"
  'apropos-short-label "s"
  'face 'font-lock-keyword-face
  'help-echo "mouse-2, RET: Display more help on this special form"
  'follow-link t
  'action (lambda (button)
            (describe-function (button-get button 'apropos-symbol))))

(defun cider-apropos-doc (button)
  "Display documentation for the symbol represented at BUTTON."
  (cider-doc-lookup (button-get button 'apropos-symbol)))

(defun cider-apropos-summary (query ns docs-p include-private-p case-sensitive-p)
  "Return a short description for the performed apropos search.

QUERY can be a regular expression list of space-separated words
\(e.g take while) which will be converted to a regular expression
\(like take.+while) automatically behind the scenes.  The search may be
limited to the namespace NS, and may optionally search doc strings
\(based on DOCS-P), include private vars (based on INCLUDE-PRIVATE-P),
and be case-sensitive (based on CASE-SENSITIVE-P)."
  (concat (if case-sensitive-p "Case-sensitive " "")
          (if docs-p "Documentation " "")
          (format "Apropos for %S" query)
          (if ns (format " in namespace %S" ns) "")
          (if include-private-p
              " (public and private symbols)"
            " (public symbols only)")))

(defun cider-apropos-highlight (doc query)
  "Return the DOC string propertized to highlight QUERY matches."
  (let ((pos 0))
    (while (string-match query doc pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'font-lock-face apropos-match-face doc)))
  doc)

(defun cider-apropos-result (result query docs-p)
  "Emit a RESULT matching QUERY into current buffer, formatted for DOCS-P."
  (nrepl-dbind-response result (name type doc)
    (let* ((label (capitalize (if (string= type "variable") "var" type)))
           (help (concat "Display doc for this " (downcase label))))
      (cider-propertize-region (list 'apropos-symbol name
                                     'action 'cider-apropos-doc
                                     'help-echo help)
        (insert-text-button name 'type 'apropos-symbol)
        (insert "\n  ")
        (insert-text-button label 'type (intern (concat "apropos-" type)))
        (insert ": ")
        (let ((beg (point)))
          (if docs-p
              (insert (cider-apropos-highlight doc query) "\n")
            (insert doc)
            (fill-region beg (point))))
        (insert "\n")))))

(declare-function cider-mode "cider-mode")

(defun cider-show-apropos (summary results query docs-p)
  "Show SUMMARY and RESULTS for QUERY in a pop-up buffer, formatted for DOCS-P."
  (with-current-buffer (cider-popup-buffer cider-apropos-buffer t)
    (let ((inhibit-read-only t))
      (apropos-mode)
      (if (boundp 'header-line-format)
          (setq-local header-line-format summary)
        (insert summary "\n\n"))
      (dolist (result results)
        (cider-apropos-result result query docs-p))
      (goto-char (point-min)))))

;;;###autoload
(defun cider-apropos (query &optional ns docs-p privates-p case-sensitive-p)
  "Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P)."
  (interactive
   (cons (read-string "Search for Clojure symbol (a regular expression): ")
         (when current-prefix-arg
           (list (let ((ns (completing-read "Namespace (default is all): " (cider-sync-request:ns-list))))
                   (if (string= ns "") nil ns))
                 (y-or-n-p "Search doc strings? ")
                 (y-or-n-p "Include private symbols? ")
                 (y-or-n-p "Case-sensitive? ")))))
  (cider-ensure-connected)
  (cider-ensure-op-supported "apropos")
  (if-let ((summary (cider-apropos-summary
                     query ns docs-p privates-p case-sensitive-p))
           (results (cider-sync-request:apropos query ns docs-p privates-p case-sensitive-p)))
      (cider-show-apropos summary results query docs-p)
    (message "No apropos matches for %S" query)))

;;;###autoload
(defun cider-apropos-documentation ()
  "Shortcut for (cider-apropos <query> nil t)."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "apropos")
  (cider-apropos (read-string "Search for Clojure documentation (a regular expression): ") nil t))

(defun cider-apropos-act-on-symbol (symbol)
  "Apply selected action on SYMBOL."
  (let* ((first-action-key (car (car cider-apropos-actions)))
         (action-key (if (= 1 (length cider-apropos-actions))
                         first-action-key
                       (completing-read (format "Choose action to apply to `%s` (default %s): "
                                                symbol first-action-key)
                                        cider-apropos-actions nil nil nil nil first-action-key)))
         (action-fn (cdr (assoc action-key cider-apropos-actions))))
    (if action-fn
        (funcall action-fn symbol)
      (user-error "Unknown action `%s`" action-key))))

;;;###autoload
(defun cider-apropos-select (query &optional ns docs-p privates-p case-sensitive-p)
  "Similar to `cider-apropos', but presents the results in a completing read.

Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P)."
  (interactive
   (cons (read-string "Search for Clojure symbol (a regular expression): ")
         (when current-prefix-arg
           (list (let ((ns (completing-read "Namespace (default is all): " (cider-sync-request:ns-list))))
                   (if (string= ns "") nil ns))
                 (y-or-n-p "Search doc strings? ")
                 (y-or-n-p "Include private symbols? ")
                 (y-or-n-p "Case-sensitive? ")))))
  (cider-ensure-connected)
  (cider-ensure-op-supported "apropos")
  (if-let ((summary (cider-apropos-summary
                     query ns docs-p privates-p case-sensitive-p))
           (results (mapcar (lambda (r) (nrepl-dict-get r "name"))
                            (cider-sync-request:apropos query ns docs-p privates-p case-sensitive-p))))
      (cider-apropos-act-on-symbol (completing-read (concat summary ": ") results))
    (message "No apropos matches for %S" query)))

;;;###autoload
(defun cider-apropos-documentation-select ()
  "Shortcut for (cider-apropos-select <query> nil t)."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "apropos")
  (cider-apropos-select (read-string "Search for Clojure documentation (a regular expression): ") nil t))

(provide 'cider-apropos)

;;; cider-apropos.el ends here
