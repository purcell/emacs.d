;;; xref-js2.el --- Jump to references/definitions using ag & js2-mode's AST -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; URL: https://github.com/NicolasPetton/xref-js2
;; Package-Version: 20170530.126
;; Keywords: javascript, convenience, tools
;; Version: 1.0
;; Package: xref-js2
;; Package-Requires: ((emacs "25") (js2-mode "20150909"))

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
;;
;; xref-js2 adds an xref backend for JavaScript files.
;;
;; Instead of using a tag system, it relies on `ag' to query the codebase of a
;; project.  This might sound crazy at first, but it turns out that `ag' is so
;; fast that jumping using xref-js2 is most of the time instantaneous, even on
;; fairly large JavaScript codebase (it successfully works with 50k lines of JS
;; code).
;;
;; Because line by line regexp search has its downside, xref-js2 does a second
;; pass on result candidates and eliminates possible false positives using
;; `js2-mode''s AST, thus giving very accurate results.

;;; Code:

(require 'subr-x)
(require 'xref)
(require 'seq)
(require 'map)
(require 'js2-mode)
(require 'vc)

(defcustom xref-js2-ag-arguments '("--js" "--noheading" "--nocolor")
  "Default arguments passed to ag."
  :type 'list
  :group 'xref-js2)

(defcustom xref-js2-ignored-dirs '("bower_components"
                                   "node_modules"
                                   "build"
                                   "lib")
  "List of directories to be ignored when performing a search."
  :type 'list
  :group 'xref-js2)

(defcustom xref-js2-ignored-files '("*.min.js")
  "List of files to be ignored when performing a search."
  :type 'list
  :group 'xref-js2)


(defcustom xref-js2-definitions-regexps '("\\b%s\\b[\\s]*[:=][^=]"
                                          "function[\\s]+\\b%s\\b"
                                          "class[\\s]+\\b%s\\b"
                                          "(?<!new)[^.]%s[\\s]*\\(")
  "List of regular expressions that match definitions of a symbol.
In each regexp string, '%s' is expanded with the searched symbol."
  :type 'list
  :group 'xref-js2)

(defcustom xref-js2-references-regexps '("\\b%s\\b(?!\\s*[:=][^=])")
  "List of regular expressions that match references to a symbol.
In each regexp string, '%s' is expanded with the searched symbol."
  :type 'list
  :group 'xref-js2)

;;;###autoload
(defun xref-js2-xref-backend ()
  "Xref-Js2 backend for Xref."
  'xref-js2)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-js2)))
  (symbol-name (symbol-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-js2)) symbol)
  (xref-js2--xref-find-definitions symbol))

(cl-defmethod xref-backend-references ((_backend (eql xref-js2)) symbol)
  (xref-js2--xref-find-references symbol))

(defun xref-js2--xref-find-definitions (symbol)
  "Return a list of candidates matching SYMBOL."
  (seq-map (lambda (candidate)
             (xref-js2--make-xref candidate))
           (xref-js2--find-definitions symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-js2)))
  "Return a list of terms for completions taken from the symbols in the current buffer.

The current implementation returns all the words in the buffer,
which is really sub optimal."
  (let (words)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "\\w+" nil t)
          (add-to-list 'words (match-string-no-properties 0)))
        (seq-uniq words)))))

(defun xref-js2--xref-find-references (symbol)
  "Return a list of reference candidates matching SYMBOL."
  (seq-map (lambda (candidate)
             (xref-js2--make-xref candidate))
           (xref-js2--find-references symbol)))

(defun xref-js2--make-xref (candidate)
  "Return a new Xref object built from CANDIDATE."
  (xref-make (map-elt candidate 'match)
             (xref-make-file-location (map-elt candidate 'file)
                                      (map-elt candidate 'line)
                                      0)))

(defun xref-js2--find-definitions (symbol)
  "Return a list of definitions for SYMBOL from an ag search."
  (xref-js2--find-candidates
   symbol
   (xref-js2--make-regexp symbol xref-js2-definitions-regexps)))

(defun xref-js2--find-references (symbol)
  "Return a list of references for SYMBOL from an ag search."
  (xref-js2--find-candidates
   symbol
   (xref-js2--make-regexp symbol xref-js2-references-regexps)))

(defun xref-js2--make-regexp (symbol regexps)
  "Return a regular expression to search for SYMBOL using REGEXPS.

REGEXPS must be a list of regular expressions, which are
concatenated together into one regexp, expanding occurrences of
'%s' with SYMBOL."
  (mapconcat #'identity
             (mapcar (lambda (str)
                       (format str symbol))
                     regexps) "|"))

(defun xref-js2--find-candidates (symbol regexp)
  (let ((default-directory (xref-js2--root-dir))
        matches)
    (with-temp-buffer
      (apply #'process-file (executable-find "ag") nil t nil
             `(,@xref-js2-ag-arguments
               ,@(seq-mapcat (lambda (dir)
                               (list "--ignore-dir" dir))
                             xref-js2-ignored-dirs)
               ,@(seq-mapcat (lambda (file)
                               (list "--ignore" file))
                             xref-js2-ignored-files)
               ,regexp))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string-no-properties 1) matches)))
    (seq-remove #'xref-js2--false-positive
                (seq-map (lambda (match)
                           (xref-js2--candidate symbol match))
                         matches))))

(defun xref-js2--false-positive (candidate)
  "Return non-nil if CANDIDATE is a false positive.
Filtering is done using the AST from js2-mode."
  (let* ((file (map-elt candidate 'file))
         (buffer-open (get-file-buffer file)))
    (prog1
        (with-current-buffer (find-file-noselect file t)
          (save-excursion
            (save-restriction
              (widen)
              (unless (or (eq major-mode 'js2-mode)
                          (seq-contains (map-keys minor-mode-alist) 'js2-minor-mode))
                (js2-minor-mode 1))
              (goto-char (point-min))
              (forward-line (1- (map-elt candidate 'line)))
              (search-forward (map-elt candidate 'symbol) nil t)
              ;; js2-mode fails to parse the AST for some minified files
              (ignore-errors
                (let ((node (js2-node-at-point)))
                  (or (js2-string-node-p node)
                      (js2-comment-node-p node))))))))))

(defun xref-js2--root-dir ()
  "Return the root directory of the project."
  (or (ignore-errors
        (projectile-project-root))
      (ignore-errors
        (vc-root-dir))
      (user-error "You are not in a project")))


(defun xref-js2--candidate (symbol match)
  "Return a candidate alist built from SYMBOL and a raw MATCH result.
The MATCH is one output result from the ag search."
  (let* ((attrs (split-string match ":" t))
         (match (string-trim (mapconcat #'identity (cddr attrs) ":"))))
    ;; Some minified JS files might match a search. To avoid cluttering the
    ;; search result, we trim the output.
    (when (> (seq-length match) 100)
      (setq match (concat (seq-take match 100) "...")))
    (list (cons 'file (expand-file-name (car attrs) (xref-js2--root-dir)))
          (cons 'line (string-to-number (cadr attrs)))
          (cons 'symbol symbol)
          (cons 'match match))))

(provide 'xref-js2)
;;; xref-js2.el ends here
