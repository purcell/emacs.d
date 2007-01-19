;;; rails-lib.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-lib.el $
;; $Id: rails-lib.el 60 2007-01-13 20:01:21Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defun rails-lib:run-primary-switch ()
  "Run the primary switch function."
  (interactive)
  (if rails-primary-switch-func
      (apply rails-primary-switch-func nil)))

(defun rails-lib:run-secondary-switch ()
  "Run the secondary switch function."
  (interactive)
  (if rails-secondary-switch-func
      (apply rails-secondary-switch-func nil)))

;;;;; Non Rails realted helper functions ;;;;;

;; Syntax macro

(defmacro* when-bind ((var expr) &rest body)
  "Binds VAR to the result of EXPR.
If EXPR is not nil exeutes BODY.

 (when-bind (var (func foo))
  (do-somth (with var)))."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;; Lists

(defun list->alist (list)
  "Convert (a b c) to ((a . a) (b . b) (c . c))."
  (mapcar #'(lambda (el) (cons el el))
    list))

(defun uniq-list (list)
  "Return a list of unique elements."
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

;; Strings

(defun string-not-empty (str) ;(+)
  "Return t if string STR is not empty."
  (and (stringp str) (not (string-equal str ""))))

(defun yml-value (name)
  "Return the value of the parameter named NAME in the current
buffer or an empty string."
  (if (search-forward-regexp (format "%s:[ ]*\\(.*\\)[ ]*$" name) nil t)
      (match-string 1)
    ""))

(defun current-line-string ()
  "Return the string value of the current line."
  (buffer-substring-no-properties
   (progn (beginning-of-line) (point))
   (progn (end-of-line) (point))))

(defun remove-prefix (word prefix)
  "Remove the PREFIX string in WORD if it exists.
PrefixBla -> Bla."
  (replace-regexp-in-string (format "^%s" prefix) "" word))

(defun remove-postfix (word postfix)
  "Remove the POSTFIX string in WORD if it exists.
BlaPostfix -> Bla."
  (replace-regexp-in-string (format "%s$" postfix) "" word))

(defun strings-join (separator strings)
  "Join all STRINGS using a SEPARATOR."
  (mapconcat 'identity strings separator))

(defun capital-word-p (word)
  "Return t if first letter of WORD is uppercased."
  (and (>= (elt word 0) 65)
       (<= (elt word 0) 90)))

;;;;;;;; def-snips stuff ;;;;

(defun snippet-abbrev-function-name (abbrev-table abbrev-name)
  "Return the name of the snippet abbreviation function in the
ABBREV-TABLE for the abbreviation ABBREV-NAME."
  (intern (concat "snippet-abbrev-"
      (snippet-strip-abbrev-table-suffix
       (symbol-name abbrev-table))
      "-"
      abbrev-name)))

(defun snippet-menu-description-variable (table name)
  "Return a variable for the menu description of the snippet ABBREV-NAME in ABBREV-TABLE."
  (intern
   (concat
    (symbol-name (snippet-abbrev-function-name table name))
    "-menu-description")))

(defmacro* def-snips ((&rest abbrev-tables) &rest snips)
  "Generate snippets with menu documentaion in several ABBREV-TABLES.

  (def-snip (some-mode-abbrev-table other-mode-abbrev-table)
    (\"abbr\"   \"some snip $${foo}\" \"menu documentation\")
    (\"anabr\"   \"other snip $${bar}\" \"menu documentation\")
"
  `(progn
     ,@(loop for table in abbrev-tables
       collect
       `(snippet-with-abbrev-table ',table
    ,@(loop for (name template desc) in snips collect
      `(,name . ,template)))
       append
       (loop for (name template desc) in snips collect
       `(setf ,(snippet-menu-description-variable table name)
        ,desc)))))

(defun snippet-menu-description (abbrev-table name)
  "Return the menu descripton for the snippet named NAME in
ABBREV-TABLE."
  (symbol-value (snippet-menu-description-variable abbrev-table name)))

(defun snippet-menu-line (abbrev-table name)
  "Generate a menu line for the snippet NAME in ABBREV-TABLE."
  (cons
   (concat name "\t" (snippet-menu-description abbrev-table name))
   (lexical-let ((func-name (snippet-abbrev-function-name abbrev-table name)))
     (lambda () (interactive) (funcall func-name)))))

;;; Define keys

(defmacro define-keys (key-map &rest key-funcs)
  "Define key bindings for KEY-MAP (create KEY-MAP, if it does
not exist."
  `(progn
     (unless (boundp ',key-map)
       (setf ,key-map (make-keymap)))
     ,@(mapcar
  #'(lambda (key-func)
      `(define-key ,key-map ,(first key-func) ,(second key-func)))
  key-funcs)))

;; Files

(defun append-string-to-file (file string)
  "Append a string to end of a file."
  (write-region string nil file t))

(defun write-string-to-file (file string)
  "Write a string to a file (erasing the previous content)."
  (write-region string nil file))

(defun read-from-file (file-name)
  "Read sexpr from a file named FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (read (current-buffer))))

;; File hierarchy functions

(defun find-recursive-files (file-regexp directory)
  "Return a list of files, found in DIRECTORY and match them to FILE-REGEXP."
  (find-recursive-filter-out
   find-recursive-exclude-files
   (find-recursive-directory-relative-files directory "" file-regexp)))

(defun directory-name (path)
  "Return the name of a directory with a given path.
For example, (path \"/foo/bar/baz/../\") returns bar."
  ;; Rewrite me
  (let ((old-path default-directory))
    (cd path)
    (let ((dir (pwd)))
      (cd old-path)
      (replace-regexp-in-string "^Directory[ ]*" "" dir))))

(defun find-or-ask-to-create (question file)
  "Open file if it exists. If it does not exist, ask to create
it."
    (if (file-exists-p file)
  (find-file file)
      (when (y-or-n-p question)
  (when (string-match "\\(.*\\)/[^/]+$" file)
    (make-directory (match-string 1 file) t))
  (find-file file))))

(defun directory-of-file (file-name)
  "Return the parent directory of a file named FILE-NAME."
  (replace-regexp-in-string "[^/]*$" "" file-name))

;; Buffers

(defun buffer-string-by-name (buffer-name)
  "Return the content of buffer named BUFFER-NAME as a string."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)
    (buffer-string)))

;; Misc

(defun rails-browse-api-url (url)
  "Browse preferentially with Emacs w3m browser."
  (if rails-browse-api-with-w3m
      (w3m-find-file (remove-prefix url "file://"))
    (rails-alternative-browse-url url)))

(defun rails-alternative-browse-url (url &rest args)
  "Fix a problem with Internet Explorer not being able to load
URLs with anchors via ShellExecute. It will only be invoked it
the user explicit sets `rails-use-alternative-browse-url'."
  (if (and (eq system-type 'windows-nt) rails-use-alternative-browse-url)
      (w32-shell-execute "open" "iexplore" url)
    (browse-url url args)))

(provide 'rails-lib)