;;; js-doc.el --- Insert JsDoc style comment easily

;; Author: mooz <stillpedant@gmail.com>
;; Version: 0.0.4
;; Keywords: document, comment
;; X-URL: http://www.d.hatena.ne.jp/mooz/

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Installation:
;; put `js-doc.el' somewhere in your emacs load path
;; add a line below to your .emacs file
;; (require 'js-doc)

;; Example:
;; paste the codes below into your .emacs.el file and you can
;;
;; 1. insert function document by pressing Ctrl + c, i
;; 2. insert @tag easily by pressing @ in the JsDoc style comment
;;
;; (setq js-doc-mail-address "your email address"
;;       js-doc-author (format "your name <%s>" js-doc-mail-address)
;;       js-doc-url "url of your website"
;;       js-doc-license "license name")
;;
;; (add-hook 'js2-mode-hook
;;           #'(lambda ()
;;               (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
;;               (define-key js2-mode-map "@" 'js-doc-insert-tag)))
;;
;; If you want to see the tag description, just input the next command
;;   M-x js-doc-describe-tag

;;; Custom:
(defgroup js-doc nil
  "Insert JsDoc style comment easily."
  :group 'comment
  :prefix "js-doc")

;; Variables
(defcustom js-doc-mail-address ""
  "Author's E-mail address."
  :group 'js-doc)

(defcustom js-doc-author ""
  "Author of the source code."
  :group 'js-doc)

(defcustom js-doc-license ""
  "License of the source code."
  :group 'js-doc)

(defcustom js-doc-url ""
  "Author's Home page URL."
  :group 'js-doc)

;; from jsdoc-toolkit wiki
;; http://code.google.com/p/jsdoc-toolkit/wiki/TagReference
(defvar js-doc-all-tag-alist
  '(("augments" . "Indicate this class uses another class as its \"base.\"")
    ("author" . "Indicate the author of the code being documented.")
    ("argument" . "Deprecated synonym for @param.")
    ("borrows that as this" . "Document that class's member as if it were a member of this class.")
    ("class" . "Provide a description of the class (versus the constructor).")
    ("constant" . "Indicate that a variable's value is a constant.")
    ("constructor" . "Identify a function is a constructor.")
    ("constructs" . "Identicate that a lent function will be used as a constructor.")
    ("default" . "Describe the default value of a variable.")
    ("deprecated" . "Indicate use of a variable is no longer supported.")
    ("description" . "Provide a description (synonym for an untagged first-line).")
    ("event" . "Describe an event handled by a class.")
    ("example" . "Provide a small code example, illustrating usage.")
    ("extends" . "Synonym for @augments.")
    ("field" . "Indicate that the variable refers to a non-function.")
    ("fileOverview" . "Provides information about the entire file.")
    ("function" . "Indicate that the variable refers to a function.")
    ("ignore" . "Indicate JsDoc Toolkit should ignore the variable.")
    ("inner" . "Indicate that the variable refers to an inner function (and so is also @private).")
    ("lends" . "Document that all an object literal's members are members of a given class.")
    ("license" . "License of the source code.")
    ("link" . "Like @see but can be used within the text of other tags.")
    ("memberOf" . "Document that this variable refers to a member of a given class.")
    ("name" . "Force JsDoc Toolkit to ignore the surrounding code and use the given variable name instead.")
    ("namespace" . "Document an object literal is being used as a \"namespace.\"")
    ("param" . "Describe a function's parameter.")
    ("private" . "Indicate a variable is private (use the -p command line option to include these).")
    ("property" . "Document a property of a class from within the constructor's doclet.")
    ("public" . "Indicate an inner variable is public.")
    ("requires" . "Describe a required resource.")
    ("returns" . "Describe the return value of a function.")
    ("see" . "Describe a related resource.")
    ("since" . "Indicate that a feature has only been available on and after a certain version number.")
    ("static" . "Indicate that accessing the variable does not require instantiation of its parent.")
    ("throws" . "Describe the exception that a function might throw.")
    ("type" . "Describe the expected type of a variable's value or the value returned by a function.")
    ("version" . "Indicate the release version of this code."))
  "JsDoc tag list
This list contains tag name and its description")

(defvar js-doc-file-doc-lines
  '(js-doc-top-line
    " * @fileOverview\n"
    " * @name %F\n"
    " * @author %a\n"
    " * @license %l\n"
    js-doc-bottom-line)
  "JsDoc style file document format.
When the `js-doc-insert-file-doc' is called,
each lines in a list will be formatted by `js-doc-format-string'
and inserted to the top of current buffer.")

(defvar js-doc-format-string-alist
  '(("%F" . (buffer-name))
    ("%P" . (buffer-file-name))
    ("%a" . js-doc-author)
    ("%l" . js-doc-license)
    ("%d" . (current-time-string))
    ("%p" . js-doc-current-parameter-name)
    ("%f" . js-doc-current-function-name))
  "Format and value pair
Format will be replaced its value in `js-doc-format-string'")

;;; Lines

;; %F => file name
;; %P => file path
;; %a => author name
;; %d => current date
;; %p => parameter name
;; %f => function name

(defcustom js-doc-top-line "/**\n"
  "top line of the js-doc style comment."
  :group 'js-doc)

(defcustom js-doc-description-line" * \n"
  "description line."
  :group 'js-doc)

(defcustom js-doc-bottom-line " */\n"
  "bottom line."
  :group 'js-doc)

;; formats for function-doc

(defcustom js-doc-parameter-line " * @param {} %p\n"
  "parameter line.
 %p will be replaced with the parameter name."
  :group 'js-doc)

(defcustom js-doc-return-line " * @returns {} \n"
  "return line."
  :group 'js-doc)

(defcustom js-doc-throw-line " * @throws {} \n"
  "bottom line."
  :group 'js-doc)

;; ========== Regular expresisons ==========

(defcustom js-doc-return-regexp "return "
  "regular expression of return
When the function body contains this pattern,
js-doc-return-line will be inserted"
  :group 'js-doc)

(defcustom js-doc-throw-regexp "throw"
  "regular expression of throw
When the function body contains this pattern,
js-doc-throw-line will be inserted"
  :group 'js-doc)

(defcustom js-doc-document-regexp "^\[ 	\]*\\*[^//]"
  "regular expression of JsDoc comment
When the string ahead of current point matches this pattarn,
js-doc regards current state as in JsDoc style comment"
  :group 'js-doc)

;;; Main codes:

;; from smart-compile.el
(defun js-doc-format-string (fmt)
  "Format given string and return its result

%F => file name
%P => file path
%a => author name
%d => current date
%p => parameter name
%f => function name
"
  (let ((case-fold-search nil))
    (dolist (pair js-doc-format-string-alist fmt)
      (when (string-match (car pair) fmt)
        (setq fmt (replace-match (eval (cdr pair)) t nil fmt))))))

(defun js-doc-tail (list)
  "Return the last cons cell of the list"
  (if (cdr list)
      (js-doc-tail (cdr list))
    (car list)))

(defun js-doc-pick-symbol-name (str)
  "Pick up symbol-name from str"
  (js-doc-tail (delete "" (split-string str "[^a-zA-Z0-9_$]"))))

(defun js-doc-block-has-regexp (begin end regexp)
  "Return t when regexp matched the current buffer string between begin-end"
  (save-excursion
    (goto-char begin)
    (and t
         (re-search-forward regexp end t 1))))

(defun js-doc-insert-file-doc ()
  "Insert specified-style comment top of the file"
  (interactive)
  (goto-char 1)
  (dolist (line-format js-doc-file-doc-lines)
    (insert (js-doc-format-string (eval line-format)))))

(defun js-doc-insert-function-doc ()
  "Insert JsDoc style comment of the function
The comment style can be custimized via `customize-group js-doc'"
  (interactive)
  ;; prevent odd behaviour of beginning-of-defun
  ;; when user call this command in the certain comment,
  ;; the cursor skip the current function and go to the
  ;; outside block
  (end-of-line)
  (while (or (js-doc-in-comment-p (point))
             (js-doc-blank-line-p (point)))
    (forward-line -1)
    (end-of-line))
  (end-of-line)
  (beginning-of-defun)
  ;; Parse function info
  (let ((params '())
	(document-list '())
	(head-of-func (point))
	from
	to
	begin
	end)
    (save-excursion
      (setq from
	    (search-forward "(" nil t)
            to
	    (1- (search-forward ")" nil t)))
      ;; Now we got the string between ()
      (when (> to from)
	(dolist (param-block
		 (split-string (buffer-substring-no-properties from to) ","))
	  (add-to-list 'params (js-doc-pick-symbol-name param-block) t)))
      ;; begin-end contains whole function body
      (setq begin
            (search-forward "{" nil t)
            end
            (scan-lists (1- begin) 1 0))
    ;; put document string into document-list
    (add-to-list 'document-list
		 (js-doc-format-string js-doc-top-line) t)
    (add-to-list 'document-list
		 (js-doc-format-string js-doc-description-line) t)
    ;; params
    (dolist (param params)
      (setq js-doc-current-parameter-name param)
      (add-to-list 'document-list
		   (js-doc-format-string js-doc-parameter-line) t))
    ;; return / throw
    (when (js-doc-block-has-regexp begin end
				   js-doc-return-regexp)
      (add-to-list 'document-list
		   (js-doc-format-string js-doc-return-line) t))
    (when (js-doc-block-has-regexp begin end
				   js-doc-throw-regexp)
      (add-to-list 'document-list
		   (js-doc-format-string js-doc-throw-line) t))
    ;; end
    (add-to-list 'document-list
		 (js-doc-format-string js-doc-bottom-line) t)
    ;; Insert the document
    (search-backward "(" nil t)
    (beginning-of-line)
    (setq from (point))                 ; for indentation
    (dolist (document document-list)
      (insert document))
    ;; Indent
    (indent-region from (point)))))

;; http://www.emacswiki.org/emacs/UseIswitchBuffer
(defun js-doc-icompleting-read (prompt collection)
  (let ((iswitchb-make-buflist-hook
	 #'(lambda ()
             (setq iswitchb-temp-buflist collection))))
    (iswitchb-read-buffer prompt nil nil)))

(defun js-doc-make-tag-list ()
  (let ((taglist '()))
    (dolist (tagpair js-doc-all-tag-alist)
      (add-to-list 'taglist (car tagpair)))
    (reverse taglist)))

(defun js-doc-blank-line-p (p)
  "Return t when the line at the current point is blank line"
  (save-excursion (eql (progn (beginning-of-line) (point))
		       (progn (end-of-line) (point)))))

(defun js-doc-in-comment-p (p)
  "Return t when the point p is in the comment"
  (save-excursion
    (let (begin end)
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point))
      (or (js-doc-block-has-regexp begin end "//")
          (js-doc-block-has-regexp begin end "/\\*")))))

(defun js-doc-in-document-p (p)
  "Return t when the point p is in JsDoc document"
  ;; Method 1 :: just search for the JsDoc
  (save-excursion
    (goto-char p)
    (and (search-backward "/**" nil t)
	 (not (search-forward "*/" p t)))))

(defun js-doc-insert-tag ()
  "Insert a JsDoc tag interactively."
  (interactive)
  (insert "@")
  (when (js-doc-in-document-p (point))
    (let ((tag (completing-read "Tag: " (js-doc-make-tag-list)
				nil nil nil nil nil)))
      (unless (string-equal tag "")
	(insert tag " ")))))

(defun js-doc-describe-tag ()
  "Describe the JsDoc tag"
  (interactive)
  (let ((tag (completing-read "Tag: " (js-doc-make-tag-list)
			      nil t (word-at-point) nil nil))
	(temp-buffer-show-hook #'(lambda ()
				  (fill-region 0 (buffer-size))
				  (fit-window-to-buffer))))
    (unless (string-equal tag "")
      (with-output-to-temp-buffer "JsDocTagDescription"
	(princ (format "@%s\n\n%s"
		       tag
		       (cdr (assoc tag js-doc-all-tag-alist))))))))

(provide 'js-doc)
