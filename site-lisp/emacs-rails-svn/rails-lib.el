;;; rails-lib.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>
;;          Howard Yeh <hayeah at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-lib.el $
;; $Id: rails-lib.el 213 2007-08-24 12:57:59Z crazypit $

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
  "Convert ((a . b) c d) to ((a . b) (c . c) (d . d))."
  (mapcar
   #'(lambda (el)
       (if (listp el) el(cons el el)))
   list))

(defun uniq-list (list)
  "Return a list of unique elements."
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

;; Strings

(defun string-repeat (char num)
  (let ((len num)
        (str ""))
  (while (not (zerop len))
    (setq len (- len 1))
    (setq str (concat char str)))
  str))


(defmacro string=~ (regex string &rest body)
  "regex matching similar to the =~ operator found in other languages."
  (let ((str (gensym)))
    `(lexical-let ((,str ,string))
       ;; Use lexical-let to make closures (in flet).
       (when (string-match ,regex ,str)
         (symbol-macrolet ,(loop for i to 9 collect
                                 (let ((sym (intern (concat "$" (number-to-string i)))))
                                   `(,sym (match-string ,i ,str))))
           (flet (($ (i) (match-string i ,str))
                  (sub (replacement &optional (i 0) &key fixedcase literal-string)
                       (replace-match replacement fixedcase literal-string ,str i)))
             (symbol-macrolet ( ;;before
                               ($b (substring ,str 0 (match-beginning 0)))
                               ;;match
                               ($m (match-string 0 ,str))
                               ;;after
                               ($a (substring ,str (match-end 0) (length ,str))))
               ,@body)))))))

(defun string-not-empty (str) ;(+)
  "Return t if string STR is not empty."
  (and (stringp str) (not (or (string-equal "" str)
                              (string-match "^ +$" str)))))

(defun yml-value (name)
  "Return the value of the parameter named NAME in the current
buffer or an empty string."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (format "%s:[ ]*\\(.*\\)[ ]*$" name) nil t)
        (match-string 1)
      "")))

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

(defalias 'string-join 'strings-join)

(defun capital-word-p (word)
  "Return t if first letter of WORD is uppercased."
  (= (elt word 0)
     (elt (capitalize word) 0)))

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

(defmacro* in-directory ((directory) &rest body)
  (let ((before-directory (gensym)))
  `(let ((,before-directory default-directory)
         (default-directory ,directory))
       (cd ,directory)
       ,@body
       (cd ,before-directory))))


;; Buffers

(defun buffer-string-by-name (buffer-name)
  "Return the content of buffer named BUFFER-NAME as a string."
  (interactive)
  (save-excursion
    (set-buffer buffer-name)
    (buffer-string)))

(defun buffer-visible-p (buffer-name)
  (if (get-buffer-window buffer-name) t nil))

;; Misc

(defun rails-browse-api-url (url)
  "Browse preferentially with Emacs w3m browser."
  (if rails-browse-api-with-w3m
      (when (fboundp 'w3m-find-file)
        (w3m-find-file (remove-prefix url "file://")))
    (rails-alternative-browse-url url)))

(defun rails-alternative-browse-url (url &rest args)
  "Fix a problem with Internet Explorer not being able to load
URLs with anchors via ShellExecute. It will only be invoked it
the user explicit sets `rails-use-alternative-browse-url'."
  (if (and (eq system-type 'windows-nt) rails-use-alternative-browse-url)
      (w32-shell-execute "open" "iexplore" url)
    (browse-url url args)))

;; abbrev
;; from http://www.opensource.apple.com/darwinsource/Current/emacs-59/emacs/lisp/derived.el
(defun merge-abbrev-tables (old new)
  "Merge an old abbrev table into a new one.
This function requires internal knowledge of how abbrev tables work,
presuming that they are obarrays with the abbrev as the symbol, the expansion
as the value of the symbol, and the hook as the function definition."
  (when old
    (mapatoms
     (lambda(it)
       (or (intern-soft (symbol-name it) new)
           (define-abbrev new
             (symbol-name it)
             (symbol-value it)
             (symbol-function it)
             nil
             t)))
     old)))

;; Colorize

(defun apply-colorize-to-buffer (name)
  (let ((buffer (current-buffer)))
    (set-buffer name)
    (make-local-variable 'after-change-functions)
    (add-hook 'after-change-functions
              '(lambda (start end len)
                 (ansi-color-apply-on-region start end)))
    (set-buffer buffer)))

;; completion-read
(defun rails-completing-read (prompt table history require-match)
  (let ((history-value (symbol-value history)))
  (list (completing-read
         (format "%s?%s: "
                 prompt
                 (if (car history-value)
                     (format " (%s)" (car history-value))
                   ""))
         (list->alist table) ; table
         nil ; predicate
         require-match ; require-match
         nil ; initial input
         history ; hist
         (car history-value))))) ;def

;; MMM

;; (defvar mmm-indent-sandbox-finish-position nil)

;; (defun mmm-run-indent-with-sandbox (indent-func)
;;   (interactive)
;;   (let* ((fragment-name "*mmm-indent-sandbox*")
;;          (ovl (mmm-overlay-at (point)))
;;          (current (when ovl (overlay-buffer ovl)))
;;          (start (when ovl (overlay-start ovl)))
;;          (end (when ovl (overlay-end ovl)))
;;          (current-pos (when ovl (point)))
;;          (ovl-line-start (when start
;;                            (progn (goto-char start)
;;                                   (line-beginning-position))))
;;          (current-line-start (when current-pos
;;                                (progn (goto-char current-pos)
;;                                       (line-beginning-position))))
;;          (fragment-pos (when (and start end) (- (point) (- start 1))))
;;          (ovl-offset (when ovl (- (progn
;;                                     (goto-char start)
;;                                     (while (not (looking-at "<"))
;;                                       (goto-char (- (point) 1)))
;;                                     (point))
;;                                   ovl-line-start)))
;;          (content (when (and start end) (buffer-substring-no-properties start end)))
;;          (fragment (when content (get-buffer-create fragment-name))))
;;     (when fragment
;;       (setq mmm-indent-sandbox-finish-position nil)
;;       (save-excursion
;;         (set-buffer fragment-name)
;;         (beginning-of-buffer)
;;         (insert content)
;;         (goto-char fragment-pos)
;;         (funcall indent-func t)
;;         (let ((start-line)
;;               (end-line)
;;               (kill-after-start)
;;               (finish-pos (- (+ start (point)) 1))
;;               (indented (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;           (set-buffer current)
;;           (kill-buffer fragment-name)
;;           (princ ovl-offset)
;;           (goto-char current-pos)
;;           (setq start-line (line-beginning-position))
;;           (setq end-line (line-end-position))
;;           (when (> start start-line)
;;             (setq start-line (+ start 1))
;;             (setq kill-after-start t))
;;           (when (> end-line end)
;;             (setq end-line end))
;;           (kill-region start-line end-line)
;;           (goto-char start-line)
;;           (unless (= ovl-line-start current-line-start)
;;             (dotimes (i ovl-offset)
;;               (setq indented (concat " " indented))))
;; ;;           (insert-char (string-to-char " ") ovl-offset))
;;           (insert indented)
;;           (when kill-after-start
;;             (goto-char (+ start 1))
;;             (backward-delete-char 1))
;; ;;           (setq mmm-indent-sandbox-finish-position finish-pos)))
;;           (if (= ovl-line-start current-line-start)
;;               (setq mmm-indent-sandbox-finish-position finish-pos)
;;             (setq mmm-indent-sandbox-finish-position (+ finish-pos ovl-offset)))))
;;       (goto-char mmm-indent-sandbox-finish-position))))

;; (defadvice ruby-indent-line (around mmm-sandbox-ruby-indent-line)
;;   (if (and (fboundp 'mmm-overlay-at)
;;            (mmm-overlay-at (point)))
;;       (mmm-run-indent-with-sandbox 'ruby-indent-line)
;;     ad-do-it))
;; (ad-activate 'ruby-indent-line)


;; Cross define functions from my rc files

(provide 'rails-lib)