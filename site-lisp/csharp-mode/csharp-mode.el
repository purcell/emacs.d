;;; csharp-mode.el --- C# mode derived mode

;; Author     : Dylan R. E. Moonfire (original)
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : Feburary 2005
;; Modified   : April 2010
;; Version    : 0.7.6
;; Keywords   : c# languages oop mode
;; X-URL      : http://code.google.com/p/csharpmode/
;; Last-saved : <2010-May-24 21:53:58>

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a separate mode to implement the C# constructs and
;;    font-locking. It is based on the java-mode example from cc-mode.
;;
;;    csharp-mode requires CC Mode 5.30 or later.  It works with
;;    cc-mode 5.31.3, which is current at this time.
;;
;; Features:
;;
;;   - font-lock and indent of C# syntax including:
;;       all c# keywords and major syntax
;;       attributes that decorate methods, classes, fields, properties
;;       enum types
;;       #if/#endif  #region/#endregion
;;       instance initializers
;;       anonymous functions and methods
;;       verbatim literal strings (those that begin with @)
;;       generics
;;
;;   - automagic code-doc generation when you type three slashes.
;;
;;   - intelligent inserttion of matched pairs of curly braces.
;;
;;   - sets the compiler regex for next-error, for csc.exe output.
;;
;;

;;; To use:
;;
;; put this in your .emacs:
;;
;;   (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;
;; or:
;;
;;   (require 'csharp-mode)
;;
;;
;; AND:
;;
;;   (setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;;   (defun my-csharp-mode-fn ()
;;      "function that runs when csharp-mode is initialized for a buffer."
;;      ...insert your code here...
;;      ...most commonly, your custom key bindings ...
;;   )
;;   (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
;;
;;

;;; Known Bugs:
;;
;;   Leading identifiers are no longer being fontified, for some reason.
;;   See matchers-before.
;;
;;   Method names with a preceding attribute are not fontified.
;;
;;   The symbol followng #if is not fontified.  It should be treated like
;;   define and get font-lock-variable-name-face .
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.
;;
;;
;;
;;  Todo:
;;
;;    Get csharp-mode.el accepted as part of the emacs standard distribution.
;;    Must contact monnier at iro.umontreal.ca to make this happen.
;;
;;
;;
;;  Acknowledgements:
;;
;;    Thanks to Alan Mackenzie and Stefan Monnier for answering questions
;;    and making suggestions. And to Trey Jackson for sharing his
;;    knowledge of emacs lisp.
;;
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indention #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads works
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.
;;    0.7.1 - Added option to indent #if/endif with code
;;          - Fixed c-opt-cpp-prefix defn (it must not include the BOL
;;            char (^).
;;          - proper fontification and indent of classes that inherit
;;            (previously the colon was confusing the parser)
;;          - reclassified namespace as a block beginner
;;          - removed $ as a legal symbol char - not legal in C#.
;;          - added struct to c-class-decl-kwds so indent is correct
;;            within a struct.
;;    0.7.2 - Added automatic codedoc insertion.
;;    0.7.3 - Instance initializers (new Type { ... } ) and
;;            (new Type() { ...} ) are now indented properly.
;;          - proper fontification and indent of enums as brace-list-*,
;;            including special treatment for enums that explicitly
;;            inherit from an int type. Previously the colon was
;;            confusing the parser.
;;          - proper fontification of verbatim literal strings,
;;            including those that end in slash. This edge case was not
;;            handled at all before; it is now handled correctly.
;;          - code cleanup and organization; removed the linefeed.
;;          - intelligent curly-brace insertion
;;    0.7.4 - added a C# style
;;          - using is now a keyword and gets fontified correctly
;;          - fixed a bug that had crept into the codedoc insertion
;;    0.7.5 - now fontify namespaces in the using statements. This is
;;            done in the csharp value for c-basic-matchers-before .
;;          - also fontify the name following namespace decl.
;;            This is done in the csharp value for c-basic-matchers-after .
;;          - turn on recognition of generic types. They are now
;;            fontified correctly.
;;          - <> are now treated as syntactic parens and can be jumped
;;            over with c-forward-sexp.
;;          - Constructors are now fontified.
;;          - Field/Prop names inside object initializers are now fontified.
;;


;;


(require 'cc-mode)

(message  (concat "Loading " load-file-name))


;; ==================================================================
;; c# upfront stuff
;; ==================================================================

;; This is a copy of the function in cc-mode which is used to handle
;; the eval-when-compile which is needed during other times.
(defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
  ;; See cc-langs.el, a direct copy.
  (unless (listp (car-safe ops))
    (setq ops (list ops)))
  (cond ((eq opgroup-filter t)
         (setq opgroup-filter (lambda (opgroup) t)))
        ((not (functionp opgroup-filter))
         (setq opgroup-filter `(lambda (opgroup)
                                 (memq opgroup ',opgroup-filter)))))
  (cond ((eq op-filter t)
         (setq op-filter (lambda (op) t)))
        ((stringp op-filter)
         (setq op-filter `(lambda (op)
                            (string-match ,op-filter op)))))
  (unless xlate
    (setq xlate 'identity))
  (c-with-syntax-table (c-lang-const c-mode-syntax-table)
    (delete-duplicates
     (mapcan (lambda (opgroup)
               (when (if (symbolp (car opgroup))
                         (when (funcall opgroup-filter (car opgroup))
                           (setq opgroup (cdr opgroup))
                           t)
                       t)
                 (mapcan (lambda (op)
                           (when (funcall op-filter op)
                             (let ((res (funcall xlate op)))
                               (if (listp res) res (list res)))))
                         opgroup)))
             ops)
     :test 'equal)))



;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'csharp-mode 'java-mode))

;; ==================================================================
;; end of c# upfront stuff
;; ==================================================================





;; ==================================================================
;; csharp-mode utility and feature defuns
;; ==================================================================


(defun csharp-at-vsemi-p (&optional pos)
  "Determines if there is a virtual semicolon at POS or point.
This is the C# version of the function.

A vsemi is a cc-mode concept implying end-of-statement, without
a semicolon or close-brace. This happens in 2 cases in C#:

 - after an attribute that decorates a class, method, field, or
   property.

 - after an ASPNET directive, that appears in a aspx/ashx/ascx file

An example of the former is  [WebMethod] or [XmlElement].
An example of the latter is something like this:

    <%@ WebHandler Language=\"C#\" Class=\"Handler\" %>

Providing this function allows the indenting in csharp-mode
to work properly with code that includes attributes and ASPNET
directives.

Returns t if at a position where a virtual-semicolon is.
Otherwise nil.
"

  (save-excursion
    (let ((pos-or-point (progn (if pos (goto-char pos)) (point))))

      (cond

       ;; put a vsemi after an ASPNET directive, like
       ;; <%@ WebHandler Language="C#" Class="Handler" %>
       ((looking-back (concat csharp-aspnet-directive-re "$") nil t)
        t)

       ;; put a vsemi after an attribute, as with
       ;;   [XmlElement]
       ((c-safe (backward-sexp) t)
        (cond
           ((re-search-forward
             (concat
              "\\(\\["
              "[ \t\n\r\f\v]*"
              "\\("
              "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
              "[A-Za-z_][[:alnum:]]*"
              "\\)"
              "[^]]*\\]\\)"
              )
             (1+ pos-or-point) t)

             (c-safe (backward-sexp))
             (c-backward-syntactic-ws)
             (cond

              ((eq (char-before) 93) ;; close sq brace
               (csharp-at-vsemi-p (point)))

              ((or
                (eq (char-before) 59) ;; semicolon
                (eq (char-before) 123) ;; open curly
                (eq (char-before) 125)) ;; close curly
               t)

              (t nil)))

           (t nil)))

        (t nil))
      )))




(defun csharp-lineup-region (langelem)
  "Indent all #region and #endregion blocks inline with code while
retaining normal column-zero indention for #if and the other
processing blocks.

To use this indenting just put the following in your emacs file:
   (c-set-offset 'cpp-macro 'csharp-lineup-region)

An alternative is to use `csharp-lineup-if-and-region'.
"

  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(end\\)?region" (c-point 'eol) [0]) 0  [0])))



(defun csharp-lineup-if-and-region (langelem)

"Indent all #region/endregion blocks and #if/endif blocks inline
with code while retaining normal column-zero indention for any
other processing blocks.

To use this indenting just put the following in your emacs file:
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

Another option is to use `csharp-lineup-region'.

"
  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(\\(end\\)?\\(if\\|region\\)\\|else\\)" (c-point 'eol) [0]) 0  [0])))





(defun csharp-insert-open-brace ()
  "Intelligently insert a pair of curly braces. This fn is most
    often bound to the open-curly brace, with

        (local-set-key (kbd \"{\") 'csharp-insert-open-brace)

    The default binding for an open curly brace in cc-modes is often
    `c-electric-brace' or `skeleton-pair-insert-maybe'.  The former
    can be configured to insert newlines around braces in various
    syntactic positions.  The latter inserts a pair of braces and
    then does not insert a newline, and does not indent.

    This fn provides another option, with some additional
    intelligence for csharp-mode.  When you type an open curly, the
    appropriate pair of braces appears, with spacing and indent set
    in a context-sensitive manner.

    Within a string literal, you just get a pair of braces, and
    point is set between them. Following an equals sign, you get
    a pair of braces, with a semincolon appended. Otherwise, you
    get the open brace on a new line, followed by an empty line
    and the closing brace on the line following, with point on
    the empty line.

    There may be another way to get this to happen appropriately just
    within emacs, but I could not figure out how to do it.  So I
    wrote this alternative.

    "
  (interactive)
  (let
      (tpoint
       (in-string (string= (csharp-in-literal) "string"))
       (preceding3
        (save-excursion
          (and
           (skip-chars-backward " \t")
           (> (- (point) 2) (point-min))
           (buffer-substring-no-properties (point) (- (point) 3)))))
       (one-word-back
        (save-excursion
          (backward-word 2)
          (thing-at-point 'word))))

    (cond

     ;; Case 1: inside a string literal?
     ;; --------------------------------------------
     ;; If so, then just insert a pair of braces and put the point
     ;; between them.  The most common case is a format string for
     ;; String.Format() or Console.WriteLine().
     (in-string
      (self-insert-command 1)
      (insert "}")
      (backward-char))

     ;; Case 2: the open brace starts an array initializer.
     ;; --------------------------------------------
     ;; When the last non-space was an equals sign or square brackets,
     ;; then it's an initializer.
     ((save-excursion
        (and (c-safe (backward-sexp) t)
             (looking-at "\\(\\w+\\b *=\\|[[]]+\\)")))
      (self-insert-command 1)
      (insert "  };")
      (backward-char 3))

     ;; Case 3: the open brace starts an instance initializer
     ;; --------------------------------------------
     ;; If one-word-back was "new", then it's an object initializer.
     ((string= one-word-back "new")
      (save-excursion
        (message "object initializer")
        (setq tpoint (point)) ;; prepare to indent-region later
        (newline)
        (self-insert-command 1)
        (newline-and-indent)
        (newline)
        (insert "};")
        (c-indent-region tpoint (point))
        (previous-line)
        (indent-according-to-mode)
        (end-of-line)
        (setq tpoint (point)))
      (goto-char tpoint))

     ;; Case 4: a lambda initialier.
     ;; --------------------------------------------
     ;; If the open curly follows =>, then it's a lambda initializer.
     ((string= (substring preceding3 -2) "=>")
      (message "lambda init")
      (self-insert-command 1)
      (insert "  }")
      (backward-char 2))

     ;; else, it's a new scope. (if, while, class, etc)
     (t
      (save-excursion
        (message "new scope")
        (set-mark (point)) ;; prepare to indent-region later
        ;; check if the prior sexp is on the same line
        (if (save-excursion
              (let ((curline (line-number-at-pos))
                    (aftline (progn
                               (if (c-safe (backward-sexp) t)
                                   (line-number-at-pos)
                                 -1))))
                (= curline aftline)))
            (newline-and-indent))
        (self-insert-command 1)
        (c-indent-line-or-region)
        (end-of-line)
        (newline)
        (insert "}")
        ;;(c-indent-command) ;; not sure of the difference here
        (c-indent-line-or-region)
        (previous-line)
        (end-of-line)
        (newline-and-indent)
        ;; point ends up on an empty line, within the braces, properly indented
        (setq tpoint (point)))

      (goto-char tpoint)))))


;; ==================================================================
;; end of csharp-mode utility and feature defuns
;; ==================================================================






;; ==================================================================
;; c# values for "language constants" defined in cc-langs.el
;; ==================================================================


;; Java uses a series of regexes to change the font-lock for class
;; references. The problem comes in because Java uses Pascal (leading
;; space in names, SomeClass) for class and package names, but
;; Camel-casing (initial lowercase, upper case in words,
;; i.e. someVariable) for variables. The notation suggested by EMCA for C# is
;; to use Pascal notation for everything, except inner variables. So,
;; the Java regex and formatting produces very wrong results in C#.
;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))

(defconst csharp-aspnet-directive-re
  "<%@.+?%>"
  "Regex for matching directive blocks in ASP.NET files (.aspx, .ashx, .ascx)")

(defconst csharp-enum-decl-re
  (concat
   "\\<enum[ \t\n\r\f\v]+"
   "\\([[:alpha:]_][[:alnum:]_]*\\)"
   "[ \t\n\r\f\v]*"
   "\\(:[ \t\n\r\f\v]*"
   "\\("
   (c-make-keywords-re nil
     (list "sbyte" "byte" "short" "ushort" "int" "uint" "long" "ulong"))
   "\\)"
   "\\)?")
  "Regex that captures an enum declaration in C#"
  )



;; X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+

;; vsemi's allow proper indentation of code that includes inline
;; attributes and ASPNET directives. These are c#-specific things that
;; need custom treatment.
(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)


;; This c-opt-after-id-concat-key is a regexp that matches
;; dot.  In other words: "\\(\\.\\)"
;; Not sure why this needs to be so complicated.
;; This const is now internal (obsolete); need to move to
;; c-after-id-concat-ops.  I don't yet understand the meaning
;; of that variable, so for now. . .  .
(c-lang-defconst c-opt-after-id-concat-key
  csharp (if (c-lang-const c-opt-identifier-concat-key)
             (c-lang-const c-symbol-start)))



;; The matchers elements can be of many forms.  It gets pretty
;; complicated.  Do a describe-variable on font-lock-keywords to get a
;; description.  (Why on font-lock-keywords? I don't know, but that's
;; where you get the help.)
;;
;; Aside from the provided documentation, the other option of course, is
;; to look in the source code as an example for what to do.  The source
;; in cc-fonts uses a defun c-make-font-lock-search-function to produce
;; most of the matchers.  Called this way:
;;
;;   (c-make-font-lock-search-function  regexp '(A B c))
;;
;; The REGEXP is used in re-search-forward, and if there's a match, the
;; A B and C are three forms that are called in a weird combination.
;;
;; Anyway the c-make-font-lock-search-function works for a single regex,
;; but more complicated scenarios such as those intended to match and
;; fontify object initializers, call for a hand-crafted lambda.
;;
;; The object initializer is special because, matching on it must
;; allow nesting.
;;
;; In c#, the object initializer block is used directly after a
;; constructor, like this:
;;
;;     new MyType {
;;        Prop1 = "foo"
;;     }
;;
;; csharp-mode needs to fontify the properties in the
;; initializer block in font-lock-variable-name-face. The key thing is
;; to set the text property on the open curly, using type c-type and
;; value c-decl-id-start. This apparently allows `parse-partial-sexp' to
;; do the right thing, later.
;;
;; This simple case is easy to handle in a regex, using the basic
;; `c-make-font-lock-search-function' form.  But the general syntax for a
;; constructor + object initializer in C# is more complex:
;;
;;     new MyType(..arglist..) {
;;        Prop1 = "foo"
;;     }
;;
;; A simple regex match won't satisfy here, because the ..arglist.. can
;; be anything, including calls to other constructors, potentially with
;; object initializer blocks. This may nest arbitrarily deeply, and the
;; regex in emacs doesn't support balanced matching.  Therefore there's
;; no way to match on the "outside" pair of parens, to find the relevant
;; open curly.  What's necessary is to do the match on "new MyType" then
;; skip over the sexp defined by the parens, then set the text property on
;; the appropriate open-curly.
;;
;; To make that happen, it's good to have insight into what the matcher
;; really does.  The output of `c-make-font-lock-search-function' before
;; byte-compiling, is:
;;
;; (lambda (limit)
;;   (let ((parse-sexp-lookup-properties
;;          (cc-eval-when-compile
;;            (boundp 'parse-sexp-lookup-properties))))
;;     (while (re-search-forward REGEX limit t)
;;       (unless
;;           (progn
;;             (goto-char (match-beginning 0))
;;             (c-skip-comments-and-strings limit))
;;         (goto-char (match-end 0))
;;         (progn
;;           B
;;           (save-match-data A)
;;           C ))))
;;   nil)
;;
;; csharp-mode uses this hand-crafted form of a matcher to handle the
;; general case for constructor + object initializer, within
;; `c-basic-matchers-after' .
;;




;; (defun c-make-font-lock-search-function (regexp &rest highlights)
;;     ;; This function makes a byte compiled function that works much like
;;     ;; a matcher element in `font-lock-keywords'.  It cuts out a little
;;     ;; bit of the overhead compared to a real matcher.  The main reason
;;     ;; is however to pass the real search limit to the anchored
;;     ;; matcher(s), since most (if not all) font-lock implementations
;;     ;; arbitrarily limits anchored matchers to the same line, and also
;;     ;; to insulate against various other irritating differences between
;;     ;; the different (X)Emacs font-lock packages.
;;     ;;
;;     ;; REGEXP is the matcher, which must be a regexp.  Only matches
;;     ;; where the beginning is outside any comment or string literal are
;;     ;; significant.
;;     ;;
;;     ;; HIGHLIGHTS is a list of highlight specs, just like in
;;     ;; `font-lock-keywords', with these limitations: The face is always
;;     ;; overridden (no big disadvantage, since hits in comments etc are
;;     ;; filtered anyway), there is no "laxmatch", and an anchored matcher
;;     ;; is always a form which must do all the fontification directly.
;;     ;; `limit' is a variable bound to the real limit in the context of
;;     ;; the anchored matcher forms.
;;     ;;
;;     ;; This function does not do any hidden buffer changes, but the
;;     ;; generated functions will.  (They are however used in places
;;     ;; covered by the font-lock context.)
;;
;;     ;; Note: Replace `byte-compile' with `eval' to debug the generated
;;     ;; lambda easier.
;;     (byte-compile
;;      `(lambda (limit)
;;         (let (;; The font-lock package in Emacs is known to clobber
;;               ;; `parse-sexp-lookup-properties' (when it exists).
;;               (parse-sexp-lookup-properties
;;                (cc-eval-when-compile
;;                  (boundp 'parse-sexp-lookup-properties))))
;;           (while (re-search-forward ,regexp limit t)
;;             (unless (progn
;;                       (goto-char (match-beginning 0))
;;                       (c-skip-comments-and-strings limit))
;;               (goto-char (match-end 0))
;;               ,@(mapcar
;;                  (lambda (highlight)
;;                    (if (integerp (car highlight))
;;                        (progn
;;                          (unless (eq (nth 2 highlight) t)
;;                            (error
;;                             "The override flag must currently be t in %s"
;;                             highlight))
;;                          (when (nth 3 highlight)
;;                            (error
;;                             "The laxmatch flag may currently not be set in %s"
;;                             highlight))
;;                          `(save-match-data
;;                             (c-put-font-lock-face
;;                              (match-beginning ,(car highlight))
;;                              (match-end ,(car highlight))
;;                              ,(elt highlight 1))))
;;                      (when (nth 3 highlight)
;;                        (error "Match highlights currently not supported in %s"
;;                               highlight))
;;                      `(progn
;;                         ,(nth 1 highlight)
;;                         (save-match-data ,(car highlight))
;;                         ,(nth 2 highlight))))
;;                  highlights))))
;;         nil))
;;     )


(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;;;; Font-lock the attributes by searching for the
           ;;;; appropriate regex and marking it as TODO.
           ;;,`(,(concat "\\(" csharp-attribute-regex "\\)")
           ;;   0 font-lock-function-name-face)

           ;; Put a warning face on the opener of unclosed strings that
           ;; can't span lines.  Later font
           ;; lock packages have a `font-lock-syntactic-face-function' for
           ;; this, but it doesn't give the control we want since any
           ;; fontification done inside the function will be
           ;; unconditionally overridden.
           ,(c-make-font-lock-search-function
             ;; Match a char before the string starter to make
             ;; `c-skip-comments-and-strings' work correctly.
             (concat ".\\(" c-string-limit-regexp "\\)")
             '((c-font-lock-invalid-string)))


           ;; Fontify keyword constants.
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil
                           (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))


           ;; Fontify the namespaces that follow using statements.
           ;; This regex handles the optional alias, but does not fontify it.
           ,`("\\<\\(using\\)\s+\\(?:[A-Za-z_][[:alnum:]]*\s*=\s*\\)?\\(\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*[A-Za-z_][[:alnum:]]*\\)\s*;"
               2 font-lock-constant-face)


           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)


           ;; Fontify leading identifiers in fully qualified names like
           ;; "Foo.Bar".
           ,@(when (c-lang-const c-opt-identifier-concat-key)
               `((,(byte-compile
                    `(lambda (limit)
                       (while (re-search-forward
                               ,(concat "\\(\\<" ; 1
                                        "\\(" (c-lang-const c-symbol-key)
                                        "\\)" ; 2
                                        "[ \t\n\r\f\v]*"
                                        (c-lang-const
                                         c-opt-identifier-concat-key)
                                        "[ \t\n\r\f\v]+"
                                        "\\)"
                                        "\\("
                                        (c-lang-const
                                         c-opt-after-id-concat-key)
                                        "\\)")
                               limit t)
                         (unless (progn
                                   (goto-char (match-beginning 0))
                                   (c-skip-comments-and-strings limit))
                           (or (get-text-property (match-beginning 2) 'face)
                               (c-put-font-lock-face (match-beginning 2)
                                                     (match-end 2)
                                                     c-reference-face-name))
                           (goto-char (match-end 1)))))))))

           ))



(c-lang-defconst c-basic-matchers-after
  csharp `(

           ;; option 1:
           ;;            ,@(when condition
           ;;                `((,(byte-compile
           ;;                     `(lambda (limit) ...
           ;;
           ;; option 2:
           ;;            ,`((lambda (limit) ...
           ;;
           ;; I don't know how to avoid the (when condition ...) in the
           ;; byte-compiled version.
           ;;
           ;; X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+

           ;; Case 1: invocation of constructor + maybe an object
           ;; initializer.  Some possible examples that satisfy:
           ;;
           ;;   new Foo ();
           ;;
           ;;   new Foo () { };
           ;;
           ;;   new Foo {  };
           ;;
           ;;   new Foo { Prop1= 7 };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7
           ;;   };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7,
           ;;     Prop2= "Fred"
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar()
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar { PropA = 5.6F }
           ;;   };
           ;;

           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                        (let ((parse-sexp-lookup-properties
                               (cc-eval-when-compile
                                 (boundp 'parse-sexp-lookup-properties))))

                          (while (re-search-forward
                                  ,(concat "\\<new"
                                           "[ \t\n\r\f\v]+"
                                           "\\(\\(?:"
                                           (c-lang-const c-symbol-key)
                                           "\\.\\)*"
                                           (c-lang-const c-symbol-key)
                                           "\\)"
                                           )
                                  limit t)
                            (unless
                                (progn
                                  (goto-char (match-beginning 0))
                                  (c-skip-comments-and-strings limit))

                              (csharp-log 3 "ctor candidate at %d" (match-beginning 1))

                              (save-match-data
                                ;; next thing could be: [] () <> or {} or nothing (semicolon, comma).

                                ;; fontify the typename
                                (c-put-font-lock-face (match-beginning 1)
                                                      (match-end 1)
                                                      'font-lock-type-face)

                                (goto-char (match-end 0))
                                (c-forward-syntactic-ws)
                                (if (eq (char-after) ?<) ;; ctor for generic type
                                    (progn
                                      (csharp-log 3 " - generic ctor")
                                      ;; skip over <> safely
                                      (c-safe (c-forward-sexp 1) t)
                                      (c-forward-syntactic-ws)))

                                ;; now, could be [] or (..) or {..} or semicolon.

                                (csharp-log 3 " - looking for sexp")

                                (if (or
                                     (eq (char-after) ?{) ;; open curly
                                     (and (eq (char-after) 91) ;; open square
                                          (while (eq (char-after) 91)
                                            (c-safe (c-forward-sexp 1)))
                                          (eq (char-before) 93)) ;; close square
                                     (and (eq (char-after) 40) ;; open paren
                                          (c-safe (c-forward-sexp 1) t)))

                                    (progn
                                      ;; at this point we've jumped over any intervening s-exp
                                      (c-forward-syntactic-ws)
                                      (csharp-log 3 " - after fwd-syn-ws point(%d)" (point))
                                      (csharp-log 3 " - next char:  %c" (char-after))
                                      (if (eq (char-after) ?{)
                                          (let ((start (point))
                                                (end (if (c-safe (c-forward-sexp 1) t)
                                                         (point) 0)))
                                            (csharp-log 3 " - put c-decl-id-start on the open-curly at %d" start)
                                            (c-put-char-property start
                                                                 'c-type
                                                                 'c-decl-id-start)
                                            (goto-char start)
                                            (if (> end start)
                                                (progn
                                                  (forward-char 1) ;; step over open curly
                                                  (c-forward-syntactic-ws)
                                                  (while (> end (point))
                                                    ;; now, try to fontify/assign variables to any properties inside the curlies
                                                    (csharp-log 3 " - inside open curly  point(%d)" (point))
                                                    (csharp-log 3 " -   next char:  %c" (char-after))
                                                    ;; fontify each property assignment
                                                    (if (re-search-forward
                                                         (concat "\\(" (c-lang-const c-symbol-key) "\\)\s*=")
                                                         end t)
                                                        (progn
                                                          (csharp-log 3 " -   found variable  %d-%d"
                                                                      (match-beginning 1)
                                                                      (match-end 1))
                                                          (c-put-font-lock-face (match-beginning 1)
                                                                                (match-end 1)
                                                                                'font-lock-variable-name-face)
                                                          (goto-char (match-end 0))
                                                          (c-forward-syntactic-ws)
                                                          ;; advance to the next assignment, if possible
                                                          (if (eq (char-after) ?@)
                                                              (forward-char 1))

                                                          (if (c-safe (c-forward-sexp 1) t)
                                                              (progn
                                                                (forward-char 1)
                                                                (c-forward-syntactic-ws))))

                                                      ;; else
                                                      (csharp-log 3 " -   no more assgnmts found")
                                                      (goto-char end)))))
                                            )))))

                              (goto-char (match-end 0))
                              )))
                        nil))
                    )))


           ;; Case 2: declaration of enum with or without an explicit
           ;; base type.
           ;;
           ;; Examples:
           ;;
           ;;  public enum Foo { ... }
           ;;
           ;;  public enum Foo : uint { ... }
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat csharp-enum-decl-re
                                          "[ \t\n\r\f\v]*"
                                          "{")
                                 limit t)

                           (csharp-log 3 "enum candidate at %d" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))
                             (progn
                               (save-match-data
                                 (goto-char (match-end 0))
                                 (c-put-char-property (1- (point))
                                                      'c-type
                                                      'c-decl-id-start)
                                 (c-forward-syntactic-ws))
                               (save-match-data
                                 (c-font-lock-declarators limit t nil))
                               (goto-char (match-end 0))
                               )
                             )))
                       nil))
                  )))


           ;; Case 3: declaration of constructor
           ;;
           ;; Example:
           ;;
           ;; private Foo(...) {...}
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties)))
                             (found-it nil))
                         (while (re-search-forward
                                 ,(concat
                                   "^[ \t\n\r\f\v]*"
                                   "\\(\\<\\(public\\|private\\|protected\\)\\)?[ \t\n\r\f\v]+"
                                   "\\(@?[[:alpha:]_][[:alnum:]_]*\\)" ;; name of constructor
                                   "[ \t\n\r\f\v]*"
                                   "\\("
                                   "("
                                   "\\)")
                                 limit t)
                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (goto-char (match-end 0))

                             (csharp-log 3 "ctor decl candidate ending at %d" (point))

                             (backward-char 1) ;; just left of the open paren
                             (save-match-data
                               ;; Jump over the parens, safely.
                               ;; If it's an unbalanced paren, no problem,
                               ;; do nothing.
                               (if (c-safe (c-forward-sexp 1) t)
                                   (progn
                                     (c-forward-syntactic-ws)
                                     (cond

                                      ;; invokes base or this constructor.
                                      ((re-search-forward
                                        ,(concat
                                          "\\(:[ \t\n\r\f\v]*\\(base\\|this\\)\\)"
                                          "[ \t\n\r\f\v]*"
                                          "("
                                          )
                                        limit t)
                                       (csharp-log 3 " - ctor with dependency?")

                                       (goto-char (match-end 0))
                                       (backward-char 1) ;; just left of the open paren
                                       (csharp-log 3 " - before paren at %d" (point))

                                       (if (c-safe (c-forward-sexp 1) t)
                                           (progn
                                             (c-forward-syntactic-ws)
                                             (csharp-log 3 " - skipped over paren pair %d" (point))
                                             (if (eq (char-after) ?{)
                                                 (setq found-it t)))))

                                      ;; open curly. no depedency on other ctor.
                                      ((eq (char-after) ?{)
                                       (csharp-log 3 " - ctor with no dependency? at %d" (point))
                                       (setq found-it t)))

                                     )))

                             (if found-it
                                 ;; fontify the constructor symbol
                                 (c-put-font-lock-face (match-beginning 3)
                                                       (match-end 3)
                                                       'font-lock-function-name-face))
                             (goto-char (match-end 0))
                             )
                           ))
                       nil))
                  )))


           ;; Case 4: using clause. Without this, using (..) gets fontified as a fn.
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat "\\<\\(using\\)"
                                          "[ \t\n\r\f\v]*"
                                          "(")
                                 limit t)

                           (csharp-log 3 "using clause at %d" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (save-match-data
                               (c-put-font-lock-face (match-beginning 1)
                                                     (match-end 1)
                                                     'font-lock-keyword-face)
                               (goto-char (match-end 0))))))
                       nil))
                  )))

           ;; Case 5: attributes
           ,`((lambda (limit)
                (let ((parse-sexp-lookup-properties
                       (cc-eval-when-compile
                         (boundp 'parse-sexp-lookup-properties))))

                  (while (re-search-forward
                          ,(concat "[ \t\n\r\f\v]+"
                                   "\\(\\["
                                   "[ \t\n\r\f\v]*"
                                   "\\(?:\\(?:return\\|assembly\\)[ \t]*:[ \t]*\\)?"
                                   "\\("
                                   "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
                                   "[A-Za-z_][[:alnum:]]*"
                                   "\\)"
                                   "[^]]*\\]\\)"
                                   )
                          limit t)

                    (csharp-log 3 "attribute? - %d limit(%d)" (match-beginning 1)
                                limit)

                    (unless
                        (progn
                          (goto-char (match-beginning 1))
                          (c-skip-comments-and-strings limit))

                      (let ((b2 (match-beginning 2))
                            (e2 (match-end 2))
                            (is-attr nil))
                        (csharp-log 3 " - type match: %d - %d"
                                    b2 e2)
                        (save-match-data
                          (c-backward-syntactic-ws)
                          (setq is-attr (or
                                         (eq (char-before) 59) ;; semicolon
                                         (eq (char-before) 93) ;; close square brace
                                         (eq (char-before) 123) ;; open curly
                                         (eq (char-before) 125) ;; close curly
                                         (save-excursion
                                           (c-beginning-of-statement-1)
                                           (looking-at
                                            "#\\(pragma\\|endregion\\|region\\|if\\|else\\|endif\\)"))
                                         )))

                        (if is-attr
                            (progn
                              (csharp-log 3 " - attribute seems likely. type: %d - %d"
                                          b2 e2)
                              (c-put-font-lock-face b2 e2 'font-lock-type-face)))))
                    (goto-char (match-end 0))
                    ))
                nil))


           ;; Case 6: directive blocks for .aspx/.ashx/.ascx
           ,`((lambda (limit)
                (let ((parse-sexp-lookup-properties
                       (cc-eval-when-compile
                         (boundp 'parse-sexp-lookup-properties))))

                  (while (re-search-forward csharp-aspnet-directive-re limit t)
                    (csharp-log 3 "aspnet template? - %d limit(%d)" (match-beginning 1)
                                limit)

                    (unless
                        (progn
                          (goto-char (match-beginning 0))
                          (c-skip-comments-and-strings limit))

                        (save-match-data
                          (let ((end-open (+ (match-beginning 0) 3))
                                (beg-close (- (match-end 0) 2)))
                            (c-put-font-lock-face (match-beginning 0)
                                                  end-open
                                                  'font-lock-preprocessor-face)

                            (c-put-font-lock-face beg-close
                                                  (match-end 0)
                                                  'font-lock-preprocessor-face)

                            ;; fontify within the directive
                            (while (re-search-forward
                                    ,(concat
                                      "\\("
                                      (c-lang-const c-symbol-key)
                                      "\\)"
                                      "=?"
                                      )
                                    beg-close t)

                            (c-put-font-lock-face (match-beginning 1)
                                                  (match-end 1)
                                                  'font-lock-keyword-face)
                            (c-skip-comments-and-strings beg-close))
                            ))
                        (goto-char (match-end 0)))))
                nil))


;;            ;; Case 5: #if
;;            ,@(when t
;;                `((,(byte-compile
;;                     `(lambda (limit)
;;                        (let ((parse-sexp-lookup-properties
;;                               (cc-eval-when-compile
;;                                 (boundp 'parse-sexp-lookup-properties))))
;;                          (while (re-search-forward
;;                                  "\\<\\(#if\\)[ \t\n\r\f\v]+\\([A-Za-z_][[:alnum:]]*\\)"
;;                                  limit t)
;;
;;                            (csharp-log 3 "#if directive - %d" (match-beginning 1))
;;
;;                            (unless
;;                                (progn
;;                                  (goto-char (match-beginning 0))
;;                                  (c-skip-comments-and-strings limit))
;;
;;                              (save-match-data
;;                                (c-put-font-lock-face (match-beginning 2)
;;                                                      (match-end 2)
;;                                                      'font-lock-variable-name-face)
;;                                (goto-char (match-end 0))))))
;;                        nil))
;;                   )))


 ;;           ,`(,(c-make-font-lock-search-function
 ;;                (concat "\\<new"
 ;;                        "[ \t\n\r\f\v]+"
 ;;                        "\\(\\(?:"
 ;;                        (c-lang-const c-symbol-key)
 ;;                        "\\.\\)*"
 ;;                        (c-lang-const c-symbol-key)
 ;;                        "\\)"
 ;;                        "[ \t\n\r\f\v]*"
 ;;                        "\\(?:"
 ;;                        "( *)[ \t\n\r\f\v]*"          ;; optional ()
 ;;                        "\\)?"
 ;;                        "{")
 ;;                '((c-font-lock-declarators limit t nil)
 ;;                  (save-match-data
 ;;                    (goto-char (match-end 0))
 ;;                    (c-put-char-property (1- (point)) 'c-type
 ;;                                         'c-decl-id-start)
 ;;                    (c-forward-syntactic-ws))
 ;;                  (goto-char (match-end 0)))))




           ;; Fontify labels after goto etc.
           ,@(when (c-lang-const c-before-label-kwds)
               `( ;; (Got three different interpretation levels here,
                 ;; which makes it a bit complicated: 1) The backquote
                 ;; stuff is expanded when compiled or loaded, 2) the
                 ;; eval form is evaluated at font-lock setup (to
                 ;; substitute c-label-face-name correctly), and 3) the
                 ;; resulting structure is interpreted during
                 ;; fontification.)
                 (eval
                  . ,(let* ((c-before-label-re
                             (c-make-keywords-re nil
                               (c-lang-const c-before-label-kwds))))
                       `(list
                         ,(concat "\\<\\(" c-before-label-re "\\)\\>"
                                  "\\s *"
                                  "\\(" ; identifier-offset
                                  (c-lang-const c-symbol-key)
                                  "\\)")
                         (list ,(+ (regexp-opt-depth c-before-label-re) 2)
                               c-label-face-name nil t))))))



           ;; Fontify the clauses after various keywords.
           ,@(when (or (c-lang-const c-type-list-kwds)
                       (c-lang-const c-ref-list-kwds)
                       (c-lang-const c-colon-type-list-kwds)
                       (c-lang-const c-paren-type-kwds))
               `((,(c-make-font-lock-search-function
                    (concat "\\<\\("
                            (c-make-keywords-re nil
                              (append (c-lang-const c-type-list-kwds)
                                      (c-lang-const c-ref-list-kwds)
                                      (c-lang-const c-colon-type-list-kwds)
                                      (c-lang-const c-paren-type-kwds)))
                            "\\)\\>")
                    '((c-fontify-types-and-refs ((c-promote-possible-types t))
                        (c-forward-keyword-clause 1)
                        (if (> (point) limit) (goto-char limit))))))))


           ;; Fontify the name that follows each namespace declaration
           ;; this needs to be done in the matchers-after because
           ;; otherwise the namespace names get the font-lock-type-face,
           ;; due to the energetic efforts of c-forward-type.
           ,`("\\<\\(namespace\\)[ \t\n\r\f\v]+\\(\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*[A-Za-z_][[:alnum:]]*\\)"
              2 font-lock-constant-face t)


           ))


;; C# does generics.  Setting this to t tells the parser to put
;; parenthesis syntax on angle braces that surround a comma-separated
;; list.
(c-lang-defconst c-recognize-<>-arglists
  csharp t)



(c-lang-defconst c-identifier-key
  csharp (concat "\\([[:alpha:]_][[:alnum:]_]*\\)" ; 1
                 "\\("
                 "[ \t\n\r\f\v]*"
                 "\\(\\.\\)"             ;;(c-lang-const c-opt-identifier-concat-key)
                 "[ \t\n\r\f\v]*"
                 "\\(\\([[:alpha:]_][[:alnum:]_]*\\)\\)"
                 "\\)*"))

;; C# has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the C#'s "base".
(c-lang-defconst c-operators
  csharp `((prefix "base")))


;; C# uses CPP-like prefixes to mark #define, #region/endregion,
;; #if/else/endif, and #pragma.  This regexp matches the prefix,
;; not including the beginning-of-line (BOL), and not including
;; the term after the prefix (define, pragma, etc).  This regexp says
;; whitespace, followed by the prefix, followed by maybe more whitespace.

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")


;; there are no message directives in C#
(c-lang-defconst c-cpp-message-directives
  csharp nil)

(c-lang-defconst c-cpp-expr-directives
  csharp '("if"))

(c-lang-defconst c-opt-cpp-macro-define
  csharp "define")

;; $ is not a legal char in an identifier in C#.  So we need to
;; create a csharp-specific definition of this constant.
(c-lang-defconst c-symbol-chars
  csharp (concat c-alnum "_"))


(c-lang-defconst c-colon-type-list-kwds
  csharp '("class"))

(c-lang-defconst c-block-prefix-disallowed-chars

  ;; Allow ':' for inherit list starters.
  csharp (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                         '(?: ?,)))


(c-lang-defconst c-assignment-operators
  csharp '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

(c-lang-defconst c-primitive-type-kwds
  ;; ECMA-344, S8
  csharp '("object" "string" "sbyte" "short" "int" "long" "byte"
           "ushort" "uint" "ulong" "float" "double" "bool" "char"
           "decimal" "void"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  ;; ECMA-344, S?
  csharp '("class" "interface" "struct"))  ;; no enum here.
                                           ;; we want enum to be a brace list.


;; Type modifier keywords. They appear anywhere in types, but modify
;; instead of create one.
(c-lang-defconst c-type-modifier-kwds
  ;; EMCA-344, S?
  csharp '("readonly" "const"))


;; Tue, 20 Apr 2010  16:02
;; need to verify that this works for lambdas...
(c-lang-defconst c-special-brace-lists
  csharp '((?{ . ?}) ))



;; dinoch
;; Thu, 22 Apr 2010  18:54
;;
;; No idea why this isn't getting set properly in the first place.
;; In cc-langs.el, it is set to the union of a bunch of things, none
;; of which include "new", or "enum".
;;
;; But somehow both of those show up in the resulting derived regexp.
;; This breaks indentation of instance initializers, such as
;;
;;         var x = new Foo { ... };
;;
;; Based on my inspection, the existing c-lang-defconst should work!
;; I don't know how to fix this c-lang-defconst, so I am re-setting this
;; variable here, to provide the regex explicitly.
;;
(c-lang-defconst c-decl-block-key

  csharp '"\\(namespace\\)\\([^[:alnum:]_]\\|$\\)\\|\\(class\\|interface\\|struct\\)\\([^[:alnum:]_]\\|$\\)"
  )



;; Thu, 22 Apr 2010  14:29
;; I want this to handle    var x = new Foo[] { ... };
;; not sure if necessary.
(c-lang-defconst c-inexpr-brace-list-kwds
  csharp '("new"))


;; ;;(c-lang-defconst c-inexpr-class-kwds
;; ;; csharp '("new"))



(c-lang-defconst c-class-decl-kwds
  ;; EMCA-344, S?
  csharp '("class" "interface" "struct" ))  ;; no "enum"!!


;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  csharp '("public" "partial" "private" "const" "abstract"
           "protected" "ref" "out" "static" "virtual"
           "override" "params" "internal"))


;; Thu, 22 Apr 2010  23:02
;; Based on inspection of the cc-mode code, the c-protection-kwds
;; c-lang-const is used only for objective-c.  So the value is
;; irrelevant for csharp.
(c-lang-defconst c-protection-kwds
  csharp nil
  ;; csharp '("private" "protected" "public" "internal")
)


;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  csharp '("struct" "class" "interface" "is" "as"
           "delegate" "event" "set" "get" "add" "remove"))


;; This allows the classes after the : in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  csharp '(":"))

;; Sets up the enum to handle the list properly, and also the new
;; keyword to handle object initializers.  This requires a modified
;; c-basic-matchers-after (see above) in order to correctly fontify C#
;; 3.0 object initializers.
(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))


;; Statement keywords followed directly by a substatement.
;; catch is not one of them.
(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "try" "finally"))


;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "using"
           "checked" "unchecked" "lock"))


;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  csharp '("return" "continue" "break" "throw" "goto" ))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  csharp nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  csharp '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base"))

;; Treat namespace as an outer block so class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

(c-lang-defconst c-other-kwds
  csharp '("in" "sizeof" "typeof" "is" "as" "yield"
           "where" "select" "from"))

(c-lang-defconst c-overloadable-operators
  ;; EMCA-344, S14.2.1
  csharp '("+" "-" "*" "/" "%" "&" "|" "^"
           "<<" ">>" "==" "!=" ">" "<" ">=" "<="))


;; This c-cpp-matchers stuff is used for fontification.
;; see cc-font.el
;;

;; There's no preprocessor in C#, but there are still compiler
;; directives to fontify: "#pragma", #region/endregion, #define, #undef,
;; #if/else/endif.  (The definitions for the extra keywords above are
;; enough to incorporate them into the fontification regexps for types
;; and keywords, so no additional font-lock patterns are required for
;; keywords.)

(c-lang-defconst c-cpp-matchers
  csharp (cons
      ;; Use the eval form for `font-lock-keywords' to be able to use
      ;; the `c-preprocessor-face-name' variable that maps to a
      ;; suitable face depending on the (X)Emacs version.
      '(eval . (list "^\\s *\\(#pragma\\|undef\\|define\\)\\>\\(.*\\)"
                     (list 1 c-preprocessor-face-name)
                     '(2 font-lock-string-face)))
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers)))

(defcustom csharp-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in C# mode.
Each list item should be a regexp matching a single identifier."
  :type 'list :group 'csharp)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal highlighting for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defvar csharp-mode-syntax-table nil
  "Syntax table used in csharp-mode buffers.")
(or csharp-mode-syntax-table
    (setq csharp-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table csharp))))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in csharp-mode buffers.")
(c-define-abbrev-table 'csharp-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar csharp-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for C#
                      map)
  "Keymap used in csharp-mode buffers.")


;; TODO
;; Defines our constant for finding attributes.
;;(defconst csharp-attribute-regex "\\[\\([XmlType]+\\)(")
;;(defconst csharp-attribute-regex "\\[\\(.\\)")
;; This doesn't work because the string regex happens before this point
;; and getting the font-locking to work before and after is fairly difficult
;;(defconst csharp-attribute-regex
;;  (concat
;;   "\\[[a-zA-Z][ \ta-zA-Z0-9.]+"
;;   "\\((.*\\)?"
;;))


;; ==================================================================
;; end of c# values for "language constants" defined in cc-langs.el
;; ==================================================================




;; ==================================================================
;; C# code-doc insertion magic
;; ==================================================================
;;
;; In Visual Studio, if you type three slashes, it immediately expands into
;; an inline code-documentation fragment.  The following method does the
;; same thing.
;;
;; This is the kind of thing that could be handled by YASnippet or
;; another similarly flexible snippet framework. But I don't want to
;; introduce a dependency on yasnippet to csharp-mode. So the capability
;; must live within csharp-mode itself.

(defun csharp-maybe-insert-codedoc (arg)

  "Insert an xml code documentation template as appropriate, when
typing slashes.  This fn gets bound to / (the slash key), in
csharp-mode.  If the slash being inserted is not the third
consecutive slash, the slash is inserted as normal.  If it is the
third consecutive slash, then a xml code documentation template
may be inserted in some cases. For example,

  a <summary> template is inserted if the prior line is empty,
        or contains only an open curly brace;
  a <remarks> template is inserted if the prior word
        closes the <summary> element;
  a <returns> template is inserted if the prior word
        closes the <remarks> element;
  an <example> template is inserted if the prior word closes
        the <returns> element;
  a <para> template is inserted if the prior word closes
        a <para> element.

In all other cases the slash is inserted as normal.

If you want the default cc-mode behavior, which implies no automatic
insertion of xml code documentation templates, then use this in
your `csharp-mode-hook' function:

     (local-set-key (kbd \"/\") 'c-electric-slash)

 "
  (interactive "*p")
  ;;(message "csharp-maybe-insert-codedoc")
  (let (
        (cur-point (point))
        (char last-command-char)
        (cb0 (char-before (- (point) 0)))
        (cb1 (char-before (- (point) 1)))
        is-first-non-whitespace
        did-auto-insert
        )

    ;; check if two prior chars were slash
    (if (and
         (= char ?/)
         cb0 (= ?/ cb0)
         cb1 (= ?/ cb1)
         )

        (progn
          ;;(message "yes - this is the third consecutive slash")
          (setq is-first-non-whitespace
                (save-excursion
                  (back-to-indentation)
                  (= cur-point (+ (point) 2))))

          (if is-first-non-whitespace
              ;; This is a 3-slash sequence.  It is the first non-whitespace text
              ;; on the line. Now we need to examine the surrounding context
              ;; in order to determine which xml cod doc template to insert.
              (let (word-back char0 char1
                    word-fore char-0 char-1
                    text-to-insert         ;; text to insert in lieu of slash
                    fn-to-call     ;; func to call after inserting text
                    (preceding-line-is-empty (or
                                              (= (line-number-at-pos) 1)
                                              (save-excursion
                                               (previous-line)
                                               (beginning-of-line)
                                               (looking-at "[ \t]*$\\|[ \t]*{[ \t]*$"))))
                    (flavor 0) ;; used only for diagnostic purposes
                    )

                ;;(message "starting a 3-slash comment")
                ;; get the prior word, and the 2 chars preceding it.
                (backward-word)

                (setq word-back (thing-at-point 'word)
                      char0 (char-before (- (point) 0))
                      char1 (char-before (- (point) 1)))

                ;; restore prior position
                (goto-char cur-point)

                ;; get the following word, and the 2 chars preceding it.
                (forward-word)
                (backward-word)
                (setq word-fore (thing-at-point 'word)
                      char-0 (char-before (- (point) 0))
                      char-1 (char-before (- (point) 1)))

                ;; restore prior position again
                (goto-char cur-point)

                (cond
                 ;; The preceding line is empty, or all whitespace, or
                 ;; contains only an open-curly.  In this case, insert a
                 ;; summary element pair.
                 (preceding-line-is-empty
                  (setq text-to-insert  "/ <summary>\n///   \n/// </summary>"
                        flavor 1) )

                 ;; The preceding word closed a summary element.  In this case,
                 ;; if the forward word does not open a remarks element, then
                 ;; insert a remarks element.
                 ((and (string-equal word-back "summary") (eq char0 ?/)  (eq char1 ?<))
                  (if (not (and (string-equal word-fore "remarks") (eq char-0 ?<)))
                      (setq text-to-insert "/ <remarks>\n///   <para>\n///     \n///   </para>\n/// </remarks>"
                            flavor 2)))

                 ;; The preceding word closed the remarks section.  In this case,
                 ;; insert an example element.
                 ((and (string-equal word-back "remarks")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <example>\n///   \n/// </example>"
                        flavor 3))

                 ;; The preceding word closed the example section.  In this
                 ;; case, insert an returns element.  This isn't always
                 ;; correct, because sometimes the xml code doc is attached to
                 ;; a class or a property, neither of which has a return
                 ;; value. A more intelligent implementation would inspect the
                 ;; syntax state and only inject a returns element if
                 ;; appropriate.
                 ((and (string-equal word-back "example")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <returns></returns>"
                        fn-to-call (lambda ()
                                     (backward-word)
                                     (backward-char)
                                     (backward-char)
                                     (c-indent-line-or-region)
                                     )
                        flavor 4))

                 ;; The preceding word opened the remarks section, or it
                 ;; closed a para section. In this case, insert a para
                 ;; element, using appropriate indentation with respect to the
                 ;; prior tag.
                 ((or
                   (and (string-equal word-back "remarks")  (eq char0 ?<)  (or (eq char1 32) (eq char1 9)))
                   (and (string-equal word-back "para")     (eq char0 ?/)  (eq char1 ?<)))

                  (let (prior-point spacer)
                    (save-excursion
                      (backward-word)
                      (backward-char)
                      (backward-char)
                      (setq prior-point (point))
                      (skip-chars-backward "\t ")
                      (setq spacer (buffer-substring (point) prior-point))
                      ;;(message (format "pt(%d) prior(%d) spacer(%s)" (point) prior-point spacer))
                      )

                    (if (string-equal word-back "remarks")
                        (setq spacer (concat spacer "   ")))

                    (setq text-to-insert (format "/%s<para>\n///%s  \n///%s</para>"
                                                 spacer spacer spacer)
                          flavor 6)))

                 ;; The preceding word opened a para element.  In this case, if
                 ;; the forward word does not close the para element, then
                 ;; close the para element.
                 ;; --
                 ;; This is a nice idea but flawed.  Suppose I have a para element with some
                 ;; text in it. If I position the cursor at the first line, then type 3 slashes,
                 ;; I get a close-element, and that would be inappropriate.  Not sure I can
                 ;; easily solve that problem, so the best thing might be to simply punt, and
                 ;; require people to close their own elements.
                 ;;
                 ;;              ( (and (string-equal word-back "para")  (eq char0 60)  (or (eq char1 32) (eq char1 9)))
                 ;;                (if (not (and (string-equal word-fore "para") (eq char-0 47) (eq char-1 60) ))
                 ;;                    (setq text-to-insert "/   \n/// </para>\n///"
                 ;;                          fn-to-call (lambda ()
                 ;;                                       (previous-line)
                 ;;                                       (end-of-line)
                 ;;                                       )
                 ;;                          flavor 7) )
                 ;;                )

                 ;; the default case - do nothing
                 (t nil))

                (if text-to-insert
                    (progn
                      ;;(message (format "inserting special text (f(%d))" flavor))

                      ;; set the flag, that we actually inserted text
                      (setq did-auto-insert t)

                      ;; save point of beginning of insertion
                      (setq cur-point (point))

                      ;; actually insert the text
                      (insert text-to-insert)

                      ;; indent the inserted string, and re-position point, either through
                      ;; the case-specific fn, or via the default progn.
                      (if fn-to-call
                          (funcall fn-to-call)

                        (let ((newline-count 0) (pos 0) ix)

                          ;; count the number of newlines in the inserted string
                          (while (string-match "\n" text-to-insert pos)
                            (setq pos (match-end 0)
                                  newline-count (+ newline-count 1) )
                            )

                          ;; indent what we just inserted
                          (c-indent-region cur-point (point) t)

                          ;; move up n/2 lines. This assumes that the
                          ;; inserted text is ~symmetric about the halfway point.
                          ;; The assumption holds if the xml code doc uses a
                          ;; begin-elt and end-elt on a new line all by themselves,
                          ;; and a blank line in between them where the point should be.
                          ;; A more intelligent implementation would use a specific
                          ;; marker string, like @@DOT, to note the desired point.
                          (previous-line (/ newline-count 2))
                          (end-of-line)))))))))

    (if (not did-auto-insert)
        (self-insert-command (prefix-numeric-value arg)))))

;; ==================================================================
;; end of c# code-doc insertion magic
;; ==================================================================




;; ==================================================================
;; c# fontification extensions
;; ==================================================================
;; Commentary:
;;
;; The purpose of the following code is to fix font-lock for C#,
;; specifically for the verbatim-literal strings. C# is a cc-mode
;; language and strings are handled mostly like other c-based
;; languages. The one exception is the verbatim-literal string, which
;; uses the syntax @"...".
;;
;; `parse-partial-sexp' treats those strings as just regular strings,
;; with the @ a non-string character.  This is fine, except when the
;; verblit string ends in a slash, in which case, font-lock breaks from
;; that point onward in the buffer.
;;
;; This is an attempt to fix that.
;;
;; The idea is to scan the buffer in full for verblit strings, and apply the
;; appropriate syntax-table text properties for verblit strings. Also setting
;; `parse-sexp-lookup-properties' to t tells `parse-partial-sexp'
;; to use the syntax-table text properties set up by the scan as it does
;; its parse.
;;
;; Also need to re-scan after any changes in the buffer, but on a more
;; limited region.
;;


;; ;; I don't remember what this is supposed to do,
;; ;; or how I figured out the value.
;; ;;
;; (defconst csharp-font-lock-syntactic-keywords
;;   '(("\\(@\\)\\(\"\\)[^\"]*\\(\"\\)\\(\"\\)[^\"]*\\(\"\\)[^\"]"
;;      (1 '(6)) (2 '(7)) (3 '(1)) (4 '(1)) (5 '(7))
;;                  ))
;;   "Highlighting of verbatim literal strings. See also the variable
;;   `font-lock-keywords'.")



;; The following fn allows this:
;;    (csharp-log 3 "scan result...'%s'" state)

(defvar csharp-log-level 0
  "The current log level for CSharp-specific operations.
This is used in particular by the verbatim-literal
string scanning.

Most other csharp functions are not instrumented.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG, 4 = SHUTUP ALREADY. ")

(defun csharp-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `csharp-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level csharp-log-level)
      (let* ((msg (apply 'format text args)))
        (message "C#: %s" msg))
    t))



(defun csharp-max-beginning-of-stmt ()
  "Return the greater of `c-beginning-of-statement-1' and
`c-beginning-of-statement' .  I don't understand why both of
these methods are necessary or why they differ. But they do."

  (let (dash
        nodash
        (curpos (point)))

    ;; I think this may need a save-excursion...
    ;; Calling c-beginning-of-statement-1 resets the point!

    (setq dash (progn (c-beginning-of-statement-1) (point)))
    (csharp-log 3 "max-bostmt dash(%d)" dash)
    (goto-char curpos)

    (setq nodash (progn (c-beginning-of-statement 1) (point)))
    (csharp-log 3 "max-bostmt nodash(%d)" nodash)
    (goto-char curpos)

    (max dash nodash)))


(defun csharp-in-literal (&optional lim detect-cpp)
  "Return the type of literal point is in, if any.
Basically this works like `c-in-literal' except it doesn't
use or fill the cache (`c-in-literal-cache').

The return value is `c' if in a C-style comment, `c++' if in a C++
style comment, `string' if in a string literal, `pound' if DETECT-CPP
is non-nil and in a preprocessor line, or nil if somewhere else.
Optional LIM is used as the backward limit of the search.  If omitted,
or nil, `c-beginning-of-syntax' is used.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

  (let ((rtn
        (save-excursion
          (let* ((pos (point))
                 (lim (or lim (progn
                                (c-beginning-of-syntax)
                                (point))))
                 (state (parse-partial-sexp lim pos)))
            (csharp-log 4 "parse lim(%d) state: %s" lim (prin1-to-string state))
            (cond
             ((elt state 3)
              (csharp-log 4 "in literal string (%d)" pos)
              'string)
             ((elt state 4)
              (csharp-log 4 "in literal comment (%d)" pos)
              (if (elt state 7) 'c++ 'c))
             ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
             (t nil))))))
    rtn))


(defun csharp-set-vliteral-syntax-table-properties (beg end)
  "Scan the buffer text between BEG and END, a verbatim literal
string, setting and clearing syntax-table text properties where
necessary.

We need to modify the default syntax-table text property in these cases:
  (backslash)    - is not an escape inside a verbatim literal string.
  (double-quote) - can be a literal quote, when doubled.

BEG is the @ delimiter. END is the 'old' position of the ending quote.

see http://www.sunsite.ualberta.ca/Documentation/Gnu/emacs-lisp-ref-21-2.7/html_node/elisp_592.html
for the list of syntax table numeric codes.

"

  (csharp-log 3 "set-vlit-syntax-table:  beg(%d) end(%d)" beg end)

  (if (and (> beg 0) (> end 0))

      (let ((curpos beg)
            (state 0))

        (c-clear-char-properties beg end 'syntax-table)

        (while (<= curpos end)

          (cond
           ((= state 0)
            (if (= (char-after curpos) ?@)
                (progn
                  (c-put-char-property curpos 'syntax-table '(6)) ; (6) = expression prefix, (3) = symbol
                  ;;(message (format "set-s-t: prefix pos(%d) chr(%c)" beg (char-after beg)))
                  )
              )
            (setq state (+ 1 state)))

           ((= state 1)
            (if (= (char-after curpos) ?\")
                (progn
                  (c-put-char-property curpos 'syntax-table '(7)) ; (7) = string quote
                  ;;(message (format "set-s-t: open quote pos(%d) chr(%c)"
                  ;; curpos (char-after curpos)))
                  ))
            (setq state (+ 1 state)))

           ((= state 2)
            (cond
             ;; handle backslash inside the string
             ((= (char-after curpos) ?\\)
              (c-put-char-property curpos 'syntax-table '(2)) ; (1) = punctuation, (2) = word
              ;;(message (format "set-s-t: backslash word pos(%d) chr(%c)" curpos (char-after curpos)))
              )

             ;; doubled double-quote
             ((and
               (= (char-after curpos) ?\")
               (= (char-after (+ 1 curpos)) ?\"))
              (c-put-char-property curpos 'syntax-table '(2)) ; (1) = punctuation, (2) = word
              (c-put-char-property (+ 1 curpos) 'syntax-table '(2)) ; (1) = punctuation
              ;;(message (format "set-s-t: double doublequote pos(%d) chr(%c)" curpos (char-after curpos)))
              (setq curpos (+ curpos 1))
              )

             ;; a single double-quote, which should be a string terminator
             ((= (char-after curpos) ?\")
              (c-put-char-property curpos 'syntax-table '(7)) ; (7) = string quote
              ;;(message (format "set-s-t: close quote pos(%d) chr(%c)" curpos (char-after curpos)))
              ;;go no further
              (setq state (+ 1 state)))

             ;; everything else
             (t
              ;;(message (format "set-s-t: none pos(%d) chr(%c)" curpos (char-after curpos)))
              nil))))
          ;; next char
          (setq curpos (+ curpos 1))))))



(defun csharp-end-of-verbatim-literal-string (&optional lim)
  "Moves to and returns the position of the end quote of the verbatim literal
string.  When calling, point should be on the @ of the verblit string.
If it is not, then no movement is performed and `point' is returned.

This function ignores text properties. In fact it is the
underlying scanner used to set the text properties in a C# buffer.
"

  (csharp-log 3 "end-of-vlit-string: point(%d) c(%c)" (point) (char-after))

  (let (curpos
        (max (or lim (point-max))))

    (if (not (looking-at "@\""))
        (point)
    (forward-char 2) ;; pass up the @ sign and first quote
    (setq curpos (point))

    ;; Within a verbatim literal string, a doubled double-quote
    ;; escapes the double-quote."
    (while (and                                  ;; process characters...
            (or                                  ;; while...
             (not (eq (char-after curpos) ?\"))  ;; it's not a quote
             (eq (char-after (+ curpos 1)) ?\")) ;; or, its a double (double) quote
            (< curpos max))                      ;; and we're not done yet

      (cond
       ((and (eq (char-after curpos) ?\")        ;; it's a double-quote.
             (eq (char-after (+ curpos 1)) ?\"))
        (setq curpos (+ 2 curpos)))              ;; Skip 2
       (t                                        ;; anything else
        (setq curpos (+ 1 curpos)))))            ;; skip fwd 1
    curpos)))




(defun csharp-scan-for-verbatim-literals-and-set-props (&optional beg end)

"Scans the buffer, between BEG and END, for verbatim literal
strings, and sets override text properties on each string to
allow proper syntax highlighting, indenting, and cursor movement.

BEG and END define the limits of the scan.  When nil, they
default to `point-min' and `point-max' respectively.

Setting text properties generally causes the buffer to be marked
as modified, but this fn suppresses that via the
`c-buffer-save-state' macro, for any changes in text properties
that it makes.  This fn also ignores the read-only setting on a
buffer, using the same macro.

This fn is called when a csharp-mode buffer is loaded, with BEG
and END set to nil, to do a full scan.  It is also called on
every buffer change, with the BEG and END set to the values for
the change.

The return value is nil if the buffer was not a csharp-mode
buffer.  Otherwise it is the last cursor position examined by the
scan.
"

  (if (not (c-major-mode-is 'csharp-mode)) ;; don't scan if not csharp mode
      nil
    (save-excursion
      (c-save-buffer-state
          ((curpos (or beg (point-min)))
           (lastpos (or end (point-max)))
           (state 0) (start 0) (cycle 0)
           literal eos limits)

        (csharp-log 3 "scan")
        (goto-char curpos)

        (while (and (< curpos lastpos) (< cycle 10000))
          (cond

           ;; Case 1: current char is a @ sign
           ;; --------------------------------------------
           ;; Check to see if it demarks the beginning of a verblit
           ;; string.
           ((= ?@ (char-after curpos))

            ;; are we in a comment?   a string?  Maybe the @ is a prefix
            ;; to allow the use of a reserved word as a symbol. Let's find out.

            ;; not sure why I need both of the following.
            (syntax-ppss-flush-cache 1)
            (parse-partial-sexp 1 curpos)
            (goto-char curpos)
            (setq literal (csharp-in-literal))
            (cond

             ;; Case 1.A: it's a @ within a string.
             ;; --------------------------------------------
             ;; This should never happen, because this scanner hops over strings.
             ;; But it might happen if the scan starts at an odd place.
             ((eq literal 'string) nil)

             ;; Case 1.B: The @ is within a comment.  Hop over it.
             ((and (memq literal '(c c++))
                   ;; This is a kludge for XEmacs where we use
                   ;; `buffer-syntactic-context', which doesn't correctly
                   ;; recognize "\*/" to end a block comment.
                   ;; `parse-partial-sexp' which is used by
                   ;; `c-literal-limits' will however do that in most
                   ;; versions, which results in that we get nil from
                   ;; `c-literal-limits' even when `c-in-literal' claims
                   ;; we're inside a comment.
                   ;;(setq limits (c-literal-limits start)))
                   (setq limits (c-literal-limits)))

              ;; advance to the end of the comment
              (if limits
                  (progn
                    (csharp-log 4 "scan: jump end comment A (%d)" (cdr limits))
                    (setq curpos (cdr limits)))))


             ;; Case 1.B: curpos is at least 2 chars before the last
             ;; position to examine, and, the following char is a
             ;; double-quote (ASCII 34).
             ;; --------------------------------------------
             ;; This looks like the beginning of a verbatim string
             ;; literal.
             ((and (< (+ 2 curpos) lastpos)
                   (= ?\" (char-after (+ 1 curpos))))

              (setq eos (csharp-end-of-verbatim-literal-string))
              ;; set override syntax properties on the verblit string
              (csharp-set-vliteral-syntax-table-properties curpos eos)

              (csharp-log 4 "scan: jump end verblit string (%d)" eos)
              (setq curpos eos))))


           ;; Case 2: current char is a double-quote.
           ;; --------------------------------------------
           ;; If this is a string, we hop over it, on the assumption that
           ;; this scanner need not bother with regular literal strings, which
           ;; get the proper syntax with the generic approach.
           ;; If in a comment, hop over the comment.
           ((= ?\" (char-after curpos))
            (goto-char curpos)
            (setq literal (c-in-literal))
            (cond

             ;; Case 2.A: a quote within a string
             ;; --------------------------------------------
             ;; This shouldn't happen, because we hop over strings.
             ;; But it might.
             ((eq literal 'string) nil)

             ;; Case 2.B: a quote within a comment
             ;; --------------------------------------------
             ((and (memq literal '(c c++))
                   ;; This is a kludge for XEmacs where we use
                   ;; `buffer-syntactic-context', which doesn't correctly
                   ;; recognize "\*/" to end a block comment.
                   ;; `parse-partial-sexp' which is used by
                   ;; `c-literal-limits' will however do that in most
                   ;; versions, which results in that we get nil from
                   ;; `c-literal-limits' even when `c-in-literal' claims
                   ;; we're inside a comment.
                   ;;(setq limits (c-literal-limits start)))
                   (setq limits (c-literal-limits)))

              ;; advance to the end of the comment
              (if limits
                  (progn
                    (setq curpos (cdr limits))
                    (csharp-log 3 "scan: jump end comment B (%s)" curpos))))


             ;; Case 2.C: Not in a comment, and not in a string.
             ;; --------------------------------------------
             ;; This is the beginning of a literal (but not verbatim) string.
             (t
              (forward-char 1) ;; pass up the quote
              (if (consp (setq limits (c-literal-limits)))
                  (progn
                    (csharp-log 4 "scan: jump end literal (%d)" (cdr limits))
                    (setq curpos (cdr limits))))))))

          (setq cycle (+ 1 cycle))
          (setq curpos (+ 1 curpos))
          (c-safe (goto-char curpos)))))))


(defun csharp-before-font-lock (beg end old-len)
  "Adjust`syntax-table' properties on the region affected by the change
in a csharp-mode buffer.

This function is the C# value for `c-before-font-lock-function'.
It intended to be called only by the cc-mode runtime.

It prepares the buffer for font locking, hence must get called
before `font-lock-after-change-function'.

It does hidden buffer changes.

BEG, END and OLD-LEN have the same meaning here as for any
after-change function.

Point is undefined both before and after this function call.
The return value is meaningless, and is ignored by cc-mode.
"
    (let ((start-scan (progn
                        (c-beginning-of-statement 1)
                        (point))))
      (csharp-scan-for-verbatim-literals-and-set-props start-scan end)))



(c-lang-defconst c-before-font-lock-function
  csharp 'csharp-before-font-lock)

;; ==================================================================
;; end of c# fontification extensions
;; ==================================================================





;; ==================================================================
;; C#-specific optimizations of cc-mode funcs
;; ==================================================================



;; There's never a need to move over an Obj-C directive in csharp-mode.
(defadvice c-forward-objc-directive (around
                                 csharp-mode-advice-2
                                 compile activate)
  (if (c-major-mode-is 'csharp-mode)
      nil
    ad-do-it)
  )

;; ==================================================================
;; end of C#-specific optimizations of cc-mode funcs
;; ==================================================================








;; ==================================================================
;; c# - monkey-patching of basic parsing logic
;; ==================================================================
;;
;; The following 2 defuns redefine functions from cc-mode, to add
;; special cases for C#.  These primarily deal with indentation of
;; instance initializers, which are somewhat unique to C#.  I couldn't
;; figure out how to get cc-mode to do what C# needs, without modifying
;; these defuns.
;;

(defun c-looking-at-inexpr-block (lim containing-sexp &optional check-at-end)
  ;; Return non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.
  ;;
  ;; LIM limits the backward search.  CONTAINING-SEXP is the start
  ;; position of the closest containing list.  If it's nil, the
  ;; containing paren isn't used to decide whether we're inside an
  ;; expression or not.  If both LIM and CONTAINING-SEXP are used, LIM
  ;; needs to be farther back.
  ;;
  ;; If CHECK-AT-END is non-nil then extra checks at the end of the
  ;; brace block might be done.  It should only be used when the
  ;; construct can be assumed to be complete, i.e. when the original
  ;; starting position was further down than that.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (let ((res 'maybe) passed-paren
          (closest-lim (or containing-sexp lim (point-min)))
          ;; Look at the character after point only as a last resort
          ;; when we can't disambiguate.
          (block-follows (and (eq (char-after) ?{) (point))))

      (while (and (eq res 'maybe)
                  (progn (c-backward-syntactic-ws)
                         (> (point) closest-lim))
                  (not (bobp))
                  (progn (backward-char)
                         (looking-at "[\]\).]\\|\\w\\|\\s_"))
                  (c-safe (forward-char)
                          (goto-char (scan-sexps (point) -1))))

        (setq res
              (if (looking-at c-keywords-regexp)
                  (let ((kw-sym (c-keyword-sym (match-string 1))))
                    (cond
                     ((and block-follows
                           (c-keyword-member kw-sym 'c-inexpr-class-kwds))
                      (and (not (eq passed-paren ?\[))

                           ;; dinoch Thu, 22 Apr 2010  18:20
                           ;; ============================================
                           ;; looking at new MyType() { ... }
                           ;; means this is a brace list, so, return nil,
                           ;; implying NOT looking-at-inexpr-block
                           (not
                            (and (c-major-mode-is 'csharp-mode)
                                 (looking-at "new\s+\\([[:alnum:]_]+\\)\\b")))

                           (or (not (looking-at c-class-key))
                               ;; If the class instantiation is at the start of
                               ;; a statement, we don't consider it an
                               ;; in-expression class.
                               (let ((prev (point)))
                                 (while (and
                                         (= (c-backward-token-2 1 nil closest-lim) 0)
                                         (eq (char-syntax (char-after)) ?w))
                                   (setq prev (point)))
                                 (goto-char prev)
                                 (not (c-at-statement-start-p)))
                               ;; Also, in Pike we treat it as an
                               ;; in-expression class if it's used in an
                               ;; object clone expression.
                               (save-excursion
                                 (and check-at-end
                                      (c-major-mode-is 'pike-mode)
                                      (progn (goto-char block-follows)
                                             (zerop (c-forward-token-2 1 t)))
                                      (eq (char-after) ?\())))
                           (cons 'inexpr-class (point))))
                     ((c-keyword-member kw-sym 'c-inexpr-block-kwds)
                      (when (not passed-paren)
                        (cons 'inexpr-statement (point))))
                     ((c-keyword-member kw-sym 'c-lambda-kwds)
                      (when (or (not passed-paren)
                                (eq passed-paren ?\())
                        (cons 'inlambda (point))))
                     ((c-keyword-member kw-sym 'c-block-stmt-kwds)
                      nil)
                     (t
                      'maybe)))

                (if (looking-at "\\s(")
                    (if passed-paren
                        (if (and (eq passed-paren ?\[)
                                 (eq (char-after) ?\[))
                            ;; Accept several square bracket sexps for
                            ;; Java array initializations.
                            'maybe)
                      (setq passed-paren (char-after))
                      'maybe)
                  'maybe))))

      (if (eq res 'maybe)
          (when (and c-recognize-paren-inexpr-blocks
                     block-follows
                     containing-sexp
                     (eq (char-after containing-sexp) ?\())
            (goto-char containing-sexp)
            (if (or (save-excursion
                      (c-backward-syntactic-ws lim)
                      (and (> (point) (or lim (point-min)))
                           (c-on-identifier)))
                    (and c-special-brace-lists
                         (c-looking-at-special-brace-list)))
                nil
              (cons 'inexpr-statement (point))))

        res))))





(defun c-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  PAREN-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; placed in inconvenient locations.  It's a trade-off we make for
  ;; speed.
  ;;
  ;; This function might do hidden buffer changes.
  (or
   ;; This will pick up brace list declarations.
   (c-safe
    (save-excursion
      (goto-char containing-sexp)
      (c-safe (c-forward-sexp -1))
      (let (bracepos)
        (if (and (or (looking-at c-brace-list-key)

                     (progn
                       (c-safe (c-forward-sexp -1))
                       (looking-at c-brace-list-key))

                     ;; dinoch Thu, 22 Apr 2010  18:20
                     ;; ============================================
                     ;; looking enum Foo : int
                     ;; means this is a brace list, so, return nil,
                     ;; implying NOT looking-at-inexpr-block

                     (and (c-major-mode-is 'csharp-mode)
                          (progn
                            (c-safe (c-forward-sexp -1))
                            (looking-at csharp-enum-decl-re))))

                 (setq bracepos (c-down-list-forward (point)))
                 (not (c-crosses-statement-barrier-p (point)
                                                     (- bracepos 2))))
            (point)))))

   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
            ;; Pike can have class definitions anywhere, so we must
            ;; check for the class key here.
            (and (c-major-mode-is 'pike-mode)
                 c-decl-block-key))
           bufpos braceassignp lim next-containing)
       (while (and (not bufpos)
                   containing-sexp)
           (when paren-state
             (if (consp (car paren-state))
                 (setq lim (cdr (car paren-state))
                       paren-state (cdr paren-state))
               (setq lim (car paren-state)))
             (when paren-state
               (setq next-containing (car paren-state)
                     paren-state (cdr paren-state))))
           (goto-char containing-sexp)
           (if (c-looking-at-inexpr-block next-containing next-containing)
               ;; We're in an in-expression block of some kind.  Do not
               ;; check nesting.  We deliberately set the limit to the
               ;; containing sexp, so that c-looking-at-inexpr-block
               ;; doesn't check for an identifier before it.
               (setq containing-sexp nil)
             ;; see if the open brace is preceded by = or [...] in
             ;; this statement, but watch out for operator=
             (setq braceassignp 'dontknow)
             (c-backward-token-2 1 t lim)
             ;; Checks to do only on the first sexp before the brace.
             (when (and c-opt-inexpr-brace-list-key
                        (eq (char-after) ?\[))
               ;; In Java, an initialization brace list may follow
               ;; directly after "new Foo[]", so check for a "new"
               ;; earlier.
               (while (eq braceassignp 'dontknow)
                 (setq braceassignp
                       (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
                             ((looking-at c-opt-inexpr-brace-list-key) t)
                             ((looking-at "\\sw\\|\\s_\\|[.[]")
                              ;; Carry on looking if this is an
                              ;; identifier (may contain "." in Java)
                              ;; or another "[]" sexp.
                              'dontknow)
                             (t nil)))))
             ;; Checks to do on all sexps before the brace, up to the
             ;; beginning of the statement.
             (while (eq braceassignp 'dontknow)
               (cond ((eq (char-after) ?\;)
                      (setq braceassignp nil))
                     ((and class-key
                           (looking-at class-key))
                      (setq braceassignp nil))
                     ((eq (char-after) ?=)
                      ;; We've seen a =, but must check earlier tokens so
                      ;; that it isn't something that should be ignored.
                      (setq braceassignp 'maybe)
                      (while (and (eq braceassignp 'maybe)
                                  (zerop (c-backward-token-2 1 t lim)))
                        (setq braceassignp
                              (cond
                               ;; Check for operator =
                               ((and c-opt-op-identifier-prefix
                                     (looking-at c-opt-op-identifier-prefix))
                                nil)
                               ;; Check for `<opchar>= in Pike.
                               ((and (c-major-mode-is 'pike-mode)
                                     (or (eq (char-after) ?`)
                                         ;; Special case for Pikes
                                         ;; `[]=, since '[' is not in
                                         ;; the punctuation class.
                                         (and (eq (char-after) ?\[)
                                              (eq (char-before) ?`))))
                                nil)
                               ((looking-at "\\s.") 'maybe)
                               ;; make sure we're not in a C++ template
                               ;; argument assignment
                               ((and
                                 (c-major-mode-is 'c++-mode)
                                 (save-excursion
                                   (let ((here (point))
                                         (pos< (progn
                                                 (skip-chars-backward "^<>")
                                                 (point))))
                                     (and (eq (char-before) ?<)
                                          (not (c-crosses-statement-barrier-p
                                                pos< here))
                                          (not (c-in-literal))
                                          ))))
                                nil)
                               (t t))))))
               (if (and (eq braceassignp 'dontknow)
                        (/= (c-backward-token-2 1 t lim) 0))
                   (setq braceassignp nil)))
             (if (not braceassignp)
                 (if (eq (char-after) ?\;)
                     ;; Brace lists can't contain a semicolon, so we're done.
                     (setq containing-sexp nil)
                   ;; Go up one level.
                   (setq containing-sexp next-containing
                         lim nil
                         next-containing nil))
               ;; we've hit the beginning of the aggregate list
               (c-beginning-of-statement-1
                (c-most-enclosing-brace paren-state))
               (setq bufpos (point))))
           )
       bufpos))
   ))

;; ==================================================================
;; end of monkey-patching of basic parsing logic
;; ==================================================================




;;(easy-menu-define csharp-menu csharp-mode-map "C# Mode Commands"
;;                ;; Can use `csharp' as the language for `c-mode-menu'
;;                ;; since its definition covers any language.  In
;;                ;; this case the language is used to adapt to the
;;                ;; nonexistence of a cpp pass and thus removing some
;;                ;; irrelevant menu alternatives.
;;                (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))



(c-add-style "C#"
 '("Java"
   (c-basic-offset . 4)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . (
       (access-label          . -)
       (arglist-close         . c-lineup-arglist)
       (arglist-cont          . 0)
       (arglist-cont-nonempty . c-lineup-arglist)
       (arglist-intro         . c-lineup-arglist-intro-after-paren)
       (block-close           . 0)
       (block-open            . 0)
       (brace-entry-open      . 0)
       (brace-list-close      . 0)
       (brace-list-entry      . 0)
       (brace-list-intro      . +)
       (brace-list-open       . +)
       (c                     . c-lineup-C-comments)
       (case-label            . +)
       (catch-clause          . 0)
       (class-close           . 0)
       (class-open            . 0)
       (comment-intro         . c-lineup-comment)
       (cpp-macro             . 0)
       (cpp-macro-cont        . c-lineup-dont-change)
       (defun-block-intro     . +)
       (defun-close           . 0)
       (defun-open            . 0)
       (do-while-closure      . 0)
       (else-clause           . 0)
       (extern-lang-close     . 0)
       (extern-lang-open      . 0)
       (friend                . 0)
       (func-decl-cont        . +)
       (inclass               . +)
       (inexpr-class          . +)
       (inexpr-statement      . 0)
       (inextern-lang         . +)
       (inher-cont            . c-lineup-multi-inher)
       (inher-intro           . +)
       (inlambda              . c-lineup-inexpr-block)
       (inline-close          . 0)
       (inline-open           . 0)
       (innamespace           . +)
       (knr-argdecl           . 0)
       (knr-argdecl-intro     . 5)
       (label                 . 0)
       (lambda-intro-cont     . +)
       (member-init-cont      . c-lineup-multi-inher)
       (member-init-intro     . +)
       (namespace-close       . 0)
       (namespace-open        . 0)
       (statement             . 0)
       (statement-block-intro . +)
       (statement-case-intro  . +)
       (statement-case-open   . +)
       (statement-cont        . +)
       (stream-op             . c-lineup-streamop)
       (string                . c-lineup-dont-change)
       (substatement          . +)
       (substatement-open     . 0)
       (template-args-cont c-lineup-template-args +)
       (topmost-intro         . 0)
       (topmost-intro-cont    . +)
       ))
   ))




;; Custom variables
;;;###autoload
(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'c)



;;; The entry point into the mode
;;;###autoload
(defun csharp-mode ()
  "Major mode for editing C# code. This mode is derived from CC Mode to
support C#.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

This mode will automatically add a symbol and regexp to the
`compilation-error-regexp-alist' and `compilation-error-regexp-alist-alist'
respectively, for Csc.exe error and warning messages.

Key bindings:
\\{csharp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (c-initialize-cc-mode t)
  (set-syntax-table csharp-mode-syntax-table)

  ;; define underscore as part of a word in the Csharp syntax table
  (modify-syntax-entry ?_ "w" csharp-mode-syntax-table)

  ;; define @ as an expression prefix in Csharp syntax table
  (modify-syntax-entry ?@ "'" csharp-mode-syntax-table)

  (setq major-mode 'csharp-mode
        mode-name "C#"
        local-abbrev-table csharp-mode-abbrev-table
        abbrev-mode t)
  (use-local-map csharp-mode-map)

  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars csharp-mode)


  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'csharp-mode)

  ;; csc.exe, the C# Compiler, produces errors like this:
  ;; file.cs(6,18): error CS1006: Name of constructor must match name of class
  (if (boundp 'compilation-error-regexp-alist-alist)
      (progn
        (add-to-list
         'compilation-error-regexp-alist-alist
         '(ms-csharp "^[ \t]*\\([A-Za-z0-9_][^(]*\\.cs\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: \\(error\\|warning\\) CS[0-9]+:" 1 2 3))
        (add-to-list
         'compilation-error-regexp-alist
         'ms-csharp)))


  ;; to allow next-error to work with csc.exe:
  (setq compilation-scroll-output t)


  (local-set-key (kbd "/") 'csharp-maybe-insert-codedoc)
  (local-set-key (kbd "{") 'csharp-insert-open-brace)


  ;; Need the following for parse-partial-sexp to work properly with
  ;; verbatim literal strings Setting this var to non-nil tells
  ;; `parse-partial-sexp' to pay attention to the syntax text
  ;; properties on the text in the buffer.  If csharp-mode attaches
  ;; text syntax to @"..." then, `parse-partial-sexp' will treat those
  ;; strings accordingly.
  (set (make-local-variable 'parse-sexp-lookup-properties)
       t)

  ;; scan the entire buffer for verblit strings
  (csharp-scan-for-verbatim-literals-and-set-props nil nil)

  (c-run-mode-hooks 'c-mode-common-hook 'csharp-mode-hook)

  ;; Allow fill-paragraph to work on xml code doc
  ;; This setting gets overwritten quietly by c-run-mode-hooks,
  ;; so I put it afterwards to make it stick.
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
       "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

  ;;(message "C#: set paragraph-separate")

  ;; Speedbar handling
  (if (fboundp 'speedbar-add-supported-extension)
      (speedbar-add-supported-extension '(".cs"))) ;; idempotent

  (c-update-modeline))



(message  (concat "Done loading " load-file-name))


(provide 'csharp-mode)

;;; csharp-mode.el ends here
