;;; mmm-sample.el --- Sample MMM submode classes

;; Copyright (C) 2003, 2004 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains several sample submode classes for use with MMM
;; Mode. For a more detailed, advanced example, see `mmm-mason.el'.

;; In order to use any of classes defined here, just require `mmm-auto' and
;; add the respective (major mode -> class <- file extension) associations
;; with `mmm-add-mode-ext-class'.

;;; Code:

(require 'cl)
(require 'mmm-auto)
(require 'mmm-vars)

;;{{{ <Perl> in httpd.conf

;; This is the simplest example. Many applications will need no more
;; than a simple regexp.
;;
;; Usage: (mmm-add-mode-ext-class 'apache-generic-mode nil 'httpd-conf-perl)

(mmm-add-classes
 '((httpd-conf-perl
    :submode perl
    :delimiter-mode nil
    :front "<Perl>"
    :back "</Perl>")))

;;}}}
;;{{{ JavaScript in HTML

;; We use two classes here, both for code in a <script> tag, one wrapped in
;; CDATA, another not. And another class to group them together.
;;
;; Usage: (mmm-add-mode-ext-class 'html-mode nil 'html-js)

(mmm-add-group
 'html-js
 '((js-script-cdata
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
   (js-script
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                 @ "" _ "" @ "\n</script>" @)))))

;;}}}
;;{{{ CSS in HTML

(mmm-add-group
 'html-css
 '((css-cdata
    :submode css
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>")
   (css
    :submode css
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?c css-tag nil @ "<style type=\"text/css\">\n"
                 @ "" _ "" @ "\n</style>" @)))))

;;}}}
;;{{{ Here-documents

;; Here we match the here-document syntax used by Perl and shell
;; scripts.  We try to be automagic about recognizing what mode the
;; here-document should be in.  To make sure that it is recognized
;; correctly, the name of the mode, perhaps minus `-mode', in upper
;; case, and/or with hyphens converted to underscores, should be
;; separated from the rest of the here-document name by hyphens or
;; underscores.

(defvar mmm-here-doc-mode-alist '()
  "Alist associating here-document name regexps to submodes.
Normally, this variable is unnecessary, as the `here-doc' submode
class tries to automagically recognize the right submode.  If you use
here-document names that it doesn't recognize, however, then you can
add elements to this alist.  Each element is \(REGEXP . MODE) where
REGEXP is a regular expression matched against the here-document name
and MODE is a major mode function symbol.")

(defun mmm-here-doc-get-mode (string)
  (string-match "[a-zA-Z_-]+" string)
  (setq string (match-string 0 string))
  (or (mmm-ensure-modename
       ;; First try the user override variable.
       (some #'(lambda (pair)
                (if (string-match (car pair) string) (cdr pair) nil))
             mmm-here-doc-mode-alist))
      (let ((words (split-string (downcase string) "[_-]+")))
        (or (mmm-ensure-modename
             ;; Try the whole name, stopping at "mode" if present.
             (intern
              (mapconcat #'identity
                         (nconc (ldiff words (member "mode" words))
                                (list "mode"))
                         "-")))
            ;; Try each word by itself (preference list)
            (some #'(lambda (word)
                      (mmm-ensure-modename (intern word)))
                  words)
            ;; Try each word with -mode tacked on
            (some #'(lambda (word)
                      (mmm-ensure-modename
                       (intern (concat word "-mode"))))
                  words)
            ;; Try each pair of words with -mode tacked on
            (loop for (one two) on words
                  if (mmm-ensure-modename
                      (intern (concat one two "-mode")))
                  return it)
            ;; I'm unaware of any modes whose names, minus `-mode',
            ;; are more than two words long, and if the entire mode
            ;; name (perhaps minus `-mode') doesn't occur in the
            ;; here-document name, we can give up.
            (signal 'mmm-no-matching-submode nil)))))

(mmm-add-classes
 '((here-doc
    :front "<<[\"\'\`]?\\([a-zA-Z0-9_-]+\\)"
    :front-offset (end-of-line 1)
    :back "^~1$"
    :save-matches 1
    :delimiter-mode nil
    :match-submode mmm-here-doc-get-mode
    :insert ((?d here-doc "Here-document Name: " @ "<<" str _ "\n"
                 @ "\n" @ str "\n" @))
    )))

;;}}}
;;{{{ Embperl

(mmm-add-group
 'embperl
 '((embperl-perl
    :submode perl
    :front "\\[\\([-\\+!\\*\\$]\\)"
    :back "~1\\]"
    :save-matches 1
    :match-name "embperl"
    :match-face (("[+" . mmm-output-submode-face)
                 ("[-" . mmm-code-submode-face)
                 ("[!" . mmm-init-submode-face)
                 ("[*" . mmm-code-submode-face)
                 ("[$" . mmm-special-submode-face))
    :insert ((?p embperl "Region Type (Character): " @ "[" str
                 @ " " _ " " @ str "]" @)
             (?+ embperl+ ?p . "+")
             (?- embperl- ?p . "-")
             (?! embperl! ?p . "!")
             (?* embperl* ?p . "*")
             (?$ embperl$ ?p . "$")
             )
    )
   (embperl-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "\\[#"
    :back "#\\]"
    :insert ((?# embperl-comment nil @ "[#" @ " " _ " " @ "#]" @))
    )))

;;}}}
;;{{{ ePerl

(mmm-add-group
 'eperl
 '((eperl-expr
    :submode perl
    :face mmm-output-submode-face
    :front "<:="
    :back ":>"
    :insert ((?= eperl-expr nil @ "<:=" @ " " _ " " @ ":>" @)))
   (eperl-code
    :submode perl
    :face mmm-code-submode-face
    :front "<:"
    :back "_?:>"
    :match-name "eperl"
    :insert ((?p eperl-code nil @ "<:" @ " " _ " " @ ":>" @)
             (?: eperl-code ?p . nil)
             (?_ eperl-code_ nil @ "<:" @ " " _ " " @ "_:>" @)))
   (eperl-comment
    :submode text
    :face mmm-comment-submode-face
    :front ":>//"
    :back "\n")
   ))

;;}}}
;;{{{ File Variables

;; This submode class puts file local variable values, specified with
;; a `Local Variables:' line as in (emacs)File Variables, into Emacs
;; Lisp Mode.  It is a good candidate to put in `mmm-global-classes'.

(defun mmm-file-variables-verify ()
  ;; It would be nice to cache this somehow, which could be done in a
  ;; buffer-local variable with markers for positions, but the trick
  ;; is knowing when to expire the cache.
  (let ((bounds
         (save-excursion
           (save-match-data
             (goto-char (point-max))
             (backward-page)
             (and (re-search-forward "^\\(.*\\)Local Variables:" nil t)
                  (list (match-string 1)
                        (progn (end-of-line) (point))
                        (and (search-forward
                              (format "%sEnd:" (match-string 1))
                              nil t)
                             (progn (beginning-of-line)
                                    (point)))))))))
    (and bounds (caddr bounds)
         (save-match-data
           (string-match (format "^%s" (regexp-quote (car bounds)))
                         (match-string 0)))
         (> (match-beginning 0) (cadr bounds))
         (< (match-end 0) (caddr bounds)))))

(defun mmm-file-variables-find-back (bound)
  (forward-sexp)
  (if (> (point) bound)
      nil
    (looking-at "")))

(mmm-add-classes
 '((file-variables
    :front ".+:"
    :front-verify mmm-file-variables-verify
    :back mmm-file-variables-find-back
    :submode emacs-lisp-mode
    :delimiter-mode nil
    )))

;;}}}
;;{{{ JSP Pages

(mmm-add-group 'jsp
 `((jsp-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "<%--"
    :back "--%>"
    :insert ((?- jsp-comment nil @ "<%--" @ " " _ " " @ "--%>" @))
    )
   (jsp-code
    :submode java
    :match-face (("<%!" . mmm-declaration-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[!=]?"
    :back "%>"
    :match-name "jsp"
    :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
             (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
             (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (jsp-directive
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
    )))

;;}}}
;;{{{ SGML DTD

;; Thanks to Yann Dirson <ydirson@fr.alcove.com> for writing and
;; contributing this submode class.

(mmm-add-classes
 '((sgml-dtd
    :submode dtd-mode
    :face mmm-declaration-submode-face
    :delimiter-mode nil
    :front "<! *doctype[^>[]*\\["
    :back "]>")))

;;}}}
;;{{{ PHP in HTML

(mmm-add-group 'html-php
 '((html-php-output
    :submode php-mode
    :face mmm-output-submode-face
    :front "<\\?php *echo "
    :back "\\(\\?>\\|\\'\\)"
    :include-front t
    :front-offset 5
    :insert ((?e php-echo nil @ "<?php" @ " echo " _ " " @ "?>" @))
    )
   (html-php-code
    :submode php-mode
    :face mmm-code-submode-face
    :front "<\\?\\(php\\)?"
    :back "\\(\\?>\\|\\'\\)"
    :insert ((?p php-section nil @ "<?php" @ " " _ " " @ "?>" @)
             (?b php-block nil @ "<?php" @ "\n" _ "\n" @ "?>" @))
    )))

;;}}}

;; NOT YET UPDATED
;;{{{ HTML in PL/SQL;-COM-
;-COM-
;-COM-;; This one is the most complex example. In PL/SQL, HTML is generally
;-COM-;; output as a (single quote delimited) string inside a call to htp.p or
;-COM-;; its brethren. The problem is that there may be strings outside of
;-COM-;; htp.p calls that should not be HTML, so we need to only look inside
;-COM-;; these calls. The situation is complicated by PL/SQL's rule that two
;-COM-;; sequential single quotes in a string mean to put a single quote
;-COM-;; inside the string.
;-COM-
;-COM-;; These functions have not been thoroughly tested, and always search
;-COM-;; the entire buffer, ignoring START and END.
;-COM-
;-COM-(defun mmm-html-in-plsql (start end)
;-COM-  (save-match-data
;-COM-    (let ((case-fold-search t))
;-COM-      (and (re-search-forward "htp.p\\(\\|rn\\|rint\\)1?(" nil t)
;-COM-           (mmm-html-in-plsql-in-htp
;-COM-            ;; Find the end of the procedure call
;-COM-            (save-excursion (forward-char -1) (forward-sexp) (point))
;-COM-            start end)))))
;-COM-
;-COM-(defun mmm-html-in-plsql-in-htp (htp-end start end)
;-COM-  (let (beg end)
;-COM-    (or (and (re-search-forward "'" htp-end 'limit)
;-COM-	     (setf beg (match-end 0))
;-COM-	     ;; Find an odd number of 's to end the string.
;-COM-	     (do ((lgth 0 (length (match-string 0))))
;-COM-		 ((oddp lgth) t)
;-COM-	       (re-search-forward "'+" nil t))
;-COM-	     (setf end (1- (match-end 0)))
;-COM-	     (cons (cons beg end)
;-COM-		   (mmm-html-in-plsql-in-htp htp-end start end)))
;-COM-	;; No more strings in the procedure call; look for another.
;-COM-	(and (eql (point) htp-end)
;-COM-	     (mmm-html-in-plsql start end)))))
;-COM-
;-COM-(add-to-list 'mmm-classes-alist
;-COM-  '(htp-p (:function html-mode mmm-html-in-plsql)))
;-COM-
;;}}}

(provide 'mmm-sample)

;;; mmm-sample.el ends here
