;;; ensime-completion-util.el
;;
;;;; License
;;
;;     Copyright (C) 2015 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'scala-mode-syntax)
(require 'dash)

;; In order to efficiently and accurately match completion prefixes, we construct
;; a regular expression which matches scala identifiers in reverse.
;;
;; For referece, Scala identifier syntax (SLS 2.11)
;; -------------------------------
;; op       ::=  opchar {opchar}
;; varid    ::=  lower idrest
;; plainid  ::=  upper idrest |  varid |  op
;; id       ::=  plainid | ‘`’ stringLiteral ‘`’
;; idrest   ::=  {letter | digit} [‘_’ op]
;;
(defconst ensime--rev-idrest-re
  (concat
   "\\(" scala-syntax:op-re "_+" "\\|" "_" "\\)?"
   "\\(" "[" scala-syntax:letter-group scala-syntax:digit-group "]+" "_?" "\\)*"
   ))
(defconst ensime--rev-alphaid-re
  ;; '$' intentionally omitted, as user identifiers should not include it, and
  ;; it breaks completion of interpolated vars in strings.
  (concat "\\(" ensime--rev-idrest-re
	  "[" "[:lower:]" "[:upper:]" "_" "]" "\\)"))
(defconst ensime--rev-plainid-re
  (concat "\\(" ensime--rev-alphaid-re "\\|" scala-syntax:op-re "\\)"))
(defconst ensime--rev-quotedid-re "`[^`\n\r]+`")
(defconst ensime--rev-id-re
  (concat "^" "\\(" ensime--rev-quotedid-re "\\|" ensime--rev-plainid-re "\\)"))
(defconst ensime--prefix-char-class
  (concat "["
	  "`"
	  scala-syntax:letterOrDigit-group
	  scala-syntax:opchar-group
	  "]"))

(defun ensime--annotate-completions (completions)
  "Maps plist structures to propertized strings that will survive
 being passed through the innards of auto-complete or company."
  (mapcar
   (lambda (completion)
     (ensime-plist-bind
      (type-info to-insert name)
      completion
      (propertize name
                  'type-info type-info
                  'to-insert to-insert)))
   completions))


(defun ensime-completion-prefix-at-point ()
  "Returns the prefix to complete."
  ;; A bit of a hack: Piggyback on font-lock's tokenization to
  ;; avoid requesting completions inside comments.
  (when (not (ensime-in-comment-p (point)))
    ;; As an optimization, first get an upper bound on the length of prefix using
    ;; ensime--prefix-char-class.
    (let ((i (point))
	  (s ""))
      (while (and (> i 1) (string-match ensime--prefix-char-class (char-to-string (char-before i))))
	(setq s (concat s (char-to-string (char-before i))))
	(decf i))
      ;; Then use a proper scala identifier regex to extract the prefix.
      (if (string-match ensime--rev-id-re s)
	  (s-reverse (match-string 1 s)) ""))))

(defun ensime-completion-suffix-at-point ()
  "Returns the suffix following point."
  ;; A bit of a hack: Piggyback on font-lock's tokenization to
  ;; avoid requesting completions inside comments.
  (when (not (ensime-in-comment-p (point)))
    (when (looking-at scala-syntax:plainid-re)
      (match-string 1))))

(defun ensime-get-completions-async
    (max-results case-sense callback)
  (ensime-rpc-async-completions-at-point max-results case-sense
   (lexical-let ((continuation callback))
     (lambda (info)
       (let* ((candidates (ensime--annotate-completions
			   (plist-get info :completions))))
	 (funcall continuation candidates))))))

(defun ensime-get-completions (max-results case-sense)
  (let* ((info
	  (ensime-rpc-completions-at-point
	   max-results case-sense))
	 (result (list :prefix (plist-get info :prefix)
		       :candidates (ensime--annotate-completions
				    (plist-get info :completions)))))
    result))

(defun ensime-unique-candidate-at-point ()
  "If the identifier preceding point is already complete, returns it as a fully
 annotated candidate. Otherwise returns nil."
  (let ((prefix (ensime-completion-prefix-at-point)))
    (when (> (length prefix) 0)
      (let* ((info (ensime-rpc-completions-at-point
		    2 ensime-company-case-sensitive))
	     (candidates (ensime--annotate-completions (plist-get info :completions))))
	(when (and (= (length candidates) 1)
		   (string= prefix (car candidates)))
	  (car candidates))))))

(provide 'ensime-completion-util)

;; Local Variables:
;; End:
