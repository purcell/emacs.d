;;; semantic-doc.el --- Routines for documentation strings

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-doc.el,v 1.4 2004/04/28 15:36:40 ponced Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; It is good practice to write documenation for your functions and
;; variables.  These core routines deal with these documentation
;; comments or strings.  They can exist either as a tag property
;; (:documentation) or as a comment just before the symbol, or after
;; the symbol on the same line.

(require 'semantic-tag)

;;; Code:

;;;###autoload
(define-overload semantic-documentation-for-tag (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token."
  (if (not tag)
      (setq tag (car (semantic-find-tag-by-overlay nil))))
  (:override
   ;; No override.  Try something simple to find documentation nearby
   (save-excursion
     (set-buffer (semantic-tag-buffer tag))
     (semantic-go-to-tag tag)
     (or
      ;; Is there doc in the tag???
      (if (semantic-tag-docstring tag)
          (if (stringp (semantic-tag-docstring tag))
              (semantic-tag-docstring tag)
            (goto-char (semantic-tag-docstring tag))
            (semantic-doc-snarf-comment-for-tag nosnarf)))
      ;; Check just before the definition.
      (save-excursion
        (re-search-backward comment-start-skip nil t)
        (if (not (semantic-brute-find-tag-by-position
                  (point) (current-buffer) t))
            ;; We found a comment that doesn't belong to the body
            ;; of a function.
            (semantic-doc-snarf-comment-for-tag nosnarf)))
      ;;  Lets look for comments either after the definition, but before code:
      ;; Not sure yet.  Fill in something clever later....
      nil))))

(make-obsolete-overload 'semantic-find-documentation
                        'semantic-documentation-for-tag)

(defun semantic-doc-snarf-comment-for-tag (nosnarf)
  "Snarf up the comment at POINT for `semantic-documentation-for-tag'.
Attempt to strip out comment syntactic sugar.
Argument NOSNARF means don't modify the found text.
If NOSNARF is 'lex, then return the lex token."
  (let* ((semantic-ignore-comments nil)
	 (semantic-lex-analyzer #'semantic-comment-lexer))
    (if (memq nosnarf '(lex flex)) ;; keep `flex' for compatibility
	(car (semantic-lex (point) (1+ (point))))
      (let ((ct (semantic-lex-token-text
		 (car (semantic-lex (point) (1+ (point)))))))
	(if nosnarf
	    nil
	  ;; ok, try to clean the text up.
	  ;; Comment start thingy
	  (while (string-match (concat "^\\s-*" comment-start-skip) ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; Arbitrary punctuation at the beginning of each line.
	  (while (string-match "^\\s-*\\s.+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; End of a block comment.
	  (if (and block-comment-end (string-match block-comment-end ct))
	      (setq ct (concat (substring ct 0 (match-beginning 0))
			       (substring ct (match-end 0)))))
	  ;; In case it's a real string, STRIPIT.
	  (while (string-match "\\s-*\\s\"+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0))))))
	;; Now return the text.
	ct))))

;;;###autoload
(semantic-alias-obsolete 'semantic-find-documentation
                         'semantic-documentation-for-tag)

(provide 'semantic-doc)

;;; semantic-doc.el ends here
