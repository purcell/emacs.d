;;; semantic-symref-grep.el --- Symref implementation using find/grep

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-symref-grep.el,v 1.3 2008/12/13 17:23:49 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Implement the symref tool API using the external tools find/grep.
;;
;; The symref GREP tool uses grep in a project to find symbol references.
;; This is a lowest-common-denominator tool with sucky performance that
;; can be used in small projects to find symbol references.

(require 'semantic-symref)

;;; Code:

;;; GREP
;;
;;;###autoload
(defclass semantic-symref-tool-grep (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using grep.
This tool uses EDE to find he root of the project, then executes
find-grep in the project.  The output is parsed for hits
and those hits returned.")

(eval-when-compile (require 'ede))

(defvar semantic-symref-filepattern-alist
  '((c-mode . "*.[ch]")
    (c++-mode "*.[chCH]" "*.[ch]pp" "*.cc" "*.hh")
    (emacs-lisp-mode . "*.el")
    )
  "List of major modes and file extension pattern regexp.
See find -regex man page for format.")

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  ;; Grep doesn't support some types of searches.
  (let ((st (oref tool :searchtype)))
    (when (not (eq st 'symbol))
      (error "Symref impl GREP does not support searchtype of %s" st))
    )
  ;; Find the root of the project, and do a find-grep...
  (let* (;; Find the file patterns to use.
	 (pat (cdr (assoc major-mode semantic-symref-filepattern-alist)))
	 (rootdir (cond 
		   ;; Project root via EDE.
		   ((eq (oref tool :searchscope) 'project)
		    (let ((rootproj (when (and (featurep 'ede) ede-minor-mode)
				      (ede-toplevel))))
		      (if rootproj
			  (ede-project-root-directory rootproj)
			default-directory)))
		   ;; Calculate the target files as just in
		   ;; this directory... cause I'm lazy.
		   ((eq (oref tool :searchscope) 'target)
		    default-directory)
		   ))
	 (cmds (cond ((stringp pat)
		      (concat "-name \"" pat "\""))
		     ((consp pat)
		      (concat "\\( "
			      (mapconcat (lambda (s)
					   (concat "-name \"" s "\""))
					 pat
					 " -o ")
			      " \\)"))
		     (t
		      (error "semantic-symref-tool-grep - Needs to be configured for %s" major-mode))
		     ))
	 ;; Grep based flags.
	 (grepflgs (cond ((eq (oref tool :resulttype) 'file)
			  "-l ")
			 (t "-n ")))
	 (greppat (cond ((eq (oref tool :searchtype) 'regexp)
			 (oref tool searchfor))
			(t
			 (concat "'\\<" (oref tool searchfor) "\\>'"))))
	 ;; Misc
	 (b (get-buffer-create "*Semantic SymRef*"))
	 (ans nil)
	 )
    
    (save-excursion
      (set-buffer b)
      (erase-buffer)
      (setq default-directory rootdir)
      ;; find . -type f -print0 | xargs -0 -e grep -nH -e 
      (call-process "sh" nil b nil
		    "-c"
		    (concat "find "
			    default-directory
			    " -type f "
			    cmds
			    " -print0 "
			    "| xargs -0 -e grep -H "
			    grepflgs
			    "-e "
			    greppat)
		    )
      )
    (setq ans (semantic-symref-parse-tool-output tool b))
    ;; Return the answer
    ans))

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-grep))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (cond ((eq (oref tool :resulttype) 'file)
	 ;; Search for files
	 (when (re-search-forward "^\\([^\n]+\\)$" nil t)
	   (match-string 1)))
	(t
	 (when (re-search-forward "^\\([^:\n]+\\):\\([0-9]+\\):" nil t)
	   (cons (string-to-number (match-string 2))
		 (match-string 1))
	   ))))

(provide 'semantic-symref-grep)
;;; semantic-symref-grep.el ends here
