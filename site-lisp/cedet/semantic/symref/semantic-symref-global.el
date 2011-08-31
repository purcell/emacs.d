;;; semantic-symref-global.el --- Use GNU Global for symbol references

;; Copyright (C) 2008, 2009 Eric Ludlam

;; Author: Eric Ludlam <eludlam@mathworks.com>
;; X-RCS: $Id: semantic-symref-global.el,v 1.7 2009/02/27 05:00:20 zappo Exp $

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
;; GNU Global use with the semantic-symref system.

(require 'cedet-global)
(require 'semantic-symref)

;;; Code:
;;;###autoload
(defclass semantic-symref-tool-global (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using GNU Global.
The GNU Global command can be used to generate lists of tags in a way
similar to that of `grep'.  This tool will parse the output to generate
the hit list.

See the function `cedet-gnu-global-search' for more details.")

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-global))
  "Perform a search with GNU Global."
  (let ((b (cedet-gnu-global-search (oref tool :searchfor)
				    (oref tool :searchtype)
				    (oref tool :resulttype)
				    (oref tool :searchscope)
				    ))
	)
    (semantic-symref-parse-tool-output tool b)
    ))

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-global))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (cond ((or (eq (oref tool :resulttype) 'file)
	     (eq (oref tool :searchtype) 'tagcompletions))
	 ;; Search for files
	 (when (re-search-forward "^\\([^\n]+\\)$" nil t)
	   (match-string 1)))
	(t
	 (when (re-search-forward "^\\([^ ]+\\) +\\([0-9]+\\) \\([^ ]+\\) " nil t)
	   (cons (string-to-number (match-string 2))
		 (match-string 3))
	   ))))

(provide 'semantic-symref-global)
;;; semantic-symref-global.el ends here
