;;; srecode-filters.el --- Filteres for use in template variables.

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Various useful srecoder template functions.

;;; Code:
;;

;;(defun srecode-BLAHBLAH (str)
;;  "Filter some string..."
;;  (let* ((tpl (srecode-peek srecode-template))
;;	 (dict srecode-inserter-variable-current-dictionary))
;;    
;;    ))

(condition-case nil
    (require 'newcomment)
  (error nil))

(require 'srecode-table)
(require 'srecode-insert)

;;;###autoload
(defun srecode-comment-prefix (str)
  "Prefix each line of STR with the comment prefix characters."
  (let* ((dict srecode-inserter-variable-current-dictionary)
	 ;; Derive the comment characters to put in front of each line.
	 (cs (or (and dict
		      (srecode-dictionary-lookup-name dict "comment_prefix"))
		 (and comment-multi-line comment-continue)
		 (and (not comment-multi-line) comment-start)))
	 (strs (split-string str "\n"))
	 (newstr "")
	 )
    (while strs
      (cond ((and (not comment-multi-line) (string= (car strs) ""))
	     ; (setq newstr (concat newstr "\n")))
	     )
	    (t
	     (setq newstr (concat newstr cs " " (car strs)))))
      (setq strs (cdr strs))
      (when strs (setq newstr (concat newstr "\n"))))
    newstr))

(provide 'srecode-filters)

;;; srecode-filters.el ends here

