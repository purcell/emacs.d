;;; srecode-texi.el --- Srecode texinfo support.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-texi.el,v 1.4 2008/02/24 01:44:37 zappo Exp $

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
;; Texinfo semantic recoder support.
;;
;; Contains some handlers, and a few simple texinfo srecoder applications.

(require 'semantic)
(require 'semantic-texi)

;;; Code:
;;;###autoload
(defun srecode-texi-add-menu (newnode)
  "Add an item into the current menu.  Add @node statements as well.
Argument NEWNODE is the name of the new node."
  (interactive "sName of new node: ")
  (srecode-load-tables-for-mode major-mode)
  (semantic-fetch-tags)
  (let ((currnode (reverse (semantic-find-tag-by-overlay)))
	(nodebounds nil))
    (when (not currnode)
      (error "Cannot find node to put menu item into"))
    (setq currnode (car currnode))
    (setq nodebounds (semantic-tag-texi-section-text-bounds currnode))
    ;; Step 1:
    ;;   Limit search within this node.
    ;; Step 2:
    ;;   Find the menu.  If there isn't one, add one to the end.
    ;; Step 3:
    ;;   Add new item to end of menu list.
    ;; Step 4:
    ;;   Find correct node new item should show up after, and stick
    ;;   the new node there.
    (if (string= (semantic-texi-current-environment) "menu")
	;; We are already in a menu, so insert the new item right here.
	(beginning-of-line)
      ;; Else, try to find a menu item to append to.
      (goto-char (car nodebounds))
      (if (not (re-search-forward "^@menu" (car (cdr nodebounds)) t))
	  (progn
	    (goto-char (car (cdr nodebounds)))
	    (if (not (y-or-n-p "Add menu here? "))
		(error "Abort"))
	    (srecode-insert "declaration:menu"))
	;; Else, find the end
	(re-search-forward "@end menu")
	(beginning-of-line)))
    ;; At this point, we are in a menu... or not.
    ;; If we are, do stuff, else error.
    (when (string= (semantic-texi-current-environment) "menu")
      (let ((menuname newnode)
	    (returnpoint nil))
	(srecode-insert "declaration:menuitem" "NAME" menuname)
	(set-mark (point))
	(setq returnpoint (make-marker))
	;; Update the bound since we added text
	(setq nodebounds (semantic-tag-texi-section-text-bounds currnode))
	(beginning-of-line)
	(forward-char -1)
	(beginning-of-line)
	(let ((end nil))
	  (if (not (looking-at "\\* \\([^:]+\\):"))
	      (setq end (car (cdr nodebounds)))
	    (let* ((nname (match-string 1))
		   (tag
		    (semantic-deep-find-tags-by-name nname (current-buffer))))
	      (when tag
		(setq end (semantic-tag-end (car tag))))
	      ))
	  (when (not end)
	    (goto-char returnpoint)
	    (error "Could not find location for new node" ))
	  (when end
	    (goto-char end)
	    (when (bolp) (forward-char -1))
	    (insert "\n")
	    (if (eq (semantic-current-tag) currnode)
		(srecode-insert "declaration:subnode" "NAME" menuname)
	      (srecode-insert "declaration:node" "NAME" menuname))
	    )
	  )))
    ))

;;;###autoload
(defun srecode-semantic-handle-:texi (dict)
  "Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level"
  (let ((tags (reverse (semantic-find-tag-by-overlay)))
	(level nil))
    (while (and tags (not (semantic-tag-of-class-p (car tags) 'section)))
      (setq tags (cdr tags)))
    (when tags
      (save-excursion
	(goto-char (semantic-tag-start (car tags)))
	(when (looking-at "@node")
	  (forward-line 1)
	  (beginning-of-line))
	(when (looking-at "@\\(\\w+\\)")
	  (setq level (match-string 1))
	  )))
    (srecode-dictionary-set-value dict "LEVEL" (or level "chapter"))
    (let ((nl (assoc level '( ( nil . "top" )
			      ("top" . "chapter")
			      ("chapter" . "section")
			      ("section" . "subsection")
			      ("subsection" . "subsubsection")
			      ("subsubsection" . "subsubsection")
			      ))))
      (srecode-dictionary-set-value dict "NEXTLEVEL" (cdr nl)))))

(provide 'srecode-texi)
;;; srecode-texi.el ends here
