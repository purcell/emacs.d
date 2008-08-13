;;; sb-texinfo.el --- provide hierarchical speedbar menu's for texinfo files

;; Copyright (c) 2000, 2002 Richard Y. Kim

;; Author: Richard Y. Kim, <ryk@ap.com>
;; Maintainer: Richard Y. Kim, <ryk@ap.com>
;; Created: Fri Jun 16 17:23:11 2000
;; Version: $Id: sb-texinfo.el,v 1.9 2005/09/30 20:26:09 zappo Exp $
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA

;;; Commentary:
;;
;; The speedbar mode provides support for texinfo files by creating
;; tags using imenu.  This presentation do not reflect the natural
;; hierarchy of the texinfo document.
;;
;; This small add-on to speedbar provides an alternate way to view the
;; document which shows the natural hierarchy of the document.  This
;; shows the same information that `M-x texinfo-show-structure'
;; displays, but in the speedbar frame with the speedbar user
;; interface.

;; Installtation procedure:
;;   Install speedbar 0.12 or later.
;;   Add the following to your ~/.emacs file:
;;   (eval-after-load "speedbar" '(load-library "sb-texinfo"))

;; Known Problems:
;;   Does not look inside files included via @include directive.
;;
;;   The indentations of nodes with and without subnodes could be better.
;;
;;   Clicking on nodes with subnodes only shows subnodes rather than
;;   displaying the node content on the target frame.  Instead the first
;;   subnode on the speedbar has to be clicked.  I believe this can be
;;   fixed if a custom speedbar button display routine is written rather
;;   than using the built-in function speedbar-insert-generic-list.
;;   If I learn to do this, I will, but I make no promises.
;;
;;   Potential problem with auctex 9.9p users.  Auctex provides it's
;;   own texinfo mode which does not use texinfo-mode-hook.  Make sure
;;   you do not use texinfo mode provided by auctex!

;;; Change Log:
;;;
;;; 1.7 - By Eric Ludlam <zappo@gnu.org>
;;;       In `speedbar-fetch-dynamic-texinfo', set the buffer to file
;;;       to correspond to a speedbar change.
;;;
;;; 1.6 - By Eric Ludlam <zappo@gnu.org>
;;;       speedbar-insert-texinfo-list no longer sets sthm to nil.
;;;       speedbar-format-texinfo-list uses new positioned group for
;;;       any chapter w/ sections (etc)  Also set new
;;;       speedbar-generic-list-{group-expand|tag}-button-list to
;;;       nice values.
;;;
;;; 1.5 - speedbar-tag-hierarchy-method is set to nil by
;;;       speedbar-insert-texinfo-list as well as
;;;       speedbar-fetch-dynamic-texinfo.  This is needed in order to
;;;       have the texinfo nodes displayed in correct order.
;;;
;;; 1.4 - speedbar-tag-hierarchy-method is now set to nil by
;;;       speedbar-fetch-dynamic-texinfo after making it buffer local
;;;       first.  I thought this was buffer-local variable alread, but
;;;       it is not.  Also added installation instruction.
;;;
;;; 1.3 - Added bunch of comments in the code as well as installation
;;;       instruction.  speedbar-texinfo-mode-hook now turns on the
;;;       speedbar mode.
;;;
;;; 1.2 - Use newly created variable speedbar-texinfo-section-regexp rather
;;;       than texinfo-section-types-regexp, because the latter does not
;;;       have "top" in it.
;;;
;;;       Also added a hook to texinfo-mode-hook which sets
;;;       speedbar-tag-hierarchy-method to nil at Eric Ludlam's
;;;       suggestion.
;;;
;;; 1.1 - first draft sent to Eric. on June 16, 2000.

;;; Code:

(require 'speedbar)

;; GNU Emacs 20.7 comes with speedbar 0.8.1 bundled which does not work
;; with sb-texinfo.  Lacking speedbar-version variable, check whether
;; speedbar-dynamic-tags-function-list is bound which was introduced after
;; 0.8.1.
(or (boundp 'speedbar-dynamic-tags-function-list)
    (error "Speedbar 0.11 or newer is required to use sb-texinfo."))

;; Attach these new functions to handle texinfo-mode.
(add-to-list 'speedbar-dynamic-tags-function-list
	     '(speedbar-fetch-dynamic-texinfo . speedbar-insert-texinfo-list))

;; speedbar-texinfo-section-regexp is same as texinfo-section-types-regexp
;; except that it has "top" entry.
(defconst speedbar-texinfo-section-regexp
  "^@\\(top \\|chapter \\|sect\\|subs\\|subh\\|unnum\\|major\\|chapheading \\|heading \\|appendix\\)")

(defun speedbar-fetch-dynamic-texinfo ( filename )
  "This returns t if the major mode of the current buffer is not 'texinfo-mode.
If it is 'texinfo-mode, then this returns a list where each element is
(LEVEL NAME . MARKER).
LEVEL is 0, 1, 2, or 3 corresponding to chapter section,
subsection, and subsubsection respectively.
NAME is the name of the node.
MARKER is emacs marker that points to the beginning of the node.
The elements in the the list returned are in ascending order of the MARKER.
This function along with it's parter, speedbar-insert-texinfo-list, are
designed to be added to the speedbar-dynamic-tags-function-list list."
  (set-buffer (find-file-noselect filename))
  (if (not (eq major-mode 'texinfo-mode))
      t
    (condition-case nil
	(save-excursion

	  ;; Set speedbar-tag-hierarchy-method to nil so that
	  ;; speedbar-create-tag-hierarchy won't reorder the list.
	  ;; Make it buffer local so that the global value is not touched.
	  (make-local-variable 'speedbar-tag-hierarchy-method)
	  (setq speedbar-tag-hierarchy-method nil)

	  (set (make-local-variable
		'speedbar-generic-list-group-expand-button-type)
	       'expandtag)
	  (set (make-local-variable
		'speedbar-generic-list-tag-button-type)
	       'statictag)

	  (let ((heading-to-level
		 '(("top" . 0)
		   ("chapter" . 0) ("section" . 1)
		   ("subsection" . 2) ("subsubsection" . 3)
		   ("unnumbered" . 0) ("unnumberedsec" . 1)
		   ("unnumberedsubsec" . 2) ("unnumberedsubsubsec" . 3)
		   ("chapheading" . 0) ("heading" . 1)
		   ("subheading" . 2) ("subsubheading" . 3)
		   ("appendix" . 0) ("appendixsec" . 1)
		   ("appendixsubsec" . 2) ("appendixsubsubsec" . 3)
		   ("centerchap" . 0) ("majorheading" . 0)))
		pos-beg title level section alist)
	    ;; Create a list of all nodes
	    (goto-char (point-min))
	    (while (re-search-forward speedbar-texinfo-section-regexp nil t)
	      (goto-char (1+ (match-beginning 0))) ;right after @
	      (setq pos-beg (point-marker))
	      (setq section (symbol-name (read (current-buffer))))
	      (setq level (cdr (assoc section heading-to-level)))
	      (setq title (buffer-substring
			   (progn (skip-chars-forward " \t")
				  (point))
			   (progn (skip-chars-forward "^,\n")
				  (skip-chars-backward " \t")
				  (point))))
	      (setq alist (cons (cons level (cons title pos-beg)) alist)))
	    (nreverse alist)))
      (error t))))

(defun speedbar-format-texinfo-list ( lst level)
  "See `speedbar-fetch-dynamic-texinfo' for description of LST.
LEVEL should be 0 unless this is called recursively.
This function converts LST from a flat list structure into
a hierarchical list suitable to be passed on to `speedbar-insert-texinfo-list'.
This function creates the hierarchical list recursively.
On first pass all the nodes of each chapter are grouped into a list.
Then each of these lists are recursively converted so that all nodes
of each section are grouped together."
  (let (new-list elem section-list)
    (while lst
      (setq section-list (list (cdr (car lst))))
      (setq lst  (cdr lst))
      (while (and lst (> (car (car lst)) level))
	(setq section-list (cons (car lst) section-list))
	(setq lst  (cdr lst)))
      (setq new-list (cons (nreverse section-list) new-list)))
    (setq new-list (nreverse new-list))
    ;; Each member of new-list is a list of all chapters (on first pass), i.e.,
    ;; ( ((chap1 marker) (1 sec1 marker) ...)  ((chap2 marker) ...) ... )

    ;; Start recursion.
    (setq level (1+ level))
    (mapcar '(lambda (x)
	       (if (eq (length x) 1)
		   ;;(cons (car (car x)) x)
		   (car x)
		 (let ((head (car x)))
		   (cons (car head)
			 (cons (cdr head)
			       (speedbar-format-texinfo-list
				(cdr x) level))))))
	    new-list)))

(defun speedbar-insert-texinfo-list (indent lst)
  "Insert a list of generic tokens reorganized into chapeters.
INDENT is the current level of indentation, and LST is the list of tokens."
  (speedbar-insert-generic-list indent
				(speedbar-format-texinfo-list lst 0)
				'speedbar-tag-expand
				'speedbar-tag-find))

(provide 'sb-texinfo)
;;; sb-texinfo.el ends here

