;;; org-fstree.el --- include a filesystem subtree into an org file


;; Copyright 2009 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtz[REMOVE]laff dot de >
;; Version: 0.4
;; Package-Version: 20090723.819
;; Keywords: org-mode filesystem tree
;; X-URL: <http://www.burtzlaff.de/org-fstree/org-fstree.el>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; org-fstree inserts the filesystem subtree for a given directory.
;; Each file/directory is formatted as a headline, provides links back 
;; to all headlines that are associated with it (by containing links to the file) 
;; and is assigned their tags.
;;
;; Installation:
;;   - put this file into your load-path 
;;   - insert "(require 'org-fstree)" into ~/.emacs
;;
;; Usage:
;;   - enter a line containing "#+BEGIN_FSTREE: <dir>" into an org buffer, 
;;     where <dir> is the directory, that is to be inserted.
;;   - while the cursor is in the line mentioned, press "C-c C-c"
;;
;; Options:
;;   Specify options in the form:
;;   "#+BEGIN_FSTREE: <dir> :<optionname1> <optionvalue1> :<optionname2> <optionvalue2>  ...
;;   Options are:
;;     - :non-recursive t , to suppress recursion into directories
;;     - :exclude-regexp-name <list of regexp strings> , exclude file/directory names matching either 
;;                                                  of the given regexp expressions
;;       Examples: 
;;         :exclude-regexp-name (".*\\.pdf$" ".*\\.zip$"), excludes files/directories ending with either ".pdf" or ".zip"
;;         :exclude-regexp-name ("^\\.git$") , excludes files/directories named ".git"
;;
;;     - :exclude-regexp-fullpath <list of regexp strings>, same as :exclude-regexp-name but matches absolute path to file/directory
;;     - :relative-links t , generates relative instead of absolute links
;;     - :show-only-matches t , only files that are being linked to show up
;;     - :only-directories t , only directories are listed
;;     - :only-regular-files t , only regular files are listed
;;     - :dynamic-update t , [EXPERIMENTAL] dynamically update a subtree on visibility cycling.
;;     - :links-as-properties t, sets the links as properties Link1, Link2,... for use in column view [Does not work with dynamic-update!]
;;     - :no-annotations t, suppresses the search and display of file annotations
;;
;; Limitations and warnings:
;;
;;   - when triggering an update (by pressing "C-c C-c" while in the line mentioned above)
;;     the COMPLETE REGION BETWEEN "#+BEGIN_FSTREE" AND "#+END_FSTREE" IS REPLACED.
;;   - speed  
;;     
;; Code:

(provide 'org-fstree)

(require 'org)

(defun org-fstree-generate (dir level options)
  (interactive)
;;  (message "org-fstree-generate") ;; DEBUG
  (if (file-directory-p dir)
     (let (
	   (non-recursive (plist-get options :non-recursive))
	   (exclude-regexp-name-list (plist-get options :exclude-regexp-name))
	   (exclude-regexp-fullpath-list (plist-get options :exclude-regexp-fullpath))
	   (links-as-properties (plist-get options :links-as-properties))
	   (dynamic-update (plist-get options :dynamic-update))
	   (fullFileNames (directory-files dir 1 nil nil) )
	   (fileNames (directory-files dir nil nil nil) )
	   fileName
	   fullFileName
	   currentHeadline
	   orgHeadlineInfo
	   curTags
	   curPos
	   (linksList nil)
	   (retString "")
	   )
       (while fileNames
	 (setq fullFileName (car fullFileNames))
	 (setq fullFileNames (cdr fullFileNames))
	 (setq fileName (car fileNames))
	 (setq fileNames (cdr fileNames))
	 (setq linksList nil)
	 (setq curTags nil)
	 (cond ((member fileName '("." "..")))
	       ;; the following two lines are really ugly. I'll be glad if someone with more lisp experience tidies this up.
	       ((reduce (function (lambda (a b) (or a b)))  (mapcar (function (lambda (regexp) (not (string= fullFileName (replace-regexp-in-string regexp "" fullFileName) )) )) exclude-regexp-fullpath-list ) :initial-value nil))
	       ((reduce (function (lambda (a b) (or a b)))  (mapcar (function (lambda (regexp) (not (string= fileName (replace-regexp-in-string regexp "" fileName) )) )) exclude-regexp-name-list ) :initial-value nil))
	       ((and (not (file-directory-p fullFileName)) (plist-get options :only-directories)))
               ((and (not (file-regular-p fullFileName)) (plist-get options :only-regular-files)))
	       (t
		(save-excursion 
                (cond ((plist-get options :no-annotations))
                      (t
                ;; Search for links in current buffer
		(goto-char (point-min))
		(setq curPos (point))
		(while (re-search-forward org-bracket-link-regexp nil t)
		  (let ((filenameInLink (match-string 1)))
		  (cond ( (org-fstree-get-parameters-if-inside-fstree-block) (re-search-forward "#\\+END_FSTREE" nil t) )
			( (string= fullFileName (expand-file-name (replace-regexp-in-string "^file:" "" filenameInLink ) ":" ) )
			  (let ((p (point)))
			    (cond ((org-before-first-heading-p))
				  (t
				   ;; go to associated heading
				   (org-back-to-heading t)
				   (setq orgHeadlineInfo (org-heading-components))
				   (setq curTags (concat curTags (nth 5 orgHeadlineInfo) ))
				   (setq currentHeadline (nth 4 orgHeadlineInfo))
				   ;; filter all links from headline, generate link to it and append to linksList
				   (let ((cleanedHeadline (replace-regexp-in-string "\\[\\[.*\\]\\]" "" currentHeadline)))
				     
				     (setq linksList (cons (concat "[[*"  cleanedHeadline "]"
								   (cond ( (plist-get options :show-only-matches) 
									   "[" (replace-regexp-in-string (regexp-quote fullFileName) "" cleanedHeadline) "]" ) )
								   "]")  
							   linksList) ) )
				   (goto-char p)
				   )))))))))

		(cond ((or (not (plist-get options :show-only-matches)) (not (null linksList)))
		       ;; construct headline for current file/directory
		       (let* ((tagString (cond ((not (null curTags)) (concat "  " (replace-regexp-in-string "::" ":" curTags)) ) ))
			      (linkCount 0)
			      (headingString (format "\n%s |%s| [[file:%s][%s]] " 
						     (make-string level ?*) 
						     (cond ((file-directory-p fullFileName) "D") ((file-symlink-p fullFileName) "L") (t " ")) 
						     (if (plist-get options :relative-links) (file-relative-name fullFileName) fullFileName) fileName)))
			 (cond (links-as-properties
				(setq retString (concat retString headingString (if tagString tagString "")
							(if (not (null linksList)) 
							    (concat "\n :PROPERTIES:\n " 
								    (mapconcat (function (lambda (string) (setq linkCount (1+ linkCount)) (concat ":Link" (number-to-string linkCount) ":" string ))) linksList "\n") 
								    "\n :END:" ) ))))
			       (t
				(setq retString (concat retString headingString 
						       (make-string (max 0 (- 100 (length headingString))) ? )
						       (if linksList (concat "{ " (mapconcat 'identity linksList " | ") " }"))
						       (if tagString tagString)
                                                       ))))
			 (if (and (not non-recursive) (not dynamic-update) (file-directory-p fullFileName) )
			     (setq retString (concat retString (org-fstree-generate fullFileName (1+ level) options) ) )
			   ))))))))
       retString)
    (message "%s is not a directory" dir)))

(defun org-fstree-apply-maybe ()
  (interactive)
;;  (message "org-fstree-apply-maybe") (sit-for 1) ;; DEBUG
  (save-excursion
     (if (save-excursion (beginning-of-line 1) (looking-at "#\\+END_FSTREE"))
	 (re-search-backward "#\\+BEGIN_FSTREE" nil t))
     (cond
      ((save-excursion (beginning-of-line 1) (looking-at "#\\+BEGIN_FSTREE"))
       (let* ((params (org-fstree-gather-parameters))
	      (dir (org-link-expand-abbrev (plist-get params :dir)))
	      (options (plist-get params :params))
	      level)
	 ;; get current level; there is a BUG if "#+BEGIN_FSTREE" is inserted after the last headlines dots, that indicate its folded state.
	;; (let ((p (point)))
	(save-excursion
	  (cond ((org-before-first-heading-p)
		 (setq level 1))
		(t (org-back-to-heading)
		   (setq level (+ (funcall outline-level) 1))
		   ;;		    (goto-char p)
		   )))
	   (forward-line)
	   (let ((beg (point)))
	     (re-search-forward "#\\+END_FSTREE\\|#\\+BEGIN_FSTREE" nil t)
	     ;;(let ((generatedString (org-fstree-generate dir level options)))
	     (cond ( (looking-back "#\\+END_FSTREE") 
		     (forward-line -1)
		     (end-of-line 1)
		     (delete-region beg (point) )
		     (insert (concat (org-fstree-generate dir level options) "\n\n")))
		   (t (goto-char beg)
		      (insert (concat (org-fstree-generate dir level options) "\n\n\n#+END_FSTREE"))))
	     ;; hide all subtrees
	     (org-map-region (function (lambda () (hide-subtree))) beg (point))
	     
	     ;;)
	     ))
       1))))
  

(defun org-fstree-show-entry-maybe (state)
  (interactive)
;;  (message "show-entry-maybe..") (sit-for 1) ;; DEBUG
  (let* ( (parameters (save-excursion (org-fstree-get-parameters-if-inside-fstree-block)))
	  (options (plist-get parameters :params)))

    (cond ((and parameters (not (plist-get options :non-recursive)) (plist-get options :dynamic-update) )
	   ;; we are inside the FSTREE block and have to update
	   ;; delete existing content
	   (save-excursion
	     (let* ((endfstree (save-excursion (re-search-forward "#\\+END_FSTREE" nil t) (beginning-of-line) (point)))
                    (end (save-excursion 
			  ;; go to the end of the subtree, specifically to the beginning of the next headline
			  (org-end-of-subtree nil t)
			  ;; check whether the end of the fstree block has been trespassed
                          (and (> (point) endfstree) (goto-char endfstree))
                          ;; got back on character, because editing heading lines in column mode is not possible.
			  ;; this line is supposed to be either empty or an entry.
			  (forward-char -1)
                          (point)
			  )))
	       (beginning-of-line 2)
	       (if (looking-at " *:PROPERTIES:") (progn (re-search-forward ":END:" nil t) (forward-line 1)))

	       
	       (when (and (> (count-lines (point) end) 0) (< (point) end))
                  (delete-region (point) end)
                 )
	       )
	     )
	   (cond ((eq state 'folded))
		 (t 
		  ;; insert new content
		  (save-excursion
		    (let ((beg (point))
			  end
			  (level (1+ (funcall outline-level)))
			  (dir (org-fstree-extract-path-from-headline))
			  (newOptions (plist-put (plist-get parameters :params) ':non-recursive 't)))
                      (when (file-directory-p dir)
                        ;;(when (plist-get options :links-as-properties) (forward-line 1))
	  	        (if (looking-at " *:PROPERTIES:") (progn (re-search-forward ":END" nil t) (forward-line 1)))
		        (end-of-line 1)
                        (when (plist-get options :links-as-parameters)
                          (org-columns-quit))

			(insert (org-fstree-generate dir level newOptions))
   
                        (when (plist-get options :links-as-parameters)
                          (org-columns))
			(setq end (point))
			;; hide all subtrees
			;;(if (plist-get options :links-as-properties)
                          ;;(progn 
                          ;; (org-map-region (function (lambda () (hide-subtree))) beg (point)))
                          (org-end-of-subtree)
                          (hide-subtree)
                              ))))
		 )))))


(defun org-fstree-extract-path-from-headline ()
;;  (interactive) ;;DEBUG
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at org-fstree-heading-regexp)
	(match-string-no-properties 1))))

(defconst org-fstree-heading-regexp ".*\\[\\[file:\\(.*\\)\\]\\[.*\\]\\]"
  "Matches headline in org-fstree section.")
(make-variable-buffer-local 'org-fstree-heading-regexp)

(defun org-fstree-get-parameters-if-inside-fstree-block ()
  (interactive)
  (and   (save-excursion
	 (re-search-forward "#\\+END_FSTREE" nil t) )
	 (save-excursion
	 (re-search-backward "#\\+BEGIN_FSTREE" nil t) 
	 (org-fstree-gather-parameters))))

(defun org-fstree-gather-parameters ()
  (save-excursion 
    (let (rtn)
      (beginning-of-line 1)
      (if (looking-at "#\\+BEGIN_FSTREE[: \t][ \t]*\\([^ \t\r\n]+\\)\\( +.*\\)?")
     	(let ((dir (org-no-properties (match-string 1)))
	      (params (if (match-end 2)
			  (read (concat "(" (match-string 2) ")")))))
	  (setq rtn (list :dir dir :params params) )
  ))
      
       rtn)
    )
)

(defun org-fstree-get-current-outline-level ()
  (save-excursion
    (cond ((org-before-first-heading-p) 1)
	  (t
	   (org-back-to-heading)
	   (+ (funcall outline-level) 1)))))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-fstree-apply-maybe)
(add-hook 'org-pre-cycle-hook 'org-fstree-show-entry-maybe)
;;; org-fstree.el ends here
