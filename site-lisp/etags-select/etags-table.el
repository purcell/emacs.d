;;; etags-table.el --- Set tags table(s) based on current file

;; Copyright (C) 2008  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 28 Oct 2008
;; Version: 1.1
;; Keywords: etags tags tag

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This extension sets the tags table(s) based on the current file.
;; `etags-table-alist' is a list of lists, where the car of each sublist is
;; compared to the current filename.  If it matches, all the rest of the list
;; elements are put on `tags-table-list'.  If `etags-table-search-up-depth' is
;; an integer, the file path will be searched upwards for a tags file.  If one
;; is found, it will be added to the tags table list; this is actually done
;; first so the local TAGS file is at the head of the list.
;;
;; When you switch files and do something tag-related, the tags table list is
;; automatically recomputed.

;;; Change log:
;;
;; 27 Mar 2009 -- v1.1
;;                Add ability to use backreferences in etags-table-alist
;;                Change files to true names when adding to table
;;                Fix the way parent directories are found
;;
;; 28 Oct 2008 -- v1.0
;;                Initial release

;;; Code:

(require 'custom)
(require 'etags)

;;;###autoload
(defgroup etags-table nil
  "*etags table"
  :group 'etags)

;;;###autoload
(defcustom etags-table-alist nil
  "*Map filename to tag file(s)

Example:

(setq etags-table-alist
      (list
       \'(\"/home/me/Projects/foo/.*\\\\.[ch]$\" \"/home/me/Projects/lib1/TAGS\" \"/home/me/Projects/lib2/TAGS\")
       \'(\"/home/me/Projects/bar/.*\\\\.py$\" \"/home/me/Projects/python/common/TAGS\")
       \'(\".*\\\\.[ch]$\" \"/usr/local/include/TAGS\")
       ))

A file named, for example, \"/home/me/Projects/foo/main.c\" would set the
`tags-table-list' to a list of:

\"/home/me/Projects/lib1/TAGS\"
\"/home/me/Projects/lib2/TAGS\"
\"/usr/local/include/TAGS\"

and possibly a local tags file at the head of the list if `etags-table-search-up-depth'
is non-nil.  You can use \\&, \\1, etc. in the tag file names to substitute pieces
captured with \\(\\) in the key.
"
  :group 'etags-table
  :type 'alist)

;;;###autoload
(defcustom etags-table-search-up-depth nil
  "*Max depth to search up for a tags file.  nil means don't search."
  :group 'etags-table
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar etags-table-last-table-list nil
  "Save the last table list so it can be reused if a new one is not found")

(defun etags-table-build-table-list (filename)
  "Build tags table list based on a filename"
  (let (tables)

    ;; Search up
    (when etags-table-search-up-depth
      (let ((depth etags-table-search-up-depth)
            (dir (file-name-directory filename)))
        (while (and (>= depth 0) dir)
          (when (file-exists-p (concat dir "TAGS"))
            (setq tables (list (concat dir "TAGS")))
            (setq depth 0))
          (setq dir (file-name-directory (directory-file-name dir)))
          (setq depth (1- depth)))))

    ;; Go through mapping alist
    (mapc (lambda (mapping)
            (let ((key (car mapping))
                  (tag-files (cdr mapping)))
              (when (string-match key filename)
                (mapc (lambda (tag-file)
                        (add-to-list 'tables (file-truename (replace-match tag-file t nil filename)) t))
                      tag-files))))
          etags-table-alist)

    ;; Return result or the original list
    (setq etags-table-last-table-list
          (or tables tags-table-list etags-table-last-table-list))))

(defun etags-table-recompute ()
  (when (and (or etags-table-alist etags-table-search-up-depth) (buffer-file-name))
    (setq tags-table-list (etags-table-build-table-list (buffer-file-name)))))

(defadvice visit-tags-table-buffer (before etags-table-recompute activate)
  "Recompute `tags-table-list'"
  (etags-table-recompute))

(defadvice tags-completion-table (before etags-table-clear-completion-table activate)
  "Clear the completion table (maybe)"
  (etags-table-recompute)
  (unless (equal tags-table-computed-list-for (mapcar 'tags-expand-table-name tags-table-list))
    (etags-table-clear-completion-table)))

(defun etags-table-clear-completion-table ()
  "Clear the tags completion table"
  (interactive)
  (setq tags-completion-table nil))

(eval-after-load "etags-select"
  '(progn
     (defadvice etags-select-get-tag-files (before etags-table-recompute activate)
       "Recompute `tags-table-list'"
       (etags-table-recompute))
     (defadvice etags-select-find-tag (before etags-table-clear-completion-table activate)
       "Clear the completion table (maybe)"
       (etags-table-recompute)
       (unless (equal tags-table-computed-list-for (mapcar 'tags-expand-table-name tags-table-list))
         (setq tags-completion-table nil)))))

(provide 'etags-table)
;;; etags-table.el ends here
