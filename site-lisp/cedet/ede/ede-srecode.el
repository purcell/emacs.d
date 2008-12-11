;;; ede-srecode.el --- EDE utilities on top of SRecoder

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-srecode.el,v 1.2 2008/09/01 02:53:58 zappo Exp $

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
;; EDE utilities for using SRecode to generate project files, such as
;; Makefiles.

(require 'srecode)

;;; Code:
;;;###autoload
(defun ede-srecode-setup ()
  "Update various paths to get SRecode to identify our macros."
  (let* ((lib (locate-library "ede.el" t))
	 (ededir (file-name-directory lib))
	 (tmpdir (file-name-as-directory
		  (expand-file-name "templates" ededir))))
    (when (not tmpdir)
      (error "Unable to location EDE Templates directory"))

    ;; Rig up the map.
    (require 'srecode-map)
    (add-to-list 'srecode-map-load-path tmpdir)
    (srecode-map-update-map t)
    
    ;; We don't call this unless we need it.  Load in the templates.
    (srecode-load-tables-for-mode 'makefile-mode)
    (srecode-load-tables-for-mode 'makefile-mode 'ede)

    ;; @todo - autoconf files.

    ))

(defmacro ede-srecode-insert-with-dictionary (template &rest forms)
  "Insert TEMPLATE after executing FORMS with a dictionary.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Locally binds the variable DICT to a dictionary which can be
updated in FORMS."
  `(let* ((dict (srecode-create-dictionary))
	  (temp (srecode-template-get-table (srecode-table)
					    ,template
					    nil
					    'ede))
	  )
     (when (not temp)
       (error "EDE template %s for %s not found!"
	      ,template major-mode))
     (srecode-resolve-arguments temp dict)

     ;; Now execute forms for updating DICT.
     (progn ,@forms)

     (srecode-insert-fcn temp dict)
     ))

;;;###autoload
(defun ede-srecode-insert (template &rest dictionary-entries)
  "Insert at the current point TEMPLATE.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Add DICTIONARY-ENTRIES into the dictionary before insertion.
Note: Just like `srecode-insert', but templates found in 'ede app."
  (ede-srecode-insert-with-dictionary template

    ;; Add in optional dictionary entries.
    (while dictionary-entries
      (srecode-dictionary-set-value dict
				    (car dictionary-entries)
				    (car (cdr dictionary-entries)))
      (setq dictionary-entries
	    (cdr (cdr dictionary-entries))))

    ))

(provide 'ede-srecode)
;;; ede-srecode.el ends here
