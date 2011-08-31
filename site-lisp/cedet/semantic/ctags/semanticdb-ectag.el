;;; semanticdb-ectag.el --- Database support for Exuberent CTags files.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-ectag.el,v 1.4 2009/01/10 00:16:57 zappo Exp $

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
;; Unlike some database support, The Exuberent CTag support only has
;; tables.  The standard semanticdb-project-database can contain these
;; tables.
;;
;; The basic idea here is to use a ctag table for supported languages
;; if the file is never loaded into a buffer.  Once it's in a buffer,
;; switch to the Emacs internal parser generated table.

(require 'semantic-ectag-lang)
(require 'semantic-ectag-parse)
(require 'semantic-ectag-util)

;;; Code:

;;; ENABLE CTAGS SUPPORT
;;
;; Off by default, users can use this to enable ctags parsing
;; if they desire.
;;;###autoload
(defun semanticdb-enable-exuberent-ctags (mode)
  "Enable the use of exuberent ctags for out-of-buffer parsing for MODE.
MODE is a `major-mode' symbol used.
Throws an error if `semantic-ectag-program' is not of the correct
version needed by Semantic ctags support."
  (interactive
   (list (completing-read
          "Mode: " obarray
          #'(lambda (s) (string-match "-mode$" (symbol-name s)))
          t (symbol-name major-mode))))
  ;; First, make sure the version is ok.
  (semantic-ectag-test-version)
  ;; Make sure mode is a symbol.
  (when (stringp mode)
    (setq mode (intern mode)))
  ;; Next, enable the table create routine.
  (eval
   `(setq-mode-local ,mode
		     semanticdb-out-of-buffer-create-table-fcn
		     (lambda (fname)
		       (semanticdb-ectag-create-table-for-file-not-in-buffer
			fname (quote ,mode)))))
  )

;;; PARSE REPLACEMENT
;;
;; These functions will substitute in Exuberent CTag parsing
;; within the Semanticdb framework.
(defun semanticdb-ectag-create-table-for-file-not-in-buffer (filename mode)
  "Create a SemanticDB table for the file in FILENAME using ctags.
The argument MODE specifies the expected major mode to use in Emacs
if FILENAME were loaded."
  (let* ((newstuff (semanticdb-create-table-for-file filename))
	 (table (cdr newstuff))
	 (tags (semantic-ectag-parse-file-with-mode filename mode))
	 )
    ;; We've made the new table, now fill it in with tags determined with
    ;; ctags.
    (semanticdb-synchronize table tags)

    ;; Leave the :mode slot empty.  The reset of semanticdb will force
    ;; :tags to be replaced by the Emacs parser.  nil also allows these tags
    ;; to be used loosely.

    ;; Return the table
    table))


(provide 'semanticdb-ectag)
;;; semanticdb-ectag.el ends here
