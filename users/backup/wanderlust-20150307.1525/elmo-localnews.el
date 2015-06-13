;;; elmo-localnews.el --- Local News Spool Interface for ELMO.

;; Copyright (C) 1998,1999,2000 OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author:  OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
;;	Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;
(require 'elmo-localdir)

(defcustom elmo-localnews-folder-path "~/News"
  "*Local news folder path."
  :type 'directory
  :group 'elmo)

(eval-and-compile
  (luna-define-class elmo-localnews-folder (elmo-localdir-folder) (group))
  (luna-define-internal-accessors 'elmo-localnews-folder))

(luna-define-method elmo-folder-initialize :before ((folder
						     elmo-localnews-folder)
						    name)
  (elmo-localnews-folder-set-group-internal folder
					    (elmo-replace-in-string
					     name "/" "\\.")))

(luna-define-method elmo-localdir-folder-path ((folder elmo-localnews-folder))
  elmo-localnews-folder-path)

(luna-define-method elmo-localdir-folder-name ((folder elmo-localnews-folder)
					       name)
  (elmo-replace-in-string name "\\." "/"))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-localnews-folder))
  (expand-file-name
   (elmo-localnews-folder-group-internal folder)
   (expand-file-name
    (symbol-name (elmo-folder-type-internal folder))
    elmo-msgdb-directory)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-localnews-folder))
  (list (elmo-localnews-folder-group-internal folder)))

(require 'product)
(product-provide (provide 'elmo-localnews) (require 'elmo-version))

;;; elmo-localnews.el ends here
