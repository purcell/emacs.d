;;; elsp-spamfilter.el --- Spamfilter support for elmo-spam.

;; Copyright (C) 2003 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news, spam

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
(require 'elmo-spam)

(require 'luna)
(require 'mime-view)
(require 'spamfilter)

(defgroup elmo-spam-spamfilter nil
  "Spam spamfilter configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-spamfilter-corpus-filename
  (expand-file-name ".spamfilter" elmo-msgdb-directory)
  "Filename of the Spamfilter corpus."
  :type '(file :tag "Filename of the Spamfilter corpus")
  :group 'elmo-spam-spamfilter)

(eval-and-compile
  (luna-define-class elsp-spamfilter (elsp-generic)
		     (good-corpus bad-corpus modified))
  (luna-define-internal-accessors 'elsp-spamfilter))

(luna-define-method initialize-instance :around ((processor elsp-spamfilter)
						 &rest init-args)
  (luna-call-next-method)
  (let ((spamf-good-corpus (make-spamf-corpus
			    :name "spamf-good-corpus"
			    :table (make-hash-table :test #'eq)
			    :message-count 0))
	(spamf-bad-corpus  (make-spamf-corpus
			    :name "spamf-bad-corpus"
			    :table (make-hash-table :test #'eq)
			    :message-count 0)))
    (spamf-load-corpus-from-file elmo-spam-spamfilter-corpus-filename)
    (elsp-spamfilter-set-good-corpus-internal processor spamf-good-corpus)
    (elsp-spamfilter-set-bad-corpus-internal  processor spamf-bad-corpus)
    processor))

(luna-define-method elmo-spam-modified-p ((processor elsp-spamfilter))
  (elsp-spamfilter-modified-internal processor))

(luna-define-method elmo-spam-save-status ((processor elsp-spamfilter))
  (spamf-save-corpus-to-file
   elmo-spam-spamfilter-corpus-filename
   (elsp-spamfilter-good-corpus-internal processor)
   (elsp-spamfilter-bad-corpus-internal  processor))
  (elsp-spamfilter-set-modified-internal processor nil))

(defun elsp-spamfilter-decode-buffer (buffer)
  (mime-display-message
   (mime-open-entity 'elmo-buffer buffer)
   (current-buffer)))

(defsubst elsp-spamfilter-register-buffer-internal (processor buffer spam)
  (spamf-register-words-buffer
   (if spam
       (elsp-spamfilter-bad-corpus-internal processor)
     (elsp-spamfilter-good-corpus-internal processor))
   buffer)
  (elsp-spamfilter-set-modified-internal processor t))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-spamfilter)
					     buffer &optional register)
  (with-temp-buffer
    (elsp-spamfilter-decode-buffer buffer)
    (let ((spam (spamf-spam-buffer-p
		 (current-buffer)
		 (elsp-spamfilter-good-corpus-internal processor)
		 (elsp-spamfilter-bad-corpus-internal  processor))))
      (when register
	(elsp-spamfilter-register-buffer-internal
	 processor (current-buffer) spam))
      spam)))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-spamfilter)
						    buffer &optional restore)
  (with-temp-buffer
    (elsp-spamfilter-decode-buffer buffer)
    (elsp-spamfilter-register-buffer-internal processor (current-buffer) t)))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-spamfilter)
						    buffer &optional restore)
  (with-temp-buffer
    (elsp-spamfilter-decode-buffer buffer)
    (elsp-spamfilter-register-buffer-internal processor (current-buffer) nil)))

(require 'product)
(product-provide (provide 'elsp-spamfilter) (require 'elmo-version))

;;; elsp-spamfilter.el ends here
