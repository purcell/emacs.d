;;; mu4e-contrib.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2013 Dirk-Jan C. Binnema

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some user-contributed functions for mu4e

;; Contributed by sabof

(require 'mu4e)

(defun mu4e-headers-mark-all-unread-read ()
  "Put a ! \(read) mark on all visible unread messages."
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'read nil)
   (lambda (msg param)
     (memq 'unread (mu4e-msg-field msg :flags)))))

(defun mu4e-headers-flag-all-read ()
  "Flag all visible messages as \"read\"."
  (interactive)
  (mu4e-headers-mark-all-unread-read)
  (mu4e-mark-execute-all t))

;;;

(defun mu4e-shr2text ()
  "Html to text using the shr engine; this can be used in
`mu4e-html2text-command' in a new enough emacs. Based on code by
Titus von der Malsburg."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))
	 ;; When HTML emails contain references to remote images,
	 ;; retrieving these images leaks information. For example,
	 ;; the sender can see when I openend the email and from which
	 ;; computer (IP address). For this reason, it is preferrable
	 ;; to not retrieve images.
	 ;; See this discussion on mu-discuss:
	 ;; https://groups.google.com/forum/#!topic/mu-discuss/gr1cwNNZnXo
	(shr-inhibit-images t))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))


;;; Bookmark handlers
;;
;;  Allow bookmarking a mu4e buffer in regular emacs bookmarks.

;; Probably this can be moved to mu4e-view.el.
(add-hook 'mu4e-view-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'mu4e-view-bookmark-make-record)))
;; And this can be moved to mu4e-headers.el.
(add-hook 'mu4e-headers-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'mu4e-view-bookmark-make-record)))

(defun mu4e-view-bookmark-make-record ()
  "Make a bookmark entry for a mu4e buffer."
  (let* ((msg     (mu4e-message-at-point))
         (maildir (plist-get msg :maildir))
         (date    (format-time-string "%Y%m%d" (plist-get msg :date)))
         (query   (format "maildir:%s date:%s" maildir date))
         (docid   (plist-get msg :docid))
         (mode    (symbol-name major-mode))
         (subject (or (plist-get msg :subject) "No subject")))
    `(,subject
      ,@(bookmark-make-record-default 'no-file 'no-context)
        (location . (,query . ,docid))
        (mode . ,mode)
        (handler . mu4e-bookmark-jump))))

(defun mu4e-bookmark-jump (bookmark)
  "Handler function for record returned by `mu4e-view-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let* ((path  (bookmark-prop-get bookmark 'location))
         (mode  (bookmark-prop-get bookmark 'mode))
         (docid (cdr path))
         (query (car path)))
    (call-interactively 'mu4e)
    (mu4e-headers-search query)
    (sit-for 0.5)
    (mu4e~headers-goto-docid docid)
    (mu4e~headers-highlight docid)
    (unless (string= mode "mu4e-headers-mode")
      (call-interactively 'mu4e-headers-view-message)
      (run-with-timer 0.1 nil
                      (lambda (bmk)
                        (bookmark-default-handler
			  `("" (buffer . ,(current-buffer)) .
			     ,(bookmark-get-bookmark-record bmk))))
                      bookmark))))



;;; handling spam with Bogofilter with possibility to define it for SpamAssassin
;;; contributed by Gour

;;  to add the actions to the menu, you can use something like:

;; (add-to-list 'mu4e-headers-actions
;;              '("sMark as spam" . mu4e-register-msg-as-spam) t)
;; (add-to-list 'mu4e-headers-actions
;;              '("hMark as ham" . mu4e-register-msg-as-ham) t)
;; (add-to-list 'mu4e-headers-actions
;;              '("aMark unsure as spam" . mu4e-mark-unsure-as-spam) t)
;; (add-to-list 'mu4e-headers-actions
;;              '("bMark unsure as ham" . mu4e-mark-unsure-as-ham) t)

(defvar mu4e-register-as-spam-cmd nil
  "Command for invoking spam processor to register message as spam,
for example for bogofilter, use \"/usr/bin/bogofilter -Ns < %s\" ")

(defvar mu4e-register-as-ham-cmd nil
  "Command for invoking spam processor to register message as ham.
For example for bogofile, use \"/usr/bin/bogofilter -Sn < %s\"")

(defun mu4e-register-msg-as-spam (msg)
  "Mark message as spam."
  (interactive)
  (let* ((path (shell-quote-argument (mu4e-message-field msg :path)))
         (command (format mu4e-register-as-spam-cmd path))) ;; re-register msg as spam 
    (shell-command command))
(mu4e-mark-at-point 'delete nil))

(defun mu4e-register-msg-as-ham (msg)
  "Mark message as ham."
  (interactive)
  (let* ((path (shell-quote-argument(mu4e-message-field msg :path)))
         (command (format mu4e-register-as-ham-cmd path))) ;; re-register msg as ham
    (shell-command command))
(mu4e-mark-at-point 'something nil))
 
;;; end of spam-filtering functions 

(provide 'mu4e-contrib)
