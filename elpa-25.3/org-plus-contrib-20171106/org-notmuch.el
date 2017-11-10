;;; org-notmuch.el --- Support for links to notmuch messages from within Org-mode

;; Copyright (C) 2010-2014  Matthieu Lemerre

;; Author: Matthieu Lemerre <racin@free.fr>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements links to notmuch messages and "searchs". A
;; search is a query to be performed by notmuch; it is the equivalent
;; to folders in other mail clients. Similarly, mails are refered to
;; by a query, so both a link can refer to several mails.

;; Links have one the following form
;; notmuch:<search terms>
;; notmuch-search:<search terms>.

;; The first form open the queries in notmuch-show mode, whereas the
;; second link open it in notmuch-search mode. Note that queries are
;; performed at the time the link is opened, and the result may be
;; different from whet the link was stored.

;;; Code:

(require 'org)

;; customisable notmuch open functions
(defcustom org-notmuch-open-function
  'org-notmuch-follow-link
  "Function used to follow notmuch links.

Should accept a notmuch search string as the sole argument."
  :group 'org-notmuch
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

(defcustom org-notmuch-search-open-function
  'org-notmuch-search-follow-link
  "Function used to follow notmuch-search links.

Should accept a notmuch search string as the sole argument."
  :group 'org-notmuch
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)



;; Install the link type
(org-link-set-parameters "notmuch"
			 :follow #'org-notmuch-open
			 :store #'org-notmuch-store-link)

(defun org-notmuch-store-link ()
  "Store a link to a notmuch search or message."
  (when (eq major-mode 'notmuch-show-mode)
    (let* ((message-id (notmuch-show-get-message-id t))
	   (subject (notmuch-show-get-subject))
	   (to (notmuch-show-get-to))
	   (from (notmuch-show-get-from))
	   (date (org-trim (notmuch-show-get-date)))
	   desc link)
      (org-store-link-props :type "notmuch" :from from :to to :date date
       			    :subject subject :message-id message-id)
      (setq desc (org-email-link-description))
      (setq link (concat "notmuch:id:" message-id))
      (org-add-link-props :link link :description desc)
      link)))

(defun org-notmuch-open (path)
  "Follow a notmuch message link specified by PATH."
  (funcall org-notmuch-open-function path))

(defun org-notmuch-follow-link (search)
  "Follow a notmuch link to SEARCH.

Can link to more than one message, if so all matching messages are shown."
  (require 'notmuch)
  (notmuch-show search))



(org-link-set-parameters "notmuch-search"
			 :follow #'org-notmuch-search-open
			 :store #'org-notmuch-search-store-link)

(defun org-notmuch-search-store-link ()
  "Store a link to a notmuch search or message."
  (when (eq major-mode 'notmuch-search-mode)
    (let ((link (concat "notmuch-search:"
			(org-link-escape notmuch-search-query-string)))
	  (desc (concat "Notmuch search: " notmuch-search-query-string)))
      (org-store-link-props :type "notmuch-search"
			    :link link
			    :description desc)
      link)))

(defun org-notmuch-search-open (path)
  "Follow a notmuch message link specified by PATH."
  (message "%s" path)
  (funcall org-notmuch-search-open-function path))

(defun org-notmuch-search-follow-link (search)
  "Follow a notmuch link by displaying SEARCH in notmuch-search mode."
  (require 'notmuch)
  (notmuch-search (org-link-unescape search)))



(defun org-notmuch-tree-follow-link (search)
  "Follow a notmuch link by displaying SEARCH in notmuch-tree mode."
  (require 'notmuch)
  (notmuch-tree (org-link-unescape search)))

(provide 'org-notmuch)

;;; org-notmuch.el ends here
