;;; org-jira.el --- add a jira:ticket protocol to Org
(defconst org-jira-version "0.1")
;; Copyright (C) 2008-2012 Jonathan Arkell.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; This adds a jira protocol to org mode.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; I had initially planned on adding bi-directional linking, so you
;; could store links from a jira ticket.  I also wanted to import
;; tickets assigned to you as a task.  However, I am no longer working
;; with JIRA, so this is now abandonware.

;;; Installation:
;; Put org-jira.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file, preferably after you load org mode.
;(require 'org-jira)

;;; TODO:
;; - bi-directional links
;; - deeper importing, like tasks...?

;;; CHANGELOG:
;; v 0.2 - ran through checkdoc
;;       - Abandoned.
;; v 0.1 - Initial release

(require 'jira)

(org-add-link-type "jira" 'org-jira-open)

(defun org-jira-open (path)
  "Open a Jira Link from PATH."
  (jira-show-issue path))


(provide 'org-jira)

;;; org-jira.el ends here
