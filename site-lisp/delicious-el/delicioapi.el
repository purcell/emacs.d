;;; delicioapi.el --- functions to interact with the Delicious API

;; Copyright (C) 2004, 2005, 2006, 2007, 2009 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Created 25 October 2004
;; Version: 0.4FIXME
;; Keywords: comm, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a set of functions for interacting with the REST API at
;; <http://delicious.com>, a "social bookmarking" project. None of these are
;; interactive commands. There is a separate library, `delicious.el', which
;; includes the interactive commands that put these functions to use in
;; various ways. These functions are provided separately to make it convenient
;; for people to write their own front-end interactive commands.
;;
;; del.icio.us was written by Joshua Shachter.
;;
;; Information about the API is at <http://delicious.com/help/api>.

;;; Code:

;;;;_+ Dependencies

(require 'url)
(require 'xml)
(eval-when-compile (require 'cl))               ; `cdaadr'

;;;;_+ Variables

(defconst delicious-api-version "0.4FIXME"
  "The version string for this copy of delicioapi.el.")

(defconst delicious-api-user-agent (format "delicioapi.el/%s"
                                           delicious-api-version)
  "The User-Agent header that we will send to the server.")

(defconst delicious-api-host "api.del.icio.us"
  "The Delicious host name.")

(defconst delicious-api "/v1/"
  "The path to the Delicious API.  It should begin and end in a slash.")

;;;;_+ API Functions

(defun delicious-api-response (buffer)
  "Process the XML response from Delicious in BUFFER."
  (declare (special url-http-end-of-headers))
  (with-current-buffer buffer
    (set-buffer-multibyte t)
    ;; FIXME do we need utf-8-dos?
    (decode-coding-region url-http-end-of-headers (point-max) 'utf-8)
    (car (xml-parse-region url-http-end-of-headers (point-max)))))

(defun delicious-api-request (path &rest params)
  "Do a Delicious API request to PATH with query PARAMS.

Optional PARAMS should be a list of query parameter
specifications of one of the following forms:

1. (KEY . VAL) -- strings specifying the parameter name and value

2. KEY -- symbol specifying the parameter name, value of which is
   stored in the symbol's value cell

Returns the result as parsed by `xml-parse-region'."
  (let ((url-package-name "delicioapi.el")
        (url-package-version delicious-api-version))
    (delicious-api-response
     (url-retrieve-synchronously
      (concat
       "https://" delicious-api-host delicious-api path
       (when params
         (concat "?"
                 (mapconcat (lambda (p)
                              (let ((name (or (car-safe p) (symbol-name p)))
                                    (val (or (cdr-safe p)
                                             (and (symbolp p)
                                                  (symbol-value p)))))
                                (when val
                                  (concat name "=" (url-hexify-string val)))))
                            params "&"))))))))

(defun delicious-api-get-timestamp ()
  "Return time of the last update for your Delicious user account.
The value returned is a timestamp string as returned by the
\"posts/update\" request."
  (xml-get-attribute (delicious-api-request "posts/update") 'time))

(defun delicious-api-get-hashes (&optional cooked)
  "Return a change manifest of all posts.
If COOKED is non-nil, returns a hash table mapping bookmark URL
MD5 hashes to their change detection signatures (both strings).
Otherwise, the raw s-expression response is returned."
  (let ((resp (delicious-api-request "posts/all?hashes")))
    (if cooked
        (when (eq (car resp) 'posts)
          (let ((res (make-hash-table :test 'equal)))
            (mapc (lambda (el)
                    (let ((attrs (cadr el)))
                      (puthash (cdr (assq 'url attrs))
                               (cdr (assq 'meta attrs))
                               res)))
                  (cddr resp))
            res))
      resp)))

(defun delicious-api/posts/add (url &optional description tags extended time)
  "Post a bookmark for URL to your Delicious account.
You may include a DESCRIPTION (string), TAGS (space-separated string),
EXTENDED (extra description string) and TIME (in the format
%C%y-%m-%dT%H:%M:%SZ)."
  (delicious-api-request "posts/add"
                         'url 'description 'tags 'extended `("dt" . ,time)))

(defun delicious-api/posts/suggest (url &optional cooked)
  "Return a list of popular, recommended and network tags for URL.
Intended as a suggestion for tagging a particular url.

If COOKED is non-nil, the return value is nil if no suggestions
were returned for URL, or an alist with elements of the
form (TYPE . TAGS) where TYPE is a symbol designating the tag
type (e.g. `popular') and TAGS is the (string) list of all tags
of that type. Otherwise, the raw XML s-expression response is
returned."
  (let ((resp (delicious-api-request "posts/suggest" 'url)))
    (if cooked
        (when (eq (car resp) 'suggest)
          (let (r)
            (dolist (item resp r)
              (when (consp item)
                (let ((el (assq (car item) r)))
                  (if el (setcdr el (cons (cdaadr item) (cdr el)))
                    (setq r (cons (list (car item) (cdaadr item)) r))))))))
      resp)))

;; unused
(defun delicious-api/tags/get (&optional tags)
  "Return your tags and the number of entries with each tag.
TAGS can be Delicious tags  in the `xml' package format.
If not provided, contact the server."
  (let ((tags (cdr (or tags (delicious-api-request "tags/get"))))
        tags-list)
    (dolist (tag tags (nreverse tags-list))
      (when (listp tag)                 ; FIXME necessary?
        (let (cell
              (name (xml-get-attribute tag 'tag))
              (count (string-to-number (xml-get-attribute tag 'count))))
          (setq cell (cons (cons 'tag name) (cons 'count count)))
          (setq tags-list (cons cell tags-list)))))))

(defun delicious-api/posts/get (&optional tag date url hashes meta)
  "Return a list of posts filtered by the supplied parameters.
If no date is supplied, the most recent date with posts will be used."
  (delicious-api-request "posts/get" 'tag `("dt" . ,date) 'url 'hashes 'meta))

(defun delicious-api/posts/recent (&optional tag count)
  "Return a list, optionally filtered by TAG, of the COUNT most recent posts.
This will max out at 100. Use `delicious-api/posts/all' if you want more
than that."
  (let ((count (number-to-string
                (cond ((null count) 15)
                      ((> count 100) 100)
                      (t count)))))
    (delicious-api-request "posts/recent" 'tag 'count)))

(defun delicious-api/posts/all (&optional tag)
  "Return all posts. If TAG is non-nil, return all posts with that tag."
  (delicious-api-request "posts/all" 'tag))

;; unused
(defun delicious-api/posts/dates (&optional tag)
  "Return dates with the number of posts at each date.
TAG is a tag to filter by."
  (delicious-api-request "posts/dates" 'tag))

(defun delicious-api/tags/rename (old new)
  "Rename OLD tag to NEW in all posts."
  (delicious-api-request "tags/rename" 'old 'new))

(defun delicious-api/posts/delete (url)
  "Delete URL from bookmarks."
  (delicious-api-request "posts/delete" 'url))

(defun delicious-api-version ()
  "Return the version of the Emacs Delicious API in use."
  (interactive)
  (message "%s" delicious-api-version))

(provide 'delicioapi)
;;; delicioapi.el ends here
