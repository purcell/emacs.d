;;; metaweblog.el --- an emacs library to access metaweblog based weblogs
;; Copyright (C) 2008 Ashish Shukla
;; Copyright (C) 2010 Puneeth Chaganti

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'xml-rpc)

(defun metaweblog-get-categories (blog-xmlrpc user-name password blog-id)
  "Retrieves list of categories from the weblog system"
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getCategories"
		       blog-id
		       user-name
		       password))

(defun wp-new-category (blog-xmlrpc user-name password blog-id category)
  "Create new category on the weblog"
  (xml-rpc-method-call blog-xmlrpc
		       "wp.newCategory"
		       blog-id
		       user-name
		       password
		       `(("name" . ,category))))

(defun wp-get-tags (blog-xmlrpc user-name password blog-id)
  "Retrieves list of tags from the weblog system. Uses wp.getTags"
  (xml-rpc-method-call blog-xmlrpc
		       "wp.getTags"
		       blog-id
		       user-name
		       password))

(defun wp-get-pages (blog-xmlrpc user-name password blog-id)
  "Retrieves list of pages from the weblog system. Uses wp.getPages."
  (xml-rpc-method-call blog-xmlrpc
		       "wp.getPages"
		       blog-id
		       user-name
		       password))

(defun wp-get-pagelist (blog-xmlrpc user-name password blog-id)
  "Retrieves list of pages (minimal information) from the weblog
system. Uses wp.getPageList."
  (xml-rpc-method-call blog-xmlrpc
		       "wp.getPageList"
		       blog-id
		       user-name
		       password))

(defun metaweblog-new-post
  (blog-xmlrpc user-name password blog-id content publish)
  "Sends a new post to the blog. If PUBLISH is non-nil, the post is
published, otherwise it is saved as draft. CONTENT will be an alist
title, description, categories, and date as keys (string-ified) mapped to the
title of the post, post contents, list of categories, and date respectively."
  (let ((post-title (cdr (assoc "title" content)))
	(post-description (cdr (assoc "description" content)))
	(post-categories (cdr (assoc "categories" content)))
	(post-tags (cdr (assoc "tags" content)))
	(post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
	(post-date (cdr (assoc "date" content))))
  ;;; since xml-rpc-method-call entitifies the HTML text in the post
  ;;; we've to use raw
  (xml-rpc-xml-to-response (xml-rpc-request
   blog-xmlrpc
   `((methodCall
      nil
      (methodName nil "metaWeblog.newPost")
      (params nil
	      (param nil (value nil (string nil ,blog-id)))
	      (param nil (value nil (string nil ,user-name)))
	      (param nil (value nil (string nil ,password)))
	      (param nil (value nil
				(struct
				 nil
				 (member nil
					 (name nil "title")
					 (value nil ,post-title))
				 (member nil
					 (name nil "description")
					 (value nil ,post-description))
				 (member nil
					 (name nil "mt_excerpt")
					 (value nil ,post-excerpt))
                                 (member nil
					 (name nil "wp_slug")
					 (value nil ,post-permalink))
				 (member nil
					 (name nil "dateCreated")
					 (dateTime.iso8601 nil ,post-date))
				 ,(when post-tags
				    `(member nil
					     (name nil "mt_keywords")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-tags))))))
				 ,(when post-categories
				    `(member nil
					     (name nil "categories")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-categories)))))))))
	      (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun wp-new-page
  (blog-xmlrpc user-name password blog-id content publish)
  "Sends a new page to the blog. If PUBLISH is non-nil, the post is
published, otherwise it is saved as draft. CONTENT will be an alist
title, description, categories, and date as keys (string-ified) mapped to the
title of the post, post contents, list of categories, and date respectively."
  (let ((post-title (cdr (assoc "title" content)))
	(post-description (cdr (assoc "description" content)))
	(post-categories (cdr (assoc "categories" content)))
	(post-tags (cdr (assoc "tags" content)))
	(post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-parent (cdr (assoc "parent" content)))
	(post-date (cdr (assoc "date" content))))
  ;;; since xml-rpc-method-call entitifies the HTML text in the post
  ;;; we've to use raw
  (xml-rpc-xml-to-response (xml-rpc-request
   blog-xmlrpc
   `((methodCall
      nil
      (methodName nil "wp.newPage")
      (params nil
	      (param nil (value nil (string nil ,blog-id)))
	      (param nil (value nil (string nil ,user-name)))
	      (param nil (value nil (string nil ,password)))
	      (param nil (value nil
				(struct
				 nil
				 (member nil
					 (name nil "title")
					 (value nil ,post-title))
				 (member nil
					 (name nil "description")
					 (value nil ,post-description))
				 (member nil
					 (name nil "mt_excerpt")
					 (value nil ,post-excerpt))
                                 (member nil
					 (name nil "wp_slug")
					 (value nil ,post-permalink))
                                 (member nil
					 (name nil "wp_page_parent_id")
					 (value nil ,post-parent))
				 (member nil
					 (name nil "dateCreated")
					 (dateTime.iso8601 nil ,post-date))
				 ,(when post-tags
				    `(member nil
					     (name nil "mt_keywords")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-tags))))))
				 ,(when post-categories
				    `(member nil
					     (name nil "categories")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-categories)))))))))
	      (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun wp-edit-page
  (blog-xmlrpc user-name password blog-id post-id content publish)
  "Edits an existing page on the blog. If PUBLISH is non-nil, the
post is published, otherwise it is saved as draft. CONTENT will
be an alist title, description, categories, and date as
keys (string-ified) mapped to the title of the post, post
contents, list of categories, and date respectively."
  (let ((post-title (cdr (assoc "title" content)))
	(post-description (cdr (assoc "description" content)))
	(post-categories (cdr (assoc "categories" content)))
	(post-tags (cdr (assoc "tags" content)))
	(post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-parent (cdr (assoc "parent" content)))
	(post-date (cdr (assoc "date" content))))
    (message post-date)
  ;;; since xml-rpc-method-call entitifies the HTML text in the post
  ;;; we've to use raw
    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "wp.editPage")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,post-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "title")
                                            (value nil ,post-title))
                                    (member nil
                                            (name nil "description")
                                            (value nil ,post-description))
                                    (member nil
                                            (name nil "mt_excerpt")
                                            (value nil ,post-excerpt))
                                    (member nil
					 (name nil "wp_slug")
					 (value nil ,post-permalink))
                                    (member nil
                                            (name nil "wp_page_parent_id")
                                            (value nil ,post-parent))
                                    (member nil
                                            (name nil "dateCreated")
                                            (dateTime.iso8601 nil ,post-date))
                                    ,(when post-tags
                                       `(member nil
                                                (name nil "mt_keywords")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-tags))))))
                                    ,(when post-categories
                                       `(member nil
                                                (name nil "categories")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-categories)))))))))
                 (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-edit-post
  (blog-xmlrpc user-name password post-id content publish)
  "Edits an exiting post, if post-id is given. If PUBLISH is non-nil, the
post is published, otherwise it is saved as draft. CONTENT will be an alist
title, description, categories, and date as keys (string-ified) mapped to the
title of the post, post contents, list of categories, and date respectively."
  (let ((post-title (cdr (assoc "title" content)))
	(post-description (cdr (assoc "description" content)))
	(post-categories (cdr (assoc "categories" content)))
	(post-tags (cdr (assoc "tags" content)))
	(post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
	(post-date (cdr (assoc "date" content))))
    (message post-date)
  ;;; since xml-rpc-method-call entitifies the HTML text in the post
  ;;; we've to use raw
  (xml-rpc-xml-to-response (xml-rpc-request
   blog-xmlrpc
   `((methodCall
      nil
      (methodName nil "metaWeblog.editPost")
      (params nil
	      (param nil (value nil (string nil ,post-id)))
	      (param nil (value nil (string nil ,user-name)))
	      (param nil (value nil (string nil ,password)))
	      (param nil (value nil
				(struct
				 nil
				 (member nil
					 (name nil "title")
					 (value nil ,post-title))
				 (member nil
					 (name nil "description")
					 (value nil ,post-description))
				 (member nil
					 (name nil "mt_excerpt")
					 (value nil ,post-excerpt))
                                 (member nil
					 (name nil "wp_slug")
					 (value nil ,post-permalink))
				 (member nil
				 	 (name nil "dateCreated")
				 	 (dateTime.iso8601 nil ,post-date))
				 ,(when post-tags
				    `(member nil
					     (name nil "mt_keywords")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-tags))))))
				 ,(when post-categories
				    `(member nil
					     (name nil "categories")
					     (value nil
						    (array
						     nil
						     ,(append
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-categories)))))))))
	      (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-get-post (blog-xmlrpc user-name password post-id)
  "Retrieves a post from the weblog. POST-ID is the id of the post
which is to be returned.  Can be used with pages as well."
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getPost"
		       post-id
		       user-name
		       password))

(defun metaweblog-delete-post (blog-xmlrpc user-name password post-id)
  "Delete an entry from the weblog system."
  (xml-rpc-method-call blog-xmlrpc
		       "blogger.deletePost"
                       nil
		       post-id
		       user-name
		       password
                       t))

(defun wp-delete-page (blog-xmlrpc blog-id user-name password page-id)
  "Delete a page from the weblog system."
  (xml-rpc-method-call blog-xmlrpc
		       "wp.deletePage"
                       blog-id
		       user-name
		       password
		       page-id))

(defun metaweblog-get-recent-posts(blog-xmlrpc blog-id user-name password number-of-posts)
  "Fetches the recent posts from the weblog. NUMBER-OF-POSTS is the
no. of posts that should be returned."
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getRecentPosts"
		       blog-id
		       user-name
		       password
		       number-of-posts))

(defun get-file-properties (file)
  "Gets the properties of a file. Returns an assoc list with
name - file name
bits - data of the file as a base64 encoded string
type - mimetype of file deduced from extension."
  (let* (base64-str type name)
    (save-excursion
      (save-restriction
	(with-current-buffer (find-file-noselect file nil t)
          (fundamental-mode)
	  (setq name (file-name-nondirectory file))
	  (setq base64-str (base64-encode-string (encode-coding-string (buffer-string) 'binary)))
	  (setq type (mailcap-extension-to-mime (file-name-extension file)))
          (kill-buffer)
	  (setq file-props `(("name" . ,name)
                             ("bits" . ,base64-str)
                             ("type" . ,type))))))
    file-props))

(defun metaweblog-upload-file (blog-xmlrpc user-name password blog-id file)
  "Uploads file to the blog. FILE will be an alist name, type,
bits, as keys mapped to name of the file, mime type and the
data."
  (let ((file-name (cdr (assoc "name" file)))
	(file-type (cdr (assoc "type" file)))
	(file-bits (cdr (assoc "bits" file))))

    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "metaWeblog.newMediaObject")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "name")
                                            (value nil ,file-name))
                                    (member nil
                                            (name nil "bits")
                                            (base64 nil ,file-bits))
                                    (member nil
                                            (name nil "type")
                                            (value nil ,file-type))
                                    (member nil
                                            (name nil "overwrite")
                                            (value nil "t")))))
                 )))))))


(provide 'metaweblog)
