;;; test-metaweblog.el --- tests for metaweblog.el
;; Copyright (C) 2012 Puneeth Chaganti

(require 'metaweblog)

;;;;; Test env-setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq blog-xmlrpc "http://localhost/xmlrpc.php"
      blog-user "admin"
      blog-pass "test123"
      blog-id "1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-string (length)
  "Return a random string of given length"
  (let ((alpha "abcdefghijklmnopqrstuvwxyz")
        (char-list))
    (dotimes (char length)
      (setq char-list (cons (string (elt "abcdefghijklmnopqrstuvwxyz" (random 25))) char-list)))
    (mapconcat 'identity char-list "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-new-category ()
  "Test if creating a new category works."
  (let* ((category (random-string 8))
         ;; Add new category
         (category-id (wp-new-category blog-xmlrpc blog-user blog-pass blog-id category)))
    ;; List all categories and check if our category is present that list
    (setq categories (metaweblog-get-categories blog-xmlrpc blog-user blog-pass blog-id))
    (dolist (cat categories)
      (if (equal (cdr (assoc "categoryId" cat)) category-id)
          (should (equal (cdr (assoc "categoryName" cat)) category))))))

(ert-deftest test-new-tag ()
  "Test if getting tags works.  This test does nothing, just
checks if Wordpress version has the API.
FIXME: Make this a real test ..."
  (let* ()
    (setq tag-list (wp-get-tags blog-xmlrpc blog-user blog-pass blog-id))))

(ert-deftest test-pages ()
  "Test if creating, listing, fetching content and deleting pages works."
  (let* ((content '(("date" . "20120817T18:30:00+0000")
                    ("title" . "Hello World")
                    ("tags" "org2blog" "emacs")
                    ("categories" "org2blog" "emacs")
                    ("post-id")
                    ("parent" . "0")
                    ("excerpt" . "")
                    ("permalink" . "")
                    ("description" . "<p>Test content.</p>")))

         ;; Post a new page
         (page-id (wp-new-page blog-xmlrpc blog-user blog-pass blog-id content nil))
         (fetched-page (metaweblog-get-post blog-xmlrpc blog-user blog-pass page-id))
         (new-content "<p>New Content</p>"))

    ;; Check if fetched-page's content and posted content match
    (should (equal (cdr (assoc "description" fetched-page)) (cdr (assoc "description" content))))

    ;; Change content and post again...
    (setcdr (assoc "description" content) new-content)
    (wp-edit-page blog-xmlrpc blog-user blog-pass blog-id page-id content nil)
    ;; Fetch page again and check if content changed
    (setq fetched-page (metaweblog-get-post blog-xmlrpc blog-user blog-pass page-id))
    (should (equal (cdr (assoc "description" fetched-page)) (cdr (assoc "description" content))))

    ;; List pages
    ;; Check if our page is listed in minimal listing
    (dolist (page (wp-get-pagelist blog-xmlrpc blog-user blog-pass blog-id))
      (if (equal (cdr (assoc "post_id" page)) page-id)
          (should (equal (cdr (assoc "page_title" page)) (cdr (assoc "title" content))))))

    ;; Check if our page is listed in full listing
    (dolist (page (wp-get-pages blog-xmlrpc blog-user blog-pass blog-id))
      (if (equal (cdr (assoc "post_id" page)) page-id)
          (should (equal (cdr (assoc "page_title" page)) (cdr (assoc "title" content))))))

    ;; Delete page
    (wp-delete-page blog-xmlrpc blog-id blog-user blog-pass page-id)))

(ert-deftest test-posts ()
  "Test if creating, listing, fetching content and deleting posts works."
  (let* ((content '(("date" . "20120817T18:30:00+0000")
                    ("title" . "Hello World")
                    ("tags" "org2blog" "emacs")
                    ("categories" "org2blog" "emacs")
                    ("post-id")
                    ("parent" . "0")
                    ("excerpt" . "")
                    ("permalink" . "")
                    ("description" . "<p>Test content.</p>")))

         ;; Make a new post
         (post-id (metaweblog-new-post blog-xmlrpc blog-user blog-pass blog-id content nil))
         (fetched-post (metaweblog-get-post blog-xmlrpc blog-user blog-pass post-id))
         (new-content "<p>New Content</p>"))

    ;; Check if fetched-post's content and posted content match
    (should (equal (cdr (assoc "description" fetched-post)) (cdr (assoc "description" content))))

    ;; Change content and post again...
    (setcdr (assoc "description" content) new-content)
    (metaweblog-edit-post blog-xmlrpc blog-user blog-pass post-id content nil)
    ;; Fetch post again and check if content changed
    (setq fetched-post (metaweblog-get-post blog-xmlrpc blog-user blog-pass post-id))
    (should (equal (cdr (assoc "description" fetched-post)) (cdr (assoc "description" content))))

    ;; List recent posts and check if our post is listed
    (dolist (post (metaweblog-get-recent-posts blog-xmlrpc blog-id blog-user blog-pass 1000))
      (when (equal (cdr (assoc "postid" post)) post-id)
        (should (equal (cdr (assoc "title" post)) (cdr (assoc "title" content))))
        (should (equal (cdr (assoc "description" post)) (cdr (assoc "description" content))))))

    ;; Delete post
    (metaweblog-delete-post blog-xmlrpc blog-user blog-pass post-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: Write tests for these functions ...
;; (defun get-file-properties (file)
;;   "Gets the properties of a file. Returns an assoc list with
;; name - file name
;; bits - data of the file as a base64 encoded string
;; type - mimetype of file deduced from extension.")

;; (defun metaweblog-upload-file (blog-xmlrpc user-name password blog-id file)
;;   "Uploads file to the blog. FILE will be an alist name, type,
;; bits, as keys mapped to name of the file, mime type and the
;; data.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
