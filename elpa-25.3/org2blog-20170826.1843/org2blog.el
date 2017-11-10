;;; org2blog.el --- blog from Org mode to wordpress

;; Copyright (C) 2010 Benjamin Beckwith <bnbeckwith@gmail.com>
;; Copyright (C) 2010 Marcel van der Boom <marcel@hsdev.com>
;; Copyright (C) 2010-2014 Puneeth Chaganti <punchagan+org2blog@muse-amuse.in>
;; Copyright (C) 2010 Sacha Chua <sacha@sachachua.com>
;; Copyright (C) 2010 Giovanni Moretti <Giovanni@reflections.co.nz>
;; Copyright (C) 2011 Mykola Nikishov <mn@mn.com.ua>
;; Copyright (C) 2010 Matt Price <matt@roke.mercey.dyndns.org>
;; Copyright (C) 2013 Peter Vasil <mail@petervasil.net>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Version: 1.0.1
;; Keywords: orgmode, wordpress, blog

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

;; A portion of the code in this file is based on blog.el posted to
;; http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01576.html
;; copyrighted by Ashish Shukla. The Copyright notice from that file is
;; given below.

;; blog.el -- a wordpress posting client
;; Copyright (C) 2008 Ashish Shukla

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


(require 'org)
(when (version< (org-version) "8.3")
  (error "Require org-version>=8.3; Current version: %s" (org-version)))

(require 'xml-rpc)
(require 'metaweblog)
(require 'ox-wp)
(require 'htmlize)

(defgroup org2blog/wp nil
  "Post to weblogs from Emacs"
  :group 'org2blog/wp)

(defcustom org2blog/wp-blog-alist nil
  "Association list to set information for each blog.
Each element of the alist is a blog name.  The CAR of each
element is a string, uniquely identifying the project.  The CDR
of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the blog.

     (:property value :property value ... )

When a property is given a value in org2blog/wp-blog-alist, its
setting overrides the value of the corresponding user
variable (if any) during publishing.

Most properties are optional, but some should always be set:

  :url                     xmlrpc url of the blog.
  :username                username to be used.

All the other properties are optional. They over-ride the global variables.

  :password                password to be used
  :default-title           `org2blog/wp-default-title'
  :default-categories      `org2blog/wp-default-categories'
                           Use a list of categories.
                           (\"category1\" \"category2\" ...)
  :tags-as-categories      `org2blog/wp-use-tags-as-categories'
  :confirm                 `org2blog/wp-confirm-post'
  :show                    `org2blog/wp-show-post-in-browser'
  :keep-new-lines          `org2blog/wp-keep-new-lines'
  :wp-latex                `org2blog/wp-use-wp-latex'
  :wp-code                 `org2blog/wp-use-sourcecode-shortcode'
  :track-posts             `org2blog/wp-track-posts'
"
  :group 'org2blog/wp
  :type '(alist :value-type plist))

(defcustom org2blog/wp-default-categories '("Uncategorized" "Hello")
  "Default list of categories"
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-buffer-template
  "#+ORG2BLOG:
#+DATE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: %s
#+TAGS:
#+DESCRIPTION:
#+TITLE: %s
\n"
  "The default template to be inserted in a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-template-prefix nil
  "A prefix to the default template used for a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-format-function 'org2blog/wp-format-buffer
  "Function formatting a buffer according to `org2blog/wp-buffer-template'."
  :group 'org2blog/wp
  :type 'function)

(defcustom org2blog/wp-default-title "Hello, World"
  "Title of the new post"
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-use-tags-as-categories nil
  "Non-nil means assign :tags: to Wordpress categories instead."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-confirm-post nil
  "Non-nil means confirm before Publishing a post."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-show-post-in-browser 'ask
  "A variable to configure if you want to view your post/draft in
the browser.  Setting it to 'ask will prompt you before opening
it in the browser.  Setting it to 'show will show it without
prompting.  Set it to nil, to turn off viewing posts in the
browser."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-keep-new-lines nil
  "Non-nil means do not strip newlines."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-sourcecode-shortcode nil
  "Non-nil means convert <pre> tags to WP sourcecode blocks."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-wp-latex t
  "Non-nil means convert LaTeX to WP latex blocks."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-shortcode-langs-map nil
  "Association list for source code languages supported by Org
and by SyntaxHighlighter.  Each element of the list maps the
orgmode source code language (key) to the language spec that
should be used for syntax highlighting in shortcode blocks."
  :group 'org2blog/wp
  :type '(alist :key-type string :value-type string))

(defcustom org2blog/wp-track-posts
  (list ".org2blog.org" "Posts")
  ".org file in which to save logs about posts, and corresponding headline
in file under which the logs should be added."
  :group 'org2blog/wp
  :type '(list string string))

(defcustom org2blog/wp-keymap-prefix
  "C-c M-p"
  "Key sequence which forms the common prefix for key-bindings in
this mode.  If this is changed,
`org2blog/wp-reload-entry-mode-map' must be called before it
takes effect."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-image-thumbnails nil
  "Non-nil means WordPress thumbnail links to full-size image."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-image-thumbnail-size "medium_large"
  "Select which size thumbnail should be used for images.

  Choices are: thumbnail (150px), medium (300px),
  medium_large (768px) or large (1024px)"
  :group 'org2blog/wp
  :type 'string)

(defvar org2blog/wp-blog nil
  "Parameters of the currently selected blog.")

(defvar org2blog/wp-blog-name nil
  "Name of the blog, to pick from `org2blog/wp-blog-alist'")

(defvar org2blog/wp-categories-list nil
  "List of weblog categories")

(defvar org2blog/wp-tags-list nil
  "List of weblog tags")

(defvar org2blog/wp-pages-list nil
  "List of WP pages.")

(defvar org2blog/wp-server-xmlrpc-url nil
  "Weblog server XML-RPC URL")

(defvar org2blog/wp-server-userid nil
  "Weblog server user id")

(defvar org2blog/wp-server-blogid nil
  "Weblog ID")

(defvar org2blog/wp-entry-mode-map nil
  "Keymap for blog entry buffer")

(defvar org2blog/wp-logged-in nil
  "Flag whether user is logged-in or not")

(defvar org2blog/wp-buffer-name "*org2blog/wp-%s*"
  "Name of the blog buffer")

(defvar org2blog/wp-buffer-kill-prompt t
  "Ask before killing buffer")
(make-variable-buffer-local 'org2blog/wp-buffer-kill-prompt)

(defconst org2blog/wp-version "1.0.1"
  "Current version of blog.el")

(defvar org2blog/wp-mode-hook nil
  "Hook to run upon entry into mode.")

(defvar org2blog/wp-after-new-post-or-page-functions nil
  "Abnormal hook run after a new post or page is created.
Each function is called with one argument, the object
representing the aforementioned post or page.")

(defvar org2blog/wp-export-options
  '(
    :section-numbers nil
                     :with-priority nil
                     :with-sub-superscript nil
                     :with-toc nil
                     :with-tags nil
                     :with-todo-keywords nil
                     )
  "Export options to be used when exporting buffers and subtrees.
Look at `org-export-options-alist' for the available options.
Also, note that these options are over-ridden by in-file
options.")

(defvar org2blog/wp-server-pass nil)

(defun org2blog/wp-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org2blog/wp-buffer-kill-prompt
         (not (buffer-file-name)))
      (if (y-or-n-p "Save entry?")
          (progn
            (save-buffer)
            (org2blog/wp-save-details (org2blog/wp--export-as-post) nil
                                      (y-or-n-p "Published?"))))))

(defun org2blog/wp-define-key (suffix function)
  "Define a key sequence in the mode's key map with the prefix
given by `org2blog/wp-keymap-prefix', and the given suffix."
  (let ((keyseq (read-kbd-macro (concat org2blog/wp-keymap-prefix " " suffix))))
    (define-key org2blog/wp-map keyseq function)))

(defun org2blog/wp-init-entry-mode-map ()
  "Initialize `org2blog/wp-entry-mode-map' based on the prefix
key sequence defined by `org2blog/wp-keymap-prefix'."
  (setq org2blog/wp-entry-mode-map
        (let ((org2blog/wp-map (make-sparse-keymap)))
          (set-keymap-parent org2blog/wp-map org-mode-map)
          (org2blog/wp-define-key "p" 'org2blog/wp-post-buffer-and-publish)
          (org2blog/wp-define-key "P" 'org2blog/wp-post-buffer-as-page-and-publish)
          (org2blog/wp-define-key "d" 'org2blog/wp-post-buffer)
          (org2blog/wp-define-key "D" 'org2blog/wp-post-buffer-as-page)
          (org2blog/wp-define-key "t" 'org2blog/wp-complete-category)
          org2blog/wp-map)))

(defun org2blog/wp-reload-entry-mode-map ()
  "Re-initialize `org2blog/wp-entry-mode-map' based on the prefix
key sequence defined by `org2blog/wp-keymap-prefix' and update
`minor-mode-map-alist' accordingly."
  (interactive)
  (org2blog/wp-init-entry-mode-map)
  (let ((keymap (assoc 'org2blog/wp-mode minor-mode-map-alist)))
    (setcdr keymap org2blog/wp-entry-mode-map)))

;; Set the mode map for org2blog.
(unless org2blog/wp-entry-mode-map (org2blog/wp-init-entry-mode-map))

;;;###autoload
(define-minor-mode org2blog/wp-mode
  "Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'."

  :init-value nil
  :lighter " o2b"
  :group 'org2blog/wp
  :keymap org2blog/wp-entry-mode-map

  (if org2blog/wp-mode
      (run-mode-hooks 'org2blog/wp-mode-hook)))

;;;###autoload
(defun org2blog/wp-org-mode-hook-fn ()
  "Enable `org2blog/wp-mode' when `#+ORG2BLOG:' is present.
   Add this to `org-mode-hook'."
  (with-current-buffer (current-buffer)
    (when (org2blog/wp-get-option "ORG2BLOG")
      (org2blog/wp-mode t))))

(defun org2blog/wp-create-categories (categories)
  "Prompt and create new categories on WordPress."
  (mapcar
   (lambda (cat)
     (if (and (not (member cat org2blog/wp-categories-list))
              (y-or-n-p (format "Create '%s' category? " cat)))
         (wp-new-category org2blog/wp-server-xmlrpc-url
                          org2blog/wp-server-userid
                          org2blog/wp-server-pass
                          org2blog/wp-server-blogid
                          cat))
     (add-to-list 'org2blog/wp-categories-list cat))
   categories))

(defun org2blog/wp-password ()
  "Prompt for, and set password."
  (interactive)
  (setq org2blog/wp-server-pass (read-passwd "Weblog password? ")))

(defun org2blog/wp-get-blog-name ()
  "Get the blog name from a post -- buffer or subtree.
NOTE: Checks for subtree only when buffer is narrowed."
  (let ((blog-name
         (if (org-buffer-narrowed-p)
             (or (org-entry-get (point) "BLOG") "")
           (or (org2blog/wp-get-option "blog") ""))))
    (or (and (assoc blog-name org2blog/wp-blog-alist) blog-name) nil)))

(defun org2blog/wp-correctly-login ()
  "Relogin to correct blog, if blog-name is found and different
from currently logged in."
  (let ((blog-name (org2blog/wp-get-blog-name)))
    (when (and blog-name (not (equal blog-name org2blog/wp-blog-name)))
      (org2blog/wp-logout))
    (unless org2blog/wp-logged-in
      (org2blog/wp-login blog-name))))

;;;###autoload
(defun org2blog/wp-login (&optional blog-name)
  "Logs into the blog. Initializes the internal data structures."
  (interactive)
  (if (not org2blog/wp-blog-alist)
      (error "Set `org2blog/wp-blog-alist' to be able to use org2blog."))
  (let ()
    (setq org2blog/wp-blog-name
          (or
           ;; Use the provided name
           blog-name
           ;; OR Use the only entry in alist
           (and (equal (length org2blog/wp-blog-alist) 1)
                (car (car org2blog/wp-blog-alist)))
           ;; OR Prompt user
           (completing-read
            "Blog to login into? ([Tab] to see list): "
            (mapcar 'car org2blog/wp-blog-alist) nil t)))
    (unless (> (length org2blog/wp-blog-name) 1)
      (error "Invalid blog name"))
    (setq org2blog/wp-blog (assoc org2blog/wp-blog-name org2blog/wp-blog-alist)
          org2blog/wp-server-xmlrpc-url (plist-get (cdr org2blog/wp-blog) :url)
          org2blog/wp-server-userid (plist-get (cdr org2blog/wp-blog) :username)
          org2blog/wp-server-blogid (or (plist-get (cdr org2blog/wp-blog) :id) "1")
          org2blog/wp-server-pass
          (or
           (plist-get (cdr org2blog/wp-blog) :password)
           (read-passwd (format "%s Weblog password? " org2blog/wp-blog-name)))
          ;; Fetch and save category list
          org2blog/wp-categories-list
          (mapcar (lambda (category) (cdr (assoc "categoryName" category)))
                  (metaweblog-get-categories org2blog/wp-server-xmlrpc-url
                                             org2blog/wp-server-userid
                                             org2blog/wp-server-pass
                                             org2blog/wp-server-blogid))
          ;; Fetch and save tag list
          org2blog/wp-tags-list
          (mapcar (lambda (tag) (cdr (assoc "slug" tag)))
                  (wp-get-tags org2blog/wp-server-xmlrpc-url
                               org2blog/wp-server-userid
                               org2blog/wp-server-pass
                               org2blog/wp-server-blogid))
          ;; Fetch and save page list
          org2blog/wp-pages-list
          (mapcar (lambda (pg)
                    (cons (cdr (assoc "page_title" pg))
                          (cdr (assoc "page_id" pg))))
                  (wp-get-pagelist org2blog/wp-server-xmlrpc-url
                                   org2blog/wp-server-userid
                                   org2blog/wp-server-pass
                                   org2blog/wp-server-blogid)))
    (setq org2blog/wp-logged-in t)
    (message "Logged in")))

(defun org2blog/wp-logout()
  "Logs out from the blog and clears. Clears the internal data structures."
  (interactive)
  (setq org2blog/wp-server-xmlrpc-url nil
        org2blog/wp-server-userid nil
        org2blog/wp-server-blogid nil
        org2blog/wp-server-pass nil
        org2blog/wp-categories-list nil
        org2blog/wp-tags-list nil
        org2blog/wp-pages-list nil
        org2blog/wp-logged-in nil)
  (message "Logged out"))

;;;###autoload
(defun org2blog/wp-new-entry ()
  "Creates a new buffer for a blog entry."
  (interactive)
  ;; Prompt for login
  (if (and (not org2blog/wp-logged-in)
           (y-or-n-p "You are not logged in. Login?"))
      (org2blog/wp-login))

  ;; Generate new buffer
  (let ((org2blog/wp-buffer (generate-new-buffer
                             (format org2blog/wp-buffer-name org2blog/wp-blog-name))))
    (switch-to-buffer org2blog/wp-buffer)
    (add-hook 'kill-buffer-hook 'org2blog/wp-kill-buffer-hook nil 'local)
    (org-mode)
    ;; Insert the post template
    (insert
     (or org2blog/wp-buffer-template-prefix "")
     (funcall org2blog/wp-buffer-format-function
              org2blog/wp-buffer-template))
    (org2blog/wp-mode t)))

(defun org2blog/wp-format-buffer (buffer-template)
  "Default buffer formatting function."
  (format buffer-template
          (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
          (mapconcat
           (lambda (cat) cat)
           (or (plist-get (cdr org2blog/wp-blog) :default-categories)
               org2blog/wp-default-categories)
           ", ")
          (or (plist-get (cdr org2blog/wp-blog) :default-title)
              org2blog/wp-default-title)))

(defun org2blog/wp-upload-files-replace-urls (text)
  "Uploads files, if any in the html, and changes their links"
  ;; text is the html as generated by orgmode HTML export
  (let ((file-all-urls nil)
        file-name file-web-url beg file-thumbnail-name upload-ret
        (file-regexp "<a href=\"\\(.*?\\)\"\\|<img src=\"\\(.*?\\)\""))
    ;; save point and mark, bring us back at the end
    (save-excursion
      (while (string-match file-regexp text beg)
        (setq file-name
              (if (match-beginning 1)
                  (substring text (match-beginning 1) (match-end 1))
                (substring text (match-beginning 2) (match-end 2))))
        (setq file-name (save-match-data (if (string-match "^file:" file-name)
                                             (substring file-name 7)
                                           file-name)))
        (setq beg (match-end 0))
        (if (save-match-data (not (or
                                   (string-match org-plain-link-re file-name)
                                   (string-match "^.*#" file-name)
                                   (string-equal (file-name-nondirectory file-name) ""))))

            (progn
              (goto-char (point-min))
              (if (re-search-forward (concat "^.*# "
                                             (regexp-quote file-name)
                                             " ") nil t 1)
                  ;; THEN
                  ;; read from right after filename to get URL + thumbnail
                  ;; then split-string on space for the web-url and thumbnail
                  ;; if there is no third part, thumbnail will be nil
                  (let ((url-thumb-parts (split-string (buffer-substring-no-properties
                                                        (point)
                                                        (or (end-of-line) (point))) " ")))
                    (setq file-web-url (car url-thumb-parts))
                    ;; we want just the name (cdr gives a list or nil)
                    ;; nth will give the name or nil if there's nothing
                    (setq file-thumbnail-name (nth 1 url-thumb-parts)))

                ;; ELSE
                ;; returns alist with id, file, url, type
                (setq upload-ret (metaweblog-upload-file
                                  org2blog/wp-server-xmlrpc-url
                                  org2blog/wp-server-userid
                                  org2blog/wp-server-pass
                                  org2blog/wp-server-blogid
                                  (get-file-properties file-name)))
                ;; grab url from returned alist
                (setq file-web-url
                      (cdr (assoc "url"
                                  upload-ret)))

                ;; get thumbnail information if we're going to link to it
                (if org2blog/wp-image-thumbnails
                    ;; get the attachment_id so we can find the thumbnail
                    (let* ((attachment-id (cdr (assoc "id" upload-ret)))
                           ;; http://codex.wordpress.org/XML-RPC_WordPress_API/Media
                           ;; get name of thumbnail image, in this case medium at 300px
                           (media-item-info
                            (xml-rpc-method-call org2blog/wp-server-xmlrpc-url
                                                 "wp.getMediaItem"
                                                 org2blog/wp-server-blogid
                                                 org2blog/wp-server-userid
                                                 org2blog/wp-server-pass
                                                 attachment-id)))

                      ;; media-item-info -> metadata -> sizes -> medium -> file == basename-300x???.jpg
                      ;; is there no built-in shortcut to access nested alists?
                      ;; https://github.com/nicferrier/emacs-dotassoc
                      ;; we end up with just the basename of the requested size thumb in
                      ;; medium-file-name
                      (let ((media-metadata (cdr (assoc "metadata" media-item-info))))
                        (setq file-thumbnail-name
                              (cdr (assoc "file"
                                          (cdr (assoc org2blog/wp-image-thumbnail-size
                                                      (cdr (assoc "sizes" media-metadata))))))))
                      ) ;; let*

                  ;; ELSE
                  (setq file-thumbnail-name nil))

                (goto-char (point-max))
                (org2blog/wp--new-line-no-indent)
                (insert (concat "# " file-name " " file-web-url
                                (if file-thumbnail-name (concat  " " file-thumbnail-name)))))

              ;; we retrieved file-web-url either via the API or from the org
              ;; add it to the list of replacements that we'll do.
              ;; (list (cons a b)) => ((a . b)) which can then be appended to
              ;; file-all-urls; cpbotha changed to a list of 3-element lists
              (setq file-all-urls
                    (append file-all-urls
                            (list (list file-name
                                        file-web-url
                                        file-thumbnail-name)))))))

      (dolist (file file-all-urls)

        (if (and (nth 2 file) org2blog/wp-image-thumbnails)
            ;; if thumbnail available AND user said yes, the new
            ;; image-thumbnail way:
            ;; 1. first replace normal href as always
            (progn
              (setq text (replace-regexp-in-string
                          (concat "\\(<a href=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                          (concat "\\1" (nth 1 file)) text))

              ;; 2. but then replace <img> with <a href="full"><img src="thumb">
              ;; with let*, subsequent bindings can refer to preceding bindings
              (let*
                  ((file-web-url (nth 1 file))
                   (file-thumbnail-name (nth 2 file))
                   ;; find the position of the last / measured from the end
                   (idx (string-match-p (regexp-quote "/")
                                        (concat (reverse (string-to-list file-web-url)))))
                   ;; chop off just the filename, replace with thumbnail name
                   (thumbnail-url (concat (substring file-web-url 0 (- idx)) file-thumbnail-name)))

                ;; replace: <img src="file://./img/blabla.png" alt="volume_cutting.png" />
                ;; note that after blabla.png" we use non-greedy matching until />
                (setq text (replace-regexp-in-string
                            (concat "<img src=\"\\(file://\\)?"
                                    (regexp-quote (car file))
                                    "\"\\(.*?\\)/>")
                            (concat "<a href=\"" file-web-url "\">"
                                    "<img src=\"" thumbnail-url "\"\\2/></a>")
                            text)))
              ) ;; progn

          ;; ELSE:
          ;; the straight-forward no-image-thumbnail way:
          ;; replace <a href="file://THEFILENAME"> or <img src="file://THEFILENAME">
          ;; with <a href="url"> or <img src="url">
          (setq text (replace-regexp-in-string
                      (concat "\\(<a href=\"\\|<img src=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                      (concat "\\1" (nth 1 file)) text))
          ) ;; if
        ) ;; dolist

      ) ;; save-excursion
    text))

(defun org2blog/wp-get-option (opt)
  "Gets an the value of the option OP from a buffer."
  (let* ((r (org-make-options-regexp (list (upcase opt) (downcase opt)))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward r nil t 1)
          (match-string-no-properties 2)))))

;;;###autoload
(defun org2blog/wp-post-buffer-and-publish ()
  "Post buffer and mark it as published"
  (interactive)
  (org2blog/wp-post-buffer t))

(defun org2blog/wp-get-post-or-page (post-or-page-id)
  "Retrieve a post or page given its `POST-OR-PAGE-ID'. For information about its fields see URL
  `https://codex.wordpress.org/XML-RPC_MetaWeblog_API#metaWeblog.newPost'"
  (interactive)
  (let ((post-or-page (metaweblog-get-post org2blog/wp-server-xmlrpc-url
                                           org2blog/wp-server-userid
                                           org2blog/wp-server-pass
                                           post-or-page-id)))
    post-or-page))

;;;###autoload
(defun org2blog/wp-post-buffer (&optional publish subtree-p)
  "Posts new blog entry to the blog or edits an existing entry."
  (interactive "P")
  (org2blog/wp-mode t) ;; turn on org2blog-wp-mode
  (org2blog/wp-correctly-login)
  (save-excursion
    (save-restriction
      (let ((post (org2blog/wp--export-as-post subtree-p))
            (confirm (and
                      (if (plist-member (cdr org2blog/wp-blog) :confirm)
                          (plist-member (cdr org2blog/wp-blog) :confirm)
                        org2blog/wp-confirm-post)
                      publish))
            (show (or (plist-member (cdr org2blog/wp-blog) :show)
                      org2blog/wp-show-post-in-browser))
            post-id)
        (org2blog/wp-create-categories (cdr (assoc "categories" post)))
        (setq post-id (cdr (assoc "post-id" post)))
        (when confirm
          (if (not (y-or-n-p (format "Publish %s ?"
                                     (cdr (assoc "title" post)))))
              (error "Post cancelled.")))
        (if post-id
            (metaweblog-edit-post org2blog/wp-server-xmlrpc-url
                                  org2blog/wp-server-userid
                                  org2blog/wp-server-pass
                                  post-id
                                  post
                                  publish)
          (progn (setq post-id (metaweblog-new-post org2blog/wp-server-xmlrpc-url
                                                    org2blog/wp-server-userid
                                                    org2blog/wp-server-pass
                                                    org2blog/wp-server-blogid
                                                    post
                                                    publish))
                 (run-hook-with-args
                  'org2blog/wp-after-new-post-or-page-functions
                  (org2blog/wp-get-post-or-page post-id)))
          (if subtree-p
              (progn
                (org-entry-put (point) "POSTID" post-id)
                (org-entry-put (point) "BLOG" org2blog/wp-blog-name))
            (goto-char (point-min))
            (insert (concat "#+BLOG: " org2blog/wp-blog-name "\n"))
            (insert (concat "#+POSTID: " post-id "\n"))))
        (org2blog/wp-save-details post post-id publish subtree-p)
        (message (if publish
                     "Published (%s): %s"
                   "Draft (%s): %s")
                 post-id
                 (cdr (assoc "title" post)))
        (when (or (equal show 'show)
                  (and
                   (equal show 'ask)
                   (y-or-n-p
                    "[For drafts, ensure you login] View in browser? y/n")))
          (if subtree-p
              (org2blog/wp-preview-subtree-post)
            (org2blog/wp-preview-buffer-post)))))))


(defun org2blog/wp-post-buffer-as-page-and-publish ()
  "Alias to post buffer and mark it as published"
  (interactive)
  (org2blog/wp-post-buffer-as-page t))

(defun org2blog/wp-post-buffer-as-page (&optional publish subtree-p)
  "Posts new page to the blog or edits an existing page."
  (interactive "P")
  (org2blog/wp-correctly-login)
  (save-excursion
    (save-restriction
      (widen)
      (let ((post (org2blog/wp--export-as-post subtree-p))
            (confirm (and
                      (if (plist-member (cdr org2blog/wp-blog) :confirm)
                          (plist-member (cdr org2blog/wp-blog) :confirm)
                        org2blog/wp-confirm-post)
                      publish))
            (show (or (plist-member (cdr org2blog/wp-blog) :show)
                      org2blog/wp-show-post-in-browser))
            post-id)
        (org2blog/wp-create-categories (cdr (assoc "categories" post)))
        (setq post-id (cdr (assoc "post-id" post)))
        (when confirm
          (if (not (y-or-n-p (format "Publish %s ?"
                                     (cdr (assoc "title" post)))))
              (error "Post cancelled.")))
        (if post-id
            (wp-edit-page org2blog/wp-server-xmlrpc-url
                          org2blog/wp-server-userid
                          org2blog/wp-server-pass
                          org2blog/wp-server-blogid
                          post-id
                          post
                          publish)
          (progn (setq post-id (wp-new-page org2blog/wp-server-xmlrpc-url
                                            org2blog/wp-server-userid
                                            org2blog/wp-server-pass
                                            org2blog/wp-server-blogid
                                            post
                                            publish))
                 (run-hook-with-args
                  'org2blog/wp-after-new-post-or-page-functions
                  (org2blog/wp-get-post-or-page post-id)))
          (setq org2blog/wp-pages-list
                (mapcar (lambda (pg)
                          (cons (cdr (assoc "title" pg))
                                (cdr (assoc "page_id" pg))))
                        (wp-get-pagelist org2blog/wp-server-xmlrpc-url
                                         org2blog/wp-server-userid
                                         org2blog/wp-server-pass
                                         org2blog/wp-server-blogid)))
          (if subtree-p
              (org-entry-put (point) "POSTID" post-id)
            (goto-char (point-min))
            (insert (concat "#+POSTID: " post-id "\n"))))
        (org2blog/wp-save-details post post-id publish subtree-p)
        (message (if publish
                     "Published (%s): %s"
                   "Draft (%s): %s")
                 post-id
                 (cdr (assoc "title" post)))
        (when (or (equal show 'show)
                  (and
                   (equal show 'ask)
                   (y-or-n-p
                    "[For drafts, ensure you login] View in browser? y/n")))
          (if subtree-p
              (org2blog/wp-preview-subtree-post)
            (org2blog/wp-preview-buffer-post)))))))

(defun org2blog/wp-delete-entry (&optional post-id)
  (interactive "P")
  (org2blog/wp-correctly-login)
  (if (null post-id)
      (setq post-id (org2blog/wp-get-option "POSTID")))
  (metaweblog-delete-post org2blog/wp-server-xmlrpc-url
                          org2blog/wp-server-userid
                          org2blog/wp-server-pass
                          post-id)
  (message "Post Deleted"))

(defun org2blog/wp-delete-page (&optional page-id)
  (interactive "P")
  (org2blog/wp-correctly-login)
  (if (null page-id)
      (setq page-id (org2blog/wp-get-option "POSTID")))
  (wp-delete-page org2blog/wp-server-xmlrpc-url
                  org2blog/wp-server-blogid
                  org2blog/wp-server-userid
                  org2blog/wp-server-pass
                  page-id)
  (message "Page Deleted"))

(defun org2blog/wp-save-details (post pid pub subtree-p)
  "Save the details of posting, to a file."
  (save-excursion
    (when (if (plist-member (cdr org2blog/wp-blog) :track-posts)
              (car (plist-get (cdr org2blog/wp-blog) :track-posts))
            (car org2blog/wp-track-posts))
      (let* ((o2b-id (if subtree-p
                         (concat "id:" (org-id-get nil t))
                       (buffer-file-name)))
             (log-file (if (plist-member (cdr org2blog/wp-blog) :track-posts)
                           (car (plist-get (cdr org2blog/wp-blog) :track-posts))
                         (car org2blog/wp-track-posts)))
             (log-file (if (file-name-absolute-p log-file)
                           log-file
                         (if org-directory
                             (expand-file-name log-file org-directory)
                           (message "org-track-posts: filename is ambiguous
use absolute path or set org-directory")
                           log-file)))
             (headline (if (plist-member (cdr org2blog/wp-blog) :track-posts)
                           (cadr (plist-get (cdr org2blog/wp-blog) :track-posts))
                         (cadr org2blog/wp-track-posts)))
             p)

        ;; Create the directory to place the log-file, if it doesn't exist.
        (make-directory (file-name-directory log-file) t)

        (when o2b-id
          (with-current-buffer (or (find-buffer-visiting log-file)
                                  (find-file-noselect log-file))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (show-all)
                (setq p (org-find-exact-headline-in-buffer headline))
                (if p
                    (progn (goto-char p) (org-narrow-to-subtree) (end-of-line))
                  (goto-char (point-max))
                  (progn (org-insert-heading t)
                         (insert headline)
                         (org-narrow-to-subtree)))
                (if (search-forward o2b-id nil t 1)
                    (progn
                      (org-back-to-heading)
                      (forward-thing 'whitespace)
                      (kill-line))
                  (org-insert-subheading t)))
              (org2blog/wp-update-details post o2b-id pid pub))
            (save-buffer)))))))

(defun org2blog/wp-update-details (post o2b-id pid pub)
  "Inserts details of a new post or updates details."
  (insert (format "[[%s][%s]]"
                  (if (cdr (assoc "subtree" post))
                      o2b-id
                    (concat "file:" o2b-id))
                  (cdr (assoc "title" post))))
  (org-entry-put (point) "POSTID" (or pid ""))
  (org-entry-put (point) "POST_DATE" (cdr (assoc "date" post)))
  (org-entry-put (point) "Published" (if pub "Yes" "No")))

(defun org2blog/wp-complete-category()
  "Provides completion for categories and tags."
  (interactive)
  (let* (current-pos tag-or-category-list tag-or-cat-list tag-or-cat-prompt)
    (setq current-pos (point))
    (forward-line 0)
    (forward-char 2)
    (if (or (looking-at "CATEGORY: ") (looking-at "TAGS: ")
           (looking-at "PARENT: "))
        (progn
          (cond
           ((looking-at "TAGS: ")
            (setq tag-or-cat-list org2blog/wp-tags-list)
            (setq tag-or-cat-prompt "Tag ?"))
           ((looking-at "CATEGORY: ")
            (setq tag-or-cat-list org2blog/wp-categories-list)
            (setq tag-or-cat-prompt "Category ?"))
           ((looking-at "PARENT: ")
            (setq tag-or-cat-list org2blog/wp-pages-list)
            (setq tag-or-cat-prompt "Parent ?")))
          (goto-char current-pos)
          (let ((word-match (or (current-word t) ""))
                (completion-match nil))
            (when word-match
              (setq completion-match (completing-read tag-or-cat-prompt tag-or-cat-list nil nil word-match))
              (when (stringp completion-match)
                (search-backward word-match nil t)
                (replace-match (concat completion-match ", ") nil t)))))
      (progn
        (goto-char current-pos)
        (command-execute (lookup-key org-mode-map (kbd "C-c t")))))))

;;;###autoload
(defun org2blog/wp-post-subtree (&optional publish)
  "Post the current entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-id-get nil t "o2b")
      (org2blog/wp-post-buffer publish t)
      (widen)
      (when (buffer-file-name) (save-buffer)))))

;;;###autoload
(defun org2blog/wp-post-subtree-as-page (&optional publish)
  "Post the current entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-id-get nil t "o2b")
      (org2blog/wp-post-buffer-as-page publish t)
      (widen)
      (when (buffer-file-name) (save-buffer)))))

;;;###autoload
(defun org2blog/wp-post-subtree-and-publish ()
  "Post subtree and mark it as published"
  (interactive)
  (org2blog/wp-post-subtree t))

;;;###autoload
(defun org2blog/wp-post-subtree-as-page-and-publish ()
  "Publish the current subtree as a page."
  (interactive)
  (org2blog/wp-post-subtree-as-page t))


;;;###autoload
(defun org2blog/wp-track-buffer ()
  "Save details of current buffer in the tracking file."
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (org2blog/wp-save-details (org2blog/wp--export-as-post) "" nil))))

;;;###autoload
(defun org2blog/wp-track-subtree ()
  "Save details of current subtree in the tracking file."
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org2blog/wp-save-details (org2blog/wp--export-as-post t) "" nil t)
      (widen))))

;;;###autoload
(defun org2blog/wp-preview-buffer-post ()
  "Preview the present buffer in browser, if posted."
  (interactive)
  (org2blog/wp-correctly-login)
  (let* ((postid (org2blog/wp-get-option "POSTID"))
         (url org2blog/wp-server-xmlrpc-url))
    (if (not postid)
        (message "This buffer hasn't been posted, yet.")
      (setq url (substring url 0 -10))
      (setq url (concat url "?p=" postid "&preview=true"))
      (browse-url url))))

;;;###autoload
(defun org2blog/wp-preview-subtree-post ()
  "Preview the present subtree in browser, if posted."
  (interactive)
  (org-narrow-to-subtree)
  (org2blog/wp-correctly-login)
  (widen)
  (let* ((postid (or (org-entry-get (point) "POSTID")
                    (org-entry-get (point) "POST_ID")))
         (url org2blog/wp-server-xmlrpc-url))
    (if (not postid)
        (message "This subtree hasn't been posted, yet.")
      (setq url (substring url 0 -10))
      (setq url (concat url "?p=" postid "&preview=true"))
      (browse-url url))))

(defun org2blog/wp-insert-post-or-page-link (&optional is-page)
  "Insert a link to the post (or page) with the given id, with
the title of the post (or page) as description."
  (interactive "P")
  (org2blog/wp-correctly-login)
  (let* ((post-list (if is-page
                        (wp-get-pagelist org2blog/wp-server-xmlrpc-url
                                         org2blog/wp-server-userid
                                         org2blog/wp-server-pass
                                         org2blog/wp-server-blogid)
                      (metaweblog-get-recent-posts org2blog/wp-server-xmlrpc-url
                                                   org2blog/wp-server-blogid
                                                   org2blog/wp-server-userid
                                                   org2blog/wp-server-pass
                                                   1000)))
         post-title post-id url title-id-map)
    (dolist (post post-list)
      (setq title-id-map (cons
                          (cons (cdr (assoc "title" post)) (cdr (assoc "postid" post)))
                          title-id-map)))
    ;; Ask user to select the title
    (setq post-title (completing-read
                      (if is-page "Select page: " "Select post: ")
                      title-id-map nil t)
          post-id (cdr (assoc post-title title-id-map)))
    (when post-title
      ;; "Generate" the actual url of the post
      (setq url (concat
                 (replace-regexp-in-string "xmlrpc\\.php$" "?p=" org2blog/wp-server-xmlrpc-url)
                 post-id))
      ;; Insert!
      (insert (format "[[%s][%s]]" url post-title)))))

;;;; Private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org2blog/wp--new-line-no-indent ()
  "Insert a new line without indenting."
  (insert (if use-hard-newlines hard-newline "\n")))

(defun org2blog/wp--collect-export-options ()
  "Return a plist of export options.

This can be passed on to the export functions to configure the
various export options."

  (let ((export-options org2blog/wp-export-options))

    (plist-put export-options :wp-keep-new-lines
               (if (plist-member (cdr org2blog/wp-blog) :keep-new-lines)
                   (plist-get (cdr org2blog/wp-blog) :keep-new-lines)
                 org2blog/wp-keep-new-lines))
    (plist-put export-options :wp-latex
               (if (plist-member (cdr org2blog/wp-blog) :wp-latex)
                   (plist-get (cdr org2blog/wp-blog) :wp-latex)
                 org2blog/wp-use-wp-latex))
    (plist-put export-options :wp-shortcode
               (if (plist-member (cdr org2blog/wp-blog) :wp-code)
                   (plist-get (cdr org2blog/wp-blog) :wp-code)
                 org2blog/wp-use-sourcecode-shortcode))
    (plist-put export-options :tags-as-categories
               (if (plist-member (cdr org2blog/wp-blog) :tags-as-categories)
                   (plist-get (cdr org2blog/wp-blog) :tags-as-categories)
                 org2blog/wp-use-tags-as-categories))
    (plist-put export-options :wp-shortcode-langs-map
               org2blog/wp-shortcode-langs-map)

    ;; Return Value
    export-options))

(defun org2blog/wp--convert-timestamp-to-iso (timestamp)
  "Convert org timestamp to ISO."
  (format-time-string
   "%Y%m%dT%T%z"
   (apply 'encode-time (org-parse-time-string timestamp))
   t))

(defun org2blog/wp--export-as-html (subtree-p export-options)
  "Return the html for the post."
  (save-excursion
    (org2blog/wp-upload-files-replace-urls
     (org-no-properties (org-wp-export-as-string nil subtree-p export-options)))))

(defun org2blog/wp--export-as-post (&optional subtree-p)
  "Parse an org2blog post (subtree or buffer)."

  (let* ((export-options (org2blog/wp--collect-export-options))
         (tags-as-categories (plist-get export-options :tags-as-categories)))

    (save-restriction
      (save-excursion
        ;; Get the required parameters for posting the blog-post
        (let ((post (if subtree-p
                        (org2blog/wp--parse-subtree-entry)
                      (org2blog/wp--parse-buffer-entry))))
          (when tags-as-categories
            (setcdr (assoc "categories" post) (cdr (assoc "tags" post)))
            (setcdr (assoc "tags" post) nil))

          (setcdr (assoc "date" post)
                  ;; Convert post date to ISO timestamp
                  (org2blog/wp--convert-timestamp-to-iso
                   ;; insert posting timestamp, else edits will change it
                   (org2blog/wp--insert-current-time subtree-p
                                                     (cdr (assoc "date" post)))))
          (setcdr (assoc "description" post)
                  (org2blog/wp--export-as-html subtree-p export-options))

          ;; Return value
          post)))))

(defun org2blog/wp--get-parent-id (parent)
  "Return a post's parent id.

If parent is the id of the parent page, the user need not be
logged in.  Otherwise, the user is prompted to login."

  (when (and parent (equal 0 (string-to-number parent)))
    (org2blog/wp-correctly-login))
  (if parent
      (or
       (cdr (assoc
             (car (split-string parent "\\( *, *\\)" t))
             org2blog/wp-pages-list))
       (number-to-string (string-to-number parent))
       "0")
    "0"))

(defun org2blog/wp--insert-current-time (subtree-p time)
  "Insert current time into the post, if no timestamp exists."
  (or time
     (let ((current-time
            (format-time-string (org-time-stamp-format t t)
                                (org-current-time))))
       (save-excursion
         (if subtree-p
             (org-entry-put (point) "POST_DATE" current-time)
           (goto-char (point-min))
           (insert (concat "#+DATE: " current-time "\n"))))

       current-time)))

(defun org2blog/wp--parse-buffer-entry ()
  "Parse an org2blog buffer entry.

The post object returned does not contain the exported html.
This post needs to be further processed by the
`org2blog/wp--export-as-post' function, to add the export html
and munge it a little to make it suitable to use with the
`metaweblog' functions. "

  (let*
      ((export-environment (org-export-with-buffer-copy (org-export-get-environment)))
       (parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (org2blog/wp-get-option "DATE"))
         (cons "title" (org-element-interpret-data
                        (or (plist-get export-environment :title)
                           "No Title")))
         (cons "description" nil)
         (cons "tags"
               (split-string (or (org2blog/wp-get-option "TAGS") "")
                             "\\( *, *\\)" t))
         (cons "categories"
               (split-string (or (org2blog/wp-get-option "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (org2blog/wp-get-option "POSTID"))
         (cons "parent" (org2blog/wp--get-parent-id
                         (org2blog/wp-get-option "PARENT")))
         (cons "excerpt" (org-element-interpret-data
                          (or (plist-get export-environment
                                        :description) "")))
         (cons "permalink" (or (org2blog/wp-get-option "PERMALINK") "")))))

    ;; Return value
    parsed-entry))

(defun org2blog/wp--parse-subtree-entry ()
  "Parse an org2blog subtree entry.

The post object returned does not contain the exported html.
This post needs to be further processed by the
`org2blog/wp--export-as-post' function, to add the export html
and munge it a little to make it suitable to use with the
`metaweblog' functions. "

  (let*
      ((parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (or (org-entry-get (point) "POST_DATE")
                         (org-entry-get (point) "SCHEDULED")
                         (org-entry-get (point) "DEADLINE")
                         (org-entry-get (point) "TIMESTAMP_IA")
                         (org-entry-get (point) "TIMESTAMP")))
         (cons "title" (or (org-entry-get (point) "TITLE")
                          (nth 4 (org-heading-components))))
         (cons "description" nil)
         (cons "tags" (or
                       (split-string (or (org-entry-get (point) "POST_TAGS") "") "\\( *, *\\)" t)
                       (mapcar 'org-no-properties (org-get-tags-at (point) nil))))
         (cons "categories"
               (split-string (or (org-entry-get (point) "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (or (org-entry-get (point) "POSTID")
                            (org-entry-get (point) "POST_ID")))
         (cons "parent" (org2blog/wp--get-parent-id
                         (org-entry-get (point) "PARENT")))
         (cons "excerpt" (org-entry-get (point) "DESCRIPTION"))
         (cons "permalink" (org-entry-get (point) "PERMALINK")))))

    ;; Return value
    parsed-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org2blog)
