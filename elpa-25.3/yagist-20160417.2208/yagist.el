;;; yagist.el --- Yet Another Emacs integration for gist.github.com

;; Original Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Contributors: Will Farrington <wcfarrington@gmail.com>
;;               Michael Ivey
;;               Phil Hagelberg
;;               Dan McKinley
;; Version: 0.8.13
;; Package-Version: 20160417.2208
;; Created: 21 Jul 2008
;; Keywords: tools
;; Package-Requires: ((cl-lib "0.3"))
;; URL: https://github.com/mhayashi1120/yagist.el

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; If you want to save encrypted token to ~/.gitconfig , install elisp from
;; following url. Or Melpa.
;;
;; https://github.com/mhayashi1120/Emacs-kaesar/raw/master/kaesar.el
;;
;; (setq yagist-encrypt-risky-config t)

;;; TODO:
;; * yagist-minor-mode
;;   API change VS repository change

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'derived)
(require 'easy-mmode)

(defgroup yagist nil
  "Simple gist application."
  :prefix "yagist-"
  :group 'applications)

(defcustom yagist-github-token nil
  "If non-nil, will be used as your GitHub OAuth token without checking
git-config(1)."
  :group 'yagist
  :type 'string)

(defcustom yagist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after they're
posted."
  :type 'boolean
  :group 'yagist)

(defcustom yagist-display-date-format "%Y-%m-%d %H:%M"
  "Date format displaying in `yagist-list' buffer."
  :type 'string
  :group 'yagist)

(defvar yagist-authenticate-function nil
  "Authentication function symbol.")
(make-obsolete-variable 'yagist-authenticate-function nil "0.8.4")

(defvar yagist-list-items-per-page nil
  "Number of gist to retrieve a page.")

(defcustom yagist-working-directory "~/.gist"
  "*Working directory where to go gist repository is."
  :type 'directory
  :group 'yagist)

(defcustom yagist-working-directory-alist nil
  "*Alist of gist id as key, value is directory path.

Example:
\(setq yagist-working-directory-alist
      `((\"1080701\" . \"~/mygist/Emacs-nativechecker\")))
"
  :type '(alist :key-type string
                :value-type directory)
  :group 'yagist)

(defcustom yagist-git-config-with-includes nil
  "*Call git-config(1) with `--includes' option "
  :type 'boolean
  :group 'yagist)

(defvar yagist-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'revert-buffer)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'forward-line)
    (define-key map "q" 'yagist-quit-window)
    map))

(defvar yagist-list--paging-info nil)
(make-variable-buffer-local 'yagist-list--paging-info)

(define-derived-mode yagist-list-mode fundamental-mode "YaGist"
  "Show your gist list"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'revert-buffer-function)
       'yagist-list-revert-buffer)
  (add-hook 'post-command-hook 'yagist-list--paging-retrieve nil t)
  (use-local-map yagist-list-mode-map))

(defun yagist--read-json (start end)
  (let* ((str (buffer-substring start end))
         (decoded (decode-coding-string str 'utf-8)))
    (json-read-from-string decoded)))

(defun yagist-request-0 (auth method url callback &optional json-or-params)
  (let* ((json (and (member method '("POST" "PATCH")) json-or-params))
         (params (and (member method '("GET" "DELETE")) json-or-params))
         (url-request-data (and json (concat (json-encode json) "\n")))
         (url-request-extra-headers
          `(("Authorization" . ,auth)))
         (url-request-method method)
         (url-max-redirection -1)
         (url (if params
                  (concat url "?" (yagist-make-query-string params))
                url)))
    (url-retrieve url callback (list url json-or-params))))

(defun yagist-request (method url callback &optional json-or-params)
  (let ((token (yagist-check-oauth-token)))
    (yagist-request-0
     (format "Bearer %s" token)
     method url callback json-or-params)))

;; http://developer.github.com/v3/oauth/#non-web-application-flow
(defun yagist-check-oauth-token ()
  (cond
   ((or yagist-github-token
        (yagist-config "oauth-token")))
   (t
    (browse-url "https://github.com/settings/applications")
    (error "You need to get OAuth Access Token by your browser"))))

;;;###autoload
(defun yagist-region (begin end &optional private name)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((description (read-from-minibuffer "Description: "))
         ;; cause of privacy reason,
         ;; set filename as empty if call from yagist-*-region function.
         ;; I think that highly expected upload just the region,
         ;; not a filename.
         (filename (or name (yagist-anonymous-file-name))))
    (yagist-request
     "POST"
     "https://api.github.com/gists"
     'yagist-created-callback
     `(("description" . ,description)
       ("public" . ,(if private :json-false 't))
       ("files" .
        ((,filename .
                    (("content" . ,(buffer-substring begin end))))))))))

(defun yagist-single-file-name ()
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file)))
    name))

(defun yagist-anonymous-file-name ()
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (file-name-extension name)))
    (concat "anonymous-gist." ext)))

(defun yagist-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (let ((hexify
         (lambda (x)
           (url-hexify-string
            (with-output-to-string (princ x))))))
    (mapconcat
     (lambda (param)
       (concat (funcall hexify (car param))
               "="
               (funcall hexify (cdr param))))
     params "&")))

(defun yagist-command-to-string (&rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (= (apply 'call-process "git" nil t nil args) 0)
        (error "git command fails %s" (buffer-string))))))

(defcustom yagist-encrypt-risky-config nil
  "*Encrypt your token by using `kaesar' package."
  :type 'boolean
  :group 'gist)

(defvar yagist-risky-config-keys
  '("oauth-token"))

(declare-function kaesar-decrypt-string "kaesar")
(declare-function kaesar-encrypt-string "kaesar")

(defun yagist-decrypt-string (key string)
  (let ((kaesar-decrypt-prompt
         (format "Password to decrypt %s: " key)))
    (kaesar-decrypt-string
     (base64-decode-string string))))

(defun yagist-encrypt-string (key string)
  (let ((kaesar-encrypt-prompt
         (format "Password to encrypt %s: " key)))
    (base64-encode-string
     (kaesar-encrypt-string string) t)))

(defun yagist-config (key)
  "Returns a GitHub specific value from the global Git config.
This function may call `yagist-set-config' to decrease security risk."
  (let ((raw-val (yagist-read-config key)))
    (cond
     ((and yagist-encrypt-risky-config
           (require 'kaesar nil t)
           (member key yagist-risky-config-keys))
      (let* ((real-key (concat "encrypted." key))
             (enc-val (yagist-read-config real-key)))
        (when raw-val
          ;; destroy unencrypted value.
          (yagist-write-config key nil)
          ;; translate raw value to encrypted value
          (yagist-set-config key raw-val))
        (let ((real-val (and enc-val
                             (yagist-decrypt-string key enc-val))))
          (or real-val raw-val))))
     (t
      raw-val))))

(defun yagist-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (cond
   ((and yagist-encrypt-risky-config
         (require 'kaesar nil t)
         (member key yagist-risky-config-keys))
    (let* ((raw-val (yagist-read-config key))
           (real-key (concat "encrypted." key))
           (enc-val (yagist-encrypt-string key value)))
      (when raw-val
        ;; destroy unencrypted value.
        (yagist-write-config key nil))
      (yagist-write-config real-key enc-val)))
   (t
    (yagist-write-config key value))))

(defun yagist-write-config (key value)
  (let ((args
         `(
           ,@(unless value '("--unset"))
           ,(format "github.%s" key)
           ,@(and value `(,value)))))
    (apply 'yagist-command-to-string
           "config" "--global" args)))

(defun yagist-read-config (key)
  (let ((val (condition-case nil
                 (apply 'yagist-command-to-string
                        `("config" "--global"
                          ,@(and yagist-git-config-with-includes
                                 '("--includes"))
                          ,(format "github.%s" key)))
               (error nil))))
    (cond
     ((null val) nil)
     ((string-match "\\`[\r\n]*\\'" val) nil)
     ((string-match "[\r\n]+\\'" val)
      (substring val 0 (match-beginning 0)))
     (t
      val))))

;;;###autoload
(defun yagist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (yagist-region begin end t))

;;;###autoload
(defun yagist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (yagist-region (point-min) (point-max)
                 private (yagist-single-file-name)))

;;;###autoload
(defun yagist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (yagist-region (point-min) (point-max)
                 t (yagist-single-file-name)))

;;;###autoload
(defun yagist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the
current buffer as a new paste at gist.github.com Copies the URL
into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (if (yagist-region-active-p)
      (yagist-region (region-beginning) (region-end) private)
    (yagist-buffer private)))

;;;###autoload
(defun yagist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the
current buffer as a new private paste at gist.github.com Copies
the URL into the kill ring."
  (interactive)
  (if (yagist-region-active-p)
      (yagist-region (region-beginning) (region-end) t)
    (yagist-buffer t)))

;;;###autoload
(defun yagist-list ()
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (yagist-list-draw-gists 1))

(defun yagist-quit-window (&optional kill-buffer)
  "Bury the *gists* buffer and delete its window.
With a prefix argument, kill the buffer instead."
  (interactive "P")
  (quit-window kill-buffer))

(defun yagist-list--paging-retrieve ()
  (cond
   ((null yagist-list--paging-info))
   ((eq yagist-list--paging-info t))
   (t
    (cl-destructuring-bind (page . max) yagist-list--paging-info
      (cond
       ((or (not (numberp page))
            (not (numberp max))))       ; Now retrieving
       ((not (eobp)))
       ((= page max)
        (message "No more next page"))
       (t
        (yagist-list-draw-gists (1+ page))))))))

(defun yagist-list-draw-gists (page)
  (with-current-buffer (get-buffer-create "*gists*")
    (when (= page 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (yagist-list-mode)
        (yagist-insert-list-header)))
    ;; suppress multiple retrieving
    (setq yagist-list--paging-info t))
  (yagist-request
   "GET"
   "https://api.github.com/gists"
   'yagist-lists-retrieved-callback
   `(,@(and yagist-list-items-per-page
            `(("per_page" . ,yagist-list-items-per-page)))
     ("page" . ,page))))

(defun yagist-list-revert-buffer (&rest ignore)
  ;; redraw gist list
  (yagist-list))

(defun yagist-region-active-p ()
  (if (functionp 'region-active-p)
      ;; trick for suppressing elint warning
      (funcall 'region-active-p)
    (and transient-mark-mode mark-active)))

(defun yagist-insert-list-header ()
  "Creates the header line in the gist list buffer."
  (save-excursion
    (insert "  ID           Updated                "
            "  Visibility  Description             "
            (yagist-fill-string "" (frame-width))
            "\n"))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'header-line))
  (forward-line))

(defun yagist-insert-gist-link (gist)
  "Inserts a button that will open the given gist when pressed."
  (let* ((data (yagist-parse-gist gist))
         (repo (car data)))
    (dolist (x (cdr data))
      (insert (format "  %s   " x)))
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'yagist-describe-button
                      'face 'default
                      'yagist-json gist))
  (insert "\n"))

(defun yagist-describe-button (button)
  (let ((json (button-get button 'yagist-json)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (yagist-describe-gist-1 json)))))

(defun yagist-describe-insert-button (text action json)
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box (:line-width 2 :color "dark grey")
                                :background "light grey"
                                :foreground "black")
                       'link))
        (id (cdr (assq 'id json))))
    (insert-text-button button-text
                        'face button-face
                        'follow-link t
                        'action action
                        'repo id
                        'yagist-json json)
    (insert " ")))

(defun yagist-describe-gist-1 (gist)
  (require 'lisp-mnt)
  (let ((id (cdr (assq 'id gist)))
        (description (cdr (assq 'description gist)))
        (url (cdr (assq 'html_url gist)))
        (updated (cdr (assq 'updated_at gist)))
        (publicp (eq (cdr (assq 'public gist)) t)))

    (insert
     (if publicp
         (propertize "Public Gist"
                     'font-lock-face `(bold underline ,font-lock-warning-face))
       (propertize "Private Gist"
                   'font-lock-face '(bold underline)))
     "\n")
    (insert "  " (propertize "Description: " 'font-lock-face 'bold)
            (or description "") "\n")
    (insert "          " (propertize "URL: " 'font-lock-face 'bold) url "\n")
    (insert "      " (propertize "Updated: " 'font-lock-face 'bold)
            (format-time-string
             yagist-display-date-format
             (yagist-parse-time-string updated)) "\n")

    (insert "\n\n")

    (yagist-describe-insert-button
     "Fetch Repository" 'yagist-fetch-button gist)
    (yagist-describe-insert-button
     "Browse" 'yagist-open-web-button gist)

    (insert "\n\n")

    (yagist-describe-insert-button
     "Edit Description" 'yagist-update-button gist)
    (yagist-describe-insert-button
     "Delete Gist" 'yagist-delete-button gist)))

(defun yagist-fetch-button (button)
  "Called when a gist [Fetch] button has been pressed.
Fetche gist repository and open the directory.

See `yagist-working-directory-alist' document to fetch repository
into the user selected directory."
  (yagist-fetch (button-get button 'repo)))

(defun yagist-delete-button (button)
  "Called when a gist [Delete] button has been pressed.
Confirm and delete the gist."
  (when (y-or-n-p "Really delete this gist? ")
    (yagist-delete (button-get button 'repo))))

(defun yagist-update-button (button)
  "Called when a gist [Edit] button has been pressed.
Edit the gist description."
  (let* ((json (button-get button 'yagist-json))
         (desc (read-from-minibuffer
                "Description: "
                (cdr (assq 'description json)))))
    (yagist-update (button-get button 'repo) desc)))

(defun yagist-open-web-button (button)
  "Called when a gist [Browse] button has been pressed."
  (let* ((json (button-get button 'yagist-json))
         (url (cdr (assq 'html_url json))))
    (browse-url url)))

(defun yagist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (cdr (assq 'id gist)))
        (updated-at (cdr (assq 'updated_at gist)))
        (description (cdr (assq 'description gist)))
        (visibility (if (eq (cdr (assq 'public gist)) 't)
                        "public"
                      "private")))
    (list repo
          (yagist-fill-string repo 8)
          (yagist-fill-string
           (format-time-string
            yagist-display-date-format (yagist-parse-time-string updated-at))
           20)
          (yagist-fill-string visibility 7)
          (or description ""))))

(defun yagist-parse-time-string (string)
  (let* ((times (split-string string "[-T:Z]" t))
         (getter (lambda (x) (string-to-number (nth x times))))
         (year (funcall getter 0))
         (month (funcall getter 1))
         (day (funcall getter 2))
         (hour (funcall getter 3))
         (min (funcall getter 4))
         (sec (funcall getter 5)))
    (encode-time sec min hour day month year 0)))

(defun yagist-fill-string (string width)
  (truncate-string-to-width string width nil ?\s "..."))

(defconst yagist-repository-url-format "git@gist.github.com:%s.git")

(defun yagist-fetch (id)
  (let* ((url (format yagist-repository-url-format id))
         (working-copy (yagist-working-copy-directory id)))
    (cond
     ((not (file-directory-p (expand-file-name ".git" working-copy)))
      (message "Cloning %s into working copy..." url)
      (yagist-start-git-for-local `("clone" ,url ".") working-copy))
     (t
      (message "Fetching %s into working copy... " url)
      (yagist-start-git-for-local `("pull" ,url) working-copy)))
    (dired working-copy)))

(defun yagist-delete (id)
  (yagist-request
   "DELETE"
   (format "https://api.github.com/gists/%s" id)
   (yagist-simple-receiver "Delete")))

(defun yagist-update (id description)
  (yagist-request
   "PATCH"
   (format "https://api.github.com/gists/%s" id)
   (yagist-simple-receiver "Update")
   `(,@(and description
            `(("description" . ,description))))))

(defun yagist-working-copy-directory (id)
  (let* ((pair (assoc id yagist-working-directory-alist))
         (dir (cond
               (pair
                (cdr pair))
               (t
                (expand-file-name id yagist-working-directory)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun yagist-start-git-for-local (args &optional directory)
  (let* ((buffer (generate-new-buffer " *gist git* "))
         (default-directory
           (or (and directory (file-name-as-directory directory))
               default-directory))
         (proc (apply 'start-process "Gist" buffer "git" args)))
    (set-process-sentinel
     proc `(lambda (p e)
             (when (memq (process-status p) '(exit signal))
               (let ((code (process-exit-status p)))
                 (cond
                  ((eq code 0)
                   (message "Done fetching gist repository."))
                  (t
                   (message "Gist git process finished with %d" code)))
                 (let ((buf (dired-find-buffer-nocreate ,default-directory)))
                   (when (and buf (buffer-live-p buf))
                     (with-current-buffer buf
                       (revert-buffer)))))
               (kill-buffer (process-buffer p)))))
    proc))

(defun yagist-simple-receiver (message)
  ;; Create a receiver of `yagist-request-0'
  `(lambda (status url json-or-params)
     (goto-char (point-min))
     (when (re-search-forward "^Status: \\([0-9]+\\)" nil t)
       (let ((code (string-to-number (match-string 1))))
         (if (and (<= 200 code) (< code 300))
             (message "%s succeeded" ,message)
           (message "%s %s"
                    ,message
                    (yagist--err-propertize "failed")))))
     (url-mark-buffer-as-dead (current-buffer))))

(defun yagist-created-callback (status url json)
  (let ((location (save-excursion
                    (goto-char (point-min))
                    (and (re-search-forward "^Location: \\(.*\\)" nil t)
                         (match-string 1))))
        (http-url))
    (cond
     ;; check redirected location indicate public/private gist url
     ((and (stringp location)
           (string-match "\\([0-9]+\\|[0-9a-zA-Z]\\{32\\}\\)$" location))
      (let ((id (match-string 1 location)))
        (setq http-url (format "https://gist.github.com/%s" id))
        (message "Paste created: %s" http-url)
        (when yagist-view-gist
          (browse-url http-url))))
     (t
      (message "Paste is %s"
               (yagist--err-propertize "failed"))))
    (when http-url
      (kill-new http-url))
    (url-mark-buffer-as-dead (current-buffer))))

(defun yagist--err-propertize (string)
  (propertize string 'face 'font-lock-warning-face))

(defun yagist-lists-retrieved-callback (status url params)
  "Called when the list of gists has been retrieved. Parses the result
and displays the list."
  (goto-char (point-min))
  (let ((max-page
         ;; search http headaer
         (and (re-search-forward "<\\([^>]+\\)>; *rel=\"last\"" nil t)
              (let ((url (match-string 1)))
                (and (string-match "\\?\\(.*\\)" url)
                     (let* ((query (match-string 1 url))
                            (params (url-parse-query-string query))
                            (max-page (cadr (assoc "page" params))))
                       (when (string-match "\\`[0-9]+\\'" max-page)
                         (string-to-number max-page))))))))
    (when (re-search-forward "^\r?$" nil t)
      (let* ((json (yagist--read-json (point) (point-max)))
             (page (cdr (assoc "page" params))))
        (with-current-buffer (get-buffer-create "*gists*")
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (mapc 'yagist-insert-gist-link json)))
          ;; no max-page means last-page
          (setq yagist-list--paging-info
                (cons page (or max-page page)))

          ;; skip header
          (forward-line)
          (set-window-buffer nil (current-buffer)))))
    (url-mark-buffer-as-dead (current-buffer))))

;;;
;;; Gist minor mode
;;;

(defvar yagist-minor-mode-gist-id nil)

;;;###autoload
(define-minor-mode yagist-minor-mode
  ""
  :init-value nil :lighter " [YaGist]" :keymap nil
  (unwind-protect
      (cond
       (yagist-minor-mode
        (let ((id (or (or yagist-minor-mode-gist-id
                          (yagist-directory-is-gist default-directory)
                          (read-from-minibuffer "Gist ID: ")))))
          (set (make-local-variable 'yagist-minor-mode-gist-id) id)))
       (t nil))
    (cond
     ((not yagist-minor-mode))
     ((null yagist-minor-mode-gist-id)
      ;; fallback to off
      (yagist-minor-mode -1))
     (t
      (add-hook 'after-save-hook 'yagist-after-save-commit nil t)))))

;;;###autoload
(define-global-minor-mode yagist-global-minor-mode
  yagist-minor-mode yagist-minor-mode-maybe
  )

(defun yagist-minor-mode-maybe ()
  (when (and default-directory
             (not (minibufferp)))
    (let ((id (yagist-directory-is-gist default-directory)))
      (setq yagist-minor-mode-gist-id id)
      (when id
        (yagist-minor-mode 1)))))

(defun yagist-directory-is-gist (directory)
  (let ((conf (expand-file-name ".git/config" directory)))
    (when (file-exists-p conf)
      (with-temp-buffer
        (insert-file-contents conf)
        (when (and (re-search-forward "^\\[remote \"origin\"]" nil t)
                   (re-search-forward "^[ \t]*url[ \t]*=[ \t]*\\(.*\\)" nil t))
          (let ((url (match-string 1)))
            ;; public gist have decimal, private gist have hex id
            (cond
             ((string-match
               "^git://gist.github.com/\\([0-9a-fA-F]+\\)\\.git$" url)
              (match-string 1 url))
             ((string-match
               "^git@gist.github.com:\\([0-9a-fA-F]+\\)\\.git$" url)
              (match-string 1 url)))))))))

(defun yagist-after-save-commit ()
  (when yagist-minor-mode-gist-id
    (let* ((file (or (buffer-file-name) (buffer-name)))
           (name (file-name-nondirectory file)))
      (yagist-update-contents
       yagist-minor-mode-gist-id name (buffer-string)))))

(defun yagist-update-contents (id name content)
  (yagist-request
   "PATCH"
   (format "https://api.github.com/gists/%s" id)
   (yagist-simple-receiver "Update")
   `(("files" .
      ((,name .
              (("content" . ,content))))))))

(provide 'yagist)

;;; yagist.el ends here
