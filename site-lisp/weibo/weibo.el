;; Copyright (C) 2011 Austin<austiny.cn@gmail.com>
          
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))
(require 'url)
(require 'json)
(require 'weibo-authorize)
(require 'weibo-timeline)
(require 'weibo-status)
(require 'weibo-user)
(require 'weibo-image)
(require 'weibo-post)
(require 'weibo-comment)
(require 'weibo-status-comment)

(defconst weibo-api-url "https://api.weibo.com/2/" "API base url")

(defvar weibo-directory "~/.t.weibo.emacs.d")
(defvar weibo-token nil)
(defvar weibo-token-expire nil)

(defun weibo-get-token-file ()
  (unless (file-exists-p (expand-file-name weibo-directory))
    (make-directory (expand-file-name weibo-directory) t))
  (expand-file-name "token2" weibo-directory))

(defun weibo-get-token ()
  (unless (and weibo-token (not (weibo-token-expired)))
    (weibo-authorize))
  weibo-token)

(defun weibo-parse-token (string)
  (when (string-match "\\([^:]*\\):\\(.*\\)" string)
    (setq weibo-token (match-string 1 string))
    (setq weibo-token-expire (match-string 2 string))))

(defun weibo-token-expired ()
  (when weibo-token-expire
    (> (float-time) (string-to-number weibo-token-expire))))

(defun weibo-authorize (&optional reauthorize)
  (when (file-exists-p (weibo-get-token-file))
    (save-excursion
	  (find-file (weibo-get-token-file))
	  (weibo-parse-token (buffer-substring-no-properties (point-min) (point-max)))	 
	  (save-buffer)
	  (kill-this-buffer)))
  (when (or reauthorize (not weibo-token) (weibo-token-expired))
    (weibo-parse-token (weibo-authorize-app))
    (setq weibo-token-expire (number-to-string (+ (float-time) (string-to-number weibo-token-expire)))))
  (save-excursion
    (find-file (weibo-get-token-file))
    (erase-buffer)
    (insert (format "%s:%s\n" weibo-token weibo-token-expire))
    (save-buffer)
    (kill-this-buffer))
  weibo-token)

(defun weibo-check-result (root)
  (if (and root (length root)
	   (if (and (listp root) (weibo-get-node root 'error)) nil t))
      t
    (print (weibo-get-node-text root 'error))
    nil))

(defun weibo-get-node (pnode tag)
  (assoc tag pnode))

(defun weibo-get-node-text (node tag)
  (let ((data (cdr (weibo-get-node node tag))))
    (cond ((numberp data) (format "%d" data))
	  (t data))))

(defun weibo-get-body ()
  (goto-char (point-min))
  (let ((start
	 (or (search-forward "\r\n\r\n" nil t)
	     (search-forward "\n\n" nil t)))
	(buffer (current-buffer))
	(max (point-max)))
    (when start
      (with-temp-buffer
	(insert-buffer-substring buffer start max)
	(mm-decode-coding-region (point-min) (point-max) 'utf-8)
	(goto-char (point-min))
	(while (re-search-forward "\"id\":\\([0-9]+\\)," nil t)
	  (replace-match "\"id\":\"\\1\"," nil nil))
	(goto-char (point-min))
	(condition-case nil
	    (json-read)
	  ((error nil) `((error . ,(buffer-substring (point-min) (point-max))))))))))

(defun weibo-retrieve-url (url)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,(format "OAuth2 %s" (url-hexify-string (weibo-get-token)))))))
    (flet ((message (&rest args) nil))
      (url-retrieve-synchronously url))))

(defun weibo-send-url (url args)
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 `(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Authorization" . ,(format "OAuth2 %s" (url-hexify-string (weibo-get-token))))))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
		    "&")))
    (flet ((message (&rest args) nil))
      (url-retrieve-synchronously url))))

(defun weibo-get-data (item callback &optional param &rest cbdata)
  (let ((root (with-current-buffer
		  (weibo-retrieve-url (concat (format "%s%s.json" weibo-api-url item) param))
		(weibo-get-body))))
    (apply callback (cons root cbdata))))

(defun weibo-post-data (item callback vars &optional param &rest cbdata)
  (let ((root (with-current-buffer
		       (weibo-send-url (concat (format "%s%s.json" weibo-api-url item) param) vars)
		     (weibo-get-body))))
    (apply callback (cons root cbdata))))

(defun weibo-parse-data-result (root &rest data)
  (when root
    (print root)))

(defun weibo-string-decrement (str)
  (let ((strl (reverse (delete "" (split-string str ""))))
	(result nil)
	(done nil))
    (while strl
      (let ((c (car strl)))
	(push
	 (if done c
	   (if (string= "0" c) "9"
	     (progn
	       (setq done t)
	       (number-to-string (- (string-to-number c) 1)))))
	 result))
      (setq strl (cdr strl)))
    (mapconcat 'identity result "")))

(defun weibo-bury-close-window ()
  (interactive)
  (bury-buffer)
  (condition-case err
      (delete-window)
    (error nil)))

(defun weibo-kill-close-window ()
  (interactive)
  (kill-buffer)
  (condition-case err
      (delete-window)
    (error nil)))

(weibo-timeline-register-provider (weibo-friends-timeline-provider))
(weibo-timeline-register-provider (weibo-user-timeline-provider))
(weibo-timeline-register-provider (weibo-mention-timeline-provider))
(weibo-timeline-register-provider (weibo-comments-mentions-timeline-provider))
(weibo-timeline-register-provider (weibo-comments-by-me-timeline-provider))
(weibo-timeline-register-provider (weibo-comments-to-me-timeline-provider))
(weibo-timeline-register-provider (weibo-public-timeline-provider))

(provide 'weibo)
