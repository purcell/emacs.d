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


(defconst weibo-timeline-status-comment-buffer-name "*weibo-status-comment*")
(defconst weibo-api-status-comments-timeline "comments/show")

(defun weibo-pull-status-comments (data parse-func new status)
  (let* ((id (and status (weibo-status-id status)))
	 (param (format "?id=%s" id)))
    (with-temp-message "获取微博评论..."
      (weibo-get-data weibo-api-status-comments-timeline
		       parse-func param t t))))

(defun weibo-status-comments-pretty-printer (comment &optional p)
  (weibo-insert-comment comment nil))

(defun weibo-status-comments-header (status)
  (with-temp-buffer
    (setq fill-column 70)
    (set (make-local-variable 'fill-nobreak-predicate) 'weibo-timeline-name-nobreak-p)    
    (insert "\n")
    (weibo-insert-status status nil)
    (buffer-string)))

(defun weibo-comment-status-comments (comment status)
  (let ((id (and status (weibo-status-id status))))
    (weibo-create-post "" "评论微博" nil 'weibo-send-comment id)))

(defun weibo-reply-status-comments (comment status)
  (when comment
    (let ((cid (weibo-comment-id comment))
	   (id (weibo-status-id (weibo-comment-status comment)))
	   (user_name (weibo-user-screen_name (weibo-comment-user comment))))
      (weibo-create-post (format "回复@%s:" user_name) "回复评论" nil 'weibo-send-reply cid id))))

(defun weibo-status-comments-timeline-provider (status)
  (make-weibo-timeline-provider
   :key nil
   :tag 'comments
   :name "察看微博"
   :make-function 'weibo-make-comment
   :pretty-printer-function 'weibo-status-comments-pretty-printer
   :pull-function 'weibo-pull-status-comments
   :post-function 'weibo-post-status
   :look-function nil
   :retweet-function nil
   :comment-function 'weibo-comment-status-comments
   :reply-function 'weibo-reply-status-comments
   :header-function 'weibo-status-comments-header
   :data status))

(defun weibo-status-comment-buffer (provider)
  (switch-to-buffer (get-buffer-create weibo-timeline-status-comment-buffer-name))
  (setq weibo-timeline-current-provider provider)
  (unless (eq major-mode 'weibo-timeline-mode)
    (weibo-timeline-mode))
  (weibo-timeline-refresh))

(provide 'weibo-status-comment)
