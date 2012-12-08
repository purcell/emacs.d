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

(require 'cl)

(defconst weibo-api-status-public-timeline "statuses/public_timeline")
(defconst weibo-api-status-friends-timeline "statuses/friends_timeline")
(defconst weibo-api-status-user-timeline "statuses/user_timeline")
(defconst weibo-api-status-mention-timeline "statuses/mentions")

(defconst weibo-api-status-update "statuses/update")
(defconst weibo-api-status-repost "statuses/repost")
(defconst weibo-api-status-counts "statuses/counts")

;; created_at: 创建时间
;; id: 微博ID
;; text：微博信息内容
;; source: 微博来源
;; favorited: 是否已收藏(正在开发中，暂不支持)
;; truncated: 是否被截断
;; in_reply_to_status_id: 回复ID
;; in_reply_to_user_id: 回复人UID
;; in_reply_to_screen_name: 回复人昵称
;; thumbnail_pic: 缩略图
;; bmiddle_pic: 中型图片
;; original_pic：原始图片
;; user: 作者信息
;; retweeted_status: 转发的博文，内容为status，如果不是转发，则没有此字段 
(defstruct weibo-status created_at id text
  source favorited truncated
  in_reply_to_status_id
  in_reply_to_user_id
  in_reply_to_screen_name
  thumbnail_pic
  bmiddle_pic
  original_pic
  user retweeted_status
  comments rt)

(defun weibo-make-status (node)
  (make-weibo-status
   :id (weibo-get-node-text node 'id)
   :text (weibo-get-node-text node 'text)
   :source (replace-regexp-in-string "<[^>]*>" "" (or (weibo-get-node-text node 'source) ""))
   :favorited (weibo-get-node-text node 'favorited)
   :truncated (weibo-get-node-text node 'truncated)
   :in_reply_to_status_id (weibo-get-node-text node 'in_reply_to_status_id)
   :in_reply_to_user_id (weibo-get-node-text node 'in_reply_to_user_id)
   :in_reply_to_screen_name (weibo-get-node-text node 'in_reply_to_screen_name)   
   :thumbnail_pic (weibo-get-node-text node 'thumbnail_pic)
   :bmiddle_pic (weibo-get-node-text node 'bmiddle_pic)
   :original_pic (weibo-get-node-text node 'original_pic)
   :retweeted_status (let ((retweeted (weibo-get-node node 'retweeted_status)))
		       (when retweeted
			 (weibo-make-status retweeted)))
   :created_at (weibo-get-node-text node 'created_at)
   :user (when (weibo-get-node node 'user) (weibo-make-user (weibo-get-node node 'user)))
   :comments "0"
   :rt "0"))

(defun weibo-pull-status (node parse-func new type)
  (let* ((keyword (if new "since_id" "max_id"))
	 (id (and node (weibo-status-id node)))
	 (idparam (and id (format "%s=%s" keyword (if new id (weibo-string-decrement id)))))
	 (param (format "?%s" (mapconcat 'identity (remove-if-not 'stringp (list weibo-timeline-extra-params idparam)) "&"))))
    (with-temp-message (concat "获取微博 " param "...")
      (weibo-get-data type
		      parse-func param
		      new)
      (when new
	(cond
	 ((string= type weibo-api-status-friends-timeline)
	  (weibo-timeline-reset-count "status"))
	 ((string= type weibo-api-status-mention-timeline)
	  (weibo-timeline-reset-count "mention_status")))))))

(defun weibo-status-pretty-printer (status &optional p)
  (weibo-insert-status status nil))

(defun weibo-insert-status (status retweeted)
  (when status
    (let ((indent (if retweeted "\t" "")))
      (unless retweeted
	(insert weibo-timeline-separator "\n"))
      (when retweeted
	(insert weibo-timeline-sub-separator "\n")
	(insert " 提到：" indent))
      (when (weibo-status-user status)
	(weibo-insert-user (weibo-status-user status) nil)
	(insert "说道："))
      (insert "\n")
      (insert indent)
      (weibo-timeline-insert-text (weibo-status-text status))
      (when (weibo-status-thumbnail_pic status) (insert indent))
      (weibo-timeline-insert-picture (weibo-status-thumbnail_pic status) (weibo-status-original_pic status))
      (unless retweeted
	(let ((retweeted_status (weibo-status-retweeted_status status)))
	  (weibo-insert-status retweeted_status t)))
      (insert indent "  " (weibo-parse-status-time (weibo-status-created_at status)) "  来自：" (weibo-status-source status) (format "  转贴(%s)  评论(%s)\n"
			     (weibo-status-rt status)
			     (weibo-status-comments status)))
      (when retweeted
	(insert weibo-timeline-sub-separator "\n")))))

(defun weibo-generate-time-string (fmtstr tmstr &rest args)
  (apply 'format (format-time-string fmtstr (date-to-time tmstr)) args))

(defun weibo-parse-status-time (time-string)
  (if (= (length time-string) 0)
      ""
    (let ((now (current-time-string)))
      (if (< 0 (days-between now time-string))
	  (if (= (nth 5 (parse-time-string now)) (nth 5 (parse-time-string time-string)))
	      (weibo-generate-time-string "%m%%s%d%%s %H:%M" time-string "月" "日")
	    (weibo-generate-time-string "%Y%%s%m%%s%d%%s %H:%M" time-string "年" "月" "日"))
	(let* ((seconds (floor (float-time (time-since time-string))))
	       (hours (/ seconds 3600))
	       (minutes (/ (% seconds 3600) 60)))
	  (cond
	   ((< 0 hours) (weibo-generate-time-string "%%s%H:%M" time-string "今天"))
	   ((< 0 minutes) (format "%d分钟前" minutes))
	   (t (format "%d秒前" seconds))))))))

(defun weibo-update-status (status-list type)
  (when status-list
    (let ((ids (mapconcat (lambda (status)
			    (concat (weibo-status-id status)
				    (let ((rtstatus (weibo-status-retweeted_status status)))
				      (when rtstatus (concat "," (weibo-status-id rtstatus))))))
			  status-list ",")))
      (weibo-get-data weibo-api-status-counts
		      'weibo-parse-update-status (format "?ids=%s" ids) status-list type))))

(defun weibo-parse-update-status (root status-list type)
  (when (weibo-check-result root)
    (let* ((first-status (and (string= type weibo-api-status-friends-timeline)
			      (car status-list)))
	   (since-id (and first-status (weibo-status-id first-status)))
	   (status-alist (mapcar (lambda (status)
				   `(,(weibo-status-id status) . ,status))
				 status-list))
	   (rtstatus-alist (mapcar (lambda (status)
				     (let ((rtstatus (weibo-status-retweeted_status status)))
				       (when rtstatus
					 `(,(weibo-status-id rtstatus) . ,rtstatus))))
				   status-list)))
      (mapc (lambda (node)
	      (let* ((id (weibo-get-node-text node 'id))
		     (comments (weibo-get-node-text node 'comments))
		     (rt (weibo-get-node-text node 'rt))
		     (astatus (assoc id status-alist))
		     (status (and astatus (cdr astatus)))
		     (artstatus (assoc id rtstatus-alist))
		     (rtstatus (and artstatus (cdr artstatus))))
		(when status (setf (weibo-status-comments status) comments
				   (weibo-status-rt status) rt))
		(when rtstatus (setf (weibo-status-comments rtstatus) comments
				     (weibo-status-rt rtstatus) rt))))
	    (append root nil))
      `(,since-id . ,status-list))))

(defun weibo-post-status (&rest p)
  (weibo-create-post "" "发表微博" nil 'weibo-send-status))

(defun weibo-look-status (status &rest p)
  (when status
    (weibo-timeline-set-provider (weibo-status-comments-timeline-provider
				  (let ((rt-status (weibo-status-retweeted_status status))
					(pos-begin (save-excursion
						     (search-backward weibo-timeline-separator nil t)))
					(pos-end (save-excursion
						   (search-forward weibo-timeline-separator nil t))))
				    (if rt-status
					(if (and
					     (save-excursion
					       (search-backward
						weibo-timeline-sub-separator pos-begin t))
					     (save-excursion
					       (search-forward
						weibo-timeline-sub-separator pos-end t)))
					    rt-status
					  status)
				      status))))))

;; reply-to-id t weibo-api-status-repost
;; reply-to-id 0 text t weibo-api-status-update
;; reply-to-id 0 text 0 message
(defun weibo-send-status (text &optional reply-to-id)
  (let ((data nil)
	(api weibo-api-status-update))
    (cond
     ((= (length text) 0) (message "不能发表空消息") nil)
     ((> (length text) 140) (message "消息长度须小于140字") nil)
     (t
      (add-to-list 'data `("status" . ,text))
      (when reply-to-id
	(add-to-list 'data `("id" . ,reply-to-id))
	(setq api weibo-api-status-repost))
      (weibo-post-data api 'weibo-parse-data-result data nil nil)))))

(defun weibo-retweet-status (data &rest p)
  (let* ((id (and data (weibo-status-id data)))
	 (retweeted (and data (weibo-status-retweeted_status data)))
	 (user_name (and retweeted (weibo-user-screen_name (weibo-status-user data))))
	 (user_name_text (and user_name (concat "//@" user_name "：")))
	 (text (and retweeted (weibo-status-text data))))
    (weibo-create-post (concat user_name_text text) "转发微博" t 'weibo-send-status id)))

(defun weibo-do-comment-status (status &rest p)
  (let* ((id (and status (weibo-status-id status))))
    (weibo-create-post "" "评论微博" nil 'weibo-send-comment id)))

(defun weibo-status-timeline-provider (key name data)
  (make-weibo-timeline-provider
   :key key
   :tag 'statuses
   :name name
   :make-function 'weibo-make-status
   :pretty-printer-function 'weibo-status-pretty-printer
   :pull-function 'weibo-pull-status
   :post-function 'weibo-post-status
   :look-function 'weibo-look-status
   :retweet-function 'weibo-retweet-status
   :comment-function 'weibo-do-comment-status
   :reply-function nil
   :header-function nil
   :update-function 'weibo-update-status
   :data data))

(defun weibo-friends-timeline-provider ()
  (weibo-status-timeline-provider "a" "我的关注" weibo-api-status-friends-timeline))

(defun weibo-user-timeline-provider ()
  (weibo-status-timeline-provider "i" "我的微博" weibo-api-status-user-timeline))

(defun weibo-mention-timeline-provider ()
  (weibo-status-timeline-provider "@" "提到我的微博" weibo-api-status-mention-timeline))

(defun weibo-public-timeline-provider ()
  (weibo-status-timeline-provider "w" "谁在说" weibo-api-status-public-timeline))
(provide 'weibo-status)

;; Local Variables: 
;; byte-compile-warnings: (not cl-functions) 
;; End: 
