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

(defconst weibo-user-buffer-name "*weibo-user*")
(defconst weibo-api-user-show "users/show")
(defconst weibo-api-friendships-friends "friendships/friends")
(defconst weibo-api-account-getuid "account/get_uid")

(defvar weibo-user-uid nil)
(defvar weibo-user-friends-list nil)
(defvar weibo-user-custom-list nil)

(defun weibo-user-get-uid ()
  (weibo-get-data weibo-api-account-getuid
		  (lambda (uid)
		    (setq weibo-user-uid (weibo-get-node-text uid 'uid)))))

(defun weibo-user-get-friends ()
  (weibo-get-data weibo-api-friendships-friends
		  (lambda (friends)
		    (setq weibo-user-friends-list
			  (mapcar (lambda (user) (format "@%s " (weibo-get-node-text user 'screen_name)))
			    (cdr (weibo-get-node friends 'users)))))
		  (format "?uid=%s&count=200" weibo-user-uid)))

;; id: 用户UID
;; screen_name: 微博昵称
;; name: 友好显示名称，如Bill Gates(此特性暂不支持)
;; province: 省份编码（参考省份编码表）
;; city: 城市编码（参考城市编码表）
;; location：地址
;; description: 个人描述
;; url: 用户博客地址
;; profile_image_url: 自定义图像
;; domain: 用户个性化URL
;; gender: 性别,m--男，f--女,n--未知
;; followers_count: 粉丝数
;; friends_count: 关注数
;; statuses_count: 微博数
;; favourites_count: 收藏数
;; created_at: 创建时间
;; following: 是否已关注(此特性暂不支持)
;; verified: 加V标示，是否微博认证用户 
(defstruct weibo-user id screen_name name
  province city location description url
  profile_image_url domain gender
  followers_count friends_count statuses_count favorites_count
  created_at following verified verified_reason avatar_large)

(defun weibo-make-user (node)
  (make-weibo-user
   :id (weibo-get-node-text node 'id)
   :screen_name (weibo-get-node-text node 'screen_name)
   :gender (weibo-get-node-text node 'gender)
   :location (weibo-get-node-text node 'location)
   :description (weibo-get-node-text node 'description)
   :followers_count (weibo-get-node-text node 'followers_count)
   :friends_count (weibo-get-node-text node 'friends_count)
   :statuses_count (weibo-get-node-text node 'statuses_count)
   :favorites_count (weibo-get-node-text node 'favorites_count)   
   :verified (weibo-get-node-text node 'verified)
   :verified_reason (weibo-get-node-text node 'verified_reason)
   :profile_image_url (weibo-get-node-text node 'profile_image_url)
   :avatar_large (weibo-get-node-text node 'avatar_large)))

(defun weibo-insert-user (user details_t)
  (if details_t
      (weibo-insert-user-detail user)
    (weibo-insert-user-simple user)))

(defun weibo-insert-user-detail (user)
  (when user
    (let ((id (weibo-user-id user))
	  (desc (weibo-user-description user))
	  (url (weibo-user-url user))
	  (domain (weibo-user-domain user))
	  (followers_count (weibo-user-followers_count user))
	  (friends_count (weibo-user-friends_count user))
	  (statuses_count (weibo-user-statuses_count user))
	  (favorites_count (weibo-user-favorites_count user))
	  (created_at (weibo-user-created_at user))
	  (profile_image_url (weibo-user-profile_image_url user))
	  (avatar_large (weibo-user-avatar_large user))
	  (verified_reason (weibo-user-verified_reason user)))
      (weibo-insert-image (weibo-get-image-file avatar_large t))
      (insert " ")
      (insert-text-button (weibo-user-screen_name user)
			  'action `(lambda (b)
				     (weibo-bury-close-window)
				     (weibo-timeline-switch-to-provider
				      "i" ,(format "uid=%s" id)))
			  'follow-link t)
      (when (eq (weibo-user-verified user) t)
	(insert " V"))
      (insert " (" 
	      (cond ((string= (weibo-user-gender user) "m") "男")
		    ((string= (weibo-user-gender user) "f") "女")
		    (t "未知"))
	      ", " (weibo-user-location user) ") ")
      (insert "\n")
      (when desc
	(insert " 个人描述：")
	(weibo-timeline-insert-text desc))
      (when verified_reason
	(insert " 认证信息：")
	(weibo-timeline-insert-text verified_reason))
      (when url
	(insert (format " 博客地址： %s\n" url)))
      (when domain
	(insert (format " 个性地址： %s\n" domain)))
      (when followers_count
	(insert (format " 粉丝数量： %s\n" followers_count)))
      (when friends_count
	(insert (format " 关注数量： %s\n" friends_count)))
      (when statuses_count
	(insert (format " 微博数量： %s\n" statuses_count)))
      (when favorites_count
	(insert (format " 收藏数量： %s\n" favorites_count)))
      (when created_at
	(insert (format " 加入时间： %s\n" created_at)))
      t)))

(defun weibo-insert-user-simple (user)
  (when user
    (weibo-insert-image (weibo-get-image-file (weibo-user-profile_image_url user)))
    (insert " ")
    (insert-text-button (weibo-user-screen_name user)
			'action (lambda (b) (weibo-show-user (button-label b)))
			'follow-link t)
    (when (eq (weibo-user-verified user) t)
      (insert " V"))
    (insert " (" 
	    (cond ((string= (weibo-user-gender user) "m") "男")
		  ((string= (weibo-user-gender user) "f") "女")
		  (t "未知"))
	    ", " (weibo-user-location user) ") ")))

(defun weibo-parse-user (root func)
  (if (not (weibo-check-result root))
      (progn
	(message "找不到此用户")
	nil)
    (apply func (list (weibo-make-user root)))))

(defun weibo-show-user (screen-name)
  (with-temp-message "获取用户资料..."
    (when screen-name
      (let ((init-t (not (get-buffer weibo-user-buffer-name)))
	    (close-t nil)
	    (name (if (string-match "@" screen-name) (substring screen-name 1)
		    screen-name)))
	(switch-to-buffer-other-window weibo-user-buffer-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(when init-t
	  (weibo-user-mode))
	(unless (weibo-get-data weibo-api-user-show
				'weibo-parse-user (format "?screen_name=%s" (url-hexify-string name))
				'weibo-insert-user-detail)
	  (setq close-t t))
	(goto-char (point-min))
	(setq buffer-read-only t)
	(when close-t (weibo-bury-close-window))))))

(defvar weibo-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'weibo-bury-close-window)
    map)
  "Keymap for weibo-user-mode")

(define-derived-mode weibo-user-mode fundamental-mode "Weibo-User"
  "Major mode for displaing weibo user"
  (use-local-map weibo-user-mode-map))

(provide 'weibo-user)
