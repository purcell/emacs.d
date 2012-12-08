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

(require 'ewoc)

(defconst weibo-timeline-buffer-name "*weibo-timeline*")
(defconst weibo-timeline-mode-name "微博时间线")

(defconst weibo-timeline-headline "微博：%s\n命令：%s\n操作：g新消息 r刷新 j下一条 k上一条 h帮助 q退出")
(defconst weibo-timeline-post-caption "P发表微博 ")
(defconst weibo-timeline-look-caption "L察看 ")
(defconst weibo-timeline-retweet-caption "T转发 ")
(defconst weibo-timeline-comment-caption "C评论 ")
(defconst weibo-timeline-reply-caption "R回复 ")

(defconst weibo-timeline-footline "提示：获取更多较早前消息(m)")

(defconst weibo-timeline-separator (make-string 70 ?=))
(defconst weibo-timeline-sub-separator (make-string 70 ?-))

(defconst weibo-timeline-name-regexp "@\\(\\w\\|_\\|-\\)+")
(defconst weibo-timeline-name-regexp2 "@\\(\\w\\|_\\|-\\)*")

(defvar weibo-timeline-data nil "Buffer local variable that holds timeline data")
(defvar weibo-timeline-current-provider nil
  "Buffer local variable that holds current timeline provider")

(defvar weibo-timeline-providers nil "Global variable that holds timeline providers")

(defvar weibo-timeline-timer nil)

(defconst weibo-api-status-unread "remind/unread_count")
(defconst weibo-api-reset-count "remind/set_count")

(defvar weibo-timeline-extra-params nil)

(defun weibo-timeline-get-unread (&optional param)
  (unless weibo-user-uid
    (weibo-user-get-uid)
    (weibo-user-get-friends))
  (weibo-get-data weibo-api-status-unread 'weibo-timeline-parse-unread param))

(defun weibo-timeline-parse-unread (root)
  (when (weibo-check-result root)
    (let ((follower (weibo-get-node-text root 'follower))
	  (dm (weibo-get-node-text root 'dm))
	  (mention_status (weibo-get-node-text root 'mention_status))
	  (mention_cmt (weibo-get-node-text root 'mention_cmt))	  
	  (cmt (weibo-get-node-text root 'cmt))
	  (status (weibo-get-node-text root 'status)))
      (unless (= 0 (string-to-number
		    (concat follower
			    dm mention_status
			    mention_cmt
			    cmt status)))
	(concat
	 (unless (string= follower "0")
	   (format "新粉丝(%s) " follower))
	 (unless (string= status "0")
	   (format "新微博(%s) " status))
	 (unless (string= cmt "0")
	   (format "新评论(%s) " cmt))	 
	 (unless (string= dm "0")
	   (format "新私信(%s) " dm))
	 (unless (string= mention_status "0")
	   (format "新@我的微博(%s) " mention_status))
	 (unless (string= mention_cmt "0")
	   (format "新@我的评论(%s) " mention_cmt)))))))

(defun weibo-timeline-reset-count (type)
  (weibo-post-data weibo-api-reset-count (lambda (root))
		       `(("type" . ,type))))

(defstruct weibo-timeline-provider
  key
  tag
  name
  make-function
  pretty-printer-function
  pull-function
  post-function
  look-function
  retweet-function
  comment-function
  reply-function
  header-function
  update-function
  data)

(defun weibo-timeline-register-provider (reg-data &optional current)
  (when (weibo-timeline-provider-p reg-data)
    (let* ((key (weibo-timeline-provider-key reg-data))
	   (switch-to-key (append '(lambda () (interactive))
				  `((weibo-timeline-switch-to-provider ,key)))))
      (add-to-list 'weibo-timeline-providers
		   `(,key . ,reg-data) t)
      (define-key weibo-timeline-mode-map key
	switch-to-key))))

(defun weibo-timeline-switch-to-provider (key &optional extra-params)
  (weibo-timeline)
  (let ((provider (cdr (assoc key weibo-timeline-providers))))
    (when provider
      (when (or (unless (string= extra-params weibo-timeline-extra-params) (setq weibo-timeline-extra-params extra-params) t) (not (eq provider weibo-timeline-current-provider)))
	(setq weibo-timeline-current-provider provider)
	(weibo-timeline-refresh)))))

(defun weibo-timeline-set-provider (provider)
  (weibo-status-comment-buffer provider))

(defun weibo-timeline-pretty-printer (item)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (apply (weibo-timeline-provider-pretty-printer-function weibo-timeline-current-provider)
	   (list item
		 (weibo-timeline-provider-data weibo-timeline-current-provider)))))

(defun weibo-timeline-insert-text (text)
  (let ((pos-begin (point)))
    (insert " " text "\n")
    (fill-region pos-begin (- (point) 1))
    (let ((pos-end (point)))
      (goto-char pos-begin)
      (while (search-forward-regexp "http://[0-9a-zA-Z\.\?&/]+" pos-end t)
	(make-text-button (match-beginning 0) (match-end 0)
			  'action (lambda (b) (browse-url (button-label b)))
			  'follow-link t))
      (goto-char pos-begin)
      (while (search-forward-regexp weibo-timeline-name-regexp pos-end t)
	(make-text-button (match-beginning 0) (match-end 0)
			  'action (lambda (b) (weibo-show-user (button-label b)))
			  'follow-link t))      
      (goto-char pos-end))))

(defun weibo-timeline-insert-picture (thumb_pic mid_pic)
  (when thumb_pic
    (insert "\t")
    (let ((begin_pos (point)))
      (weibo-insert-image (weibo-get-image-file thumb_pic) mid_pic)	    
      (when mid_pic
	(make-text-button begin_pos (point)
			  'face 'default
			  'action (lambda (b) (weibo-show-image (button-label b)))
			  'follow-link t)))
    (insert "\n")))

(defun weibo-timeline-parse-data (root front-t &optional clear-t)
  (when clear-t (ewoc-filter weibo-timeline-data (lambda (data) nil)))
  (when (weibo-check-result root)
    (let* ((proc-func (if front-t 'ewoc-enter-first 'ewoc-enter-last))
	   (tag (weibo-timeline-provider-tag weibo-timeline-current-provider))
	   (data (append (cdr (assoc tag root)) nil)))
      (mapc (function (lambda (node)
	       (apply proc-func (list weibo-timeline-data
				      (apply
				       (weibo-timeline-provider-make-function
					weibo-timeline-current-provider)
				       `(,node))))))
	    (if front-t (reverse data) data))))
  (run-with-idle-timer 0.2 nil 'weibo-download-image-in-queue))

(defun weibo-timeline-pull (new)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (let* ((pos (if new 0 -1))
	   (node (ewoc-nth weibo-timeline-data pos))
	   (node-data (and node (ewoc-data node))))
      (apply (weibo-timeline-provider-pull-function weibo-timeline-current-provider)
	     (list
	      node-data
	      'weibo-timeline-parse-data new
	      (weibo-timeline-provider-data weibo-timeline-current-provider))))
    (weibo-timeline-update)))

(defun weibo-timeline-pull-new ()
  (interactive)
  (weibo-timeline-pull t)
  (goto-char (point-min)))

(defun weibo-timeline-pull-old ()
  (interactive)
  (let ((p (point)))
    (weibo-timeline-pull nil)
    (goto-char p)))

(defun weibo-timeline-move-next ()
  (interactive)  
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (ewoc_node (and node (ewoc-next weibo-timeline-data node))))
    (if (not ewoc_node)
	(weibo-timeline-pull-old)
      (goto-char (ewoc-location ewoc_node))
      (recenter-top-bottom 0))))

(defun weibo-timeline-move-prev ()
  (interactive)  
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (ewoc_node (and node (ewoc-prev weibo-timeline-data node))))
    (when ewoc_node
      (goto-char (ewoc-location ewoc_node))
      (recenter-top-bottom 0))))

(defun weibo-timeline-retweet ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-retweet-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-comment ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-comment-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-reply ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-reply-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-post ()
  (interactive)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (let ((func (weibo-timeline-provider-post-function
		   weibo-timeline-current-provider)))
      (and func (apply func
		       (list (weibo-timeline-provider-data weibo-timeline-current-provider)))))))

(defun weibo-timeline-look ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-look-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-inspect ()
  (interactive)
  (let ((node (ewoc-locate weibo-timeline-data)))
    (when node
      (print (ewoc-data node))
      (ewoc-invalidate weibo-timeline-data node))))

(defun weibo-timeline-update-header (&optional msg)
  (ewoc-set-hf weibo-timeline-data
	       (concat (format weibo-timeline-headline
			       (mapconcat
				(lambda (item)
				  (let ((provider (cdr item)))
				    (concat
				     (weibo-timeline-provider-key provider) 
				     (weibo-timeline-provider-name provider))))
				weibo-timeline-providers " ")
			       (concat
				(when (weibo-timeline-provider-post-function
				 weibo-timeline-current-provider)
				  weibo-timeline-post-caption)
				(when (weibo-timeline-provider-look-function
				       weibo-timeline-current-provider)
				  weibo-timeline-look-caption)		       
				(when (weibo-timeline-provider-retweet-function
				       weibo-timeline-current-provider)
				  weibo-timeline-retweet-caption)
				(when (weibo-timeline-provider-comment-function
				       weibo-timeline-current-provider)
				  weibo-timeline-comment-caption)
				(when (weibo-timeline-provider-reply-function
				       weibo-timeline-current-provider)
				  weibo-timeline-reply-caption)))
		       (when (> (length msg) 0)
			 (concat "\n"
				 weibo-timeline-separator
				 "\n微博提示："
				 msg))
		       (let ((header-func
			      (weibo-timeline-provider-header-function
			       weibo-timeline-current-provider)))
			 (when header-func
			   (apply header-func
				  (list (weibo-timeline-provider-data
					 weibo-timeline-current-provider))))))
	       weibo-timeline-footline))

(defun weibo-timeline-refresh ()
  (interactive)
  (setq mode-name (format "%s-%s" weibo-timeline-mode-name
			  (weibo-timeline-provider-name
			   weibo-timeline-current-provider)))
  (weibo-timeline-update-header)
  (ewoc-filter weibo-timeline-data (lambda (data) nil))
  (weibo-timeline-pull-new)
  (weibo-user-get-friends))

(defun weibo-timeline-update ()
  (interactive)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (let* ((old-point (point))
	   (data-list (ewoc-collect weibo-timeline-data (lambda (obj) t)))
	   (func (weibo-timeline-provider-update-function
		  weibo-timeline-current-provider))
	   (result (and func
			(apply func (list data-list
					  (weibo-timeline-provider-data weibo-timeline-current-provider)))))
	   (result-list (and result (cdr result)))
	   (since-id (and result (car result)))
	   (unread (weibo-timeline-get-unread "")))
      (when result-list
	(ewoc-filter weibo-timeline-data (lambda (data) nil))
	(mapc (lambda (data)
		(ewoc-enter-last weibo-timeline-data data)) result-list))
      (weibo-timeline-update-header unread)
      (goto-char old-point))
    (run-with-idle-timer 0.2 nil 'weibo-download-image-in-queue)))

(defconst weibo-timeline-help-content
  "* 简介

\"使用Emacs的微博\"（weibo.emacs)是一个在Emacs上使用的微博客户端。它使用Elisp语言编写，提供了察看时间线，察看微博及评论，察看图片，发表和转发微博，发表评论和回复的功能。它基于Emacs，可以在Windows, Linux和Mac上使用。

* 功能

- 察看时间线
提供察看以下时间线的功能
    我的关注
    我的微博
    提到我的
    谁在说
    我的评论
    收到评论
    
- 察看微博和评论

- 提供察看某一条微博和评论的功能
发表微博，发表评论和回复评论

- 提供发表新微博，对某一条微薄发表评论及回复评论的功能

* 使用方法

- 授权

第一次使用时，将会自动跳转到微博应用授权的页面，登录后确认授权，然后将提示的pin码输回Emacs的mini-buffer即可。

- 开始

通过命令weibo-timeline开始察看时间线: M-x weibo-timeline

- 时间线界面

屏幕的最顶端，会出现如下三排提示：

微博：我的关注(a) 我的微博(i) 提到我的(@) 谁在说(w) 我的评论(o) 收到评论(c)

命令：发表微博(P) 察看(L) 转发(T) 评论(C) 回复(R)

操作：新消息(g) 刷新(r) 下一条（空格) 帮助(h) 退出(q)

微博行包括了不同的时间线及它们的切换方法。括号内的按键将在当前窗口显示对应的时间线：如提到我的(@)，当按下@键时，将切换提到我的时间线。

命令行包括了可以在时间线中使用的命令。除了发表微博外，其他命令都应将光标移到对应的微博或者评论上使用。

操作行包括了可以在时间线中使用的其他命令，如获取新消息，移到下一条消息，以及退出等等。

- 切换时间线

使用微博行所提示的按键进行切换。

- 察看时间线

使用Emacs默认的组合键移动光标。此外p，n，b，f键可用来上下左右移动光标。当光标移动到某一条微博或评论时，可按L键察看其对应的微博和最新评论。当微博中有图片时，会在时间线中显示缩略图。将光标移到图片上，按Enter键，会在新窗口中打开原图（按q键退出图片察看）。其他命令如操作中所示。

- 发表微博

在时间线界面中按P，将出现名为“发表微博”的窗口。在此窗口中编辑你想要发表的微博。完成后，按\\C-c\\C-c(Ctrl-c, Ctrl-c)提交微博。如果在编辑中想取消发表，按\\C-c\\C-d将关闭发表窗口（注意：此时你所编辑的内容将消失）。

- 转发微博

在时间线中，将光标移至某一条微博，按T将出现名为“转发微博”的窗口。此时操作与发表微博类似，完成编辑后，按\\C-c\\C-c提交微博，按\\C-c\\C-d取消。

- 发表评论和回复

在时间线中，按C对当前光标所在的微博发表评论。此时将出现“发表评论”窗口，操作与发表微博相似：完成编辑后，按\\C-c\\C-c提交评论，按\\C-c\\C-d取消。在微博详情和评论时间线中，按R键可以对评论进行回复。

- 备注

注意并非所有命令都对所有时间线适用。比如你不能对微博进行回复和对评论进行转发。命令行将之包含当前时间线可用的命令。")

(defun weibo-timeline-help ()
  (interactive)
  (with-output-to-temp-buffer "微博帮助"
    (princ weibo-timeline-help-content)))

(defvar weibo-timeline-mode-map
  nil
  "Keymap for weibo-timeline-mode")

(setq weibo-timeline-mode-map
      (let ((map (make-sparse-keymap)))
	
	(define-key map "g" 'weibo-timeline-pull-new)
	(define-key map "m" 'weibo-timeline-pull-old)
	(define-key map " " 'weibo-timeline-move-next)
	(define-key map "j" 'weibo-timeline-move-next)
	(define-key map "k" 'weibo-timeline-move-prev)	
	(define-key map "r" 'weibo-timeline-refresh)
	(define-key map "u" 'weibo-timeline-update)	
	(define-key map "s" 'weibo-timeline-inspect)
	(define-key map "h" 'weibo-timeline-help)
	
	(define-key map "P" 'weibo-timeline-post)
	(define-key map "L" 'weibo-timeline-look)
	(define-key map (kbd "RET") 'weibo-timeline-look)
	(define-key map "T" 'weibo-timeline-retweet)
	(define-key map "C" 'weibo-timeline-comment)
	(define-key map "R" 'weibo-timeline-reply)

	(define-key map "t" 'beginning-of-buffer)
	(define-key map "d" 'end-of-buffer)	
	(define-key map "n" 'next-line)
	(define-key map "p" 'previous-line)
	(define-key map "f" 'forward-char)
	(define-key map "b" 'backward-char)
	(define-key map (kbd "TAB") 'forward-button)
	
	(define-key map "q" 'weibo-bury-close-window)
	(define-key map "Q" 'weibo-timeline-close)
	map))

(defun weibo-timeline-close ()
  (interactive)
  (when weibo-timeline-timer
    (cancel-timer weibo-timeline-timer)
    (setq weibo-timeline-timer nil))
  (weibo-kill-close-window))

(defun weibo-timeline-name-nobreak-p ()
  (and (not (eq (char-after (point)) ?\@))
       (or (looking-back weibo-timeline-name-regexp)
	   (looking-back "@"))))

(define-derived-mode weibo-timeline-mode fundamental-mode weibo-timeline-mode-name
  "Major mode for displaying weibo timeline"
  (use-local-map weibo-timeline-mode-map)
  (setq buffer-read-only t)
  (setq fill-column 70)
  (make-local-variable 'weibo-timeline-data)
  (make-local-variable 'weibo-timeline-current-provider)
  (set (make-local-variable 'fill-nobreak-predicate) 'weibo-timeline-name-nobreak-p)
  (unless (ewoc-p weibo-timeline-data)
    (setq weibo-timeline-data
	  (ewoc-create 'weibo-timeline-pretty-printer
		       weibo-timeline-headline weibo-timeline-footline)))
  (unless (weibo-timeline-provider-p weibo-timeline-current-provider)
    (setq weibo-timeline-current-provider (cdar weibo-timeline-providers))
    (mapc (lambda (ele)
	    (let* ((key (weibo-timeline-provider-key (cdr ele)))
		   (switch-to-key (append '(lambda () (interactive))
					  `((weibo-timeline-switch-to-provider ,key)))))
	      (define-key weibo-timeline-mode-map key switch-to-key)))
	  weibo-timeline-providers)))

(defun weibo-timeline-buffer ()
  (with-current-buffer (get-buffer-create weibo-timeline-buffer-name)
    (unless (eq major-mode 'weibo-timeline-mode)
      (weibo-timeline-mode)
      (weibo-timeline-refresh)
      (unless (timerp weibo-timeline-timer)
	(setq weibo-timeline-timer (run-with-idle-timer 300 t
							'weibo-timeline-update))))
    (current-buffer)))

(defun weibo-timeline ()
  (interactive)
  (switch-to-buffer (weibo-timeline-buffer)))

(provide 'weibo-timeline)
