;;================================================================
;; Config for email client mu4e
;;================================================================

(require 'mu4e)
(require 'smtpmail)
(setq mu4e-maildir "~/.mutt/mails/lu.jianmei")
(setq mu4e-drafts-folder "/drafts"
      mu4e-sent-folder   "/sent"
      mu4e-trash-folder  "/trash"
      mu4e-sent-messages-behavior 'delete
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 60
      user-mail-address "lu.jianmei@trs.com.cn"
      user-full-name  "Lu Jianmei"
      mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/haier"   . ?h)
         ("/pm"       . ?p)
         ("/trs"       . ?t)
         ;;("/Gmail/[Gmail].All Mail"    . ?a)
         )
      message-signature
      (concat
       " -- "
       " \n "
       "陆健美 "
       " \n "
       "青岛办项目部 "
       " \n "
       "股票代码：300229 "
       "北京拓尔思信息技术股份有限公司 "
       "Tel：0532-68601852 "
       "Mobile：18661813293 E-mail：lu.jianmei@trs.com.cn "
       "Add：青岛市市南区山东路2号华仁国际大厦12F,G室 266200 "
       "官方网站：www.trs.com.cn "
       "\n")

      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.qiye.163.com"
      smtpmail-smtp-server "smtp.qiye.163.com"
      smtpmail-smtp-service 587
      message-kill-buffer-on-exit t)

(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "html2text -utf8 -width 72")
(setq mail-user-agent 'mu4e-user-agent)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'init-mu4e)
;; init-mu4e.el end here
