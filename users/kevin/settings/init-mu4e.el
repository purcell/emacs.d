;;================================================================
;; Config for email client mu4e
;;================================================================

(require 'mu4e)
(require 'smtpmail)
(require 'starttls)



;; Reading email config
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
         ("/send"       . ?s)
         ("/drafts"       . ?d)
         ;;("/Gmail/[Gmail].All Mail"    . ?a)
         )
      ;;message-signature
      mu4e-compose-signature
      (concat
       " -- "
       " \n "
       "陆健美 "
       " \n "
       "青岛办项目部 "
       " \n "
       "股票代码：300229 "
       " \n "
       "北京拓尔思信息技术股份有限公司 "
       " \n "
       "Tel：0532-68601852 "
       " \n "
       "Mobile：18661813293 E-mail：lu.jianmei@trs.com.cn "
       " \n "
       "Add：青岛市市南区山东路2号华仁国际大厦12F,G室 266200 "
       " \n "
       "官方网站：www.trs.com.cn "
       "\n")

      mu4e-view-show-images t
      mu4e-view-prefer-html t
      ;;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
      mu4e-html2text-command "html2text "
      mail-user-agent 'mu4e-user-agent
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


;; Send email config
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.qiye.163.com"
      smtpmail-smtp-server "smtp.qiye.163.com"
      smtpmail-smtp-service 25 ;;587(starttls) or 465(tls/ssl) or ?

      ;;tls-program '("gnutls-cli --priority NORMAL:%COMPAT -p %p %h")
      ;;starttls-gnutls-program "gnutls-cli --priority NORMAL:%COMPAT"
      starttls-gnutls-program "gnutls-cli"
      starttls-use-gnutls t
      smtpmail-debug-info t
      smtpmail-debug-verb t

      ;;starttls-extra-arguments '("--priority NORMAL:%COMPAT")
      starttls-extra-arguments '("--insecure")
      )

(provide 'init-mu4e)
;; init-mu4e.el end here
