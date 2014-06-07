;;; Package --- Show line number

;; {{ see@http://stackoverflow.com/questions/5229705/emacs-org-mode-turn-off-line-numbers
(require 'linum-off)
;; }}

(global-linum-mode t)

(custom-set-variables
 '(linum-disabled-modes-list '(eshell-mode
			       shell-mode
			       erc-mode
			       help-mode
			       jabber-roster-mode
			       jabber-chat-mode
			       twittering-mode
			       compilation-mode
			       weibo-timeline-mode
			       woman-mode
			       Info-mode
			       calc-mode
			       calc-trail-mode
			       comint-mode
			       gnus-group-mode
			       inf-ruby-mode
			       gud-mode
			       term-mode
			       w3m-mode
			       speedbar-mode
			       gnus-summary-mode
			       gnus-article-mode
			       calendar-mode
			       image-mode
			       org-mode)))

(provide 'init-linum-mode)

