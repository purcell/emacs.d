(setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
(setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
(setq twittering-use-master-password t)

(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))
(autoload 'twittering-numbering "twittering-numbering" nil t)
(add-hook 'twittering-mode-hook
          (lambda ()
            (twittering-numbering)
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                                  (read-kbd-macro key) func)))
                  '(("F" . twittering-friends-timeline)
                    ("R" . twittering-replies-timeline)
                    ("U" . twittering-user-timeline)
                    ("W" . twittering-update-status-interactive)))))

(provide 'init-twittering-mode)
