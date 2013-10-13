(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun moz-custom-setup ()
  (moz-minor-mode 1)
  ;; @see  http://www.emacswiki.org/emacs/MozRepl
  ;; Example - you may want to add hooks for your own modes.
  ;; I also add this to python-mode when doing django development.
  (auto-reload-firefox-on-after-save-hook)
  )

(add-hook 'js2-mode-hook 'moz-custom-setup)
(add-hook 'html-mode-hook 'moz-custom-setup)
(add-hook 'nxml-mode-hook 'moz-custom-setup)
(add-hook 'web-mode-hook 'moz-custom-setup)

(eval-after-load 'moz
  '(progn
     (global-set-key (kbd "C-x p")
                     (lambda ()
                       (interactive)
                       (comint-send-string (inferior-moz-process)
                                           "BrowserReload();")))
     ))

(defun auto-reload-firefox-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimeout(BrowserReload(), '500');"))
            'append 'local)) ;; buffer-local

(provide 'init-moz)
