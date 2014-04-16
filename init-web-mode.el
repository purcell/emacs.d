(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xul?\\'" . web-mode))

(defun flymake-html-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "tidy" (list local-file))))

(defun flymake-html-load ()
  (interactive)
  (when (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
    (set (make-local-variable 'flymake-allowed-file-name-masks)
         '(("\\.html\\|\\.ctp\\|\\.ftl\\|\\.jsp\\|\\.php\\|\\.erb\\|\\.rhtml" flymake-html-init))
         )
    (set (make-local-variable 'flymake-err-line-patterns)
         '(("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)" nil 1 2 4))
         )
    (flymake-mode t)))

(add-hook 'web-mode-hook
          (lambda ()
            (flymake-html-load)
            (flyspell-mode 1)
            (remove-hook 'yas-after-exit-snippet-hook
                         'web-mode-yasnippet-exit-hook t)
            (remove-hook 'yas/after-exit-snippet-hook
                         'web-mode-yasnippet-exit-hook t)
            ))
(provide 'init-web-mode)
