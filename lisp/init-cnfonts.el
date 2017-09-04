(require-package 'cnfonts)
;; 让 cnfonts 随着 Emacs 自动生效。
(cnfonts-enable)
;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
(cnfonts-set-spacemacs-fallback-fonts)
(setq cnfonts-profiles
      '("program" "org-mode" "read-book"))
(setq cnfonts-use-face-font-rescale t)

(provide 'init-cnfonts)
