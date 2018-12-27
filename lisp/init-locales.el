(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'chinese-gb18030)
  (setq locale-coding-system 'chinese-gb18030)
  (set-default-coding-systems 'chinese-gb18030)
  (set-terminal-coding-system 'chinese-gb18030)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'chinese-gb18030 'utf-8))
  (prefer-coding-system 'chinese-gb18030))

;; Charset 设置
;; (use-package mule
;;              :ensure nil
;;              :config

;;              (set-language-environment "UTF-8")
;;              (set-buffer-file-coding-system 'utf-8-unix)
;;              (set-clipboard-coding-system 'utf-8-unix)
;;              (set-file-name-coding-system 'utf-8-unix)
;;              (set-keyboard-coding-system 'utf-8-unix)
;;              (set-next-selection-coding-system 'utf-8-unix)
;;              (set-selection-coding-system 'utf-8-unix)
;;              (set-terminal-coding-system 'utf-8-unix)

;;              (when (eq system-type 'windows-nt)
;;                (set-selection-coding-system 'gbk-dos)
;;                (set-next-selection-coding-system 'gbk-dos)
;;                (set-clipboard-coding-system 'gbk-dos)))

(provide 'init-locales)
