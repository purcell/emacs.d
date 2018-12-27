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
  (set-language-environment 'gb18030)
  (setq locale-coding-system 'gb18030)
  (set-default-coding-systems 'gb18030)
  (set-terminal-coding-system 'gb18030)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'gb18030 'gb18030))
  (prefer-coding-system 'gb18030))

(provide 'init-locales)
