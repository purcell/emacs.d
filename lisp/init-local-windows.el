;; windows-config.el - windows specific settings  -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :font "Aporetic Sans M Nerd Font-12")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cl-loop for font in '("Aporetic Sans M Nerd Font" "Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                ((eq system-type 'windows-nt) 12.5)))))

  (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                         "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 12.5)))
                                    nil 'prepend))

  (cl-loop for font in '("Maple Mono NL NF CN" "思源黑体CN" "思源宋体CN" "微软雅黑CN"
                         "Source Han Sans CN" "Source Han Serif CN"
                         "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 12.5)))
                                    nil 'prepend))

  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 12.5)))
                                    nil 'prepend)))

;; Windows paths with forward slashes
(setq default-directory "c:/Users/xxx/")

;; Default browser on Windows
(setq browse-url-browser-function 'browse-url-default-windows-browser)

;; Fix performance issues on Windows
(setq w32-get-true-file-attributes nil)
(setq inhibit-compacting-font-caches t)

;; Set cursor color
(set-face-attribute 'cursor nil :background "#d00000")

(provide 'init-local-windows)
