;;; Package --- macOS specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; macOS specific key bindings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
;; (setq mac-control-modifier 'super)

; macOS specific font settings
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (and (eq system-type 'darwin) (display-graphic-p))
  (cl-loop for font in '("MonoLisa Nerd Font" "Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size 16.0)))
  (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Apple Color Emoji"
                         "Segoe UI Emoji" "EmojiOne Color" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size 16.0)
                                    nil 'prepend))
  (cl-loop for font in '("Maple Mono NF CN" "思源黑体 CN" "思源宋体 CN" "Source Han Sans CN" "Source Han Serif CN"
                         "微软雅黑 CN" "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size 16.0)))
  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size 16.0))))

;; Install sis
(use-package sis
  :ensure t
  :config
  ;; Configure input sources based on platform
  (when *is-a-mac*
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "im.rime.inputmethod.Squirrel.Hans")

    ;; Set cursor colors for different input sources
    (setq sis-default-cursor-color "#b81e19")  ; English input source
    (setq sis-other-cursor-color "#b81e19")    ; Other input source (change as needed)

    ;; Enable global modes
    (sis-global-cursor-color-mode t)
    (sis-global-respect-mode t)
    (sis-global-context-mode t)

    ;; Auto-switch to other input source when entering Evil insert mode
    (add-hook 'evil-insert-state-entry-hook #'sis-set-other)
    (add-hook 'evil-insert-state-exit-hook #'sis-set-english)))

(provide 'init-local-macos)
;;; init-local-macos.el ends here
