;;; lib-face.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary: reference from https://github.com/LuciusChen/.emacs.d.git
;;; Code:

(defun set-face-like-default (face)
  "Set FACE attributes to match the default face."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)
                      :weight (face-attribute 'default :weight)
                      :slant (face-attribute 'default :slant)))

(defun +setup-fonts ()
  "Setup fonts."
  ;; Setting the default
  (set-face-attribute 'default nil :font DEFAULT-FONT :weight 'normal)
  (set-face-like-default 'fixed-pitch-serif)
  (set-face-like-default 'variable-pitch)

  ;; https://www.wfonts.com/font/symbola
  (cl-loop for font in SYMBOL-FONT
           when (find-font (font-spec :name font))
           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

  ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
  ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
  ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
  ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
  ;; https://emacs-china.org/t/emacs/15676/34
  ;;
  ;; 另外 emoji 的尺寸会导致 corfu candidates 显示不全，因此要缩小。
  (cl-loop for font in EMOJI-FONTS
           when (find-font (font-spec :name font))
           return (set-fontset-font t 'emoji (font-spec :family font :size (* FONT-SIZE 0.85)) nil 'prepend))
  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the English font setting invalid
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family ZH-DEFAULT-FONT)))
  ;; Setting fall-back fonts
  ;; https://idiocy.org/emacs-fonts-and-fontsets.html
  (dolist (font FALLBACK-FONTS)
    (when (member font (font-family-list))
      (set-fontset-font "fontset-default" 'han font nil 'append)))
  ;; Force Emacs to search by using font-spec
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
  (when IS-LINUX
    ;; Set character composition rule for U+FE0F on Linux 2025-10-31
    ;;
    ;; Background:
    ;; On some Linux systems, U+FE0F (VARIATION SELECTOR-16) may display as a box
    ;; instead of the expected variant display (such as a colored emoji). This is
    ;; due to the lack of proper character composition rules.
    ;;
    ;; Solution:
    ;; Use the `set-char-table-range` function to set a composition rule for U+FE0F
    ;; in the `composition-function-table`. This ensures it combines correctly with
    ;; preceding characters to display as the intended variant.
    ;;
    ;; To avoid unnecessary settings on non-Linux systems, the `when` conditional
    ;; is used to apply this rule only in a Linux environment.
    ;; https://t.me/emacs_china/297476
    (set-char-table-range composition-function-table #xFE0F '(["\\c.\\c^+" 1 compose-gstring-for-graphic])))
  ;; Some characters are not being covered, so this workaround is used. 2025-04-07
  ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
  (let ((ranges '((#xE5FA . #xE6B7)    ;; Seti-UI + Custom
                  (#xE700 . #xE8EF)    ;; Devicons
                  (#xED00 . #xF2FF)    ;; Font Awesome
                  (#xE200 . #xE2A9)    ;; Font Awesome Extension
                  (#xF0001 . #xF1AF0)  ;; Material Design Icons
                  (#xE300 . #xE3E3)    ;; Weather
                  (#xF400 . #xF533)    ;; Octicons
                  (#x2665 . #x2665)    ;; Octicons
                  (#x26A1 . #x26A1)    ;; Octicons
                  (#xE000 . #xE00A)    ;; Pomicons
                  (#xEA60 . #xEC1E)))) ;; Codicons
    (dolist (range ranges)
      (set-fontset-font t range NERD-ICONS-FONT))))

(defun +suggest-other-faces (func &rest args)
  "Temporarily disable `global-hl-line-mode' while executing FUNC with ARGS."
  (let ((was-hl-line-mode-enabled global-hl-line-mode))
    (when was-hl-line-mode-enabled
      (global-hl-line-mode -1))
    (unwind-protect
        (apply func args)
      (when was-hl-line-mode-enabled
        (global-hl-line-mode 1)))))

;; 用于检查 unicode 是否被字体覆盖
(defun check-symbols-nerd-font-mono-coverage (unicode)
  "Check if 'Symbols Nerd Font Mono' covers the specified UNICODE character."
  (interactive "sEnter Unicode (e.g., 0F11E7): ")
  (let ((font-family "Symbols Nerd Font Mono")
        (char (string-to-number unicode 16))
        (buffer (get-buffer-create "*Font Check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Checking coverage for Unicode %s in font: %s\n\n" unicode font-family))
      (insert (propertize (string char)
                          'face `(:family ,font-family :height 200)))
      (display-buffer buffer))))

(provide 'lib-face)
;;; lib-face.el ends here
