;; (require-package 'cnfonts)
;; ;; 让 cnfonts 随着 Emacs 自动生效。
;; (cnfonts-enable)

(setq 4honor/fonts-profiles
      '("program" "orgmode" "read"))

(defun 4honor/fonts-switch-profile ()
  "Switch font profile to PROFILE-NAME"
  (interactive)
  (let ((profile (completing-read "Set font profile to: " 4honor/fonts-profiles nil t "orgmode")))
    (message profile)
    (cond
     (
      (string-collate-equalp profile "read")
      (4honor/fonts-profile-read))
     (
      (string-collate-equalp profile "program")
      (4honor/fonts-profile-program))
     (t
      (4honor/fonts-profile-orgmode)))))

(defun 4honor/fonts-profile-read ()
  "Font Profile for Reading"
  (set-face-attribute
   'default nil
   :font (font-spec :name "-*-Courier New-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 14.0))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-*-SimSun-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 16.5)))
  (message "Fonts profile read set."))

(defun 4honor/fonts-profile-orgmode ()
  "Font Profine for Org-mode"
  (set-face-attribute
   'default nil
   :font (font-spec :name "-*-Monaco-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 12.5))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-*-SimHei-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 14.5)))
  (message "Fonts profile orgmode set."))


(defun 4honor/fonts-profile-program ()
  "Font Profile for Programming"
  (set-face-attribute
   'default nil
   :font (font-spec :name "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 14.0))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-*-SimSun-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 16.5)))
  (message "Fonts profile program set."))

(4honor/fonts-profile-program)

(provide 'init-cnfonts)
