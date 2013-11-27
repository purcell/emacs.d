(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Consolas")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 125)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  (set-fontset-font t 'han (font-spec :family "华文黑体" :size 12))
  ;; you may want to add different for other charset in this way.
  )
(provide 'my-font-settings)
