(require 'color-theme)


(defun color-theme-sanityinc-light ()
  "Based on `color-theme-pierson`"
  (interactive)
  (color-theme-install
   '(color-theme-sanityinc-light
     ((background-color . "AntiqueWhite")
      (background-mode . light)
      (border-color . "black")
      (cursor-color . "Orchid")
      (foreground-color . "black")
      (mouse-color . "Orchid"))

     ;; Standard font lock faces
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (underline ((t (:underline t))))
     (italic ((t (:italic t))))
     (font-lock-builtin-face ((t (:foreground "Orchid"))))
     (font-lock-comment-delimiter-face ((t (:foreground "ForestGreen"))))
     (font-lock-comment-face ((t (:foreground "ForestGreen"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-doc-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "blue3"))))
     (font-lock-keyword-face ((t (:foreground "Blue"))))
     (font-lock-string-face ((t (:foreground "Firebrick"))))
     (font-lock-type-face ((t (:foreground "Purple"))))
     (font-lock-variable-name-face ((t (:foreground "blue3"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     ;; Search
     (isearch ((t (:foreground "#080808" :background "#edd400"))))
     (isearch-lazy-highlight-face ((t (:foreground "#080808" :background "#2e3436"))))

     ;; IDO
     (ido-subdir ((t (:foreground "Purple"))))
     (ido-first-match ((t (:foreground "Firebrick"))))
     (ido-only-match ((t (:foreground "ForestGreen"))))

     ;; Emacs interface
     (fringe ((t (:background "white"))))
     (border ((t (:background "white"))))
     (border-glyph ((t (nil))))
     (highlight ((t (:background "darkseagreen2"))))
     (gui-element ((t (:background "#0f0f0f" :foreground "black"))))
     (mode-line ((t (:foreground "antiquewhite" :background "black"))))
     (mode-line-buffer-id ((t (:foreground "antiquewhite" :background "black"))))
     (mode-line-inactive ((t (:foreground "antiquewhite" :background "darkgray"))))
     (minibuffer-prompt ((t (:foreground "blue3"))))
     (region ((t (:background "gray"))))
     (secondary-selection ((t (:background "paleturquoise"))))

     ;; Parenthesis matching
     (show-paren-match-face ((t (:background "turquoise"))))
     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
     )))

(defun color-theme-sanityinc-dark ()
  "Based on `color-theme-subdued"
  (interactive)
  (color-theme-install
   (let ((med-dark-choc    "#855c1b")
         (dark-aluminium   "#61635e")
         (dark-chameleon   "#4e9a06")
         (medium-chameleon "#73d216")
         (plum             "#77507b")
         (dark-butter      "#c4a000")
         (light-sky-blue   "#729fcf")
         (aluminium        "#888a85")
         (light-plum       "#ad7fa8")
         (scarlet          "#cc0000")
         (foreground-white "#d3d7cf")
         (background-black "#000")
         (very-dark-grey   "#0f0f0f")
         (dark-grey        "#1f1f1f")
         (light-grey       "#2e3436")
         (very-light-grey  "#eeeeec"))
     `(color-theme-sanityinc-dark
       ((foreground-color . ,foreground-white)
        (background-color . ,background-black)
        (background-mode . dark)
        (cursor-color . ,medium-chameleon)
        (mouse-color . ,medium-chameleon))

       ;; Standard font lock faces
       (default ((t (nil))))
       (font-lock-builtin-face ((t (:foreground ,med-dark-choc))))
       (font-lock-comment-delimiter-face ((t (:foreground ,dark-aluminium))))
       (font-lock-comment-face ((t (:foreground ,dark-aluminium))))
       (font-lock-constant-face ((t (:foreground ,dark-chameleon))))
       (font-lock-doc-face ((t (:foreground ,plum))))
       (font-lock-doc-string-face ((t (:foreground ,plum))))
       (font-lock-function-name-face ((t (:foreground ,dark-butter))))
       (font-lock-keyword-face ((t (:foreground ,light-sky-blue))))
       (font-lock-preprocessor-face ((t (:foreground ,aluminium))))
       (font-lock-regexp-grouping-backslash ((t (:foreground ,dark-aluminium))))
       (font-lock-regexp-grouping-construct ((t (:foreground ,light-plum))))
       (font-lock-string-face ((t (:foreground ,plum))))
       (font-lock-type-face ((t (:foreground ,light-plum))))
       (font-lock-variable-name-face ((t (nil))))
       (font-lock-warning-face ((t (:bold t :foreground ,scarlet))))

       ;; Search
       (isearch ((t (:foreground ,dark-grey :background ,dark-chameleon))))
       (isearch-lazy-highlight-face ((t (:foreground ,dark-grey :background ,light-sky-blue))))

       ;; IDO
       (ido-subdir ((t (:foreground ,aluminium))))
       (ido-first-match ((t (:foreground ,plum))))
       (ido-only-match ((t (:foreground ,light-plum))))

       ;; Auto-complete
       (popup-face ((t (:foreground ,background-black :background ,light-grey))))
       ;popup-menu-face
       ;popup-menu-selection-face
       (popup-scroll-bar-foreground-face ((t (:background ,dark-aluminium))))
       (popup-scroll-bar-background-face ((t (:background ,aluminium))))
       (ac-completion-face ((t (:foreground ,dark-grey :background ,light-sky-blue))))
       (ac-candidate-face ((t (:foreground ,background-black :background ,very-light-grey))))
       (ac-selection-face ((t (:foreground ,background-black :background ,dark-chameleon))))

       ;; Emacs Interface
       (fringe ((t (:background ,very-dark-grey))))
       (border ((t (:background ,very-dark-grey))))
       (border-glyph ((t (nil))))
       (gui-element ((t (:background ,very-dark-grey :foreground ,background-black))))
       (mode-line ((t (:background ,dark-grey :foreground ,very-light-grey))))
       (mode-line-buffer-id ((t (:background ,dark-grey :foreground ,very-light-grey))))
       (mode-line-inactive ((t (:background ,dark-grey :foreground ,aluminium))))
       (minibuffer-prompt ((t (:foreground ,light-sky-blue))))
       (region ((t (:background ,light-grey))))
       (secondary-selection ((t (:background ,aluminium :foreground ,dark-grey))))

       ;; Parenthesis matching
       (show-paren-match-face ((t (:foreground ,light-grey :background ,medium-chameleon))))
       (show-paren-mismatch-face ((t (:foreground ,very-light-grey :background ,scarlet))))

       ;; Calendar
       (holiday-face ((t (:foreground ,scarlet))))

       ;; Info
       (info-xref ((t (:foreground ,light-sky-blue))))
       (info-xref-visited ((t (:foreground ,light-plum))))

       ;; AUCTeX
       (font-latex-sectioning-5-face ((t (:foreground ,dark-butter))))
       (font-latex-bold-face ((t (:foreground ,dark-chameleon :bold t))))
       (font-latex-italic-face ((t (:foreground ,dark-chameleon :italic t))))
       (font-latex-math-face ((t (:foreground ,med-dark-choc))))
       (font-latex-string-face ((t (:foreground ,plum))))
       (font-latex-warning-face ((t (:foreground ,scarlet))))
       (font-latex-slide-title-face ((t (:foreground ,dark-butter))))
       ))))


(provide 'color-theme-sanityinc)