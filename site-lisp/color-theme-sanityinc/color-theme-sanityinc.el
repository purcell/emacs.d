(require 'color-theme)


(defun color-theme-sanityinc-light ()
  "Based on `color-theme-pierson`"
  (interactive)
  (color-theme-install
   (let ((background-white "AntiqueWhite1")
         (foreground-black "Grey15")
         (green "DarkOliveGreen4")
         (orange "DarkOrange3")
         (purple "Purple")
         (dark-purple "MediumPurple4")
         (blue "RoyalBlue3")
         (orchid "Orchid")
         (dark-rose "MistyRose4")
         (light-rose "MistyRose2")
         (pale-pink "PaleVioletRed")
         (bright-white "ivory1")
         (yellow "#edd400"))
     `(color-theme-sanityinc-light
       ((background-color . ,background-white)
        (background-mode . light)
        (border-color . ,foreground-black)
        (cursor-color . ,orchid)
        (foreground-color . ,foreground-black)
        (mouse-color . ,orchid))

       ;; Standard font lock faces
       (default ((t (nil))))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (underline ((t (:underline t))))
       (italic ((t (:italic t))))
       (font-lock-builtin-face ((t (:foreground ,dark-purple))))
       (font-lock-comment-delimiter-face ((t (:foreground ,dark-rose))))
       (font-lock-comment-face ((t (:foreground ,dark-rose))))
       (font-lock-constant-face ((t (:foreground "CadetBlue"))))
       (font-lock-doc-face ((t (:foreground ,pale-pink))))
       (font-lock-function-name-face ((t (:foreground ,green))))
       (font-lock-keyword-face ((t (:foreground ,blue))))
       (font-lock-string-face ((t (:foreground ,orange))))
       (font-lock-type-face ((t (:foreground ,purple))))
       (font-lock-variable-name-face ((t (:foreground ,green))))
       (font-lock-warning-face ((t (:bold t :foreground "Red"))))

       ;; Search
       (isearch ((t (:foreground ,background-white :background ,green))))
       (isearch-lazy-highlight-face ((t (:foreground ,foreground-black :background ,yellow))))

       ;; IDO
       (ido-subdir ((t (:foreground ,purple))))
       (ido-first-match ((t (:foreground ,orange))))
       (ido-only-match ((t (:foreground ,green))))

       ;; Emacs interface
       (fringe ((t (:background ,bright-white))))
       (border ((t (:background ,bright-white))))
       (border-glyph ((t (nil))))
       (highlight ((t (:background "darkseagreen2"))))
       (gui-element ((t (:background "#0f0f0f" :foreground ,foreground-black))))
       (mode-line ((t (:foreground ,background-white :background ,foreground-black))))
       (mode-line-buffer-id ((t (:foreground ,background-white :background ,foreground-black))))
       (mode-line-inactive ((t (:foreground ,dark-rose :background ,foreground-black))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (region ((t (:background "gray"))))
       (secondary-selection ((t (:background "paleturquoise"))))

       ;; Parenthesis matching
       (show-paren-match-face ((t (:background "turquoise"))))
       (show-paren-mismatch-face ((t (:background ,purple :foreground ,background-white))))

       (slime-highlight-edits-face ((t (:background ,light-rose))))

       (link ((t (:foreground ,blue :underline t))))
       (org-link ((t (:foreground ,blue :underline t))))
       (org-date ((t (:foreground ,blue :underline t))))
       (org-done ((t (:foreground ,green))))
       (org-todo ((t (:foreground "Maroon"))))
       (org-special-keyword ((t (:foreground ,pale-pink))))
       (org-level-1 ((t (:foreground ,dark-purple))))
       (org-level-2 ((t (:foreground ,foreground-black))))
       (org-level-3 ((t (:foreground ,orange))))
       (highlight-80+ ((t (:background "palegoldenrod"))))
       ))))

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
         (slate-blue       "#463b8a")
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

       (highlight-80+ ((t (:background ,slate-blue))))
       (link ((t (:foreground ,light-sky-blue :underline t))))
       (org-link ((t (:foreground ,light-sky-blue :underline t))))
       (org-date ((t (:foreground ,light-sky-blue :underline t))))
       (org-done ((t (:foreground "PaleGreen3"))))
       (org-todo ((t (:foreground ,light-plum))))
       (org-special-keyword ((t (:foreground ,plum))))
       (org-level-1 ((t (:foreground ,dark-butter))))
       (org-level-2 ((t (:foreground ,foreground-white))))
       (org-level-3 ((t (:foreground ,aluminium))))
       (slime-highlight-edits-face ((t (:background "gray17"))))

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