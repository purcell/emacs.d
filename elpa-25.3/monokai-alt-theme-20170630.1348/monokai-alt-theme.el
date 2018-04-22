;;; monokai-alt-theme.el --- Theme with a dark background. Based on sublime monokai theme.

;; Copyright (C) 2017, Dmytro Koval

;; Author: Dmytro Koval
;; URL: https://github.com/dawidof/emacs-monokai-theme
;; Package-Version: 20170630.1348
;; Version: 0.105
;; Package-Requires: ((emacs "24"))
;; Created with emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

(deftheme monokai-alt)
(let ((class '((class color) (min-colors 89)))
      (fg1 "#f8f8f2")
      (fg2 "#bdbdb1")
      (fg3 "#ababa0")
      (fg4 "#999990")
      (bg1 "#262626")
      (bg2 "#363631")
      (bg3 "#454640")
      (bg4 "#545550")
      (key2 "#fd4d80")
      (key3 "#db2765")
      (builtin "#66d9ef")
      (keyword "#f92672")
      (const   "#a281ff")
      (comment "#626262")
      (func    "#9ce22e")
      (str     "#e6db74")
      (type    "#9ce22e")
      (var     "#f92672")
      (warning "#ff0000"))
  (custom-theme-set-faces
   'monokai-alt
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background  ,bg2))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
   `(cursor ((,class (:background ,bg3))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))

   ;; modeline (status)
   `(mode-line ((,class (:box (:line-width 3 :color ,bg1) :bold t :foreground ,fg4 :background ,bg1))))
   `(mode-line-inactive ((,class (:box (:line-width 3 :color ,bg2) :foreground ,comment :background ,bg2 :weight normal))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,func :background nil))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))

   `(vertical-border ((,class (:foreground ,fg3))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,key3))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))

   ;; org
   `(org-code ((,class (:foreground ,fg2))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.1))))
   `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
   `(org-level-3 ((,class (:bold t :foreground ,fg4))))
   `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
   `(org-agenda-done ((,class (:foreground ,bg4))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(org-sexp-date ((,class (:foreground ,fg4))))

   ;; font-latex
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ido-only-match ((,class (:foreground ,warning))))

   ;; gnus
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(ffap ((,class (:foreground ,fg4))))

   ;; js
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   `(js3-error-face ((,class (:underline ,warning))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,key3))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-instance-member-face ((,class (:foreground ,const))))

   `(warning ((,class (:foreground ,warning))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))

   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))

   ;; magit
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))

   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,class (:foreground "#7fff00"))))
   `(realgud-overlay-arrow2
     ((,class (:foreground "cyan"))))
   `(realgud-overlay-arrow3
      ((,class  (:foreground "#5FAFAF"))))
   ;; term
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
   `(term-color-blue ((,class (:foreground ,func :background ,func))))
   `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
   `(term-color-green ((,class (:foreground ,type :background ,bg3))))
   `(term-color-yellow ((,class (:foreground ,var :background ,var))))
   `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
   `(term-color-cyan ((,class (:foreground ,str :background ,str))))
   `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))

   ;; helm
   `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
   `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
   `(helm-selection ((,class (:background ,bg2 :underline nil))))
   `(helm-selection-line ((,class (:background ,bg2))))
   `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
   `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
   `(helm-separator ((,class (:foreground ,type :background ,bg1))))
   `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
   `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
   `(helm-ff-executable ((,class (:foreground ,key2 :background ,bg1 :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,key3 :background ,bg1 :weight bold))))
   `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
   `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
   `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
   `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
   `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
   `(helm-bookmark-w3m ((,class (:foreground ,type))))

   ;; company
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,key2))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
   `(company-tooltop-annotation ((,class (:foreground ,const))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-template-field ((,class (:inherit region))))


   ;; web-mode
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))

   ;; linum
   `(linum ((t (:foreground ,bg4 :background ,bg1 :height 95))))

   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,bg2 :background ,bg1 :height 95))))


   ;; jde
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,key3))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))

   ;; buttons
   '(custom-button ((t (:background "gray30" :foreground "white smoke" :box (:line-width 2 :color "gray68" :style released-button)))))
   '(custom-button-mouse ((t (:background "gray23" :foreground "white smoke" :box (:line-width 2 :style released-button)))))

   ;; window
   '(window-divider ((t (:foreground "gray20"))))
   '(window-divider-first-pixel ((t nil)))

    ;; smerge (diff tool in files)
    '(smerge-base ((t (:background "#24292e"))))
    '(smerge-mine ((t (:background "#997676"))))
    '(smerge-other ((t (:background "PaleGreen4"))))
    '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#405540"))))
    '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#705353"))))

    '(hl-line ((t (:background "#363631"))))
    ))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'monokai-alt)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 120
;; End:

;;; monokai-alt-theme.el ends here
