;;; color-theme-sanityinc-solarized.el --- A version of Ethan Schoonover's Solarized themes

;; Copyright (C) 2011-2014 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; Version: {{VERSION}}

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here are two slightly subdued color themes that are easy on the eyes
;; and cover a reasonably complete set of faces.

;; The themes are designed for use with Emacs' built-in theme support
;; in Emacs 24. However, they also work with older Emacs versions, in
;; which case color-theme.el is required.

;; Usage:

;; If your Emacs has the `load-theme' command, you can use it to
;; activate one of these themes programatically, or use
;; `customize-themes' to select a theme interactively.

;; Alternatively, or in older Emacs versions, use one of the provided
;; wrapper commands to activate a theme:

;;     M-x color-theme-sanityinc-solarized-light
;;     M-x color-theme-sanityinc-solarized-dark
;;
;;; Credit:

;; Genius colour selection by Ethan Schoonover:
;; http://ethanschoonover.com/solarized

;; Some faces borrowed from Greg Pfeil's emacs theme:
;; https://github.com/sellout/solarized/blob/master/emacs-color-theme-solarized/color-theme-solarized.el

;;; Code:

(require 'cl)

(defgroup color-theme-sanityinc-solarized nil
  "The sanityinc solarized theme pair."
  :group 'appearance
  :prefix "color-theme-sanityinc-solarized-")

(defcustom color-theme-sanityinc-solarized-rgb-is-srgb
  (or (not (eq window-system 'ns))
      (when (eval-when-compile (boundp 'ns-use-srgb-colorspace))
        ns-use-srgb-colorspace))
  "Indicates whether RGB triplets are treated as sRGB by the host Emacs.
Set this to t if using the sRGB patch on OS X."
  :group 'color-theme-sanityinc-solarized)

;; name     sRGB      Gen RGB   256       16              8
'((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
  (base02  "#073642" "#0a2832" "#262626" "black"         "black")
  (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
  (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
  (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
  (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
  (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
  (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
  (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
  (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
  (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
  (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
  (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
  (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
  (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
  (green   "#859900" "#728a05" "#5f8700" "green"         "green"))

(defmacro color-theme-sanityinc-solarized--with-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various solarized colors.

`MODE' should be set to either 'light or 'dark."
  ;; These are the Generic RGB equivalents of the "official" sRGB hex values
  `(let* ((srgb color-theme-sanityinc-solarized-rgb-is-srgb)
          (base03  (if srgb "#002b36" "#042028")) ; (0.0159 0.1265 0.1597)
          (base02  (if srgb "#073642" "#0a2832")) ; (0.0394 0.1601 0.1983)
          (base01  (if srgb "#586e75" "#465a61")) ; (0.2767 0.3567 0.3830)
          (base00  (if srgb "#657b83" "#52676f")) ; (0.3244 0.4072 0.4385)
          (base0   (if srgb "#839496" "#708183")) ; (0.4406 0.5096 0.5169)
          (base1   (if srgb "#93a1a1" "#81908f")) ; (0.5060 0.5649 0.5636)
          (base2   (if srgb "#eee8d5" "#e9e2cb")) ; (0.9161 0.8900 0.7978)
          (base3   (if srgb "#fdf6e3" "#fcf4dc")) ; (0.9894 0.9579 0.8641)
          (yellow  (if srgb "#b58900" "#a57705")) ; (0.6475 0.4675 0.0235)
          (orange  (if srgb "#cb4b16" "#bd3612")) ; (0.7418 0.2133 0.0735)
          (red     (if srgb "#dc322f" "#c60007")) ; (0.7770 0.0000 0.0290)
          (magenta (if srgb "#d33682" "#c61b6e")) ; (0.7774 0.1080 0.4352)
          (violet  (if srgb "#6c71c4" "#5859b7")) ; (0.3479 0.3514 0.7179)
          (blue    (if srgb "#268bd2" "#2075c7")) ; (0.1275 0.4627 0.7823)
          (cyan    (if srgb "#2aa198" "#259185")) ; (0.1468 0.5708 0.5250)
          (green   (if srgb "#859900" "#728a05")) ; (0.4498 0.5412 0.0202)
          (foregrounds (list base1 base0 base00 base01))
          (backgrounds (list base03 base02))
          (contrast-backgrounds (list base3 base2)))
     (when (eq 'light ,mode)
       (rotatef backgrounds contrast-backgrounds)
       (setq foregrounds (reverse foregrounds)))
     (let ((background (nth 0 backgrounds))
           (alt-background (nth 1 backgrounds))
           (strong (nth 0 foregrounds))
           (normal (nth 1 foregrounds))
           (faint (nth 2 foregrounds))
           (faintest (nth 3 foregrounds))
           (contrast-background (nth 1 contrast-backgrounds))
           (class '((class color) (min-colors 89))))
       ,@body)))

(defmacro color-theme-sanityinc-solarized--face-specs ()
  "Return a backquote which defines a list of face specs.

It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   (mapcar
    (lambda (entry)
      (list (car entry) `((,class ,@(cdr entry)))))
    `(;; Standard font lock faces
      (default (:foreground ,normal :background ,background))
      (bold (:weight bold))
      (bold-italic (:slant italic :weight bold))
      (underline (:underline t))
      (italic (:slant italic))
      (font-lock-builtin-face (:foreground ,violet))
      (font-lock-comment-delimiter-face (:foreground ,faintest :slant italic))
      (font-lock-comment-face (:foreground ,faintest :slant italic))
      (font-lock-constant-face (:foreground ,blue))
      (font-lock-doc-face (:foreground ,magenta))
      (font-lock-doc-string-face (:foreground ,violet))
      (font-lock-function-name-face (:foreground ,yellow))
      (font-lock-keyword-face (:foreground ,green))
      (font-lock-negation-char-face (:foreground ,blue))
      (font-lock-preprocessor-face (:foreground ,magenta))
      (font-lock-regexp-grouping-backslash (:foreground ,violet))
      (font-lock-regexp-grouping-construct (:foreground ,magenta))
      (font-lock-string-face (:foreground ,cyan))
      (font-lock-type-face (:foreground ,blue))
      (font-lock-variable-name-face (:foreground ,violet))
      (font-lock-warning-face (:weight bold :foreground ,red))
      (shadow (:foreground ,(fourth foregrounds)))
      (success (:foreground ,green))
      (error (:foreground ,red))
      (warning (:foreground ,orange))

      ;; Flycheck
      (flycheck-error (:underline (:style wave :color ,red)))
      (flycheck-info (:underline (:style wave :color ,cyan)))
      (flycheck-warning (:underline (:style wave :color ,yellow)))
      (flycheck-fringe-error (:foreground ,red :background ,red))
      (flycheck-fringe-info (:foreground ,cyan :background ,cyan))
      (flycheck-fringe-warning (:foreground ,yellow :background ,orange))

      ;; Flymake
      (flymake-warnline (:underline (:style wave :color ,yellow) :background ,background))
      (flymake-errline (:underline (:style wave :color ,red) :background ,background))

      ;; Clojure errors
      (clojure-test-failure-face (:background nil :inherit flymake-warnline))
      (clojure-test-error-face (:background nil :inherit flymake-errline))
      (clojure-test-success-face (:background nil :foreground nil :underline ,green))

      ;; For Brian Carper's extended clojure syntax table
      (clojure-keyword (:foreground ,yellow))
      (clojure-parens (:foreground ,strong))
      (clojure-braces (:foreground ,green))
      (clojure-brackets (:foreground ,yellow))
      (clojure-double-quote (:foreground ,cyan :background nil))
      (clojure-special (:foreground ,blue))
      (clojure-java-call (:foreground ,magenta))

      ;; Rainbow-delimiters
      (rainbow-delimiters-depth-1-face (:foreground ,normal))
      (rainbow-delimiters-depth-2-face (:foreground ,cyan))
      (rainbow-delimiters-depth-3-face (:foreground ,yellow))
      (rainbow-delimiters-depth-4-face (:foreground ,green))
      (rainbow-delimiters-depth-5-face (:foreground ,blue))
      (rainbow-delimiters-depth-6-face (:foreground ,normal))
      (rainbow-delimiters-depth-7-face (:foreground ,cyan))
      (rainbow-delimiters-depth-8-face (:foreground ,yellow))
      (rainbow-delimiters-depth-9-face (:foreground ,green))
      (rainbow-delimiters-unmatched-face (:foreground ,red))

      ;; EDTS errors
      (edts-face-warning-line (:background nil :inherit flymake-warnline))
      (edts-face-warning-mode-line (:background nil :foreground ,orange :weight bold))
      (edts-face-error-line (:background nil :inherit flymake-errline))
      (edts-face-error-mode-line (:background nil :foreground ,red :weight bold))

      ;; MMM-mode
      (mmm-code-submode-face (:background ,alt-background))
      (mmm-comment-submode-face (:inherit font-lock-comment-face))
      (mmm-output-submode-face (:background ,alt-background))

      ;; Search
      (match (:foreground ,blue :background ,background :inverse-video t))
      (isearch (:foreground ,yellow :background ,background :inverse-video t))
      (isearch-lazy-highlight-face (:foreground ,cyan :background ,background :inverse-video t))
      (isearch-fail (:background ,background :inherit font-lock-warning-face :inverse-video t))

      ;; Anzu
      (anzu-mode-line (:foreground ,orange))
      (anzu-replace-highlight (:inherit isearch-lazy-highlight-face))
      (anzu-replace-to (:inherit isearch))

      ;; IDO
      (ido-subdir (:foreground ,magenta))
      (ido-first-match (:foreground ,yellow))
      (ido-only-match (:foreground ,green))
      (ido-indicator (:foreground ,red :background ,background))
      (ido-virtual (:foreground ,faintest))

      (flx-highlight-face (:inherit nil :foreground ,cyan :weight normal :underline nil))

      ;; which-function
      (which-func (:foreground ,blue :background nil :weight bold))

      ;; Emacs interface
      (cursor (:background ,strong))
      (fringe (:background ,alt-background :foreground ,normal))
      (linum (:background ,alt-background :foreground ,green :italic nil))
      (border (:background ,alt-background))
      (border-glyph (nil))
      (highlight (:inverse-video nil :background ,alt-background))
      (gui-element (:background ,alt-background :foreground ,normal))
      (mode-line (:foreground nil :background ,alt-background :weight normal
                              :box (:line-width 1 :color ,normal)))
      (mode-line-buffer-id (:foreground ,magenta :background nil))
      (mode-line-inactive (:inherit mode-line
                                    :foreground ,faintest
                                    :background ,alt-background :weight normal
                                    :box (:line-width 1 :color ,normal)))
      (mode-line-emphasis (:foreground ,strong))
      (mode-line-highlight (:foreground ,magenta :box nil :weight bold))
      (minibuffer-prompt (:foreground ,blue))
      (region (:foreground ,strong :inverse-video t))
      (secondary-selection (:background ,alt-background))

      (header-line (:inherit mode-line :foreground ,magenta :background nil))
      (trailing-whitespace (:background ,red :underline nil))

      ;; Parenthesis matching (built-in)
      (show-paren-match (:background nil :foreground nil :inverse-video t))
      (show-paren-mismatch (:background ,magenta :foreground ,background))

      ;; Smartparens paren matching
      (sp-show-pair-match-face (:foreground nil :background nil :inherit show-paren-match))
      (sp-show-pair-mismatch-face (:foreground nil :background nil :inherit show-paren-mismatch))

      ;; Parenthesis matching (mic-paren)
      (paren-face-match (:foreground nil :background nil :inherit show-paren-match))
      (paren-face-mismatch (:foreground nil :background nil :inherit show-paren-mismatch))
      (paren-face-no-match (:foreground nil :background nil :inherit show-paren-mismatch))

      ;; Parenthesis dimming (parenface)
      (paren-face (:foreground ,faintest :background nil))

      (sh-heredoc (:foreground nil :inherit font-lock-string-face :weight normal))
      (sh-quoted-exec (:foreground nil :inherit font-lock-preprocessor-face))
      (slime-highlight-edits-face (:foreground ,strong))
      (slime-repl-input-face (:weight normal :underline nil))
      (slime-repl-prompt-face (:underline nil :weight bold :foreground ,magenta))
      (slime-repl-result-face (:foreground ,green))
      (slime-repl-output-face (:foreground ,blue :background ,background))

      (csv-separator-face (:foreground ,yellow))

      (diff-added (:foreground ,green))
      (diff-changed (:foreground ,violet))
      (diff-removed (:foreground ,orange))
      (diff-header (:foreground ,cyan :background nil))
      (diff-file-header (:foreground ,blue :background nil))
      (diff-hunk-header (:foreground ,magenta))
      (diff-refine-added (:inherit diff-added :inverse-video t))
      (diff-refine-removed (:inherit diff-removed :inverse-video t))

      (diff-hl-insert (:foreground ,green :background ,green))
      (diff-hl-change (:foreground ,blue :background ,blue))
      (diff-hl-delete (:foreground ,yellow :background ,yellow))
      (diff-hl-unknown (:foreground ,violet :background ,violet))

      (ediff-even-diff-A (:foreground nil :background nil :inverse-video t))
      (ediff-even-diff-B (:foreground nil :background nil :inverse-video t))
      (ediff-odd-diff-A  (:foreground ,faint :background nil :inverse-video t))
      (ediff-odd-diff-B  (:foreground ,faint :background nil :inverse-video t))

      (eldoc-highlight-function-argument (:foreground ,green :weight bold))

      ;; macrostep
      (macrostep-expansion-highlight-face (:inherit highlight :foreground nil))

      ;; undo-tree
      (undo-tree-visualizer-default-face (:foreground ,normal))
      (undo-tree-visualizer-current-face (:foreground ,green :weight bold))
      (undo-tree-visualizer-active-branch-face (:foreground ,red))
      (undo-tree-visualizer-register-face (:foreground ,yellow))

      ;; dired+
      (diredp-compressed-file-suffix (:foreground ,blue))
      (diredp-date-time (:foreground ,blue))
      (diredp-deletion (:inherit error :inverse-video t))
      (diredp-deletion-file-name (:inherit error))
      (diredp-dir-heading (:foreground ,green :background nil :weight bold))
      (diredp-dir-priv (:foreground ,cyan :background nil))
      (diredp-exec-priv (:foreground ,blue :background nil))
      (diredp-executable-tag (:foreground ,red :background nil))
      (diredp-file-name (:foreground ,yellow))
      (diredp-file-suffix (:foreground ,green))
      (diredp-flag-mark (:foreground ,green :inverse-video t))
      (diredp-flag-mark-line (:background nil :inherit highlight))
      (diredp-ignored-file-name (:foreground ,faintest))
      (diredp-link-priv (:background nil :foreground ,violet))
      (diredp-mode-line-flagged (:foreground ,red))
      (diredp-mode-line-marked (:foreground ,green))
      (diredp-no-priv (:background nil))
      (diredp-number (:foreground ,yellow))
      (diredp-other-priv (:background nil :foreground ,magenta))
      (diredp-rare-priv (:foreground ,red :background nil))
      (diredp-read-priv (:foreground ,green :background nil))
      (diredp-symlink (:foreground ,violet))
      (diredp-write-priv (:foreground ,yellow :background nil))

      ;; Magit
      (magit-header-line (:inherit nil :weight bold))
      (magit-dimmed (:foreground ,faintest))
      (magit-hash (:foreground ,faint))
      (magit-tag (:foreground ,yellow))
      (magit-branch-local (:foreground ,cyan))
      (magit-branch-remote (:foreground ,green))
      (magit-branch-current (:foreground ,blue))
      (magit-refname (:inherit comment))
      (magit-signature-good (:inherit success))
      (magit-signature-bad (:inherit error))
      (magit-signature-untrusted (:foreground ,cyan))
      (magit-signature-unmatched (:foreground ,cyan))
      (magit-cherry-equivalent (:foreground ,violet))

      (magit-log-graph (:foreground ,faint))
      (magit-log-author (:foreground ,orange))
      (magit-log-date (:foreground ,blue))

      ;; TODO: magit-{reflog,rebase,sequence,diff,blame}-*

      (magit-process-ok (:inherit success))
      (magit-process-ng (:inherit error))
      (magit-section-heading (:foreground ,yellow :weight bold))
      (magit-section-heading-selection (:foreground ,orange :weight bold))
      (magit-section-highlight (:inherit highlight))

      ;; git-gutter
      (git-gutter:modified (:foreground ,violet :weight bold))
      (git-gutter:added (:foreground ,green :weight bold))
      (git-gutter:deleted (:foreground ,red :weight bold))
      (git-gutter:unchanged (:background ,yellow))

      ;; git-gutter-fringe
      (git-gutter-fr:modified (:foreground ,violet :weight bold))
      (git-gutter-fr:added (:foreground ,green :weight bold))
      (git-gutter-fr:deleted (:foreground ,red :weight bold))

      ;; guide-key
      (guide-key/prefix-command-face (:foreground ,blue))
      (guide-key/highlight-command-face (:foreground ,green))
      (guide-key/key-face (:foreground ,faintest))

      (link (:foreground nil :underline t))
      (widget-button (:underline t))
      (widget-field (:background ,alt-background :box (:line-width 1 :color ,normal)))

      ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
      (compilation-column-number (:foreground ,yellow))
      (compilation-line-number (:foreground ,yellow))
      (compilation-message-face (:foreground ,blue))
      (compilation-mode-line-exit (:foreground ,green))
      (compilation-mode-line-fail (:foreground ,red))
      (compilation-mode-line-run (:foreground ,blue))

      ;; Grep
      (grep-context-face (:foreground ,faint))
      (grep-error-face (:foreground ,red :weight bold :underline t))
      (grep-hit-face (:foreground ,blue))
      (grep-match-face (:foreground nil :background nil :inherit match))

      (regex-tool-matched-face (:foreground nil :background nil :inherit match))

      ;; mark-multiple
      (mm/master-face (:inherit region :foreground nil :background nil))
      (mm/mirror-face (:inherit region :foreground nil :background nil))

      ;; helm
      (helm-buffer-saved-out (:inherit warning))
      (helm-buffer-size (:foreground ,violet))
      (helm-buffer-not-saved (:foreground ,yellow))
      (helm-buffer-process (:foreground ,cyan))
      (helm-buffer-directory (:foreground ,blue))
      (helm-ff-directory (:foreground ,cyan))
      (helm-candidate-number (:foreground ,red))
      (helm-match (:inherit match))
      (helm-selection (:inherit highlight))
      (helm-separator (:foreground ,magenta))
      (helm-source-header (:weight bold :foreground ,yellow :height 1.44))

      ;; company
      (company-preview (:foreground ,faint :background ,contrast-background))
      (company-preview-common (:inherit company-preview :foreground ,yellow))
      (company-preview-search (:inherit company-preview :foreground ,blue))
      (company-tooltip (:background ,contrast-background))
      (company-tooltip-selection (:background ,alt-background))
      (company-tooltip-common (:inherit company-tooltip :foreground ,yellow))
      (company-tooltip-common-selection (:inherit company-tooltip-selection :foreground ,yellow))
      (company-tooltip-search (:inherit company-tooltip :foreground ,blue))
      (company-tooltip-annotation (:inherit company-tooltip :foreground ,green))
      (company-scrollbar-bg (:inherit 'company-tooltip :background ,alt-background))
      (company-scrollbar-fg (:background ,contrast-background))
      (company-echo-common (:inherit company-echo :foreground ,yellow))

      ;; Term
      (term-color-black (:background ,base02 :foreground ,base02))
      (term-color-blue (:background ,blue :foreground ,blue))
      (term-color-cyan (:background ,cyan :foreground ,cyan))
      (term-color-green (:background ,green :foreground ,green))
      (term-color-magenta (:background ,magenta :foreground ,magenta))
      (term-color-red (:background ,red :foreground ,red))
      (term-color-white (:background ,base2 :foreground ,base2))
      (term-color-yellow (:background ,yellow :foreground ,yellow))

      ;; Eshell
      (eshell-ls-archive (:foreground ,cyan :weight normal))
      (eshell-ls-backup (:foreground ,yellow))
      (eshell-ls-clutter (:foreground ,orange :weight normal))
      (eshell-ls-directory (:foreground ,blue :weight normal))
      (eshell-ls-executable (:foreground ,red :weight normal))
      (eshell-ls-missing (:foreground ,violet :weight normal))
      (eshell-ls-product (:foreground ,yellow))
      (eshell-ls-readonly (:foreground ,base1))
      (eshell-ls-special (:foreground ,green :weight normal))
      (eshell-ls-symlink (:foreground ,magenta :weight normal))
      (eshell-ls-unreadable (:foreground ,base00))
      (eshell-prompt (:foreground ,green :weight normal))

      (org-agenda-structure (:foreground ,violet))
      (org-agenda-date (:foreground ,blue :underline nil))
      (org-agenda-done (:foreground ,green))
      (org-agenda-dimmed-todo-face (:foreground ,faint))
      (org-block (:foreground ,orange))
      (org-code (:foreground ,yellow))
      (org-column (:background ,alt-background))
      (org-column-title (:inherit org-column :weight bold :underline t))
      (org-date (:foreground ,blue :underline t))
      (org-document-info (:foreground ,cyan))
      (org-document-info-keyword (:foreground ,green))
      (org-document-title (:weight bold :foreground ,yellow :height 1.44))
      (org-done (:foreground ,green))
      (org-ellipsis (:foreground ,faint))
      (org-footnote (:foreground ,cyan))
      (org-formula (:foreground ,orange))
      (org-hide (:foreground ,background :background ,background))
      (org-link (:foreground ,blue :underline t))
      (org-scheduled (:foreground ,green))
      (org-scheduled-previously (:foreground ,yellow))
      (org-scheduled-today (:foreground ,green))
      (org-special-keyword (:foreground ,yellow))
      (org-table (:foreground ,violet))
      (org-todo (:foreground ,red))
      (org-upcoming-deadline (:foreground ,yellow))
      (org-warning (:weight bold :foreground ,red))

      (markdown-url-face (:inherit link))
      (markdown-link-face (:foreground ,blue :underline t))

      (hl-sexp-face (:background ,alt-background))
      (highlight-symbol-face (:inherit isearch-lazy-highlight-face))
      (highlight-80+ (:background ,alt-background))

      ;; Python-specific overrides
      (py-builtins-face (:foreground ,orange :weight normal))

      ;; js2-mode
      (js2-warning-face (:underline ,yellow))
      (js2-error-face (:foreground nil :underline ,red))
      (js2-external-variable-face (:foreground ,magenta))
      (js2-function-param-face (:foreground ,blue))
      (js2-instance-member-face (:foreground ,blue))
      (js2-private-function-call-face (:foreground ,red))

      ;; js3-mode
      (js3-warning-face (:underline ,yellow))
      (js3-error-face (:foreground nil :underline ,red))
      (js3-external-variable-face (:foreground ,magenta))
      (js3-function-param-face (:foreground ,blue))
      (js3-jsdoc-tag-face (:foreground ,magenta))
      (js3-jsdoc-type-face (:foreground ,cyan))
      (js3-jsdoc-value-face (:foreground ,violet))
      (js3-jsdoc-html-tag-name-face (:foreground ,blue))
      (js3-jsdoc-html-tag-delimiter-face (:foreground ,green))
      (js3-instance-member-face (:foreground ,blue))
      (js3-private-function-call-face (:foreground ,red))

      ;; coffee-mode
      (coffee-mode-class-name (:foreground ,yellow :weight bold))
      (coffee-mode-function-param (:foreground ,violet))

      ;; nxml
      (nxml-name-face (:foreground unspecified :inherit font-lock-constant-face))
      (nxml-attribute-local-name-face (:foreground unspecified :inherit font-lock-variable-name-face))
      (nxml-ref-face (:foreground unspecified :inherit font-lock-preprocessor-face))
      (nxml-delimiter-face (:foreground unspecified :inherit font-lock-keyword-face))
      (nxml-delimited-data-face (:foreground unspecified :inherit font-lock-string-face))
      (rng-error-face (:underline ,red))

      ;; RHTML
      (erb-delim-face (:background ,alt-background))
      (erb-exec-face (:background ,alt-background :weight bold))
      (erb-exec-delim-face (:background ,alt-background))
      (erb-out-face (:background ,alt-background :weight bold))
      (erb-out-delim-face (:background ,alt-background))
      (erb-comment-face (:background ,alt-background :weight bold :slant italic))
      (erb-comment-delim-face (:background ,alt-background))

      ;; Message-mode
      (message-header-other (:foreground nil :background nil :weight normal))
      (message-header-subject (:inherit message-header-other :weight bold :foreground ,yellow))
      (message-header-to (:inherit message-header-other :weight bold :foreground ,orange))
      (message-header-cc (:inherit message-header-to :foreground nil))
      (message-header-name (:foreground ,green :background nil))
      (message-header-newsgroups (:foreground ,cyan :background nil :slant normal))
      (message-separator (:foreground ,magenta))

      ;; Jabber
      (jabber-chat-prompt-local (:foreground ,yellow))
      (jabber-chat-prompt-foreign (:foreground ,orange))
      (jabber-chat-prompt-system (:foreground ,yellow :weight bold))
      (jabber-chat-text-local (:foreground ,yellow))
      (jabber-chat-text-foreign (:foreground ,orange))
      (jabber-chat-text-error (:foreground ,red))

      (jabber-roster-user-online (:foreground ,green))
      (jabber-roster-user-xa :foreground ,faint)
      (jabber-roster-user-dnd :foreground ,yellow)
      (jabber-roster-user-away (:foreground ,orange))
      (jabber-roster-user-chatty (:foreground ,violet))
      (jabber-roster-user-error (:foreground ,red))
      (jabber-roster-user-offline (:foreground ,faint))

      (jabber-rare-time-face (:foreground ,faint))
      (jabber-activity-face (:foreground ,violet))
      (jabber-activity-personal-face (:foreground ,cyan))

      ;; Powerline
      (powerline-active1 (:foreground ,normal :background ,contrast-background))
      (powerline-active2 (:foreground ,normal :background ,alt-background))

      ;; Outline
      (outline-1 (:inherit nil :foreground ,blue))
      (outline-2 (:inherit nil :foreground ,violet))
      (outline-3 (:inherit nil :foreground ,cyan))
      (outline-4 (:inherit nil :foreground ,yellow))
      (outline-5 (:inherit nil :foreground ,orange))
      (outline-6 (:inherit nil :foreground ,blue))
      (outline-7 (:inherit nil :foreground ,violet))
      (outline-8 (:inherit nil :foreground ,cyan))
      (outline-9 (:inherit nil :foreground ,yellow))

      ;; Ledger-mode
      (ledger-font-comment-face (:inherit font-lock-comment-face))
      (ledger-font-occur-narrowed-face (:inherit font-lock-comment-face :invisible t))
      (ledger-font-occur-xact-face (:inherit highlight))
      (ledger-font-payee-cleared-face (:foreground ,green))
      (ledger-font-payee-uncleared-face (:foreground ,cyan))
      (ledger-font-posting-date-face (:foreground ,yellow))
      (ledger-font-posting-amount-face (:foreground ,normal))
      (ledger-font-posting-account-cleared-face (:foreground ,blue))
      (ledger-font-posting-account-face (:foreground ,violet))
      (ledger-font-posting-account-pending-face (:foreground ,yellow))
      (ledger-font-xact-highlight-face (:inherit highlight))
      (ledger-occur-narrowed-face (:inherit font-lock-comment-face :invisible t))
      (ledger-occur-xact-face (:inherit highlight))

      ;; EMMS
      (emms-browser-artist-face (:inherit outline-2))
      (emms-browser-album-face (:inherit outline-3))
      (emms-browser-track-face (:inherit outline-4))
      (emms-browser-year/genre-face (:inherit outline-1))
      (emms-playlist-selected-face (:inverse-video t))
      (emms-playlist-track-face (:inherit outline-4))

      ;; mu4e
      (mu4e-header-highlight-face (:underline nil :inherit region))
      (mu4e-header-marks-face (:underline nil :foreground ,yellow))
      (mu4e-flagged-face (:foreground ,orange :inherit nil))
      (mu4e-replied-face (:foreground ,blue :inherit nil))
      (mu4e-unread-face (:foreground ,green :inherit nil))
      (mu4e-cited-1-face (:inherit outline-1 :slant normal))
      (mu4e-cited-2-face (:inherit outline-2 :slant normal))
      (mu4e-cited-3-face (:inherit outline-3 :slant normal))
      (mu4e-cited-4-face (:inherit outline-4 :slant normal))
      (mu4e-cited-5-face (:inherit outline-5 :slant normal))
      (mu4e-cited-6-face (:inherit outline-6 :slant normal))
      (mu4e-cited-7-face (:inherit outline-7 :slant normal))
      (mu4e-ok-face (:foreground ,green))
      (mu4e-view-contact-face (:inherit nil :foreground ,yellow))
      (mu4e-view-link-face (:inherit link :foreground ,blue))
      (mu4e-view-url-number-face (:inherit nil :foreground ,cyan))
      (mu4e-view-attach-number-face (:inherit nil :foreground ,orange))
      (mu4e-highlight-face (:inherit highlight))
      (mu4e-title-face (:inherit nil :foreground ,green))

      ;; Gnus
      (gnus-cite-1 (:inherit outline-1 :foreground nil))
      (gnus-cite-2 (:inherit outline-2 :foreground nil))
      (gnus-cite-3 (:inherit outline-3 :foreground nil))
      (gnus-cite-4 (:inherit outline-4 :foreground nil))
      (gnus-cite-5 (:inherit outline-5 :foreground nil))
      (gnus-cite-6 (:inherit outline-6 :foreground nil))
      (gnus-cite-7 (:inherit outline-7 :foreground nil))
      (gnus-cite-8 (:inherit outline-8 :foreground nil))
      ;; there are several more -cite- faces...
      (gnus-header-content (:inherit message-header-other))
      (gnus-header-subject (:inherit message-header-subject))
      (gnus-header-from (:inherit message-header-other-face :weight bold :foreground ,orange))
      (gnus-header-name (:inherit message-header-name))
      (gnus-button (:inherit link :foreground nil))
      (gnus-signature (:inherit font-lock-comment-face))

      (gnus-summary-normal-unread (:foreground ,strong :weight normal))
      (gnus-summary-normal-read (:foreground ,normal :weight normal))
      (gnus-summary-normal-ancient (:foreground ,cyan :weight normal))
      (gnus-summary-normal-ticked (:foreground ,orange :weight normal))
      (gnus-summary-low-unread (:foreground ,faint :weight normal))
      (gnus-summary-low-read (:foreground ,faintest :weight normal))
      (gnus-summary-low-ancient (:foreground ,faintest :weight normal))
      (gnus-summary-high-unread (:foreground ,yellow :weight normal))
      (gnus-summary-high-read (:foreground ,green :weight normal))
      (gnus-summary-high-ancient (:foreground ,green :weight normal))
      (gnus-summary-high-ticked (:foreground ,orange :weight normal))
      (gnus-summary-cancelled (:foreground ,red :background nil :weight normal))

      (gnus-group-mail-low (:foreground ,faintest))
      (gnus-group-mail-low-empty (:foreground ,faintest))
      (gnus-group-mail-1 (:foreground nil :weight normal :inherit outline-1))
      (gnus-group-mail-2 (:foreground nil :weight normal :inherit outline-2))
      (gnus-group-mail-3 (:foreground nil :weight normal :inherit outline-3))
      (gnus-group-mail-4 (:foreground nil :weight normal :inherit outline-4))
      (gnus-group-mail-5 (:foreground nil :weight normal :inherit outline-5))
      (gnus-group-mail-6 (:foreground nil :weight normal :inherit outline-6))
      (gnus-group-mail-1-empty (:inherit gnus-group-mail-1 :foreground ,faint))
      (gnus-group-mail-2-empty (:inherit gnus-group-mail-2 :foreground ,faint))
      (gnus-group-mail-3-empty (:inherit gnus-group-mail-3 :foreground ,faint))
      (gnus-group-mail-4-empty (:inherit gnus-group-mail-4 :foreground ,faint))
      (gnus-group-mail-5-empty (:inherit gnus-group-mail-5 :foreground ,faint))
      (gnus-group-mail-6-empty (:inherit gnus-group-mail-6 :foreground ,faint))
      (gnus-group-news-1 (:foreground nil :weight normal :inherit outline-5))
      (gnus-group-news-2 (:foreground nil :weight normal :inherit outline-6))
      (gnus-group-news-3 (:foreground nil :weight normal :inherit outline-7))
      (gnus-group-news-4 (:foreground nil :weight normal :inherit outline-8))
      (gnus-group-news-5 (:foreground nil :weight normal :inherit outline-1))
      (gnus-group-news-6 (:foreground nil :weight normal :inherit outline-2))
      (gnus-group-news-1-empty (:inherit gnus-group-news-1 :foreground ,faint))
      (gnus-group-news-2-empty (:inherit gnus-group-news-2 :foreground ,faint))
      (gnus-group-news-3-empty (:inherit gnus-group-news-3 :foreground ,faint))
      (gnus-group-news-4-empty (:inherit gnus-group-news-4 :foreground ,faint))
      (gnus-group-news-5-empty (:inherit gnus-group-news-5 :foreground ,faint))
      (gnus-group-news-6-empty (:inherit gnus-group-news-6 :foreground ,faint))

      (erc-direct-msg-face (:foreground ,yellow))
      (erc-error-face (:foreground ,red))
      (erc-header-face (:foreground ,strong :background ,alt-background))
      (erc-input-face (:foreground ,green))
      (erc-current-nick-face (:foreground ,green))
      (erc-my-nick-face (:foreground ,green))
      (erc-nick-default-face (:weight normal :foreground ,violet))
      (erc-nick-msg-face (:weight normal :foreground ,yellow))
      (erc-notice-face (:foreground ,faintest))
      (erc-pal-face (:foreground ,orange))
      (erc-prompt-face (:foreground ,blue))
      (erc-timestamp-face (:foreground ,cyan))
      (erc-keyword-face (:foreground ,green))

      ;; twittering-mode
      (twittering-username-face (:inherit erc-pal-face))
      (twittering-uri-face (:foreground ,blue :inherit link))
      (twittering-timeline-header-face (:foreground ,green :weight bold))
      (twittering-timeline-footer-face (:inherit twittering-timeline-header-face))

      (custom-variable-tag (:foreground ,blue))
      (custom-group-tag (:foreground ,blue))
      (custom-state-tag (:foreground ,green))

      ;; ansi-term
      (term (:foreground nil :background nil :inherit default))
      (term-color-black   (:foreground ,normal :background ,normal))
      (term-color-red     (:foreground ,red :background ,red))
      (term-color-green   (:foreground ,green :background ,green))
      (term-color-yellow  (:foreground ,yellow :background ,yellow))
      (term-color-blue    (:foreground ,blue :background ,blue))
      (term-color-magenta (:foreground ,magenta :background ,magenta))
      (term-color-cyan    (:foreground ,cyan :background ,cyan))
      (term-color-white   (:foreground ,background :background ,background))
      ))))

(defmacro color-theme-sanityinc-solarized--frame-parameter-specs ()
  "Return a backquote which defines a list of frame parameter specs.

These are required by color-theme's `color-theme-install', but
not by the new `deftheme' mechanism. It expects to be evaluated
in a scope in which the various color names to which it refers
are bound."
  (quote
   `(((background-color . ,background)
      (background-mode . light)
      (border-color . ,normal)
      (cursor-color . ,magenta)
      (foreground-color . ,normal)
      (mouse-color . ,cyan)))))


(defmacro color-theme-sanityinc-solarized--define-theme (mode)
  "Define either the dark or the light theme.
Argument MODE: 'light or 'dark"
  (let ((name (intern (format "sanityinc-solarized-%s" (symbol-name mode))))
        (doc (format "A version of Ethan Schoonover's 'Solarized' theme (%s version)" mode)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (color-theme-sanityinc-solarized--with-colors
        ',mode
        (apply 'custom-theme-set-faces ',name
               (color-theme-sanityinc-solarized--face-specs))
        (custom-theme-set-variables
         ',name
         `(fci-rule-color ,alt-background)
         `(vc-annotate-color-map
           '((20 . ,red)
             (40 . ,orange)
             (60 . ,yellow)
             (80 . ,green)
             (100 . ,cyan)
             (120 . ,blue)
             (140 . ,magenta)
             (160 . ,violet)
             (180 . ,red)
             (200 . ,orange)
             (220 . ,yellow)
             (240 . ,green)
             (260 . ,cyan)
             (280 . ,blue)
             (300 . ,magenta)
             (320 . ,violet)
             (340 . ,red)
             (360 . ,orange)))
         `(vc-annotate-very-old-color nil)
         `(vc-annotate-background nil)
         `(ansi-color-names-vector (vector ,normal ,red ,green ,yellow ,blue ,magenta ,cyan ,contrast-background))
         '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
       (provide-theme ',name))))


(defun color-theme-sanityinc-solarized (mode)
  "Apply either the dark or the light theme."
  (if (fboundp 'load-theme)
      (let ((name (cond
                    ((eq 'light mode) 'sanityinc-solarized-light)
                    ((eq 'dark mode) 'sanityinc-solarized-dark)
                    (t (error "invalid mode: %s" mode)))))
        (if (boundp 'custom-enabled-themes)
            (custom-set-variables `(custom-enabled-themes '(,name)))
          (if (> emacs-major-version 23)
              (load-theme name t)
            (load-theme name))))
    (progn
      (require 'color-theme)
      (color-theme-sanityinc-solarized--with-colors
       mode
       (color-theme-install
        `(,(intern (concat "color-theme-sanityinc-solarized-" (symbol-name mode)))
          ,@(color-theme-sanityinc-solarized--frame-parameter-specs)
          ,@(color-theme-sanityinc-solarized--face-specs)))
       ;; ansi-color - comint and other modes that handle terminal color escape sequences
       (setq ansi-color-names-vector (vector normal red green yellow blue magenta cyan contrast-background))
       (setq ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun color-theme-sanityinc-solarized-dark ()
  "Apply the dark solarized theme."
  (interactive)
  (color-theme-sanityinc-solarized 'dark))

;;;###autoload
(defun color-theme-sanityinc-solarized-light ()
  "Apply the light solarized theme."
  (interactive)
  (color-theme-sanityinc-solarized 'light))


(provide 'color-theme-sanityinc-solarized)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; color-theme-sanityinc-solarized.el ends here
