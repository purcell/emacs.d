;;; flatland-theme.el --- A simple theme for Emacs based on the Flatland theme for Sublime Text

;; Copyright (C) 2013 Greg Chapple

;; Author: Greg Chapple <info@gregchapple.com>
;; URL: http://github.com/gregchapple/flatland-emacs
;; Package-Version: 20140805.305
;; Version: 0.1.2

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

;; A port of the popular Sublime Text Flatland theme for Emacs 24

;;; Code:

(deftheme flatland "The Flatland color theme")

;;; Color Palette

(defvar flatland-colors-alist
  '(("flatland-bg+3"      . "#353a3d")
    ("flatland-bg+2"      . "#2e3235")
    ("flatland-bg+1"      . "#2e303a")
    ("flatland-bg"        . "#26292c")
    ("flatland-bg-1"      . "#1f2124")
    ("flatland-bg-05"     . "#202325")
    ("flatland-cursor"    . "#bbbcbd")
    ("flatland-fg"        . "#f8f8f8")
    ("flatland-fg-1"      . "#e0e0e0")
    ("flatland-selection" . "#3c3f42")
    ("flatland-green+4"   . "#b9d977")
    ("flatland-green+3"   . "#b8d977")
    ("flatland-green+2"   . "#b7d877")
    ("flatland-green+1"   . "#b6d877")
    ("flatland-green"     . "#40b83e")
    ("flatland-green-1"   . "#41a83e")
    ("flatland-blue+1"    . "#cfe2f2")
    ("flatland-blue"      . "#afc4db")
    ("flatland-blue-1"    . "#8996a8")
    ("flatland-blue-2"    . "#72aaca")
    ("flatland-blue-3"    . "#65a4a4")
    ("flatland-blue-4"    . "#0f0031")
    ("flatland-blue-5"    . "#0e2231")
    ("flatland-cyan"      . "#93e0e3")
    ("flatland-magenta"   . "#dc8cc3")
    ("flatland-organge+1" . "#ffb454")
    ("flatland-orange"    . "#f6aa11")
    ("flatland-orange-1"  . "#ffaa00")
    ("flatland-orange-2"  . "#fa9a4b")
    ("flatland-orange-3"  . "#df9400")
    ("flatland-yellow+4"  . "#ffffaa")
    ("flatland-yellow+3"  . "#f7f09d")
    ("flatland-yellow+2"  . "#edf080")
    ("flatland-yellow+1"  . "#edef7d")
    ("flatland-yellow"    . "#f6f080")
    ("flatland-yellow-1"  . "#f5f080")
    ("flatland-yellow-2"  . "#f1e94b")
    ("flatland-yellow-3"  . "#c4b14a")
    ("flatland-red+2"     . "#ff3a83")
    ("flatland-red+1"     . "#eb939a")
    ("flatland-red"       . "#ff4a52")
    ("flatland-red-1"     . "#ff0000")
    ("flatland-red-2"     . "#d8290d")
    ("flatland-red-3"     . "#d03620")
    ("flatland-red-4"     . "#c92b14")
    ("flatland-white"     . "#ffffff")
    ("flatland-white-1"   . "#f8f8f8")
    ("flatland-white-2"   . "#b1b3ba")
    ("flatland-white-3"   . "#b0b3ba")
    ("flatland-white-4"   . "#73817d")
    ("flatland-white-5"   . "#798188")
    )
  "List of flatland colors")


(defmacro flatland-with-color-variables (&rest body)
  "`let' bind all colors defined in `flatland-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   flatland-colors-alist))
     ,@body))

;;; Theme Faces
(flatland-with-color-variables
  (custom-theme-set-faces
   'flatland
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,flatland-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,flatland-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,flatland-fg :background ,flatland-bg))))
   `(cursor ((t (:foreground ,flatland-fg :background ,flatland-cursor))))
   `(escape-glyph ((t (:foreground ,flatland-yellow :bold t))))
   `(fringe ((t (:foreground ,flatland-fg :background ,flatland-bg+1))))
   `(header-line ((t (:foreground ,flatland-yellow
                                  :background ,flatland-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,flatland-bg-05))))
   `(success ((t (:foreground ,flatland-green :weight bold))))
   `(warning ((t (:foreground ,flatland-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,flatland-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,flatland-green))))
   `(compilation-error-face ((t (:foreground ,flatland-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,flatland-fg))))
   `(compilation-info-face ((t (:foreground ,flatland-blue))))
   `(compilation-info ((t (:foreground ,flatland-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,flatland-green))))
   `(compilation-line-face ((t (:foreground ,flatland-yellow))))
   `(compilation-line-number ((t (:foreground ,flatland-yellow))))
   `(compilation-message-face ((t (:foreground ,flatland-blue))))
   `(compilation-warning-face ((t (:foreground ,flatland-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,flatland-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,flatland-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,flatland-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,flatland-fg))))
   `(grep-error-face ((t (:foreground ,flatland-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,flatland-blue))))
   `(grep-match-face ((t (:foreground ,flatland-orange :weight bold))))
   `(match ((t (:background ,flatland-bg-1 :foreground ,flatland-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,flatland-yellow-2 :weight bold :background ,flatland-bg-1))))
   `(isearch-fail ((t (:foreground ,flatland-fg :background ,flatland-red-4))))
   `(lazy-highlight ((t (:foreground ,flatland-yellow-2 :weight bold :background ,flatland-bg-05))))

   `(menu ((t (:foreground ,flatland-fg :background ,flatland-bg))))
   `(minibuffer-prompt ((t (:foreground ,flatland-yellow))))
   `(mode-line
     ((,class (:foreground ,flatland-green+1
                           :background ,flatland-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,flatland-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,flatland-green-1
                      :background ,flatland-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,flatland-selection))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,flatland-bg+2))))
   `(trailing-whitespace ((t (:background ,flatland-red))))
   `(vertical-border ((t (:foreground ,flatland-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,flatland-orange-2 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,flatland-white-5))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,flatland-white-5))))
   `(font-lock-constant-face ((t (:foreground ,flatland-green+4))))
   `(font-lock-doc-face ((t (:foreground ,flatland-white-5))))
   `(font-lock-function-name-face ((t (:foreground ,flatland-blue-2))))
   `(font-lock-keyword-face ((t (:foreground ,flatland-orange-2 :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,flatland-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,flatland-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,flatland-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,flatland-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,flatland-blue+1))))
   `(font-lock-type-face ((t (:foreground ,flatland-blue-2))))
   `(font-lock-variable-name-face ((t (:foreground ,flatland-yellow))))
   `(font-lock-color-constant-face ((t (:foreground ,flatland-red+1))))
   `(font-lock-reference-face ((t (:foreground ,flatland-red+1))))
   `(font-lock-other-type-face ((t (:foreground ,flatland-red+1))))
   `(font-lock-special-keyword-face ((t (:foreground ,flatland-red+1))))
   `(font-lock-other-emphasized-face ((t (:foreground ,flatland-red+1))))
   `(font-lock-warning-face ((t (:foreground ,flatland-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,flatland-fg))))
   `(newsticker-default-face ((t (:foreground ,flatland-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,flatland-green+3))))
   `(newsticker-extra-face ((t (:foreground ,flatland-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,flatland-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,flatland-green))))
   `(newsticker-new-item-face ((t (:foreground ,flatland-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,flatland-red))))
   `(newsticker-old-item-face ((t (:foreground ,flatland-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,flatland-fg))))
   `(newsticker-treeview-face ((t (:foreground ,flatland-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,flatland-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,flatland-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,flatland-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,flatland-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,flatland-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,flatland-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,flatland-fg-1 :background ,flatland-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,flatland-green+2 :background ,flatland-bg :inverse-video nil))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,flatland-cyan :weight bold))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,flatland-fg))))
   `(ack-file ((t (:foreground ,flatland-blue))))
   `(ack-line ((t (:foreground ,flatland-yellow))))
   `(ack-match ((t (:foreground ,flatland-orange :background ,flatland-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:inherit font-lock-warning))))
   `(font-latex-sectioning-5-face ((t (:foreground ,flatland-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,flatland-yellow))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,flatland-fg-1 :foreground "black"))))
   `(ac-selection-face ((t (:background ,flatland-selection :foreground ,flatland-fg))))
   `(popup-tip-face ((t (:background ,flatland-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,flatland-bg+3))))
   `(popup-scroll-bar-background-face ((t (:background ,flatland-white))))
   `(popup-isearch-match ((t (:background ,flatland-bg :foreground ,flatland-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,flatland-green+1))))
   `(android-mode-error-face ((t (:foreground ,flatland-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,flatland-fg))))
   `(android-mode-verbose-face ((t (:foreground ,flatland-green))))
   `(android-mode-warning-face ((t (:foreground ,flatland-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,flatland-yellow-1 :foreground ,flatland-bg))))
   `(bm-fringe-face ((t (:background ,flatland-yellow-1 :foreground ,flatland-bg))))
   `(bm-fringe-persistent-face ((t (:background ,flatland-green-1 :foreground ,flatland-bg))))
   `(bm-persistent-face ((t (:background ,flatland-green-1 :foreground ,flatland-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,flatland-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,flatland-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,flatland-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,flatland-blue :foreground ,flatland-bg))))
   `(ctbl:face-continue-bar ((t (:background ,flatland-bg-05 :foreground ,flatland-bg))))
   `(ctbl:face-row-select ((t (:background ,flatland-cyan :foreground ,flatland-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,flatland-green+4 :background nil))
                 (t (:foreground ,flatland-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,flatland-yellow))))
   `(diff-removed ((,class (:foreground ,flatland-red :background nil))
                   (t (:foreground ,flatland-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,flatland-bg+2))
                  (t (:background ,flatland-fg :foreground ,flatland-bg))))
   `(diff-file-header
     ((,class (:background ,flatland-bg+2 :foreground ,flatland-fg :bold t))
      (t (:background ,flatland-fg :foreground ,flatland-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,flatland-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,flatland-orange))))
   `(diredp-date-time ((t (:foreground ,flatland-magenta))))
   `(diredp-deletion ((t (:foreground ,flatland-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,flatland-red))))
   `(diredp-dir-heading ((t (:foreground ,flatland-blue :background ,flatland-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,flatland-cyan))))
   `(diredp-exec-priv ((t (:foreground ,flatland-red))))
   `(diredp-executable-tag ((t (:foreground ,flatland-green+1))))
   `(diredp-file-name ((t (:foreground ,flatland-blue))))
   `(diredp-file-suffix ((t (:foreground ,flatland-green))))
   `(diredp-flag-mark ((t (:foreground ,flatland-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,flatland-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,flatland-red))))
   `(diredp-link-priv ((t (:foreground ,flatland-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,flatland-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,flatland-orange))))
   `(diredp-no-priv ((t (:foreground ,flatland-fg))))
   `(diredp-number ((t (:foreground ,flatland-green+1))))
   `(diredp-other-priv ((t (:foreground ,flatland-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,flatland-red-1))))
   `(diredp-read-priv ((t (:foreground ,flatland-green-1))))
   `(diredp-symlink ((t (:foreground ,flatland-yellow))))
   `(diredp-write-priv ((t (:foreground ,flatland-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,flatland-fg :background ,flatland-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,flatland-fg :background ,flatland-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,flatland-fg :background ,flatland-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,flatland-fg :background ,flatland-blue-5))))
   `(ediff-even-diff-A ((t (:background ,flatland-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,flatland-bg+1))))
   `(ediff-even-diff-B ((t (:background ,flatland-bg+1))))
   `(ediff-even-diff-C ((t (:background ,flatland-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,flatland-fg :background ,flatland-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,flatland-fg :background ,flatland-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,flatland-fg :background ,flatland-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,flatland-fg :background ,flatland-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,flatland-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,flatland-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,flatland-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,flatland-bg+2))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,flatland-green+4 :background ,flatland-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,flatland-red :background ,flatland-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,flatland-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,flatland-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,flatland-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,flatland-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,flatland-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,flatland-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,flatland-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,flatland-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-red) :inherit unspecified))
      (t (:foreground ,flatland-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-orange) :inherit unspecified))
      (t (:foreground ,flatland-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,flatland-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,flatland-orange :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flatland-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flatland-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flatland-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-orange) :inherit unspecified))
      (t (:foreground ,flatland-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flatland-red) :inherit unspecified))
      (t (:foreground ,flatland-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,flatland-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,flatland-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,flatland-yellow))))
   `(erc-keyword-face ((t (:foreground ,flatland-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,flatland-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,flatland-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,flatland-green))))
   `(erc-pal-face ((t (:foreground ,flatland-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,flatland-orange :background ,flatland-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,flatland-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,flatland-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,flatland-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,flatland-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,flatland-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,flatland-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,flatland-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,flatland-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,flatland-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,flatland-blue))))
   `(gnus-summary-high-read ((t (:foreground ,flatland-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,flatland-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,flatland-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,flatland-blue))))
   `(gnus-summary-low-read ((t (:foreground ,flatland-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,flatland-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,flatland-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,flatland-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,flatland-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,flatland-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,flatland-fg))))
   `(gnus-summary-selected ((t (:foreground ,flatland-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,flatland-blue))))
   `(gnus-cite-10 ((t (:foreground ,flatland-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,flatland-yellow))))
   `(gnus-cite-2 ((t (:foreground ,flatland-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,flatland-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,flatland-green+2))))
   `(gnus-cite-5 ((t (:foreground ,flatland-green+1))))
   `(gnus-cite-6 ((t (:foreground ,flatland-green))))
   `(gnus-cite-7 ((t (:foreground ,flatland-red))))
   `(gnus-cite-8 ((t (:foreground ,flatland-red-1))))
   `(gnus-cite-9 ((t (:foreground ,flatland-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,flatland-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,flatland-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,flatland-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,flatland-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,flatland-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,flatland-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,flatland-bg+2))))
   `(gnus-signature ((t (:foreground ,flatland-yellow))))
   `(gnus-x ((t (:background ,flatland-fg :foreground ,flatland-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,flatland-blue))))
   `(guide-key/key-face ((t (:foreground ,flatland-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,flatland-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,flatland-green
                      :background ,flatland-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,flatland-yellow
                      :underline nil
                      :weight bold
                      :box nil))))
   `(helm-selection ((t (:background ,flatland-selection :underline nil))))
   `(helm-selection-line ((t (:background ,flatland-bg+1))))
   `(helm-visible-mark ((t (:foreground ,flatland-bg :background ,flatland-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,flatland-green+4 :background ,flatland-bg-1))))
   `(helm-ff-directory ((t (:foreground ,flatland-magenta))))
   `(helm-buffer-directory ((t (:background ,flatland-blue-2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,flatland-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,flatland-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,flatland-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,flatland-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,flatland-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,flatland-yellow))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,flatland-bg+2 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,flatland-orange :weight bold))))
   `(js2-error ((t (:foreground ,flatland-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,flatland-green-1 :weight bold))))
   `(js2-jsdoc-type ((t (:foreground ,flatland-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,flatland-green+3))))
   `(js2-function-param ((t (:foreground, flatland-green+3))))
   `(js2-external-variable ((t (:foreground ,flatland-white))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,flatland-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,flatland-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,flatland-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,flatland-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,flatland-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,flatland-red+1))))
   `(jabber-activity-face((t (:foreground ,flatland-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,flatland-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,flatland-selection :background ,flatland-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,flatland-green+2 :background ,flatland-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,flatland-red+1 :background ,flatland-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,flatland-blue+1 :background ,flatland-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,flatland-magenta :background ,flatland-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,flatland-yellow :background ,flatland-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,flatland-yellow :background ,flatland-bg :weight bold :box nil :underline t))))
   `(magit-branch ((t (:foreground ,flatland-orange :background ,flatland-bg :weight bold :box nil))))
   `(magit-item-highlight ((t (:background ,flatland-bg+1 :bold nil))))
   `(magit-blame-header (( t (:box (:line-width 1 :style released-button)))))
   `(magit-blame-sha1 ((t (:foreground ,flatland-green :box (:line-width 1 :style released-button)))))
   `(magit-blame-subject ((t (:foreground ,flatland-yellow-2 :box (:line-width 1 :style released-button)))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,flatland-fg))))
   `(egg-help-header-1 ((t (:foreground ,flatland-yellow))))
   `(egg-help-header-2 ((t (:foreground ,flatland-green+3))))
   `(egg-branch ((t (:foreground ,flatland-yellow))))
   `(egg-branch-mono ((t (:foreground ,flatland-yellow))))
   `(egg-term ((t (:foreground ,flatland-yellow))))
   `(egg-diff-add ((t (:foreground ,flatland-green+4))))
   `(egg-diff-del ((t (:foreground ,flatland-red+1))))
   `(egg-diff-file-header ((t (:foreground ,flatland-yellow-2))))
   `(egg-section-title ((t (:foreground ,flatland-yellow))))
   `(egg-stash-mono ((t (:foreground ,flatland-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,flatland-green+1))))
   `(message-header-other ((t (:foreground ,flatland-green))))
   `(message-header-to ((t (:foreground ,flatland-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,flatland-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,flatland-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,flatland-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,flatland-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,flatland-green))))
   `(message-mml ((t (:foreground ,flatland-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,flatland-orange))))
   `(mew-face-header-from ((t (:foreground ,flatland-yellow))))
   `(mew-face-header-date ((t (:foreground ,flatland-green))))
   `(mew-face-header-to ((t (:foreground ,flatland-red))))
   `(mew-face-header-key ((t (:foreground ,flatland-green))))
   `(mew-face-header-private ((t (:foreground ,flatland-green))))
   `(mew-face-header-important ((t (:foreground ,flatland-blue))))
   `(mew-face-header-marginal ((t (:foreground ,flatland-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,flatland-red))))
   `(mew-face-header-xmew ((t (:foreground ,flatland-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,flatland-red))))
   `(mew-face-body-url ((t (:foreground ,flatland-orange))))
   `(mew-face-body-comment ((t (:foreground ,flatland-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,flatland-green))))
   `(mew-face-body-cite2 ((t (:foreground ,flatland-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,flatland-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,flatland-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,flatland-red))))
   `(mew-face-mark-review ((t (:foreground ,flatland-blue))))
   `(mew-face-mark-escape ((t (:foreground ,flatland-green))))
   `(mew-face-mark-delete ((t (:foreground ,flatland-red))))
   `(mew-face-mark-unlink ((t (:foreground ,flatland-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,flatland-green))))
   `(mew-face-mark-unread ((t (:foreground ,flatland-red-2))))
   `(mew-face-eof-message ((t (:foreground ,flatland-green))))
   `(mew-face-eof-part ((t (:foreground ,flatland-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,flatland-cyan :background ,flatland-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,flatland-bg :background ,flatland-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,flatland-bg :background ,flatland-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,flatland-blue))))
   `(mingus-pausing-face ((t (:foreground ,flatland-magenta))))
   `(mingus-playing-face ((t (:foreground ,flatland-cyan))))
   `(mingus-playlist-face ((t (:foreground ,flatland-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,flatland-yellow))))
   `(mingus-stopped-face ((t (:foreground ,flatland-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,flatland-yellow))))
   `(nav-face-button-num ((t (:foreground ,flatland-cyan))))
   `(nav-face-dir ((t (:foreground ,flatland-green))))
   `(nav-face-hdir ((t (:foreground ,flatland-red))))
   `(nav-face-file ((t (:foreground ,flatland-fg))))
   `(nav-face-hfile ((t (:foreground ,flatland-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,flatland-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,flatland-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,flatland-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,flatland-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,flatland-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,flatland-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,flatland-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,flatland-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,flatland-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,flatland-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,flatland-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,flatland-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,flatland-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,flatland-fg :weight bold))))
   `(org-checkbox ((t (:background ,flatland-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,flatland-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,flatland-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,flatland-green+3))))
   `(org-formula ((t (:foreground ,flatland-yellow-2))))
   `(org-headline-done ((t (:foreground ,flatland-green+3))))
   `(org-hide ((t (:foreground ,flatland-bg-1))))
   `(org-level-1 ((t (:foreground ,flatland-orange))))
   `(org-level-2 ((t (:foreground ,flatland-green+4))))
   `(org-level-3 ((t (:foreground ,flatland-blue-1))))
   `(org-level-4 ((t (:foreground ,flatland-yellow-2))))
   `(org-level-5 ((t (:foreground ,flatland-cyan))))
   `(org-level-6 ((t (:foreground ,flatland-green+2))))
   `(org-level-7 ((t (:foreground ,flatland-red-4))))
   `(org-level-8 ((t (:foreground ,flatland-blue-4))))
   `(org-link ((t (:foreground ,flatland-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,flatland-green+4))))
   `(org-scheduled-previously ((t (:foreground ,flatland-red-4))))
   `(org-scheduled-today ((t (:foreground ,flatland-blue+1))))
   `(org-sexp-date ((t (:foreground ,flatland-blue+1 :underline t))))
   `(org-special-keyword ((t (:foreground ,flatland-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,flatland-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,flatland-orange))))
   `(org-todo ((t (:bold t :foreground ,flatland-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,flatland-red :weight bold :underline nil))))
   `(org-column ((t (:background ,flatland-bg-1))))
   `(org-column-title ((t (:background ,flatland-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,flatland-orange))))
   `(outline-2 ((t (:foreground ,flatland-green+4))))
   `(outline-3 ((t (:foreground ,flatland-blue-1))))
   `(outline-4 ((t (:foreground ,flatland-yellow-2))))
   `(outline-5 ((t (:foreground ,flatland-cyan))))
   `(outline-6 ((t (:foreground ,flatland-green+2))))
   `(outline-7 ((t (:foreground ,flatland-red-4))))
   `(outline-8 ((t (:foreground ,flatland-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,flatland-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,flatland-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,flatland-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,flatland-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,flatland-bg+3 :inherit mode-line-inactive))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,flatland-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,flatland-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,flatland-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,flatland-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,flatland-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,flatland-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,flatland-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,flatland-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,flatland-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,flatland-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,flatland-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,flatland-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,flatland-blue))))
   `(rcirc-other-nick ((t (:foreground ,flatland-orange))))
   `(rcirc-bright-nick ((t (:foreground ,flatland-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,flatland-blue-2))))
   `(rcirc-server ((t (:foreground ,flatland-green))))
   `(rcirc-server-prefix ((t (:foreground ,flatland-green+1))))
   `(rcirc-timestamp ((t (:foreground ,flatland-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,flatland-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,flatland-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,flatland-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,flatland-green))))
   `(rpm-spec-doc-face ((t (:foreground ,flatland-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,flatland-red))))
   `(rpm-spec-macro-face ((t (:foreground ,flatland-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,flatland-red))))
   `(rpm-spec-package-face ((t (:foreground ,flatland-red))))
   `(rpm-spec-section-face ((t (:foreground ,flatland-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,flatland-blue))))
   `(rpm-spec-var-face ((t (:foreground ,flatland-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,flatland-orange))))
   `(rst-level-2-face ((t (:foreground ,flatland-green+1))))
   `(rst-level-3-face ((t (:foreground ,flatland-blue-1))))
   `(rst-level-4-face ((t (:foreground ,flatland-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,flatland-cyan))))
   `(rst-level-6-face ((t (:foreground ,flatland-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,flatland-red-3 :background ,flatland-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,flatland-blue-1 :background ,flatland-bg :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:background ,flatland-bg+3 :foreground ,flatland-red-3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,flatland-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,flatland-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,flatland-fg
                                    :background ,flatland-bg))))
   `(tabbar-selected ((t (:foreground ,flatland-fg
                                      :background ,flatland-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,flatland-fg
                                        :background ,flatland-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,flatland-bg
                                       :background ,flatland-bg-1))))
   `(term-color-red ((t (:foreground ,flatland-red-2
                                       :background ,flatland-red-4))))
   `(term-color-green ((t (:foreground ,flatland-green
                                       :background ,flatland-green+2))))
   `(term-color-yellow ((t (:foreground ,flatland-orange
                                       :background ,flatland-yellow))))
   `(term-color-blue ((t (:foreground ,flatland-blue-1
                                      :background ,flatland-blue-4))))
   `(term-color-magenta ((t (:foreground ,flatland-magenta
                                         :background ,flatland-red))))
   `(term-color-cyan ((t (:foreground ,flatland-cyan
                                       :background ,flatland-blue))))
   `(term-color-white ((t (:foreground ,flatland-fg
                                       :background ,flatland-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,flatland-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,flatland-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,flatland-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,flatland-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,flatland-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,flatland-green+2 :background ,flatland-bg))))
   `(w3m-lnum-match ((t (:background ,flatland-bg-1
                                     :foreground ,flatland-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,flatland-yellow))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,flatland-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,flatland-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,flatland-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,flatland-blue))))
   `(web-mode-css-selector-face ((t (:foreground ,flatland-green+1 :weight normal))))
   `(web-mode-css-property-name-face ((t (:foreground ,flatland-blue-2))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,flatland-green+1))))
   `(web-mode-html-attr-name-face ((t (:foreground ,flatland-yellow))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,flatland-green+1))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,flatland-blue-3))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:foreground ,flatland-white))))
   `(web-mode-server-background-face ((t (:background ,flatland-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,flatland-red))))
   
;;;;; css-mode
   `(css-property ((t (:foreground ,flatland-blue-2))))
   `(css-selector ((t (:foreground ,flatland-green+1))))

;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,flatland-bg+1 :foreground ,flatland-bg+1))))
   `(whitespace-hspace ((t (:background ,flatland-bg+1 :foreground ,flatland-bg+1))))
   `(whitespace-tab ((t (:background ,flatland-red-1))))
   `(whitespace-newline ((t (:foreground ,flatland-bg+1))))
   `(whitespace-trailing ((t (:background ,flatland-red))))
   `(whitespace-line ((t (:background ,flatland-bg :foreground ,flatland-magenta))))
   `(whitespace-space-before-tab ((t (:background ,flatland-orange :foreground ,flatland-orange))))
   `(whitespace-indentation ((t (:background ,flatland-yellow :foreground ,flatland-red))))
   `(whitespace-empty ((t (:background ,flatland-yellow))))
   `(whitespace-space-after-tab ((t (:background ,flatland-yellow :foreground ,flatland-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,flatland-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,flatland-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,flatland-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,flatland-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,flatland-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,flatland-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,flatland-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,flatland-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,flatland-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,flatland-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,flatland-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,flatland-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,flatland-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,flatland-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,flatland-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,flatland-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,flatland-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,flatland-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,flatland-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,flatland-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,flatland-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,flatland-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,flatland-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,flatland-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,flatland-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,flatland-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,flatland-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,flatland-bg-1 :foreground ,flatland-bg-1))))
   ))

;;; Theme Variables
(flatland-with-color-variables
  (custom-theme-set-variables
   'flatland
;;;;; ansi-color
   `(ansi-color-names-vector [,flatland-bg ,flatland-red ,flatland-green ,flatland-yellow
                                          ,flatland-blue ,flatland-magenta ,flatland-cyan ,flatland-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,flatland-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,flatland-red-1)
       ( 40. . ,flatland-red)
       ( 60. . ,flatland-orange)
       ( 80. . ,flatland-yellow-2)
       (100. . ,flatland-yellow-1)
       (120. . ,flatland-yellow)
       (140. . ,flatland-green-1)
       (160. . ,flatland-green)
       (180. . ,flatland-green+1)
       (200. . ,flatland-green+2)
       (220. . ,flatland-green+3)
       (240. . ,flatland-green+4)
       (260. . ,flatland-cyan)
       (280. . ,flatland-blue-2)
       (300. . ,flatland-blue-1)
       (320. . ,flatland-blue)
       (340. . ,flatland-blue+1)
       (360. . ,flatland-magenta)))
   `(vc-annotate-very-old-color ,flatland-magenta)
   `(vc-annotate-background ,flatland-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar flatland-add-font-lock-keywords nil
  "Whether to add font-lock keywords for flatland color names.
In buffers visiting library `flatland-theme.el' the flatland
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar flatland-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after flatland activate)
;;   "Maybe also add font-lock keywords for flatland colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or flatland-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "flatland-theme.el")))
;;     (unless flatland-colors-font-lock-keywords
;;       (setq flatland-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car flatland-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc flatland-colors-alist))))))
;;     (font-lock-add-keywords nil flatland-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after flatland activate)
;;   "Also remove font-lock keywords for flatland colors."
;;   (font-lock-remove-keywords nil flatland-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'flatland)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; flatland-theme.el ends here
