;;; monokai-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai-emacs
;; Package-Version: 20171013.236
;; Version: 3.3.0

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
;;
;; A port of the popular Textmate theme Monokai for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal monokai theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/monokai-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The monokai theme requires Emacs 24 or later!"))

(deftheme monokai "The Monokai colour theme")

(defgroup monokai nil
  "Monokai theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom monokai-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'monokai)

(defcustom monokai-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'monokai)

(defcustom monokai-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monokai)

;; Primary colors
(defcustom monokai-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'monokai)

(defcustom monokai-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'monokai)

(defcustom monokai-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'monokai)

(defcustom monokai-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'monokai)

(defcustom monokai-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'monokai)

(defcustom monokai-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'monokai)

(defcustom monokai-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'monokai)

(defcustom monokai-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'monokai)

(defcustom monokai-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'monokai)

(defcustom monokai-foreground "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'monokai)

(defcustom monokai-background "#272822"
  "Adaptive colors - background"
  :type 'string
  :group 'monokai)

(defcustom monokai-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'monokai)

(defcustom monokai-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'monokai)

(defcustom monokai-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'monokai)

(let* (;; Variable pitch
       (monokai-pitch (if monokai-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (monokai-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (monokai-yellow-d       "#BEB244")
       (monokai-yellow-l       "#FFF7A8")
       (monokai-orange-d       "#D47402")
       (monokai-orange-l       "#FFAC4A")
       (monokai-red-d          "#F70057")
       (monokai-red-l          "#FA518D")
       (monokai-magenta-d      "#FB35EA")
       (monokai-magenta-l      "#FE8CF4")
       (monokai-violet-d       "#945AFF")
       (monokai-violet-l       "#C9ACFF")
       (monokai-blue-d         "#40CAE4")
       (monokai-blue-l         "#92E7F7")
       (monokai-cyan-d         "#74DBCD")
       (monokai-cyan-l         "#D3FBF6")
       (monokai-green-d        "#86C30D")
       (monokai-green-l        "#BBEF53")
       (monokai-gray-d         "#35331D")
       (monokai-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-foreground-hc  "#141414")
       (monokai-foreground-lc  "#171A0B")
       ;; High contrast colors
       (monokai-yellow-hc      "#FFFACE")
       (monokai-yellow-lc      "#9A8F21")
       (monokai-orange-hc      "#FFBE74")
       (monokai-orange-lc      "#A75B00")
       (monokai-red-hc         "#FEB0CC")
       (monokai-red-lc         "#F20055")
       (monokai-magenta-hc     "#FEC6F9")
       (monokai-magenta-lc     "#F309DF")
       (monokai-violet-hc      "#F0E7FF")
       (monokai-violet-lc      "#7830FC")
       (monokai-blue-hc        "#CAF5FD")
       (monokai-blue-lc        "#1DB4D0")
       (monokai-cyan-hc        "#D3FBF6")
       (monokai-cyan-lc        "#4BBEAE")
       (monokai-green-hc       "#CCF47C")
       (monokai-green-lc       "#679A01")

       ;; Distinct fringe
       (monokai-fringe-bg (if monokai-distinct-fringe-background
                              monokai-gray
                            monokai-background))

       ;; Definitions for terminals that do not support 256 colors
       (monokai-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (monokai-256-yellow         "#CDC673")
       (monokai-256-orange         "#FF8C00")
       (monokai-256-red            "#FF1493")
       (monokai-256-magenta        "#D700D7")
       (monokai-256-violet         "#AF87FF")
       (monokai-256-blue           "#5FD7FF")
       (monokai-256-cyan           "#5FFFFF")
       (monokai-256-green          "#87D700")
       (monokai-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (monokai-256-yellow-d       "#878700")
       (monokai-256-yellow-l       "#FFFF87")
       (monokai-256-orange-d       "#AF5F00")
       (monokai-256-orange-l       "#FFAF5F")
       (monokai-256-red-d          "#870000")
       (monokai-256-red-l          "#FF5F87")
       (monokai-256-magenta-d      "#AF0087")
       (monokai-256-magenta-l      "#FF87DF")
       (monokai-256-violet-d       "#5F00AF")
       (monokai-256-violet-l       "#AF87D7")
       (monokai-256-blue-d         "#008787")
       (monokai-256-blue-l         "#87D7FF")
       (monokai-256-cyan-d         "#5FAFAF")
       (monokai-256-cyan-l         "#AFFFFF")
       (monokai-256-green-d        "#5F8700")
       (monokai-256-green-l        "#AFD700")
       (monokai-256-gray-d         "#333333")
       (monokai-256-gray-l         "#707070")
       ;; Adaptive colors
       (monokai-256-foreground     "#F5F5F5")
       (monokai-256-background     "#1B1E1C")
       (monokai-256-comments       "#8B8878")
       (monokai-256-emphasis       "#FFFAFA")
       (monokai-256-line-number    "#8F908A")
       (monokai-256-highlight      "#474747")
       (monokai-256-highlight-alt  "#3E3E3E")
       (monokai-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-256-foreground-hc  "#171A0B")
       (monokai-256-foreground-lc  "#141414")
       ;; High contrast colors
       (monokai-256-yellow-hc      monokai-256-yellow-d)
       (monokai-256-yellow-lc      monokai-256-yellow-l)
       (monokai-256-orange-hc      monokai-256-orange-d)
       (monokai-256-orange-lc      monokai-256-orange-l)
       (monokai-256-red-hc         monokai-256-red-d)
       (monokai-256-red-lc         monokai-256-red-l)
       (monokai-256-magenta-hc     monokai-256-magenta-d)
       (monokai-256-magenta-lc     monokai-256-magenta-l)
       (monokai-256-violet-hc      monokai-256-violet-d)
       (monokai-256-violet-lc      monokai-256-violet-l)
       (monokai-256-blue-hc        monokai-256-blue-d)
       (monokai-256-blue-lc        monokai-256-blue-l)
       (monokai-256-cyan-hc        monokai-256-cyan-d)
       (monokai-256-cyan-lc        monokai-256-cyan-l)
       (monokai-256-green-hc       monokai-256-green-d)
       (monokai-256-green-lc       monokai-256-green-l)

       ;; Distinct fringe
       (monokai-256-fringe-bg (if monokai-distinct-fringe-background
                                  monokai-256-gray
                                monokai-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'monokai

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(font-lock-comment-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(font-lock-constant-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(font-lock-doc-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(font-lock-function-name-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(font-lock-keyword-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,monokai-class (:foreground ,monokai-violet
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(font-lock-type-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :italic nil))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(font-lock-warning-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,monokai-class (:inherit font-lock-constant-face))
      (,monokai-256-class  (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,monokai-class (:foreground ,monokai-foreground
                                    :background ,monokai-background))
       (,monokai-256-class  (:foreground ,monokai-256-foreground
                                         :background ,monokai-256-background))))

   `(highlight
     ((,monokai-class (:background ,monokai-highlight))
      (,monokai-256-class  (:background ,monokai-256-highlight))))

   `(lazy-highlight
     ((,monokai-class (:inherit highlight
                                :background ,monokai-highlight-alt))
      (,monokai-256-class  (:inherit highlight
                                     :background ,monokai-256-highlight-alt))))

   `(region
     ((,monokai-class (:inherit highlight
                                :background ,monokai-highlight))
      (,monokai-256-class  (:inherit highlight
                                     :background ,monokai-256-highlight))))

   `(secondary-selection
     ((,monokai-class (:inherit region
                                :background ,monokai-highlight-alt))
      (,monokai-256-class  (:inherit region
                                     :background ,monokai-256-highlight-alt))))

   `(shadow
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(match
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background
                                        :weight bold))))

   `(cursor
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-foreground
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-foreground
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(escape-glyph-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(fringe
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-fringe-bg))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :background ,monokai-256-fringe-bg))))

   `(link
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,monokai-class (:foreground ,monokai-violet
                                   :underline t
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,monokai-class (:foreground ,monokai-green ))
      (,monokai-256-class  (:foreground ,monokai-256-green ))))

   `(warning
     ((,monokai-class (:foreground ,monokai-yellow ))
      (,monokai-256-class  (:foreground ,monokai-256-yellow ))))

   `(error
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(eval-sexp-fu-flash
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-green))))

   `(eval-sexp-fu-flash-error
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-red))))

   `(trailing-whitespace
     ((,monokai-class (:background ,monokai-red))
      (,monokai-256-class  (:background ,monokai-256-red))))

   `(vertical-border
     ((,monokai-class (:foreground ,monokai-gray))
      (,monokai-256-class  (:foreground ,monokai-256-gray))))

   `(menu
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :background ,monokai-256-background))))

   `(minibuffer-prompt
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(mode-line
     ((,monokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-emphasis
                                      :background ,monokai-highlight
                                      :box (:line-width 1
                                                        :color ,monokai-gray
                                                        :style unspecified)))
      (,monokai-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,monokai-256-foreground
                                           :background ,monokai-256-background
                                           :box (:line-width 1
                                                             :color ,monokai-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,monokai-class (:background ,monokai-gray-d))
      (,monokai-256-class  (:background ,monokai-256-gray-d))))

   `(powerline-active2
     ((,monokai-class (:background ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-background))))


   `(mode-line-inactive
     ((,monokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-comments
                                      :background ,monokai-background
                                      :box (:line-width 1
                                                        :color ,monokai-gray
                                                        :style unspecified)))
      (,monokai-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,monokai-256-comments
                                           :background ,monokai-256-background
                                           :box (:line-width 1
                                                             :color ,monokai-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,monokai-class (:background ,monokai-gray-d))
      (,monokai-256-class  (:background ,monokai-256-gray-d))))

   `(powerline-inactive2
     ((,monokai-class (:background ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-background))))

   ;; header-line
   `(header-line
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight
                                   :box (:color ,monokai-gray
                                                :line-width 1
                                                :style unspecified)))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-highlight
                                        :box (:color ,monokai-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-background))))

   `(cua-rectangle
     ((,monokai-class (:inherit region))
      (,monokai-256-class  (:inherit region))))

   `(cua-rectangle-noselect
     ((,monokai-class (:inherit secondary-selection))
      (,monokai-256-class  (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   ;; dired
   `(dired-directory
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(dired-flagged
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(dired-header
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,monokai-class (:inherit shadow))
      (,monokai-256-class  (:inherit shadow))))

   `(dired-mark
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(dired-marked
     ((,monokai-class (:foreground ,monokai-violet
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,monokai-class (:foreground ,monokai-foreground
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,monokai-class (:foreground ,monokai-cyan
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,monokai-class (:foreground ,monokai-orange
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-blue))))

   `(dropdown-list-selection-face
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,monokai-class (:inherit ecb-history-bucket-node-face
                                :foreground ,monokai-yellow))
      (,monokai-256-class  (:inherit ecb-history-bucket-node-face
                                     :foreground ,monokai-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,monokai-class (:inherit ecb-directories-general-face
                                :foreground ,monokai-foreground))
      (,monokai-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,monokai-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,monokai-class (:inherit ecb-history-general-face
                                :foreground ,monokai-comments))
      (,monokai-256-class  (:inherit ecb-history-general-face
                                     :foreground ,monokai-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,monokai-class (:inherit ecb-directories-general-face
                                :foreground ,monokai-comments))
      (,monokai-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,monokai-256-comments))))

   `(ecb-bucket-node-face
     ((,monokai-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,monokai-blue))
      (,monokai-256-class  (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,monokai-256-blue))))

   `(ecb-tag-header-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,monokai-class (:inherit ecb-analyse-general-face
                                :foreground ,monokai-green))
      (,monokai-256-class  (:inherit ecb-analyse-general-face
                                     :foreground ,monokai-256-green))))

   `(ecb-directories-general-face
     ((,monokai-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,monokai-256-class  (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,monokai-class (:inherit ecb-methods-general-face
                                :foreground ,monokai-cyan))
      (,monokai-256-class  (:inherit ecb-methods-general-face
                                     :foreground ,monokai-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(ecb-tree-guide-line-face
     ((,monokai-class (:inherit ecb-default-general-face
                                :foreground ,monokai-gray
                                :height 1.0))
      (,monokai-256-class  (:inherit ecb-default-general-face
                                     :foreground ,monokai-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis))))

   `(ee-category
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(ee-link
     ((,monokai-class (:inherit link))
      (,monokai-256-class  (:inherit link))))

   `(ee-link-visited
     ((,monokai-class (:inherit link-visited))
      (,monokai-256-class  (:inherit link-visited))))

   `(ee-marked
     ((,monokai-class (:foreground ,monokai-magenta
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(ee-shadow
     ((,monokai-class (:inherit shadow))
      (,monokai-256-class  (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(grep-error-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(grep-match-face
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,monokai-class (:inherit region
                                :foreground ,monokai-background
                                :background ,monokai-yellow))
      (,monokai-256-class  (:inherit region
                                     :foreground ,monokai-256-background
                                     :background ,monokai-256-yellow))))

   `(isearch-fail
     ((,monokai-class (:inherit isearch
                                :foreground ,monokai-red
                                :background ,monokai-background
                                :bold t))
      (,monokai-256-class  (:inherit isearch
                                     :foreground ,monokai-256-red
                                     :background ,monokai-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :inverse-video nil))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-background
                                   :inverse-video nil
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background ,monokai-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,monokai-class (:inherit bold
                                :foreground ,monokai-emphasis))
      (,monokai-256-class  (:inherit bold
                                     :foreground ,monokai-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,monokai-class (:background unspecified))
      (,monokai-256-class  (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,monokai-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,monokai-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,monokai-class (:inherit italic :foreground ,monokai-emphasis))
      (,monokai-256-class  (:inherit italic :foreground ,monokai-256-emphasis))))

   `(font-latex-math-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(font-latex-sectioning-0-face
     ((,monokai-class (:inherit font-latex-sectioning-1-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit font-latex-sectioning-1-face
                                     :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,monokai-class (:inherit font-latex-sectioning-2-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit font-latex-sectioning-2-face
                                     :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,monokai-class (:inherit font-latex-sectioning-3-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit font-latex-sectioning-3-face
                                     :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,monokai-class (:inherit font-latex-sectioning-4-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit font-latex-sectioning-4-face
                                     :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,monokai-class (:inherit font-latex-sectioning-5-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit font-latex-sectioning-5-face
                                     :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow
                                :weight bold))
      (,monokai-256-class  (:inherit ,monokai-pitch :
                                     foreground ,monokai-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis))))

   `(font-latex-slide-title-face
     ((,monokai-class (:inherit (,monokai-pitch font-lock-type-face)
                                :weight bold
                                :height ,monokai-height-plus-3))
      (,monokai-256-class  (:inherit (,monokai-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,monokai-height-plus-3))))

   `(font-latex-string-face
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(font-latex-subscript-face
     ((,monokai-class (:height ,monokai-height-minus-1))
      (,monokai-256-class  (:height ,monokai-height-minus-1))))

   `(font-latex-superscript-face
     ((,monokai-class (:height ,monokai-height-minus-1))
      (,monokai-256-class  (:height ,monokai-height-minus-1))))

   `(font-latex-verbatim-face
     ((,monokai-class (:inherit fixed-pitch
                                :foreground ,monokai-foreground
                                :slant italic))
      (,monokai-256-class  (:inherit fixed-pitch
                                     :foreground ,monokai-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,monokai-class (:inherit bold
                                :foreground ,monokai-orange))
      (,monokai-256-class  (:inherit bold
                                     :foreground ,monokai-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-blue))))

   `(ac-selection-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(ac-candidate-mouse-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(ac-completion-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-blue))))

   `(ac-gtags-selection-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(ac-yasnippet-candidate-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-blue))))

   `(ahs-edit-mode-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-highlight))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-highlight))))

   `(ahs-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-violet ))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-green))))

   `(ahs-warning-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(android-mode-error-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(android-mode-verbose-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(android-mode-warning-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,monokai-class (:foreground ,monokai-violet
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc
                                        :foreground ,monokai-256-background))))

   `(bm-fringe-face
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc
                                        :foreground ,monokai-256-background))))

   `(bm-fringe-persistent-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-background))))

   `(bm-persistent-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(cfw:face-annotation
     ((,monokai-class (:inherit cfw:face-day-title
                                :foreground ,monokai-yellow))
      (,monokai-256-class  (:inherit cfw:face-day-title
                                     :foreground ,monokai-256-yellow))))

   `(cfw:face-default-content
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(cfw:face-default-day
     ((,monokai-class (:inherit cfw:face-day-title
                                :weight bold))
      (,monokai-256-class  (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,monokai-class (:inherit cfw:face-day-title
                                :foreground ,monokai-comments))
      (,monokai-256-class  (:inherit cfw:face-day-title
                                     :foreground ,monokai-256-comments))))

   `(cfw:face-grid
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(cfw:face-header
     ((,monokai-class (:foreground ,monokai-blue-hc
                                   :background ,monokai-blue-lc
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue-hc
                                        :background ,monokai-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,monokai-class (:background nil
                                   :foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:background nil
                                        :foreground ,monokai-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,monokai-class (:foreground ,monokai-magenta))
      (,monokai-256-class  (:foreground ,monokai-256-magenta))))

   `(cfw:face-select
     ((,monokai-class (:background ,monokai-magenta-lc
                                   :foreground ,monokai-magenta-hc))
      (,monokai-256-class  (:background ,monokai-256-magenta-lc
                                        :foreground ,monokai-256-magenta-hc))))

   `(cfw:face-saturday
     ((,monokai-class (:foreground ,monokai-cyan-hc
                                   :background ,monokai-cyan-lc))
      (,monokai-256-class  (:foreground ,monokai-256-cyan-hc
                                        :background ,monokai-256-cyan-lc))))

   `(cfw:face-sunday
     ((,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red-hc
                                        :background ,monokai-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow
                                :weight bold
                                :height ,monokai-height-plus-4))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-yellow
                                     :weight bold
                                     :height ,monokai-height-plus-4))))

   `(cfw:face-today
     ((,monokai-class (:weight bold
                               :background ,monokai-highlight-line
                               :foreground nil))
      (,monokai-256-class  (:weight bold
                                    :background ,monokai-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-yellow-hc
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc
                                        :foreground ,monokai-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-yellow-hc
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc
                                        :foreground ,monokai-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,monokai-class (:background ,monokai-yellow-hc
                                   :foreground ,monokai-yellow-lc
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-yellow-hc
                                        :foreground ,monokai-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background nil
                                   :box (:color ,monokai-yellow :line-width -1 :style nil)))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background nil
                                        :box (:color ,monokai-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(cider-instrumented-face
     ((,monokai-class (:foreground ,monokai-violet
                                   :background nil
                                   :box (:color ,monokai-violet :line-width -1 :style nil)))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :background nil
                                        :box (:color ,monokai-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background nil
                                   :box (:color ,monokai-blue :line-width -1 :style nil)))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background nil
                                        :box (:color ,monokai-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-orange))))

   `(cider-test-failure-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-red))))

   `(cider-test-success-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-green))))

   `(cider-traced-face
     ((,monokai-class :box (:color ,monokai-blue :line-width -1 :style nil))
      (,monokai-256-class  :box (:color ,monokai-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis))))

   `(company-tooltip-selection
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(company-tooltip-mouse
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(company-tooltip-common
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-blue
                                        :underline t))))

   `(company-preview
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis))))

   `(company-preview-common
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,monokai-class (:background ,monokai-gray))
      (,monokai-256-class  (:background ,monokai-256-gray))))

   `(company-scrollbar-fg
     ((,monokai-class (:background ,monokai-comments))
      (,monokai-256-class  (:background ,monokai-256-comments))))

   `(company-tooltip-annotation
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-green))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-green))))

   `(company-template-field
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,monokai-class (:inherit font-lock-doc-face
                                :foreground ,monokai-cyan
                                :underline nil))
      (,monokai-256-class  (:inherit font-lock-doc-face
                                     :foreground ,monokai-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :underline nil))))

   `(compilation-error
     ((,monokai-class (:inherit error
                                :underline nil))
      (,monokai-256-class  (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,monokai-class (:foreground ,monokai-red
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :underline nil))))

   `(compilation-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,monokai-class (:foreground ,monokai-comments
                                   :underline nil
                                   :bold nil))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,monokai-class (:inherit warning
                                :underline nil))
      (,monokai-256-class  (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,monokai-class (:inherit compilation-info
                                :foreground ,monokai-green
                                :weight bold))
      (,monokai-256-class  (:inherit compilation-info
                                     :foreground ,monokai-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,monokai-class (:inherit compilation-error
                                :foreground ,monokai-red
                                :weight bold))
      (,monokai-256-class  (:inherit compilation-error
                                     :foreground ,monokai-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(cscope-line-number-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(cscope-line-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(cscope-mouse-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :underline ,monokai-emphasis
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :underline ,monokai-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,monokai-class (:background ,monokai-gray
                                   :foreground ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-gray
                                        :foreground ,monokai-256-yellow))))

   `(ctbl:face-row-select
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground
                                   :underline t))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,monokai-class (:foreground ,monokai-violet
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-3
                                :foreground ,monokai-violet
                                :weight bold))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-3
                                     :foreground ,monokai-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-cyan
                                :height ,monokai-height-plus-3))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-cyan
                                     :height ,monokai-height-plus-3))))

   `(custom-comment-tag
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(custom-group-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue
                                :height ,monokai-height-plus-3))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-blue
                                     :height ,monokai-height-plus-3))))

   `(custom-group-tag-1
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-red
                                :height ,monokai-height-plus-3))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-red
                                     :height ,monokai-height-plus-3))))

   `(custom-state
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   ;; diff
   `(diff-added
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background))))

   `(diff-changed
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background))))

   `(diff-removed
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background))))

   `(diff-header
     ((,monokai-class (:background ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-background))))

   `(diff-file-header
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-green))))

   `(diff-refine-change
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-blue))))

   `(diff-refine-removed
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,monokai-class (:background ,monokai-blue-lc
                                   :foreground ,monokai-blue-hc))
      (,monokai-256-class  (:background ,monokai-256-blue-lc
                                        :foreground ,monokai-256-blue-hc))))

   `(diff-hl-delete
     ((,monokai-class (:background ,monokai-red-lc
                                   :foreground ,monokai-red-hc))
      (,monokai-256-class  (:background ,monokai-256-red-lc
                                        :foreground ,monokai-256-red-hc))))

   `(diff-hl-insert
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-green-hc))
      (,monokai-256-class  (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-green-hc))))

   `(diff-hl-unknown
     ((,monokai-class (:background ,monokai-violet-lc
                                   :foreground ,monokai-violet-hc))
      (,monokai-256-class  (:background ,monokai-256-violet-lc
                                        :foreground ,monokai-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,monokai-class (:background ,monokai-orange-lc))
      (,monokai-256-class  (:background ,monokai-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,monokai-class (:background ,monokai-green-lc))
      (,monokai-256-class  (:background ,monokai-256-green-lc))))

   `(ediff-fine-diff-C
     ((,monokai-class (:background ,monokai-yellow-lc))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,monokai-class (:background ,monokai-blue-lc))
      (,monokai-256-class  (:background ,monokai-256-blue-lc))))

   `(ediff-even-diff-A
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-lc ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-hc ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-hc ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-lc ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-foreground ))))

   `(ediff-odd-diff-C
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-background ))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-red)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-red)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-red-hc
                                        :background ,monokai-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-yellow)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-yellow-hc
                                   :background ,monokai-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-yellow)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-yellow-hc
                                        :background ,monokai-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,monokai-class (:foreground ,monokai-red
                                   :background unspecified
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background unspecified
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,monokai-class (:background ,monokai-red
                                   :foreground unspecified))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground unspecified))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(elfeed-search-feed-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(elfeed-search-tag-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(elfeed-search-title-face
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))
   `(ein:cell-output-prompt
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))
   `(ein:notification-tab-normal
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))
   `(ein:notification-tab-selected
     ((,monokai-class (:foreground ,monokai-orange :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))
      (,monokai-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))
      (,monokai-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))
      (,monokai-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,monokai-class (:inherit font-lock-keyword-face))
      (,monokai-256-class  (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-red)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-red-hc
                                        :background ,monokai-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-orange)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-orange-hc
                                   :background ,monokai-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-orange)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-orange-hc
                                        :background ,monokai-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,monokai-class (:inherit erc-default-face))
      (,monokai-256-class  (:inherit erc-default-face))))

   `(erc-bold-face
     ((,monokai-class (:weight bold))
      (,monokai-256-class  (:weight bold))))

   `(erc-current-nick-face
     ((,monokai-class (:foreground ,monokai-blue :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,monokai-class (:inherit font-lock-warning-face))
      (,monokai-256-class  (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(erc-highlight-face
     ((,monokai-class (:inherit erc-default-face
                                :background ,monokai-highlight))
      (,monokai-256-class  (:inherit erc-default-face
                                     :background ,monokai-256-highlight))))

   `(erc-direct-msg-face
     ((,monokai-class (:inherit erc-default-face))
      (,monokai-256-class  (:inherit erc-default-face))))

   `(erc-error-face
     ((,monokai-class (:inherit font-lock-warning-face))
      (,monokai-256-class  (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,monokai-class (:inherit erc-default-face))
      (,monokai-256-class  (:inherit erc-default-face))))

   `(erc-input-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(erc-keyword-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,monokai-class (:inherit erc-default-face))
      (,monokai-256-class  (:inherit erc-default-face))))

   `(erc-notice-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(erc-pal-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :background ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :background ,monokai-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,monokai-class (:inherit font-lock-comment-face))
      (,monokai-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,monokai-class (:inherit font-lock-comment-face))
      (,monokai-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(eshell-ls-missing
     ((,monokai-class (:inherit font-lock-warning-face))
      (,monokai-256-class  (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,monokai-class (:inherit font-lock-doc-face))
      (,monokai-256-class  (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,monokai-class (:foreground ,monokai-yellow
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,monokai-class (:foreground ,monokai-cyan
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-red-l
                                   :inherit italic))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-green-l
                                   :inherit italic))
      (,monokai-256-class  (:background ,monokai-256-highlight-line :foreground ,monokai-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,monokai-class (:inherit region))
      (,monokai-256-class  (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :underline t
                                   :slant italic))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :weight normal
                                   :slant italic))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :weight normal
                                   :slant italic))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-red-hc
                                        :background ,monokai-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-green-hc
                                   :background ,monokai-green-lc))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-green-hc
                                        :background ,monokai-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-yellow-hc
                                   :background ,monokai-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-yellow-hc
                                        :background ,monokai-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-red)))
      (,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-red)))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-orange)))
      (,monokai-class (:foreground ,monokai-orange
                                   :background ,monokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-orange)))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :background ,monokai-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-blue)))
      (,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-blue)))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,monokai-class (:foreground ,monokai-red-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,monokai-class (:foreground ,monokai-orange-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,monokai-class (:foreground ,monokai-blue-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-yellow)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-yellow)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-red)
                   :inherit unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,monokai-class (:foreground ,monokai-red
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-background
                                   :inherit bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-highlight-line
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-blue
                                        :background ,monokai-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(guide-key/key-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(guide-key/prefix-command-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,monokai-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,monokai-class (:inherit gnus-group-news-1-empty))
      (,monokai-256-class  (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,monokai-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,monokai-class (:inherit gnus-group-news-2-empty))
      (,monokai-256-class  (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,monokai-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,monokai-class (:inherit gnus-group-news-3-empty))
      (,monokai-256-class  (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,monokai-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,monokai-class (:inherit gnus-group-news-low-empty))
      (,monokai-256-class  (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,monokai-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,monokai-256-class  (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,monokai-class (:inherit message-header-other))
      (,monokai-256-class  (:inherit message-header-other))))

   `(gnus-header-from
     ((,monokai-class (:inherit message-header-other))
      (,monokai-256-class  (:inherit message-header-other))))

   `(gnus-header-name
     ((,monokai-class (:inherit message-header-name))
      (,monokai-256-class  (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,monokai-class (:inherit message-header-other))
      (,monokai-256-class  (:inherit message-header-other))))

   `(gnus-header-subject
     ((,monokai-class (:inherit message-header-subject))
      (,monokai-256-class  (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(gnus-summary-high-ancient
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-summary-low-read
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-summary-low-ticked
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(gnus-summary-low-unread
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-summary-normal-read
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-summary-normal-ticked
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(gnus-summary-normal-unread
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(gnus-summary-selected
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-cite-2
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-cite-3
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-cite-4
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-cite-5
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-cite-6
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-cite-7
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(gnus-cite-8
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(gnus-cite-9
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(gnus-cite-10
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(gnus-cite-11
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(gnus-group-news-1-empty
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(gnus-group-news-2-empty
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-group-news-3-empty
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(gnus-group-news-4-empty
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-group-news-5-empty
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(gnus-group-news-6-empty
     ((,monokai-class (:foreground ,monokai-blue-lc))
      (,monokai-256-class  (:foreground ,monokai-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(gnus-signature
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(gnus-x-face
     ((,monokai-class (:background ,monokai-foreground
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-foreground
                                        :foreground ,monokai-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(helm-apt-installed
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-bookmark-directory
     ((,monokai-class (:inherit helm-ff-directory))
      (,monokai-256-class  (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(helm-bookmark-gnus
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(helm-bookmark-info
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-bookmark-man
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(helm-bookmark-w3m
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(helm-bookmarks-su
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(helm-buffer-file
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(helm-buffer-directory
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(helm-buffer-process
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(helm-buffer-saved-out
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(helm-candidate-number
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :bold t))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(helm-ff-executable
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-ff-file
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :slant italic))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background))))

   `(helm-ff-symlink
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(helm-grep-file
     ((,monokai-class (:foreground ,monokai-cyan
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-grep-lineno
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(helm-grep-match
     ((,monokai-class (:inherit helm-match)))
     ((,monokai-256-class  (:inherit helm-match))))

   `(helm-grep-running
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(helm-header
     ((,monokai-class (:inherit header-line))
      (,monokai-256-class  (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(helm-lisp-show-completion
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-highlight-line
                                   :bold t))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background ,monokai-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,monokai-class (:foreground ,monokai-orange
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,monokai-class (:foreground ,monokai-cyan
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :underline t))))

   `(helm-match
     ((,monokai-class (:foreground ,monokai-green :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-green :inherit bold))))

   `(helm-match-item
     ((,monokai-class (:inherit helm-match))
      (,monokai-256-class  (:inherit helm-match))))

   `(helm-selection
     ((,monokai-class (:background ,monokai-highlight
                                   :inherit bold
                                   :underline nil))
      (,monokai-256-class  (:background ,monokai-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :underline nil))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,monokai-class (:foreground ,monokai-gray))
      (,monokai-256-class  (:foreground ,monokai-256-gray))))

   `(helm-source-header
     ((,monokai-class (:background ,monokai-violet-l
                                   :foreground ,monokai-background
                                   :underline nil))
      (,monokai-256-class  (:background ,monokai-256-violet-l
                                        :foreground ,monokai-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-time-zone-current
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(helm-time-zone-home
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(helm-visible-mark
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-magenta :bold t))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,monokai-class :foreground ,monokai-blue)
      (,monokai-256-class  :foreground ,monokai-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,monokai-class :foreground ,monokai-blue-l)
      (,monokai-256-class  :foreground ,monokai-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,monokai-class :foreground ,monokai-blue-l)
      (,monokai-256-class  :foreground ,monokai-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,monokai-class :foreground ,monokai-orange)
      (,monokai-256-class  :foreground ,monokai-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,monokai-class :foreground ,monokai-green)
      (,monokai-256-class  :foreground ,monokai-256-green)))

   `(helm-ls-git-added-modified-face
     ((,monokai-class :foreground ,monokai-green-l)
      (,monokai-256-class  :foreground ,monokai-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,monokai-class :foreground ,monokai-red)
      (,monokai-256-class  :foreground ,monokai-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,monokai-class :foreground ,monokai-red-l)
      (,monokai-256-class  :foreground ,monokai-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,monokai-class :foreground ,monokai-yellow)
      (,monokai-256-class  :foreground ,monokai-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,monokai-class (:foreground ,monokai-yellow-lc
                                   :background ,monokai-yellow-hc))
      (,monokai-256-class  (:foreground ,monokai-256-yellow-lc
                                        :background ,monokai-256-yellow-hc))))

   `(hi-pink
     ((,monokai-class (:foreground ,monokai-magenta-lc
                                   :background ,monokai-magenta-hc))
      (,monokai-256-class  (:foreground ,monokai-256-magenta-lc
                                        :background ,monokai-256-magenta-hc))))

   `(hi-green
     ((,monokai-class (:foreground ,monokai-green-lc
                                   :background ,monokai-green-hc))
      (,monokai-256-class  (:foreground ,monokai-256-green-lc
                                        :background ,monokai-256-green-hc))))

   `(hi-blue
     ((,monokai-class (:foreground ,monokai-blue-lc
                                   :background ,monokai-blue-hc))
      (,monokai-256-class  (:foreground ,monokai-256-blue-lc
                                        :background ,monokai-256-blue-hc))))

   `(hi-black-b
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,monokai-class (:foreground ,monokai-blue-lc
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,monokai-class (:foreground ,monokai-green-lc
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))))

   `(hi-black-hb
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(highlight-changes-delete
     ((,monokai-class (:foreground ,monokai-red
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,monokai-class (:background ,monokai-gray))
      (,monokai-256-class  (:background ,monokai-256-gray))))

   `(highlight-indentation-current-column-face
     ((,monokai-class (:background ,monokai-gray))
      (,monokai-256-class  (:background ,monokai-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,monokai-class (:background ,monokai-highlight))
      (,monokai-256-class  (:background ,monokai-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(hl-line-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-yellow
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(ido-incomplete-regexp
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold ))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :width condensed))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   ;; info
   `(info-header-xref
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(info-node
     ((,monokai-class (:foreground ,monokai-violet
                                   :inherit bold))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(info-reference-item
     ((,monokai-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,monokai-256-class  (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(info-title-1
     ((,monokai-class (:height ,monokai-height-plus-4))
      (,monokai-256-class  (:height ,monokai-height-plus-4))))

   `(info-title-2
     ((,monokai-class (:height ,monokai-height-plus-3))
      (,monokai-256-class  (:height ,monokai-height-plus-3))))

   `(info-title-3
     ((,monokai-class (:height ,monokai-height-plus-2))
      (,monokai-256-class  (:height ,monokai-height-plus-2))))

   `(info-title-4
     ((,monokai-class (:height ,monokai-height-plus-1))
      (,monokai-256-class  (:height ,monokai-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,monokai-class (:background ,monokai-gray :inherit bold))
      (,monokai-256-class  (:background ,monokai-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,monokai-class (:inherit bold))
      (,monokai-256-class  (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,monokai-class (:foreground ,monokai-violet
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,monokai-class (:foreground ,monokai-green
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,monokai-class (:foreground ,monokai-yellow
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(swiper-line-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(swiper-match-face-1
     ((,monokai-class (:background ,monokai-gray-d))))

   `(swiper-match-face-2
     ((,monokai-class (:background ,monokai-green))))

   `(swiper-match-face-3
     ((,monokai-class (:background ,monokai-orange))))

   `(swiper-match-face-4
     ((,monokai-class (:background ,monokai-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,monokai-class (:weight bold
                               :foreground ,monokai-red))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-red))))

   `(jabber-activity-personal-face
     ((,monokai-class (:weight bold
                               :foreground ,monokai-blue))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-blue))))

   `(jabber-chat-error
     ((,monokai-class (:weight bold
                               :foreground ,monokai-red))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-red))))

   `(jabber-chat-prompt-foreign
     ((,monokai-class (:weight bold
                               :foreground ,monokai-red))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-red))))

   `(jabber-chat-prompt-local
     ((,monokai-class (:weight bold
                               :foreground ,monokai-blue))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-blue))))

   `(jabber-chat-prompt-system
     ((,monokai-class (:weight bold
                               :foreground ,monokai-green))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-green))))

   `(jabber-chat-text-foreign
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(jabber-chat-text-local
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,monokai-class (:underline t
                                  :foreground ,monokai-green))
      (,monokai-256-class  (:underline t
                                       :foreground ,monokai-256-green))))

   `(jabber-roster-user-away
     ((,monokai-class (:slant italic
                              :foreground ,monokai-green))
      (,monokai-256-class  (:slant italic
                                   :foreground ,monokai-256-green))))

   `(jabber-roster-user-chatty
     ((,monokai-class (:weight bold
                               :foreground ,monokai-orange))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-orange))))

   `(jabber-roster-user-dnd
     ((,monokai-class (:slant italic
                              :foreground ,monokai-red))
      (,monokai-256-class  (:slant italic
                                   :foreground ,monokai-256-red))))

   `(jabber-roster-user-error
     ((,monokai-class (:weight light
                               :slant italic
                               :foreground ,monokai-red))
      (,monokai-256-class  (:weight light
                                    :slant italic
                                    :foreground ,monokai-256-red))))

   `(jabber-roster-user-offline
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(jabber-roster-user-online
     ((,monokai-class (:weight bold
                               :foreground ,monokai-blue))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-blue))))

   `(jabber-roster-user-xa
     ((,monokai-class (:slant italic
                              :foreground ,monokai-magenta))
      (,monokai-256-class  (:slant italic
                                   :foreground ,monokai-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(js2-external-variable
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(js2-function-call
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(js2-function-param
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(js2-instance-member
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(js2-jsdoc-tag
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(js2-jsdoc-type
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(js2-jsdoc-value
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(js2-magic-paren
     ((,monokai-class (:underline t))
      (,monokai-256-class  (:underline t))))

   `(js2-object-property
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(js2-private-function-call
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(js2-private-member
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(js2-warning
     ((,monokai-class (:underline ,monokai-orange))
      (,monokai-256-class  (:underline ,monokai-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,monokai-class (:inherit bold))
      (,monokai-256-class  (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,monokai-class (:foreground ,monokai-line-number
                                   :background ,monokai-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-line-number
                                        :background ,monokai-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,monokai-class (:foreground ,monokai-line-number
                                   :background ,monokai-highlight-line
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-line-number
                                        :background ,monokai-256-highlight-line
                                        :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,monokai-class (:inherit dimonokai-red-directory))
      (,monokai-256-class  (:inherit dimonokai-red-directory))))

   `(lusty-file-face
     ((,monokai-class nil)
      (,monokai-256-class  nil)))

   `(lusty-match-face
     ((,monokai-class (:inherit ido-first-match))
      (,monokai-256-class  (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background))))

   `(magit-diff-added-highlight
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-highlight-line))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-highlight-line))))

   `(magit-diff-removed
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background))))

   `(magit-diff-removed-highlight
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-highlight-line))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-highlight-line))))

   `(magit-section-title
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,monokai-class (:background ,monokai-highlight-line
                                   :weight unspecified))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(magit-log-graph
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,monokai-class (:background ,monokai-red-hc
                                   :foreground ,monokai-red-lc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-red-hc
                                        :foreground ,monokai-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,monokai-class (:background ,monokai-green-hc
                                   :foreground ,monokai-green-lc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-green-hc
                                        :foreground ,monokai-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,monokai-class (:background ,monokai-highlight-line
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,monokai-class (:background ,monokai-blue-lc
                                   :foreground ,monokai-blue-hc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-blue-lc
                                        :foreground ,monokai-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,monokai-class (:background ,monokai-red-lc
                                   :foreground ,monokai-red-hc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-red-lc
                                        :foreground ,monokai-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-green-hc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-yellow-hc
                                   :box 1))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc
                                        :foreground ,monokai-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(Man-underline
     ((,monokai-class (:foreground ,monokai-green :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(monky-diff-del
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(markdown-header-face-1
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-4))
      (,monokai-256-class  (:inherit markdown-header-face
                                     :height ,monokai-height-plus-4))))

   `(markdown-header-face-2
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-3))
      (,monokai-256-class  (:inherit markdown-header-face
                                     :height ,monokai-height-plus-3))))

   `(markdown-header-face-3
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-2))
      (,monokai-256-class  (:inherit markdown-header-face
                                     :height ,monokai-height-plus-2))))

   `(markdown-header-face-4
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-1))
      (,monokai-256-class  (:inherit markdown-header-face
                                     :height ,monokai-height-plus-1))))

   `(markdown-header-face-5
     ((,monokai-class (:inherit markdown-header-face))
      (,monokai-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,monokai-class (:inherit markdown-header-face))
      (,monokai-256-class  (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(message-header-name
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(message-header-other
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,monokai-class (:foreground ,monokai-cyan
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(message-mml
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(mew-face-header-from
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(mew-face-header-date
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-header-to
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-header-key
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-header-private
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-header-important
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(mew-face-header-marginal
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-header-xmew
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-header-xmew-bad
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-body-url
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(mew-face-body-comment
     ((,monokai-class (:foreground ,monokai-foreground
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-body-cite2
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(mew-face-body-cite3
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(mew-face-body-cite4
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(mew-face-body-cite5
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-mark-review
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(mew-face-mark-escape
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-mark-delete
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-mark-unlink
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(mew-face-mark-refile
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-mark-unread
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(mew-face-eof-message
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(mew-face-eof-part
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(mingus-pausing-face
     ((,monokai-class (:foreground ,monokai-magenta))
      (,monokai-256-class  (:foreground ,monokai-256-magenta))))

   `(mingus-playing-face
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(mingus-playlist-face
     ((,monokai-class (:foreground ,monokai-cyan ))
      (,monokai-256-class  (:foreground ,monokai-256-cyan ))))

   `(mingus-song-file-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(mingus-stopped-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,monokai-class (:background ,monokai-violet-d))
      (,monokai-256-class  (:background ,monokai-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,monokai-class (:background ,monokai-orange-d))
      (,monokai-256-class  (:background ,monokai-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,monokai-class (:background ,monokai-cyan-d))
      (,monokai-256-class  (:background ,monokai-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,monokai-class (:background ,monokai-blue-d))
      (,monokai-256-class  (:background ,monokai-256-blue-d))))

   `(mmm-output-submode-face
     ((,monokai-class (:background ,monokai-red-d))
      (,monokai-256-class  (:background ,monokai-256-red-d))))

   `(mmm-special-submode-face
     ((,monokai-class (:background ,monokai-green-d))
      (,monokai-256-class  (:background ,monokai-256-green-d))))

   `(mmm-code-submode-face
     ((,monokai-class (:background ,monokai-gray))
      (,monokai-256-class  (:background ,monokai-256-gray))))

   `(mmm-default-submode-face
     ((,monokai-class (:background ,monokai-gray-d))
      (,monokai-256-class  (:background ,monokai-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,monokai-class (:underline t))
      (,monokai-256-class  (:underline t))))

   `(moccur-edit-done-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-background))))

   `(moccur-edit-file-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(moccur-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,monokai-class (:foreground ,monokai-green
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,monokai-class (:foreground ,monokai-green
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :slant italic
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,monokai-class (:foreground ,monokai-magenta
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,monokai-class (:foreground ,monokai-red
                                   :slant normal
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,monokai-class (:inherit unspecified
                                :foreground unspecified
                                :background ,monokai-highlight-line
                                :underline ,monokai-emphasis
                                :weight normal))
      (,monokai-256-class  (:inherit unspecified
                                     :foreground unspecified
                                     :background ,monokai-256-highlight-line
                                     :underline ,monokai-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,monokai-class (:inherit font-lock-string-face))
      (,monokai-256-class  (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,monokai-class (:inherit font-lock-comment-face))
      (,monokai-256-class  (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,monokai-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,monokai-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,monokai-class (:inherit default))
      (,monokai-256-class  (:inherit default))))

   `(mu4e-header-marks-face
     ((,monokai-class (:inherit font-lock-preprocessor-face))
      (,monokai-256-class  (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,monokai-class (:inherit font-lock-type-face))
      (,monokai-256-class  (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,monokai-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,monokai-256-class  (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,monokai-class (:inherit font-lock-comment-face
                                :slant italic))
      (,monokai-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,monokai-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,monokai-256-class  (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,monokai-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,monokai-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,monokai-class (:inherit font-lock-comment-face
                                :slant italic))
      (,monokai-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,monokai-class (:inherit font-lock-type-face
                                :weight bold))
      (,monokai-256-class  (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,monokai-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,monokai-256-class  (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,monokai-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,monokai-256-class  (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,monokai-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,monokai-256-class  (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,monokai-class (:inherit message-header-name
                                :weight normal))
      (,monokai-256-class  (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :weight normal
                                   :slant normal))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,monokai-class (:inherit link))
      (,monokai-256-class  (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(nav-face-button-num
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(nav-face-dir
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(nav-face-hdir
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(nav-face-file
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(nav-face-hfile
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background))))

   `(neo-root-dir-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background))))

   `(neo-dir-link-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-background))))

   `(neo-file-link-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(neo-button-face
     ((,monokai-class (:underline nil))
      (,monokai-256-class  (:underline nil))))

   `(neo-expand-btn-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(neo-vc-default-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(neo-vc-user-face
     ((,monokai-class (:foreground ,monokai-red
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(neo-vc-edited-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(neo-vc-needs-update-face
     ((,monokai-class (:underline t))
      (,monokai-256-class  (:underline t))))

   `(neo-vc-needs-merge-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-comments))))

   `(neo-vc-added-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(neo-vc-removed-face
     ((,monokai-class (:strike-through t))
      (,monokai-256-class  (:strike-through t))))

   `(neo-vc-conflict-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(neo-vc-missing-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(neo-vc-ignored-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,monokai-class (:foreground ,monokai-gray-l))
      (,monokai-256-class  (:foreground ,monokai-256-gray-l))))

   `(markup-table-face
     ((,monokai-class (:foreground ,monokai-blue-hc
                                   :background ,monokai-blue-lc))
      (,monokai-256-class  (:foreground ,monokai-256-blue-hc
                                        :background ,monokai-256-blue-lc))))

   `(markup-verbatim-face
     ((,monokai-class (:background ,monokai-orange-lc))
      (,monokai-256-class  (:background ,monokai-256-orange-lc))))

   `(markup-list-face
     ((,monokai-class (:foreground ,monokai-violet-hc
                                   :background ,monokai-violet-lc))
      (,monokai-256-class  (:foreground ,monokai-256-violet-hc
                                        :background ,monokai-256-violet-lc))))

   `(markup-replacement-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(markup-complex-replacement-face
     ((,monokai-class (:foreground ,monokai-violet-hc
                                   :background ,monokai-violet-lc))
      (,monokai-256-class  (:foreground ,monokai-256-violet-hc
                                        :background ,monokai-256-violet-lc))))

   `(markup-gen-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(markup-secondary-text-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,monokai-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,monokai-background)))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,monokai-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,monokai-256-background)))))

   `(org-agenda-calendar-event
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,monokai-class (:foreground ,monokai-foreground
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,monokai-background)))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,monokai-256-background)))) t)

   `(org-agenda-date-weekend
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,monokai-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,monokai-256-class  (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,monokai-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,monokai-blue
                                :background ,monokai-background))
      (,monokai-256-class  (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,monokai-256-blue
                                     :background ,monokai-256-background))) t)

   `(org-agenda-done
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,monokai-class (:foreground ,monokai-comments
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :weight normal))))

   `(org-block
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight-alt))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-highlight-alt))))

   `(org-block-background
     ((,monokai-class (:background ,monokai-highlight-alt))
      (,monokai-256-class  (:background ,monokai-256-highlight-alt))))

   `(org-block-begin-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :box (:line-width 1 :style released-button)))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(org-date
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline t))))

   `(org-done
     ((,monokai-class (:weight bold
                               :foreground ,monokai-green))
      (,monokai-256-class  (:weight bold
                                    :foreground ,monokai-256-green))))

   `(org-ellipsis
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(org-formula
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(org-headline-done
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(org-hide
     ((,monokai-class (:foreground ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-background))))

   `(org-level-1
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-4
                                :foreground ,monokai-orange))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-4
                                     :foreground ,monokai-256-orange))))

   `(org-level-2
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-3
                                :foreground ,monokai-green))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-3
                                     :foreground ,monokai-256-green))))

   `(org-level-3
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-2
                                :foreground ,monokai-blue))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-2
                                     :foreground ,monokai-256-blue))))

   `(org-level-4
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-1
                                :foreground ,monokai-yellow))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-1
                                     :foreground ,monokai-256-yellow))))

   `(org-level-5
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-cyan))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-cyan))))

   `(org-level-6
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-green))))

   `(org-level-7
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-red))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-red))))

   `(org-level-8
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-blue))))

   `(org-link
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(org-scheduled
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(org-scheduled-previously
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(org-scheduled-today
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,monokai-class (:foreground ,monokai-comments
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :weight bold))))

   `(org-table
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(org-tag
     ((,monokai-class (:weight bold))
      (,monokai-256-class  (:weight bold))))

   `(org-time-grid
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(org-todo
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold)))
     ((,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class  (:foreground ,monokai-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,monokai-class (:background ,monokai-blue-lc
                                   :foreground ,monokai-blue-hc))
      (,monokai-256-class  (:background ,monokai-256-blue-lc
                                        :foreground ,monokai-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,monokai-class (:background ,monokai-blue-lc))
      (,monokai-256-class  (:background ,monokai-256-blue-lc))))

   `(org-habit-ready-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-green))
      (,monokai-256-class  (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-green))))

   `(org-habit-ready-future-face
     ((,monokai-class (:background ,monokai-green-lc))
      (,monokai-256-class  (:background ,monokai-256-green-lc))))

   `(org-habit-alert-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-yellow-lc))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,monokai-class (:background ,monokai-yellow-lc))
      (,monokai-256-class  (:background ,monokai-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-red-lc))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,monokai-class (:background ,monokai-red-lc))
      (,monokai-256-class  (:background ,monokai-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(org-agenda-restriction-lock
     ((,monokai-class (:background ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-yellow))))

   `(org-clock-overlay
     ((,monokai-class (:background ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-yellow))))

   `(org-column
     ((,monokai-class (:background ,monokai-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,monokai-class (:background ,monokai-highlight-line
                                   :underline t
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,monokai-class (:foreground ,monokai-red
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(org-document-title
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :weight bold
                                   :height ,monokai-height-plus-4))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :weight bold
                                        :height ,monokai-height-plus-4))))

   `(org-drawer
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(org-footnote
     ((,monokai-class (:foreground ,monokai-magenta
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(org-mode-line-clock-overrun
     ((,monokai-class (:inherit mode-line))
      (,monokai-256-class  (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,monokai-class (:inherit org-level-1))
      (,monokai-256-class  (:inherit org-level-1))))

   `(outline-2
     ((,monokai-class (:inherit org-level-2))
      (,monokai-256-class  (:inherit org-level-2))))

   `(outline-3
     ((,monokai-class (:inherit org-level-3))
      (,monokai-256-class  (:inherit org-level-3))))

   `(outline-4
     ((,monokai-class (:inherit org-level-4))
      (,monokai-256-class  (:inherit org-level-4))))

   `(outline-5
     ((,monokai-class (:inherit org-level-5))
      (,monokai-256-class  (:inherit org-level-5))))

   `(outline-6
     ((,monokai-class (:inherit org-level-6))
      (,monokai-256-class  (:inherit org-level-6))))

   `(outline-7
     ((,monokai-class (:inherit org-level-7))
      (,monokai-256-class  (:inherit org-level-7))))

   `(outline-8
     ((,monokai-class (:inherit org-level-8))
      (,monokai-256-class  (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,monokai-256-class  (:foreground ,monokai-comments))))

   ;; perspective
   `(persp-selected-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   `(popup-isearch-match
     ((,monokai-class (:background ,monokai-green))
      (,monokai-256-class  (:background ,monokai-256-green))))

   `(popup-menu-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   `(popup-menu-mouse-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-foreground))))

   `(popup-menu-selection-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-magenta
                                        :foreground ,monokai-256-background))))

   `(popup-scroll-bar-background-face
     ((,monokai-class (:background ,monokai-comments))
      (,monokai-256-class  (:background ,monokai-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,monokai-class (:background ,monokai-emphasis))
      (,monokai-256-class  (:background ,monokai-256-emphasis))))

   `(popup-tip-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-background
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :background ,monokai-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,monokai-class (:foreground ,monokai-green-d))
      (,monokai-256-class  (:foreground ,monokai-256-green-d))))

   `(realgud-overlay-arrow2
     ((,monokai-class (:foreground ,monokai-yellow-d))
      (,monokai-256-class  (:foreground ,monokai-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,monokai-class (:foreground ,monokai-orange-d))
      (,monokai-256-class  (:foreground ,monokai-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,monokai-class (:inherit error)))
     ((,monokai-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,monokai-class (:inherit secondary-selection)))
     ((,monokai-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,monokai-class (:foreground ,monokai-red-d)))
     ((,monokai-256-class (:foreground ,monokai-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,monokai-class (:inherit secondary-selection)))
     ((,monokai-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,monokai-class (:inerhit monokai-line-number)))
     ((,monokai-256-class (:inerhit monokai-line-number))))

   `(realgud-backtrace-number
     ((,monokai-class (:foreground ,monokai-yellow-d
                                   :weight bold)))
     ((,monokai-256-class (:foreground ,monokai-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background))))

   `(erb-delim-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :background ,monokai-256-background))))

   `(erb-exec-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background))))

   `(erb-exec-delim-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :background ,monokai-256-background))))

   `(erb-out-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background))))

   `(erb-out-delim-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :background ,monokai-256-background))))

   `(erb-comment-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background))))

   `(erb-comment-delim-face
     ((,monokai-class (:foreground ,monokai-cyan
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :background ,monokai-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-background))))

   `(rst-level-2-face
     ((,monokai-class (:background ,monokai-cyan
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-cyan
                                        :foreground ,monokai-256-background))))

   `(rst-level-3-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background))))

   `(rst-level-4-face
     ((,monokai-class (:background ,monokai-violet
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-violet
                                        :foreground ,monokai-256-background))))

   `(rst-level-5-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-magenta
                                        :foreground ,monokai-256-background))))

   `(rst-level-6-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(rpm-spec-doc-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(rpm-spec-ghost-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(rpm-spec-macro-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(rpm-spec-package-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(rpm-spec-section-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(rpm-spec-tag-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(rpm-spec-var-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,monokai-class (:foreground ,monokai-violet
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,monokai-class (:inherit highlight))
      (,monokai-256-class  (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-comments))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-comments))))

   `(speedbar-directory-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-blue))))

   `(speedbar-file-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-foreground))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-foreground))))

   `(speedbar-highlight-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :background ,monokai-highlight-line))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :background ,monokai-256-highlight-line))))

   `(speedbar-selected-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow
                                :underline t))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :background ,monokai-blue
                                :foreground ,monokai-background
                                :overline ,monokai-cyan-lc))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :background ,monokai-256-blue
                                     :foreground ,monokai-256-background
                                     :overline ,monokai-256-cyan-lc))))

   `(speedbar-tag-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))
      (,monokai-256-class  (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :height ,monokai-height-plus-1
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-blue
                                        :foreground ,monokai-256-background
                                        :height ,monokai-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background
                                   :weight bold
                                   :height ,monokai-height-plus-1))
      (,monokai-256-class  (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-background
                                        :weight bold
                                        :height ,monokai-height-plus-1))))

   `(sr-highlight-path-face
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :weight bold
                                   :height ,monokai-height-plus-1))
      (,monokai-256-class  (:background ,monokai-256-green
                                        :foreground ,monokai-256-background
                                        :weight bold
                                        :height ,monokai-height-plus-1))))

   `(sr-passive-path-face
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-background
                                   :weight bold
                                   :height ,monokai-height-plus-1))
      (,monokai-256-class  (:background ,monokai-256-comments
                                        :foreground ,monokai-256-background
                                        :weight bold
                                        :height ,monokai-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,monokai-class (:inherit dimonokai-red-marked))
      (,monokai-256-class  (:inherit dimonokai-red-marked))))

   `(sr-marked-file-face
     ((,monokai-class (:inherit dimonokai-red-marked))
      (,monokai-256-class  (:inherit dimonokai-red-marked))))

   `(sr-alt-marked-dir-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-magenta
                                        :foreground ,monokai-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-magenta
                                        :foreground ,monokai-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,monokai-class (:inherit dimonokai-red-directory
                                :weight normal))
      (,monokai-256-class  (:inherit dimonokai-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,monokai-class (:inherit dimonokai-red-directory
                                :slant italic
                                :weight normal))
      (,monokai-256-class  (:inherit dimonokai-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,monokai-class (:inherit dimonokai-red-symlink
                                :slant italic
                                :weight normal))
      (,monokai-256-class  (:inherit dimonokai-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,monokai-class (:inherit dimonokai-red-warning
                                :slant italic
                                :weight normal))
      (,monokai-256-class  (:inherit dimonokai-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(sr-encrypted-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(sr-log-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(sr-packaged-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(sr-html-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(sr-xml-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:background ,monokai-256-red
                                        :foreground ,monokai-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-yellow))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-yellow))))

   `(syslog-hour-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-green))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-green))))

   `(syslog-error-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-blue
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-cyan
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-magenta))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-magenta))))

   ;; table
   `(table-cell
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-highlight-line))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :background ,monokai-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-highlight-line))
      (,monokai-256-class  (:foreground ,monokai-256-background
                                        :background ,monokai-256-highlight-line))))

   `(term-color-red
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-red-d))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :background ,monokai-256-red-d))))

   `(term-color-green
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-green-d))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :background ,monokai-256-green-d))))

   `(term-color-yellow
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-yellow-d))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background ,monokai-256-yellow-d))))

   `(term-color-blue
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-blue-d))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-blue-d))))

   `(term-color-magenta
     ((,monokai-class (:foreground ,monokai-magenta
                                   :background ,monokai-magenta-d))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :background ,monokai-256-magenta-d))))

   `(term-color-cyan
     ((,monokai-class (:foreground ,monokai-cyan
                                   :background ,monokai-cyan-d))
      (,monokai-256-class  (:foreground ,monokai-256-cyan
                                        :background ,monokai-256-cyan-d))))

   `(term-color-white
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-foreground))))

   `(term-default-fg-color
     ((,monokai-class (:inherit term-color-white))
      (,monokai-256-class  (:inherit term-color-white))))

   `(term-default-bg-color
     ((,monokai-class (:inherit term-color-black))
      (,monokai-256-class  (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,monokai-class (:background ,monokai-yellow-hc
                                   :foreground ,monokai-background
                                   :inherit ,monokai-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,monokai-class (:foreground ,monokai-magenta
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-highlight-line
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :background ,monokai-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :background ,monokai-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class  (:foreground ,monokai-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(undo-tree-visualizer-current-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
      ((,monokai-class (:background ,monokai-highlight-alt))
        (,monokai-256-class  (:background ,monokai-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,monokai-class (:inherit link))
      (,monokai-256-class  (:inherit link))))

   `(w3m-arrived-anchor
     ((,monokai-class (:inherit link-visited))
      (,monokai-256-class  (:inherit link-visited))))

   `(w3m-form
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground))))

   `(w3m-header-line-location-title
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-yellow))))

   `(w3m-header-line-location-content

     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   `(w3m-bold
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-cyan
                                   :inherit link))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-cyan))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis))))

   `(w3m-lnum-match
     ((,monokai-class (:background ,monokai-highlight-line))
      (,monokai-256-class  (:background ,monokai-256-highlight-line))))

   `(w3m-lnum
     ((,monokai-class (:underline nil
                                  :bold nil
                                  :foreground ,monokai-red))
      (,monokai-256-class  (:underline nil
                                       :bold nil
                                       :foreground ,monokai-256-red))))

   `(w3m-session-select
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(w3m-session-selected
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :bold t
                                   :underline t))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground))))

   `(w3m-tab-selected-background
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground))))

   `(w3m-tab-mouse
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-yellow))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-yellow))))

   `(w3m-tab-selected
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :bold t))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-red))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-orange))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-violet))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :foreground ,monokai-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(web-mode-comment-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(web-mode-constant-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,monokai-class (:underline unspecified
                                  :weight unspecified
                                  :background ,monokai-highlight-line))
      (,monokai-256-class  (:underline unspecified
                                       :weight unspecified
                                       :background ,monokai-256-highlight-line))))

   `(web-mode-doctype-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,monokai-class (:underline t))
      (,monokai-256-class  (:underline t))))

   `(web-mode-function-name-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(web-mode-html-attr-name-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,monokai-class (:inherit web-mode-html-attr-name-face))
      (,monokai-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,monokai-class (:inherit web-mode-block-delimiter-face))
      (,monokai-256-class  (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,monokai-class (:inherit web-mode-html-attr-name-face))
      (,monokai-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(web-mode-html-tag-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(web-mode-keyword-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(web-mode-preprocessor-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :slant normal
                                   :weight unspecified))
      (,monokai-256-class  (:foreground ,monokai-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(web-mode-type-face
     ((,monokai-class (:inherit font-lock-type-face))
      (,monokai-256-class  (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(web-mode-warning-face
     ((,monokai-class (:inherit font-lock-warning-face))
      (,monokai-256-class  (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,monokai-class (:background unspecified))
      (,monokai-256-class  (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,monokai-class (:inherit font-lock-preprocessor-face))
      (,monokai-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))
      (,monokai-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,monokai-class (:inherit font-lock-preprocessor-face))
      (,monokai-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,monokai-class (:inherit web-mode-string-face))
      (,monokai-256-class  (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,monokai-class (:box 1 :weight bold))
      (,monokai-256-class  (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,monokai-class (:inherit font-lock-constant-face))
      (,monokai-256-class  (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,monokai-class (:inherit font-lock-builtin-face))
      (,monokai-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,monokai-class (:inherit font-lock-builtin-face))
      (,monokai-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,monokai-class (:inherit font-lock-function-name-face))
      (,monokai-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,monokai-class (:inherit font-lock-builtin-face))
      (,monokai-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,monokai-class (:inherit font-lock-function-name-face))
      (,monokai-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,monokai-class (:inherit font-lock-builtin-face))
      (,monokai-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,monokai-class (:inherit font-lock-variable-name-face))
      (,monokai-256-class  (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,monokai-class (:inherit font-lock-keyword-face))
      (,monokai-256-class  (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,monokai-class (:inherit web-mode-string-face))
      (,monokai-256-class  (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,monokai-class (:inherit web-mode-string-face))
      (,monokai-256-class  (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))
      (,monokai-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(web-mode-json-key-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(web-mode-json-string-face
     ((,monokai-class (:inherit web-mode-string-face))
      (,monokai-256-class  (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(web-mode-part-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))
      (,monokai-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,monokai-class (:inherit web-mode-block-face))
      (,monokai-256-class  (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,monokai-class (:inherit web-mode-string-face))
      (,monokai-256-class  (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class  (:foreground ,monokai-256-violet))))

   `(web-mode-whitespace-face
     ((,monokai-class (:background ,monokai-red))
      (,monokai-256-class  (:background ,monokai-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-emphasis
                                   :inverse-video unspecified))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,monokai-class(:background unspecified
                                  :foreground ,monokai-comments
                                  :inverse-video unspecified))
      (,monokai-256-class (:background unspecified
                                       :foreground ,monokai-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange-lc
                                   :inverse-video t))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-magenta
                                   :inverse-video unspecified))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,monokai-class (:background ,monokai-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,monokai-256-class  (:background ,monokai-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red-lc
                                   :inverse-video t))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange
                                   :inverse-video t
                                   :weight bold))
      (,monokai-256-class  (:background unspecified
                                        :foreground ,monokai-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(wl-highlight-folder-many-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(wl-highlight-folder-path-face
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class  (:foreground ,monokai-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-message-citation-header
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-headers-face
     ((,monokai-class (:foreground ,monokai-red))
      (,monokai-256-class  (:foreground ,monokai-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-header-contents
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-signature
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(wl-highlight-summary-answemonokai-red-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :slant italic))
      (,monokai-256-class  (:foreground ,monokai-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,monokai-class (:foreground ,monokai-blue))
      (,monokai-256-class  (:foreground ,monokai-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class  (:foreground ,monokai-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,monokai-class (:foreground ,monokai-magenta))
      (,monokai-256-class  (:foreground ,monokai-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,monokai-class (:underline t
                                  :weight bold))
      (,monokai-256-class  (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,monokai-class (:inherit error))
      (,monokai-256-class  (:inherit error))))

   `(weechat-highlight-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,monokai-class (:foreground ,monokai-green
                                   :weight unspecified
                                   :inverse-video t))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,monokai-class (:inherit minibuffer-prompt))
      (,monokai-256-class  (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,monokai-class (:foreground ,monokai-green
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(which-key-note-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments))))

   `(which-key-command-description-face
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class  (:foreground ,monokai-256-foreground))))

   `(which-key-local-map-description-face
     ((,monokai-class (:foreground ,monokai-yellow-hc))
      (,monokai-256-class  (:foreground ,monokai-256-yellow-hc))))

   `(which-key-group-description-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight bold))
      (,monokai-256-class  (:foreground ,monokai-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class  (:foreground ,monokai-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-comments))))

   `(yascroll:thumb-fringe
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-comments))
      (,monokai-256-class  (:foreground ,monokai-256-comments
                                        :background ,monokai-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,monokai-class (:background ,monokai-highlight-line
                                   :box ,monokai-emphasis))
      (,monokai-256-class  (:background ,monokai-256-highlight-line
                                        :box ,monokai-256-emphasis)))))

  (custom-theme-set-variables
   'monokai
   `(ansi-color-names-vector [,monokai-background ,monokai-red ,monokai-green ,monokai-yellow
                                                  ,monokai-blue ,monokai-magenta ,monokai-cyan ,monokai-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,monokai-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,monokai-magenta ,monokai-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,monokai-highlight-line . 0)
       (,monokai-green-lc . 20)
       (,monokai-cyan-lc . 30)
       (,monokai-blue-lc . 50)
       (,monokai-yellow-lc . 60)
       (,monokai-orange-lc . 70)
       (,monokai-magenta-lc . 85)
       (,monokai-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,monokai-background)
   `(pos-tip-background-color ,monokai-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,monokai-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,monokai-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,monokai-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,monokai-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,monokai-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,monokai-background ,monokai-highlight-line
                  ,monokai-red-d ,monokai-red
                  ,monokai-green-d ,monokai-green
                  ,monokai-yellow-d ,monokai-yellow
                  ,monokai-blue-d ,monokai-blue
                  ,monokai-magenta-d ,monokai-magenta
                  ,monokai-cyan-d ,monokai-cyan
                  ,monokai-foreground ,monokai-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monokai)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; monokai-theme.el ends here
