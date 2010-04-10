;; color-theme-subdued.el -- a tango-based color theme for Emacs
;; Revision 7
;;
;; Copyright (C) 2009-2010 Jason R. Blevins <jrblevin@sdf.lonestar.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'color-theme)

(defun color-theme-subdued ()
  "Subdued color theme for Emacs by Jason Blevins.
Based on the Tango color palette."
  (interactive)
  (color-theme-install
   '(color-theme-subdued
     ((foreground-color . "#d3d7cf")
      (background-color . "#000")
      (background-mode . dark)
      (cursor-color . "#73d216") ; medium chameleon
      (mouse-color . "#73d216"))

     ;;; Standard font lock faces
     (default ((t (nil))))
     (font-lock-comment-face ((t (:foreground "#61635e")))) ; dark aluminum
     (font-lock-comment-delimiter-face ((t (:foreground "#61635e")))) ; dark aluminum
     (font-lock-doc-face ((t (:foreground "#77507b")))) ; plum
     (font-lock-doc-string-face ((t (:foreground "#77507b")))) ; plum
     (font-lock-string-face ((t (:foreground "#77507b")))) ; plum
     (font-lock-keyword-face ((t (:foreground "#729fcf")))) ; light sky blue
     (font-lock-builtin-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
     (font-lock-function-name-face ((t (:foreground "#c4a000")))) ; dark butter
     (font-lock-variable-name-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "#888a85")))) ; aluminum
     (font-lock-constant-face ((t (:foreground "#4e9a06")))) ; dark chameleon
     (font-lock-type-face ((t (:foreground "#ad7fa8")))) ; light plum
     (font-lock-warning-face ((t (:bold t :foreground "#cc0000")))) ; scarlet red

     ;; Search
     (isearch ((t (:foreground "#080808" :background "#edd400"))))
     (isearch-lazy-highlight-face ((t (:foreground "#080808" :background "#2e3436"))))

     ;; Emacs Interface
     (fringe ((t (:background "#0f0f0f"))))
     (border ((t (:background "#0f0f0f"))))
     (mode-line ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     (mode-line-buffer-id ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     (mode-line-inactive ((t (:background "#1f1f1f" :foreground "#888a85"))))
     (minibuffer-prompt ((t (:foreground "#729fcf")))) ; light sky blue
     (region ((t (:background "#2e3436"))))

     ;; Parenthesis matching
     (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
     (show-paren-mismatch-face ((t (:foreground "#2e3436" :background "#ef2929"))))

     ;; Line highlighting
     (highlight ((t (:background "#1f1f1f" :foreground nil))))
     (highlight-current-line-face ((t (:background "#1f1f1f" :foreground nil))))

     ;; Calendar
     (holiday-face ((t (:foreground "#cc0000")))) ; dark scarlet red

     ;; Info
     (info-xref ((t (:foreground "#729fcf")))) ; light sky blue
     (info-xref-visited ((t (:foreground "#ad7fa8")))) ; light plum

     ;;; AUCTeX
     (font-latex-sectioning-5-face ((t (:foreground "#c4a000" :bold t)))) ; dark butter
     (font-latex-bold-face ((t (:foreground "#4e9a06" :bold t)))) ; dark chameleon
     (font-latex-italic-face ((t (:foreground "#4e9a06" :italic t)))) ; dark chameleon
     (font-latex-math-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
     (font-latex-string-face ((t (:foreground "#77507b")))) ; plum
     (font-latex-warning-face ((t (:foreground "#cc0000")))) ; dark scarlet red
     (font-latex-slide-title-face ((t (:foreground "#c4a000")))) ; dark butter

     ;;; post-mode
     (post-emoticon-face ((t (:background "#edd400" :foreground "#000000")))) ; medium butter
     (post-header-value-face ((t (:foreground "#4e9a06")))) ; dark chameleon
     (post-header-keyword-face ((t (:foreground "#4e9a06" :bold t)))) ; dark chameleon
     (post-signature-text-face ((t (:foreground "#cc0000")))) ; dark scarlet red
     (post-quoted-text-face ((t (:foreground "#855c1b" :slant normal)))) ; med-dark chocolate
     (post-double-quoted-text-face ((t (:foreground "#77507b" :slant normal)))) ; plum
     (post-multiply-quoted-text-face ((t (:foreground "#61635e" :slant normal)))) ; dark aluminum
     (post-email-address-text-face ((t (:foreground "#729fcf" :bold t)))) ; light sky blue
     (post-url-face ((t (:foreground "#729fcf" :bold t)))) ; light sky blue
     )))

(provide 'color-theme-subdued)
