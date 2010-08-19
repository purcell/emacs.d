;;; rainbow-mode.el --- Colorize color names in buffers

;; Copyright (C) 2010 Free Software Foundation, Inc

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: faces
;; Version: 0.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'regexp-opt)
(require 'faces)

(defgroup rainbow nil
  "Show color strings with a background color."
  :tag "Rainbow"
  :group 'help)

;; Hexadecimal colors
(defvar rainbow-hexadecimal-colors-font-lock-keywords
  '(("#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?"
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for hexadecimal colors.")

;; rgb() colors
(defvar rainbow-html-rgb-colors-font-lock-keywords
  '(("rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
     (0 (rainbow-colorize-rgb)))
    ("rgba(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*[0-9]\\{1,3\\}\s*%?\s*)"
     (0 (rainbow-colorize-rgb))))
  "Font-lock keywords to add for RGB colors.")

;; HTML colors name
(defvar rainbow-html-colors-font-lock-keywords nil
  "Font-lock keywords to add for HTML colors.")
(make-variable-buffer-local 'rainbow-html-colors-font-lock-keywords)

(defcustom rainbow-html-colors-alist
  '(("black" . "#000000")
    ("silver" . "#C0C0C0")
    ("gray" . "#808080")
    ("white" . "#FFFFFF")
    ("maroon" . "#800000")
    ("red" . "#FF0000")
    ("purple" . "#800080")
    ("fuchsia" . "#FF00FF")
    ("green" . "#008000")
    ("lime" . "#00FF00")
    ("olive" . "#808000")
    ("yellow" . "#FFFF00")
    ("navy" . "#000080")
    ("blue" . "#0000FF")
    ("teal" . "#008080")
    ("aqua" . "#00FFFF"))
  "Alist of HTML colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :group 'rainbow)

(defcustom rainbow-html-colors-major-mode-list
  '(html-mode css-mode php-mode nxml-mode xml-mode)
  "List of major mode where HTML colors are enabled when
`rainbow-html-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-html-colors 'auto
  "When to enable HTML colors.
If set to t, the HTML colors will be enabled.  If set to nil, the
HTML colors will not be enabled.  If set to auto, the HTML colors
will be enabled if a major mode has been detected from the
`rainbow-html-colors-major-mode-list'."
  :group 'rainbow)

;; X colors
(defvar rainbow-x-colors-font-lock-keywords
  `((,(regexp-opt (x-defined-colors) 'words)
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for X colors.")

(defcustom rainbow-x-colors-major-mode-list
  '(emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode)
  "List of major mode where X colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-x-colors 'auto
  "When to enable X colors.
If set to t, the X colors will be enabled.  If set to nil, the
X colors will not be enabled.  If set to auto, the X colors
will be enabled if a major mode has been detected from the
`rainbow-x-colors-major-mode-list'."
  :group 'rainbow)

;; LaTeX colors
(defvar rainbow-latex-rgb-colors-font-lock-keywords
  '(("{rgb}{\\([0-9.]+\\),\\([0-9.]+\\),\\([0-9.]+\\)}"
     (0 (rainbow-colorize-rgb-float)))
    ("{RGB}{\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\)}"
     (0 (rainbow-colorize-rgb)))
    ("{HTML}{\\([0-9A-Fa-f]\\{6\\}\\)}"
     (0 (rainbow-colorize-hexadecimal-without-sharp))))
  "Font-lock keywords to add for X colors.")

(defcustom rainbow-latex-colors-major-mode-list
  '(latex-mode)
  "List of major mode where X colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-latex-colors 'auto
  "When to enable LaTeX colors.
If set to t, the LaTeX colors will be enabled. If set to nil, the
X colors will not be enabled.  If set to auto, the LaTeX colors
will be enabled if a major mode has been detected from the
`rainbow-latex-colors-major-mode-list'."
  :group 'rainbow)

;; Functions
(defun rainbow-colorize-match (color)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
  (put-text-property
   (match-beginning 0) (match-end 0)
   'face `((:foreground ,(if (> 128.0 (rainbow-x-color-luminance color))
                             "white" "black"))
           (:background ,color))))

(defun rainbow-colorize-itself ()
  "Colorize a match with itself."
  (rainbow-colorize-match (match-string-no-properties 0)))

(defun rainbow-colorize-hexadecimal-without-sharp ()
  "Colorize an hexadecimal colors and prepend # to it."
  (rainbow-colorize-match (concat "#" (match-string-no-properties 1))))

(defun rainbow-colorize-by-assoc (assoc-list)
  "Colorize a match with its association from ASSOC-LIST."
  (rainbow-colorize-match (cdr (assoc (match-string-no-properties 0) assoc-list))))

(defun rainbow-rgb-relative-to-absolute (number)
  "Convert a relative NUMBER to absolute. If NUMBER is absolute, return NUMBER.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\"."
  (let ((string-length (- (length number) 1)))
    ;; Is this a number with %?
    (if (eq (elt number string-length) ?%)
        (/ (* (string-to-number (substring number 0 string-length)) 255) 100)
      (string-to-number number))))

(defun rainbow-colorize-rgb ()
  "Colorize a match with itself."
  (let ((r (rainbow-rgb-relative-to-absolute (match-string-no-properties 1)))
        (g (rainbow-rgb-relative-to-absolute (match-string-no-properties 2)))
        (b (rainbow-rgb-relative-to-absolute (match-string-no-properties 3))))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-colorize-rgb-float ()
  "Colorize a match with itself, with relative value."
  (let ((r (* (string-to-number (match-string-no-properties 1)) 255.0))
        (g (* (string-to-number (match-string-no-properties 2)) 255.0))
        (b (* (string-to-number (match-string-no-properties 3)) 255.0)))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN."
  (floor (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun rainbow-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\")."
  (let* ((values (x-color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (caddr values)))
    (rainbow-color-luminance r g b)))

(defun rainbow-turn-on ()
  "Turn on raibow-mode."
  (font-lock-add-keywords nil
                          rainbow-hexadecimal-colors-font-lock-keywords)
  ;; Activate X colors?
  (when (or (eq rainbow-x-colors t)
            (and (eq rainbow-x-colors 'auto)
                 (memq major-mode rainbow-x-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-x-colors-font-lock-keywords))
  ;; Activate LaTeX colors?
  (when (or (eq rainbow-latex-colors t)
            (and (eq rainbow-latex-colors 'auto)
                 (memq major-mode rainbow-latex-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-latex-rgb-colors-font-lock-keywords))
  ;; Activate HTML colors?
  (when (or (eq rainbow-html-colors t)
            (and (eq rainbow-html-colors 'auto)
                 (memq major-mode rainbow-html-colors-major-mode-list)))
    (setq rainbow-html-colors-font-lock-keywords
          `((,(regexp-opt (mapcar 'car rainbow-html-colors-alist) 'words)
             (0 (rainbow-colorize-by-assoc rainbow-html-colors-alist)))))
    (font-lock-add-keywords nil
                            `(,@rainbow-html-colors-font-lock-keywords
                              ,@rainbow-html-rgb-colors-font-lock-keywords))))

(defun rainbow-turn-off ()
  "Turn off rainbow-mode."
  (font-lock-remove-keywords
   nil
   `(,@rainbow-hexadecimal-colors-font-lock-keywords
     ,@rainbow-x-colors-font-lock-keywords
     ,@rainbow-latex-rgb-colors-font-lock-keywords
     ,@rainbow-html-colors-font-lock-keywords
     ,@rainbow-html-rgb-colors-font-lock-keywords)))

;;;###autoload
(define-minor-mode rainbow-mode
  "Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\"."
  :lighter " Rbow"
  (progn
    (if rainbow-mode
        (rainbow-turn-on)
      (rainbow-turn-off))
    ;; Turn on font lock
    (font-lock-mode 1)))

(provide 'rainbow-mode)

;;; rainbow-mode.el ends here
