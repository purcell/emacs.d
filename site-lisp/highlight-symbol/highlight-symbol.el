;;; highlight-symbol.el --- automatic and manual symbol highlighting
;;
;; Copyright (C) 2007-2009, 2013 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.2
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-symbol/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;
;; Use `highlight-symbol-at-point' to toggle highlighting of the symbol at
;; point throughout the current buffer.  Use `highlight-symbol-mode' to keep the
;; symbol at point highlighted.
;;
;; The functions `highlight-symbol-next', `highlight-symbol-prev',
;; `highlight-symbol-next-in-defun' and `highlight-symbol-prev-in-defun' allow
;; for cycling through the locations of any symbol at point.
;; When `highlight-symbol-on-navigation-p' is set, highlighting is triggered
;; regardless of `highlight-symbol-idle-delay'.
;;
;; `highlight-symbol-query-replace' can be used to replace the symbol.
;;
;;; Change Log:
;;
;;    Added `highlight-symbol-foreground-color'.  (thanks to rubikitch)
;;
;; 2013-01-10 (1.2)
;;    `highlight-symbol-colors' may now contain faces in addition to colors.
;;    No longer depend on hi-lock (to support the latest Emacs 24).
;;    Added `highlight-symbol-list-all'.  (thanks to lewang)
;;    Added `highlight-symbol-occur'.  (thanks to Jim Turner)
;;
;; 2009-04-13 (1.1)
;;    Added `highlight-symbol-query-replace'.
;;
;; 2009-03-19 (1.0.5)
;;    Fixed `highlight-symbol-idle-delay' void variable message.
;;    Fixed color repetition bug.  (thanks to Hugo Schmitt)
;;
;; 2008-05-02 (1.0.4)
;;    Added `highlight-symbol-on-navigation-p' option.
;;
;; 2008-02-26 (1.0.3)
;;    Added `highlight-symbol-remove-all'.
;;
;; 2007-09-06 (1.0.2)
;;    Fixed highlighting with delay set to 0.  (thanks to Stefan Persson)
;;
;; 2007-09-05 (1.0.1)
;;    Fixed completely broken temporary highlighting.
;;
;; 2007-07-30 (1.0)
;;    Keep temp highlight while jumping.
;;    Replaced `highlight-symbol-faces' with `highlight-symbol-colors'.
;;    Fixed dependency and Emacs 21 bug.  (thanks to Gregor Gorjanc)
;;    Prevent calling `highlight-symbol-at-point' on nil.
;;
;; 2007-04-20 (0.9.1)
;;    Fixed bug in `highlight-symbol-jump'.  (thanks to Per Nordlöw)
;;
;; 2007-04-06 (0.9)
;;    Initial release.
;;
;;; Code:

(require 'thingatpt)
(eval-when-compile (require 'cl))

(push "^No symbol at point$" debug-ignored-errors)

(defgroup highlight-symbol nil
  "Automatic and manual symbols highlighting"
  :group 'faces
  :group 'matching)

(defface highlight-symbol-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray90")))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

(defvar highlight-symbol-timer nil)

(defun highlight-symbol-update-timer (value)
  (when highlight-symbol-timer
    (cancel-timer highlight-symbol-timer))
  (setq highlight-symbol-timer
        (and value (/= value 0)
             (run-with-idle-timer value t 'highlight-symbol-temp-highlight))))

(defvar highlight-symbol-mode nil)

(defun highlight-symbol-set (symbol value)
  (when symbol (set symbol value))
  (when highlight-symbol-mode
    (highlight-symbol-update-timer value)))

(defcustom highlight-symbol-idle-delay 1.5
  "Number of seconds of idle time before highlighting the current symbol.
If this variable is set to 0, no idle time is required.
Changing this does not take effect until `highlight-symbol-mode' has been
disabled for all buffers."
  :type 'number
  :set 'highlight-symbol-set
  :group 'highlight-symbol)

(defcustom highlight-symbol-colors
  '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
    "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")
  "Colors and/or faces used by `highlight-symbol-at-point'.
highlighting the symbols will use these colors/faces in order."
  :type '(repeat (choice color face))
  :group 'highlight-symbol)

(defcustom highlight-symbol-on-navigation-p nil
  "Whether or not to temporarily highlight the symbol when using
`highlight-symbol-jump' family of functions."
  :type 'boolean
  :group 'highlight-symbol)

(defvar highlight-symbol-color-index 0)
(make-variable-buffer-local 'highlight-symbol-color-index)

(defvar highlight-symbol nil)
(make-variable-buffer-local 'highlight-symbol)

(defvar highlight-symbol-list nil)
(make-variable-buffer-local 'highlight-symbol-list)

(defconst highlight-symbol-border-pattern
  (if (>= emacs-major-version 22) '("\\_<" . "\\_>") '("\\<" . "\\>")))

(defcustom highlight-symbol-foreground-color "black"
  "*Foreground color of highlighted symbols."
  :type '(choice color
                 (const :tag "Keep original text color" nil))
  :group 'highlight-symbol)

;;;###autoload
(define-minor-mode highlight-symbol-mode
  "Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'."
  nil " hl-s" nil
  (if highlight-symbol-mode
      ;; on
      (progn
        (highlight-symbol-update-timer highlight-symbol-idle-delay)
        (add-hook 'post-command-hook 'highlight-symbol-mode-post-command nil t))
    ;; off
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t)
    (highlight-symbol-mode-remove-temp)
    (kill-local-variable 'highlight-symbol)))

;;;###autoload
(defun highlight-symbol-at-point ()
  "Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'."
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (if (highlight-symbol-symbol-highlighted-p symbol)
        (highlight-symbol-remove-symbol symbol)
      (highlight-symbol-add-symbol symbol))))

(defun highlight-symbol-symbol-highlighted-p (symbol)
  "Test if the a symbol regexp is currently highlighted."
  (member symbol highlight-symbol-list))

(defun highlight-symbol-add-symbol (symbol)
  (unless (highlight-symbol-symbol-highlighted-p symbol)
    (when (equal symbol highlight-symbol)
      (highlight-symbol-mode-remove-temp))
    (let ((color (nth highlight-symbol-color-index
                      highlight-symbol-colors)))
      (if color ;; wrap
          (incf highlight-symbol-color-index)
        (setq highlight-symbol-color-index 1
              color (car highlight-symbol-colors)))
      (unless (facep color)
        (setq color `((background-color . ,color)
                      (foreground-color . ,highlight-symbol-foreground-color))))
      ;; highlight
      (highlight-symbol-add-symbol-with-face symbol color)
      (push symbol highlight-symbol-list))))

(defun highlight-symbol-add-symbol-with-face (symbol face)
  (font-lock-add-keywords nil `((,symbol 0 ',face prepend)) 'append)
  (font-lock-fontify-buffer))

(defun highlight-symbol-remove-symbol (symbol)
  (setq highlight-symbol-list (delete symbol highlight-symbol-list))
  (let ((keywords (assoc symbol (highlight-symbol-uncompiled-keywords))))
    (font-lock-remove-keywords nil (list keywords))
    (font-lock-fontify-buffer)))

(defun highlight-symbol-uncompiled-keywords ()
  (if (eq t (car font-lock-keywords))
      (cadr font-lock-keywords)
    font-lock-keywords))

;;;###autoload
(defun highlight-symbol-remove-all ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (mapc 'highlight-symbol-remove-symbol highlight-symbol-list))

;;;###autoload
(defun highlight-symbol-list-all ()
  "List all symbols highlighted in the buffer."
  (interactive)
  (message "%s" (mapconcat 'highlight-symbol-fontify-symbol
                           highlight-symbol-list ", ")))

(defun highlight-symbol-fontify-symbol (symbol)
  (let ((prefix-length (length (car highlight-symbol-border-pattern)))
        (suffix-length (length (cdr highlight-symbol-border-pattern))))
    (propertize (substring symbol prefix-length
                           (- (length symbol) suffix-length))
                'face (assoc symbol (highlight-symbol-uncompiled-keywords)))))

;;;###autoload
(defun highlight-symbol-next ()
  "Jump to the next location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump 1))

;;;###autoload
(defun highlight-symbol-prev ()
  "Jump to the previous location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump -1))

;;;###autoload
(defun highlight-symbol-next-in-defun ()
  "Jump to the next location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump 1)))

;;;###autoload
(defun highlight-symbol-prev-in-defun ()
  "Jump to the previous location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump -1)))

;;;###autoload
(defun highlight-symbol-query-replace (replacement)
  "Replace the symbol at point with REPLACEMENT."
  (interactive (let ((symbol (or (thing-at-point 'symbol)
                                 (error "No symbol at point"))))
                 (highlight-symbol-temp-highlight)
                 (set query-replace-to-history-variable
                      (cons (substring-no-properties symbol)
                            (eval query-replace-to-history-variable)))
                 (list
                  (read-from-minibuffer "Replacement: " nil nil nil
                                        query-replace-to-history-variable))))
  (goto-char (beginning-of-thing 'symbol))
  (query-replace-regexp (highlight-symbol-get-symbol) replacement))

;;;###autoload
(defun highlight-symbol-occur (&optional nlines)
  "Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative."
  (interactive "P")
  (if (thing-at-point 'symbol)
      (occur (highlight-symbol-get-symbol) nlines)
    (error "No symbol at point")))

(defun highlight-symbol-get-symbol ()
  "Return a regular expression identifying the symbol at point."
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol (concat (car highlight-symbol-border-pattern)
                         (regexp-quote symbol)
                         (cdr highlight-symbol-border-pattern)))))

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (highlight-symbol-symbol-highlighted-p symbol))
        (highlight-symbol-mode-remove-temp)
        (when symbol
          (setq highlight-symbol symbol)
          (highlight-symbol-add-symbol-with-face symbol 'highlight-symbol-face)
          (font-lock-fontify-buffer))))))

(defun highlight-symbol-mode-remove-temp ()
  "Remove the temporary symbol highlighting."
  (when highlight-symbol
    (highlight-symbol-remove-symbol highlight-symbol)
    (setq highlight-symbol nil)))

(defun highlight-symbol-mode-post-command ()
  "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
  (if (eq this-command 'highlight-symbol-jump)
      (when highlight-symbol-on-navigation-p
        (highlight-symbol-temp-highlight))
    (if (eql highlight-symbol-idle-delay 0)
        (highlight-symbol-temp-highlight)
      (highlight-symbol-mode-remove-temp))))

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (highlight-symbol-get-symbol)))
    (if symbol
        (let* ((case-fold-search nil)
               (bounds (bounds-of-thing-at-point 'symbol))
               (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          ;; move a little, so we don't find the same instance again
          (goto-char (- (point) offset))
          (let ((target (re-search-forward symbol nil t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (message "Continued from beginning of buffer")
              (setq target (re-search-forward symbol nil nil dir)))
            (goto-char (+ target offset)))
          (setq this-command 'highlight-symbol-jump))
      (error "No symbol at point"))))

(provide 'highlight-symbol)

;;; highlight-symbol.el ends here
