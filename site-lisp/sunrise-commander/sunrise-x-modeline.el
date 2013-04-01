;;; sunrise-x-modeline.el --- navigable mode line for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 10 Oct 2009
;; Version: 2
;; RCS Version: $Rev: 423 $
;; Keywords: sunrise commander, modeline, path mode line
;; URL: http://www.emacswiki.org/emacs/sunrise-x-modeline.el
;; Compatibility: GNU Emacs 22+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension modifies the format of the mode lines under the Sunrise
;; Commander panes so they display only the paths to the current directories (or
;; the tail if the whole path is too long) and a row of three small icons. These
;; icons are by default plain ASCII characters, but nicer semigraphical versions
;; (in Unicode) can also be used by customizing the variable
;; `sr-modeline-use-utf8-marks'.
;;
;; Here is the complete list of indicator icons (in ASCII and Unicode) and their
;; respective meanings:
;;                      (ascii) (unicode)
;; 1. Pane modes:          *        ☼     Normal mode.
;;                         !        ⚡     Editable Pane mode.
;;                         @        ☯     Virtual Directory mode.
;;                         T        ⚘     Tree View mode (with tree extension).
;;
;; 2. Navigation modes:    &        ⚓     Synchronized Navigation.
;;                         $        ♻     Sticky Search.
;;
;; 3. Transient states:    #        ♥     Contents snapshot available.
;;
;; (if you can't see the icons on the right, don't use utf8 marks)

;; The regular mode line format remains available: press C-c m to toggle between
;; one format and the other.

;; The extension is provided as a minor mode, so you can enable / disable it
;; totally by issuing the command `sr-modeline'.

;; It was written on GNU Emacs 24 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise‐x‐modeline) expression to your .emacs file
;; somewhere after the (require 'sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)
(require 'desktop)
(require 'easymenu)
(eval-when-compile (require 'cl))

(defcustom sr-modeline-use-utf8-marks nil
  "Set to t to use fancy marks (using UTF-8 glyphs) in the mode line."
  :group 'sunrise
  :type 'boolean)

;; slot 0 -- pane modes:
(defconst sr-modeline-norm-mark '("*" . "☼"))
(defconst sr-modeline-edit-mark '("!" . "⚡"))
(defconst sr-modeline-virt-mark '("@" . "☯"))
(defconst sr-modeline-tree-mark '("T" . "⚘"))

;; slot 1 -- navigation modes:
(defconst sr-modeline-sync-mark '("&" . "⚓"))
(defconst sr-modeline-srch-mark '("$" . "♻"))

;; slot 2 -- transient states:
(defconst sr-modeline-bkup-mark '("#" . "♥"))

(defface sr-modeline-separator-face
  '((t (:height 0.3)))
  "Face of the string used to separate the state indicators from one another."
  :group 'sunrise)

(defconst sr-modeline-sep #(" " 0 1 (face sr-modeline-separator-face))
  "Sunrise Modeline separator character.")

;;; ============================================================================
;;; Core functions:

(defvar sr-modeline-mark-map (make-sparse-keymap))
(define-key sr-modeline-mark-map [mode-line mouse-1] 'sr-modeline-popup-menu)
(define-key sr-modeline-mark-map [mode-line mouse-2] 'sr-modeline-popup-menu)

(defvar sr-modeline-path-map (make-sparse-keymap))
(define-key sr-modeline-path-map [mode-line mouse-1] 'sr-modeline-navigate-path)
(define-key sr-modeline-path-map [mode-line mouse-2] 'sr-modeline-navigate-path)

(defun sr-modeline-select-mark (mark &optional slot)
  "Select the right character for the given MARK in SLOT.
Depends on whether UTF-8 has been enabled in the mode line via
the variable `sr-modeline-use-utf8-marks'."
  (let ((select (if sr-modeline-use-utf8-marks #'cdr #'car))
        (slot (or slot 0)))
    (case slot
      (0 (funcall select (case mark
                           (edit sr-modeline-edit-mark)
                           (virt sr-modeline-virt-mark)
                           (tree sr-modeline-tree-mark)
                           (t sr-modeline-norm-mark))))
      (1 (cond ((or (memq 'sr-sticky-post-isearch isearch-mode-end-hook)
                    (memq 'sr-tree-post-isearch isearch-mode-end-hook))
                (funcall select sr-modeline-srch-mark))
               (sr-synchronized
                (funcall select sr-modeline-sync-mark))
               (t " ")))
      (t (if (buffer-live-p sr-backup-buffer)
             (funcall select sr-modeline-bkup-mark)
           " ")))))

(defun sr-modeline-select-mode (mode)
  "Assemble the indicators section on the left of the modeline."
  (concat sr-modeline-sep (sr-modeline-select-mark mode 0)
          sr-modeline-sep (sr-modeline-select-mark mode 1)
          sr-modeline-sep (sr-modeline-select-mark mode 2)
          sr-modeline-sep))

(defun sr-modeline-setup ()
  "Determine the mode indicator (character) to display in the mode line.
On success, sets the mode line format by calling
`sr-modeline-set'."
  (let ((mode nil))
    (case major-mode
      (sr-mode
       (setq mode (sr-modeline-select-mode (if buffer-read-only 'norm 'edit))))
      (sr-tree-mode
       (setq mode (sr-modeline-select-mode 'tree)))
      (sr-virtual-mode
       (setq mode (sr-modeline-select-mode 'virt))))
    (if mode (sr-modeline-set mode))))

(defun sr-modeline-set (mark)
  "Adjust the current mode line format.
Uses the given mode indicator and the path to the current
directory of the pane. Truncates the path if it is longer than
the available width of the pane."
  (let ((path (expand-file-name default-directory))
        (path-length (length default-directory))
        (max-length (- (window-width) 12)))
    (if (< max-length path-length)
        (setq path (concat "..." (substring path (- path-length max-length)))))
    (eval
     `(setq mode-line-format
            '("%[" ,(sr-modeline-mark mark) "%] " ,(sr-modeline-path path))))))

(defun sr-modeline-mark (marks-string)
  "Propertize MARKS-STRING for use in displaying the mode line indicators."
  (let ((mode-name "") (marks (split-string marks-string "|")))
    (setq mode-name
          (concat
           (cond ((member (sr-modeline-select-mark 'edit) marks)
                  "Editable Pane Mode")
                 ((member (sr-modeline-select-mark 'virt) marks)
                  "Virtual Directory Mode")
                 ((member (sr-modeline-select-mark 'tree) marks)
                  "Tree View Mode")
                 (t "Normal Mode"))
           (if sr-synchronized " | Synchronized Navigation" "")
           (if (or (memq 'sr-sticky-post-isearch isearch-mode-end-hook)
                  (memq 'sr-tree-post-isearch isearch-mode-end-hook))
              " | Sticky Search"
            "")
           (if (buffer-live-p sr-backup-buffer) " | Snapshot Available" "")))
    (propertize marks-string
                'font 'bold
                'mouse-face 'mode-line-highlight
                'help-echo (format "Sunrise Commander: %s" mode-name)
                'local-map sr-modeline-mark-map)))

(defun sr-modeline-path (path)
  "Propertize the string PATH for use in the mode line format.
PATH is the current directory in the file system."
  (propertize path
              'local-map sr-modeline-path-map
              'mouse-face 'mode-line-highlight
              'help-echo "Click to navigate directory path"
              'sr-selected-window sr-selected-window))

(defun sr-modeline-navigate-path ()
  "Handle click events occuring on the mode line directory path.
Analyzes all click events detected on the directory path and
modifies the current directory of the corresponding panel
accordingly."
  (interactive)
  (let* ((event (caddr (cddadr last-input-event)))
         (path (car event)) (pos (cdr event)) (slash) (levels))
    (or (eq sr-selected-window (get-text-property 0 'sr-selected-window path))
        (sr-change-window))
    (setq slash (string-match "/" path pos)
          levels (- (length (split-string (substring path slash) "/")) 2))
    (if (< 0 levels)
        (sr-dired-prev-subdir levels)
      (sr-beginning-of-buffer))))

;;; ============================================================================
;;; Private interface:

(defvar sr-modeline)

(defun sr-modeline-refresh ()
  (setq sr-modeline t)
  (sr-modeline-setup))

(defun sr-modeline-engage ()
  "Activate and enforce the navigation mode line format."
  (add-hook 'sr-refresh-hook 'sr-modeline-refresh)
  (sr-modeline-setup)
  (sr-in-other (sr-modeline-setup)))

(defun sr-modeline-disengage ()
  "De-activate the navigation mode line format, restoring the default one."
  (remove-hook 'sr-refresh-hook 'sr-modeline-refresh)
  (setq mode-line-format (default-value 'mode-line-format))
  (sr-in-other (setq mode-line-format (default-value 'mode-line-format))))

(defun sr-modeline-toggle (&optional force)
  ;; FIXME explain the argument
  "Toggle display of the navigation mode line format."
  (interactive)
  (cond ((and force (< 0 force)) (sr-modeline-engage))
        ((and force (> 0 force)) (sr-modeline-disengage))
        (t
         (if (eq mode-line-format (default-value 'mode-line-format))
             (sr-modeline-engage)
           (sr-modeline-disengage)))))

;;; ============================================================================
;;; User interface:

(defvar sr-modeline-map (make-sparse-keymap))
(define-key sr-modeline-map "\C-cm" 'sr-modeline-toggle)

(define-minor-mode sr-modeline
  "Provide navigable mode line for the Sunrise Commander.
This is a minor mode that provides a single keybinding:

  C-c m ................ Toggle between navigation and default mode line formats

  To totally disable this extension do: M-x sr-modeline <RET>"

  nil (sr-modeline-select-mode 'norm) sr-modeline-map
  (unless (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
    (setq sr-modeline nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander"))
  (sr-modeline-toggle 1))

(defvar sr-modeline-menu
  (easy-menu-create-menu
   "Mode Line"
   '(["Toggle navigation mode line" sr-modeline-toggle t]
     ["Navigation mode line help" (lambda ()
                                    (interactive)
                                    (describe-function 'sr-modeline))] )))
(defun sr-modeline-popup-menu ()
  (interactive)
  (popup-menu sr-modeline-menu))

;;; ============================================================================
;;; Bootstrap:

(defun sr-modeline-menu-init ()
  "Initialize the Sunrise Mode Line extension menu."
  (unless (lookup-key sr-mode-map [menu-bar Sunrise])
    (define-key sr-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Mode Line")))
    (define-key sr-mode-map [menu-bar Sunrise mode-line]
      (cons "Mode Line" menu-map))
    (define-key menu-map [help] '("Help" . (lambda ()
                                             (interactive)
                                             (describe-function 'sr-modeline))))
    (define-key menu-map [disable] '("Toggle" . sr-modeline-toggle))))

(defun sr-modeline-start-once ()
  "Bootstrap the navigation mode line on the first execution of
the Sunrise Commander, after module installation."
  (sr-modeline t)
  (sr-modeline-menu-init)
  (remove-hook 'sr-start-hook 'sr-modeline-start-once)
  (unintern 'sr-modeline-menu-init obarray)
  (unintern 'sr-modeline-start-once obarray))
(add-hook 'sr-start-hook 'sr-modeline-start-once)

;;; ============================================================================
;;; Desktop support:

(add-to-list 'desktop-minor-mode-table '(sr-modeline nil))

(defun sr-modeline-desktop-restore-function (&rest _)
  "Call this instead of `sr-modeline' when restoring a desktop."
  (sr-modeline-refresh))

(add-to-list 'desktop-minor-mode-handlers
             '(sr-modeline . sr-modeline-desktop-restore-function))

(provide 'sunrise-x-modeline)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-modeline))

;;; sunrise-x-modeline.el ends here
