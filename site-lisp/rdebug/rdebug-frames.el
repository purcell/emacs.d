;;; rdebug-frames.el --- Ruby debugger frames buffer

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-frames.el 735 2008-02-29 15:24:51Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; This file contains code dealing with the frames secondary buffer.

;;; Code:

(require 'rdebug-dbg)
(require 'rdebug-fns)
(require 'rdebug-regexp)
(require 'rdebug-secondary)
(require 'rdebug-source)

(defun rdebug-display-frame-buffer ()
  "Display the rdebug stack-frame buffer."
  (interactive)
  (rdebug-display-secondary-buffer "frame"))

(defvar rdebug-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-mouse-1] 'rdebug-goto-frame-mouse)
    (define-key map [mouse-2] 'rdebug-goto-frame-mouse)
    (define-key map [mouse-3] 'rdebug-goto-frame-mouse)
    (define-key map [(control m)] 'rdebug-goto-frame)
    (define-key map "0" 'rdebug-goto-frame-n)
    (define-key map "1" 'rdebug-goto-frame-n)
    (define-key map "2" 'rdebug-goto-frame-n)
    (define-key map "3" 'rdebug-goto-frame-n)
    (define-key map "4" 'rdebug-goto-frame-n)
    (define-key map "5" 'rdebug-goto-frame-n)
    (define-key map "6" 'rdebug-goto-frame-n)
    (define-key map "7" 'rdebug-goto-frame-n)
    (define-key map "8" 'rdebug-goto-frame-n)
    (define-key map "9" 'rdebug-goto-frame-n)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Stack window" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger stack]
        (cons "Stack window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger stack goto]
      '(menu-item "Goto frame" rdebug-goto-frame))
    map)
  "Keymap to navigate rdebug stack frames.")

(defun rdebug-goto-frame (pt)
  "Show the rdebug stack frame corresponding at PT in the rdebug stack buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (concat "-->" (buffer-substring (line-beginning-position)
                                             (line-end-position))))
	  (s2 (if (= (line-number-at-pos (line-end-position 2))
                     (line-number-at-pos (point-max)))
		  nil
		;;else
                (buffer-substring (line-beginning-position 2)
                                  (line-end-position 2)))))
      (when (or (string-match rdebug-stack-frame-regexp s)
		;; need to match 1st line last to get the match position right
		(and s2 (string-match rdebug-stack-frame-2nd-regexp s2)
		     (string-match rdebug-stack-frame-1st-regexp s)))
        (let ((frame (substring s (match-beginning 2) (match-end 2))))
          (gud-call (concat "frame " frame)))))))

(defun rdebug-goto-frame-mouse (event)
  "Show the rdebug stack frame under the mouse in the rdebug stack buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rdebug-goto-frame (posn-point (event-end event)))))

;; The following is split in two to facilitate debugging.
(defun rdebug-goto-frame-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq rdebug-goto-entry-acc (concat rdebug-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc rdebug-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (rdebug-goto-entry-try acc))
                (setq acc (substring acc 1))
              (gud-call (format "frame %s" acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`rdebug-goto-frame-n' must be bound to a number key")))

(defun rdebug-goto-frame-n ()
  "Go to the frame number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'rdebug-goto-frame-n))
      (setq rdebug-goto-entry-acc ""))
  (rdebug-goto-frame-n-internal (this-command-keys)))

(defun rdebug-frames-match-current-line (limit)
  (and rdebug-frames-current-frame-number
       (re-search-forward
        (concat "^ *#"
                (number-to-string rdebug-frames-current-frame-number)
                ;; At least one space (so that we don't match #1 when looking for #10).
                " +"
                ;; The entire line.
                ".*"
                "\n"
                ;; And the next, if this entry was split into two.
                "\\( *[^# ].*$\\)?") limit t)))

(defvar rdebug-frames-current-frame-face 'highlight)

;; Example of frame buffer content:
;;
;;  #0 Integer.under_cover at line test.rb:13
;;  #1 ClassA::Nested::DeepDown.under_cover(p#ClassA::Nested::DeepD...)
;;     at line test.rb:12
;;  #2 Object.sune(s#String, i#Fixnum) at line test.rb:24
;;  #3 at line test.rb:27

(defvar rdebug-frames-font-lock-keywords
  '(
    ;; Parameters and first type entry.
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)#\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>"
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face))
    ;; "::Type", which occurs in class name of function and in parameter list.
    ("::\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-type-face))
    ;; The frame number and first type name, if present.
    ("^ *#\\([0-9]+\\) *\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[.:]\\)?"
     (1 font-lock-constant-face)
     (3 font-lock-type-face nil t))     ; t means optional.
    ;; File name and line number.
    ("at line \\(.*\\):\\([0-9]+\\)$"
     (1 font-lock-warning-face)
     (2 font-lock-constant-face))
    ;; Function name.
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))
    (rdebug-frames-match-current-line
     (0 rdebug-frames-current-frame-face append)))
  "Font-lock rules for the stack frame window in `rdebug'.")

(defun rdebug-frames-mode ()
  "Major mode for displaying the stack trace in the `rdebug' Ruby debugger.
\\{rdebug-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-frames-mode)
  (setq mode-name "RDEBUG Stack Frames")
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (use-local-map rdebug-frames-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-frames-font-lock-keywords))
  (run-mode-hooks 'rdebug-frames-mode-hook))

;; Note: This function can't restore the original point alone, since
;; the point is already at the end of the buffer when this is called.
(defun rdebug-setup-frame-buffer (buf comint-buffer)
  "Find the current frame and display the corresponding source line.

Also, cleans the buffer somewhat and sets up help for the font-lock rules."
  (rdebug-debug-enter "rdebug-setup-stack-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t)
	    (current-frame-number 0))
        (rdebug-frames-mode)
        (set (make-local-variable 'gud-comint-buffer) comint-buffer)
        (goto-char (point-min))
        (when (re-search-forward "-->" nil t)
          (beginning-of-line)
          (setq overlay-arrow-position (make-marker))
          (set-marker overlay-arrow-position (point))
          (when (looking-at rdebug-stack-frame-1st-regexp)
	    (setq current-frame-number 
		  (string-to-number 
		   (match-string rdebug-stack-frame-number-group)))
            (set (make-local-variable 'rdebug-frames-current-frame-number)
		 current-frame-number)
	    (with-current-buffer comint-buffer
	      (setq rdebug-frames-current-frame-number current-frame-number))
	    (when gud-last-frame
	      (rdebug-set-frame-arrow (gud-find-file (car gud-last-frame))))
	    (rdebug-set-frame-arrow buf)))
        ;; Remove initial '   '  or '-->'.
        (save-excursion
          (goto-char (point-max))
          (beginning-of-line)
          (if (> (point) 4)
              (delete-rectangle 4 (point))))))))

(provide 'rdebug-frames)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-frames.el ends here
