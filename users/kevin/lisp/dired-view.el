;;; dired-view.el --- dired view mode

;; Copyright (C) 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; When browsing files in dired buffer, it would be convenient to be
;; able to jump to a file by typing that filename's first
;; character. This is what this extension does.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;       (require 'dired-view)
;;
;; To enable it by default,
;;
;;       (add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
;;
;; Also, you could define keys to toggle it,
;;
;;       (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
;;       (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)

;;; Code:

(define-minor-mode dired-view-minor-mode
  "Toggle dired-view-minor-mode.

With dired-view-minor-mode enabled, you could jump to files based on
filenames' first character.
\\{dired-view-minor-mode-map}."
  nil " Dired-View"
  '(("a" . (lambda () (interactive) (dired-view-jump "a")))
    ("b" . (lambda () (interactive) (dired-view-jump "b")))
    ("c" . (lambda () (interactive) (dired-view-jump "c")))
    ("d" . (lambda () (interactive) (dired-view-jump "d")))
    ("e" . (lambda () (interactive) (dired-view-jump "e")))
    ("f" . (lambda () (interactive) (dired-view-jump "f")))
    ("g" . (lambda () (interactive) (dired-view-jump "g")))
    ("h" . (lambda () (interactive) (dired-view-jump "h")))
    ("i" . (lambda () (interactive) (dired-view-jump "i")))
    ("j" . (lambda () (interactive) (dired-view-jump "j")))
    ("k" . (lambda () (interactive) (dired-view-jump "k")))
    ("l" . (lambda () (interactive) (dired-view-jump "l")))
    ("m" . (lambda () (interactive) (dired-view-jump "m")))
    ("n" . (lambda () (interactive) (dired-view-jump "n")))
    ("o" . (lambda () (interactive) (dired-view-jump "o")))
    ("p" . (lambda () (interactive) (dired-view-jump "p")))
    ("q" . (lambda () (interactive) (dired-view-jump "q")))
    ("r" . (lambda () (interactive) (dired-view-jump "r")))
    ("s" . (lambda () (interactive) (dired-view-jump "s")))
    ("t" . (lambda () (interactive) (dired-view-jump "t")))
    ("u" . (lambda () (interactive) (dired-view-jump "u")))
    ("v" . (lambda () (interactive) (dired-view-jump "v")))
    ("w" . (lambda () (interactive) (dired-view-jump "w")))
    ("x" . (lambda () (interactive) (dired-view-jump "x")))
    ("y" . (lambda () (interactive) (dired-view-jump "y")))
    ("z" . (lambda () (interactive) (dired-view-jump "z")))
    ("A" . (lambda () (interactive) (dired-view-jump "A")))
    ("B" . (lambda () (interactive) (dired-view-jump "B")))
    ("C" . (lambda () (interactive) (dired-view-jump "C")))
    ("D" . (lambda () (interactive) (dired-view-jump "D")))
    ("E" . (lambda () (interactive) (dired-view-jump "E")))
    ("F" . (lambda () (interactive) (dired-view-jump "F")))
    ("G" . (lambda () (interactive) (dired-view-jump "G")))
    ("H" . (lambda () (interactive) (dired-view-jump "H")))
    ("I" . (lambda () (interactive) (dired-view-jump "I")))
    ("J" . (lambda () (interactive) (dired-view-jump "J")))
    ("K" . (lambda () (interactive) (dired-view-jump "K")))
    ("L" . (lambda () (interactive) (dired-view-jump "L")))
    ("M" . (lambda () (interactive) (dired-view-jump "M")))
    ("N" . (lambda () (interactive) (dired-view-jump "N")))
    ("O" . (lambda () (interactive) (dired-view-jump "O")))
    ("P" . (lambda () (interactive) (dired-view-jump "P")))
    ("Q" . (lambda () (interactive) (dired-view-jump "Q")))
    ("R" . (lambda () (interactive) (dired-view-jump "R")))
    ("S" . (lambda () (interactive) (dired-view-jump "S")))
    ("T" . (lambda () (interactive) (dired-view-jump "T")))
    ("U" . (lambda () (interactive) (dired-view-jump "U")))
    ("V" . (lambda () (interactive) (dired-view-jump "V")))
    ("W" . (lambda () (interactive) (dired-view-jump "W")))
    ("X" . (lambda () (interactive) (dired-view-jump "X")))
    ("Y" . (lambda () (interactive) (dired-view-jump "Y")))
    ("Z" . (lambda () (interactive) (dired-view-jump "Z")))
    ("1" . (lambda () (interactive) (dired-view-jump "1")))
    ("2" . (lambda () (interactive) (dired-view-jump "2")))
    ("3" . (lambda () (interactive) (dired-view-jump "3")))
    ("4" . (lambda () (interactive) (dired-view-jump "4")))
    ("5" . (lambda () (interactive) (dired-view-jump "5")))
    ("6" . (lambda () (interactive) (dired-view-jump "6")))
    ("7" . (lambda () (interactive) (dired-view-jump "7")))
    ("8" . (lambda () (interactive) (dired-view-jump "8")))
    ("9" . (lambda () (interactive) (dired-view-jump "9")))
    ("0" . (lambda () (interactive) (dired-view-jump "0"))))
  (setq dired-view-last-arg "")
  (setq dired-view-last-arg-count 0))

(defvar dired-view-last-arg ""
  "Last searched arg.")

(defvar dired-view-last-arg-count 0
  "How many times we've searched a same arg till now.")

(defun dired-view-jump (arg)
  "Jump to filename startting with ARG."
  (interactive)
  (let ((old-arg dired-view-last-arg)
        (old-count dired-view-last-arg-count))
    (unless (string-equal dired-view-last-arg arg)
      (setq dired-view-last-arg-count 0
            dired-view-last-arg arg))
    (let* ((count dired-view-last-arg-count)
           (filename
            (catch 'return
              (progn
                (mapc
                 (lambda (name)
                   (when (string-equal arg (substring name 0 1))
                     (if (zerop count)
                         (throw 'return name)
                       (setq count (1- count)))))
                 ;; (directory-files (dired-current-directory))
                 (remove
                  ""
                  (split-string
                   (shell-command-to-string
                    (concat "ls "       ; possible caveats here
                            (replace-regexp-in-string
                             "l" "" dired-listing-switches)))
                   "\n")))
                nil))))
      (cond (filename                   ; success
             (goto-char (point-min))
             (search-forward filename nil t)
             (backward-char (length (match-string 0)))
             (when (string-equal dired-view-last-arg arg)
               (setq dired-view-last-arg-count
                     (1+ dired-view-last-arg-count))))
            ((and (zerop count)         ; wrap around
                  (> dired-view-last-arg-count 0))
             (setq dired-view-last-arg ""
                   dired-view-last-arg-count 0)
             (dired-view-jump arg))
            (t                          ; not found
             (setq dired-view-last-arg old-arg
                   dired-view-last-arg-count old-count)
             (message "file not found"))))))

(defun dired-view-minor-mode-on ()
  "Turn on `dired-view-minor-mode'."
  (interactive)
  (dired-view-minor-mode 1))

(defun dired-view-minor-mode-off ()
  "Turn off `dired-view-minor-mode'."
  (interactive)
  (dired-view-minor-mode -1))

(defun dired-view-minor-mode-toggle ()
  "Toggle `dired-view-minor-mode'."
  (interactive)
  (if dired-view-minor-mode
      (dired-view-minor-mode -1)
    (dired-view-minor-mode 1)))

(defun dired-view-minor-mode-dired-toggle ()
  "Toggle `dired-view-minor-mode' in dired buffer.

This has long-term effects, i.e., it will also affect newly
created dired buffers."
  (interactive)
  (dired-view-minor-mode-toggle)
  (cond ((member 'dired-view-minor-mode-on dired-mode-hook)
         (remove-hook 'dired-mode-hook 'dired-view-minor-mode-on))
        (t
         (add-hook 'dired-mode-hook 'dired-view-minor-mode-on))))

(provide 'dired-view)

;;; dired-view.el ends here
