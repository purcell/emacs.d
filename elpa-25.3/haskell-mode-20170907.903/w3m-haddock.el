;;; -*- lexical-binding: t -*-
;;; w3m-haddock.el --- Make browsing haddocks with w3m-mode better.

;; Copyright (C) 2014 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'cl-lib)
(require 'haskell-mode)
(require 'haskell-font-lock)

(declare-function w3m-buffer-title "ext:w3m")
(declare-function w3m-browse-url "ext:w3m")
(defvar w3m-current-url)

(add-hook 'w3m-display-hook 'w3m-haddock-display)

;;;###autoload
(defface w3m-haddock-heading-face
  '((((class color)) :inherit highlight))
  "Face for quarantines."
  :group 'haskell)

(defcustom haskell-w3m-haddock-dirs
  '("~/.cabal/share/doc/")
  "The path to your cabal documentation dir. It should contain
directories of package-name-x.x.

You can rebind this if you're using hsenv by adding it to your
.dir-locals.el in your project root. E.g.

    ((haskell-mode . ((haskell-w3m-haddock-dirs . (\"/home/chris/Projects/foobar/.hsenv/cabal/share/doc\")))))

"
  :group 'haskell
  :type 'list)

(defvar w3m-haddock-entry-regex "^\\(\\(data\\|type\\) \\|[a-z].* :: \\)"
  "Regex to match entry headings.")

(defun haskell-w3m-open-haddock ()
  "Open a haddock page in w3m."
  (interactive)
  (let* ((entries (cl-remove-if (lambda (s) (string= s ""))
                                (apply 'append (mapcar (lambda (dir)
                                                         (split-string (shell-command-to-string (concat "ls -1 " dir))

                                                                       "\n"))
                                                       haskell-w3m-haddock-dirs))))
         (package-dir (ido-completing-read
                       "Package: "
                       entries)))
    (cond
     ((member package-dir entries)
      (unless (cl-loop for dir in haskell-w3m-haddock-dirs
                       when (w3m-haddock-find-index dir package-dir)
                       do (progn (w3m-browse-url (w3m-haddock-find-index dir package-dir)
                                                 t)
                                 (cl-return t)))
        (w3m-browse-url (concat "http://hackage.haskell.org/package/"
                                package-dir)
                        t)))
     (t
      (w3m-browse-url (concat "http://hackage.haskell.org/package/"
                              package-dir)
                      t)))))

(defun w3m-haddock-find-index (dir package)
  (let ((html-index (concat dir "/" package "/html/index.html"))
        (index (concat dir "/" package "/index.html")))
    (cond
     ((file-exists-p html-index)
      html-index)
     ((file-exists-p index)
      index))))

(defun w3m-haddock-page-p ()
  "Haddock general page?"
  (save-excursion
    (goto-char (point-max))
    (forward-line -2)
    (looking-at "[ ]*Produced by Haddock")))

(defun w3m-haddock-source-p ()
  "Haddock source page?"
  (save-excursion
    (goto-char (point-min))
    (or (looking-at "Location: https?://hackage.haskell.org/package/.*/docs/src/")
        (looking-at "Location: file://.*cabal/share/doc/.*/html/src/")
        (looking-at "Location: .*src/.*.html$"))))

(defun w3m-haddock-p ()
  "Any haddock page?"
  (or (w3m-haddock-page-p)
      (w3m-haddock-source-p)))

(defun w3m-haddock-find-tag ()
  "Find a tag by jumping to the \"All\" index and doing a
  search-forward."
  (interactive)
  (when (w3m-haddock-p)
    (let ((ident (haskell-ident-at-point)))
      (when ident
        (w3m-browse-url
         (replace-regexp-in-string "docs/.*" "docs/doc-index-All.html" w3m-current-url))
        (search-forward ident)))))

(defun w3m-haddock-display (_url)
  "To be run by w3m's display hook. This takes a normal w3m
  buffer containing hadddock documentation and reformats it to be
  more usable and look like a dedicated documentation page."
  (when (w3m-haddock-page-p)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (delete-region (point)
                       (line-end-position))
        (w3m-haddock-next-heading)
        ;; Start formatting entries
        (while (looking-at w3m-haddock-entry-regex)
          (when (w3m-haddock-valid-heading)
            (w3m-haddock-format-heading))
          (w3m-haddock-next-heading))))
    (rename-buffer (concat "*haddock: " (w3m-buffer-title (current-buffer)) "*")))
  (when (w3m-haddock-source-p)
    (font-lock-mode -1)
    (let ((n (line-number-at-pos)))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (let ((text (buffer-substring (point)
                                      (point-max)))
              (inhibit-read-only t))
          (delete-region (point)
                         (point-max))
          (insert
           (haskell-fontify-as-mode text
                                    'haskell-mode))))
      (goto-char (point-min))
      (forward-line (1- n)))))

(defun w3m-haddock-format-heading ()
  "Format a haddock entry."
  (let ((o (make-overlay (line-beginning-position)
                         (1- (save-excursion (w3m-haddock-header-end))))))
    (overlay-put o 'face 'w3m-haddock-heading-face))
  (let ((end (save-excursion
               (w3m-haddock-next-heading)
               (when (w3m-haddock-valid-heading)
                 (point)))))
    (when end
      (save-excursion
        (w3m-haddock-header-end)
        (indent-rigidly (point)
                        end
                        4)))))

(defun w3m-haddock-next-heading ()
  "Go to the next heading, or end of the buffer."
  (forward-line 1)
  (or (search-forward-regexp w3m-haddock-entry-regex nil t 1)
      (goto-char (point-max)))
  (goto-char (line-beginning-position)))

(defun w3m-haddock-valid-heading ()
  "Is this a valid heading?"
  (not (get-text-property (point) 'face)))

(defun w3m-haddock-header-end ()
  "Go to the end of the header."
  (search-forward-regexp "\n[ \n]"))

(provide 'w3m-haddock)
