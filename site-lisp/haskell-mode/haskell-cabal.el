;;; haskell-cabal.el --- Support for Cabal packages

;; Copyright (C) 2007, 2008  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;;; Commentary:

;; Todo:

;; - distinguish continued lines from indented lines.
;; - indent-line-function.
;; - outline-minor-mode.

;;; Code:

;; (defun haskell-cabal-extract-fields-from-doc ()
;;   (require 'xml)
;;   (require 'cl)
;;   (let ((section (completing-read
;;                   "Section: "
;;                   '("general-fields" "library" "executable" "buildinfo"))))
;;     (goto-char (point-min))
;;     (search-forward (concat "<sect3 id=\"" section "\">")))
;;   (let* ((xml (xml-parse-region
;;                (progn (search-forward "<variablelist>") (match-beginning 0))
;;                (progn (search-forward "</variablelist>") (point))))
;;          (varlist (remove-if-not 'consp (cddar xml)))
;;          (syms (mapcar (lambda (entry) (caddr (assq 'literal (assq 'term entry))))
;;                        varlist))
;;          (fields (mapcar (lambda (sym) (substring-no-properties sym 0 -1)) syms)))
;;     fields))

(eval-when-compile (require 'cl))

(defconst haskell-cabal-general-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "general-fields")
  '("name" "version" "cabal-version" "license" "license-file" "copyright"
    "author" "maintainer" "stability" "homepage" "package-url" "synopsis"
    "description" "category" "tested-with" "build-depends" "data-files"
    "extra-source-files" "extra-tmp-files"))

(defconst haskell-cabal-library-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "library")
  '("exposed-modules"))

(defconst haskell-cabal-executable-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "executable")
  '("executable" "main-is"))

(defconst haskell-cabal-buildinfo-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "buildinfo")
  '("buildable" "other-modules" "hs-source-dirs" "extensions" "ghc-options"
    "ghc-prof-options" "hugs-options" "nhc-options" "includes"
    "install-includes" "include-dirs" "c-sources" "extra-libraries"
    "extra-lib-dirs" "cc-options" "ld-options" "frameworks"))

(defvar haskell-cabal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; The comment syntax can't be described simply in syntax-table.
    ;; We could use font-lock-syntactic-keywords, but is it worth it?
    ;; (modify-syntax-entry ?-  ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar haskell-cabal-font-lock-keywords
  ;; The comment syntax can't be described simply in syntax-table.
  ;; We could use font-lock-syntactic-keywords, but is it worth it?
  '(("^[ \t]*--.*" . font-lock-comment-face)
    ("^ *\\([^ \t:]+\\):" (1 font-lock-keyword-face))
    ("^\\(Library\\)[ \t]*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^\\(Executable\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("^\\(Flag\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^ *\\(if\\)[ \t]+.*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^ *\\(}[ \t]*\\)?\\(else\\)[ \t]*\\({\\|$\\)"
     (2 font-lock-keyword-face))))

(defvar haskell-cabal-buffers nil
  "List of Cabal buffers.")

;; (defsubst* inferior-haskell-string-prefix-p (str1 str2)
;;   "Return non-nil if STR1 is a prefix of STR2"
;;   (eq t (compare-strings str2 nil (length str1) str1 nil nil)))

(defun haskell-cabal-find-file ()
  "Return a buffer visiting the cabal file of the current directory, or nil."
  (catch 'found
    ;; ;; First look for it in haskell-cabal-buffers.
    ;; (dolist (buf haskell-cabal-buffers)
    ;;   (if (inferior-haskell-string-prefix-p
    ;;        (with-current-buffer buf default-directory) default-directory)
    ;;       (throw 'found buf)))
    ;; Then look up the directory hierarchy.
    (let ((user (nth 2 (file-attributes default-directory)))
          ;; Abbreviate, so as to stop when we cross ~/.
          (root (abbreviate-file-name default-directory))
          files)
      (while (and root (equal user (nth 2 (file-attributes root))))
        (if (setq files (directory-files root 'full "\\.cabal\\'"))
            ;; Avoid the .cabal directory.
            (dolist (file files (throw 'found nil))
              (unless (file-directory-p file)
                (throw 'found (find-file-noselect file))))
          (if (equal root
                     (setq root (file-name-directory
                                 (directory-file-name root))))
              (setq root nil))))
      nil)))

(autoload 'derived-mode-p "derived")	; Emacs 21

(defun haskell-cabal-buffers-clean (&optional buffer)
  (let ((bufs ()))
    (dolist (buf haskell-cabal-buffers)
      (if (and (buffer-live-p buf) (not (eq buf buffer))
               (with-current-buffer buf (derived-mode-p 'haskell-cabal-mode)))
          (push buf bufs)))
    (setq haskell-cabal-buffers bufs)))

(defun haskell-cabal-unregister-buffer ()
  (haskell-cabal-buffers-clean (current-buffer)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;;;###autoload
(define-derived-mode haskell-cabal-mode fundamental-mode "Haskell-Cabal"
  "Major mode for Cabal package description files."
  (set (make-local-variable 'font-lock-defaults)
       '(haskell-cabal-font-lock-keywords t t nil nil))
  (add-to-list 'haskell-cabal-buffers (current-buffer))
  (add-hook 'change-major-mode-hook 'haskell-cabal-unregister-buffer nil 'local)
  (add-hook 'kill-buffer-hook 'haskell-cabal-unregister-buffer nil 'local)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-start-skip) "\\(^[ \t]*\\)--[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ 	]*\\(\\s>\\|\n\\)")
)

(defun haskell-cabal-get-setting (name)
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote name)
                     ":[ \t]*\\(.*\\(\n[ \t]+[ \t\n].*\\)*\\)")
             nil t)
        (let ((val (match-string 1))
              (start 1))
          (when (match-end 2)             ;Multiple lines.
            ;; The documentation is not very precise about what to do about
            ;; the \n and the indentation: are they part of the value or
            ;; the encoding?  I take the point of view that \n is part of
            ;; the value (so that values can span multiple lines as well),
            ;; and that only the first char in the indentation is part of
            ;; the encoding, the rest is part of the value (otherwise, lines
            ;; in the value cannot start with spaces or tabs).
            (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
              (setq start (1+ (match-beginning 0)))
              (setq val (replace-match "" t t val))))
          val)))))

(provide 'haskell-cabal)

;; arch-tag: d455f920-5e4d-42b6-a2c7-4a7e84a05c29
;;; haskell-cabal.el ends here
