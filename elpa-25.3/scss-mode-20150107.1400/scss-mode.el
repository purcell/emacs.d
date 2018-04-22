;;; scss-mode.el --- Major mode for editing SCSS files
;;
;; Author: Anton Johansson <anton.johansson@gmail.com> - http://antonj.se
;; URL: https://github.com/antonj/scss-mode
;; Package-Version: 20150107.1400
;; Created: Sep 1 23:11:26 2010
;; Version: 0.5.0
;; Keywords: scss css mode
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;;
;; Command line utility sass is required, see http://sass-lang.com/
;; To install sass:
;; gem install sass
;;
;; Also make sure sass location is in emacs PATH, example:
;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
;; or customize `scss-sass-command' to point to your sass executable.
;;
;;; Code:

(require 'derived)
(require 'compile)
(require 'flymake)

(defgroup scss nil
  "Scss mode"
  :prefix "scss-"
  :group 'css)

(defcustom scss-sass-command "sass"
  "Command used to compile SCSS files, should be sass or the
  complete path to your sass runnable example:
  \"~/.gem/ruby/1.8/bin/sass\""
  :group 'scss)

(defcustom scss-compile-at-save nil
  "If not nil the SCSS buffers will be compiled after each save"
  :type 'boolean
  :group 'scss)

(defcustom scss-sass-options '()
  "Command line Options for sass executable, for example:
'(\"--cache-location\" \"'/tmp/.sass-cache'\")"
  :group 'scss)

(defcustom scss-output-directory nil
  "Output directory for compiled files, for example:
\"../css\""
  :group 'scss)

(defcustom scss-compile-error-regex '("\\(Syntax error:\s*.*\\)\n\s*on line\s*\\([0-9]+\\) of \\([^, \n]+\\)" 3 2 nil nil 1)
  "Regex for finding line number file and error message in
compilation buffers, syntax from
`compilation-error-regexp-alist' (REGEXP FILE LINE COLUMN TYPE
HYPERLINK HIGHLIGHT)"
  :group 'scss)

(defconst scss-font-lock-keywords
  ;; Variables
  '(("$[a-z_-][a-z-_0-9]*" . font-lock-constant-face)))

(defun scss-compile-maybe()
  "Runs `scss-compile' on if `scss-compile-at-save' is t"
  (if scss-compile-at-save
      (scss-compile)))

(defun scss-compile()
  "Compiles the directory belonging to the current buffer, using the --update option"
  (interactive)
  (compile (concat scss-sass-command " " (mapconcat 'identity scss-sass-options " ") " --update "
                   (when (string-match ".*/" buffer-file-name)
                     (concat "'" (match-string 0 buffer-file-name) "'"))
                   (when scss-output-directory
                     (concat ":'" scss-output-directory "'")))))

;;;###autoload
(define-derived-mode scss-mode css-mode "SCSS"
  "Major mode for editing SCSS files, http://sass-lang.com/
Special commands:
\\{scss-mode-map}"
  (font-lock-add-keywords nil scss-font-lock-keywords)
  ;; Add the single-line comment syntax ('//', ends with newline)
  ;; as comment style 'b' (see "Syntax Flags" in elisp manual)
  (modify-syntax-entry ?/ ". 124" css-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" css-mode-syntax-table)
  (modify-syntax-entry ?\n ">" css-mode-syntax-table)
  (add-to-list 'compilation-error-regexp-alist scss-compile-error-regex)
  (add-hook 'after-save-hook 'scss-compile-maybe nil t))

(define-key scss-mode-map "\C-c\C-c" 'scss-compile)

(defun flymake-scss-init ()
  "Flymake support for SCSS files"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list scss-sass-command (append scss-sass-options (list "--scss" "--check" local-file)))))

(push '(".+\\.scss$" flymake-scss-init) flymake-allowed-file-name-masks)

;;;; TODO: Not possible to use multiline regexs flymake? flymake-err-[line]-patterns
;; '("Syntax error:\s*\\(.*\\)\n\s*on line\s*\\([0-9]+\\) of \\([^ ]+\\)$" 3 2 nil 1)
(push '("on line \\([0-9]+\\) of \\([^ ]+\\)$" 2 1 nil 2) flymake-err-line-patterns)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(provide 'scss-mode)
;;; scss-mode.el ends here
