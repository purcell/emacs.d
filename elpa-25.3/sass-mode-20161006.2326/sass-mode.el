;;; sass-mode.el --- Major mode for editing Sass files

;; Copyright (c) 2007, 2008 Natalie Weizenbaum

;; Author: Natalie Weizenbaum
;; URL: http://github.com/nex3/haml/tree/master
;; Package-Version: 20161006.2326
;; Version: 3.0.16
;; Created: 2007-03-15
;; By: Natalie Weizenbaum
;; Keywords: markup, language, css
;; Package-Requires: ((haml-mode "3.0.15") (cl-lib "0.5"))

;;; Commentary:

;; Because Sass's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'sass-mode)

;; sass-mode requires haml-mode, which can be found at http://github.com/nex3/haml-mode.

;;; Code:

(require 'haml-mode)
(require 'cl-lib)

;; User definable variables

(defgroup sass nil
  "Support for the Sass template language."
  :group 'languages
  :prefix "sass-")

(defcustom sass-mode-hook nil
  "Hook run when entering Sass mode."
  :type 'hook
  :group 'sass)

(defcustom sass-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'sass)

(defcustom sass-command-options nil
  "Options to pass to the `sass' command."
  :type '(repeat string)
  :group 'sass)

(defcustom sass-before-eval-hook nil
  "Hook run in the buffer used as input to the `sass' command."
  :type 'hook
  :group 'sass)

(defvar sass-non-block-openers
  '("^.*,$" ;; Continued selectors
    "^ *@\\(extend\\|debug\\|warn\\|include\\|import\\)" ;; Single-line mixins
    "^ *[$!]" ;; Variables
    "^ *[[:alnum:]\\-]+ *:" ;; Plain attribute values
    )
  "A list of regexps that match lines of Sass that couldn't have
text nested beneath them.")

;; Font lock

(defconst sass-selector-font-lock-keywords
  '(;; Attribute selectors (e.g. p[foo=bar])
    ("\\[\\([^]=]+\\)" (1 font-lock-variable-name-face)
     ("[~|$^*]?=\\([^]=]+\\)" nil nil (1 font-lock-string-face)))
    ("&"       0 font-lock-constant-face)
    ("\\.\\w+" 0 font-lock-type-face)
    ("#\\w+"   0 font-lock-keyword-face)
    ;; Pseudo-selectors, optionally with arguments (e.g. :first, :nth-child(12))
    ("\\(::?\\w+\\)" (1 font-lock-function-name-face)
     ("(\\([^)]+\\))" nil nil (1 font-lock-string-face)))))

(defconst sass-script-font-lock-keywords
  `(("\"\\([^\"\\\\]\\|\\\\.\\)*\"" 0 font-lock-string-face)
    ("!\\(\\w\\|_\\)+" 0 font-lock-variable-name-face)
    ("#[0-9a-fA-F]\\{0,6\\}" 0 font-lock-preprocessor-face)
    (,(regexp-opt
       '("true" "false" "black" "silver" "gray" "white" "maroon" "red"
         "purple" "fuchsia" "green" "lime" "olive" "yellow" "navy"
         "blue" "teal" "aqua"))
     0 font-lock-constant-face)
    (,(regexp-opt '("and" "or" "not")) 0 font-lock-keyword-face)))

(defconst sass-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?_ "w" st)
    st))

(defconst sass-script-syntax-table
  (let ((st (make-syntax-table sass-syntax-table)))
    (modify-syntax-entry ?- "." st)
    st))

(defconst sass-font-lock-keywords
  '((sass-highlight-line 1 nil nil t)))

(defconst sass-line-keywords
  '(("@\\(\\w+\\)"    0 font-lock-keyword-face sass-highlight-directive)
    ("/[/*].*"  0 font-lock-comment-face)
    ("[=+]\\w+" 0 font-lock-function-name-face sass-highlight-script-after-match)
    ("!\\w+"    0 font-lock-variable-name-face sass-highlight-script-after-match)
    (":\\w+"    0 font-lock-variable-name-face)
    ("\\w+\s*:" 0 font-lock-variable-name-face)
    ("\\(\\w+\\)\s*="  1 font-lock-variable-name-face sass-highlight-script-after-match)
    ("\\(:\\w+\\)\s*=" 1 font-lock-variable-name-face sass-highlight-script-after-match)
    (".*"      sass-highlight-selector))
  "A list of full-line Sass syntax to highlight, used by `sass-highlight-line'.

Each item is either of the form (REGEXP SUBEXP FACE), (REGEXP FN),
or (REGEXP SUBEXP FACE FN).  Each REGEXP is run successively on the
beginning of non-whitespace on the current line until one matches.
If it has SUBEXP and FACE, then SUBEXP is highlighted using FACE.
If it has FN, FN is run.")

(defun sass-highlight-line (limit)
  "Highlight a single line using some Sass single-line syntax.
This syntax is taken from `sass-line-keywords'.
LIMIT is the limit of the search."
  (save-match-data
    (when (re-search-forward "^ *\\(.+\\)$" limit t)
      (goto-char (match-beginning 1))
      (dolist (keyword sass-line-keywords)
        (cl-destructuring-bind
            (keyword subexp-or-fn &optional face fn) keyword
          (when (looking-at keyword)
            (if (integerp subexp-or-fn)
                (put-text-property (match-beginning subexp-or-fn)
                                   (match-end subexp-or-fn)
                                   'face face)
              (setq fn subexp-or-fn))
            (when fn (funcall fn))
            (end-of-line)
            (cl-return t)))))))

(defun sass-highlight-selector ()
  "Highlight a CSS selector starting at `point' and ending at `end-of-line'."
  (let ((font-lock-keywords sass-selector-font-lock-keywords)
        font-lock-multiline)
    (font-lock-fontify-region
     (point) (progn (end-of-line) (point))))
  t)

(defun sass-highlight-script (beg end)
  "Highlight a section of SassScript between BEG and END."
  (save-match-data
    (with-syntax-table sass-script-syntax-table
      (let ((font-lock-keywords sass-script-font-lock-keywords)
            font-lock-syntax-table
            font-lock-extend-region-functions)
        (font-lock-fontify-region beg end)))))

(defun sass-highlight-script-after-match ()
  "Highlight a section of SassScript after the last match."
  (end-of-line)
  (sass-highlight-script (match-end 0) (point)))

(defun sass-highlight-directive ()
  "Highlight a Sass directive."
  (goto-char (match-end 0))
  (cl-block nil
    (cl-case (intern (match-string 1))
      (for
       (unless (looking-at " +!\\w+") (cl-return))
       (put-text-property (match-beginning 0) (match-end 0)
                          'face font-lock-variable-name-face)
       (goto-char (match-end 0))
       (unless (looking-at " +from") (cl-return))
       (put-text-property (match-beginning 0) (match-end 0)
                          'face font-lock-keyword-face)
       (goto-char (match-end 0))
       (when (looking-at " +\\(.+?\\) +\\(to\\|through\\)")
         (sass-highlight-script (match-beginning 1) (match-end 1))
         (put-text-property (match-beginning 2) (match-end 2)
                            'face font-lock-keyword-face))
       (sass-highlight-script-after-match))

      (else
       (unless (looking-at " +if") (cl-return))
       (put-text-property (match-beginning 0) (match-end 0)
                          'face font-lock-keyword-face)
       (sass-highlight-script-after-match))

      ((if while debug) (sass-highlight-script-after-match)))))

;; Constants

;; Mode setup

(defvar sass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'sass-output-region)
    (define-key map "\C-c\C-l" 'sass-output-buffer)
    map))

;;;###autoload
(define-derived-mode sass-mode haml-mode "Sass"
  "Major mode for editing Sass files.

\\{sass-mode-map}"
  (set-syntax-table sass-syntax-table)
  (setq font-lock-extend-region-functions
        '(font-lock-extend-region-wholelines font-lock-extend-region-multiline))
  (set (make-local-variable 'font-lock-multiline) nil)
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-start-skip) "/[/*]\s*")
  (set (make-local-variable 'haml-indent-function) 'sass-indent-p)
  (set (make-local-variable 'haml-indent-offset) sass-indent-offset)
  (setq font-lock-defaults '(sass-font-lock-keywords t t)))

;; Indentation

(defun sass-indent-p ()
  "Return non-nil if the current line can have lines nested beneath it."
  (cl-loop for opener in sass-non-block-openers
           if (looking-at opener) return nil
           finally return t))

(defun sass--remove-leading-indent ()
  "Reindent buffer so that the first line content begins in the first column.
This assumes that the buffer is valid SASS source, such that no
subsequent line has a lesser indent."
  (let ((min-indent nil))
    (goto-char (point-min))
    (back-to-indentation)
    (setq min-indent (1- (point)))
    (indent-rigidly (point-min) (point-max) (- min-indent))))

;; Command

(defun sass-output-region (start end)
  "Displays the CSS output for the current block of Sass code.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (let ((output-buffer "*sass-output*")
        (errors-buffer "*sass-errors*")
        (region-contents (buffer-substring start end)))
    (let ((exit-code
           (with-temp-buffer
             (insert region-contents)
             (newline-and-indent)
             (run-hooks 'sass-before-eval-hook)
             (sass--remove-leading-indent)
             (shell-command-on-region (point-min)
                                      (point-max)
                                      (mapconcat #'identity
                                                 (append '("sass") sass-command-options '("--stdin")) " ")
                                      output-buffer
                                      nil
                                      errors-buffer
                                      t))))

      (if (zerop exit-code)
          (progn
            (when (fboundp 'css-mode)
              (with-current-buffer output-buffer
                (css-mode)))
            (switch-to-buffer-other-window output-buffer))
        (with-current-buffer errors-buffer
          (view-mode))))))

(defun sass-output-buffer ()
  "Displays the CSS output for entire buffer."
  (interactive)
  (sass-output-region (point-min) (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

;; Setup/Activation
(provide 'sass-mode)
;;; sass-mode.el ends here
