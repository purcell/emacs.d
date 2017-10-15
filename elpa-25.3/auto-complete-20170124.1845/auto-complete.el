;;; auto-complete.el --- Auto Completion for GNU Emacs

;; Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; URL: https://github.com/auto-complete/auto-complete
;; Keywords: completion, convenience
;; Version: 1.5.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension provides a way to complete with popup menu like:
;;
;;     def-!-
;;     +-----------------+
;;     |defun::::::::::::|
;;     |defvar           |
;;     |defmacro         |
;;     |       ...       |
;;     +-----------------+
;;
;; You can complete by typing and selecting menu.
;;
;; Entire documents are located in doc/ directory.
;; Take a look for information.
;;
;; Enjoy!

;;; Code:



(defconst ac-version "1.5.1"
  "Version of auto-complete in string format.
Use `version-to-list' to get version component.")

(defconst ac-version-major (car (version-to-list ac-version))
  "Major version number of auto-complete")

(defconst ac-version-minor (cadr (version-to-list ac-version))
  "Minor version number of auto-complete")

(require 'cl-lib)
(require 'popup)

;;;; Global stuff

(defun ac-error (&optional var)
  "Report an error and disable `auto-complete-mode'."
  (ignore-errors
    (message "auto-complete error: %s" var)
    (auto-complete-mode -1)
    var))



;;;; Customization

(defgroup auto-complete nil
  "Auto completion."
  :group 'completion
  :prefix "ac-")

(defcustom ac-delay 0.1
  "Delay to completions will be available."
  :type 'float
  :group 'auto-complete)

(defcustom ac-auto-show-menu 0.8
  "Non-nil means completion menu will be automatically shown."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil)
                 (float :tag "Timer"))
  :group 'auto-complete)

(defcustom ac-show-menu-immediately-on-auto-complete t
  "Non-nil means menu will be showed immediately on `auto-complete'."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-expand-on-auto-complete t
  "Non-nil means expand whole common part on first time `auto-complete'."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-disable-faces '(font-lock-comment-face font-lock-string-face font-lock-doc-face)
  "Non-nil means disable automatic completion on specified faces."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-stop-flymake-on-completing t
  "Non-nil means disble flymake temporarily on completing."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-flycheck-poll-completion-end-interval 0.5
  "Polling interval to restart automatically flycheck's checking after completion is end."
  :type 'float
  :group 'auto-complete)

(defcustom ac-use-fuzzy (and (locate-library "fuzzy") t)
  "Non-nil means use fuzzy matching."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-fuzzy-cursor-color "red"
  "Cursor color in fuzzy mode."
  :type 'string
  :group 'auto-complete)

(defcustom ac-use-comphist t
  "Non-nil means use intelligent completion history."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-comphist-threshold 0.7
  "Percentage of ignoring low scored candidates."
  :type 'float
  :group 'auto-complete)

(defcustom ac-comphist-file
  (expand-file-name (concat (if (boundp 'user-emacs-directory)
                                user-emacs-directory
                              "~/.emacs.d/")
                            "/ac-comphist.dat"))
  "Completion history file name."
  :type 'string
  :group 'auto-complete)

(defcustom ac-user-dictionary nil
  "User defined dictionary"
  :type '(repeat string)
  :group 'auto-complete)

(defcustom ac-dictionary-files '("~/.dict")
  "Dictionary files."
  :type '(repeat string)
  :group 'auto-complete)
(defvaralias 'ac-user-dictionary-files 'ac-dictionary-files)

(defcustom ac-dictionary-directories
  (ignore-errors
    (when load-file-name
      (let ((installed-dir (file-name-directory load-file-name)))
        (cl-loop for name in '("ac-dict" "dict")
              for dir = (concat installed-dir name)
              if (file-directory-p dir)
              collect dir))))
  "Dictionary directories."
  :type '(repeat string)
  :group 'auto-complete)

(defcustom ac-use-quick-help t
  "Non-nil means use quick help."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-quick-help-delay 1.5
  "Delay to show quick help."
  :type 'float
  :group 'auto-complete)

(defcustom ac-menu-height 10
  "Max height of candidate menu."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-menu-height 'ac-menu-height)

(defcustom ac-quick-help-height 20
  "Max height of quick help."
  :type 'integer
  :group 'auto-complete)

(defcustom ac-quick-help-prefer-pos-tip t
  "Prefer native tooltip with pos-tip than overlay popup for displaying quick help."
  :type 'boolean
  :group 'auto-complete)
(defvaralias 'ac-quick-help-prefer-x 'ac-quick-help-prefer-pos-tip)

(defcustom ac-candidate-limit nil
  "Limit number of candidates. Non-integer means no limit."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-max 'ac-candidate-limit)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-mode lisp-interaction-mode
    slime-repl-mode
    nim-mode c-mode cc-mode c++-mode objc-mode swift-mode go-mode
    java-mode malabar-mode clojure-mode clojurescript-mode  scala-mode
    scheme-mode
    ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode
    perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode
    ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode
    coffee-mode php-mode css-mode scss-mode less-css-mode
    elixir-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode web-mode
    ts-mode
    sclang-mode
    verilog-mode
    qml-mode
    apples-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-compatible-packages-regexp
  "^ac-"
  "Regexp to indicate what packages can work with auto-complete."
  :type 'string
  :group 'auto-complete)

(defcustom ac-non-trigger-commands
  '(*table--cell-self-insert-command
    electric-buffer-list)
  "Commands that can't be used as triggers of `auto-complete'."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-commands
  '(self-insert-command)
  "Trigger commands that specify whether `auto-complete' should start or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-commands-on-completing
  '(delete-backward-char
    backward-delete-char
    backward-delete-char-untabify
    ;; autopair
    autopair-backspace
    ;; paredit
    paredit-backward-delete
    paredit-backward-delete-word)
  "Trigger commands that specify whether `auto-complete' should continue or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-key nil
  "Non-nil means `auto-complete' will start by typing this key.
If you specify this TAB, for example, `auto-complete' will start by typing TAB,
and if there is no completions, an original command will be fallbacked."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Key"))
  :group 'auto-complete
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and value
                    (fboundp 'ac-set-trigger-key))
           (ac-set-trigger-key value))))

(defcustom ac-auto-start 2
  "Non-nil means completion will be started automatically.
Positive integer means if a length of a word you entered is larger than the value,
completion will be started automatically.
If you specify `nil', never be started automatically."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil)
                 (integer :tag "Require"))
  :group 'auto-complete)

(defcustom ac-stop-words nil
  "List of string to stop completion."
  :type '(repeat string)
  :group 'auto-complete)
(defvaralias 'ac-ignores 'ac-stop-words)

(defcustom ac-use-dictionary-as-stop-words t
  "Non-nil means a buffer related dictionary will be thought of as stop words."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-ignore-case 'smart
  "Non-nil means auto-complete ignores case.
If this value is `smart', auto-complete ignores case only when
a prefix doesn't contain any upper case letters."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Smart" smart)
                 (const :tag "No" nil))
  :group 'auto-complete)

(defcustom ac-dwim t
  "Non-nil means `auto-complete' works based on Do What I Mean."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-use-menu-map nil
  "Non-nil means a special keymap `ac-menu-map' on completing menu will be used."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-use-overriding-local-map nil
  "Non-nil means `overriding-local-map' will be used to hack for overriding key events on auto-completion."
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-disable-inline nil
  "Non-nil disable inline completion visibility"
  :type 'boolean
  :group 'auto-complete)

(defcustom ac-candidate-menu-min 1
  "Number of candidates required to display menu"
  :type 'integer
  :group 'auto-complete)

(defcustom ac-max-width nil
  "Maximum width for auto-complete menu to have"
  :type '(choice (const :tag "No limit" nil)
                 (const :tag "Character Limit" 25)
                 (const :tag "Window Ratio Limit" 0.5))
  :group 'auto-complete)

(defface ac-completion-face
  '((t (:foreground "darkgray" :underline t)))
  "Face for inline completion"
  :group 'auto-complete)

(defface ac-candidate-face
  '((t (:inherit popup-face)))
  "Face for candidate."
  :group 'auto-complete)

(defface ac-candidate-mouse-face
  '((t (:inherit popup-menu-mouse-face)))
  "Mouse face for candidate."
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:inherit popup-menu-selection-face)))
  "Face for selected candidate."
  :group 'auto-complete)

(defvar auto-complete-mode-hook nil
  "Hook for `auto-complete-mode'.")



;;;; Internal variables

(defvar auto-complete-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar ac-cursor-color nil
  "Old cursor color.")

(defvar ac-inline nil
  "Inline completion instance.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-show-menu nil
  "Flag to show menu on timer tick.")

(defvar ac-last-completion nil
  "Cons of prefix marker and selected item of last completion.")

(defvar ac-quick-help nil
  "Quick help instance")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-buffer nil
  "Buffer where auto-complete is started.")

(defvar ac-point nil
  "Start point of prefix.")

(defvar ac-last-point nil
  "Last point of updating pattern.")

(defvar ac-prefix nil
  "Prefix string.")
(defvaralias 'ac-target 'ac-prefix)

(defvar ac-selected-candidate nil
  "Last selected candidate.")

(defvar ac-common-part nil
  "Common part string of meaningful candidates.
If there is no common part, this will be nil.")

(defvar ac-whole-common-part nil
  "Common part string of whole candidates.
If there is no common part, this will be nil.")

(defvar ac-prefix-overlay nil
  "Overlay for prefix string.")

(defvar ac-timer nil
  "Completion idle timer.")

(defvar ac-show-menu-timer nil
  "Show menu idle timer.")

(defvar ac-quick-help-timer nil
  "Quick help idle timer.")

(defvar ac-triggered nil
  "Flag to update.")

(defvar ac-limit nil
  "Limit number of candidates for each sources.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-candidates-cache nil
  "Candidates cache for individual sources.")

(defvar ac-fuzzy-enable nil
  "Non-nil means fuzzy matching is enabled.")

(defvar ac-dwim-enable nil
  "Non-nil means DWIM completion will be allowed.")

(defvar ac-mode-map (make-sparse-keymap)
  "Auto-complete mode map. It is also used for trigger key command. See also `ac-trigger-key'.")

(defvar ac-completing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map [tab] 'ac-expand)
    (define-key map "\r" 'ac-complete)
    (define-key map (kbd "M-TAB") 'auto-complete)

    (define-key map "\M-n" 'ac-next)
    (define-key map "\M-p" 'ac-previous)
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    (define-key map [f1] 'ac-help)
    (define-key map [M-f1] 'ac-persist-help)
    (define-key map (kbd "C-?") 'ac-help)
    (define-key map (kbd "C-M-?") 'ac-persist-help)

    (define-key map [C-down] 'ac-quick-help-scroll-down)
    (define-key map [C-up] 'ac-quick-help-scroll-up)
    (define-key map "\C-\M-n" 'ac-quick-help-scroll-down)
    (define-key map "\C-\M-p" 'ac-quick-help-scroll-up)

    (dotimes (i 9)
      (let ((symbol (intern (format "ac-complete-select-%d" (1+ i)))))
        (fset symbol
              `(lambda ()
                 (interactive)
                 (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
                   (ac-complete))))
        (define-key map (read-kbd-macro (format "M-%s" (1+ i))) symbol)))

    map)
  "Keymap for completion.")
(defvaralias 'ac-complete-mode-map 'ac-completing-map)

(defvar ac-menu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ac-completing-map)
    (define-key map (kbd "RET") 'ac-complete)
    (define-key map "\C-n" 'ac-next)
    (define-key map "\C-p" 'ac-previous)
    (define-key map "\C-s" 'ac-isearch)
    (define-key map [mouse-1] 'ac-mouse-1)
    (define-key map [down-mouse-1] 'ac-ignore)
    (define-key map [mouse-4] 'ac-mouse-4)
    (define-key map [mouse-5] 'ac-mouse-5)
    map)
  "Keymap for completion on completing menu.")

(defvar ac-current-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ac-completing-map)
    map))

(defvar ac-match-function 'all-completions
  "Default match function.")

(defvar ac-prefix-definitions
  '((symbol . ac-prefix-symbol)
    (file . ac-prefix-file)
    (valid-file . ac-prefix-valid-file)
    (c-dot . ac-prefix-c-dot)
    (c-dot-ref . ac-prefix-c-dot-ref)
    (cc-member . ac-prefix-cc-member))
  "Prefix definitions for common use.")

(defvar ac-sources '(ac-source-words-in-same-mode-buffers)
  "Sources for completion.")
(make-variable-buffer-local 'ac-sources)

(defvar ac-compiled-sources nil
  "Compiled source of `ac-sources'.")

(defvar ac-current-sources nil
  "Current working sources. This is sublist of `ac-compiled-sources'.")

(defvar ac-omni-completion-sources nil
  "Do not use this anymore.")

(defvar ac-current-prefix-def nil)

(defvar ac-ignoring-prefix-def nil)



;;;; Intelligent completion history

(defvar ac-comphist nil
  "Database of completion history.")

(defsubst ac-comphist-make-tab ()
  (make-hash-table :test 'equal))

(defsubst ac-comphist-tab (db)
  (nth 0 db))

(defsubst ac-comphist-cache (db)
  (nth 1 db))

(defun ac-comphist-make (&optional tab)
  (list (or tab (ac-comphist-make-tab)) (make-hash-table :test 'equal :weakness t)))

(defun ac-comphist-get (db string &optional create)
  (let* ((tab (ac-comphist-tab db))
         (index (gethash string tab)))
    (when (and create (null index))
      (setq index (make-vector (length string) 0))
      (puthash string index tab))
    index))

(defun ac-comphist-add (db string prefix)
  (setq prefix (min prefix (1- (length string))))
  (when (<= 0 prefix)
    (setq string (substring-no-properties string))
    (let ((stat (ac-comphist-get db string t)))
      (cl-incf (aref stat prefix))
      (remhash string (ac-comphist-cache db)))))

(defun ac-comphist-score (db string prefix)
  (setq prefix (min prefix (1- (length string))))
  (if (<= 0 prefix)
      (let ((cache (gethash string (ac-comphist-cache db))))
        (or (and cache (aref cache prefix))
            (let ((stat (ac-comphist-get db string))
                  (score 0.0))
              (when stat
                (cl-loop for p from 0 below (length string)
                      ;; sigmoid function
                      with a = 5
                      with b = (/ 700.0 a) ; bounds for avoiding range error in `exp'
                      with d = (/ 6.0 a)
                      for x = (max (- b) (min b (- d (abs (- prefix p)))))
                      for r = (/ 1.0 (1+ (exp (* (- a) x))))
                      do
                      (cl-incf score (* (aref stat p) r))))
              ;; Weight by distance
              (cl-incf score (max 0.0 (- 0.3 (/ (- (length string) prefix) 100.0))))
              (unless cache
                (setq cache (make-vector (length string) nil))
                (puthash string cache (ac-comphist-cache db)))
              (aset cache prefix score)
              score)))
    0.0))

(defun ac-comphist-sort (db collection prefix &optional threshold)
  (let (result
        (n 0)
        (total 0)
        (cur 0))
    (setq result (mapcar (lambda (a)
                           (when (and cur threshold)
                             (if (>= cur (* total threshold))
                                 (setq cur nil)
                               (cl-incf n)
                               (cl-incf cur (cdr a))))
                           (car a))
                         (sort (mapcar (lambda (string)
                                         (let ((score (ac-comphist-score db string prefix)))
                                           (cl-incf total score)
                                           (cons string score)))
                                       collection)
                               (lambda (a b) (< (cdr b) (cdr a))))))
    (if threshold
        (cons n result)
      result)))

(defun ac-comphist-serialize (db)
  (let (alist)
    (maphash (lambda (k v)
               (push (cons k v) alist))
             (ac-comphist-tab db))
    (list alist)))

(defun ac-comphist-deserialize (sexp)
  (condition-case nil
      (ac-comphist-make (let ((tab (ac-comphist-make-tab)))
                          (mapc (lambda (cons)
                                  (puthash (car cons) (cdr cons) tab))
                                (nth 0 sexp))
                          tab))
    (error (message "Invalid comphist db.") nil)))

(defun ac-comphist-init ()
  (ac-comphist-load)
  (add-hook 'kill-emacs-hook 'ac-comphist-save))

(defun ac-comphist-load ()
  (interactive)
  (let ((db (if (file-exists-p ac-comphist-file)
                (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents ac-comphist-file)
                    (goto-char (point-min))
                    (ac-comphist-deserialize (read (current-buffer))))))))
    (setq ac-comphist (or db (ac-comphist-make)))))

(defun ac-comphist-save ()
  (interactive)
  (require 'pp)
  (ignore-errors
    (with-temp-buffer
      (pp (ac-comphist-serialize ac-comphist) (current-buffer))
      (write-region (point-min) (point-max) ac-comphist-file))))



;;;; Dictionary
(defvar ac-buffer-dictionary nil)
(defvar ac-file-dictionary (make-hash-table :test 'equal))

(defun ac-clear-dictionary-cache ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (local-variable-p 'ac-buffer-dictionary)
          (kill-local-variable 'ac-buffer-dictionary))))
  (clrhash ac-file-dictionary))

(defun ac-file-dictionary (filename)
  (let ((cache (gethash filename ac-file-dictionary 'none)))
    (if (and cache (not (eq cache 'none)))
        cache
      (let (result)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents filename)
            (setq result (split-string (buffer-string) "\n" t))))
        (puthash filename result ac-file-dictionary)
        result))))

(defun ac-mode-dictionary (mode)
  (cl-loop for name in (cons (symbol-name mode)
                          (ignore-errors (list (file-name-extension (buffer-file-name)))))
        append (cl-loop for dir in ac-dictionary-directories
                     for file = (concat dir "/" name)
                     if (file-exists-p file)
                     append (ac-file-dictionary file))))

(defun ac-buffer-dictionary (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (local-variable-p 'ac-buffer-dictionary)
        ac-buffer-dictionary
      (make-local-variable 'ac-buffer-dictionary)
      (setq ac-buffer-dictionary
            (apply 'append
                   ac-user-dictionary
                   (ac-mode-dictionary major-mode)
                   (mapcar 'ac-file-dictionary ac-dictionary-files))))))



;;;; Auto completion internals

(defun ac-menu-at-wrapper-line-p ()
  "Return non-nil if current line is long and wrapped to next visual line."
  (and (not truncate-lines)
       (eq (line-beginning-position)
           (save-excursion
             (vertical-motion 1)
             (line-beginning-position)))))

(defun ac-stop-word-p (word)
  (or (member word ac-stop-words)
      (if ac-use-dictionary-as-stop-words
          (member word (ac-buffer-dictionary)))))

(defun ac-prefix-default ()
  "Same as `ac-prefix-symbol' but ignore a number prefix."
  (let ((start (ac-prefix-symbol))
        (case-fold-search t))
    (when (and start
             (not (string-match-p "\\`\\(?:0[xbo][0-9a-f]+\\|[0-9]+\\)"
                                (buffer-substring-no-properties start (point)))))
      start)))

(defun ac-prefix-symbol ()
  "Default prefix definition function."
  (require 'thingatpt)
  (car-safe (bounds-of-thing-at-point 'symbol)))

(defun ac-prefix-file ()
  "File prefix."
  (let ((point (re-search-backward "[\"<>' \t\r\n]" nil t)))
    (if point (1+ point))))

(defsubst ac-windows-remote-file-p (file)
  (and (memq system-type '(ms-dos windows-nt cygwin))
       (string-match-p "\\`\\(?://\\|\\\\\\\\\\)" file)))

(defun ac-prefix-valid-file ()
  "Existed (or to be existed) file prefix."
  (let* ((line-beg (line-beginning-position))
         (end (point))
         (start (or (let ((point (re-search-backward "[\"<>'= \t\r\n]" line-beg t)))
                      (if point (1+ point)))
                    line-beg))
         (file (buffer-substring start end)))
    (if (and file (or (string-match "^/" file)
                      (and (setq file (and (string-match "^[^/]*/" file)
                                           (match-string 0 file)))
                           (file-directory-p file))))
        (unless (ac-windows-remote-file-p file)
          start))))

(defun ac-prefix-c-dot ()
  "C-like languages dot(.) prefix."
  (if (re-search-backward "\\.\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-prefix-c-dot-ref ()
  "C-like languages dot(.) and reference(->) prefix."
  (if (re-search-backward "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-prefix-cc-member ()
  "C-like languages member(.)(->)(::) prefix."
  (when (re-search-backward "\\(?:\\.\\|->\\|::\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
    (match-beginning 1)))

(defun ac-define-prefix (name prefix)
  "Define new prefix definition.
You can not use it in source definition like (prefix . `NAME')."
  (push (cons name prefix) ac-prefix-definitions))

(defun ac-match-substring (prefix candidates)
  (cl-loop with regexp = (regexp-quote prefix)
           for candidate in candidates
           if (string-match regexp candidate)
           collect candidate))

(defsubst ac-source-entity (source)
  (if (symbolp source)
      (symbol-value source)
    source))

(defun ac-source-available-p (source)
  (if (and (symbolp source)
           (get source 'available))
      (eq (get source 'available) t)
    (let* ((src (ac-source-entity source))
           (avail-pair (assq 'available src))
           (avail-cond (cdr avail-pair))
           (available (and (if avail-pair
                               (cond
                                ((symbolp avail-cond)
                                 (funcall avail-cond))
                                ((listp avail-cond)
                                 (eval avail-cond)))
                             t)
                           (cl-loop for feature in (assoc-default 'depends src)
                                    unless (require feature nil t) return nil
                                    finally return t))))
      (if (symbolp source)
          (put source 'available (if available t 'no)))
      available)))

(defun ac-compile-sources (sources)
  "Compiled `SOURCES' into expanded sources style."
  (cl-loop for source in sources
           if (ac-source-available-p source)
           do
           (setq source (ac-source-entity source))
           ;; prefix
           (let* ((prefix (assoc 'prefix source))
                  (real (assoc-default (cdr prefix) ac-prefix-definitions)))
             (cond
              (real
               (add-to-list 'source (cons 'prefix real)))
              ((null prefix)
               (add-to-list 'source (cons 'prefix 'ac-prefix-default)))))
           ;; match
           (let ((match (assq 'match source)))
             (cond
              ((eq (cdr match) 'substring)
               (setcdr match 'ac-match-substring))))
           and collect source))

(defun ac-compiled-sources ()
  (or ac-compiled-sources
      (setq ac-compiled-sources
            (ac-compile-sources ac-sources))))

(defsubst ac-menu-live-p ()
  (popup-live-p ac-menu))

(defun ac-menu-create (point width height)
  (setq ac-menu
        (popup-create point width height
                      :around t
                      :face 'ac-candidate-face
                      :max-width ac-max-width
                      :mouse-face 'ac-candidate-mouse-face
                      :selection-face 'ac-selection-face
                      :symbol t
                      :scroll-bar t
                      :margin-left 1
                      :keymap ac-menu-map
                      )))

(defun ac-menu-delete ()
  (when ac-menu
    (popup-delete ac-menu)
    (setq ac-menu nil)))

(defsubst ac-inline-overlay ()
  (nth 0 ac-inline))

(defsubst ac-inline-live-p ()
  (and ac-inline (ac-inline-overlay) t))

(defun ac-inline-show (point string)
  (unless ac-inline
    (setq ac-inline (list nil)))
  (save-excursion
    (let ((overlay (ac-inline-overlay))
          (width 0)
          (string-width (string-width string))
          (length 0)
          (original-string string))
      ;; Calculate string space to show completion
      (goto-char point)
      (let (c)
        (while (and (not (eolp))
                    (< width string-width)
                    (setq c (char-after))
                    (not (eq c ?\t)))   ; special case for tab
        (cl-incf width (char-width c))
        (cl-incf length)
        (forward-char)))

      ;; Show completion
      (goto-char point)
      (cond
       ((= width 0)
        ;; End-of-line
        ;; Do nothing
        )
       ((<= width string-width)
        ;; No space to show
        ;; Do nothing
        )
       ((> width string-width)
        ;; Need to fill space
        (setq string (concat string (make-string (- width string-width) ? )))))
      (setq string (propertize string 'face 'ac-completion-face))
      (if overlay
          (progn
            (move-overlay overlay point (+ point length))
            (overlay-put overlay 'invisible nil))
        (setq overlay (make-overlay point (+ point length)))
        (setf (nth 0 ac-inline)  overlay)
        (overlay-put overlay 'priority 9999)
        ;; Help prefix-overlay in some cases
        (overlay-put overlay 'keymap ac-current-map))
      ;; TODO no width but char
      (if (eq length 0)
          ;; Case: End-of-line
          (progn
            (put-text-property 0 1 'cursor t string)
            (overlay-put overlay 'after-string string))
        (let ((display (substring string 0 1))
              (after-string (substring string 1)))
          (overlay-put overlay 'display display)
          (overlay-put overlay 'after-string after-string)))
      (overlay-put overlay 'string original-string))))

(defun ac-inline-delete ()
  (when (ac-inline-live-p)
    (ac-inline-hide)
    (delete-overlay (ac-inline-overlay))
    (setq ac-inline nil)))

(defun ac-inline-hide ()
  (when (ac-inline-live-p)
    (let ((overlay (ac-inline-overlay))
          (buffer-undo-list t))
      (when overlay
        (move-overlay overlay (point-min) (point-min))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'after-string nil)))))

(defun ac-inline-update ()
  (if (and ac-completing ac-prefix (stringp ac-common-part))
      (let ((common-part-length (length ac-common-part))
            (prefix-length (length ac-prefix)))
        (if (> common-part-length prefix-length)
            (progn
              (ac-inline-hide)
              (ac-inline-show (point) (substring ac-common-part prefix-length)))
          (ac-inline-delete)))
    (ac-inline-delete)))

(defun ac-put-prefix-overlay ()
  (unless ac-prefix-overlay
    (let (newline)
      ;; Insert newline to make sure that cursor always on the overlay
      (when (eobp)
        (popup-save-buffer-state
          (insert "\n"))
        (setq newline t))
      (setq ac-prefix-overlay (make-overlay ac-point (1+ (point)) nil t t))
      (overlay-put ac-prefix-overlay 'priority 9999)
      (overlay-put ac-prefix-overlay 'keymap (make-sparse-keymap))
      (overlay-put ac-prefix-overlay 'newline newline))))

(defun ac-remove-prefix-overlay ()
  (when ac-prefix-overlay
    (when (overlay-get ac-prefix-overlay 'newline)
      ;; Remove inserted newline
      (popup-save-buffer-state
        (goto-char (point-max))
        (if (eq (char-before) ?\n)
            (delete-char -1))))
    (delete-overlay ac-prefix-overlay)))

(defun ac-activate-completing-map ()
  (if (and ac-show-menu ac-use-menu-map)
      (set-keymap-parent ac-current-map ac-menu-map))
  (when (and ac-use-overriding-local-map
             (null overriding-terminal-local-map))
    (setq overriding-terminal-local-map ac-current-map))
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) ac-current-map)))

(defun ac-deactivate-completing-map ()
  (set-keymap-parent ac-current-map ac-completing-map)
  (when (and ac-use-overriding-local-map
             (eq overriding-terminal-local-map ac-current-map))
    (setq overriding-terminal-local-map nil))
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) nil)))

(defsubst ac-selected-candidate ()
  (if ac-menu
      (popup-selected-item ac-menu)))

(defun ac-prefix (requires ignore-list)
  (cl-loop with current = (point)
           with point
           with point-def
           with prefix-def
           with sources
           for source in (ac-compiled-sources)
           for prefix = (assoc-default 'prefix source)
           for req = (or (assoc-default 'requires source) requires 1)

           do
           (unless (member prefix ignore-list)
             (save-excursion
               (setq point (cond
                            ((symbolp prefix)
                             (funcall prefix))
                            ((stringp prefix)
                             (and (re-search-backward (concat prefix "\\=") nil t)
                                  (or (match-beginning 1) (match-beginning 0))))
                            ((stringp (car-safe prefix))
                             (let ((regexp (nth 0 prefix))
                                   (end (nth 1 prefix))
                                   (group (nth 2 prefix)))
                               (and (re-search-backward (concat regexp "\\=") nil t)
                                    (funcall (if end 'match-end 'match-beginning)
                                             (or group 0)))))
                            (t
                             (eval prefix))))
               (if (and point
                        (integerp req)
                        (< (- current point) req))
                   (setq point nil))
               (when point
                 (if (null prefix-def)
                     (setq prefix-def prefix
                           point-def point))
                 (if (equal point point-def)
                     (push source sources)))))

           finally return
           (and point-def (list prefix-def point-def (nreverse sources)))))

(defun ac-init ()
  "Initialize current sources to start completion."
  (setq ac-candidates-cache nil)
  (cl-loop for source in ac-current-sources
           for function = (assoc-default 'init source)
           if function do
           (save-excursion
             (cond
              ((functionp function)
               (funcall function))
              (t
               (eval function))))))

(defun ac-candidates-1 (source)
  (let* ((do-cache (assq 'cache source))
         (function (assoc-default 'candidates source))
         (action (assoc-default 'action source))
         (document (assoc-default 'document source))
         (symbol (assoc-default 'symbol source))
         (ac-limit (or (assoc-default 'limit source) ac-limit))
         (face (or (assoc-default 'face source) (assoc-default 'candidate-face source)))
         (selection-face (assoc-default 'selection-face source))
         (cache (and do-cache (assq source ac-candidates-cache)))
         (candidates (cdr cache)))
    (unless cache
      (setq candidates (save-excursion
                         (cond
                          ((functionp function)
                           (funcall function))
                          (t
                           (eval function)))))
      ;; Convert (name value) format candidates into name with text properties.
      (setq candidates (mapcar (lambda (candidate)
                                 (if (consp candidate)
                                     (propertize (car candidate) 'value (cdr candidate))
                                   candidate))
                               candidates))
      (when do-cache
        (push (cons source candidates) ac-candidates-cache)))
    (setq candidates (funcall (or (assoc-default 'match source)
                                  ac-match-function)
                              ac-prefix candidates))
    ;; Remove extra items regarding to ac-limit
    (if (and (integerp ac-limit) (> ac-limit 1) (> (length candidates) ac-limit))
        (setcdr (nthcdr (1- ac-limit) candidates) nil))
    ;; Put candidate properties
    (setq candidates (mapcar (lambda (candidate)
                               (popup-item-propertize candidate
                                                      'action action
                                                      'symbol symbol
                                                      'document document
                                                      'popup-face face
                                                      'selection-face selection-face))
                             candidates))
    candidates))

(defun ac-delete-duplicated-candidates (candidates)
  (cl-delete-duplicates
   candidates
   :test (lambda (x y)
           ;; We assume two candidates are same if their titles are
           ;; equal and their actions are equal.
           (and (equal x y)
                (eq (popup-item-property x 'action)
                    (popup-item-property y 'action))))
   :from-end t))

(defun ac-reduce-candidates (candidates)
  ;; Call `ac-delete-duplicated-candidates' on first portion of
  ;; candidate list for speed.
  (let ((size 20))
    (if (< (length candidates) size)
        (ac-delete-duplicated-candidates candidates)
      (cl-loop for c on candidates by 'cdr
               repeat (1- size)
               finally return
               (let ((rest (cdr c)))
                 (setcdr c nil)
                 (append (ac-delete-duplicated-candidates candidates) (copy-sequence rest)))))))

(defun ac-candidates ()
  "Produce candidates for current sources."
  (cl-loop with completion-ignore-case = (or (eq ac-ignore-case t)
                                             (and (eq ac-ignore-case 'smart)
                                                  (let ((case-fold-search nil)) (not (string-match "[[:upper:]]" ac-prefix)))))
           with case-fold-search = completion-ignore-case
           with prefix-len = (length ac-prefix)
           for source in ac-current-sources
           append (ac-candidates-1 source) into candidates
           finally return
           (progn
             (if (and ac-use-comphist ac-comphist)
                 (if ac-show-menu
                     (let* ((pair (ac-comphist-sort ac-comphist candidates prefix-len ac-comphist-threshold))
                            (n (car pair))
                            (result (ac-reduce-candidates (cdr pair)))
                            (cons (if (> n 0) (nthcdr (1- n) result)))
                            (cdr (cdr cons)))
                       ;; XXX ugly
                       (if cons (setcdr cons nil))
                       (setq ac-common-part (try-completion ac-prefix result))
                       (setq ac-whole-common-part (try-completion ac-prefix candidates))
                       (if cons (setcdr cons cdr))
                       result)
                   (setq candidates (ac-comphist-sort ac-comphist candidates prefix-len))
                   (setq ac-common-part (if candidates (popup-x-to-string (car candidates))))
                   (setq ac-whole-common-part (try-completion ac-prefix candidates))
                   candidates)
               (when ac-show-menu
                 (setq candidates (ac-reduce-candidates candidates)))
               (setq ac-common-part (try-completion ac-prefix candidates))
               (setq ac-whole-common-part ac-common-part)
               candidates))))

(defun ac-update-candidates (cursor scroll-top)
  "Update candidates of menu to `ac-candidates' and redraw it."
  (setf (popup-cursor ac-menu) cursor
        (popup-scroll-top ac-menu) scroll-top)
  (setq ac-dwim-enable (= (length ac-candidates) 1))
  (if ac-candidates
      (progn
        (setq ac-completing t)
        (ac-activate-completing-map))
    (setq ac-completing nil)
    (ac-deactivate-completing-map))
  (unless ac-disable-inline
    (ac-inline-update))
  (popup-set-list ac-menu ac-candidates)
  (if (and (not ac-fuzzy-enable)
           (<= (length ac-candidates) ac-candidate-menu-min))
      (popup-hide ac-menu)
    (if ac-show-menu
        (popup-draw ac-menu))))

(defun ac-reposition ()
  "Force to redraw candidate menu with current `ac-candidates'."
  (let ((cursor (popup-cursor ac-menu))
        (scroll-top (popup-scroll-top ac-menu))
        (height (popup-height ac-menu)))
    (ac-menu-delete)
    (ac-menu-create ac-point (popup-preferred-width ac-candidates) height)
    (ac-update-candidates cursor scroll-top)))

(defun ac-cleanup ()
  "Cleanup auto completion."
  (if ac-cursor-color
      (set-cursor-color ac-cursor-color))
  (when (and ac-use-comphist ac-comphist)
    (when (and (null ac-selected-candidate)
               (member ac-prefix ac-candidates))
      ;; Assume candidate is selected by just typing
      (setq ac-selected-candidate ac-prefix)
      (setq ac-last-point ac-point))
    (when ac-selected-candidate
      (ac-comphist-add ac-comphist
                       ac-selected-candidate
                       (if ac-last-point
                           (- ac-last-point ac-point)
                         (length ac-prefix)))))
  (ac-deactivate-completing-map)
  (ac-remove-prefix-overlay)
  (ac-remove-quick-help)
  (ac-inline-delete)
  (ac-menu-delete)
  (ac-cancel-timer)
  (ac-cancel-show-menu-timer)
  (ac-cancel-quick-help-timer)
  (setq ac-cursor-color nil
        ac-inline nil
        ac-show-menu nil
        ac-menu nil
        ac-completing nil
        ac-point nil
        ac-last-point nil
        ac-prefix nil
        ac-prefix-overlay nil
        ac-selected-candidate nil
        ac-common-part nil
        ac-whole-common-part nil
        ac-triggered nil
        ac-limit nil
        ac-candidates nil
        ac-candidates-cache nil
        ac-fuzzy-enable nil
        ac-dwim-enable nil
        ac-compiled-sources nil
        ac-current-sources nil
        ac-current-prefix-def nil
        ac-ignoring-prefix-def nil))

(defsubst ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-extend-region-to-delete (string)
  "Determine the boundary of the region to delete before
inserting the completed string. This will be either the position
of current point, or the end of the symbol at point, if the text
from point to end of symbol is the right part of the completed
string."
  (let* ((end-of-symbol (or (cdr-safe (bounds-of-thing-at-point 'symbol))
                            (point)))
         (remaindar (buffer-substring-no-properties (point) end-of-symbol))
         (remaindar-length (length remaindar)))
    (if (and (>= (length string) remaindar-length)
             (string= (substring-no-properties string (- remaindar-length))
                      remaindar))
        end-of-symbol
      (point))))

(defun ac-expand-string (string &optional remove-undo-boundary)
  "Expand `STRING' into the buffer and update `ac-prefix' to `STRING'.
This function records deletion and insertion sequences by `undo-boundary'.
If `remove-undo-boundary' is non-nil, this function also removes `undo-boundary'
that have been made before in this function.  When `buffer-undo-list' is
`t', `remove-undo-boundary' has no effect."
  (when (eq buffer-undo-list t)
    (setq remove-undo-boundary nil))
  (when (not (equal string (buffer-substring ac-point (point))))
    (undo-boundary)
    ;; We can't use primitive-undo since it undoes by
    ;; groups, divided by boundaries.
    ;; We don't want boundary between deletion and insertion.
    ;; So do it manually.
    ;; Delete region silently for undo:
    (if remove-undo-boundary
        (progn
          (let (buffer-undo-list)
            (save-excursion
              (delete-region ac-point (ac-extend-region-to-delete string))))
          (setq buffer-undo-list
                (nthcdr 2 buffer-undo-list)))
      (delete-region ac-point (ac-extend-region-to-delete string)))
    (insert (substring-no-properties string))
    ;; Sometimes, possible when omni-completion used, (insert) added
    ;; to buffer-undo-list strange record about position changes.
    ;; Delete it here:
    (when (and remove-undo-boundary
               (integerp (cadr buffer-undo-list)))
      (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))
    (undo-boundary)
    (setq ac-selected-candidate string)
    (setq ac-prefix string)))

(defun ac-set-trigger-key (key)
  "Set `ac-trigger-key' to `KEY'. It is recommemded to use this function instead of calling `setq'."
  ;; Remove old mapping
  (when ac-trigger-key
    (define-key ac-mode-map (read-kbd-macro ac-trigger-key) nil))

  ;; Make new mapping
  (setq ac-trigger-key key)
  (when key
    (define-key ac-mode-map (read-kbd-macro key) 'ac-trigger-key-command)))

(defun ac-set-timer ()
  (unless ac-timer
    (setq ac-timer (run-with-idle-timer ac-delay ac-delay 'ac-update-greedy))))

(defun ac-cancel-timer ()
  (when (timerp ac-timer)
    (cancel-timer ac-timer)
    (setq ac-timer nil)))

(defun ac-update (&optional force)
  (when (and auto-complete-mode
             ac-prefix
             (or ac-triggered
                 force)
             (not isearch-mode))
    (ac-put-prefix-overlay)
    (setq ac-candidates (ac-candidates))
    (let ((preferred-width (popup-preferred-width ac-candidates)))
      ;; Reposition if needed
      (when (or (null ac-menu)
                (>= (popup-width ac-menu) preferred-width)
                (<= (popup-width ac-menu) (- preferred-width 10))
                (and (> (popup-direction ac-menu) 0)
                     (ac-menu-at-wrapper-line-p)))
        (ac-inline-hide) ; Hide overlay to calculate correct column
        (ac-remove-quick-help)
        (ac-menu-delete)
        (ac-menu-create ac-point preferred-width ac-menu-height)))
    (ac-update-candidates 0 0)
    t))

(defun ac-update-greedy (&optional force)
  (let (result)
    (while (when (and (setq result (ac-update force))
                      (null ac-candidates))
             (add-to-list 'ac-ignoring-prefix-def ac-current-prefix-def)
             (ac-start :force-init t)
             ac-current-prefix-def))
    result))

(defun ac-set-show-menu-timer ()
  (when (and (or (integerp ac-auto-show-menu) (floatp ac-auto-show-menu))
             (null ac-show-menu-timer))
    (setq ac-show-menu-timer (run-with-idle-timer ac-auto-show-menu ac-auto-show-menu 'ac-show-menu))))

(defun ac-cancel-show-menu-timer ()
  (when (timerp ac-show-menu-timer)
    (cancel-timer ac-show-menu-timer)
    (setq ac-show-menu-timer nil)))

(defun ac-show-menu ()
  (when (not (eq ac-show-menu t))
    (setq ac-show-menu t)
    (ac-inline-hide)
    (ac-remove-quick-help)
    (ac-update t)))

(defun ac-help (&optional persist)
  (interactive "P")
  (when ac-menu
    (popup-menu-show-help ac-menu persist)))

(defun ac-persist-help ()
  (interactive)
  (ac-help t))

(defun ac-last-help (&optional persist)
  (interactive "P")
  (when ac-last-completion
    (popup-item-show-help (cdr ac-last-completion) persist)))

(defun ac-last-persist-help ()
  (interactive)
  (ac-last-help t))

(defun ac-set-quick-help-timer ()
  (when (and ac-use-quick-help
             (null ac-quick-help-timer))
    (setq ac-quick-help-timer (run-with-idle-timer ac-quick-help-delay ac-quick-help-delay 'ac-quick-help))))

(defun ac-cancel-quick-help-timer ()
  (when (timerp ac-quick-help-timer)
    (cancel-timer ac-quick-help-timer)
    (setq ac-quick-help-timer nil)))

(defun ac-pos-tip-show-quick-help (menu &optional item &rest args)
  (let* ((point (plist-get args :point))
         (around nil)
         (parent-offset (popup-offset menu))
         (doc (popup-menu-documentation menu item)))
    (when (stringp doc)
      (if (popup-hidden-p menu)
          (setq around t)
        (setq point nil))
      (with-no-warnings
        (pos-tip-show doc
                      'popup-tip-face
                      (or point
                          (and menu
                               (popup-child-point menu parent-offset))
                          (point))
                      nil 300
                      popup-tip-max-width
                      nil nil
                      (and (not around) 0))
        (unless (plist-get args :nowait)
          (clear-this-command-keys)
          (unwind-protect
              (push (read-event (plist-get args :prompt)) unread-command-events)
            (pos-tip-hide))
          t)))))

(defun ac-quick-help-use-pos-tip-p ()
  (and ac-quick-help-prefer-pos-tip
       window-system
       (featurep 'pos-tip)))

(defun ac-quick-help (&optional force)
  (interactive)
  ;; TODO don't use FORCE
  (when (and (or force
                 (with-no-warnings
                   ;; called-interactively-p can take no args
                   (called-interactively-p))
                 ;; ac-isearch'ing
                 (null this-command))
             (ac-menu-live-p)
             (null ac-quick-help))
      (setq ac-quick-help
            (funcall (if (ac-quick-help-use-pos-tip-p)
                         'ac-pos-tip-show-quick-help
                       'popup-menu-show-quick-help)
                     ac-menu nil
                     :point ac-point
                     :height ac-quick-help-height
                     :nowait t))))

(defun ac-remove-quick-help ()
  (when (ac-quick-help-use-pos-tip-p)
    (with-no-warnings
      (pos-tip-hide)))
  (when ac-quick-help
    (popup-delete ac-quick-help)
    (setq ac-quick-help nil)))

(defun ac-last-quick-help ()
  (interactive)
  (when (and ac-last-completion
             (eq (marker-buffer (car ac-last-completion))
                 (current-buffer)))
    (let ((doc (popup-item-documentation (cdr ac-last-completion)))
          (point (marker-position (car ac-last-completion))))
      (when (stringp doc)
        (if (ac-quick-help-use-pos-tip-p)
            (with-no-warnings (pos-tip-show doc nil point nil 300))
          (popup-tip doc
                     :point point
                     :around t
                     :scroll-bar t
                     :margin t))))))

(defmacro ac-define-quick-help-command (name arglist &rest body)
  (declare (indent 2))
  `(progn
     (defun ,name ,arglist ,@body)
     (put ',name 'ac-quick-help-command t)))

(ac-define-quick-help-command ac-quick-help-scroll-down ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-down ac-quick-help)))

(ac-define-quick-help-command ac-quick-help-scroll-up ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-up ac-quick-help)))



;;;; Auto completion isearch

(defun ac-isearch-callback (list)
  (setq ac-dwim-enable (eq (length list) 1)))

(defun ac-isearch ()
  (interactive)
  (when (ac-menu-live-p)
    (ac-cancel-show-menu-timer)
    (ac-show-menu)
    (if ac-use-quick-help
        (let ((popup-menu-show-quick-help-function
               (if (ac-quick-help-use-pos-tip-p)
                   'ac-pos-tip-show-quick-help
                 'popup-menu-show-quick-help)))
          (popup-isearch ac-menu
                         :callback 'ac-isearch-callback
                         :help-delay ac-quick-help-delay))
      (popup-isearch ac-menu :callback 'ac-isearch-callback))))



;;;; Auto completion commands

(cl-defun auto-complete-1 (&key sources (triggered 'command))
  (let ((menu-live (ac-menu-live-p))
        (inline-live (ac-inline-live-p))
        started)
    (ac-abort)
    (let ((ac-sources (or sources ac-sources)))
      (if (or ac-show-menu-immediately-on-auto-complete
              inline-live)
          (setq ac-show-menu t))
      (setq started (ac-start :triggered triggered)))
    (when (ac-update-greedy t)
      ;; TODO Not to cause inline completion to be disrupted.
      (if (ac-inline-live-p)
          (ac-inline-hide))
      ;; Not to expand when it is first time to complete
      (when (and (or (and (not ac-expand-on-auto-complete)
                          (> (length ac-candidates) 1)
                          (not menu-live))
                     (not (let ((ac-common-part ac-whole-common-part))
                            (ac-expand-common))))
                 ac-use-fuzzy
                 (null ac-candidates))
        (ac-fuzzy-complete)))
    started))

;;;###autoload
(defun auto-complete (&optional sources)
  "Start auto-completion at current point."
  (interactive)
  (auto-complete-1 :sources sources))

(defun ac-fuzzy-complete ()
  "Start fuzzy completion at current point."
  (interactive)
  (if (not (require 'fuzzy nil t))
      (message "Please install fuzzy.el if you use fuzzy completion")
    (unless (ac-menu-live-p)
      (ac-start))
    (let ((ac-match-function 'fuzzy-all-completions))
      (when ac-fuzzy-cursor-color
        (unless ac-cursor-color
          (setq ac-cursor-color (frame-parameter (selected-frame) 'cursor-color)))
        (set-cursor-color ac-fuzzy-cursor-color))
      (setq ac-show-menu t)
      (setq ac-fuzzy-enable t)
      (setq ac-triggered nil)
      (ac-update t)))
  t)

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (popup-next ac-menu)
    (if (eq this-command 'ac-next)
        (setq ac-dwim-enable t))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (popup-previous ac-menu)
    (if (eq this-command 'ac-previous)
        (setq ac-dwim-enable t))))

(defun ac-expand (arg)
  "Try expand, and if expanded twice, select next candidate.
If given a prefix argument, select the previous candidate."
  (interactive "P")
  (unless (ac-expand-common)
    (let ((string (ac-selected-candidate)))
      (when string
        (when (equal ac-prefix string)
          (if (not arg)
              (ac-next)
            (ac-previous))
          (setq string (ac-selected-candidate)))
        (ac-expand-string string
                          (or (eq last-command 'ac-expand)
                             (eq last-command 'ac-expand-previous)))
        ;; Do reposition if menu at long line
        (if (and (> (popup-direction ac-menu) 0)
               (ac-menu-at-wrapper-line-p))
            (ac-reposition))
        (setq ac-show-menu t)
        string))))

(defun ac-expand-previous (arg)
  "Like `ac-expand', but select previous candidate."
  (interactive "P")
  (ac-expand (not arg)))

(defun ac-expand-common ()
  "Try to expand meaningful common part."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (when (and (ac-inline-live-p)
               ac-common-part)
      (ac-inline-hide)
      (ac-expand-string ac-common-part (eq last-command this-command))
      (setq ac-common-part nil)
      t)))

(defun ac-complete-1 (candidate)
  (let ((action (popup-item-property candidate 'action))
        (fallback nil))
    (when candidate
      (unless (ac-expand-string candidate)
        (setq fallback t))
      ;; Remember to show help later
      (when (and ac-point candidate)
        (unless ac-last-completion
          (setq ac-last-completion (cons (make-marker) nil)))
        (set-marker (car ac-last-completion) ac-point ac-buffer)
        (setcdr ac-last-completion candidate)))
    (ac-abort)
    (cond
     (action
      (funcall action))
     (fallback
      (ac-fallback-command)))
    candidate))

(defun ac-complete ()
  "Try complete."
  (interactive)
  (ac-complete-1 (ac-selected-candidate)))

(cl-defun ac-start (&key
                    requires
                    force-init
                    (triggered (or ac-triggered t)))
  "Start completion."
  (interactive)
  (if (not auto-complete-mode)
      (message "auto-complete-mode is not enabled")
    (let* ((info (ac-prefix requires ac-ignoring-prefix-def))
           (prefix-def (nth 0 info))
           (point (nth 1 info))
           (sources (nth 2 info))
           prefix
           (init (or force-init (not (eq ac-point point)))))
      (if (or (null point)
              (progn
                (setq prefix (buffer-substring-no-properties point (point)))
                (and (not (eq triggered 'command))
                     (ac-stop-word-p prefix))))
          (prog1 nil
            (ac-abort))
        (when (and ac-use-fuzzy ac-fuzzy-cursor-color)
          (unless ac-cursor-color
            (setq ac-cursor-color (frame-parameter (selected-frame) 'cursor-color))))
        (setq ac-show-menu (or ac-show-menu (if (eq ac-auto-show-menu t) t))
              ac-current-sources sources
              ac-buffer (current-buffer)
              ac-point point
              ac-prefix prefix
              ac-limit ac-candidate-limit
              ac-triggered triggered
              ac-current-prefix-def prefix-def)
        (when (or init (null ac-prefix-overlay))
          (ac-init))
        (ac-set-timer)
        (ac-set-show-menu-timer)
        (ac-set-quick-help-timer)
        (ac-put-prefix-overlay)
        t))))

(defun ac-stop ()
  "Stop completing."
  (interactive)
  (setq ac-selected-candidate nil)
  (ac-abort))

(defun ac-ignore (&rest ignore)
  "Same as `ignore'."
  (interactive))

(defun ac-mouse-1 (event)
  (interactive "e")
  (popup-awhen (popup-menu-item-of-mouse-event event)
    (ac-complete-1 it)))

(defun ac-mouse-4 (event)
  (interactive "e")
  (ac-previous))

(defun ac-mouse-5 (event)
  (interactive "e")
  (ac-next))

(defun ac-trigger-key-command (&optional force)
  (interactive "P")
  (let (started)
    (when (or force (ac-trigger-command-p last-command))
      (setq started (auto-complete-1 :triggered 'trigger-key)))
    (unless started
      (ac-fallback-command 'ac-trigger-key-command))))



;;;; Basic cache facility

(defvar ac-clear-variables-every-minute-timer nil)
(defvar ac-clear-variables-after-save nil)
(defvar ac-clear-variables-every-minute nil)
(defvar ac-minutes-counter 0)

(defun ac-clear-variable-after-save (variable &optional pred)
  (add-to-list 'ac-clear-variables-after-save (cons variable pred)))

(defun ac-clear-variables-after-save ()
  (dolist (pair ac-clear-variables-after-save)
    (if (or (null (cdr pair))
            (funcall (cdr pair)))
        (set (car pair) nil))))

(defun ac-clear-variable-every-minutes (variable minutes)
  (add-to-list 'ac-clear-variables-every-minute (cons variable minutes)))

(defun ac-clear-variable-every-minute (variable)
  (ac-clear-variable-every-minutes variable 1))

(defun ac-clear-variable-every-10-minutes (variable)
  (ac-clear-variable-every-minutes variable 10))

(defun ac-clear-variables-every-minute ()
  (cl-incf ac-minutes-counter)
  (dolist (pair ac-clear-variables-every-minute)
    (if (eq (% ac-minutes-counter (cdr pair)) 0)
        (set (car pair) nil))))



;;;; Auto complete mode

(defun ac-cursor-on-diable-face-p (&optional point)
  (memq (get-text-property (or point (point)) 'face) ac-disable-faces))

(defun ac-trigger-command-p (command)
  "Return non-nil if `COMMAND' is a trigger command."
  (and (symbolp command)
       (not (memq command ac-non-trigger-commands))
       (or (memq command ac-trigger-commands)
           (string-match "self-insert-command" (symbol-name command))
           (string-match "electric" (symbol-name command)))))

(defun ac-fallback-key-sequence ()
  (setq unread-command-events
        (append (this-single-command-raw-keys)
                unread-command-events))
  (read-key-sequence-vector ""))

(defun ac-fallback-command (&optional except-command)
  (let* ((auto-complete-mode nil)
         (keys (ac-fallback-key-sequence))
         (command (and keys (key-binding keys))))
    (when (and (commandp command)
               (not (eq command except-command)))
      (setq this-command command)
      (call-interactively command))))

(defun ac-compatible-package-command-p (command)
  "Return non-nil if `COMMAND' is compatible with auto-complete."
  (and (symbolp command)
       (string-match ac-compatible-packages-regexp (symbol-name command))))

(defun ac-handle-pre-command ()
  (condition-case var
      (if (or (setq ac-triggered (and (not ac-fuzzy-enable) ; ignore key storkes in fuzzy mode
                                      (or (eq this-command 'auto-complete) ; special case
                                          (ac-trigger-command-p this-command)
                                          (and ac-completing
                                               (memq this-command ac-trigger-commands-on-completing)))
                                      (not (ac-cursor-on-diable-face-p))
                                      (or ac-triggered t)))
              (ac-compatible-package-command-p this-command))
          (progn
            (if (or (not (symbolp this-command))
                    (not (get this-command 'ac-quick-help-command)))
                (ac-remove-quick-help))
            ;; Not to cause inline completion to be disrupted.
            (ac-inline-hide))
        (ac-abort))
    (error (ac-error var))))

(defun ac-handle-post-command ()
  (condition-case var
      (when (and ac-triggered
                 (or ac-auto-start
                     ac-completing)
                 (not isearch-mode))
        (setq ac-last-point (point))
        (ac-start :requires (unless ac-completing ac-auto-start))
        (unless ac-disable-inline
          (ac-inline-update)))
    (error (ac-error var))))

(defvar ac-flycheck-poll-completion-end-timer nil
  "Timer to poll end of completion.")

(defun ac-syntax-checker-workaround ()
  (if ac-stop-flymake-on-completing
      (progn
        (make-local-variable 'ac-flycheck-poll-completion-end-timer)
        (when (require 'flymake nil t)
          (defadvice flymake-on-timer-event (around ac-flymake-stop-advice activate)
            (unless ac-completing
              ad-do-it)))
        (when (require 'flycheck nil t)
          (defadvice flycheck-handle-idle-change (around ac-flycheck-stop-advice activate)
            (if ac-completing
                (setq ac-flycheck-poll-completion-end-timer
                      (run-at-time ac-flycheck-poll-completion-end-interval
                                   nil
                                   #'flycheck-handle-idle-change))
              ad-do-it))))
    (when (featurep 'flymake)
      (ad-disable-advice 'flymake-on-timer-event 'around 'ac-flymake-stop-advice))
    (when (featurep 'flycheck)
      (ad-disable-advice 'flycheck-handle-idle-change 'around 'ac-flycheck-stop-advice))))

(defun ac-setup ()
  (if ac-trigger-key
      (ac-set-trigger-key ac-trigger-key))
  (if ac-use-comphist
      (ac-comphist-init))
  (unless ac-clear-variables-every-minute-timer
    (setq ac-clear-variables-every-minute-timer (run-with-timer 60 60 'ac-clear-variables-every-minute)))
  (ac-syntax-checker-workaround))

;;;###autoload
(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :keymap ac-mode-map
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (ac-setup)
        (add-hook 'pre-command-hook 'ac-handle-pre-command nil t)
        (add-hook 'post-command-hook 'ac-handle-post-command nil t)
        (add-hook 'after-save-hook 'ac-clear-variables-after-save nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'pre-command-hook 'ac-handle-pre-command t)
    (remove-hook 'post-command-hook 'ac-handle-post-command t)
    (remove-hook 'after-save-hook 'ac-clear-variables-after-save t)
    (ac-abort)))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

;;;###autoload
(define-global-minor-mode global-auto-complete-mode
  auto-complete-mode auto-complete-mode-maybe
  :group 'auto-complete)



;;;; Compatibilities with other extensions

(defun ac-flyspell-workaround ()
  "Flyspell uses `sit-for' for delaying its process. Unfortunatelly,
it stops auto completion which is trigger with `run-with-idle-timer'.
This workaround avoid flyspell processes when auto completion is being started."
  (interactive)
  (defadvice flyspell-post-command-hook (around ac-flyspell-workaround activate)
    (unless ac-triggered
      ad-do-it)))

(defun ac-linum-workaround ()
  "linum-mode tries to display the line numbers even for the
completion menu. This workaround stops that annoying behavior."
  (interactive)
  (defadvice linum-update (around ac-linum-update-workaround activate)
    (unless ac-completing
      ad-do-it)))



;;;; Standard sources

(defmacro ac-define-source (name source)
  "Source definition macro. It defines a complete command also."
  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "ac-source-%s" name)))
     ;; Use `setq' to reset ac-source-NAME every time
     ;; `ac-define-source' is called.  This is useful, for example
     ;; when evaluating `ac-define-source' using C-M-x (`eval-defun').
     (setq ,(intern (format "ac-source-%s" name)) ,source)
     (defun ,(intern (format "ac-complete-%s" name)) ()
       (interactive)
       (auto-complete '(,(intern (format "ac-source-%s" name)))))))

;; Words in buffer source
(defvar ac-word-index nil)

(defun ac-candidate-words-in-buffer (point prefix limit)
  (let ((i 0)
        candidate
        candidates
        (regexp (concat "\\_<" (regexp-quote prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
    (save-excursion
      ;; Search backward
      (goto-char point)
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-backward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (cl-incf i)))
      ;; Search backward
      (goto-char (+ point (length prefix)))
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (cl-incf i)))
      (nreverse candidates))))

(defun ac-incremental-update-word-index ()
  (unless (local-variable-p 'ac-word-index)
    (make-local-variable 'ac-word-index))
  (if (null ac-word-index)
      (setq ac-word-index (cons nil nil)))
  ;; Mark incomplete
  (if (car ac-word-index)
      (setcar ac-word-index nil))
  (let ((index (cdr ac-word-index))
        (words (ac-candidate-words-in-buffer ac-point ac-prefix (or (and (integerp ac-limit) ac-limit) 10))))
    (dolist (word words)
      (unless (member word index)
        (push word index)
        (setcdr ac-word-index index)))))

(defun ac-update-word-index-1 ()
  (unless (local-variable-p 'ac-word-index)
    (make-local-variable 'ac-word-index))
  (when (and (not (car ac-word-index))
             (< (buffer-size) 1048576))
    ;; Complete index
    (setq ac-word-index
          (cons t
                (split-string (buffer-substring-no-properties (point-min) (point-max))
                              "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)")))))

(defun ac-update-word-index ()
  (dolist (buffer (buffer-list))
    (when (or ac-fuzzy-enable
              (not (eq buffer (current-buffer))))
      (with-current-buffer buffer
        (ac-update-word-index-1)))))

(defun ac-word-candidates (&optional buffer-pred)
  (cl-loop initially (unless ac-fuzzy-enable (ac-incremental-update-word-index))
           for buffer in (buffer-list)
           if (and (or (not (integerp ac-limit)) (< (length candidates) ac-limit))
                   (if buffer-pred (funcall buffer-pred buffer) t))
           append (funcall ac-match-function
                           ac-prefix
                           (and (local-variable-p 'ac-word-index buffer)
                                (cdr (buffer-local-value 'ac-word-index buffer))))
           into candidates
           finally return (delete-dups candidates)))

(ac-define-source words-in-buffer
  '((candidates . ac-word-candidates)))

(ac-define-source words-in-all-buffer
  '((init . ac-update-word-index)
    (candidates . ac-word-candidates)))

(ac-define-source words-in-same-mode-buffers
  '((init . ac-update-word-index)
    (candidates . (ac-word-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))))))

;; Lisp symbols source
(defvar ac-symbols-cache nil)
(ac-clear-variable-every-10-minutes 'ac-symbols-cache)

(defun ac-symbol-file (symbol type)
  (if (fboundp 'find-lisp-object-file-name)
      (find-lisp-object-file-name symbol type)
    (let ((file-name (with-no-warnings
                       (describe-simplify-lib-file-name
                        (symbol-file symbol type)))))
      (when (equal file-name "loaddefs.el")
        ;; Find the real def site of the preloaded object.
        (let ((location (condition-case nil
                            (if (eq type 'defun)
                                (find-function-search-for-symbol symbol nil
                                                                 "loaddefs.el")
                              (find-variable-noselect symbol file-name))
                          (error nil))))
          (when location
            (with-current-buffer (car location)
              (when (cdr location)
                (goto-char (cdr location)))
              (when (re-search-backward
                     "^;;; Generated autoloads from \\(.*\\)" nil t)
                (setq file-name (match-string 1)))))))
      (if (and (null file-name)
               (or (eq type 'defun)
                   (integerp (get symbol 'variable-documentation))))
          ;; It's a object not defined in Elisp but in C.
          (if (get-buffer " *DOC*")
              (if (eq type 'defun)
                  (help-C-file-name (symbol-function symbol) 'subr)
                (help-C-file-name symbol 'var))
            'C-source)
        file-name))))

(defun ac-symbol-documentation (symbol)
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))
  (ignore-errors
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (prin1 symbol)
        (princ " is ")
        (cond
         ((fboundp symbol)
          ;; import help-xref-following
          (require 'help-mode)
          (let ((help-xref-following t)
                (major-mode 'help-mode)) ; avoid error in Emacs 24
            (describe-function-1 symbol))
          (buffer-string))
         ((boundp symbol)
          (let ((file-name  (ac-symbol-file symbol 'defvar)))
            (princ "a variable")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'variable-documentation t)
                       "Not documented."))
            (buffer-string)))
         ((facep symbol)
          (let ((file-name  (ac-symbol-file symbol 'defface)))
            (princ "a face")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'face-documentation t)
                       "Not documented."))
            (buffer-string)))
         (t
          (let ((doc (documentation-property symbol 'group-documentation t)))
            (when doc
              (princ "a group.\n\n")
              (princ doc)
              (buffer-string)))))))))

(defun ac-symbol-candidates ()
  (or ac-symbols-cache
      (setq ac-symbols-cache
            (cl-loop for x being the symbols
                     if (or (fboundp x)
                            (boundp x)
                            (symbol-plist x))
                     collect (symbol-name x)))))

(ac-define-source symbols
  '((candidates . ac-symbol-candidates)
    (document . ac-symbol-documentation)
    (symbol . "s")
    (cache)))

;; Lisp functions source
(defvar ac-functions-cache nil)
(ac-clear-variable-every-10-minutes 'ac-functions-cache)

(defun ac-function-candidates ()
  (or ac-functions-cache
      (setq ac-functions-cache
            (cl-loop for x being the symbols
                     if (fboundp x)
                     collect (symbol-name x)))))

(ac-define-source functions
  '((candidates . ac-function-candidates)
    (document . ac-symbol-documentation)
    (symbol . "f")
    (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
    (cache)))

;; Lisp variables source
(defvar ac-variables-cache nil)
(ac-clear-variable-every-10-minutes 'ac-variables-cache)

(defun ac-variable-candidates ()
  (or ac-variables-cache
      (setq ac-variables-cache
            (cl-loop for x being the symbols
                     if (boundp x)
                     collect (symbol-name x)))))

(ac-define-source variables
  '((candidates . ac-variable-candidates)
    (document . ac-symbol-documentation)
    (symbol . "v")
    (cache)))

;; Lisp features source
(defvar ac-emacs-lisp-features nil)
(ac-clear-variable-every-10-minutes 'ac-emacs-lisp-features)

(defun ac-emacs-lisp-feature-candidates ()
  (or ac-emacs-lisp-features
      (if (fboundp 'find-library-suffixes)
          (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
            (setq ac-emacs-lisp-features
                  (append (mapcar 'prin1-to-string features)
                          (cl-loop for dir in load-path
                                   if (file-directory-p dir)
                                   append (cl-loop for file in (directory-files dir)
                                                   if (string-match suffix file)
                                                   collect (substring file 0 (match-beginning 0))))))))))

(ac-define-source features
  '((depends find-func)
    (candidates . ac-emacs-lisp-feature-candidates)
    (prefix . "require +'\\(\\(?:\\sw\\|\\s_\\)*\\)")
    (requires . 0)))

(defvaralias 'ac-source-emacs-lisp-features 'ac-source-features)

;; Abbrev source
(ac-define-source abbrev
  '((candidates . (mapcar 'popup-x-to-string (append (vconcat local-abbrev-table global-abbrev-table) nil)))
    (action . expand-abbrev)
    (symbol . "a")
    (cache)))

;; Files in current directory source
(ac-define-source files-in-current-dir
  '((candidates . (directory-files default-directory))
    (cache)))

;; Filename source
(defvar ac-filename-cache nil)

(defun ac-filename-candidate ()
  (let (file-name-handler-alist)
    (unless (or (and comment-start-skip
                     (string-match comment-start-skip ac-prefix))
                (file-regular-p ac-prefix))
      (ignore-errors
        (cl-loop with dir = (file-name-directory ac-prefix)
                 with files = (or (assoc-default dir ac-filename-cache)
                                  (let ((files (directory-files dir nil "^[^.]")))
                                    (push (cons dir files) ac-filename-cache)
                                    files))
                 for file in files
                 for path = (concat dir file)
                 collect (if (file-directory-p path)
                             (concat path "/")
                           path))))))

(ac-define-source filename
  '((init . (setq ac-filename-cache nil))
    (candidates . ac-filename-candidate)
    (prefix . valid-file)
    (requires . 0)
    (action . ac-start)
    (limit . nil)))

;; Dictionary source
(ac-define-source dictionary
  '((candidates . ac-buffer-dictionary)
    (symbol . "d")))

(provide 'auto-complete)
;;; auto-complete.el ends here
