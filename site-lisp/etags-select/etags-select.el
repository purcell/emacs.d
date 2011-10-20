;;; etags-select.el --- Select from multiple tags

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 07 Jun 2007
;; Version: 1.13
;; Keywords: etags tags tag select

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; Open a buffer with file/lines of exact-match tags shown.  Select one by
;; going to a line and pressing return.  pop-tag-mark still works with this
;; code.
;;
;; If there is only one match, you can skip opening the selection window by
;; setting a custom variable.  This means you could substitute the key binding
;; for find-tag-at-point with etags-select-find-tag-at-point, although it
;; won't play well with tags-loop-continue.  On the other hand, if you like
;; the behavior of tags-loop-continue you probably don't need this code.
;;
;; I use this:
;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; (global-set-key "\M-." 'etags-select-find-tag)
;;
;; Contributers of ideas and/or code:
;; David Engster
;; James Ferguson
;;
;;; Change log:
;;
;; 28 Oct 2008 -- v1.13
;;                Add short tag name completion option
;;                Add go-if-tagnum-is-unambiguous option
;; 13 May 2008 -- v1.12
;;                Fix completion bug for XEmacs etags
;;                Add highlighting of tag after jump
;; 28 Apr 2008 -- v1.11
;;                Add tag completion
;; 25 Sep 2007 -- v1.10
;;                Fix save window layout bug
;; 25 Sep 2007 -- v1.9
;;                Add function to prompt for tag to find (instead of using
;;                what is at point)
;; 25 Sep 2007 -- v1.8
;;                Don't mess up user's window layout.
;;                Add function/binding to go to the tag in other window.
;; 10 Sep 2007 -- v1.7
;;                Disambiguate tags with matching suffixes
;; 04 Sep 2007 -- v1.6
;;                Speed up tag searching
;; 27 Jul 2007 -- v1.5
;;                Respect case-fold-search and tags-case-fold-search
;; 24 Jul 2007 -- v1.4
;;                Fix filenames for tag files with absolute paths
;; 24 Jul 2007 -- v1.3
;;                Handle qualified and implicit tags.
;;                Add tag name to display.
;;                Add tag numbers so you can jump directly to one.
;; 13 Jun 2007 -- v1.2
;;                Need to regexp-quote the searched-for string.

;;; Code:

(require 'custom)
(require 'etags)

;;; Custom stuff

;;;###autoload
(defgroup etags-select-mode nil
  "*etags select mode."
  :group 'etags)

;;;###autoload
(defcustom etags-select-no-select-for-one-match t
  "*If non-nil, don't open the selection window if there is only one
matching tag."
  :group 'etags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom etags-select-mode-hook nil
  "*List of functions to call on entry to etags-select-mode mode."
  :group 'etags-select-mode
  :type 'hook)

;;;###autoload
(defcustom etags-select-highlight-tag-after-jump t
  "*If non-nil, temporarily highlight the tag after you jump to it."
  :group 'etags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom etags-select-highlight-delay 1.0
  "*How long to highlight the tag."
  :group 'etags-select-mode
  :type 'number)

;;;###autoload
(defface etags-select-highlight-tag-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags."
  :group 'etags-select-mode)

;;;###autoload
(defcustom etags-select-use-short-name-completion nil
  "*Use short tag names during completion.  For example, say you
have a function named foobar in several classes and you invoke
`etags-select-find-tag'.  If this variable is nil, you would have
to type ClassA::foo<TAB> to start completion.  Since avoiding
knowing which class a function is in is the basic idea of this
package, if you set this to t you can just type foo<TAB>.

Only works with GNU Emacs."
  :group 'etags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom etags-select-go-if-unambiguous nil
  "*If non-nil, jump by tag number if it is unambiguous."
  :group 'etags-select-mode
  :type 'boolean)

 ;;; Variables

(defvar etags-select-buffer-name "*etags-select*"
  "etags-select buffer name.")

(defvar etags-select-mode-font-lock-keywords nil
  "etags-select font-lock-keywords.")

(defvar etags-select-source-buffer nil
  "etags-select source buffer tag was found from.")

(defvar etags-select-opened-window nil
  "etags-select opened a select window.")

(defconst etags-select-non-tag-regexp "\\(\\s-*$\\|In:\\|Finding tag:\\)"
  "etags-select non-tag regex.")

;;; Functions

(if (string-match "XEmacs" emacs-version)
    (fset 'etags-select-match-string 'match-string)
  (fset 'etags-select-match-string 'match-string-no-properties))

;; I use Emacs, but with a hacked version of XEmacs' etags.el, thus this variable

(defvar etags-select-use-xemacs-etags-p (fboundp 'get-tag-table-buffer)
  "Use XEmacs etags?")

(defun etags-select-case-fold-search ()
  "Get case-fold search."
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun etags-select-insert-matches (tagname tag-file tag-count)
  "Insert matches to tagname in tag-file."
  (let ((tag-table-buffer (etags-select-get-tag-table-buffer tag-file))
        (tag-file-path (file-name-directory tag-file))
        (tag-regex (concat "^.*?\\(" "\^?\\(.+[:.']" tagname "\\)\^A"
                           "\\|" "\^?" tagname "\^A"
                           "\\|" "\\<" tagname "[ \f\t()=,;]*\^?[0-9,]"
                           "\\)"))
        (case-fold-search (etags-select-case-fold-search))
        full-tagname tag-line filename current-filename)
    (set-buffer tag-table-buffer)
    (modify-syntax-entry ?_ "w")
    (goto-char (point-min))
    (while (search-forward tagname nil t)
      (beginning-of-line)
      (when (re-search-forward tag-regex (point-at-eol) 'goto-eol)
        (setq full-tagname (or (etags-select-match-string 2) tagname))
        (setq tag-count (1+ tag-count))
        (beginning-of-line)
        (re-search-forward "\\s-*\\(.*?\\)\\s-*\^?")
        (setq tag-line (etags-select-match-string 1))
        (end-of-line)
        (save-excursion
          (re-search-backward "\f")
          (re-search-forward "^\\(.*?\\),")
          (setq filename (etags-select-match-string 1))
          (unless (file-name-absolute-p filename)
            (setq filename (concat tag-file-path filename))))
        (save-excursion
          (set-buffer etags-select-buffer-name)
          (when (not (string= filename current-filename))
            (insert "\nIn: " filename "\n")
            (setq current-filename filename))
          (insert (int-to-string tag-count) " [" full-tagname "] " tag-line "\n"))))
    (modify-syntax-entry ?_ "_")
    tag-count))

(defun etags-select-get-tag-table-buffer (tag-file)
  "Get tag table buffer for a tag file."
  (if etags-select-use-xemacs-etags-p
      (get-tag-table-buffer tag-file)
    (visit-tags-table-buffer tag-file)
    (get-file-buffer tag-file)))

;;;###autoload
(defun etags-select-find-tag-at-point ()
  "Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (etags-select-find (find-tag-default)))

;;;###autoload
(defun etags-select-find-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (setq etags-select-source-buffer (buffer-name))
  (let* ((default (find-tag-default))
         (tagname (completing-read
                   (format "Find tag (default %s): " default)
                   'etags-select-complete-tag nil nil nil 'find-tag-history default)))
    (etags-select-find tagname)))

(defun etags-select-complete-tag (string predicate what)
  "Tag completion."
  (etags-select-build-completion-table)
  (if (eq what t)
      (all-completions string (etags-select-get-completion-table) predicate)
    (try-completion string (etags-select-get-completion-table) predicate)))

(defun etags-select-build-completion-table ()
  "Build tag completion table."
  (save-excursion
    (set-buffer etags-select-source-buffer)
    (let ((tag-files (etags-select-get-tag-files)))
      (mapcar (lambda (tag-file) (etags-select-get-tag-table-buffer tag-file)) tag-files))))

(defun etags-select-get-tag-files ()
  "Get tag files."
  (if etags-select-use-xemacs-etags-p
      (buffer-tag-table-list)
    (mapcar 'tags-expand-table-name tags-table-list)))

(defun etags-select-get-completion-table ()
  "Get the tag completion table."
  (if etags-select-use-xemacs-etags-p
      tag-completion-table
    (tags-completion-table)))

(defun etags-select-tags-completion-table-function ()
  "Short tag name completion."
  (let ((table (make-vector 16383 0))
        (tag-regex "^.*?\\(\^?\\(.+\\)\^A\\|\\<\\(.+\\)[ \f\t()=,;]*\^?[0-9,]\\)")
        (progress-reporter
         (make-progress-reporter
          (format "Making tags completion table for %s..." buffer-file-name)
          (point-min) (point-max))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at tag-regex)
          (intern (replace-regexp-in-string ".*[:.']" "" (or (match-string 2) (match-string 3))) table))
        (forward-line 1)
        (progress-reporter-update progress-reporter (point))))
    table))

(unless etags-select-use-xemacs-etags-p
  (defadvice etags-recognize-tags-table (after etags-select-short-name-completion activate)
    "Turn on short tag name completion (maybe)"
    (when etags-select-use-short-name-completion
      (setq tags-completion-table-function 'etags-select-tags-completion-table-function))))

(defun etags-select-find (tagname)
  "Core tag finding function."
  (let ((tag-files (etags-select-get-tag-files))
        (tag-count 0))
    (setq etags-select-source-buffer (buffer-name))
    (get-buffer-create etags-select-buffer-name)
    (set-buffer etags-select-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Finding tag: " tagname "\n")
    (mapcar (lambda (tag-file)
              (setq tag-count (etags-select-insert-matches tagname tag-file tag-count)))
            tag-files)
    (cond ((= tag-count 0)
           (message (concat "No matches for tag \"" tagname "\""))
           (ding))
          ((and (= tag-count 1) etags-select-no-select-for-one-match)
           (setq etags-select-opened-window nil)
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (etags-select-goto-tag))
          (t
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (set-buffer-modified-p nil)
           (setq buffer-read-only t)
           (setq etags-select-opened-window (selected-window))
           (select-window (split-window-vertically))
           (switch-to-buffer etags-select-buffer-name)
           (etags-select-mode tagname)))))

(defun etags-select-goto-tag (&optional arg other-window)
  "Goto the file/line of the tag under the cursor.
Use the C-u prefix to prevent the etags-select window from closing."
  (interactive "P")
  (let ((case-fold-search (etags-select-case-fold-search))
        tagname tag-point text-to-search-for filename filename-point (search-count 1))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Finding tag: \\(.*\\)$")
      (setq tagname (etags-select-match-string 1)))
    (beginning-of-line)
    (if (looking-at etags-select-non-tag-regexp)
        (message "Please put the cursor on a line with the tag.")
      (setq tag-point (point))
      (setq overlay-arrow-position (point-marker))
      (re-search-forward "\\]\\s-+\\(.+?\\)\\s-*$")
      (setq text-to-search-for (regexp-quote (etags-select-match-string 1)))
      (goto-char tag-point)
      (re-search-backward "^In: \\(.*\\)$")
      (setq filename (etags-select-match-string 1))
      (setq filename-point (point))
      (goto-char tag-point)
      (while (re-search-backward (concat "^.*?\\]\\s-+" text-to-search-for) filename-point t)
        (setq search-count (1+ search-count)))
      (goto-char tag-point)
      (unless arg
        (kill-buffer etags-select-buffer-name)
        (when etags-select-opened-window
          (delete-window (selected-window))
          (select-window etags-select-opened-window)))
      (switch-to-buffer etags-select-source-buffer)
      (if etags-select-use-xemacs-etags-p
          (push-tag-mark)
        (ring-insert find-tag-marker-ring (point-marker)))
      (if other-window
          (find-file-other-window filename)
        (find-file filename))
      (goto-char (point-min))
      (while (> search-count 0)
        (unless (re-search-forward (concat "^\\s-*" text-to-search-for) nil t)
          (message "TAGS file out of date ... stopping at closest match")
          (setq search-count 1))
        (setq search-count (1- search-count)))
      (beginning-of-line)
      (re-search-forward tagname)
      (goto-char (match-beginning 0))
      (when etags-select-highlight-tag-after-jump
        (etags-select-highlight (match-beginning 0) (match-end 0))))))

(defun etags-select-highlight (beg end)
  "Highlight a region temporarily."
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-property extent 'face 'etags-select-highlight-tag-face)
        (sit-for etags-select-highlight-delay)
        (delete-extent extent))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'etags-select-highlight-tag-face)
      (sit-for etags-select-highlight-delay)
      (delete-overlay ov))))

(defun etags-select-goto-tag-other-window (&optional arg)
  "Goto the file/line of the tag under the cursor in other window.
Use the C-u prefix to prevent the etags-select window from closing."
  (interactive "P")
  (etags-select-goto-tag arg t))

(defun etags-select-next-tag ()
  "Move to next tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (eobp))
    (forward-line))
  (while (and (looking-at etags-select-non-tag-regexp) (not (eobp)))
    (forward-line))
  (when (eobp)
    (ding)))

(defun etags-select-previous-tag ()
  "Move to previous tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (forward-line -1))
  (while (and (looking-at etags-select-non-tag-regexp) (not (bobp)))
    (forward-line -1))
  (when (bobp)
    (ding)))

(defun etags-select-quit ()
  "Quit etags-select buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun etags-select-by-tag-number (first-digit)
  "Select a tag by number."
  (let ((current-point (point)) tag-num)
    (if (and etags-select-go-if-unambiguous (not (re-search-forward (concat "^" first-digit) nil t 2)))
        (setq tag-num first-digit)
      (setq tag-num (read-from-minibuffer "Tag number? " first-digit)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" tag-num) nil t)
        (etags-select-goto-tag)
      (goto-char current-point)
      (message (concat "Couldn't find tag number " tag-num))
      (ding))))

;;; Keymap

(defvar etags-select-mode-map nil "'etags-select-mode' keymap.")
(if (not etags-select-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'etags-select-goto-tag)
      (define-key map [(meta return)] 'etags-select-goto-tag-other-window)
      (define-key map [(down)] 'etags-select-next-tag)
      (define-key map [(up)] 'etags-select-previous-tag)
      (define-key map "n" 'etags-select-next-tag)
      (define-key map "p" 'etags-select-previous-tag)
      (define-key map "q" 'etags-select-quit)
      (define-key map "0" (lambda () (interactive) (etags-select-by-tag-number "0")))
      (define-key map "1" (lambda () (interactive) (etags-select-by-tag-number "1")))
      (define-key map "2" (lambda () (interactive) (etags-select-by-tag-number "2")))
      (define-key map "3" (lambda () (interactive) (etags-select-by-tag-number "3")))
      (define-key map "4" (lambda () (interactive) (etags-select-by-tag-number "4")))
      (define-key map "5" (lambda () (interactive) (etags-select-by-tag-number "5")))
      (define-key map "6" (lambda () (interactive) (etags-select-by-tag-number "6")))
      (define-key map "7" (lambda () (interactive) (etags-select-by-tag-number "7")))
      (define-key map "8" (lambda () (interactive) (etags-select-by-tag-number "8")))
      (define-key map "9" (lambda () (interactive) (etags-select-by-tag-number "9")))
      (setq etags-select-mode-map map)))

;;; Mode startup

(defun etags-select-mode (tagname)
  "etags-select-mode is a mode for browsing through tags.\n\n
\\{etags-select-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'etags-select-mode)
  (setq mode-name "etags-select")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map etags-select-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq etags-select-mode-font-lock-keywords
        (list (list "^\\(Finding tag:\\)" '(1 font-lock-keyword-face))
              (list "^\\(In:\\) \\(.*\\)" '(1 font-lock-keyword-face) '(2 font-lock-string-face))
              (list "^[0-9]+ \\[\\(.+?\\)\\]" '(1 font-lock-type-face))
              (list tagname '(0 font-lock-function-name-face))))
  (setq font-lock-defaults '(etags-select-mode-font-lock-keywords))
  (setq overlay-arrow-position nil)
  (run-hooks 'etags-select-mode-hook))

(provide 'etags-select)
;;; etags-select.el ends here
