;;; anything-config.el --- Predefined configurations for `anything.el'

;; Filename: anything-config.el

;; Description: Predefined configurations for `anything.el'
;; Time-stamp: <2009-02-27 17:38:59 (JST) rubikitch>
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;;             Andy Stewart <lazycat.manatee@gmail.com>
;;             rubikitch    <rubikitch@ruby-lang.org>
;; Copyright (C) 2007 ~ 2009, Tassilo Horn, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2009-02-16 21:38:23
;; Version: 0.3.7
;; URL: http://www.emacswiki.org/emacs/download/anything-config.el
;; Keywords: anything, anything-config
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Predefined configurations for `anything.el'
;;
;; To configure anything you should setup `anything-sources'
;; with specify source, like below:
;;
;; (setq anything-sources
;;       (list anything-c-source-buffers
;;             anything-c-source-buffer-not-found
;;             anything-c-source-file-name-history
;;             anything-c-source-info-pages
;;             anything-c-source-info-elisp
;;             anything-c-source-man-pages
;;             anything-c-source-locate
;;             anything-c-source-emacs-commands
;;             ))
;;
;; Below are complete source list you can setup in `anything-sources':
;;
;;  Buffer:
;;     `anything-c-source-buffers'          (Buffers)
;;     `anything-c-source-buffer-not-found' (Create buffer)
;;  File:
;;     `anything-c-source-file-name-history'    (File Name History)
;;     `anything-c-source-files-in-current-dir' (Files from Current Directory)
;;     `anything-c-source-file-cache'           (File Cache)
;;     `anything-c-source-locate'               (Locate)
;;     `anything-c-source-recentf'              (Recentf)
;;     `anything-c-source-ffap-guesser'         (File at point)
;;  Help:
;;     `anything-c-source-man-pages'  (Manual Pages)
;;     `anything-c-source-info-pages' (Info Pages)
;;     `anything-c-source-info-elisp' (Info Elisp)
;;  Command:
;;     `anything-c-source-complex-command-history'  (Complex Command History)
;;     `anything-c-source-extended-command-history' (Emacs Commands History)
;;     `anything-c-source-emacs-commands'           (Emacs Commands)
;;     `anything-c-source-lacarte'                  (Lacarte)
;;  Function:
;;     `anything-c-source-emacs-functions'              (Emacs Functions)
;;     `anything-c-source-emacs-functions-with-abbrevs' (Emacs Functions)
;;  Bookmark:
;;     `anything-c-source-bookmarks'     (Bookmarks)
;;     `anything-c-source-bookmark-set'  (Set Bookmark)
;;     `anything-c-source-w3m-bookmarks' (W3m Bookmarks)
;;  Library:
;;     `anything-c-source-elisp-library-scan' (Elisp libraries (Scan))
;;  Programming:
;;     `anything-c-source-imenu'                              (Imenu)
;;     `anything-c-source-ctags'                              (Exuberant ctags)
;;     `anything-c-source-semantic'                           (Semantic Tags)
;;     `anything-c-source-simple-call-tree-functions-callers' (Function is called by)
;;     `anything-c-source-simple-call-tree-callers-functions' (Function calls)
;;  Color and Face:
;;     `anything-c-source-customize-face' (Customize Face)
;;     `anything-c-source-colors'         (Colors)
;;  Search Engine:
;;     `anything-c-source-tracker-search' (Tracker Search)
;;     `anything-c-source-mac-spotlight'  (mdfind)
;;  Kill ring:
;;     `anything-c-source-kill-ring' (Kill Ring)
;;  Register:
;;     `anything-c-source-register' (Registers)
;;  Headline Extraction:
;;     `anything-c-source-fixme'                   (TODO/FIXME/DRY comments)
;;     `anything-c-source-rd-headline'             (RD HeadLine)
;;     `anything-c-source-oddmuse-headline'        (Oddmuse HeadLine)
;;     `anything-c-source-emacs-source-defun'      (Emacs Source DEFUN)
;;     `anything-c-source-emacs-lisp-expectations' (Emacs Lisp Expectations)
;;     `anything-c-source-emacs-lisp-toplevels'    (Emacs Lisp Toplevel and Linkd Star)
;;     `anything-c-source-org-headline'            (Org HeadLine)
;;  Misc:
;;     `anything-c-source-picklist'           (Picklist)
;;     `anything-c-source-bbdb'               (BBDB)
;;     `anything-c-source-evaluation-result'  (Evaluation Result)
;;     `anything-c-source-calculation-result' (Calculation Result)
;;     `anything-c-source-google-suggest'     (Google Suggest)
;;     `anything-c-source-jabber-contacts'    (Jabber Contacts)
;;     `anything-c-source-call-source'        (Call anything source)
;;     `anything-c-source-occur'              (Occur)

;;; Change log:
;;
;; 2009/02/27
;;   * rubikitch:
;;      * Improve `anything-c-shorten-home-path'.
;;      * Fix bug of `anything-c-source-emacs-source-defun'.
;; 2009/02/25
;;   * rubikitch:
;;      * Fix bug in `anything-c-source-register' (thanks Thierry).
;; 2009/02/24
;;   * Andy Stewart:
;;      * Fix doc.
;;      * Improve `anything-c-elisp-library-scan-list'.
;;      * Fix bug of `anything-c-source-file-cache'.
;;      * Make "Send a mail" as default action of
;;        `anything-c-source-bbdb'.
;;      * Fix the bug of `anything-c-source-bbdb'
;;      * Suppress warning of `bm-lists' and `anything-headline-get-candidates'.
;;   * rubikitch:
;;      * Source list generator uses `align-regexp' now.
;;
;; 2009/02/23
;;   * rubikitch:
;;      * Add anything sexp comment to facilitate test.
;;      * source list generator.
;;      * New option `anything-c-boring-buffer-regexp'.
;;      * New option `anything-c-boring-file-regexp'.
;;      * New command `anything-call-source-from-anything'.
;;      * New plug-in `candidates-file'.
;;      * New plug-in `headline'.
;;      * New type attribute `file-line'.
;;      * New type attribute `line'.
;;      * Add many new sources.
;;      * Handle `kill-ring' properly by `anything-c-source-kill-ring'.
;;
;; 2009/02/20
;;   * Thierry Volpiatto:
;;      * Fix the bug of `anything-c-source-imenu'.
;;
;; 2009/02/16
;;   * Andy Stewart:
;;      * Fix the bug of `anything-c-source-imenu'.
;;      * Clean up code.
;;      * Add many new features.
;;      * Fix doc.
;;
;; 2007
;;      * First released.
;;

;;; Contributors:
;;
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen (see http://www.skamphausen.de for more informations)
;;     Drew Adams <drew.adams@oracle.com>
;;     Jason McBrayer <jmcbray@carcosa.net>
;;     Andy Stewart <lazycat.manatee@gmail.com>
;;     Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;     rubikitch <rubikitch@ruby-lang.org>
;;     Scott Vokes <vokes.s@gmail.com>
;;

;;; For Maintainers:
;;
;; Evaluate (anything-c-insert-summary) before commit. This function
;; generates anything-c-source-* list.
;;
;; [EVAL IT] (anything-c-insert-summary)
;;
;; Please write details documentation about function, then others will
;; read code more easier.   -- Andy Stewart
;;


;;; TODO
;;
;; - anything-c-adaptive stores infos for sources/types that don't have
;;   set it as `filtered-candidate-transformer'.
;;
;; - Fix documentation, now many functions haven't documentations.
;;

;;; Require
(require 'anything)
(require 'thingatpt)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup anything-config nil
  "Predefined configurations for `anything.el'."
  :group 'anything)

(defcustom anything-c-use-standard-keys nil
  "Whether use standard keybindings.
If non-nil the keybindings of anything will be the standard
bindings used in most parts of emacs, e.g. M-p/M-n for minibuffer
history, C-s for isearch, etc.

If it's nil anything uses some bindings that don't conflict with
`iswitchb', e.g. C-p/C-n for the minibuffer history.  If you use
`iswitchb' you probably should say nil here."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-adaptive-history-file "~/.emacs.d/anything-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'anything-config)

(defcustom anything-c-google-suggest-url
  "http://www.google.com/complete/search?hl=en&js=true&qu="
  "URL used for looking up suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-boring-buffer-regexp
  (rx (or
       (group bos  " ")
       ;; anything-buffer
       "*anything"
       ;; echo area
       " *Echo Area" " *Minibuf"))
  "The regexp that match boring buffers.
Buffer candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-buffers' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-buffers' is used."
  :type 'string
  :group 'anything-config)
;; (string-match anything-c-boring-buffer-regexp "buf")
;; (string-match anything-c-boring-buffer-regexp " hidden")
;; (string-match anything-c-boring-buffer-regexp " *Minibuf-1*")

(defcustom anything-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and line-start  ".#")
       (and (or ".class" ".la" ".o" "~") eol)))
  "The regexp that match boring files.
File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used."
  :type 'string
  :group 'anything-config)

(defcustom anything-kill-ring-threshold 10
  "*Minimum length to be listed by `anything-c-source-kill-ring'."
  :type 'integer
  :group 'anything-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-insert-buffer-name ()
  "Insert buffer name."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (if buffer-file-name (file-name-nondirectory buffer-file-name)
       (buffer-name)))))

(defun anything-insert-symbol ()
  "Insert current symbol."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (save-excursion
       (buffer-substring (beginning-of-thing 'symbol)
                         (end-of-thing 'symbol))))))

(defun anything-insert-selection ()
  "Insert current selection."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (anything-get-selection))))

(defun anything-show-buffer-only ()
  "Only show sources about buffer."
  (interactive)
  (anything-set-source-filter '("Buffers")))

(defun anything-show-bbdb-only ()
  "Only show sources about BBDB."
  (interactive)
  (anything-set-source-filter '("BBDB")))

(defun anything-show-locate-only ()
  "Only show sources about Locate."
  (interactive)
  (anything-set-source-filter '("Locate")))

(defun anything-show-info-only ()
  "Only show sources about Info."
  (interactive)
  (anything-set-source-filter '("Info Pages"
                                "Info Elisp"
                                "Info Common-Lisp")))

(defun anything-show-imenu-only ()
  "Only show sources about Imenu."
  (interactive)
  (anything-set-source-filter '("Imenu")))

(defun anything-show-files-only ()
  "Only show sources about File."
  (interactive)
  (anything-set-source-filter '("File Name History"
                                "Files from Current Directory"
                                "Recentf")))

(defun anything-show-w3m-bookmarks-only ()
  "Only show source about w3m bookmark."
  (interactive)
  (anything-set-source-filter '("W3m Bookmarks")))

(defun anything-show-colors-only ()
  "Only show source about color."
  (interactive)
  (anything-set-source-filter '("Colors"
                                "Customize Faces")))

(defun anything-show-kill-ring-only ()
  "Only show source about kill ring."
  (interactive)
  (anything-set-source-filter '("Kill Ring")))

(defun anything-test-sources ()
  "Insert source to buffer `*scratch*' for test."
  (interactive)
  (switch-to-buffer "*scratch*")
  (goto-char (point-max))
  (insert "\n")
  (save-excursion
    (insert
     "(setq anything-sources-old anything-sources)\n"
     "(setq anything-sources anything-sources-old)\n"
     (mapconcat (lambda (sym) (format "(setq anything-sources (list %s))" sym))
                (apropos-internal "^anything-c-source" #'boundp)
                "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-nest (&rest same-as-anything)
  "Nested `anything'. If you use `anything' within `anything', use it."
  (with-selected-window (anything-window)
    (let (anything-current-position
          anything-current-buffer
          (orig-anything-buffer anything-buffer)
          anything-pattern
          anything-buffer
          anything-sources
          anything-compiled-sources
          anything-buffer-chars-modified-tick
          (anything-samewindow t)
          (enable-recursive-minibuffers t))
      (unwind-protect
          (apply #'anything same-as-anything)
        (anything-initialize-overlays orig-anything-buffer)
        (add-hook 'post-command-hook 'anything-check-minibuffer-input)))))

(defun anything-displaying-source-names ()
  "Display sources name."
  (with-current-buffer anything-buffer
    (goto-char (point-min))
    (loop with pos
          while (setq pos (next-single-property-change (point) 'anything-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

(defun anything-select-source ()
  "Select source."
  (interactive)
  (let ((default (assoc-default 'name (anything-get-current-source)))
        (source-names (anything-displaying-source-names))
        (all-source-names (mapcar (lambda (s) (assoc-default 'name s))
                                  (anything-get-sources))))
    (setq anything-candidate-number-limit 9999)
    (anything-aif
        (let (anything-source-filter)
          (anything-nest '(((name . "Anything Source")
                            (candidates . source-names)
                            (action . identity))
                           ((name . "Anything Source (ALL)")
                            (candidates . all-source-names)
                            (action . identity)))
                         nil "Source: " nil
                         default "*anything select source*"))
        (anything-set-source-filter (list it))
      (anything-set-source-filter nil))))

(defun anything-insert-string (str)
  "Insert STR."
  (delete-minibuffer-contents)
  (insert str))

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' match the filename (without directory part) of CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' match the directory part of CANDIDATE (a file)."
  (let ((dir (file-name-directory candidate)))
    (when dir
      (string-match anything-pattern dir))))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

(defun anything-c-compose (arg-lst func-lst)
  "Call each function in FUNC-LST with the arguments specified in ARG-LST.
The result of each function will be the new `car' of ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun anything-c-skip-entries (list regexp)
  "Remove entries which matches REGEXP from LIST."
  (remove-if (lambda (x) (and (stringp x) (string-match regexp x)))
             list))

(defun anything-c-shadow-entries (list regexp)
  "Elements of LIST matching REGEXP will be displayed with the `file-name-shadow' face if available."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                          ;; fall back to default on XEmacs
                          'default)))
              (if (string-match regexp file)
                  (setq file (propertize file 'face face))))
            file)
          list))

(defsubst anything-c-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
    (symbol-name str-or-sym)))

(defsubst anything-c-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Prefix argument in action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
(defvar anything-current-prefix-arg nil
  "`current-prefix-arg' when selecting action.
It is cleared after executing action.")

(defadvice anything-exit-minibuffer (before anything-current-prefix-arg activate)
  (unless anything-current-prefix-arg
    (setq anything-current-prefix-arg current-prefix-arg)))

(add-hook 'anything-after-action-hook
          (lambda () (setq anything-current-prefix-arg nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Document Generator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-c-create-summary ()
  "Create `anything' summary."
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "^;;;; <\\(.+?\\)>$\\|^;; (anything '\\(.+?\\))$"  nil t)
          collect (if (match-beginning 1)
                      (cons 'section (match-string-no-properties 1))
                    (cons 'source
                          (cons (match-string-no-properties 2)
                                (assoc-default 'name (symbol-value (intern (match-string-no-properties 2))))))))))
;; (find-epp (anything-c-create-summary))

(defun anything-c-insert-summary ()
  "Insert `anything' summary."
  (save-excursion
    (goto-char (point-min))
    (search-forward ";; Below are complete source list you can setup in")
    (forward-line 1)
    (delete-region (point)
                   (progn (search-forward ";;; Change log:" nil t)
                          (forward-line -1) (point)))
    (insert ";;\n")
    (loop with beg
          for (kind . value) in (anything-c-create-summary)
          for i from 0
          do (cond ((eq kind 'section)
                    (unless (zerop i)
                      (align-regexp beg (point) "\\(\\s-*\\)(" 1 1 nil))
                    (insert ";;  " value ":\n")
                    (setq beg (point)))
                   (t
                    (insert ";;     `" (car value) "'    (" (cdr value) ")\n")))
          finally (align-regexp beg (point) "\\(\\s-*\\)(" 1 1 nil))))
;; (anything-c-insert-summary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything Sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <Buffer>
(defun anything-c-buffer-list ()
  "Return the list of names of buffers with boring buffers filtered out.
Boring buffers is specified by `anything-c-boring-buffer-regexp'.
The first buffer in the list will be the last recently used
buffer that is not the current buffer."
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (append (cdr buffers) (list (car buffers)))))

(defvar anything-c-source-buffers
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (volatile)
    (type . buffer)))
;; (anything 'anything-c-source-buffers)

(defvar anything-c-source-buffer-not-found
  '((name . "Create buffer")
    (dummy)
    (type . buffer)))
;; (anything 'anything-c-source-buffer-not-found)

;;;; <File>
;;; File name history
(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))
;; (anything 'anything-c-source-file-name-history)

;;; Files in current dir
(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (init . (lambda ()
              (setq anything-c-default-directory
                    default-directory)))
    (candidates . (lambda ()
                    (directory-files
                     anything-c-default-directory)))
    (volatile)
    (type . file)))
;; (anything 'anything-c-source-files-in-current-dir)

;;; File Cache
(defvar anything-c-source-file-cache-initialized nil)

(defvar anything-c-file-cache-files nil)

(defvar anything-c-source-file-cache
  '((name . "File Cache")
    (init . (lambda ()
              (require 'filecache nil t)
              (unless anything-c-source-file-cache-initialized
                (setq anything-c-file-cache-files
                      (loop for item in file-cache-alist append
                            (destructuring-bind (base &rest dirs) item
                              (loop for dir in dirs collect
                                    (concat dir base)))))
                (defadvice file-cache-add-file (after file-cache-list activate)
                  (add-to-list 'anything-c-file-cache-files (expand-file-name file)))
                (setq anything-c-source-file-cache-initialized t))))
    (candidates . anything-c-file-cache-files)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))
;; (anything 'anything-c-source-file-cache)

;;; Locate
(defvar anything-c-locate-options
  (cond
   ((eq system-type 'darwin) '("locate"))
   ((eq system-type 'berkeley-unix) '("locate" "-i"))
   (t '("locate" "-i" "-r")))
  "A list where the `car' is the name of the locat program followed by options.
The search pattern will be appended, so the
\"-r\" option should be the last option.")

(defvar anything-c-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (apply 'start-process "locate-process" nil
                           (append anything-c-locate-options
                                   (list anything-pattern)))))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern with locate.")
;; (anything 'anything-c-source-locate)

;;; Recentf files
(defvar anything-c-source-recentf
  '((name . "Recentf")
    (candidates . recentf-list)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file))
  "See (info \"(emacs)File Conveniences\").")
;; (anything 'anything-c-source-recentf)

;;; ffap
(defvar anything-c-source-ffap-guesser
  '((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (let ((guess (with-current-buffer anything-current-buffer
                                   (ffap-guesser))))
                      (if guess (list guess)))))
    (type . file)))
;; (anything 'anything-c-source-ffap-guesser)

;;;; <Help>
;;; Man Pages
(defvar anything-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if anything-c-man-pages
                        anything-c-man-pages
                      ;; XEmacs doesn't have a woman :)
                      (setq anything-c-man-pages
                            (condition-case nil
                                (progn
                                  (require 'woman)
                                  (woman-file-name "")
                                  (sort (mapcar 'car
                                                woman-topic-all-completions)
                                        'string-lessp))
                              (error nil))))))
    (action . (("Show with Woman" . woman)))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-man-pages)

;;; Info pages
(defvar anything-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-info-pages
  `((name . "Info Pages")
    (candidates . (lambda ()
                    (if anything-c-info-pages
                        anything-c-info-pages
                      (setq anything-c-info-pages
                            (save-window-excursion
                              (save-excursion
                                (require 'info)
                                (Info-find-node "dir" "top")
                                (goto-char (point-min))
                                (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
                                      topics)
                                  (while (re-search-forward info-topic-regexp nil t)
                                    (add-to-list 'topics (match-string-no-properties 1)))
                                  (goto-char (point-min))
                                  (Info-exit)
                                  topics)))))))
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string "^[^:]+: "
                                                                    ""
                                                                    node-str))))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-pages)

;; Info Elisp
(defvar anything-c-info-elisp nil)
(defvar anything-c-source-info-elisp
  `((name . "Info Elisp")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-elisp
                  (with-temp-buffer
                    (Info-find-node "elisp" "Index")
                    (setq anything-c-info-elisp (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-elisp
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "elisp" "Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (volatile)
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-elisp)

;; Info-Common-Lisp
(defvar anything-c-info-cl-fn nil)
(defvar anything-c-source-info-cl
  `((name . "Info Common-Lisp")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-cl-fn
                  (with-temp-buffer
                    (Info-find-node "cl" "Function Index")
                    (setq anything-c-info-cl-fn (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-cl-fn
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (("Goto Info Node" . (lambda (candidate)
                                     (Info-find-node "cl" "Function Index")
                                     (Info-index (replace-regexp-in-string "* " "" candidate))))
               ("Find Example" . (lambda (candidate)
                                   (and (fboundp 'traverse-deep-rfind)
                                        (traverse-deep-rfind traverse-example-directory
                                                             (replace-regexp-in-string "* " "" candidate)
                                                             ".el"))))))
    (volatile)
    (requires-pattern . 2)))

;;;; <Command>
;;; Complex command history
(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda ()
                    (mapcar 'prin1-to-string
                            command-history)))
    (volatile)
    (type . sexp)))
;; (anything 'anything-c-source-complex-command-history)

;;; M-x history
(defvar anything-c-source-extended-command-history
  '((name . "Emacs Commands History")
    (candidates . extended-command-history)
    (type . command)))
;; (anything 'anything-c-source-extended-command-history)

;;; Emacs commands
(defvar anything-c-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (volatile)
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands.
A command is a function with interactive spec that can
be invoked with `M-x'.

To get non-interactive functions listed, use
`anything-c-source-emacs-functions'.")
;; (anything 'anything-c-source-emacs-commands)

;;; LaCarte
(defvar anything-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda ()
              (require 'lacarte )))
    (candidates . (lambda ()
                    (delete '(nil) (lacarte-get-overall-menu-item-alist))))
    (candidate-number-limit . 9999)
    (action . anything-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")
;; (anything 'anything-c-source-lacarte)

;;;; <Function>
;;; Emacs functions
(defvar anything-c-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (functionp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (volatile)
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")
;; (anything 'anything-c-source-emacs-functions)

;;; With abbrev expansion
;;; Similar to my exec-abbrev-cmd.el
;;; See http://www.tsdh.de/cgi-bin/wiki.pl/exec-abbrev-cmd.el
(defvar anything-c-function-abbrev-regexp nil
  "The regexp for `anything-c-source-emacs-functions-with-abbrevs'.
Regexp built from the current `anything-pattern' interpreting it
as abbreviation.
Only for internal use.")

(defun anything-c-match-function-by-abbrev (candidate)
  "Return non-nil if `anything-pattern' is an abbreviation of the function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `switch-to-buffer'."
  (string-match anything-c-function-abbrev-regexp candidate))

(defvar anything-c-source-emacs-functions-with-abbrevs
  (append anything-c-source-emacs-functions
          '((match . (anything-c-match-function-by-abbrev
                      anything-c-string-match)))
          '((init . (lambda ()
                      (defadvice anything-update
                        (before anything-c-update-function-abbrev-regexp activate)
                        (let ((char-list (append anything-pattern nil))
                              (str "^"))
                          (dolist (c char-list)
                            (setq str (concat str (list c) "[^-]*-")))
                          (setq str (concat (substring str 0 (1- (length str))) "$"))
                          (setq anything-c-function-abbrev-regexp str))))))))
;; (anything 'anything-c-source-emacs-functions-with-abbrevs)

;;;; <Bookmark>
;;; Bookmarks
(defvar anything-c-source-bookmarks
  '((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks)

;;; bookmark-set
(defvar anything-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set)))
;; (anything 'anything-c-source-bookmark-set)

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
;; modified by me
(defvar anything-c-source-bm
  '((name . "Visible Bookmarks")
    (init . anything-c-source-bm-init)
    (candidates-in-buffer)
    (type . line)))

(defun anything-c-source-bm-init ()
  "Init function for `anything-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (anything-candidate-buffer 'global)))
        (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                           '< :key 'overlay-start))
          (let ((start (overlay-start bm))
                (end (overlay-end bm))
                (annotation (or (overlay-get bm 'annotation) "")))
            (unless (< (- end start) 1) ; org => (if (< (- end start) 2)
              (let ((str (format "%7d: [%s]: %s\n"
                                 (line-number-at-pos start)
                                 annotation
                                 (buffer-substring start (1- end)))))
                (with-current-buffer buf (insert str))))))))))


;; W3m bookmark
(unless (and (require 'w3m nil t)
             (require 'w3m-bookmark nil t))
  (defvar w3m-bookmark-file "~/.w3m/bookmark.html"))
(defvar anything-w3m-bookmarks-regexp ">[^><]+[^</a>]+[a-z)0-9]+")
(defun anything-w3m-bookmarks-to-alist ()
  "Translate w3m bookmark to alist."
  (let ((bookmarks-alist)
        (url)
        (title))
    (with-temp-buffer
      (insert-file-contents w3m-bookmark-file)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (when (re-search-forward "href=" nil t)
          (beginning-of-line)
          (when (re-search-forward "http://[^>]*" nil t)
            (setq url (concat "\"" (match-string 0))))
          (beginning-of-line)
          (when (re-search-forward anything-w3m-bookmarks-regexp nil t)
            (setq title (match-string 0)))
          (push (cons title url) bookmarks-alist))))
    (setq bookmarks-alist (reverse bookmarks-alist))))


(defun anything-c-w3m-bookmarks-get-value (elm)
  "Get value of w3m bookmark element ELM."
  (let ((value
         (replace-regexp-in-string "\"" ""
                                   (cdr (assoc elm
                                               anything-c-w3m-bookmarks-alist)))))
    value))

(defun anything-c-w3m-browse-bookmark (elm &optional use-firefox new-tab)
  "Browse w3m bookmark element ELM.
If USE-FIREFOX is non-nil, use firefox browse.
If NEW-TAB is non-nil, browse page in new tab."
  (let* ((fn (if use-firefox
                 'browse-url-firefox
               'w3m-browse-url))
         (arg (if (and (eq fn 'w3m-browse-url)
                       new-tab)
                  t
                nil)))
    (funcall fn (anything-c-w3m-bookmarks-get-value elm) arg)))

(defun anything-c-highlight-w3m-bookmarks (books)
  "Highlight w3m bookmark with BOOKS."
  (let ((cand-mod (loop for i in books
                        collect (propertize i
                                            'face 'underline))))
    cand-mod))

(defun anything-c-w3m-delete-bookmark (elm)
  "Delete w3m bookmark element ELM."
  (save-excursion
    (find-file-literally w3m-bookmark-file)
    (goto-char (point-min))
    (when (re-search-forward elm nil t)
      (beginning-of-line)
      (delete-region (point)
                     (line-end-position))
      (delete-blank-lines))
    (save-buffer (current-buffer))
    (kill-buffer (current-buffer))))

(defun anything-c-w3m-rename-bookmark (elm)
  "Rename w3m bookmark element ELM."
  (let* ((old-title (replace-regexp-in-string ">" "" elm))
         (new-title (read-string "NewTitle: " old-title)))
    (save-excursion
      (find-file-literally w3m-bookmark-file)
      (goto-char (point-min))
      (when (re-search-forward (concat elm "<") nil t)
        (goto-char (1- (point)))
        (delete-backward-char (length old-title))
        (insert new-title))
      (save-buffer (current-buffer))
      (kill-buffer (current-buffer)))))

(defvar anything-c-w3m-bookmarks-alist nil)
(defvar anything-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq anything-c-w3m-bookmarks-alist
                    (anything-w3m-bookmarks-to-alist))))
    (candidates . (lambda ()
                    (mapcar #'car
                            anything-c-w3m-bookmarks-alist)))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (anything-c-compose
                                         (list candidates)
                                         '(anything-c-highlight-w3m-bookmarks))))
    (action . (("Browse Url" . (lambda (candidate)
                                 (if (> (length (w3m-list-buffers)) 1)
                                     (progn
                                       (set-buffer anything-current-buffer)
                                       (anything-c-w3m-browse-bookmark candidate))
                                   (anything-c-w3m-browse-bookmark candidate))))
               ("Copy Url" . (lambda (elm)
                               (kill-new (anything-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Firefox" . (lambda (candidate)
                                         (anything-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark" . (lambda (candidate)
                                      (anything-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark" . (lambda (candidate)
                                      (anything-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (anything-c-w3m-browse-bookmark candidate t)
                             (anything-c-w3m-browse-bookmark candidate nil t))))
    (delayed)
    (volatile)))
;; (anything 'anything-c-source-w3m-bookmarks)

;;;; <Library>
;;; Elisp library scan
(defvar anything-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (anything-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action . (("Find library" . (lambda (candidate)
                                   (find-file (find-library-name candidate))))
               ("Find library other window" . (lambda (candidate)
                                                (find-file-other-window (find-library-name candidate))))
               ("Load library" . (lambda (candidate)
                                   (load-library candidate)))))))
;; (anything 'anything-c-source-elisp-library-scan)

(defun anything-c-elisp-library-scan-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (anything-c-elisp-library-scan-list)))
    (with-current-buffer anything-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun anything-c-elisp-library-scan-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        ;; File regexp that suffix match `load-file-rep-suffixes'.
        (match-regexp (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match match-regexp name)
              (add-to-list 'names name)))))
    names))

;;;; <Programming>
;;; Imenu
(defvar anything-c-imenu-delimiter " / ")

(defvar anything-c-imenu-index-filter nil)
(make-variable-buffer-local 'anything-c-imenu-index-filter)

(defvar anything-c-cached-imenu-alist nil)
(make-variable-buffer-local 'anything-c-cached-imenu-alist)

(defvar anything-c-cached-imenu-candidates nil)
(make-variable-buffer-local 'anything-c-cached-imenu-candidates)

(defvar anything-c-cached-imenu-tick nil)
(make-variable-buffer-local 'anything-c-cached-imenu-tick)

(setq imenu-auto-rescan t)

(defun anything-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan (lambda (sub)
                (if (consp (cdr sub))
                    (mapcar
                     (lambda (subentry)
                       (concat (car entry) anything-c-imenu-delimiter subentry))
                     (anything-imenu-create-candidates sub))
                  (list (concat (car entry) anything-c-imenu-delimiter (car sub)))))
              (cdr entry))
    (list entry)))

(defvar anything-c-source-imenu
  '((name . "Imenu")
    (init . (lambda ()
              (setq anything-c-imenu-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (with-current-buffer anything-c-imenu-current-buffer
                      (let ((tick (buffer-modified-tick)))
                        (if (eq anything-c-cached-imenu-tick tick)
                            anything-c-cached-imenu-candidates
                          (setq imenu--index-alist nil)
                          (setq anything-c-cached-imenu-tick tick
                                anything-c-cached-imenu-candidates
                                (condition-case nil
                                    (mapcan
                                     'anything-imenu-create-candidates
                                     (setq anything-c-cached-imenu-alist
                                           (let ((index (imenu--make-index-alist)))
                                             (if anything-c-imenu-index-filter
                                                 (funcall anything-c-imenu-index-filter index)
                                               index))))
                                  (error nil)))
                          (setq anything-c-cached-imenu-candidates
                                (mapcar #'(lambda (x)
                                            (if (stringp x)
                                                x
                                              (car x)))
                                        anything-c-cached-imenu-candidates)))))))
    (volatile)
    (persistent-action . (lambda (elm)
                           (anything-c-imenu-default-action elm)
                           (if (fboundp 'anything-traverse-occur-color-current-line)
                               (anything-traverse-occur-color-current-line))))
    (action . (lambda (elm)
                (anything-c-imenu-default-action elm)))))
;; (anything 'anything-c-source-imenu)

(defun anything-c-imenu-default-action (elm)
  "The default action for `anything-c-source-imenu'."
  (let ((path (split-string elm anything-c-imenu-delimiter))
        (alist anything-c-cached-imenu-alist))
    (if (> (length path) 1)
        (progn
          (setq alist (assoc (car path) alist))
          (setq elm (cadr path))
          (imenu (assoc elm alist)))
      (imenu (assoc elm alist)))))

;;; Ctags
(defvar anything-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
            makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
            scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun anything-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode anything-c-ctags-modes)
             (anything-current-buffer-is-modified))
    (with-current-buffer (anything-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" anything-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) " anything-buffer-file-name)
         (format "ctags -e -u -f- --fields=n %s " anything-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring lineno-start (1- (search-forward "," (point-at-eol) t)))
            do
            (beginning-of-line)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar anything-c-source-ctags
  '((name . "Exuberant ctags")
    (init
     . anything-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line)))
;; (anything 'anything-c-source-ctags)

;; Semantic
(defun anything-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply 'append (mapcar (lambda (tag)
                             (if (listp tag)
                                 (let ((type (semantic-tag-type tag))
                                       (class (semantic-tag-class tag)))
                                   (if (or (and (stringp type)
                                                (string= type "class"))
                                           (eq class 'function)
                                           (eq class 'variable))
                                       (cons (cons (concat (make-string (* depth 2) ?\s)
                                                           (semantic-format-tag-summarize tag nil t)) tag)
                                             (anything-semantic-construct-candidates (semantic-tag-components tag) (1+ depth)))))))
                           tags))))

(defvar anything-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq anything-semantic-candidates
                    (condition-case nil
                        (anything-semantic-construct-candidates (semantic-fetch-tags) 0)
                      (error nil)))))
    (candidates . (lambda ()
                    (if anything-semantic-candidates
                        (mapcar 'car anything-semantic-candidates))))
    (action . (("Goto tag" . (lambda (candidate)
                               (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
                                 (semantic-go-to-tag tag))))))))
;; (anything 'anything-c-source-semantic)

;;; Function is called by
(defvar anything-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . (lambda ()
              (require 'simple-call-tree)
              (when (anything-current-buffer-is-modified)
                (simple-call-tree-analyze)
                (let ((list (simple-call-tree-invert simple-call-tree-alist)))
                  (with-current-buffer (anything-candidate-buffer 'local)
                    (dolist (entry list)
                      (let ((callers (mapconcat #'identity (cdr entry) ", ")))
                        (insert (car entry) " is called by "
                                (if (string= callers "")
                                    "no functions."
                                  callers)
                                ".\n"))))))))
    (delayed)
    (candidates-in-buffer))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el
")
;; (anything 'anything-c-source-simple-call-tree-functions-callers)

;;; Function calls
(defvar anything-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . (lambda ()
              (require 'simple-call-tree)
              (when (anything-current-buffer-is-modified)
                (simple-call-tree-analyze)
                (let ((list simple-call-tree-alist))
                  (with-current-buffer (anything-candidate-buffer 'local)
                    (dolist (entry list)
                      (let ((functions (mapconcat #'identity (cdr entry) ", ")))
                        (insert (car entry) " calls "
                                (if (string= functions "")
                                    "no functions"
                                  functions)
                                ".\n"))))))))
    (delayed)
    (candidates-in-buffer))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el
")
;; (anything 'anything-c-source-simple-call-tree-callers-functions)

;;;; <Color and Face>
;;; Customize Face
(defvar anything-c-source-customize-face
  '((name . "Customize Face")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (save-window-excursion (list-faces-display))
                (anything-candidate-buffer (get-buffer "*Faces*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line))))))
    (requires-pattern . 3)))
;; (anything 'anything-c-source-customize-face)

;; Color
(defvar anything-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (anything-candidate-buffer)
                         (save-window-excursion (list-colors-display))
                         (anything-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (("Copy Name" . (lambda (candidate)
                                (kill-new (anything-c-colors-get-name candidate))))
               ("Copy RGB" . (lambda (candidate)
                               (kill-new (anything-c-colors-get-rgb candidate))))
               ("Insert Name" . (lambda (candidate)
                                  (with-current-buffer anything-current-buffer
                                    (insert (anything-c-colors-get-name candidate)))))
               ("Insert RGB" . (lambda (candidate)
                                 (with-current-buffer anything-current-buffer
                                   (insert (anything-c-colors-get-rgb candidate)))))))
    (requires-pattern . 3)))
;; (anything 'anything-c-source-colors)

(defun anything-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (kill-line)
     (buffer-string))))

(defun anything-c-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (kill-region (point) (point-min))
     (buffer-string))))

;;;; <Search Engine>
;;; Tracker desktop search
(defvar anything-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")
;; (anything 'anything-c-source-tracker-search)

;;; Spotlight (MacOS X desktop search)
(defvar anything-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
                                   "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")
;; (anything 'anything-c-source-mac-spotlight)

;;;; <Kill ring>
;;; Kill ring
(defvar anything-c-source-kill-ring
  '((name . "Kill Ring")
    (init . (lambda () (anything-attrset 'last-command last-command)))
    (candidates . (lambda ()
                    (loop for kill in kill-ring
                          unless (or (< (length kill) anything-kill-ring-threshold)
                                     (string-match "^[\\s\\t]+$" kill))
                          collect kill)))
    (action . anything-c-kill-ring-action)
    (last-command)
    (migemo)
    (multiline)))

(defun anything-c-kill-ring-action (str)
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (anything-attr 'last-command) 'yank))
      (insert-for-yank str)
    ;; from `yank-pop'
    (let ((inhibit-read-only t)
          (before (< (point) (mark t))))
      (if before
          (funcall (or yank-undo-function 'delete-region) (point) (mark t))
        (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (insert-for-yank str)
      ;; Set the window start back where it was in the yank command,
      ;; if possible.
      (set-window-start (selected-window) yank-window-start t)
      (if before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))
  (kill-new str))

;; (anything 'anything-c-source-kill-ring)

;;;; <Register>
;;; Insert from register
(defvar anything-c-source-register
  '((name . "Registers")
    (candidates . anything-c-registers)
    (action ("insert" . insert))))

;; based on list-register.el
(defun anything-c-registers ()
  (loop for (char . val) in register-alist
        collect
        (let ((key (single-key-description char))
              (string (cond
                       ((numberp val)
                        (int-to-string val))
                       ((markerp val)
                        (let ((buf (marker-buffer val)))
                          (if (null buf)
                              "a marker in no buffer"
                            (concat
                             "a buffer position:"
                             (buffer-name buf)
                             ", position "
                             (int-to-string (marker-position val))))))
                       ((and (consp val) (window-configuration-p (car val)))
                        "conf:a window configuration.")
                       ((and (consp val) (frame-configuration-p (car val)))
                        "conf:a frame configuration.")
                       ((and (consp val) (eq (car val) 'file))
                        (concat "file:"
                                (prin1-to-string (cdr val))
                                "."))
                       ((and (consp val) (eq (car val) 'file-query))
                        (concat "file:a file-query reference: file "
                                (car (cdr val))
                                ", position "
                                (int-to-string (car (cdr (cdr val))))
                                "."))
                       ((consp val)
                        (let ((lines (format "%4d" (length val))))
                          (format "%s: %s\n" lines
                                  (truncate-string-to-width
                                   (mapconcat 'identity (list (car val))
;;                                   (mapconcat (lambda (y) y) val
                                              "^J") (- (window-width) 15)))))
                       ((stringp val)
                        (anything-c-string-no-properties val))
                       (t
                        "GARBAGE!"))))
          (cons (format "register %3s: %s" key string) string))))

(defun anything-c-string-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)

;; (anything 'anything-c-source-register)

;;;; <Headline Extraction>
(defvar anything-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter)))
;; (anything 'anything-c-source-fixme)

(defvar anything-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1)))
;; (anything 'anything-c-source-rd-headline)

(defvar anything-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
               "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1)))
;; (anything 'anything-c-source-oddmuse-headline)


(defvar anything-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$" (or buffer-file-name "")))))
;; (anything 'anything-c-source-emacs-source-defun)

(defvar anything-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc \\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode))))
;; (anything 'anything-c-source-emacs-lisp-expectations)

(defvar anything-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel and Linkd Star")
    (headline . "^(\\|(@\\*")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust)))
;; (anything 'anything-c-source-emacs-lisp-toplevels)

(defvar anything-c-source-org-headline
  '((name . "Org HeadLine")
    (headline  "^\\* \\(.+\\)$"
               "^\\*\\* \\(.+\\)$"
               "^\\*\\*\\* \\(.+\\)$"
               "^\\*\\*\\*\\* \\(.+\\)$"
               "^\\*\\*\\*\\*\\* \\(.+\\)$"
               "^\\*\\*\\*\\*\\*\\* \\(.+\\)$"
               "^\\*\\*\\*\\*\\*\\*\\* \\(.+\\)$"
               "^\\*\\*\\*\\*\\*\\*\\*\\* \\(.+\\)$")
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)))
;; (anything 'anything-c-source-org-headline)

;;;; <Misc>
;;; Picklist
(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda ()
                    (mapcar 'car picklist-list)))
    (volatile)
    (type . file)))
;; (anything 'anything-c-source-picklist)

;;; BBDB
(defun anything-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun anything-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `anything-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " anything-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
    actions))

(defun anything-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar anything-c-bbdb-name nil
  "Only for internal use.")

(defvar anything-c-source-bbdb
  '((name . "BBDB")
    (candidates . anything-c-bbdb-candidates)
    (volatile)
    (action ("Send a mail" . (lambda (candidate)
                               (bbdb-send-mail (anything-c-bbdb-get-record candidate))))
            ("View person's data" . (lambda (candidate)
                                      (bbdb-redisplay-one-record (anything-c-bbdb-get-record candidate)))))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq anything-c-bbdb-name anything-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                          candidates)))
    (action-transformer . (lambda (actions candidate)
                            (anything-c-bbdb-create-contact actions candidate)))))
;; (anything 'anything-c-source-bbdb)

;;; Evaluation Result
(defvar anything-c-source-evaluation-result
  '((name . "Evaluation Result")
    (requires-pattern)
    (match (lambda (candidate) t))
    (candidates  "dummy")
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (prin1-to-string
                                              (eval (read anything-pattern)))
                                           (error "Error")))))
    (volatile)
    (action ("Do Nothing" . ignore))))
;; (anything 'anything-c-source-evaluation-result)

;;; Calculation Result
(defvar anything-c-source-calculation-result
  '((name . "Calculation Result")
    (requires-pattern)
    (match (lambda (candidate) t))
    (candidates  "dummy")
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (calc-eval anything-pattern)
                                           (error "error")))))
    (volatile)
    (action ("Do Nothing" . ignore))))
;; (anything 'anything-c-source-calculation-result)

;;; Google Suggestions
(defvar anything-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . (lambda ()
                    (let ((suggestions (anything-c-google-suggest-fetch anything-input)))
                      (if (some (lambda (suggestion)
                                  (equal (cdr suggestion) anything-input))
                                suggestions)
                          suggestions
                        ;; if there is no suggestion exactly matching the input then
                        ;; prepend a Search on Google item to the list
                        (append (list (cons (concat "Search for "
                                                    "'" anything-input "'"
                                                    " on Google")
                                            anything-input))
                                suggestions)))))
    (action . (("Google Search" .
                (lambda (candidate)
                  (browse-url (concat anything-c-google-suggest-search-url
                                      (url-hexify-string candidate)))))))
    (volatile)
    (requires-pattern . 3)
    (delayed)))
;; (anything 'anything-c-source-google-suggest)

(defun anything-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT."
  (let* ((result (with-current-buffer
                     (url-retrieve-synchronously
                      (concat anything-c-google-suggest-url
                              (url-hexify-string input)))
                   (buffer-substring (point-min) (point-max))))
         (split (split-string result "new Array("))
         (suggestions (anything-c-google-suggest-get-items (second split)))
         (numbers (anything-c-google-suggest-get-items (third split)))
         (longest (+ (apply 'max 0 (let (lengths)
                                     (dotimes (i (length suggestions))
                                       (push (+ (length (nth i suggestions))
                                                (length (nth i numbers)))
                                             lengths))
                                     lengths))
                     10))
         items)
    (dotimes (i (length suggestions))
      (let ((suggestion (nth i suggestions))
            (number (nth i numbers)))
        (push (cons (concat suggestion
                            (make-string (- longest
                                            (length suggestion)
                                            (length number))
                                         32)
                            number)
                    suggestion)
              items)))
    items))

(defun anything-c-google-suggest-get-items (str)
  "Extract items from STR returned by Google Suggest."
  (let ((start nil)
        items)
    (while (string-match "\"\\([^\"]+?\\)\"" str start)
      (push (match-string 1 str) items)
      (setq start (1+ (match-end 1))))
    items))

;;; Jabber Contacts (jabber.el)
(defun anything-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                  (cons (symbol-name item) item)) jids))))))

(defvar anything-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda ()
                    (mapcar
                     'car
                     (anything-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (anything-c-jabber-online-contacts)))))))))
;; (anything 'anything-c-source-jabber-contacts)


;;; Call source.
(defvar anything-source-select-buffer "*anything source select*")
(defvar anything-c-source-call-source
  `((name . "Call anything source")
    (candidate-number-limit . 9999)
    (candidates . (lambda ()
                    (loop for vname in (all-completions "anything-c-source-" obarray)
                          for var = (intern vname)
                          for name = (ignore-errors (assoc-default 'name (symbol-value var)))
                          if name collect (cons (format "%s (%s)" name vname) var))))
    (action . (("Invoke anything with selected source" .
                (lambda (candidate)
                  (anything candidate nil nil nil nil
                            anything-source-select-buffer)))
               ("Describe variable" . describe-variable)))
    (persistent-action . describe-variable)))
;; (anything 'anything-c-source-call-source)

(defun anything-call-source ()
  "Call anything source."
  (interactive)
  (anything 'anything-c-source-call-source nil nil nil nil
            anything-source-select-buffer))

(defun anything-call-source-from-anything ()
  "Call anything source within `anything' session."
  (interactive)
  (setq anything-input-idle-delay 0)
  (anything-set-sources '(anything-c-source-call-source)))

;; Occur
(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-c-source-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (setq anything-occur-buf (get-buffer-create "*Anything Occur*"))
                    (with-current-buffer anything-occur-buf
                      (erase-buffer)
                      (let ((count (occur-engine anything-pattern
                                                 (list anything-c-source-occur-current-buffer) anything-occur-buf
                                                 list-matching-lines-default-context-lines nil
                                                 list-matching-lines-buffer-name-face
                                                 nil list-matching-lines-face
                                                 (not (eq occur-excluded-properties t)))))
                        (when (> count 0)
                          (let ((lines (split-string (buffer-string) "\n" t)))
                            (cdr lines)))))))
    (action . (("Goto line" . (lambda (candidate)
                                (goto-line (string-to-number candidate) anything-c-source-occur-current-buffer)))))
    (requires-pattern . 1)
    (volatile)))
;; (anything 'anything-c-source-occur)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Action Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
(defvar anything-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-c-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `anything-c-external-commands-list'.

The code is ripped out of `eshell-complete-commands-list'."
  (if anything-c-external-commands-list
      anything-c-external-commands-list
    (setq anything-c-external-commands-list
          (let* ((paths (split-string (getenv "PATH") path-separator))
                 (cwd (file-name-as-directory
                       (expand-file-name default-directory)))
                 (path "") (comps-in-path ())
                 (file "") (filepath "") (completions ()))
            ;; Go thru each path in the search path, finding completions.
            (while paths
              (setq path (file-name-as-directory
                          (expand-file-name (or (car paths) ".")))
                    comps-in-path
                    (and (file-accessible-directory-p path)
                         (file-name-all-completions "" path)))
              ;; Go thru each completion found, to see whether it should be
              ;; used, e.g. see if it's executable.
              (while comps-in-path
                (setq file (car comps-in-path)
                      filepath (concat path file))
                (if (and (not (member file completions))
                         (or (string-equal path cwd)
                             (not (file-directory-p filepath)))
                         (file-executable-p filepath))
                    (setq completions (cons file completions)))
                (setq comps-in-path (cdr comps-in-path)))
              (setq paths (cdr paths)))
            completions))))

(defun anything-c-file-buffers (filename)
  "Returns a list of those buffer names which correspond to the
file given by FILENAME."
  (let (name ret)
    (dolist (buf (buffer-list) ret)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn
                   (string= filename bfn))
          (push (buffer-name buf) ret)))
      ret)))

(defun anything-c-delete-file (file)
  "Delete the given file after querying the user.  Ask to kill
buffers associated with that file, too."
  (if (y-or-n-p (format "Really delete file %s? " file))
      (progn
        (let ((buffers (anything-c-file-buffers file)))
          (delete-file file)
          (dolist (buf buffers)
            (when (y-or-n-p (format "Kill buffer %s, too? " buf))
              (kill-buffer buf)))))
    (message "Nothing deleted.")))

(defun anything-c-open-file-externally (file)
  "Open FILE with an external tool.  Query the user which tool to
use."
  (start-process "anything-c-open-file-externally"
                 nil
                 (completing-read "Program: "
                                  (anything-c-external-commands-list-1))
                 file))

(defun w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (replace-regexp-in-string ;for UNC paths
                               "/" "\\"
                               (replace-regexp-in-string ; strip cygdrive paths
                                "/cygdrive/\\(.\\)" "\\1:" file nil nil) nil t))))
(defun anything-c-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute-open-file file)
    (start-process "anything-c-open-file-with-default-tool"
                   nil
                   (cond ((eq system-type 'gnu/linux)
                          "xdg-open")
                         ((or (eq system-type 'darwin) ;; Mac OS X
                              (eq system-type 'macos)) ;; Mac OS 9
                          "open"))
                   file)))

(defun anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

(defun anything-c-display-to-real-line (candidate)
  (if (string-match "^ *\\([0-9]+\\):\\(.+\\)$" candidate)
      (list (string-to-number (match-string 1 candidate)) (match-string 2 candidate))
    (error "Line number not found")))

(defun anything-c-action-line-goto (lineno-and-content)
  (apply #'anything-goto-file-line (anything-attr 'target-file)
         (append lineno-and-content
                 (list (if (and (anything-attr-defined 'target-file)
                                (not anything-in-persistent-action))
                           'find-file-other-window
                         'find-file)))))

(defun* anything-c-action-file-line-goto (file-line-content &optional (find-file-function #'find-file))
  (apply #'anything-goto-file-line file-line-content))

(require 'compile)
(defun anything-c-filtered-candidate-transformer-file-line (candidates source)
  (mapcar
   (lambda (candidate)
     (if (not (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.+\\)$" candidate))
         (error "Filename and line number not found")
       (let ((filename (match-string 1 candidate))
             (lineno (match-string 2 candidate))
             (content (match-string 3 candidate)))
         (cons (format "%s:%s\n %s"
                       (propertize filename 'face compilation-info-face)
                       (propertize lineno 'face compilation-line-face)
                       content)
               (list (expand-file-name
                      filename
                      (anything-aif (anything-attr 'default-directory)
                          (if (functionp it) (funcall it) it)
                        (and (anything-candidate-buffer)
                             (buffer-local-value
                              'default-directory
                              (anything-candidate-buffer)))))
                     (string-to-number lineno) content)))))
   candidates))

(defun* anything-goto-file-line (file lineno content &optional (find-file-function #'find-file))
  (anything-aif (anything-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (anything-attr-defined 'adjust)
      (anything-c-goto-line-with-adjustment lineno content)
    (goto-line lineno))
  (unless (anything-attr-defined 'recenter)
    (set-window-start (get-buffer-window anything-current-buffer) (point)))
  (anything-aif (anything-attr 'after-jump-hook)
      (funcall it))
  (when anything-in-persistent-action
    (anything-persistent-highlight-point (point-at-bol) (point-at-eol))))

;; borrowed from etags.el
;; (anything-c-goto-line-with-adjustment (line-number-at-pos) ";; borrowed from etags.el")
(defun anything-c-goto-line-with-adjustment (line line-content)
  (let ((startpos)
        offset found pat)
    ;; This constant is 1/2 the initial search window.
    ;; There is no sense in making it too small,
    ;; since just going around the loop once probably
    ;; costs about as much as searching 2000 chars.
    (setq offset 1000
          found nil
          pat (concat (if (eq selective-display t)
                          "\\(^\\|\^m\\) *" "^ *") ;allow indent
                      (regexp-quote line-content)))
    ;; If no char pos was given, try the given line number.
    (setq startpos (progn (goto-line line) (point)))
    (or startpos (setq startpos (point-min)))
    ;; First see if the tag is right at the specified location.
    (goto-char startpos)
    (setq found (looking-at pat))
    (while (and (not found)
                (progn
                  (goto-char (- startpos offset))
                  (not (bobp))))
      (setq found
            (re-search-forward pat (+ startpos offset) t)
            offset (* 3 offset)))       ; expand search window
    (or found
        (re-search-forward pat nil t)
        (error "not found")))
  ;; Position point at the right place
  ;; if the search string matched an extra Ctrl-m at the beginning.
  (and (eq selective-display t)
       (looking-at "\^m")
       (forward-char 1))
  (beginning-of-line))

(anything-document-attribute 'no-new-window "type . file-line" "")
(anything-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(anything-document-attribute 'before-jump-hook "type . file-line / line" "")
(anything-document-attribute 'after-jump-hook "type . file-line / line" "")
(anything-document-attribute 'adjust "type . file-line"
  "Search around line matching line contents.")
(anything-document-attribute 'recenter "type . file-line / line"
  "`recenter' after jumping.")
(anything-document-attribute 'target-file "type . line"
  "Goto line of target-file.")

(defun anything-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (anything-c-stringify cmd-or-name)
              (delete (anything-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg anything-current-prefix-arg))
    (call-interactively (anything-c-symbolify cmd-or-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Persistent Action Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-persistent-highlight-overlay
  (make-overlay (point) (point)))

(defun anything-persistent-highlight-point (start &optional end buf face rec)
  (goto-char start)
  (when (overlayp anything-c-persistent-highlight-overlay)
    (move-overlay anything-c-persistent-highlight-overlay
                  start
                  (or end (line-end-position))
                  buf))
  (overlay-put anything-c-persistent-highlight-overlay 'face (or face 'highlight))
  (when rec
    (recenter)))

(add-hook 'anything-cleanup-hook
          (lambda ()
            (when (overlayp anything-c-persistent-highlight-overlay)
              (delete-overlay anything-c-persistent-highlight-overlay))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Actions Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "el")
          (string= (file-name-extension candidate) "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "htm")
          (string= (file-name-extension candidate) "html"))
      (append actions '(("Browse with Browser" . browse-url)))
    actions))

;;;; Function
(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern candidate))
      (append actions '(("Call Interactively"
                         .
                         anything-c-call-interactively)))
    actions))

;;;; S-Expressions
(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
    actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Candidate Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
(defun anything-c-skip-boring-buffers (buffers)
  (anything-c-skip-entries buffers anything-c-boring-buffer-regexp))

(defun anything-c-skip-current-buffer (buffers)
  (remove (buffer-name anything-current-buffer) buffers))

(defun anything-c-shadow-boring-buffers (buffers)
  "Buffers matching `anything-c-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries buffers anything-c-boring-buffer-regexp))

;;; Files
(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries files anything-c-boring-file-regexp))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
skipped."
  (anything-c-skip-entries files anything-c-boring-file-regexp))
;; (anything-c-skip-boring-files '("README" "/src/.svn/hoge"))

(defun anything-c-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
      (mapcar (lambda (x)
                (replace-regexp-in-string "/cygdrive/\\(.\\)" "\\1:" x))
              (mapcar (lambda (y)
                        (replace-regexp-in-string "\\\\" "/" y)) args))
    args))

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with ~."
  (mapcar (lambda (file)
            (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                  (getenv "HOME"))))
              (if (and (stringp file) (string-match home file))
                  (cons (replace-match "~" nil nil file) file)
                file)))
          files))

;;; Functions
(defun anything-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Adaptive Sorting of Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar anything-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defadvice anything-initialize (before anything-c-adaptive-initialize activate)
  "Advise `anything-initialize' to reset `anything-c-adaptive-done'
when anything is started."
  (setq anything-c-adaptive-done nil))

(defadvice anything-exit-minibuffer (before anything-c-adaptive-exit-minibuffer activate)
  "Advise `anything-exit-minibuffer' to store history information
when a candidate is selected with RET."
  (anything-c-adaptive-store-selection))

(defadvice anything-select-action (before anything-c-adaptive-select-action activate)
  "Advise `anything-select-action' to store history information
when the user goes to the action list with TAB."
  (anything-c-adaptive-store-selection))

(defun anything-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless anything-c-adaptive-done
    (setq anything-c-adaptive-done t)
    (let* ((source (anything-get-current-source))
           (source-name (or (assoc-default 'type source)
                            (assoc-default 'name source)))
           (source-info (or (assoc source-name anything-c-adaptive-history)
                            (progn
                              (push (list source-name) anything-c-adaptive-history)
                              (car anything-c-adaptive-history))))
           (selection (anything-get-selection))
           (selection-info (progn
                             (setcdr source-info
                                     (cons
                                      (let ((found (assoc selection (cdr source-info))))
                                        (if (not found)
                                            ;; new entry
                                            (list selection)

                                          ;; move entry to the beginning of the
                                          ;; list, so that it doesn't get
                                          ;; trimmed when the history is
                                          ;; truncated
                                          (setcdr source-info
                                                  (delete found (cdr source-info)))
                                          found))
                                      (cdr source-info)))
                             (cadr source-info)))
           (pattern-info (progn
                           (setcdr selection-info
                                   (cons
                                    (let ((found (assoc anything-pattern (cdr selection-info))))
                                      (if (not found)
                                          ;; new entry
                                          (cons anything-pattern 0)

                                        ;; move entry to the beginning of the
                                        ;; list, so if two patterns used the
                                        ;; same number of times then the one
                                        ;; used last appears first in the list
                                        (setcdr selection-info
                                                (delete found (cdr selection-info)))
                                        found))
                                    (cdr selection-info)))
                           (cadr selection-info))))

      ;; increase usage count
      (setcdr pattern-info (1+ (cdr pattern-info)))

      ;; truncate history if needed
      (if (> (length (cdr selection-info)) anything-c-adaptive-history-length)
          (setcdr selection-info
                  (subseq (cdr selection-info) 0 anything-c-adaptive-history-length))))))

(if (file-readable-p anything-c-adaptive-history-file)
    (load-file anything-c-adaptive-history-file))
(add-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

(defun anything-c-adaptive-save-history ()
  "Save history information to file given by
`anything-c-adaptive-history-file'."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for anything adaptive display.\n")
    (prin1 `(setq anything-c-adaptive-history ',anything-c-adaptive-history)
           (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) anything-c-adaptive-history-file nil
                  (unless (interactive-p) 'quiet))))

(defun anything-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`anything-sources' or a type in `anything-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name anything-c-adaptive-history)))
    (if (not source-info)
        ;; if there is no information stored for this source then do nothing
        candidates
      ;; else...
      (let ((usage
             ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
             ;; pairs
             (mapcar (lambda (candidate-info)
                       (let ((count 0))
                         (dolist (pattern-info (cdr candidate-info))
                           (if (not (equal (car pattern-info)
                                           anything-pattern))
                               (incf count (cdr pattern-info))

                             ;; if current pattern is equal to the previously
                             ;; used one then this candidate has priority
                             ;; (that's why its count is boosted by 10000) and
                             ;; it only has to compete with other candidates
                             ;; which were also selected with the same pattern
                             (setq count (+ 10000 (cdr pattern-info)))
                             (return)))
                         (cons (car candidate-info) count)))
                     (cdr source-info)))
            sorted)

        ;; sort the list in descending order, so candidates with highest
        ;; priorty come first
        (setq usage (sort usage (lambda (first second)
                                  (> (cdr first) (cdr second)))))

        ;; put those candidates first which have the highest usage count
        (dolist (info usage)
          (when (member* (car info) candidates
                         :test 'anything-c-adaptive-compare)
            (push (car info) sorted)
            (setq candidates (remove* (car info) candidates
                                      :test 'anything-c-adaptive-compare))))

        ;; and append the rest
        (append (reverse sorted) candidates nil)))))

(defun anything-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
           x)
         (if (listp y)
             (cdr y)
           y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Plug-in ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plug-in: candidates-file
(defun anything-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init anything-p-candidats-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                  (cond ((null orig-init) nil)
                        ((functionp orig-init) (list orig-init))
                        (t orig-init))))
        (candidates-in-buffer)
        ,@source)
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--candidates-file)

(defun anything-p-candidats-file-init ()
  (destructuring-bind (file &optional updating)
      (anything-mklist (anything-attr 'candidates-file))
    (with-current-buffer (anything-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(anything-document-attribute 'candidates-file "candidates-file plugin"
  "Use a file as the candidates buffer.

If optional 2nd argument is non-nil, the file opened with `auto-revert-mode'.")

;; Plug-in: headline
(defun anything-compile-source--anything-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . anything-headline-init)
                (get-line-fn . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)))
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--anything-headline)

(defun anything-headline-init ()
  (when (and (anything-current-buffer-is-modified)
             (with-current-buffer anything-current-buffer
               (eval (or (anything-attr 'condition) t))))
    (anything-headline-make-candidate-buffer
     (anything-attr 'headline)
     (anything-attr 'subexp))))

(anything-document-attribute 'headline "Headline plug-in"
  "Regexp string for anything-headline to scan.")
(anything-document-attribute 'condition "Headline plug-in"
  "A sexp representing the condition to use anything-headline.")
(anything-document-attribute 'subexp "Headline plug-in"
  "Display (match-string-no-properties subexp).")

(defun anything-headline-get-candidates (regexp subexp)
  (save-excursion
    (set-buffer anything-current-buffer)
    (when t
      (goto-char (point-min))
      (if (functionp regexp) (setq regexp (funcall regexp)))
      (let (hierarchy curhead)
        (flet ((matched ()
                        (if (numberp subexp)
                            (cons (match-string-no-properties subexp) (match-beginning subexp))
                          (cons (buffer-substring (point-at-bol) (point-at-eol))
                                (point-at-bol))))
               (hierarchies (headlines)
                            (1+ (loop for (_ . hierarchy) in headlines
                                      maximize hierarchy)))
               (vector-0-n (v n)
                           (loop for i from 0 to hierarchy
                                 collecting (aref curhead i)))
               (arrange (headlines)
                        (loop with curhead = (make-vector (hierarchies headlines) "")
                              for ((str . pt) . hierarchy) in headlines
                              do (aset curhead hierarchy str)
                              collecting
                              (cons
                               (mapconcat 'identity (vector-0-n curhead hierarchy) " / ")
                               pt))))
          (if (listp regexp)
              (arrange
               (sort
                (loop for re in regexp
                      for hierarchy from 0
                      do (goto-char (point-min))
                      appending
                      (loop
                       while (re-search-forward re nil t)
                       collect (cons (matched) hierarchy)))
                (lambda (a b) (> (cdar b) (cdar a)))))
            (loop while (re-search-forward regexp nil t)
                  collect (matched))))))))

(defun anything-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (anything-candidate-buffer 'local)
    (loop for (content . pos) in (anything-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-current-buffer anything-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun anything-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window anything-current-buffer) (point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap
(when anything-c-use-standard-keys
  (setq anything-map
        (let ((map (copy-keymap minibuffer-local-map)))
          (define-key map (kbd "<down>")  'anything-next-line)
          (define-key map (kbd "<up>")    'anything-previous-line)
          (define-key map (kbd "C-n")     'anything-next-line)
          (define-key map (kbd "C-p")     'anything-previous-line)
          (define-key map (kbd "<prior>") 'anything-previous-page)
          (define-key map (kbd "<next>")  'anything-next-page)
          (define-key map (kbd "M-v")     'anything-previous-page)
          (define-key map (kbd "C-v")     'anything-next-page)
          (define-key map (kbd "<right>") 'anything-next-source)
          (define-key map (kbd "<left>")  'anything-previous-source)
          (define-key map (kbd "<RET>")   'anything-exit-minibuffer)
          (define-key map (kbd "C-1")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-2")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-3")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-4")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-5")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-6")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-7")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-8")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-9")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "<tab>")   'anything-select-action)
          (defalias 'anything-next-history-element     'next-history-element)
          (defalias 'anything-previous-history-element 'previous-history-element)
          (define-key map (kbd "M-p")     'anything-previous-history-element)
          (define-key map (kbd "M-n")     'anything-next-history-element)
          (define-key map (kbd "C-s")     'anything-isearch)
          (define-key map (kbd "C-r")     'undefined)
          map))

  (setq anything-isearch-map
        (let ((map (copy-keymap (current-global-map))))
          (define-key map (kbd "<return>")    'anything-isearch-default-action)
          (define-key map (kbd "<tab>")       'anything-isearch-select-action)
          (define-key map (kbd "C-g")         'anything-isearch-cancel)
          (define-key map (kbd "C-s")         'anything-isearch-again)
          (define-key map (kbd "C-r")         'undefined)
          (define-key map (kbd "<backspace>") 'anything-isearch-delete)
          ;; add printing chars
          (let ((i ?\s))
            (while (< i 256)
              (define-key map (vector i) 'anything-isearch-printing-char)
              (setq i (1+ i))))
          map)))

;;; Type Attributes
(setq anything-type-attributes
      `((buffer
         (action
          ,@(if pop-up-frames
                '(("Switch to buffer other window" . switch-to-buffer-other-window)
                  ("Switch to buffer" . switch-to-buffer))
              '(("Switch to buffer" . switch-to-buffer)
                ("Switch to buffer other window" . switch-to-buffer-other-window)
                ("Switch to buffer other frame" . switch-to-buffer-other-frame)))
          ("Display buffer"   . display-buffer)
          ("Kill buffer"      . kill-buffer))
         (candidate-transformer . anything-c-skip-boring-buffers))
        (file
         (action
          ,@(if pop-up-frames
                '(("Find file other window" . find-file-other-window)
                  ("Find file" . find-file))
              '(("Find file" . find-file)
                ("Find file other window" . find-file-other-window)
                ("Find file other frame" . find-file-other-frame)))
          ("Open dired in file's directory" . anything-c-open-dired)
          ("Delete file" . anything-c-delete-file)
          ("Open file externally" . anything-c-open-file-externally)
          ("Open file with default tool" . anything-c-open-file-with-default-tool))
         (action-transformer . (lambda (actions candidate)
                                 (anything-c-compose
                                  (list actions candidate)
                                  '(anything-c-transform-file-load-el
                                    anything-c-transform-file-browse-url))))
         (candidate-transformer . (lambda (candidates)
                                    (anything-c-compose
                                     (list candidates)
                                     '(anything-c-w32-pathname-transformer
                                       anything-c-skip-boring-files
                                       anything-c-shorten-home-path)))))
        (command (action ("Call interactively" . anything-c-call-interactively)
                         ("Describe command" . (lambda (command-name)
                                                 (describe-function (intern command-name))))
                         ("Add command to kill ring" . kill-new)
                         ("Go to command's definition" . (lambda (command-name)
                                                           (find-function
                                                            (intern command-name)))))
                 ;; Sort commands according to their usage count.
                 (filtered-candidate-transformer . anything-c-adaptive-sort))
        (function (action ("Describe function" . (lambda (function-name)
                                                   (describe-function (intern function-name))))
                          ("Add function to kill ring" . kill-new)
                          ("Go to function's definition" . (lambda (function-name)
                                                             (find-function
                                                              (intern function-name)))))
                  (action-transformer . (lambda (actions candidate)
                                          (anything-c-compose
                                           (list actions candidate)
                                           '(anything-c-transform-function-call-interactively))))
                  (candidate-transformer . (lambda (candidates)
                                             (anything-c-compose
                                              (list candidates)
                                              '(anything-c-mark-interactive-functions)))))
        (sexp (action ("Eval s-expression" . (lambda (c)
                                               (eval (read c))))
                      ("Add s-expression to kill ring" . kill-new))
              (action-transformer . (lambda (actions candidate)
                                      (anything-c-compose
                                       (list actions candidate)
                                       '(anything-c-transform-sexp-eval-command-sexp)))))
        (bookmark (action ("Jump to bookmark" . bookmark-jump)
                          ("Delete bookmark" . bookmark-delete)))
        (line (display-to-real . anything-c-display-to-real-line)
              (action ("Go to Line" . anything-c-action-line-goto)))))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
     (desc "candidates-file plug-in")
     (expect '(anything-p-candidats-file-init)
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")))
                                  '(anything-compile-source--candidates-file)))))
     (expect '(anything-p-candidats-file-init
               (lambda () 1))
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")
                                     (init . (lambda () 1))))
                                  '(anything-compile-source--candidates-file)))))
     (expect '(anything-p-candidats-file-init
               (lambda () 1))
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")
                                     (init (lambda () 1))))
                                  '(anything-compile-source--candidates-file)))))
     (desc "anything-c-source-buffers")
     (expect '(("Buffers" ("foo" "curbuf")))
             (stub buffer-list => '("curbuf" " hidden" "foo" "*anything*"))
             (let ((anything-c-boring-buffer-regexp
                    (rx (or
                         (group bos  " ")
                         "*anything"
                         ;; echo area
                         " *Echo Area" " *Minibuf"))))
               (flet ((buffer-name (x) x))
                 (anything-test-candidates 'anything-c-source-buffers))))
     (desc "anything-c-stringify")
     (expect "str1"
             (anything-c-stringify "str1"))
     (expect "str2"
             (anything-c-stringify 'str2))
     (desc "anything-c-symbolify")
     (expect 'sym1
             (anything-c-symbolify "sym1"))
     (expect 'sym2
             (anything-c-symbolify 'sym2)))))


(provide 'anything-config)

;;; Local Variables:
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S (%Z) %u"
;;; End:

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-config.el")
;;; anything-config.el ends here

;;; LocalWords:  Tassilo Patrovics Vagn Johansen Dahl Clementson infos
;;; LocalWords:  Kamphausen informations McBrayer Volpiatto bbdb bb
;;; LocalWords:  iswitchb imenu Recentf sym samewindow pos bol eol
;;; LocalWords:  aif str lst func attrib recentf lessp prin mapatoms commandp
;;; LocalWords:  cmd stb Picklist picklist mapcan subentry destructuring dirs
;;; LocalWords:  darwin locat MacOS mdfind Firstname Lastname calc prepend jids
;;; LocalWords:  dotimes Thierry online vname
;;; LocalWords:  csharp javascript lua makefile cperl zcat lineno buf
;;; LocalWords:  multiline href fn cand NewTitle cwd filepath thru ret
;;; LocalWords:  bfn fOpen UNC cygdrive nt xdg macos FILE's elc rx svn hg
;;; LocalWords:  CANDIDATE's darcs facep pathname args pathnames subseq priorty
;;; LocalWords:  Vokes rfind berkeley JST ffap lacarte bos
;;; LocalWords:  Lacarte Minibuf epp LaCarte bm attrset migemo attr conf mklist
;;; LocalWords:  startpos noselect dont desc

