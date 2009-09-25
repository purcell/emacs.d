;;; anything-config.el --- Predefined configurations for `anything.el'

;; Filename: anything-config.el

;; Description: Predefined configurations for `anything.el'
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;;             rubikitch    <rubikitch@ruby-lang.org>
;;             Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2007 ~ 2009, Tassilo Horn, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: 2009-02-16 21:38:23
;; Version: 0.4.1
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

;;; !NOTICE!
;;
;; If this file does not work, upgrade anything.el!
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything.el

;;; Commentary:
;;
;; Predefined configurations for `anything.el'
;;
;; For quick start, try `anything-for-files' to open files.
;; 
;; To configure anything you should setup `anything-sources'
;; with specify source, like below:
;;
;; (setq anything-sources
;;       '(anything-c-source-buffers
;;         anything-c-source-buffer-not-found
;;         anything-c-source-file-name-history
;;         anything-c-source-info-pages
;;         anything-c-source-info-elisp
;;         anything-c-source-man-pages
;;         anything-c-source-locate
;;         anything-c-source-emacs-commands
;;         ))
;;
;; Below are complete source list you can setup in `anything-sources':
;;
;;  Buffer:
;;     `anything-c-source-buffers'          (Buffers)
;;     `anything-c-source-buffer-not-found' (Create buffer)
;;     `anything-c-source-buffers+'         (Buffers)
;;  File:
;;     `anything-c-source-file-name-history'     (File Name History)
;;     `anything-c-source-files-in-current-dir'  (Files from Current Directory)
;;     `anything-c-source-files-in-current-dir+' (Files from Current Directory)
;;     `anything-c-source-file-cache'            (File Cache)
;;     `anything-c-source-locate'                (Locate)
;;     `anything-c-source-recentf'               (Recentf)
;;     `anything-c-source-ffap-guesser'          (File at point)
;;     `anything-c-source-ffap-line'             (File/Lineno at point)
;;  Help:
;;     `anything-c-source-man-pages'  (Manual Pages)
;;     `anything-c-source-info-pages' (Info Pages)
;;     `anything-c-source-info-elisp' (Info Elisp)
;;     `anything-c-source-info-cl'    (Info Common-Lisp)
;;  Command:
;;     `anything-c-source-complex-command-history'  (Complex Command History)
;;     `anything-c-source-extended-command-history' (Emacs Commands History)
;;     `anything-c-source-emacs-commands'           (Emacs Commands)
;;     `anything-c-source-lacarte'                  (Lacarte)
;;  Function:
;;     `anything-c-source-emacs-functions'              (Emacs Functions)
;;     `anything-c-source-emacs-functions-with-abbrevs' (Emacs Functions)
;;  Variable:
;;     `anything-c-source-emacs-variables' (Emacs Variables)
;;  Bookmark:
;;     `anything-c-source-bookmarks'               (Bookmarks)
;;     `anything-c-source-bookmark-set'            (Set Bookmark)
;;     `anything-c-source-bookmarks-ssh'           (Bookmarks-ssh)
;;     `anything-c-source-bookmarks-su'            (Bookmarks-root)
;;     `anything-c-source-bookmarks-local'         (Bookmarks-Local)
;;     `anything-c-source-bookmark-regions'        (Bookmark Regions)
;;     `anything-c-source-bookmark-w3m'            (Bookmark W3m)
;;     `anything-c-source-bookmark-gnus'           (Bookmark Gnus)
;;     `anything-c-source-bookmark-info'           (Bookmark Info)
;;     `anything-c-source-bookmark-files&dirs'     (Bookmark Files&Directories)
;;     `anything-c-source-bookmark-su-files&dirs'  (Bookmark Root-Files&Directories)
;;     `anything-c-source-bookmark-ssh-files&dirs' (Bookmark Ssh-Files&Directories)
;;     `anything-c-source-w3m-bookmarks'           (W3m Bookmarks)
;;  Library:
;;     `anything-c-source-elisp-library-scan' (Elisp libraries (Scan))
;;  Programming:
;;     `anything-c-source-imenu'                              (Imenu)
;;     `anything-c-source-ctags'                              (Exuberant ctags)
;;     `anything-c-source-semantic'                           (Semantic Tags)
;;     `anything-c-source-simple-call-tree-functions-callers' (Function is called by)
;;     `anything-c-source-simple-call-tree-callers-functions' (Function calls)
;;     `anything-c-source-commands-and-options-in-file'       (Commands/Options in file)
;;  Color and Face:
;;     `anything-c-source-customize-face' (Customize Face)
;;     `anything-c-source-colors'         (Colors)
;;  Search Engine:
;;     `anything-c-source-tracker-search' (Tracker Search)
;;     `anything-c-source-mac-spotlight'  (mdfind)
;;  icicle:
;;     `anything-c-source-icicle-region' (Icicle Regions)
;;  Kill ring:
;;     `anything-c-source-kill-ring' (Kill Ring)
;;  Mark ring:
;;     `anything-c-source-mark-ring'        (mark-ring)
;;     `anything-c-source-global-mark-ring' (global-mark-ring)
;;  Register:
;;     `anything-c-source-register' (Registers)
;;  Headline Extraction:
;;     `anything-c-source-fixme'                            (TODO/FIXME/DRY comments)
;;     `anything-c-source-rd-headline'                      (RD HeadLine)
;;     `anything-c-source-oddmuse-headline'                 (Oddmuse HeadLine)
;;     `anything-c-source-emacs-source-defun'               (Emacs Source DEFUN)
;;     `anything-c-source-emacs-lisp-expectations'          (Emacs Lisp Expectations)
;;     `anything-c-source-emacs-lisp-toplevels'             (Emacs Lisp Toplevel / Level 4 Comment / Linkd Star)
;;     `anything-c-source-org-headline'                     (Org HeadLine)
;;     `anything-c-source-yaoddmuse-emacswiki-edit-or-view' (Yaoddmuse Edit or View (EmacsWiki))
;;     `anything-c-source-yaoddmuse-emacswiki-post-library' (Yaoddmuse Post library (EmacsWiki))
;;     `anything-c-source-eev-anchor'                       (Anchors)
;;  Misc:
;;     `anything-c-source-picklist'           (Picklist)
;;     `anything-c-source-bbdb'               (BBDB)
;;     `anything-c-source-evaluation-result'  (Evaluation Result)
;;     `anything-c-source-calculation-result' (Calculation Result)
;;     `anything-c-source-google-suggest'     (Google Suggest)
;;     `anything-c-source-surfraw'            (Surfraw)
;;     `anything-c-source-emms-streams'       (Emms Streams)
;;     `anything-c-source-emms-dired'         (Music Directory)
;;     `anything-c-source-jabber-contacts'    (Jabber Contacts)
;;     `anything-c-source-call-source'        (Call anything source)
;;     `anything-c-source-occur'              (Occur)
;;     `anything-c-source-create'             (Create)
;;     `anything-c-source-minibuffer-history' (Minibuffer History)
;;     `anything-c-source-elscreen'           (Elscreen)
;;  System:
;;     `anything-c-source-xrandr-change-resolution' (Change Resolution)
;;     `anything-c-source-xfonts'                   (X Fonts)
;;     `anything-c-source-gentoo'                   (Portage sources)
;;     `anything-c-source-use-flags'                (Use Flags)
;;     `anything-c-source-emacs-process'            (Emacs Process)

;;; Change log:
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog

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
;; Install also http://www.emacswiki.org/emacs/auto-document.el
;; And eval it or run interactively.
;;
;; [EVAL IT] (anything-c-insert-summary)
;; [EVAL IT] (auto-document)
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
  "Whether use standard keybindings. (no effect)

Key definitions in anything-config.el are removed because
anything.el uses Emacs-standard keys by default. e.g. M-p/M-n for
minibuffer history, C-s for isearch, etc.

If you use `iswitchb' with `anything',
evaluate (anything-iswitchb-setup) .  Then some bindings that
conflict with `iswitchb', e.g. C-p/C-n for the minibuffer
history, are removed from `anything-map'. "
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

(defcustom anything-su-or-sudo "su"
  "What command to use for root access."
  :type 'string
  :group 'anything-config)

(defcustom anything-for-files-prefered-list '(anything-c-source-ffap-line
                                              anything-c-source-ffap-guesser
                                              anything-c-source-buffers+
                                              anything-c-source-recentf
                                              anything-c-source-bookmarks
                                              anything-c-source-file-cache
                                              anything-c-source-files-in-current-dir+
                                              anything-c-source-locate)
  "Your prefered sources to find files."
  :type 'list
  :group 'anything-config)

(defcustom anything-create--actions-private nil
  "User defined actions for `anything-create' / `anything-c-source-create'.
It is a list of (DISPLAY . FUNCTION) pairs like `action'
attribute of `anything-sources'.

It is prepended to predefined pairs."
  :type 'list
  :group 'anything-config)

(defcustom anything-allow-skipping-current-buffer t
  "Show current buffer or not in anything buffer"
  :type 'boolean
  :group 'anything-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preconfigured Anything ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-for-files ()
  "Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate"
  (interactive)
  (anything-other-buffer anything-for-files-prefered-list "*anything for files*"))

(defun anything-info-at-point ()
  "Preconfigured `anything' for searching info at point."
  (interactive)
  (anything '(anything-c-source-info-elisp
              anything-c-source-info-cl
              anything-c-source-info-pages)
            (thing-at-point 'symbol) nil nil nil "*anything info*"))

(defun anything-show-kill-ring ()
  "Preconfigured `anything' for `kill-ring'. It is drop-in replacement of `yank-pop'.
You may bind this command to M-y."
  (interactive)
  (anything-other-buffer 'anything-c-source-kill-ring "*anything kill-ring*"))

(defun anything-minibuffer-history ()
  "Preconfigured `anything' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (anything-other-buffer 'anything-c-source-minibuffer-history
                           "*anything minibuffer-history*")))

(dolist (map (list minibuffer-local-filename-completion-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-filename-map
                   minibuffer-local-map
                   minibuffer-local-isearch-map
                   minibuffer-local-must-match-map
                   minibuffer-local-ns-map))
  (define-key map "\C-r" 'anything-minibuffer-history))

(defun anything-gentoo ()
  "Preconfigured `anything' for gentoo linux."
  (interactive)
  (anything-other-buffer '(anything-c-source-gentoo
                           anything-c-source-use-flags)
                         "*anything gentoo*"))

(defun anything-surfraw-only ()
  "Preconfigured `anything' for surfraw.
If region is marked set anything-pattern to region.
With one prefix arg search symbol at point.
With two prefix args allow choosing in which symbol to search."
  (interactive)
  (let (search pattern)
    (cond ((region-active-p)
           (setq pattern (buffer-substring (region-beginning) (region-end))))
          ((equal current-prefix-arg '(4))
           (setq pattern (thing-at-point 'symbol)))
          ((equal current-prefix-arg '(16))
           (setq search
                 (intern
                  (completing-read "Search in: "
                                   (list "symbol" "sentence" "sexp" "line" "word"))))
           (setq pattern (thing-at-point search))))
    (anything 'anything-c-source-surfraw
              (and pattern (replace-regexp-in-string "\n" "" pattern))
              nil nil nil "*anything surfraw*")))

(defun anything-imenu ()
  "Preconfigured `anything' for `imenu'."
  (interactive)
  (anything 'anything-c-source-imenu nil nil nil nil "*anything imenu*"))

(defun anything-google-suggest ()
  "Preconfigured `anything' for google search with google suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-google-suggest "*anything google*"))

;;; Converted from anything-show-*-only
(defun anything-for-buffers ()
  "Preconfigured `anything' for buffer."
  (interactive)
  (anything-other-buffer 'anything-c-source-buffers "*anything for buffers*"))

(defun anything-bbdb ()
  "Preconfigured `anything' for BBDB."
  (interactive)
  (anything-other-buffer 'anything-c-source-bbdb "*anything bbdb*"))

(defun anything-locate ()
  "Preconfigured `anything' for Locate."
  (interactive)
  (anything-other-buffer 'anything-c-source-locate "*anything locate*"))

(defun anything-w3m-bookmarks ()
  "Preconfigured `anything' for w3m bookmark."
  (interactive)
  (anything-other-buffer 'anything-c-source-w3m-bookmarks "*anything w3m bookmarks*"))

(defun anything-colors ()
  "Preconfigured `anything' for color."
  (interactive)
  (anything-other-buffer '(anything-c-source-colors anything-c-source-customize-face)
                         "*anything colors*"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill buffers
(defun anything-kill-buffers ()
  "You can continuously kill buffer you selected."
  (interactive)
  (anything
   '(((name . "Kill Buffers")
      (candidates . anything-c-buffer-list)
      (action
       ("Kill Buffer" . (lambda (candidate)
                          (kill-buffer candidate)
                          (anything-kill-buffers)
                          )))))
   nil nil))

;;; Regexp
(defun anything-query-replace-regexp (&rest args)
  "Drop-in replacement of `query-replace-regexp' with building regexp visually."
  (interactive
   (or (anything-c-regexp-base "Query Replace Regexp: "
                               '((name . "Lines matching Regexp")
                                 (action . anything-c-query-replace-args)))
       (keyboard-quit)))
  (apply 'query-replace-regexp args))

(defun anything-regexp ()
  "It is like `re-builder'. It helps buliding regexp and replacement."
  (interactive)
  (anything-c-regexp-base
   "Regexp: "
   '((name . "Regexp Builder")
     (action
      ("Kill Regexp as sexp" .
       (lambda (x) (anything-c-regexp-kill-new (prin1-to-string anything-input))))
      ("Query Replace Regexp" .
       (lambda (x) (apply 'query-replace-regexp (anything-c-query-replace-args (point)))))
      ("Kill Regexp" .
       (lambda (x) (anything-c-regexp-kill-new anything-input)))))))

(defun anything-c-query-replace-args (start-point)
  ;; create arguments of `query-replace-regexp'.
  (let ((region-only (and transient-mark-mode mark-active)))
    (list
     anything-input
     (read-string (format "Query replace regexp %s%s%s with: "
                          (if region-only "in region " "")
                          anything-input
                          (if current-prefix-arg "(word) " "")))
     current-prefix-arg
     (if region-only
         (region-beginning)
       start-point)
     (if region-only
         (region-end)
       (point-max)))))

(defun anything-c-regexp-get-line (s e)
  (propertize
   (apply 'concat
          ;; Line contents
          (format "%5d: %s" (line-number-at-pos s) (buffer-substring s e))
          ;; subexps
          (loop for i from 0 to (1- (/ (length (match-data)) 2))
                unless (zerop i)
                collect (format "\n         $%d = %s"
                                i (match-string i))))
   ;; match beginning
   'anything-realvalue s))

(defun anything-c-regexp-persistent-action (txt)
  (goto-line (anything-aif (string-match "^ *\\([0-9]+\\)" txt)
                 (string-to-number (match-string 1 txt)))))

(defun anything-c-regexp-base (prompt attributes)
  (save-restriction
    (let ((anything-compile-source-functions
           ;; rule out anything-match-plugin because the input is one regexp.
           (delq 'anything-compile-source--match-plugin
                 (copy-sequence anything-compile-source-functions))))
      (if (and transient-mark-mode mark-active)
          (narrow-to-region (region-beginning) (region-end)))
      (anything
       (list
        (append
         '((init . (lambda () (anything-candidate-buffer anything-current-buffer)))
           (candidates-in-buffer)
           (get-line . anything-c-regexp-get-line)
           (persistent-action . anything-c-regexp-persistent-action)
           (multiline)
           (delayed))
         attributes))
       nil prompt nil nil "*anything regexp*"))))

(defun anything-c-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

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
  "[OBSOLETE] Only show sources about buffer.
Use `anything-for-buffers' instead."
  (interactive)
  (anything-set-source-filter '("Buffers")))

(defun anything-show-bbdb-only ()
  "[OBSOLETE] Only show sources about BBDB.
Use `anything-bbdb' instead."
  (interactive)
  (anything-set-source-filter '("BBDB")))

(defun anything-show-locate-only ()
  "[OBSOLETE] Only show sources about Locate.
Use `anything-locate' instead."
  (interactive)
  (anything-set-source-filter '("Locate")))

(defun anything-show-info-only ()
  "[OBSOLETE] Only show sources about Info.
Use `anything-info-at-point' instead."
  (interactive)
  (anything-set-source-filter '("Info Pages"
                                "Info Elisp"
                                "Info Common-Lisp")))

(defun anything-show-imenu-only ()
  "[OBSOLETE] Only show sources about Imenu.
Use `anything-imenu' instead."
  (interactive)
  (anything-set-source-filter '("Imenu")))

(defun anything-show-files-only ()
  "[OBSOLETE] Only show sources about File.
Use `anything-for-files' instead."
  (interactive)
  (anything-set-source-filter '("File Name History"
                                "Files from Current Directory"
                                "Recentf")))

(defun anything-show-w3m-bookmarks-only ()
  "[OBSOLETE] Only show source about w3m bookmark.
Use `anything-w3m-bookmarks' instead."
  (interactive)
  (anything-set-source-filter '("W3m Bookmarks")))

(defun anything-show-colors-only ()
  "[OBSOLETE] Only show source about color.
Use `anything-colors' instead."
  (interactive)
  (anything-set-source-filter '("Colors"
                                "Customize Faces")))

(defun anything-show-kill-ring-only ()
  "[OBSOLETE] Only show source about kill ring.
Use `anything-show-kill-ring' instead."
  (interactive)
  (anything-set-source-filter '("Kill Ring")))

(defun anything-show-this-source-only ()
  "Only show this source."
  (interactive)
  (setq anything-candidate-number-limit 9999)
  (anything-set-source-filter
   (list (assoc-default 'name (anything-get-current-source)))))

(defun anything-test-sources ()
  "List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp]."
  (interactive)
  (with-output-to-temp-buffer "*Anything Test Sources*"
    (mapc (lambda (s) (princ (format ";; (anything '%s)\n" s)))
          (apropos-internal "^anything-c-source" #'boundp))
    (pop-to-buffer standard-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For compatibility
(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Return t if Transient Mark mode is enabled and the mark is active.

Most commands that act on the region if it is active and
Transient Mark mode is enabled, and on the text near point
otherwise, should use `use-region-p' instead.  That function
checks the value of `use-empty-active-region' as well."
    (and transient-mark-mode mark-active)))

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
  (anything-aif (file-name-directory candidate)
      (string-match anything-pattern it)))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

;; `anything-c-compose' is no more needed, it is for compatibility.
(defalias 'anything-c-compose 'anything-compose)

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

(defun anything-c-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (anything-c-symbolify func)))

(defun anything-c-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (anything-c-symbolify var)))

(defun anything-c-find-function (func)
  "FUNC is symbol or string."
  (find-function (anything-c-symbolify func)))

(defun anything-c-find-variable (var)
  "VAR is symbol or string."
  (find-variable (anything-c-symbolify var)))

(defun anything-c-kill-new (string &optional replace yank-handler)
  "STRING is symbol or string."
  (kill-new (anything-c-stringify string) replace yank-handler))

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
    (loop while (re-search-forward "^;;;; <\\(.+?\\)>$\\|^;; (anything '\\(.+?\\))$\\|^ *;; (anything '\\(.+?\\))$"  nil t)
          collect (cond ((match-beginning 1)
                         (cons 'section (match-string-no-properties 1)))
                        ((match-beginning 2)
                         (cons 'source
                               (cons (match-string-no-properties 2)
                                     (assoc-default 'name (symbol-value (intern (match-string-no-properties 2)))))))
                        ((match-beginning 3)
                         (cons 'source
                               (cons (match-string-no-properties 3)
                                     (assoc-default 'name (symbol-value (intern (match-string-no-properties 3)))))))))))
                         
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

;;; Buffers+
(defface anything-dir-heading '((t (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in dired buffers."
  :group 'anything)

(defface anything-file-name
  '((t (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'anything)

(defface anything-dir-priv
  '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'anything)

(defvar anything-c-buffers-face1 'anything-dir-priv)
(defvar anything-c-buffers-face2 'font-lock-type-face)
(defvar anything-c-buffers-face3 'italic)
(eval-when-compile (require 'dired))
(defun anything-c-highlight-buffers (buffers)
  (require 'dired)
  (loop for i in buffers
        if (rassoc (get-buffer i) dired-buffers)
        collect (propertize i
                            'face anything-c-buffers-face1
                            'help-echo (car (rassoc (get-buffer i) dired-buffers)))
        if (buffer-file-name (get-buffer i))
        collect (propertize i
                            'face anything-c-buffers-face2
                            'help-echo (buffer-file-name (get-buffer i)))
        if (and (not (rassoc (get-buffer i) dired-buffers))
                (not (buffer-file-name (get-buffer i))))
        collect (propertize i
                            'face anything-c-buffers-face3)))

(defvar anything-c-source-buffers+
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (volatile)
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-buffers+-persistent-action)))

(defun anything-c-buffers+-persistent-action (name)
  (flet ((kill (item)
               (with-current-buffer item
                 (if (and (buffer-modified-p)
                          (buffer-file-name (current-buffer)))
                     (progn
                       (save-buffer)
                       (kill-buffer item))
                   (kill-buffer item))))
         (goto (item)
               (switch-to-buffer item)))
    (if current-prefix-arg
        (progn
          (kill name)
          (anything-delete-current-selection))
      (goto name))))

;; (anything 'anything-c-source-buffers+)


;;;; <File>
;;; File name history
(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file)))
;; (anything 'anything-c-source-file-name-history)

;;; Files in current dir
(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (init . (lambda () (setq anything-c-default-directory default-directory)))
    (candidates . (lambda () (directory-files anything-c-default-directory)))
    (volatile)
    (type . file)))
;; (anything 'anything-c-source-files-in-current-dir)

(defvar anything-c-files-face1 'anything-dir-priv)
(defvar anything-c-files-face2 'anything-file-name)
(defun anything-c-highlight-files (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize (file-name-nondirectory i)
                            'face anything-c-files-face1
                            'help-echo (expand-file-name i))
        else
        collect (propertize (file-name-nondirectory i)
                            'face anything-c-files-face2
                            'help-echo (expand-file-name i))))


(defvar anything-c-source-files-in-current-dir+
  '((name . "Files from Current Directory")
    (init . (lambda ()
              (setq anything-c-default-directory
                    (expand-file-name default-directory))))
    (candidates . (lambda ()
                    (directory-files
                     anything-c-default-directory t)))
    (candidate-transformer anything-c-highlight-files)
    (volatile)
    (type . file)))

;; (anything 'anything-c-source-files-in-current-dir+)

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
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
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
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))
              ;; Big value empowers anything/recentf
              (when (and (numberp recentf-max-saved-items)
                         (<= recentf-max-saved-items 20))
                (setq recentf-max-saved-items 500))))
    (candidates . recentf-list)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
if `recentf-max-saved-items' is too small, set it to 500.")
;; (anything 'anything-c-source-recentf)

;;; ffap
(eval-when-compile (require 'ffap))
(defvar anything-c-source-ffap-guesser
  '((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (anything-aif
                        (with-current-buffer anything-current-buffer
                          (ffap-guesser))
                        (list it))))
    (type . file)))
;; (anything 'anything-c-source-ffap-guesser)

;;; ffap with line number
(defun anything-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (anything-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defvar anything-c-ffap-line-location nil
  "(FILENAME . LINENO) used by `anything-c-source-ffap-line'.
It is cleared after jumping line.")

(defun anything-c-ffap-line-candidates ()
  (with-current-buffer anything-current-buffer
    (setq anything-c-ffap-line-location (anything-c-ffap-file-line-at-point)))
  (when anything-c-ffap-line-location
    (destructuring-bind (file . line) anything-c-ffap-line-location
      (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `anything-c-source-ffap-line'.
(defun anything-c-ffap-line-goto-line ()
  (when (car anything-c-ffap-line-location)
    (unwind-protect
        (ignore-errors
          (with-selected-window (get-buffer-window
                                 (get-file-buffer (car anything-c-ffap-line-location)))
            (goto-line (cdr anything-c-ffap-line-location))))
      (setq anything-c-ffap-line-location nil))))
(add-hook 'anything-after-action-hook 'anything-c-ffap-line-goto-line)

(defvar anything-c-source-ffap-line
  '((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . anything-c-ffap-line-candidates)
    (type . file)))
;; (anything 'anything-c-source-ffap-line)


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
                            (ignore-errors
                              (require 'woman)
                              (woman-file-name "")
                              (sort (mapcar 'car woman-topic-all-completions)
                                    'string-lessp))))))
    (action  ("Show with Woman" . woman))
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
    (action . (lambda (candidate)
                (Info-find-node "cl" "Function Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (volatile)
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-cl)

;;;; <Command>
;;; Complex command history
(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda () (mapcar 'prin1-to-string command-history)))
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
    (init . (lambda () (require 'lacarte )))
    (candidates . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
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
                      (mapatoms (lambda (a) (if (functionp a)
                                                (push (symbol-name a) commands))))
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
          '((match anything-c-match-function-by-abbrev
                   anything-c-string-match))
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

;;;; <Variable>
;;; Emacs variables
(defvar anything-c-source-emacs-variables
  '((name . "Emacs Variables")
    (candidates . (lambda ()
                    (sort (all-completions "" obarray 'boundp) 'string-lessp)))
    (type . variable)
    (requires-pattern . 2))
  "Source for completing Emacs variables.")
;; (anything 'anything-c-source-emacs-variables)

;;;; <Bookmark>
;;; Bookmarks
(eval-when-compile (require 'bookmark))
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
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmark-set)

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar anything-c-source-bm
  '((name . "Visible Bookmarks")
    (init . anything-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun anything-c-bm-init ()
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

;;; Special bookmarks
(defvar anything-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-ssh)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-ssh (loop for i in lis-all
                                          if (string-match "^(ssh)" i)
                                          collect i))
                      (sort lis-ssh 'string-lessp))))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-ssh)

(defvar anything-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-su)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-su (loop for i in lis-all
                                         if (string-match (format "^(%s)" anything-su-or-sudo) i)
                                         collect i))
                      (sort lis-su 'string-lessp))))
    (candidate-transformer anything-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-su)

(defface anything-bookmarks-su-face '((t (:foreground "red")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)

(defvar anything-c-bookmarks-face1 'anything-dir-heading)
(defvar anything-c-bookmarks-face2 'anything-file-name)
(defvar anything-c-bookmarks-face3 'anything-bookmarks-su-face)

(defun tv-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." anything-su-or-sudo) i)
        (throw 'break t)))))


(defun anything-c-highlight-bookmark-su (files)
  (if (tv-root-logged-p)
      (anything-c-highlight-bookmark files)
    (anything-c-highlight-not-logged files)))

(defun anything-c-highlight-not-logged (files)
  (loop for i in files
        collect (propertize i 'face anything-c-bookmarks-face3)))

(defun anything-c-highlight-bookmark (bookmarks)
  "Colors mean:
Grey ==> non--buffer-filename with saved region or not.
Yellow ==> w3m url with saved region.
Magenta ==> Gnus buffer.
Green ==> info buffer with saved region.
Blue ==> regular file with maybe a region saved.
RedOnWhite ==> Directory."
  (loop for i in bookmarks
     for pred      = (bookmark-get-filename i)
     for bufp      = (and (fboundp 'bookmarkp-get-buffer-name)
                          (bookmarkp-get-buffer-name i))
     for regp      = (and (fboundp 'bookmarkp-get-end-position)
                          (bookmarkp-get-end-position i)
                          (/= (bookmark-get-position i)
                              (bookmarkp-get-end-position i)))
     for handlerp  = (and (fboundp 'bookmark-get-handler)
                          (bookmark-get-handler i))
     for isannotation  = (bookmark-get-annotation i)       
     if (and isannotation (not (string-equal isannotation "")))
     do (setq i (concat "*" i))
     ;; info buffers
     if (and (fboundp 'bookmarkp-get-buffer-name)
             (eq handlerp 'Info-bookmark-jump)
             (string= bufp "*info*"))
     collect (propertize i 'face '((:foreground "green")) 'help-echo pred)
     ;; w3m buffers
     if (and (fboundp 'bookmarkp-get-buffer-name)
             (string= bufp "*w3m*"))
     collect (propertize i 'face '((:foreground "yellow")) 'help-echo pred)
     ;; gnus buffers
     if (eq handlerp 'bookmarkp-jump-gnus)
     collect (propertize i 'face '((:foreground "magenta")) 'help-echo pred)
     ;; directories
     if (and pred 
             (file-directory-p pred))
     collect (propertize i 'face anything-c-bookmarks-face1 'help-echo pred)
     ;; regular files with regions saved
     if (and pred 
             (not (file-directory-p pred))
             (file-exists-p pred)
             regp)
     collect (propertize i 'face '((:foreground "Indianred2")) 'help-echo pred)
     ;; regular files
     if (and pred 
             (not (file-directory-p pred))
             (file-exists-p pred)
             (not regp))
     collect (propertize i 'face anything-c-bookmarks-face2 'help-echo pred)
     ;; buffer non--filename
     if (and (fboundp 'bookmarkp-get-buffer-name)
             bufp
             (not (bookmark-get-handler i))
             (if pred (not (file-exists-p pred)) (not pred)))
     collect (propertize i 'face '((:foreground "grey")))))
       

(defvar anything-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-loc)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-loc (loop for i in lis-all
                                          if (and (not (string-match "^(ssh)" i))
                                                  (not (string-match "^(su)" i)))
                                          collect i))
                      (sort lis-loc 'string-lessp))))
    (candidate-transformer anything-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-local)

;;; Sources to filter bookmark+ bookmarks.
;; Dependency: http://www.emacswiki.org/cgi-bin/emacs/bookmark+.el

(when (require 'bookmark+ nil t)
  (bookmark-maybe-load-default-file)
  
  (defun anything-c-bookmark+-filter-setup-alist (fn &rest args)
    "Return a filtered `bookmark-alist' using one of the bookmark+ filters functions."
    (loop
       with alist = (if args
                        (apply #'(lambda (x) (funcall fn x)) args)
                        (funcall fn))
       for i in alist
       for b = (car i)
       collect b into sa
       finally return (sort sa 'string-lessp)))

  ;; Regions
  (defvar anything-c-source-bookmark-regions
    '((name . "Bookmark Regions")
      (candidates . anything-c-bookmark-region-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark)
      (type . bookmark)))

  ;; (anything 'anything-c-source-bookmark-regions)

  (defun anything-c-bookmark-region-setup-alist ()
    "Specialized filter function for bookmark+ regions."
    (anything-c-bookmark+-filter-setup-alist 'bookmarkp-region-alist-only))

  ;; W3m
  (defvar anything-c-source-bookmark-w3m
    '((name . "Bookmark W3m")
      (candidates . anything-c-bookmark-w3m-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark)
      (type . bookmark)))

  ;; (anything 'anything-c-source-bookmark-w3m)

  (defun anything-c-bookmark-w3m-setup-alist ()
    "Specialized filter function for bookmark+ w3m."
    (anything-c-bookmark+-filter-setup-alist 'bookmarkp-w3m-alist-only))

  ;; Gnus
  (defvar anything-c-source-bookmark-gnus
    '((name . "Bookmark Gnus")
      (candidates . anything-c-bookmark-gnus-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark)
      (type . bookmark)))
  ;; (anything 'anything-c-source-bookmark-gnus)

  (defun anything-c-bookmark-gnus-setup-alist ()
    "Specialized filter function for bookmark+ gnus."
    (anything-c-bookmark+-filter-setup-alist 'bookmarkp-gnus-alist-only))

  ;; Info
  (defvar anything-c-source-bookmark-info
    '((name . "Bookmark Info")
      (candidates . anything-c-bookmark-info-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark)
      (type . bookmark)))
  ;; (anything 'anything-c-source-bookmark-info)

  (defun anything-c-bookmark-info-setup-alist ()
    "Specialized filter function for bookmark+ info."
    (anything-c-bookmark+-filter-setup-alist 'bookmarkp-info-alist-only))

  ;; Local Files&directories
  (defvar anything-c-source-bookmark-files&dirs
    '((name . "Bookmark Files&Directories")
      (candidates . anything-c-bookmark-local-files-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark)
      (type . bookmark)))
  ;; (anything 'anything-c-source-bookmark-files&dirs)

  (defun anything-c-bookmark-local-files-setup-alist ()
    "Specialized filter function for bookmark+ locals files."
    (anything-c-bookmark+-filter-setup-alist 'bookmarkp-local-file-alist-only))

  ;; Su Files&directories

  (defun anything-c-highlight-bookmark+-su (bmk)
    (if (bookmarkp-root-or-sudo-logged-p)
        (anything-c-highlight-bookmark bmk)
        (anything-c-highlight-not-logged bmk)))

  (defvar anything-c-source-bookmark-su-files&dirs
    '((name . "Bookmark Root-Files&Directories")
      (candidates . anything-c-bookmark-su-files-setup-alist)
      (candidate-transformer anything-c-highlight-bookmark+-su)
      (type . bookmark)))
  ;; (anything 'anything-c-source-bookmark-su-files&dirs)

  (defun anything-c-bookmark-su-files-setup-alist ()
    "Specialized filter function for bookmark+ su/sudo files."
    (loop
       with l = (anything-c-bookmark+-filter-setup-alist 'bookmarkp-remote-file-alist-only)
       for i in l
       for isfile = (bookmark-get-filename i)
       for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                          (save-match-data
                            (string-match tramp-file-name-regexp isfile)))
       for issu = (and istramp
                       (string-match bookmarkp-su-or-sudo-regexp isfile))
       if issu
       collect i))

  ;; Ssh Files&directories
  (defvar anything-c-source-bookmark-ssh-files&dirs
    '((name . "Bookmark Ssh-Files&Directories")
      (candidates . anything-c-bookmark-ssh-files-setup-alist)
      (type . bookmark)))
  ;; (anything 'anything-c-source-bookmark-ssh-files&dirs)

  (defun anything-c-bookmark-ssh-files-setup-alist ()
    "Specialized filter function for bookmark+ ssh files."
    (loop
       with l = (anything-c-bookmark+-filter-setup-alist 'bookmarkp-remote-file-alist-only)
       for i in l
       for isfile = (bookmark-get-filename i)
       for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                          (save-match-data
                            (string-match tramp-file-name-regexp isfile)))
       for isssh = (and istramp
                        (string-match "/ssh:" isfile))
       if isssh
       collect i))

  ;; All bookmark+ sources.
  (defun anything-bookmark+ ()
    "Preconfigured anything for bookmark+ sources."
    (interactive)
    (anything '(anything-c-source-bookmark-files&dirs
                anything-c-source-bookmark-w3m
                anything-c-source-bookmark-gnus
                anything-c-source-bookmark-regions
                anything-c-source-bookmark-su-files&dirs
                anything-c-source-bookmark-ssh-files&dirs))))


;; W3m bookmark
(eval-when-compile (require 'w3m-bookmark nil t))
(unless (and (require 'w3m nil t)
             (require 'w3m-bookmark nil t))
  (defvar w3m-bookmark-file "~/.w3m/bookmark.html"))
;; (defvar anything-w3m-bookmarks-regexp ">[^><]+[^</a>]+[a-z)0-9]+")

(defface anything-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'anything)

(defvar anything-w3m-bookmarks-regexp ">[^><]+.[^</a>]")
(defun anything-w3m-bookmarks-to-alist ()
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents w3m-bookmark-file) ;; or w3m-bookmark-file
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (when (re-search-forward "href=" nil t)
          (beginning-of-line)
          (when (re-search-forward "\\(http\\|file\\)://[^>]*" nil t)
            (setq url (concat "\"" (match-string 0))))
          (beginning-of-line)
          (when (re-search-forward anything-w3m-bookmarks-regexp nil t)
            (setq title (match-string 0)))
          (push (cons title url) bookmarks-alist))))
    (reverse bookmarks-alist)))

(defvar anything-c-w3m-bookmarks-alist nil)
(defvar anything-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq anything-c-w3m-bookmarks-alist
                    (anything-w3m-bookmarks-to-alist))))
    (candidates . (lambda ()
                    (mapcar #'car
                            anything-c-w3m-bookmarks-alist)))
    (candidate-transformer anything-c-highlight-w3m-bookmarks)
    (action . (("Browse Url" . (lambda (candidate)
                                 (anything-c-w3m-browse-bookmark candidate)))
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
    (delayed)))

;; (anything 'anything-c-source-w3m-bookmarks)

(defun anything-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string "\"" ""
                            (cdr (assoc elm
                                        anything-c-w3m-bookmarks-alist))))


(defun anything-c-w3m-browse-bookmark (elm &optional use-firefox new-tab)
  (let* ((fn (if use-firefox
                 'browse-url-firefox
               'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url)
                   new-tab)))
    (funcall fn (anything-c-w3m-bookmarks-get-value elm) arg)))


(defun anything-c-highlight-w3m-bookmarks (books)
  (loop for i in books
        collect (propertize i
                            'face 'anything-w3m-bookmarks-face
                            'help-echo (anything-c-w3m-bookmarks-get-value i))))


(defun anything-c-w3m-delete-bookmark (elm)
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

;;;; <Library>
;;; Elisp library scan
(defvar anything-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (anything-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library" . (lambda (candidate)
                                (find-file (find-library-name candidate))))
            ("Find library other window" . (lambda (candidate)
                                             (find-file-other-window (find-library-name candidate))))
            ("Load library" . (lambda (candidate)
                                (load-library candidate))))))
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

(eval-when-compile (require 'imenu))
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
    (candidates . anything-c-imenu-candidates)
    (volatile)
    (persistent-action . (lambda (elm)
                           (anything-c-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (anything-match-line-color-current-line))))
    (action . anything-c-imenu-default-action))
  "See (info \"(emacs)Imenu\")")

;; (anything 'anything-c-source-imenu)

(defun anything-c-imenu-candidates ()
  (with-current-buffer anything-current-buffer
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
                      anything-c-cached-imenu-candidates))))))

(setq imenu-default-goto-function 'imenu-default-goto-function)
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
    (init . anything-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")
;; (anything 'anything-c-source-ctags)

;; Semantic
(eval-when-compile (require 'semantic nil t))
(defun anything-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply 'append
           (mapcar (lambda (tag)
                     (if (listp tag)
                         (let ((type (semantic-tag-type tag))
                               (class (semantic-tag-class tag)))
                           (if (or (and (stringp type)
                                        (string= type "class"))
                                   (eq class 'function)
                                   (eq class 'variable))
                               (cons (cons (concat (make-string (* depth 2) ?\s)
                                                   (semantic-format-tag-summarize tag nil t)) tag)
                                     (anything-semantic-construct-candidates (semantic-tag-components tag)
                                                                             (1+ depth)))))))
                   tags))))

(defun anything-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
    (semantic-go-to-tag tag)))

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
    (persistent-action . (lambda (elm)
                           (anything-semantic-default-action elm)
                           (anything-match-line-color-current-line)))
    (action . anything-semantic-default-action)
  "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))

;; (anything 'anything-c-source-semantic)

;;; Function is called by
(defvar anything-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . anything-c-simple-call-tree-functions-callers-init)
    (delayed)
    (candidates-in-buffer))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun anything-c-simple-call-tree-functions-callers-init ()
  (require 'simple-call-tree)
  (with-no-warnings
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
;; (anything 'anything-c-source-simple-call-tree-functions-callers)

;;; Function calls
(defvar anything-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . anything-c-simple-call-tree-callers-functions-init)
    (delayed)
    (candidates-in-buffer))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun anything-c-simple-call-tree-callers-functions-init ()
  (require 'simple-call-tree)
  (with-no-warnings
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

;; (anything 'anything-c-source-simple-call-tree-callers-functions)

;;; Commands/Options with doc
(defvar anything-c-auto-document-data nil)
(make-variable-buffer-local 'anything-c-auto-document-data)
(defvar anything-c-source-commands-and-options-in-file
  '((name . "Commands/Options in file")
    (header-name
     . (lambda (x) (format "Commands/Options in %s"
                           (buffer-local-value 'buffer-file-name anything-current-buffer))))
    (candidates . anything-command-and-options-candidates)
    (multiline)
    (action . imenu))
  "List Commands and Options with doc. It needs auto-document.el .

http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el")

(eval-when-compile (require 'auto-document nil t))
(defun anything-command-and-options-candidates ()
  (with-current-buffer anything-current-buffer
    (when (and (require 'auto-document nil t)
               (eq major-mode 'emacs-lisp-mode)
               (or (anything-current-buffer-is-modified)
                   (not anything-c-auto-document-data)))
      (or imenu--index-alist (imenu--make-index-alist t))
      (setq anything-c-auto-document-data
            (destructuring-bind (commands options)
                (adoc-construct anything-current-buffer)
              (append
               (loop for (command . doc) in commands
                     for cmdname = (symbol-name command)
                     collect
                     (cons (format "Command: %s\n %s"
                                   (propertize cmdname 'face font-lock-function-name-face)
                                   (adoc-first-line doc))
                           (assoc cmdname imenu--index-alist)))
               (loop with var-alist = (cdr (assoc "Variables" imenu--index-alist))
                     for (option doc default) in options
                     for optname = (symbol-name option)
                     collect
                     (cons (format "Option: %s\n %s\n default = %s"
                                   (propertize optname 'face font-lock-variable-name-face)
                                   (adoc-first-line doc)
                                   (adoc-prin1-to-string default))
                           (assoc optname
                                  var-alist)))))))
    anything-c-auto-document-data))

;; (anything 'anything-c-source-commands-and-options-in-file)

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
    (requires-pattern . 3))
  "See (info \"(emacs)Faces\")")
;; (anything 'anything-c-source-customize-face)

;; Color
(defvar anything-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (anything-candidate-buffer)
                         (save-window-excursion (list-colors-display))
                         (anything-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action ("Copy Name" . (lambda (candidate)
                             (kill-new (anything-c-colors-get-name candidate))))
            ("Copy RGB" . (lambda (candidate)
                            (kill-new (anything-c-colors-get-rgb candidate))))
            ("Insert Name" . (lambda (candidate)
                               (with-current-buffer anything-current-buffer
                                 (insert (anything-c-colors-get-name candidate)))))
            ("Insert RGB" . (lambda (candidate)
                              (with-current-buffer anything-current-buffer
                                (insert (anything-c-colors-get-rgb candidate))))))
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
                    (start-process "mdfind-process" nil "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")
;; (anything 'anything-c-source-mac-spotlight)

;;;; <icicle>
;;; Icicle regions
;; See: http://www.emacswiki.org/emacs-en/Icicles_-_Multiple_Regions 
;; That is the anything interface.

(defvar anything-icicle-region-alist nil)
(defvar anything-c-source-icicle-region
  '((name . "Icicle Regions")
    (init . (lambda ()
              (setq anything-icicle-region-alist
                    (loop
                       for i in icicle-region-alist
                       collect (concat (car i) " => " (cadr i))))))
    (candidates . anything-icicle-region-alist)
    (action . (("Go to region" . anything-c-icicle-region-goto-region)
               ("Insert region at point" . (lambda (elm)
                                             (let (reg)
                                               (save-window-excursion
                                                 (anything-c-icicle-region-goto-region elm)
                                                 (setq reg (buffer-substring (mark) (point))))
                                               (insert reg))))
               ("Remove region" . anything-c-icicle-region-delete-region)
               ("Update" . (lambda (elm)
                             (icicle-purge-bad-file-regions)))))))

;; (anything 'anything-c-source-icicle-region)

(defun anything-icicle-select-region-action (pos)
  "Go to the region at nth `pos' in `icicle-region-alist'.
See `icicle-select-region-action'."
  (let ((icicle-get-alist-candidate-function #'(lambda (pos)
                                                 (nth pos icicle-region-alist))))
    (icicle-select-region-action pos)))

(defun anything-icicle-delete-region-from-alist (pos)
  "Delete the region at nth `pos' from `icicle-region-alist'.
See `icicle-delete-region-from-alist'."
  (let ((alist-cand  (nth pos icicle-region-alist)))
    (setq icicle-region-alist
          (delete alist-cand icicle-region-alist)))
  (funcall icicle-customize-save-variable-function 'icicle-region-alist icicle-region-alist))

(defun anything-c-icicle-region-goto-region (candidate)
  "Get the position of `candidate' and call `anything-icicle-select-region-action'." 
  (let ((pos (position candidate anything-icicle-region-alist))
        (buf (second (split-string candidate " => "))))
    (if (equal buf "*info*")
        (info (caddr (nth pos icicle-region-alist))))
    (anything-icicle-select-region-action pos)))

(defun anything-c-icicle-region-delete-region (candidate)
  "Get the position of `candidate' and call `anything-icicle-delete-region-from-alist'."
  (let ((pos (position candidate anything-icicle-region-alist)))
    (anything-icicle-delete-region-from-alist pos)))


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
    (multiline))
  "Source for browse and insert contents of kill-ring.")

(defun anything-c-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank', replace with STR as yanked string."
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

;;;; <Mark ring>
;; DO NOT include these sources in `anything-sources' use
;; the commands `anything-mark-ring' and `anything-global-mark-ring' instead.

(defun anything-c-source-mark-ring-candidates ()
  (flet ((get-marks (pos)
           (save-excursion
             (goto-char pos)
             (beginning-of-line)
             (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (when (string= "" line)
                 (setq line  "<EMPTY LINE>"))
               (format "%7d: %s" (line-number-at-pos) line)))))
    (with-current-buffer anything-current-buffer
      (loop
         with marks = (cons (mark-marker) mark-ring)
         with recip = nil
         for i in marks
         for f = (get-marks i) 
         if (not (member f recip))
         do
           (push f recip)
         finally (return (reverse recip))))))
           
(defvar anything-mark-ring-cache nil)
(defvar anything-c-source-mark-ring
  '((name . "mark-ring")
    (init . (lambda ()
              (setq anything-mark-ring-cache
                    (anything-c-source-mark-ring-candidates))))
    (candidates . (lambda ()
                    (anything-aif anything-mark-ring-cache
                        it)))
    (action . (("Goto line" . (lambda (candidate)
                                (goto-line (string-to-number candidate))))))
    (persistent-action . (lambda (candidate)
                           (goto-line (string-to-number candidate))
                           (anything-match-line-color-current-line)))))

;; (anything 'anything-c-source-mark-ring)

(defun anything-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-mark-ring'."
  (interactive)
  (anything 'anything-c-source-mark-ring))

;;; Global-mark-ring
(defvar anything-c-source-global-mark-ring
  '((name . "global-mark-ring")
    (candidates . anything-c-source-global-mark-ring-candidates)
    (action . (("Goto line" . (lambda (candidate)
                                (let ((items (split-string candidate ":")))
                                  (switch-to-buffer (second items))
                                  (goto-line (string-to-number (car items))))))))
    (persistent-action . (lambda (candidate)
                           (let ((items (split-string candidate ":")))
                             (switch-to-buffer (second items))
                             (goto-line (string-to-number (car items)))
                             (anything-match-line-color-current-line))))))
                             
(defun anything-c-source-global-mark-ring-candidates ()
  (flet ((buf-fn (m)
           (with-current-buffer (marker-buffer m)
             (goto-char m)
             (beginning-of-line)
             (let (line)
               (if (string= "" line)
                   (setq line  "<EMPTY LINE>")
                   (setq line (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (format "%7d:%s:    %s" (line-number-at-pos) (marker-buffer m) line)))))
    (loop
       with marks = global-mark-ring
       with recip = nil  
       for i in marks
       if (not (or (string-match "^ " (format "%s" (marker-buffer i)))
                   (null (marker-buffer i))))
       for a = (buf-fn i)
       if (and a (not (member a recip)))
       do
         (push a recip)
       finally (return (reverse recip)))))

;; (anything 'anything-c-source-global-mark-ring)

(defun anything-global-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-global-mark-ring'."
  (interactive)
  (anything 'anything-c-source-global-mark-ring))

;;;; <Register>
;;; Insert from register
(defvar anything-c-source-register
  '((name . "Registers")
    (candidates . anything-c-register-candidates)
    (action-transformer . anything-c-register-action-transformer)
    (multiline)
    (action))
  "See (info \"(emacs)Registers\")")

(defun anything-c-register-candidates ()
  "Collecting register contents and appropriate commands."
  (loop for (char . val) in register-alist
        for key    = (single-key-description char)
        for string-actions = (cond
                              ((numberp val)
                               (list (int-to-string val)
                                     'insert-register
                                     'increment-register))
                              ((markerp val)
                               (let ((buf (marker-buffer val)))
                                 (if (null buf)
                                     (list "a marker in no buffer")
                                   (list (concat
                                          "a buffer position:"
                                          (buffer-name buf)
                                          ", position "
                                          (int-to-string (marker-position val)))
                                         'jump-to-register
                                         'insert-register))))
                              ((and (consp val) (window-configuration-p (car val)))
                               (list "window configuration."
                                     'jump-to-register))
                              ((and (consp val) (frame-configuration-p (car val)))
                               (list "frame configuration."
                                     'jump-to-register))
                              ((and (consp val) (eq (car val) 'file))
                               (list (concat "file:"
                                             (prin1-to-string (cdr val))
                                             ".")
                                     'jump-to-register))
                              ((and (consp val) (eq (car val) 'file-query))
                               (list (concat "file:a file-query reference: file "
                                             (car (cdr val))
                                             ", position "
                                             (int-to-string (car (cdr (cdr val))))
                                             ".")
                                     'jump-to-register))
                              ((consp val)
                               (let ((lines (format "%4d" (length val))))
                                 (list (format "%s: %s\n" lines
                                               (truncate-string-to-width
                                                (mapconcat 'identity (list (car val))
                                                           ;; (mapconcat (lambda (y) y) val
                                                           "^J") (- (window-width) 15)))
                                       'insert-register)))
                              ((stringp val)
                               (list ;; without properties
                                (substring-no-properties val)
                                'insert-register
                                'append-to-register
                                'prepend-to-register))
                              (t
                               "GARBAGE!"))
        collect (cons (format "register %3s: %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun anything-c-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (loop with func-actions =
        '((insert-register
           "Insert Register" .
           (lambda (c) (insert-register (car c))))
          (jump-to-register
           "Jump to Register" .
           (lambda (c) (jump-to-register (car c))))
          (append-to-register
           "Append Region to Register" .
           (lambda (c) (append-to-register (car c) (region-beginning) (region-end))))
          (prepend-to-register
           "Prepend Region to Register" .
           (lambda (c) (prepend-to-register (car c) (region-beginning) (region-end))))
          (increment-register
           "Increment Prefix Arg to Register" .
           (lambda (c) (increment-register anything-current-prefix-arg (car c)))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        collect (cdr cell)))

;; (anything 'anything-c-source-register)

;;;; <Headline Extraction>
(defvar anything-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")
;; (anything 'anything-c-source-fixme)

(defvar anything-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")
;; (anything 'anything-c-source-rd-headline)

(defvar anything-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
               "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")
;; (anything 'anything-c-source-oddmuse-headline)

(defvar anything-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$" (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")
;; (anything 'anything-c-source-emacs-source-defun)

(defvar anything-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (anything 'anything-c-source-emacs-lisp-expectations)

(defvar anything-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")
;; (anything 'anything-c-source-emacs-lisp-toplevels)

(defvar anything-c-source-org-headline
  '((name . "Org HeadLine")
    (headline
     "^\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$")
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)
    (persistent-action . (lambda (elm)
                           (anything-c-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to Line" . anything-c-action-line-goto)
           ("Insert Link to This Headline" . anything-c-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun anything-c-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (goto-line (car lineno-and-content))
     (and (looking-at "^\\*+ \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$")
          (org-make-link-string (concat "*" (match-string 1)))))))

;; (anything 'anything-c-source-org-headline)

;;; Anything yaoddmuse
;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar anything-yaoddmuse-use-cache-file nil)
(defvar anything-c-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar anything-c-yaoddmuse-ew-cache nil)
(defvar anything-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . (lambda ()
                    (if anything-yaoddmuse-use-cache-file
                        (condition-case nil
                            (progn
                              (unless anything-c-yaoddmuse-ew-cache
                                (load anything-c-yaoddmuse-cache-file)
                                (setq anything-c-yaoddmuse-ew-cache
                                      (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                              anything-c-yaoddmuse-ew-cache)
                          (error nil))
                        (yaoddmuse-update-pagename t)
                        (gethash "EmacsWiki" yaoddmuse-pages-hash))))
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page" . (lambda (candidate)
                                  (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window" . (lambda (candidate)
                                               (if (one-window-p)
                                                   (split-window-vertically))
                                               (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff" . (lambda (candidate)
                                  (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL" . (lambda (candidate)
                               (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                               (message "Have copy page %s's URL to yank." candidate)))
               ("Create page" . (lambda (candidate)
                                  (yaoddmuse-edit "EmacsWiki" anything-input)))
               ("Update cache" . (lambda (candidate)
                                   (if anything-yaoddmuse-use-cache-file
                                       (progn
                                         (anything-yaoddmuse-cache-pages t)
                                         (setq anything-c-yaoddmuse-ew-cache
                                               (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                                       (yaoddmuse-update-pagename))))))
    (action-transformer anything-c-yaoddmuse-action-transformer))) 

;; (anything 'anything-c-source-yaoddmuse-emacswiki-edit-or-view)

(defvar anything-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (anything-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library and Browse" . (lambda (candidate)
                                              (yaoddmuse-post-file (find-library-name candidate)
                                                                   "EmacsWiki"
                                                                   (file-name-nondirectory (find-library-name candidate))
                                                                   nil t)))
               ("Post library" . (lambda (candidate)
                                   (yaoddmuse-post-file (find-library-name candidate)
                                                        "EmacsWiki"
                                                        (file-name-nondirectory (find-library-name candidate)))))))))

;; (anything 'anything-c-source-yaoddmuse-emacswiki-post-library)

(defun anything-c-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp" . (lambda (elm)
                                             (install-elisp-from-emacswiki elm)))))
      actions))

(defun anything-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file anything-c-yaoddmuse-cache-file)
    (erase-buffer)
    (insert "(puthash \"EmacsWiki\" '(")
    (loop for i in (gethash "EmacsWiki" yaoddmuse-pages-hash)
       do
          (insert (concat "(\"" (car i) "\") ")))
    (insert ") yaoddmuse-pages-hash)\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (when (or current-prefix-arg
              load)
      (load anything-c-yaoddmuse-cache-file))))

(defun anything-yaoddmuse-emacswiki-edit-or-view ()
  "Edit or View EmacsWiki page."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-edit-or-view))

(defun anything-yaoddmuse-emacswiki-post-library ()
  "Post library to EmacsWiki."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-post-library))

(defun anything-yaoddmuse-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer anything-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))

;;; Eev anchors
(defvar anything-c-source-eev-anchor
  '((name . "Anchors")
    (init . (lambda ()
              (setq anything-c-eev-anchor-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (condition-case nil
                        (save-excursion
                          (with-current-buffer anything-c-eev-anchor-buffer
                            (goto-char (point-min))
                            (let (anchors)
                              (while (re-search-forward (format ee-anchor-format "\\([^\.].+\\)") nil t)
                                (push (match-string-no-properties 1) anchors))
                              (setq anchors (reverse anchors)))))
                      (error nil))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (anything-match-line-color-current-line)))
    (action . (("Goto link" . (lambda (item)
                                (ee-to item)))))))

;; (anything 'anything-c-source-eev-anchor)

;;;; <Misc>
;;; Picklist
(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
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
    (action ("Copy result to kill-ring" . kill-new))))
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
  (let ((start nil) items)
    (while (string-match "\"\\([^\"]+?\\)\"" str start)
      (push (match-string 1 str) items)
      (setq start (1+ (match-end 1))))
    items))

;;; Surfraw
;;; Need external program surfraw.
;;; http://surfraw.alioth.debian.org/
;; user variables
(defvar anything-c-surfraw-favorites '("google" "wikipedia"
                                       "yahoo" "translate"
                                       "codesearch" "genpkg"
                                       "genportage" "fast" 
                                       "currency")
  "All elements of this list will appear first in results.")
(defvar anything-c-surfraw-use-only-favorites nil
  "If non-nil use only `anything-c-surfraw-favorites'.")


(defun anything-c-build-elvi-alist ()
  "Build elvi alist.
A list of search engines."
  (let* ((elvi-list
          (with-temp-buffer
            (call-process "surfraw" nil t nil
                          "-elvi")
            (split-string (buffer-string) "\n")))
         (elvi-alist
          (let (line)
            (loop for i in elvi-list
               do
               (setq line (split-string i))
               collect (cons (first line) (mapconcat #'(lambda (x) x) (cdr line) " "))))))
    elvi-alist))

(defun anything-c-surfraw-sort-elvi (&optional only-fav)
  "Sort elvi alist according to `anything-c-surfraw-favorites'."
  (let* ((elvi-alist (anything-c-build-elvi-alist))
         (fav-alist (loop for j in anything-c-surfraw-favorites
                      collect (assoc j elvi-alist)))
         (rest-elvi (loop for i in elvi-alist
                         if (not (member i fav-alist))
                         collect i)))
    (if only-fav
        fav-alist
        (append fav-alist rest-elvi))))

(defun anything-c-surfraw-get-url (engine pattern)
  "Get search url from `engine' for `anything-pattern'."
  (with-temp-buffer
    (apply #'call-process "surfraw" nil t nil
           `(,engine
             "-p"
             ,anything-pattern))
    (buffer-string)))


(defvar anything-c-surfraw-elvi nil)
(defvar anything-c-surfraw-cache nil)
(defvar anything-c-source-surfraw
  '((name . "Surfraw")
    (init . (lambda ()
              (unless anything-c-surfraw-cache
                (setq anything-c-surfraw-elvi (anything-c-surfraw-sort-elvi
                                               anything-c-surfraw-use-only-favorites))
                (setq anything-c-surfraw-cache
                      (loop for i in anything-c-surfraw-elvi 
                         if (car i)
                         collect (car i))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-surfraw-cache
                       for s = (anything-c-surfraw-get-url i anything-pattern)
                       collect (concat (propertize i
                                                   'face '((:foreground "green"))
                                                   'help-echo (cdr (assoc i anything-c-surfraw-elvi)))
                                       ">>>" (replace-regexp-in-string "\n" "" s)))))
    (action . (("Browse" . (lambda (candidate)
                             (let ((url (second (split-string candidate ">>>"))))
                               (browse-url url))))
               ("Browse firefox" . (lambda (candidate)
                                     (let ((url (second (split-string candidate ">>>"))))
                                       (browse-url-firefox url t))))))
    (volatile)
    (requires-pattern . 3)
    (multiline)
    (delayed)))

;; (anything 'anything-c-source-surfraw)

;;; Emms

(defun anything-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from anything."
  (interactive)
  (let* ((cur-buf anything-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name     (read-from-minibuffer "Description: "
                                         (nth 0 bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (nth 1 bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (nth 2 bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (car (last bookmark))))))
    (save-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-add-bookmark name url (string-to-number fd) type)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (switch-to-buffer cur-buf)))))

(defun anything-emms-stream-delete-bookmark (elm)
  "Delete an emms-stream bookmark from anything."
  (interactive)
  (let* ((cur-buf anything-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name (nth 0 bookmark)))
    (save-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (switch-to-buffer cur-buf)))))

(defvar anything-c-source-emms-streams
  '((name . "Emms Streams")
    (init . (lambda ()
              (emms-stream-init)))
    (candidates . (lambda ()
                    (mapcar 'car emms-stream-list)))
    (action . (("Play" . (lambda (elm)
                           (let* ((stream (assoc elm emms-stream-list))
                                  (fn (intern (concat "emms-play-" (symbol-name (car (last stream))))))
                                  (url (second stream)))
                             (funcall fn url))))
               ("Delete" . anything-emms-stream-delete-bookmark)
               ("Edit" . anything-emms-stream-edit-bookmark)))
    (volatile)))
;; (anything 'anything-c-source-emms-streams)

;; Don't forget to set `emms-source-file-default-directory'
(defvar anything-c-source-emms-dired
  '((name . "Music Directory")
    (candidates . (lambda ()
                    (cddr (directory-files emms-source-file-default-directory))))
    (action . (("Play Directory" . (lambda (item)
                                     (emms-play-directory
                                      (expand-file-name item
                                                        emms-source-file-default-directory))))
               ("Open dired in file's directory" . (lambda (item)
                                                     (anything-c-open-dired
                                                      (expand-file-name item
                                                                        emms-source-file-default-directory))))))
    (volatile)))
;; (anything 'anything-c-source-emms-dired)

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
    (candidates . (lambda () (mapcar 'car (anything-c-jabber-online-contacts))))
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
                  (setq anything-candidate-number-limit 9999)
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

;; Do many actions for input
(defvar anything-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (candidate-number-limit . 9999)
    (action-transformer . anything-create--actions))
  "Do many create actions from `anything-pattern'.
See also `anything-create--actions'.")
;; (anything 'anything-c-source-create)

(defun anything-create-from-anything ()
  "Run `anything-create' from `anything' as a fallback."
  (interactive)
  (anything-run-after-quit 'anything-create nil anything-pattern))

(defun anything-create (&optional string initial-input)
  "Do many create actions from STRING.
See also `anything-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Anything: " initial-input)))
  (anything '(((name . "Anything Create")
               (header-name . (lambda (_) (format "Action for \"%s\"" string)))
               (candidates . anything-create--actions)
               (candidate-number-limit . 9999)
               (action . (lambda (func) (funcall func string)))))))

(defun anything-create--actions (&rest ignored)
  "Default actions for `anything-create' / `anything-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append anything-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . switch-to-buffer)
             ("New buffer other window" . switch-to-buffer-other-window)
             ("Bookmark Set" . bookmark-set)
             ("Set Register" .
              (lambda (x) (set-register (read-char "Register: ") x)))
             ("Insert Linkd star" . linkd-insert-star)
             ("Insert Linkd Tag" . linkd-insert-tag)
             ("Insert Linkd Link" . linkd-insert-link)
             ("Insert Linkd Lisp" . linkd-insert-lisp)
             ("Insert Linkd Wiki" . linkd-insert-wiki)
             ("Google Search" . google)))))

;; Minibuffer History
(defvar anything-c-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name) (format "%s (%s)" name minibuffer-history-variable)))
    (candidates . (lambda () (let ((history (symbol-value minibuffer-history-variable)))
                               (if (consp (car history))
                                   (mapcar 'prin1-to-string history)
                                 history))))
    (migemo)
    (action . insert)))
;; (anything 'anything-c-source-minibuffer-history)

;; elscreen
(defvar anything-c-source-elscreen
  '((name . "Elscreen")
    (candidates . (lambda ()
                    (if (cdr (elscreen-get-screen-to-name-alist))
                        (sort
                         (loop for sname in (elscreen-get-screen-to-name-alist)
                               append (list (format "[%d] %s" (car sname) (cdr sname))) into lst
                               finally (return lst))
                         '(lambda (a b) (compare-strings a nil nil b nil nil))))))
    (action . (("Change Screen".
                (lambda (candidate)
                  (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
               ("Kill Screen".
                (lambda (candidate)
                  (elscreen-kill (- (aref candidate 1) (aref "0" 0)))))
               ("Only Screen".
                (lambda (candidate)
                  (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                  (elscreen-kill-others)))))))
;; (anything 'anything-c-source-elscreen)

;;;; <System>

;;; X RandR resolution change
;;; FIXME I do not care multi-display.
(defvar anything-c-xrandr-output "VGA")
(defvar anything-c-xrandr-screen "0")
(defvar anything-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" anything-c-xrandr-screen "-q")
           (goto-char 1)
           (loop while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 collect (match-string 1)))))
    (action
     ("Change Resolution" . (lambda (mode)
                              (call-process "xrandr" nil nil nil
                                            "--screen" anything-c-xrandr-screen
                                            "--output" anything-c-xrandr-output
                                            "--mode" mode))))))
;; (anything 'anything-c-source-xrandr-change-resolution)

;;; Xfont selection
(defun anything-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((default-font elm))
    (set-default-font default-font)))

(defvar anything-c-xfonts-cache nil)
(defvar anything-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless anything-c-xfonts-cache
                (setq anything-c-xfonts-cache
                      (x-list-fonts "*")))))  
    (candidates . anything-c-xfonts-cache)
    (multiline)
    (volatile)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-default-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . anything-c-persistent-xfont-action)))
  
;; (anything 'anything-c-source-xfonts)

;; Sources for gentoo users

(defvar anything-gentoo-prefered-shell 'eshell
  "Your favorite shell to run emerge command.")

(defvar anything-c-gentoo-use-flags nil)
(defvar anything-c-gentoo-buffer "*anything-gentoo-output*")
(defvar anything-c-cache-gentoo nil)
(defvar anything-c-cache-world nil)
(defvar anything-c-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create anything-c-gentoo-buffer)
              (unless anything-c-cache-gentoo
                (anything-c-gentoo-setup-cache))
              (unless anything-c-cache-world
                (setq anything-c-cache-world (anything-c-gentoo-get-world)))
              (anything-c-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (anything-c-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm anything-c-cache-world)
                                       (anything-c-gentoo-eshell-action elm "genlop -qe")
                                       (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (browse-url (car (anything-c-gentoo-get-url elm)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm anything-c-cache-world)
                                           (anything-c-gentoo-eshell-action elm "genlop -qi")
                                           (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (anything-c-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (anything-c-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (anything-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (anything-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (anything-c-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (anything-c-gentoo-default-action elm "equery" "files")))
               ("Update" . (lambda (elm)
                             (anything-c-gentoo-setup-cache)
                             (setq anything-c-cache-world (anything-c-gentoo-get-world))))))))

;; (anything 'anything-c-source-gentoo)

(defun* anything-gentoo-install (candidate &key action)
  (funcall anything-gentoo-prefered-shell)
  (let ((command (case action
                   ('install "sudo emerge -av ")
                   ('uninstall "sudo emerge -avC ")
                   (t (error "Unknow action")))))
  (if anything-c-marked-candidate-list
      (let ((elms (mapconcat 'identity anything-c-marked-candidate-list " ")))
        (insert (concat command elms)))
      (insert (concat command candidate)))))


(defun anything-c-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `anything-c-gentoo-buffer'."
  (if (member elm anything-c-cache-world)
      (progn
        (switch-to-buffer anything-c-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
      (message "No infos on packages not yet installed")))

(defvar anything-c-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless anything-c-gentoo-use-flags
                (anything-c-gentoo-setup-use-flags-cache))
              (anything-c-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-local-use)
    (action . (("Show which dep use this flag"
                . (lambda (elm)
                    (switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))
               ("Description"
                . (lambda (elm)
                    (switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))))))


;; (anything 'anything-c-source-use-flags)

(defun anything-c-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*anything-gentoo*"))
         (buf (anything-candidate-buffer 'portage-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-cache-gentoo)
        (insert (concat i "\n"))))))

(defun anything-c-gentoo-setup-cache ()
  "Set up `anything-c-cache-gentoo'"
  (setq anything-c-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun anything-c-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name anything-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
        (eshell-command (format "%s %s" command elm)))))

(defun anything-c-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*anything-gentoo-use*"))
         (buf (anything-candidate-buffer 'use-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun anything-c-gentoo-setup-use-flags-cache ()
  "Setup `anything-c-gentoo-use-flags'"
  (setq anything-c-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun anything-c-gentoo-get-url (elm)
  "Return a list of urls from eix output."
  (split-string (eshell-command-result
                 (format "eix %s | grep Homepage | awk '{print $2}'" elm))))

(defun anything-c-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun anything-c-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))

(defface anything-gentoo-match-face '((t (:foreground "red")))
  "Face for anything-gentoo installed packages."
  :group 'traverse-faces)

(defun anything-c-highlight-world (eix)
  "Highlight all installed package."
  (loop for i in eix
        if (member i anything-c-cache-world)
        collect (propertize i 'face 'anything-gentoo-match-face)
        else
        collect i))

(defun anything-c-highlight-local-use (use-flags)
  (let ((local-uses (anything-c-gentoo-get-local-use)))
    (loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'anything-gentoo-match-face)
          else
          collect i)))

(defvar anything-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda ()
                    (mapcar #'process-name
                            (process-list))))
    (action . (("Kill Process" . (lambda (elm)
                                   (delete-process (get-process elm))))))))

;; (anything 'anything-c-source-emacs-process)


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
    (anything-match-line-color-current-line)))

(defun anything-find-file-as-root (candidate)
  (find-file (concat "/" anything-su-or-sudo "::" (expand-file-name candidate))))

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

(anything-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(anything-document-attribute 'before-jump-hook "type . file-line / line"
  "Function to call before jumping to the target location.")
(anything-document-attribute 'after-jump-hook "type . file-line / line"
  "Function to call after jumping to the target location.")
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

(defun anything-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (anything-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (anything-c-set-variable 'hh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Persistent Action Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-match-line-overlay-face nil)
(defvar anything-match-line-overlay nil)

(defun anything-match-line-color-current-line (&optional start end buf face rec)
  "Highlight and underline current position"
  (let ((args (list (or start (line-beginning-position))
                    (or end (1+ (line-end-position)))
                    buf)))
    (if (not anything-match-line-overlay)
        (setq anything-match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay anything-match-line-overlay args)))
  (overlay-put anything-match-line-overlay
               'face (or face anything-match-line-overlay-face))
  (when rec
    (goto-char start)
    (recenter)))

(defalias 'anything-persistent-highlight-point 'anything-match-line-color-current-line)

(defface anything-overlay-line-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

(setq anything-match-line-overlay-face 'anything-overlay-line-face)

(add-hook 'anything-cleanup-hook #'(lambda ()
                                     (when anything-match-line-overlay
                                       (delete-overlay anything-match-line-overlay)
                                       (setq anything-match-line-overlay nil))))

(add-hook 'anything-after-persistent-action-hook #'(lambda ()
                                                     (when anything-match-line-overlay
                                                       (delete-overlay anything-match-line-overlay)
                                                       (anything-match-line-color-current-line))))

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
file or URL.  Else return ACTIONS unmodified."
  (if (string-match "^http\\|^ftp\\|html?$" candidate)
      (cons '("Browse with Browser" . browse-url) actions )
    actions))

;;;; Function
(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
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
  (if anything-allow-skipping-current-buffer
      (remove (buffer-name anything-current-buffer) buffers)
      buffers))

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
  "Files matching `anything-c-boring-file-regexp' will be skipped."
  (anything-c-skip-entries files anything-c-boring-file-regexp))
;; (anything-c-skip-boring-files '("README" "/src/.svn/hoge"))

(defun anything-c-skip-current-file (files)
  "Current file will be skipped."
  (remove (buffer-file-name anything-current-buffer) files))

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
                                 (when (commandp (intern-soft function)) " (i)"))
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
  "Save history information to file given by `anything-c-adaptive-history-file'."
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
    (when (symbolp file)
      (setq file (symbol-value file)))
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
    (save-excursion
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

(defun anything-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (buffer-modified-p)
      (revert-buffer t t))))

(defun anything-revert-marked-buffers (candidate)
  (dolist (i anything-c-marked-candidate-list)
    (anything-revert-buffer i)))

(defun anything-kill-marked-buffers (candidate)
  (dolist (i anything-c-marked-candidate-list)
    (kill-buffer i)))

(defun anything-delete-marked-files (candidate)
  (dolist (i anything-c-marked-candidate-list)
    (anything-c-delete-file i)))

(defun anything-ediff-marked-buffers (candidate &optional merge)
  "Ediff 2 marked buffers or 1 marked buffer and current-buffer.
With optional arg `merge' call `ediff-merge-buffers'."
  (let ((lg-lst (length anything-c-marked-candidate-list))
        buf1 buf2)
    (case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 anything-current-buffer
             buf2 (first anything-c-marked-candidate-list)))
      (2 
       (setq buf1 (first anything-c-marked-candidate-list)
             buf2 (second anything-c-marked-candidate-list)))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
        (ediff-buffers buf1 buf2))))

(defun anything-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun anything-delete-marked-bookmarks (elm)
  "Delete this bookmark or all marked bookmarks."
  (let ((bookmark (anything-bookmark-get-bookmark-from-name elm)))
    (anything-aif anything-c-marked-candidate-list
        (dolist (i it)
          (let ((bmk (anything-bookmark-get-bookmark-from-name i)))
            (bookmark-delete bmk 'batch)))
      (bookmark-delete bookmark 'batch))))

(defun anything-bookmark-active-region-maybe (candidate)
  "Active saved region if this bookmark have one."
  (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
    (condition-case nil
        (when (and (boundp bookmarkp-use-region-flag)
                   bookmarkp-use-region-flag)
          (let ((bmk-name (or (bookmarkp-get-buffer-name bookmark)
                              (file-name-nondirectory
                               (bookmark-get-filename bookmark)))))
            (when bmk-name
              (with-current-buffer bmk-name
                (setq deactivate-mark nil)))))
      (error nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type Attributes
(define-anything-type-attribute 'buffer
  `((action
     ,@(if pop-up-frames
           '(("Switch to buffer other window" . switch-to-buffer-other-window)
             ("Switch to buffer" . switch-to-buffer))
         '(("Switch to buffer" . switch-to-buffer)
           ("Switch to buffer other window" . switch-to-buffer-other-window)
           ("Switch to buffer other frame" . switch-to-buffer-other-frame)))
     ("Display buffer"   . display-buffer)
     ("Revert buffer" . anything-revert-buffer)
     ("Revert Marked buffers" . anything-revert-marked-buffers)
     ("Kill buffer" . kill-buffer)
     ("Kill Marked buffers" . anything-kill-marked-buffers)
     ("Ediff Marked buffers" . anything-ediff-marked-buffers)
     ("Ediff Merge marked buffers" . (lambda (candidate)
                                       (anything-ediff-marked-buffers candidate t))))
    (candidate-transformer anything-c-skip-current-buffer anything-c-skip-boring-buffers))
  "Buffer or buffer name.")

(define-anything-type-attribute 'file
  `((action
     ,@(if pop-up-frames
           '(("Find file other window" . find-file-other-window)
             ("Find file" . find-file)
             ("Find file as root" . anything-find-file-as-root))
         '(("Find file" . find-file)
           ("Find file as root" . anything-find-file-as-root)
           ("Find file other window" . find-file-other-window)
           ("Find file other frame" . find-file-other-frame)))
     ("Open dired in file's directory" . anything-c-open-dired)
     ("Delete file" . anything-c-delete-file)
     ("Delete Marked files" . anything-delete-marked-files)
     ("Open file externally" . anything-c-open-file-externally)
     ("Open file with default tool" . anything-c-open-file-with-default-tool))
    (action-transformer anything-c-transform-file-load-el
                        anything-c-transform-file-browse-url)
    (candidate-transformer anything-c-w32-pathname-transformer
                           anything-c-skip-current-file
                           anything-c-skip-boring-files
                           anything-c-shorten-home-path))
  "File name.")

(define-anything-type-attribute 'command
  `((action ("Call interactively" . anything-c-call-interactively)
            ("Describe command" . anything-c-describe-function)
            ("Add command to kill ring" . anything-c-kill-new)
            ("Go to command's definition" . anything-c-find-function))
    ;; Sort commands according to their usage count.
    (filtered-candidate-transformer . anything-c-adaptive-sort))
  "Command. (string or symbol)")

(define-anything-type-attribute 'function
  '((action ("Describe function" . anything-c-describe-function)
            ("Add function to kill ring" . anything-c-kill-new)
            ("Go to function's definition" . anything-c-find-function))
    (action-transformer anything-c-transform-function-call-interactively)
    (candidate-transformer anything-c-mark-interactive-functions))
  "Function. (string or symbol)")

(define-anything-type-attribute 'variable
  '((action ("Describe variable" . anything-c-describe-variable)
            ("Add variable to kill ring" . anything-c-kill-new)
            ("Go to variable's definition" . anything-c-find-variable)
            ("Set variable" . anything-c-set-variable)))
  "Variable.")

(define-anything-type-attribute 'sexp
  '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
            ("Add s-expression to kill ring" . kill-new))
    (action-transformer anything-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

(define-anything-type-attribute 'bookmark
  `((action
     ("Jump to bookmark" . (lambda (candidate)
                             (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                               (bookmark-jump bookmark))
                             (anything-update)
                             (anything-bookmark-active-region-maybe candidate)))
     ("Jump to BM other window" . (lambda (candidate)
                                    (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                      (bookmark-jump-other-window bookmark))
                                    (anything-update)
                                    (anything-bookmark-active-region-maybe candidate)))
     ("Bookmark edit annotation" . (lambda (candidate)
                                     (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                       (bookmark-edit-annotation bookmark))))
     ("Bookmark show annotation" . (lambda (candidate)
                                     (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                       (bookmark-show-annotation bookmark))))
     ("Delete bookmark(s)" . anything-delete-marked-bookmarks)
     ,@(when (fboundp 'bookmarkp-edit-bookmark)
             '(("Edit Bookmark" . (lambda (candidate)
                                    (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                            (bookmarkp-edit-bookmark bookmark))))))
     ("Rename bookmark" . (lambda (candidate)
                            (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                              (bookmark-rename bookmark))))
     ("Relocate bookmark" . (lambda (candidate)
                              (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                (bookmark-relocate bookmark))))
     "Bookmark name.")))

(define-anything-type-attribute 'line
  '((display-to-real . anything-c-display-to-real-line)
    (action ("Go to Line" . anything-c-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".")

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

