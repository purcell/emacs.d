;;; wl-summary.el --- Summary mode for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1999,2000      TSUMURA Tomoaki <tsumura@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;	TSUMURA Tomoaki <tsumura@kuis.kyoto-u.ac.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'elmo)
(require 'elmo-multi)
(eval-when-compile (require 'elmo-filter))
(require 'wl-message)
(require 'wl-vars)
(require 'wl-highlight)
(require 'wl-refile)
(require 'wl-util)
(require 'timezone nil t)
(require 'easymenu nil t)
(require 'elmo-date)
(require 'ps-print nil t)

(eval-when-compile
  (require 'cl)
  (require 'timer nil t)
  (defalias-maybe 'ps-print-buffer-with-faces 'ignore)
  (defalias-maybe 'elmo-database-msgid-put 'ignore)
  (defalias-maybe 'elmo-database-close 'ignore)
  (defalias-maybe 'elmo-database-msgid-get 'ignore)
  (defalias-maybe 'run-with-idle-timer 'ignore)
  (defalias-maybe 'ps-print-preprint 'ignore))

(defvar dragdrop-drop-functions)
(defvar scrollbar-height)
(defvar mail-reply-buffer)
(defvar elmo-global-flags)

(defvar wl-summary-buffer-name "Summary")
(defvar wl-summary-mode-map nil)
(defvar wl-current-summary-buffer nil)

(defvar wl-summary-buffer-elmo-folder nil)

(defun wl-summary-buffer-folder-name ()
  (and wl-summary-buffer-elmo-folder
       (elmo-folder-name-internal wl-summary-buffer-elmo-folder)))

(defvar wl-summary-buffer-disp-msg    nil)
(defvar wl-summary-buffer-disp-folder nil)
(defvar wl-summary-buffer-temp-mark-list nil)
(defvar wl-summary-buffer-message-ring nil)
(defvar wl-summary-buffer-current-msg nil)
(defvar wl-summary-buffer-unread-count 0)
(defvar wl-summary-buffer-new-count    0)
(defvar wl-summary-buffer-answered-count 0)
(defvar wl-summary-buffer-mime-charset  nil)
(defvar wl-summary-buffer-weekday-name-lang  nil)
(defvar wl-summary-buffer-thread-indent-set-alist  nil)
(defvar wl-summary-buffer-view nil)
(defvar wl-summary-buffer-message-modified nil)
(defvar wl-summary-buffer-thread-modified nil)

(defvar wl-summary-buffer-number-column nil)
(defvar wl-summary-buffer-temp-mark-column nil)
(defvar wl-summary-buffer-persistent-mark-column nil)

(defvar wl-summary-buffer-persistent-mark-version 0)

(defvar wl-summary-buffer-persistent nil)
(defvar wl-summary-buffer-thread-nodes nil)
(defvar wl-summary-buffer-target-mark-list nil)
(defvar wl-summary-buffer-prev-refile-destination nil)
(defvar wl-summary-buffer-saved-message nil)
(defvar wl-summary-buffer-prev-folder-function nil)
(defvar wl-summary-buffer-next-folder-function nil)
(defvar wl-summary-buffer-exit-function nil)
(defvar wl-summary-buffer-next-message-function nil)
(defvar wl-summary-buffer-window-scroll-functions nil)
(defvar wl-summary-buffer-number-list nil)
(defvar wl-summary-buffer-folder-name nil)
(defvar wl-summary-buffer-line-formatter nil)
(defvar wl-summary-buffer-line-format nil)
(defvar wl-summary-buffer-mode-line-formatter nil)
(defvar wl-summary-buffer-mode-line nil)
(defvar wl-summary-buffer-display-mime-mode 'mime)
(defvar wl-summary-buffer-display-header-mode 'partial)

(defvar wl-thread-indent-level-internal nil)
(defvar wl-thread-have-younger-brother-str-internal nil)
(defvar wl-thread-youngest-child-str-internal nil)
(defvar wl-thread-vertical-str-internal nil)
(defvar wl-thread-horizontal-str-internal nil)
(defvar wl-thread-space-str-internal nil)
(defvar wl-summary-last-visited-folder nil)
(defvar wl-read-folder-history nil)
(defvar wl-summary-scored nil)
(defvar wl-crosspost-alist-modified nil)
(defvar wl-summary-alike-hashtb nil)
(defvar wl-summary-search-buf-name " *wl-search-subject*")
(defvar wl-summary-delayed-update nil)
(defvar wl-summary-search-buf-folder-name nil)

(defvar wl-summary-get-petname-function 'wl-address-get-petname-1)

(defvar wl-summary-shell-command-last "")

(defvar wl-ps-preprint-hook nil)
(defvar wl-ps-print-hook nil)

(defvar wl-thread-saved-entity-hashtb-internal nil)

(make-variable-buffer-local 'wl-summary-buffer-elmo-folder)
(make-variable-buffer-local 'wl-summary-search-buf-folder-name)
(make-variable-buffer-local 'wl-summary-buffer-disp-msg)
(make-variable-buffer-local 'wl-summary-buffer-disp-folder)
(make-variable-buffer-local 'wl-summary-buffer-target-mark-list)
(make-variable-buffer-local 'wl-summary-buffer-temp-mark-list)
(make-variable-buffer-local 'wl-summary-buffer-message-ring)
(make-variable-buffer-local 'wl-summary-buffer-unread-count)
(make-variable-buffer-local 'wl-summary-buffer-new-count)
(make-variable-buffer-local 'wl-summary-buffer-answered-count)
(make-variable-buffer-local 'wl-summary-buffer-mime-charset)
(make-variable-buffer-local 'wl-summary-buffer-weekday-name-lang)
(make-variable-buffer-local 'wl-summary-buffer-thread-indent-set)
(make-variable-buffer-local 'wl-summary-buffer-view)
(make-variable-buffer-local 'wl-summary-buffer-message-modified)
(make-variable-buffer-local 'wl-summary-buffer-thread-modified)
(make-variable-buffer-local 'wl-summary-buffer-number-column)
(make-variable-buffer-local 'wl-summary-buffer-temp-mark-column)
(make-variable-buffer-local 'wl-summary-buffer-persistent-mark-column)
(make-variable-buffer-local 'wl-summary-buffer-persistent-mark-version)
(make-variable-buffer-local 'wl-summary-buffer-persistent)
(make-variable-buffer-local 'wl-summary-buffer-thread-nodes)
(make-variable-buffer-local 'wl-summary-buffer-prev-refile-destination)
(make-variable-buffer-local 'wl-summary-buffer-saved-message)
(make-variable-buffer-local 'wl-summary-scored)
(make-variable-buffer-local 'wl-summary-default-score)
(make-variable-buffer-local 'wl-summary-move-direction-downward)
(make-variable-buffer-local 'wl-summary-important-above)
(make-variable-buffer-local 'wl-summary-target-above)
(make-variable-buffer-local 'wl-summary-mark-below)
(make-variable-buffer-local 'wl-summary-expunge-below)
(make-variable-buffer-local 'wl-thread-indent-level-internal)
(make-variable-buffer-local 'wl-thread-have-younger-brother-str-internal)
(make-variable-buffer-local 'wl-thread-youngest-child-str-internal)
(make-variable-buffer-local 'wl-thread-vertical-str-internal)
(make-variable-buffer-local 'wl-thread-horizontal-str-internal)
(make-variable-buffer-local 'wl-thread-space-str-internal)
(make-variable-buffer-local 'wl-summary-buffer-prev-folder-function)
(make-variable-buffer-local 'wl-summary-buffer-next-folder-function)
(make-variable-buffer-local 'wl-summary-buffer-exit-function)
(make-variable-buffer-local 'wl-summary-buffer-next-message-function)
(make-variable-buffer-local 'wl-summary-buffer-window-scroll-functions)
(make-variable-buffer-local 'wl-summary-buffer-number-list)
(make-variable-buffer-local 'wl-summary-buffer-folder-name)
(make-variable-buffer-local 'wl-summary-buffer-line-formatter)
(make-variable-buffer-local 'wl-summary-buffer-line-format)
(make-variable-buffer-local 'wl-summary-buffer-mode-line-formatter)
(make-variable-buffer-local 'wl-summary-buffer-mode-line)
(make-variable-buffer-local 'wl-summary-buffer-display-mime-mode)
(make-variable-buffer-local 'wl-summary-buffer-display-header-mode)

(defvar wl-datevec)
(defvar wl-thr-indent-string)
(defvar wl-thr-children-number)
(defvar wl-thr-linked)
(defvar wl-message-entity)
(defvar wl-parent-message-entity)
(defvar wl-temp-mark)
(defvar wl-persistent-mark)

(defun wl-summary-sticky-buffer-name (name)
  (concat wl-summary-buffer-name ":" name))

(defun wl-summary-default-subject (subject-string)
  (if (string-match "^[ \t]*\\[[^:]+[,: ][0-9]+\\][ \t]*" subject-string)
      (substring subject-string (match-end 0))
    subject-string))

(defun wl-summary-default-from (from)
  "Instance of `wl-summary-from-function'.
Ordinarily returns the sender name. Returns recipient names if (1)
summary's folder name matches with `wl-summary-showto-folder-regexp'
and (2) sender address is yours.

See also variable `wl-use-petname'."
  (let ((translator (if wl-use-petname
			(lambda (string)
			  (or (funcall wl-summary-get-petname-function string)
			      (car (std11-extract-address-components string))
			      string))
		      #'identity))
	to ng)
    (or (and (eq major-mode 'wl-summary-mode)
	     (stringp wl-summary-showto-folder-regexp)
	     (string-match wl-summary-showto-folder-regexp
			   (wl-summary-buffer-folder-name))
	     (wl-address-user-mail-address-p from)
	     (cond
	      ((setq to (elmo-message-entity-field wl-message-entity 'to))
	       (concat "To:" (mapconcat translator to ",")))
	      ((setq ng (elmo-message-entity-field wl-message-entity
						   'newsgroups))
	       (concat "Ng:" ng))))
	(funcall translator from))))

(defun wl-summary-simple-from (string)
  (if wl-use-petname
      (or (funcall wl-summary-get-petname-function string)
	  (car (std11-extract-address-components string))
	  string)
    string))

(defvar wl-summary-sort-specs '(number date subject from list-info size))
(defvar wl-summary-default-sort-spec 'date)

(defvar wl-summary-mode-menu-spec
  '("Summary"
    ["Read" wl-summary-read t]
    ["Edit draft message" wl-summary-reedit :visible (string= (wl-summary-buffer-folder-name) wl-draft-folder)]
    ["Prev page" wl-summary-prev-page t]
    ["Next page" wl-summary-next-page t]
    ["Top"       wl-summary-display-top t]
    ["Bottom"    wl-summary-display-bottom t]
    ["Prev"      wl-summary-prev t]
    ["Next"      wl-summary-next t]
    ["Up"        wl-summary-up t]
    ["Down"      wl-summary-down t]
    ["Parent message" wl-summary-jump-to-parent-message t]
    "----"
    ["Sync"            wl-summary-sync t]
    ["Execute"         wl-summary-exec t]
    ["Go to other folder" wl-summary-goto-folder t]
    ["Pick" wl-summary-pick t]
    ["Mark as read all" wl-summary-mark-as-read-all t]
    ["Unmark all"      wl-summary-unmark-all t]
    ["Toggle display message" wl-summary-toggle-disp-msg t]
    ["Display folder" wl-summary-toggle-disp-folder t]
    ["Toggle threading" wl-summary-toggle-thread t]
    ["Stick" wl-summary-stick t]
    ("Sort"
     ["By Number" wl-summary-sort-by-number t]
     ["By Size" wl-summary-sort-by-size t]
     ["By Date" wl-summary-sort-by-date t]
     ["By From" wl-summary-sort-by-from t]
     ["By Subject" wl-summary-sort-by-subject t]
     ["By List Info" wl-summary-sort-by-list-info t])
    "----"
    ("Message Operation"
     ["Mark as read"    wl-summary-mark-as-read t]
     ["Set flags"	wl-summary-set-flags t]
     ["Mark as unread"   wl-summary-mark-as-unread t]
     ["Mark as answered" wl-summary-mark-as-answered t]
     ["Set dispose mark" wl-summary-dispose t]
     ["Set refile mark" wl-summary-refile t]
     ["Set copy mark"   wl-summary-copy t]
     ["Set resend mark" wl-summary-resend t]
     ["Prefetch"	wl-summary-prefetch t]
     ["Set target mark" wl-summary-target-mark t]
     ["Unmark"		wl-summary-unmark t]
     ["Save"		wl-summary-save t]
     ["Cancel posted news" wl-summary-cancel-message t]
     ["Supersedes message" wl-summary-supersedes-message t]
     ["Resend bounced mail" wl-summary-resend-bounced-mail t]
     ["Enter the message" wl-summary-jump-to-current-message t]
     ["Pipe message" wl-summary-pipe-message t]
     ["Print message" wl-summary-print-message t]
     ["View raw message" wl-summary-display-raw t]
     )
    ("Thread Operation"
     ["Open or Close" wl-thread-open-close (eq wl-summary-buffer-view 'thread)]
     ["Open all"     wl-thread-open-all (eq wl-summary-buffer-view 'thread)]
     ["Close all"    wl-thread-close-all (eq wl-summary-buffer-view 'thread)]
     ["Mark as read" wl-thread-mark-as-read (eq wl-summary-buffer-view 'thread)]
     ["Set flags"	wl-thread-set-flags (eq wl-summary-buffer-view 'thread)]
     ["Mark as unread"		wl-thread-mark-as-unread (eq wl-summary-buffer-view 'thread)]
     ["Mark as answered"	wl-thread-mark-as-answered (eq wl-summary-buffer-view 'thread)]
     ["Set delete mark"  wl-thread-delete (eq wl-summary-buffer-view 'thread)]
     ["Set refile mark"  wl-thread-refile (eq wl-summary-buffer-view 'thread)]
     ["Set copy mark"    wl-thread-copy (eq wl-summary-buffer-view 'thread)]
     ["Prefetch"     wl-thread-prefetch (eq wl-summary-buffer-view 'thread)]
     ["Set target mark"        wl-thread-target-mark (eq wl-summary-buffer-view 'thread)]
     ["Unmark"      wl-thread-unmark (eq wl-summary-buffer-view 'thread)]
     ["Save"		wl-thread-save (eq wl-summary-buffer-view 'thread)]
     ["Execute"      wl-thread-exec (eq wl-summary-buffer-view 'thread)])
    ("Region Operation"
     ["Mark as read" wl-summary-mark-as-read-region t]
     ["Set flags" wl-summary-set-flags-region t]
     ["Mark as unread" wl-summary-mark-as-unread-region t]
     ["Mark as answered" wl-summary-mark-as-answered-region t]
     ["Set dispose mark" wl-summary-dispose-region t]
     ["Set refile mark" wl-summary-refile-region t]
     ["Set copy mark" wl-summary-copy-region t]
     ["Prefetch" wl-summary-prefetch-region t]
     ["Set target mark" wl-summary-target-mark-region t]
     ["Unmark" wl-summary-unmark-region t]
     ["Save" wl-summary-save-region t]
     ["Execute" wl-summary-exec-region t])
    ("Mark Operation"
     ["Mark as read" wl-summary-target-mark-mark-as-read t]
     ["Set flags" wl-summary-target-mark-set-flags t]
     ["Mark as unread" wl-summary-target-mark-mark-as-unread t]
     ["Set delete mark" wl-summary-target-mark-delete t]
     ["Set refile mark" wl-summary-target-mark-refile t]
     ["Set copy mark" wl-summary-target-mark-copy t]
     ["Prefetch" wl-summary-target-mark-prefetch t]
     ["Save" wl-summary-target-mark-save t]
     ["Reply with citation" wl-summary-target-mark-reply-with-citation t]
     ["Forward" wl-summary-target-mark-forward t]
     ["uudecode" wl-summary-target-mark-uudecode t])
    ("Score Operation"
     ["Switch current score file" wl-score-change-score-file t]
     ["Edit current score file" wl-score-edit-current-scores t]
     ["Edit score file" wl-score-edit-file t]
     ["Set mark below" wl-score-set-mark-below t]
     ["Set expunge below" wl-score-set-expunge-below t]
     ["Rescore buffer" wl-summary-rescore t]
     ["Increase score" wl-summary-increase-score t]
     ["Lower score" wl-summary-lower-score t])
    "----"
    ("Writing Messages"
     ["Write a message" wl-summary-write t]
     ["Write for current folder" wl-summary-write-current-folder t]
     ["Reply" wl-summary-reply t]
     ["Reply with citation" wl-summary-reply-with-citation t]
     ["Forward" wl-summary-forward t])
    "----"
    ["Toggle Plug Status" wl-toggle-plugged t]
    ["Change Plug Status" wl-plugged-change t]
    "----"
    ["Exit Current Folder" wl-summary-exit t]))

(if wl-on-xemacs
    (defun wl-summary-setup-mouse ()
      (define-key wl-summary-mode-map 'button4 'wl-summary-prev)
      (define-key wl-summary-mode-map 'button5 'wl-summary-next)
      (define-key wl-summary-mode-map [(shift button4)]
	'wl-summary-up)
      (define-key wl-summary-mode-map [(shift button5)]
	'wl-summary-down)
      (define-key wl-summary-mode-map 'button2 'wl-summary-click))
  (defun wl-summary-setup-mouse ()
    (define-key wl-summary-mode-map [mouse-4] 'wl-summary-prev)
    (define-key wl-summary-mode-map [mouse-5] 'wl-summary-next)
    (define-key wl-summary-mode-map [S-mouse-4] 'wl-summary-up)
    (define-key wl-summary-mode-map [S-mouse-5] 'wl-summary-down)
    ;; For Meadow2
    (define-key wl-summary-mode-map [mouse-wheel1]
      'wl-summary-wheel-dispatcher)
    (define-key wl-summary-mode-map [S-mouse-wheel1]
      'wl-summary-wheel-dispatcher)
    (define-key wl-summary-mode-map [mouse-2] 'wl-summary-click)))

(if wl-summary-mode-map
    ()
  (setq wl-summary-mode-map (make-keymap))
  (suppress-keymap wl-summary-mode-map)
  (substitute-key-definition 'kill-buffer
			     'wl-summary-mimic-kill-buffer
			     wl-summary-mode-map
			     global-map)
  ;; basic commands
  (define-key wl-summary-mode-map " "    'wl-summary-read)
  (define-key wl-summary-mode-map "."    'wl-summary-redisplay)
  (define-key wl-summary-mode-map ","    'wl-summary-display-raw)
  (define-key wl-summary-mode-map "<"    'wl-summary-display-top)
  (define-key wl-summary-mode-map ">"    'wl-summary-display-bottom)
  (define-key wl-summary-mode-map "\177" 'wl-summary-prev-page)
  (define-key wl-summary-mode-map [backspace] 'wl-summary-prev-page)
  (define-key wl-summary-mode-map "\r"   'wl-summary-enter-handler)
  (define-key wl-summary-mode-map "\C-m" 'wl-summary-enter-handler)
  (define-key wl-summary-mode-map "/"    'wl-thread-open-close)
  (define-key wl-summary-mode-map "["    'wl-thread-open-all)
  (define-key wl-summary-mode-map "]"    'wl-thread-close-all)
  (define-key wl-summary-mode-map "-"    'wl-summary-prev-line-content)
  (define-key wl-summary-mode-map "\e\r" 'wl-summary-prev-line-content)
  (define-key wl-summary-mode-map "g"    'wl-summary-goto-folder)
  (define-key wl-summary-mode-map "G"    'wl-summary-goto-folder-sticky)
  (define-key wl-summary-mode-map "c"    'wl-summary-mark-as-read-all)

  (define-key wl-summary-mode-map "a"    'wl-summary-reply)
  (define-key wl-summary-mode-map "A"    'wl-summary-reply-with-citation)
  (define-key wl-summary-mode-map "C"    'wl-summary-cancel-message)
  (define-key wl-summary-mode-map "E"    'wl-summary-reedit)
  (define-key wl-summary-mode-map "\eE"  'wl-summary-resend-bounced-mail)
  (define-key wl-summary-mode-map "f"    'wl-summary-forward)
  (define-key wl-summary-mode-map "$"    'wl-summary-mark-as-important)
  (define-key wl-summary-mode-map "F"    'wl-summary-set-flags)
  (define-key wl-summary-mode-map "\M-k"  'wl-summary-toggle-persistent-mark)
  (define-key wl-summary-mode-map "&"    'wl-summary-mark-as-answered)
  (define-key wl-summary-mode-map "@"    'wl-summary-edit-addresses)

  (define-key wl-summary-mode-map "y"    'wl-summary-save)
  (define-key wl-summary-mode-map "n"    'wl-summary-next)
  (define-key wl-summary-mode-map "p"    'wl-summary-prev)
  (define-key wl-summary-mode-map "N"    'wl-summary-down)
  (define-key wl-summary-mode-map "P"    'wl-summary-up)
  (define-key wl-summary-mode-map "w"    'wl-summary-write)
  (define-key wl-summary-mode-map "W"    'wl-summary-write-current-folder)
  (define-key wl-summary-mode-map "e"     'wl-summary-save)
  (define-key wl-summary-mode-map "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (define-key wl-summary-mode-map "\C-c\C-a" 'wl-addrmgr)
  (define-key wl-summary-mode-map "\C-c\C-p" 'wl-summary-previous-buffer)
  (define-key wl-summary-mode-map "\C-c\C-n" 'wl-summary-next-buffer)
  (define-key wl-summary-mode-map "H"    'wl-summary-toggle-all-header)
  (define-key wl-summary-mode-map "M"    'wl-summary-toggle-mime)
  (define-key wl-summary-mode-map "\C-cm" 'wl-summary-toggle-mime-buttons)
  (define-key wl-summary-mode-map "B"    'wl-summary-burst)
  (define-key wl-summary-mode-map "Z"    'wl-status-update)
  (define-key wl-summary-mode-map "#"    'wl-summary-print-message)
  (define-key wl-summary-mode-map "|"    'wl-summary-pipe-message)
  (define-key wl-summary-mode-map "z"    'wl-summary-suspend)
  (define-key wl-summary-mode-map "q"    'wl-summary-exit)
  (define-key wl-summary-mode-map "Q"    'wl-summary-force-exit)

  (define-key wl-summary-mode-map "j"    'wl-summary-jump-to-current-message)
  (define-key wl-summary-mode-map "J"    'wl-thread-jump-to-msg)
  (define-key wl-summary-mode-map "I"    'wl-summary-incorporate)
  (define-key wl-summary-mode-map "\M-j" 'wl-summary-jump-to-msg-by-message-id)
  (define-key wl-summary-mode-map "^"    'wl-summary-jump-to-parent-message)
  (define-key wl-summary-mode-map "!"    'wl-summary-mark-as-unread)

  (define-key wl-summary-mode-map "s"    'wl-summary-sync)
  (define-key wl-summary-mode-map "S"    'wl-summary-sort)
  (define-key wl-summary-mode-map "\M-s"    'wl-summary-stick)
  (define-key wl-summary-mode-map "T"    'wl-summary-toggle-thread)

  (define-key wl-summary-mode-map "l"    'wl-summary-toggle-disp-folder)
  (define-key wl-summary-mode-map "v"    'wl-summary-toggle-disp-msg)
  (define-key wl-summary-mode-map "V"    'wl-summary-virtual)

  (define-key wl-summary-mode-map "\C-i"  'wl-summary-goto-last-displayed-msg)
  (define-key wl-summary-mode-map "?"    'wl-summary-pick)
  (define-key wl-summary-mode-map "\ee"  'wl-summary-expire)

  ;; copy & paste.
  (define-key wl-summary-mode-map "\ew"  'wl-summary-save-current-message)
  (define-key wl-summary-mode-map "\C-y"  'wl-summary-yank-saved-message)

  ;; line commands
  (define-key wl-summary-mode-map "R"    'wl-summary-mark-as-read)
  (define-key wl-summary-mode-map "i"    'wl-summary-prefetch)
  (define-key wl-summary-mode-map "x"    'wl-summary-exec)
  (define-key wl-summary-mode-map "*"    'wl-summary-target-mark)
  (define-key wl-summary-mode-map "o"    'wl-summary-refile)
  (define-key wl-summary-mode-map "O"    'wl-summary-copy)
  (define-key wl-summary-mode-map "\M-o" 'wl-summary-refile-prev-destination)
  (define-key wl-summary-mode-map "\C-o" 'wl-summary-auto-refile)
  (define-key wl-summary-mode-map "d"    'wl-summary-dispose)
  (define-key wl-summary-mode-map "u"    'wl-summary-unmark)
  (define-key wl-summary-mode-map "U"    'wl-summary-unmark-all)
  (define-key wl-summary-mode-map "D"    'wl-summary-delete)
  (define-key wl-summary-mode-map "~"    'wl-summary-resend)

  ;; thread commands
  (define-key wl-summary-mode-map "t"	(make-sparse-keymap))
  (define-key wl-summary-mode-map "tR" 'wl-thread-mark-as-read)
  (define-key wl-summary-mode-map "ti" 'wl-thread-prefetch)
  (define-key wl-summary-mode-map "tx" 'wl-thread-exec)
  (define-key wl-summary-mode-map "t*" 'wl-thread-target-mark)
  (define-key wl-summary-mode-map "to" 'wl-thread-refile)
  (define-key wl-summary-mode-map "tO" 'wl-thread-copy)
  (define-key wl-summary-mode-map "t\M-o" 'wl-thread-refile-prev-destination)
  (define-key wl-summary-mode-map "td" 'wl-thread-dispose)
  (define-key wl-summary-mode-map "tD" 'wl-thread-delete)
  (define-key wl-summary-mode-map "t~" 'wl-thread-resend)
  (define-key wl-summary-mode-map "tu" 'wl-thread-unmark)
  (define-key wl-summary-mode-map "t!" 'wl-thread-mark-as-unread)
  (define-key wl-summary-mode-map "t$" 'wl-thread-mark-as-important)
  (define-key wl-summary-mode-map "tF" 'wl-thread-set-flags)
  (define-key wl-summary-mode-map "t&" 'wl-thread-mark-as-answered)
  (define-key wl-summary-mode-map "ty" 'wl-thread-save)
  (define-key wl-summary-mode-map "ts" 'wl-thread-set-parent)

  ;; target-mark commands
  (define-key wl-summary-mode-map "m"	  (make-sparse-keymap))
  (define-key wl-summary-mode-map "mi"   'wl-summary-target-mark-prefetch)
  (define-key wl-summary-mode-map "mo"   'wl-summary-target-mark-refile)
  (define-key wl-summary-mode-map "mO"   'wl-summary-target-mark-copy)
  (define-key wl-summary-mode-map "m\M-o" 'wl-summary-target-mark-refile-prev-destination)
  (define-key wl-summary-mode-map "md"   'wl-summary-target-mark-dispose)
  (define-key wl-summary-mode-map "mD"   'wl-summary-target-mark-delete)
  (define-key wl-summary-mode-map "m~"   'wl-summary-target-mark-resend)

  (define-key wl-summary-mode-map "mu"   'wl-summary-delete-all-temp-marks)

  (define-key wl-summary-mode-map "my"   'wl-summary-target-mark-save)
  (define-key wl-summary-mode-map "mR"   'wl-summary-target-mark-mark-as-read)
  (define-key wl-summary-mode-map "m!"   'wl-summary-target-mark-mark-as-unread)
  (define-key wl-summary-mode-map "m&"   'wl-summary-target-mark-mark-as-answered)
  (define-key wl-summary-mode-map "m$"   'wl-summary-target-mark-mark-as-important)
  (define-key wl-summary-mode-map "mF"   'wl-summary-target-mark-set-flags)
  (define-key wl-summary-mode-map "mU"   'wl-summary-target-mark-uudecode)
  (define-key wl-summary-mode-map "ma"   'wl-summary-target-mark-all)
  (define-key wl-summary-mode-map "mt"   'wl-summary-target-mark-thread)
  (define-key wl-summary-mode-map "mA"   'wl-summary-target-mark-reply-with-citation)
  (define-key wl-summary-mode-map "mf"   'wl-summary-target-mark-forward)
  (define-key wl-summary-mode-map "m?"   'wl-summary-target-mark-pick)
  (define-key wl-summary-mode-map "m#"   'wl-summary-target-mark-print)
  (define-key wl-summary-mode-map "m|"   'wl-summary-target-mark-pipe)

  ;; region commands
  (define-key wl-summary-mode-map "r"    (make-sparse-keymap))
  (define-key wl-summary-mode-map "rR"   'wl-summary-mark-as-read-region)
  (define-key wl-summary-mode-map "ri"   'wl-summary-prefetch-region)
  (define-key wl-summary-mode-map "rx"   'wl-summary-exec-region)
  (define-key wl-summary-mode-map "mr"   'wl-summary-target-mark-region)
  (define-key wl-summary-mode-map "r*"   'wl-summary-target-mark-region)
  (define-key wl-summary-mode-map "ro"   'wl-summary-refile-region)
  (define-key wl-summary-mode-map "rO"   'wl-summary-copy-region)
  (define-key wl-summary-mode-map "r\M-o" 'wl-summary-refile-prev-destination-region)
  (define-key wl-summary-mode-map "rd"   'wl-summary-dispose-region)
  (define-key wl-summary-mode-map "rD"   'wl-summary-delete-region)
  (define-key wl-summary-mode-map "r~"   'wl-summary-resend-region)
  (define-key wl-summary-mode-map "ru"   'wl-summary-unmark-region)
  (define-key wl-summary-mode-map "r!"   'wl-summary-mark-as-unread-region)
  (define-key wl-summary-mode-map "r$"   'wl-summary-mark-as-important-region)
  (define-key wl-summary-mode-map "rF"   'wl-summary-set-flags-region)
  (define-key wl-summary-mode-map "r&"   'wl-summary-mark-as-answered-region)
  (define-key wl-summary-mode-map "ry"   'wl-summary-save-region)

  ;; score commands
  (define-key wl-summary-mode-map "K"    'wl-summary-increase-score)
  (define-key wl-summary-mode-map "L"    'wl-summary-lower-score)
  (define-key wl-summary-mode-map "h"    (make-sparse-keymap))
  (define-key wl-summary-mode-map "hR"   'wl-summary-rescore)
  (define-key wl-summary-mode-map "hc"   'wl-score-change-score-file)
  (define-key wl-summary-mode-map "he"   'wl-score-edit-current-scores)
  (define-key wl-summary-mode-map "hf"   'wl-score-edit-file)
  (define-key wl-summary-mode-map "hF"   'wl-score-flush-cache)
  (define-key wl-summary-mode-map "hm"	 'wl-score-set-mark-below)
  (define-key wl-summary-mode-map "hx"   'wl-score-set-expunge-below)

  ;; misc
  (define-key wl-summary-mode-map "\C-c\C-f" 'wl-summary-toggle-header-narrowing)
  (define-key wl-summary-mode-map "\M-t" 'wl-toggle-plugged)
  (define-key wl-summary-mode-map "\C-t" 'wl-plugged-change)
  ;;
  (define-key wl-summary-mode-map "\C-x\C-s" 'wl-summary-save-status)
  (wl-summary-setup-mouse)
  (easy-menu-define
   wl-summary-mode-menu
   wl-summary-mode-map
   "Menu used in Summary mode."
   wl-summary-mode-menu-spec))

(defun wl-summary-mimic-kill-buffer (buffer)
  "Kill the current (Summary) buffer with query."
  (interactive "bKill buffer: ")
  (if (or (not buffer)
	  (string-equal buffer "")
	  (string-equal buffer (buffer-name)))
      (wl-summary-exit 'force-exit)
    (kill-buffer buffer)))

(defsubst wl-summary-message-visible-p (number)
  "Return non-nil if the message with NUMBER is visible."
  (or (eq wl-summary-buffer-view 'sequence)
      (not (wl-thread-entity-parent-invisible-p
	    (wl-thread-get-entity number)))))

(defun wl-summary-push-message (number)
  (when (and number
	     (not (equal number (car wl-summary-buffer-message-ring))))
    (setq wl-summary-buffer-message-ring
	  (cons number wl-summary-buffer-message-ring))
    (when (> (length wl-summary-buffer-message-ring)
	     wl-summary-message-ring-max)
      (setcdr (nthcdr (1- wl-summary-message-ring-max)
		      wl-summary-buffer-message-ring)
	      nil))))

(defun wl-summary-pop-message (&optional current-number)
  (when wl-summary-buffer-message-ring
    (when current-number
      (setq wl-summary-buffer-message-ring
	    (nconc wl-summary-buffer-message-ring (list current-number))))
    (prog1
	(car wl-summary-buffer-message-ring)
      (setq wl-summary-buffer-message-ring
	    (cdr wl-summary-buffer-message-ring)))))

(defsubst wl-summary-message-status (&optional number)
  (elmo-message-status wl-summary-buffer-elmo-folder
		       (or number (wl-summary-message-number))))

(defun wl-summary-update-mark-and-highlight-window (&optional win beg)
  "A function to be called as window-scroll-functions."
  (with-current-buffer (window-buffer win)
    (when (eq major-mode 'wl-summary-mode)
      (let ((beg (or beg (window-start win)))
	    (end (condition-case nil
		     (window-end win t)	; old emacsen doesn't support 2nd arg.
		   (error (window-end win)))))
	(save-excursion
	  (goto-char beg)
	  (while (and (< (point) end) (not (eobp)))
	    (when (or (null (get-text-property (point) 'face))
		      (wl-summary-persistent-mark-invalid-p))
	      (wl-summary-update-persistent-mark (wl-summary-message-number)))
	    (forward-line)))))
    (set-buffer-modified-p nil)))

(defun wl-summary-window-scroll-functions ()
  (cond ((and wl-summary-highlight
	      wl-summary-lazy-highlight
	      wl-summary-lazy-update-mark)
	 (list 'wl-summary-update-mark-and-highlight-window))
	((and wl-summary-highlight
	      wl-summary-lazy-highlight)
	 (list 'wl-highlight-summary-window))
	(wl-summary-lazy-update-mark
	 (list 'wl-summary-update-mark-window))))

(defun wl-summary-after-resize-function (frame)
  "Called from `window-size-change-functions'."
  (save-excursion
    (save-selected-window
      (select-frame frame)
      (walk-windows
       (lambda (window)
	 (set-buffer (window-buffer window))
	 (when (eq major-mode 'wl-summary-mode)
	   (run-hook-with-args 'wl-summary-buffer-window-scroll-functions
			       window)))
       'nomini frame))))

;; Handler of event from elmo-folder
(defun wl-summary-update-persistent-mark-on-event (buffer numbers)
  (with-current-buffer buffer
    (save-excursion
      (if wl-summary-lazy-update-mark
	  (let ((window-list (get-buffer-window-list (current-buffer) 'nomini t))
		invalidate)
	    (dolist (number numbers)
	      (when (wl-summary-message-visible-p number)
		(if (catch 'visible
		      (let ((window-list window-list)
			    win)
			(while (setq win (car window-list))
			  (when (wl-summary-jump-to-msg number
							(window-start win)
							(window-end win))
			    (throw 'visible t))
			  (setq window-list (cdr window-list)))))
		    (wl-summary-update-persistent-mark number)
		  (setq invalidate t))))
	    (when invalidate
	      (wl-summary-invalidate-persistent-mark)
	      (dolist (win window-list)
		(wl-summary-validate-persistent-mark
		 (window-start win)
		 (window-end win)))))
	(dolist (number numbers)
	  (when (and (wl-summary-message-visible-p number)
		     (wl-summary-jump-to-msg number))
	    (wl-summary-update-persistent-mark number)))))))

(defun wl-summary-buffer-attach ()
  (when wl-summary-buffer-elmo-folder
    (elmo-connect-signal
     wl-summary-buffer-elmo-folder
     'flag-changed
     (current-buffer)
     (elmo-define-signal-handler (buffer folder numbers)
       (wl-summary-update-persistent-mark-on-event buffer numbers)))
    (elmo-connect-signal
     wl-summary-buffer-elmo-folder
     'status-changed
     (current-buffer)
     (elmo-define-signal-handler (buffer folder numbers)
       (wl-summary-update-persistent-mark-on-event buffer numbers)))
    (elmo-connect-signal
     wl-summary-buffer-elmo-folder
     'update-overview
     (current-buffer)
     (elmo-define-signal-handler (buffer folder number)
       (with-current-buffer buffer
	 (wl-summary-rescan-message number))))))

(defun wl-summary-buffer-detach ()
  (when (and (eq major-mode 'wl-summary-mode)
	     wl-summary-buffer-elmo-folder)
    (elmo-disconnect-signal 'flag-changed (current-buffer))
    (elmo-disconnect-signal 'status-changed (current-buffer))
    (elmo-disconnect-signal 'update-overview (current-buffer))))

(defun wl-status-update ()
  (interactive)
  (wl-address-init))

(defun wl-summary-display-top ()
  (interactive)
  (goto-char (point-min))
  (run-hooks 'wl-summary-buffer-window-scroll-functions)
  (if wl-summary-buffer-disp-msg
      (wl-summary-redisplay)))

(defun wl-summary-display-bottom ()
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (run-hooks 'wl-summary-buffer-window-scroll-functions)
  (if wl-summary-buffer-disp-msg
      (wl-summary-redisplay)))

(defun wl-summary-count-unread ()
  (let ((flag-count (elmo-folder-count-flags wl-summary-buffer-elmo-folder)))
    (setq wl-summary-buffer-new-count
	  (or (cdr (assq 'new flag-count)) 0)
	  wl-summary-buffer-unread-count
	  (or (cdr (assq 'unread flag-count)) 0)
	  wl-summary-buffer-answered-count
	  (or (cdr (assq 'answered flag-count)) 0))
    flag-count))

(defun wl-summary-message-string (&optional use-cache)
  "Return full body string of current message.
If optional USE-CACHE is non-nil, use cache if exists."
  (let ((number (wl-summary-message-number))
	(folder wl-summary-buffer-elmo-folder))
    (if (null number)
	(message "No message.")
      (elmo-message-fetch-string folder
				 number
				 (elmo-make-fetch-strategy
				  'entire
				  use-cache ; use cache
				  nil ; save cache (should `t'?)
				  (and
				   use-cache
				   (elmo-file-cache-get-path
				    (elmo-message-field folder number
							'message-id))))
				 'unread))))

(defun wl-summary-reedit (&optional arg)
  "Re-edit current message.
If ARG is non-nil, Supersedes message"
  (interactive "P")
  (wl-summary-toggle-disp-msg 'off)
  (cond
   ((null (wl-summary-message-number))
    (message "No message."))
   (arg
    (wl-summary-supersedes-message))
   ((string= (wl-summary-buffer-folder-name) wl-draft-folder)
    (wl-draft-reedit (wl-summary-message-number))
    (if (wl-message-news-p)
	(mail-position-on-field "Newsgroups")
      (mail-position-on-field "To")))
   (t
    (wl-draft-edit-string (wl-summary-message-string 'maybe)))))

(defun wl-summary-resend-bounced-mail ()
  "Re-mail the current message.
This only makes sense if the current message is a bounce message which
contains some mail you have written but has been bounced back to
you."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (set-buffer (wl-message-get-original-buffer))
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (cond
       ((and
	 (re-search-forward
	  (concat "^\\($\\|[Cc]ontent-[Tt]ype:[ \t]+multipart/\\(report\\|mixed\\)\\)") nil t)
	 (not (bolp))
	 (re-search-forward "boundary=\"\\([^\"]+\\)\"" nil t))
	(let ((boundary (buffer-substring (match-beginning 1) (match-end 1)))
	      start)
	  (cond
	   ((and (setq start (re-search-forward
			      (concat "^--" boundary "\n"
				      "\\([Cc]ontent-[Dd]escription:.*\n\\)?"
				      "[Cc]ontent-[Tt]ype:[ \t]+"
				      "\\(message/rfc822\\|text/rfc822-headers\\).*\n"
				      "\\(.+\n\\)*\n") nil t))
		 (re-search-forward
		  (concat "\n\\(--" boundary "\\)--\n") nil t))
	    (wl-draft-edit-string (buffer-substring start (match-beginning 1))))
	   (t
	    (message "Seems no message/rfc822 part.")))))
       ((let ((case-fold-search t))
	  (re-search-forward wl-rejected-letter-start nil t))
	(skip-chars-forward " \t\n")
	(wl-draft-edit-string (buffer-substring (point) (point-max))))
       (t
	(message "Does not appear to be a rejected letter."))))))

(defun wl-summary-detect-mark-position ()
  (let ((column wl-summary-buffer-number-column)
	(formatter wl-summary-buffer-line-formatter)
	(lang wl-summary-buffer-weekday-name-lang)
	(dummy-number 10000)
	(dummy-temp (char-to-string 200))
	;; bind only for the check.
	(wl-summary-new-uncached-mark (char-to-string 201))
	(wl-summary-persistent-mark-priority-list '(new))     ; ditto.
	wl-summary-highlight
	temp persistent)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (setq wl-summary-buffer-number-column column
	    wl-summary-buffer-line-formatter formatter
	    wl-summary-buffer-weekday-name-lang lang)
      (insert
       (wl-summary-create-line
	(elmo-msgdb-make-message-entity
	 (luna-make-entity 'modb-entity-handler)
	 :number dummy-number
	 :from "foo"
	 :subject "bar"
	 :size 100)
	nil
	dummy-temp
	(let ((status (elmo-message-status nil dummy-number)))
	  (elmo-message-status-set-flags status '(new))
	  (elmo-message-status-set-killed status nil)
	  status)))
      (goto-char (point-min))
      (setq temp (save-excursion
		   (when (search-forward dummy-temp nil t)
		     (current-column)))
	    persistent
	    (save-excursion
	      (when (search-forward wl-summary-new-uncached-mark nil t)
		(current-column)))))
    (setq wl-summary-buffer-temp-mark-column temp
	  wl-summary-buffer-persistent-mark-column persistent)))

(defun wl-summary-buffer-set-folder (folder)
  (wl-summary-buffer-detach)
  (if (stringp folder)
      (setq folder (wl-folder-get-elmo-folder folder)))
  (setq wl-summary-buffer-elmo-folder folder)
  (make-local-variable 'wl-message-buffer)
  (setq wl-summary-buffer-mime-charset (wl-folder-mime-charset
					(elmo-folder-name-internal folder)))
  (setq wl-summary-buffer-weekday-name-lang
	(or (wl-get-assoc-list-value
	     wl-folder-weekday-name-lang-alist
	     (elmo-folder-name-internal folder))
	    wl-summary-weekday-name-lang))
  (setq wl-summary-buffer-thread-indent-set
	(wl-get-assoc-list-value
	 wl-folder-thread-indent-set-alist
	 (elmo-folder-name-internal folder)))
  (setq wl-summary-buffer-number-column
	(or (wl-get-assoc-list-value wl-summary-number-column-alist
				     (wl-summary-buffer-folder-name))
	    wl-summary-default-number-column))
  (wl-line-formatter-setup
   wl-summary-buffer-line-formatter
   (setq wl-summary-buffer-line-format
	 (or (wl-get-assoc-list-value
	      wl-folder-summary-line-format-alist
	      (elmo-folder-name-internal folder))
	     wl-summary-line-format))
   wl-summary-line-format-spec-alist)
  (wl-line-formatter-setup
   wl-summary-buffer-mode-line-formatter
   wl-summary-mode-line-format
   wl-summary-mode-line-format-spec-alist)
  (setq wl-summary-buffer-persistent
	(wl-folder-persistent-p (elmo-folder-name-internal folder)))
  (elmo-folder-set-persistent-internal folder wl-summary-buffer-persistent)
  (wl-summary-buffer-attach)
  ;; process duplicates.
  (elmo-folder-set-process-duplicates-internal
   folder (cdr (elmo-string-matched-assoc
		(elmo-folder-name-internal folder)
		wl-folder-process-duplicates-alist)))
  (setq
   wl-thread-indent-level-internal
   (or (nth 0 wl-summary-buffer-thread-indent-set)
       wl-thread-indent-level)
   wl-thread-have-younger-brother-str-internal
   (or (nth 1 wl-summary-buffer-thread-indent-set)
       wl-thread-have-younger-brother-str)
   wl-thread-youngest-child-str-internal
   (or (nth 2 wl-summary-buffer-thread-indent-set)
       wl-thread-youngest-child-str)
   wl-thread-vertical-str-internal
   (or (nth 3 wl-summary-buffer-thread-indent-set)
       wl-thread-vertical-str)
   wl-thread-horizontal-str-internal
   (or (nth 4 wl-summary-buffer-thread-indent-set)
       wl-thread-horizontal-str)
   wl-thread-space-str-internal
   (or (nth 5 wl-summary-buffer-thread-indent-set)
       wl-thread-space-str))
  (run-hooks 'wl-summary-buffer-set-folder-hook))

(defun wl-summary-mode ()
  "Major mode for reading threaded messages.
See Info under Wanderlust for full documentation.

Special commands:
\\{wl-summary-mode-map}

Entering Folder mode calls the value of `wl-summary-mode-hook'."
  (interactive)
  (unless (interactive-p) (kill-all-local-variables))
  (setq major-mode 'wl-summary-mode)
  (setq mode-name "Summary")
  (use-local-map wl-summary-mode-map)
;;;  (setq default-directory (or wl-tmp-dir (expand-file-name "~/")))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (when (boundp 'show-trailing-whitespace)
    (setq show-trailing-whitespace nil))
;;;  (make-local-variable 'tab-width)
;;;  (setq tab-width 1)
  (buffer-disable-undo (current-buffer))
  (setq selective-display t
	selective-display-ellipses nil)
  (when (boundp 'bidi-paragraph-direction)
    (set 'bidi-paragraph-direction 'left-to-right))
  (wl-mode-line-buffer-identification '(wl-summary-buffer-mode-line))
  (easy-menu-add wl-summary-mode-menu)
  (setq wl-summary-buffer-window-scroll-functions
	(wl-summary-window-scroll-functions))
  (when wl-summary-buffer-window-scroll-functions
    (let ((hook (if wl-on-xemacs 'pre-idle-hook 'window-scroll-functions)))
      (if (fboundp 'make-local-hook)
	  (make-local-hook hook))
      (dolist (function wl-summary-buffer-window-scroll-functions)
	(add-hook hook function nil t)))
    (add-hook 'window-size-change-functions
	      #'wl-summary-after-resize-function))
  (dolist (hook '(change-major-mode-hook kill-buffer-hook))
    (if (fboundp 'make-local-hook)
	(make-local-hook hook))
    (add-hook hook #'wl-summary-buffer-detach nil t))
  ;; This hook may contain the function `wl-setup-summary' for reasons
  ;; of system internal to accord facilities for the Emacs variants.
  (run-hooks 'wl-summary-mode-hook))

;;;
(defun wl-summary-overview-entity-compare-by-size (x y)
   "Compare entity X and Y by size."
   (< (elmo-message-entity-field x 'size)
      (elmo-message-entity-field y 'size)))


(defun wl-summary-overview-entity-compare-by-date (x y)
  "Compare entity X and Y by date."
  (condition-case nil
      (elmo-time<
       (elmo-message-entity-field x 'date)
       (elmo-message-entity-field y 'date))
    (error))) ;; ignore error.

(defun wl-summary-overview-entity-compare-by-number (x y)
   "Compare entity X and Y by number."
  (<
   (elmo-message-entity-number x)
   (elmo-message-entity-number y)))

(defun wl-summary-overview-entity-compare-by-from (x y)
  "Compare entity X and Y by from."
  (string<
   (or (elmo-message-entity-field x 'from)
       wl-summary-no-from-message)
   (or (elmo-message-entity-field y 'from)
       wl-summary-no-from-message)))

(defun wl-summary-overview-entity-compare-by-subject (x y)
  "Compare entity X and Y by subject."
  (string< (elmo-message-entity-field x 'subject)
	   (elmo-message-entity-field y 'subject)))

(defun wl-summary-get-list-info (entity)
  "Returns (\"ML-name\" . ML-count) of ENTITY."
  (or (elmo-message-entity-field entity 'ml-info)
      (lexical-let ((entity entity))
	(let* ((getter (lambda (field)
			 (elmo-message-entity-field entity field)))
	       (name (elmo-find-list-match-value
		      elmo-mailing-list-name-spec-list
		      getter))
	       (count (elmo-find-list-match-value
		       elmo-mailing-list-count-spec-list
		       getter)))
	  (cons name (and count (string-to-number count)))))))

(defun wl-summary-overview-entity-compare-by-list-info (x y)
  "Compare entity X and Y by mailing-list info."
  (let* ((list-info-x (wl-summary-get-list-info x))
	 (list-info-y (wl-summary-get-list-info y)))
    (if (equal (car list-info-x) (car list-info-y))
	(if (equal (cdr list-info-x) (cdr list-info-y))
	    (wl-summary-overview-entity-compare-by-date x y)
	  (< (or (cdr list-info-x) 0)
	     (or (cdr list-info-y) 0)))
      (string< (or (car list-info-x) "")
	       (or (car list-info-y) "")))))

(defun wl-summary-define-sort-command ()
  "Define functions to sort summary lines by `wl-summary-sort-specs'."
  (interactive)
  (dolist (sort-by wl-summary-sort-specs)
    (fset (intern (format "wl-summary-sort-by-%s" sort-by))
	  `(lambda (&optional reverse)
	     ,(format "\
Sort summary lines into the order by %s.
If optional argument REVERSE is non-nil, sort into descending order.

This function is defined by `wl-summary-define-sort-command'." sort-by)
	     (interactive "P")
	     (wl-summary-rescan ,(symbol-name sort-by) reverse)))))

(defun wl-summary-sort-function-from-spec (spec reverse)
  (let (function)
    (when (string-match "^!\\(.+\\)$" spec)
      (setq spec (match-string 1 spec)
	    reverse (not reverse)))
    (setq function
	  (intern (format "wl-summary-overview-entity-compare-by-%s" spec)))
    (if reverse
	`(lambda (x y) (not (,function x y)))
      function)))

(defun wl-summary-sort-messages (numbers sort-by reverse)
  (let* ((functions (mapcar
		     (lambda (spec)
		       (wl-summary-sort-function-from-spec spec reverse))
		     (if (listp sort-by) sort-by (list sort-by))))
	 (predicate (if (= (length functions) 1)
			(car functions)
		      (lambda (x y)
			(let ((functions functions))
			  (catch 'done
			    (dolist (function functions)
			      (when (funcall function x y)
				(throw 'done t))
			      (when (funcall function y x)
				(throw 'done nil)))))))))
    (mapcar #'elmo-message-entity-number
	    (sort (mapcar (lambda (number)
			    (elmo-message-entity
			     wl-summary-buffer-elmo-folder
			     number))
			  numbers)
		  predicate))))

(defun wl-summary-rescan (&optional sort-by reverse disable-killed disable-thread)
  "Rescan current folder without updating."
  (interactive)
  (let ((elmo-mime-charset wl-summary-buffer-mime-charset)
	gc-message			; for XEmacs
	(inhibit-read-only t)
	(buffer-read-only nil)
	(numbers (elmo-folder-list-messages wl-summary-buffer-elmo-folder
					    (not disable-killed) t)) ; in-msgdb
	(wl-thread-saved-entity-hashtb-internal (and (not disable-thread)
						     wl-thread-entity-hashtb))
	(wl-summary-search-parent-by-subject-regexp
	 (and disable-thread wl-summary-search-parent-by-subject-regexp))
	(wl-summary-divide-thread-when-subject-changed
	 (and disable-thread wl-summary-divide-thread-when-subject-changed))
	num
	expunged)
    (erase-buffer)
    (message "Re-scanning...")
    (when (and sort-by numbers)
      (let ((action  (if reverse "Reverse sorting" "Sorting")))
	(message "%s by %s..." action sort-by)
	(setq numbers (wl-summary-sort-messages numbers sort-by reverse))
	(message "%s by %s...done" action sort-by)))
    (setq num (length numbers))
    (setq wl-thread-entity-hashtb (elmo-make-hash (* num 2))
	  wl-thread-entity-list nil
	  wl-thread-entities nil
	  wl-summary-scored nil
	  wl-summary-buffer-number-list nil
	  wl-summary-buffer-persistent-mark-version 0
	  wl-summary-buffer-target-mark-list nil
	  wl-summary-buffer-temp-mark-list nil
	  wl-summary-delayed-update nil)
    (elmo-kill-buffer wl-summary-search-buf-name)
    (elmo-with-progress-display (wl-summary-insert-line num)
	"Constructing summary structure"
      (dolist (number numbers)
	(wl-summary-insert-message (elmo-message-entity
				    wl-summary-buffer-elmo-folder
				    number)
				   wl-summary-buffer-elmo-folder
				   nil))
      (while wl-summary-delayed-update
	(message "Parent (%d) of message %d is no entity"
		 (caar wl-summary-delayed-update)
		 (elmo-message-entity-number
		  (cdar wl-summary-delayed-update)))
	(wl-summary-insert-message
	 (cdar wl-summary-delayed-update)
	 wl-summary-buffer-elmo-folder nil t)
	(setq wl-summary-delayed-update (cdr wl-summary-delayed-update))))
    (when (eq wl-summary-buffer-view 'thread)
      (wl-thread-insert-top))
    (when wl-use-scoring
      (wl-summary-score-headers (wl-summary-rescore-msgs
				 wl-summary-buffer-number-list)
				t)
      (when (and wl-summary-scored
		 (setq expunged (wl-summary-score-update-all-lines)))
	(message "%d message(s) are expunged by scoring." (length expunged))))
    (wl-summary-set-message-modified)
    (wl-summary-count-unread)
    (wl-summary-update-modeline)
    (goto-char (point-max))
    (forward-line -1)
    (set-buffer-modified-p nil)))

(defun wl-summary-rescan-message (number &optional reparent)
  "Rescan current message without updating."
  (interactive (list (wl-summary-message-number) current-prefix-arg))
  (let ((start-number (wl-summary-message-number))
	(start-column (current-column)))
    (when (wl-summary-jump-to-msg number)
      (let* ((folder wl-summary-buffer-elmo-folder)
	     (entity (elmo-message-entity folder number))
	     (inhibit-read-only t))
	(if (eq wl-summary-buffer-view 'thread)
	    (let* ((thread-entity (wl-thread-get-entity number))
		   (thread-parent (wl-thread-entity-get-parent thread-entity))
		   (entity-parent (elmo-message-entity-number
				   (elmo-message-entity-parent folder entity)))
		   update-top-list)
	      (if (and (not reparent)
		       (eq thread-parent entity-parent))
		  (progn
		    (wl-thread-entity-set-linked thread-entity nil)
		    (wl-thread-update-line-on-buffer-sub nil number))
		(let ((replacements
		       (cons number
			     (wl-thread-entity-get-descendant thread-entity))))
		  (wl-thread-delete-message number 'deep 'update)
		  (wl-thread-cleanup-symbols replacements)
		  (dolist (number replacements)
		    (setq update-top-list
			  (nconc
			   update-top-list
			   (wl-summary-insert-thread
			    (elmo-message-entity folder number)
			    folder
			    'update))))
		  (when update-top-list
		    (wl-thread-update-indent-string-thread
		     (elmo-uniq-list update-top-list))))))
	    (delete-region (point-at-bol) (1+ (point-at-eol)))
	    (wl-summary-insert-line
	     (wl-summary-create-line entity nil
				     (wl-summary-temp-mark number)
				     (elmo-message-status folder number)))))
      (when (and wl-summary-buffer-disp-msg
		 wl-summary-buffer-current-msg)
	(save-excursion
	  (when (wl-summary-jump-to-msg wl-summary-buffer-current-msg)
	    (wl-highlight-summary-displaying))))
      (wl-summary-set-message-modified)
      (wl-summary-jump-to-msg start-number)
      (move-to-column start-column))))

(defun wl-summary-next-folder-or-exit (&optional next-entity upward)
  (if (and next-entity
	   wl-auto-select-next)
      (let (retval)
	(wl-summary-toggle-disp-msg 'off)
	(unwind-protect
	    (setq retval
		  (wl-summary-goto-folder-subr next-entity
					       'force-update
					       nil
					       nil ; not sticky
					       t   ; interactive!
					       ))
	  (wl-folder-set-current-entity-id (wl-folder-get-entity-id next-entity))
	  (if (and (eq retval 'more-next)
		   (memq wl-auto-select-next '(unread skip-no-unread))
		   (memq this-command wl-summary-next-no-unread-command))
	      (if upward
		  (wl-summary-up
		   t (eq wl-auto-select-next 'skip-no-unread))
		(goto-char (point-max))
		(forward-line -1)
		(wl-summary-down
		 t (eq wl-auto-select-next 'skip-no-unread))))))
    (wl-summary-exit)))

(defun wl-summary-entity-info-msg (entity finfo)
  (or (and entity
	   (concat
	    (if (memq 'ask-folder wl-use-folder-petname)
		(wl-folder-get-petname entity)
	      entity)
	    (if (null (car finfo))
		" (? new/? unread)"
	      (format
	       " (%d new/%d unread)"
	       (nth 0 finfo)
	       (+ (nth 0 finfo)
		  (nth 1 finfo))))))
      "folder mode"))

(defun wl-summary-set-message-modified ()
  (setq wl-summary-buffer-message-modified t))
(defun wl-summary-message-modified-p ()
  wl-summary-buffer-message-modified)
(defun wl-summary-set-thread-modified ()
  (setq wl-summary-buffer-thread-modified t))
(defun wl-summary-thread-modified-p ()
  wl-summary-buffer-thread-modified)

(defun wl-summary-exec-with-confirmation (&optional message)
  (when wl-summary-buffer-temp-mark-list
    (if (y-or-n-p (or message
		      (format "Execute marks in %s? "
			      (wl-summary-buffer-folder-name))))
	(progn
	  (wl-summary-exec)
	  (if wl-summary-buffer-temp-mark-list
	      (error "Some execution was failed")))
      ;; temp-mark-list is remained.
      (message ""))))

(defun wl-summary-cleanup-temp-marks ()
  (when wl-summary-buffer-temp-mark-list
    (wl-summary-exec-with-confirmation
     (format "Execute marks in %s? (answer \"n\" to discard them) "
	     (wl-summary-buffer-folder-name))))
  (wl-summary-delete-all-temp-marks 'no-msg)
  (setq wl-summary-scored nil))

;; a subroutine for wl-summary-exit/wl-save-status
;; Note that folder is not commited here.
(defun wl-summary-save-view ()
  ;; already in summary buffer.
  (when wl-summary-buffer-persistent
    ;; save the current summary buffer view.
    (if (and wl-summary-cache-use
	     (or (wl-summary-message-modified-p)
		 (wl-summary-thread-modified-p)))
	(wl-summary-save-view-cache))))

(defun wl-summary-save-status ()
  "Save summary view and msgdb."
  (interactive)
  (if (interactive-p) (message "Saving summary status..."))
  (wl-summary-save-view)
  (elmo-folder-commit wl-summary-buffer-elmo-folder)
  (elmo-folder-check wl-summary-buffer-elmo-folder)
  (if wl-use-scoring (wl-score-save))
  (if (interactive-p) (message "Saving summary status...done")))

(defun wl-summary-force-exit ()
  "Exit current summary.  Buffer is deleted even the buffer is sticky."
  (interactive)
  (wl-summary-exit 'force-exit))

(defun wl-summary-exit (&optional force-exit)
  "Exit current summary.  if FORCE-EXIT, exits even the summary is sticky."
  (interactive "P")
  (let ((summary-buf (current-buffer))
	(sticky (wl-summary-sticky-p))
	summary-win
	message-buf message-win
	folder-buf folder-win)
    (run-hooks 'wl-summary-exit-pre-hook)
    (if wl-summary-buffer-exit-function
	(funcall wl-summary-buffer-exit-function)
      (if (or force-exit (not sticky))
	  (wl-summary-cleanup-temp-marks))
      (unwind-protect
	  ;; save summary status
	  (progn
	    (wl-summary-save-view)
	    (if (or force-exit (not sticky))
		(elmo-folder-close wl-summary-buffer-elmo-folder)
	      (elmo-folder-commit wl-summary-buffer-elmo-folder)
	      (elmo-folder-check wl-summary-buffer-elmo-folder))
	    (if wl-use-scoring (wl-score-save)))
	;; for sticky summary
	(wl-delete-all-overlays)
	(setq wl-summary-buffer-disp-msg nil)
	(elmo-kill-buffer wl-summary-search-buf-name)
	;; delete message window if displayed.
	(if (and wl-message-buffer (get-buffer-window wl-message-buffer))
	    (delete-window (get-buffer-window wl-message-buffer)))
	(if (and wl-summary-use-frame
		 (> (length (visible-frame-list)) 1))
	    (delete-frame))
	(if (setq folder-buf (get-buffer wl-folder-buffer-name))
	    (if wl-summary-use-frame
		(let (select-frame)
		  (save-selected-window
		    (dolist (frame (visible-frame-list))
		      (select-frame frame)
		      (if (get-buffer-window folder-buf)
			  (setq select-frame frame))))
		  (if select-frame
		      (select-frame select-frame)
		    (switch-to-buffer folder-buf)))
	      (if (setq folder-win (get-buffer-window folder-buf))
		  ;; folder win is already displayed.
		  (select-window folder-win)
		;; folder win is not displayed.
		(switch-to-buffer folder-buf)))
	  ;; currently no folder buffer
	  (wl-folder))
	(and wl-folder-move-cur-folder
	     wl-folder-buffer-cur-point
	     (goto-char wl-folder-buffer-cur-point))
	(setq wl-folder-buffer-cur-path nil)
	(setq wl-folder-buffer-last-visited-entity-id wl-folder-buffer-cur-entity-id)
	(setq wl-folder-buffer-cur-entity-id nil)
	(wl-delete-all-overlays)
	(if wl-summary-exit-next-move
	    (wl-folder-next-unsync t)
	  (beginning-of-line))
	(if (setq summary-win (get-buffer-window summary-buf))
	    (delete-window summary-win))
	(if (or force-exit
		(not sticky))
	    (progn
	      (set-buffer summary-buf)
	      (kill-buffer summary-buf)))
	(run-hooks 'wl-summary-exit-hook)))))

(defun wl-summary-suspend ()
  (interactive)
  (wl-summary-exit)
  (wl-folder-suspend))

(defun wl-summary-sync-force-update (&optional unset-cursor no-check)
  (interactive)
  (wl-summary-sync-update unset-cursor nil nil no-check))

(defsubst wl-summary-sync-all-init ()
  (wl-summary-cleanup-temp-marks)
  (erase-buffer)
  (wl-summary-set-message-modified)
  (setq wl-thread-entity-hashtb (elmo-make-hash
				 (* (elmo-folder-length
				     wl-summary-buffer-elmo-folder)
				    2)))
  (setq wl-thread-entity-list nil)
  (setq wl-thread-entities nil)
  (setq wl-summary-buffer-number-list nil)
  (setq wl-summary-buffer-target-mark-list nil)
  (setq wl-summary-buffer-temp-mark-list nil))

(defun wl-summary-sync (&optional unset-cursor force-range)
  (interactive)
  (let* ((folder wl-summary-buffer-elmo-folder)
	 (inhibit-read-only t)
	 (buffer-read-only nil)
	 (msgdb-dir (elmo-folder-msgdb-path folder))
	 (range (or force-range (wl-summary-input-range
				 (elmo-folder-name-internal folder)))))
    (when (symbolp range)
      (setq range (symbol-name range)))
    (cond ((string-match "rescan" range)
	   (let ((msg (wl-summary-message-number))
		 (wl-use-scoring (if (string-match "noscore" range)
				     nil
				   wl-use-scoring)))
	     (wl-summary-rescan nil
				nil
				(string-match "noscore" range)
				(string-match "thread" range))
	     (and msg (wl-summary-jump-to-msg msg))))
	  ((string= range "mark")
	   (let ((msg (wl-summary-message-number)))
	     (call-interactively 'wl-summary-sync-marks)
	     (and msg (wl-summary-jump-to-msg msg))))
	  ((string= range "cache-status")
	   (let ((msg (wl-summary-message-number)))
	     (wl-summary-resume-cache-status)
	     (and msg (wl-summary-jump-to-msg msg))))
	  ((string= range "no-sync"))
	  ((or (string-match "^last:" range)
	       (string-match "^first:" range))
	   (wl-summary-goto-folder-subr (concat "/" range "/"
						(elmo-folder-name-internal
						 folder))
					'force-update nil nil t))
	  (t
	   (wl-summary-sync-update unset-cursor
				   (string-match "entirely" range)
				   (string-match "all" range))))))

(defvar wl-summary-edit-addresses-candidate-fields
  ;; First element becomes default.
  '("from" "to" "cc"))

(defun wl-summary-edit-addresses-collect-candidate-fields (mime-charset)
  (let ((fields wl-summary-edit-addresses-candidate-fields)
	body candidates components)
    (while fields
      (setq body
	    (mapconcat 'identity (elmo-multiple-field-body (car fields))
		       ","))
      (setq body (wl-parse-addresses body))
      (if body (setq candidates (append candidates body)))
      (setq fields (cdr fields)))
    (setq candidates (elmo-uniq-list candidates))
    (elmo-with-enable-multibyte
      (mapcar
       (lambda (x)
	 (setq components (std11-extract-address-components x))
	 (cons (nth 1 components)
	       (and (car components)
		    (eword-decode-string
		     (elmo-mime-charset-decode-string
		      (car components)
		      mime-charset)))))
       candidates))))

(defun wl-summary-edit-addresses-subr (the-email name-in-addr)
  ;; returns nil if there's no change.
  (if (elmo-get-hash-val (downcase the-email) wl-address-petname-hash)
      (let (char)
	(message "'%s' already exists. (e)dit/(d)elete/(c)ancel?"
		 the-email)
	(while (not (or (eq (setq char (read-char)) ?\r)
			(eq char ?\n)
			(eq char (string-to-char " "))
			(eq char ?e)
			(eq char ?c)
			(eq char ?d)))
	  (message
	   "Please answer `e' or `d' or `c'. (e)dit/(d)elete/(c)ancel?"))
	(cond
	 ((or (eq char ?e)
	      (eq char ?\n)
	      (eq char ?\r)
	      (eq char (string-to-char " ")))
	  ;; Change Addresses
	  (wl-address-add-or-change
	   the-email
	   (wl-address-header-extract-realname
	    (cdr (assoc
		  (let ((completion-ignore-case t) comp)
		    (setq comp
			  (try-completion the-email wl-address-completion-list))
		    (if (equal comp t) the-email comp))
		  wl-address-completion-list))))
	  "edited")
	 ((eq char ?d)
	  ;; Delete Addresses
	  (if (y-or-n-p (format "Delete '%s'? "
				the-email))
	      (progn
		(wl-address-delete the-email)
		"deleted")
	    (message "")
	    nil))
	 (t (message "")
	    nil)))
    ;; Add Petname
    (wl-address-add-or-change the-email name-in-addr)
    "added"))

(defun wl-summary-edit-addresses (&optional addr-str)
  "Edit address book interactively.
Optional argument ADDR-STR is used as a target address if specified."
  (interactive (if current-prefix-arg
		   (list (read-from-minibuffer "Target address: "))))
  (if (null (wl-summary-message-number))
      (message "No message.")
    (save-excursion
      (let* ((charset wl-summary-buffer-mime-charset)
	     (candidates
	      (with-current-buffer (wl-summary-get-original-buffer)
		(wl-summary-edit-addresses-collect-candidate-fields
		 charset)))
	     address pair result)
	(if addr-str
	    (setq address addr-str)
	  (when candidates
	    (setq address (car (car candidates)))
	    (setq address
		  (completing-read
		   (format "Target address (%s): " address)
		   (mapcar
		    (lambda (x) (cons (car x) (car x)))
		    candidates)
		   nil nil nil nil address))))
	(when address
	  (setq pair (assoc address candidates))
	  (unless pair
	    (setq pair (cons address nil)))
	  (when (setq result (wl-summary-edit-addresses-subr (car pair) (cdr pair)))
	    ;; update alias
	    (wl-status-update)
	    (setq address (assoc (car pair) wl-address-list))
	    (if address
		(message "%s, %s, <%s> is %s."
			 (nth 2 address)
			 (nth 1 address)
			 (nth 0 address)
			 result)))
;;; i'd like to update summary-buffer, but...
;;;	  (wl-summary-rescan)
	  (run-hooks 'wl-summary-edit-addresses-hook))))))

(defun wl-summary-incorporate (&optional arg)
  "Check and prefetch all uncached messages.
If ARG is non-nil, checking is omitted."
  (interactive "P")
  (unless arg
    (save-excursion
      (wl-summary-sync-force-update)))
  (wl-summary-prefetch-region-no-mark (point-min) (point-max)
				      wl-summary-incorporate-marks))

(defun wl-summary-force-prefetch ()
  "All uncached messages are cached."
  (interactive)
  (unless (elmo-folder-local-p wl-summary-buffer-elmo-folder)
    (let* ((targets (elmo-folder-list-flagged wl-summary-buffer-elmo-folder
					      'uncached 'in-msgdb))
	   (count 0)
	   wl-prefetch-confirm
	   wl-prefetch-threshold
	   (length (length targets))
	   msg)
      (save-excursion
	(elmo-with-progress-display (wl-summary-prefetch-message length)
	    "Retrieving"
	  (goto-char (point-min))
	  (dolist (target targets)
	    (when (if (not (wl-thread-entity-parent-invisible-p
			    (wl-thread-get-entity target)))
		      (progn
			(wl-summary-jump-to-msg target)
			(wl-summary-prefetch-msg
			 (wl-summary-message-number)))
		    (wl-summary-prefetch-msg target))
	      (incf count))
	    (elmo-progress-notify 'wl-summary-prefetch-message)))
	(message "Retrieved %d/%d message(s)" count length)))))

(defun wl-summary-prefetch-msg (number &optional arg)
  "Prefetch message and return non-nil value. If skipped, return nil."
  ;; prefetching procedure.
  (save-excursion
    (let* ((size (elmo-message-field wl-summary-buffer-elmo-folder
				     number 'size))
	   (file-cached (elmo-file-cache-exists-p
			 (elmo-message-field wl-summary-buffer-elmo-folder
					     number 'message-id)))
	   (force-read (and size
			    (or file-cached
				(and (null wl-prefetch-confirm) arg)
				(null wl-prefetch-threshold)
				(< size wl-prefetch-threshold))))
	   mark new-mark)
      (ignore-errors
	(when (and (or arg (not file-cached))
		   size (not force-read) wl-prefetch-confirm)
	  (let ((wl-message-entity (elmo-message-entity
				    wl-summary-buffer-elmo-folder
				    number)))
	    (setq force-read
		  (save-restriction
		    (widen)
		    (y-or-n-p
		     (format
		      "Message from %s has %s bytes.  Prefetch it? "
		      (concat
		       "[ "
		       (save-match-data
			 (wl-set-string-width
			  17
			  (funcall
			   wl-summary-from-function
			   (elmo-delete-char
			    ?\"
			    (or
			     (elmo-message-entity-field
			      wl-message-entity
			      'from)
			     "??")))))
		       " ]")
		      (do ((size (/ size 1024.0) (/ size 1024.0))
			   ;; kilo, mega, giga, tera, peta, exa
			   (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
			  ((< size 1024) (format "%.0f%s" size (car post-fixes))))))))
	    (message "")))		; flush.
	(if force-read
	    (save-excursion
	      (save-match-data
		;; online
		(when (or arg (not file-cached))
		  (elmo-message-encache wl-summary-buffer-elmo-folder
					number))
		(elmo-message-set-cached wl-summary-buffer-elmo-folder
					 number t))
	      t)
	  nil)))))

(defsubst wl-summary-narrow-to-region (beg end)
  (narrow-to-region
   (save-excursion
     (goto-char beg)
     (point-at-bol))
   (save-excursion
     (goto-char end)
     (if (= (current-column) 0)
	 (point-at-bol)
       (point-at-eol)))))

(defun wl-summary-prefetch-region-no-mark (beg end &optional prefetch-marks)
  (interactive "r")
  (let ((count 0)
	targets
	mark length
	entity msg
	start-pos pos)
    (save-excursion
      (setq start-pos (point))
      (save-restriction
	(wl-summary-narrow-to-region beg end)
	;; collect prefetch targets.
	(message "Collecting marks...")
	(goto-char (point-min))
	(while (not (eobp))
	  (setq msg (wl-summary-message-number))
	  (setq mark (wl-summary-persistent-mark msg))
	  (if (or (and (null prefetch-marks)
		       msg
		       (null (elmo-file-cache-exists-p
			      (elmo-message-field
			       wl-summary-buffer-elmo-folder
			       msg
			       'message-id))))
		  (member mark prefetch-marks))
	      (setq targets (nconc targets (list msg))))
	  (setq entity (wl-thread-get-entity msg))
	  (if (or (not (eq wl-summary-buffer-view 'thread))
		  (wl-thread-entity-get-opened entity))
	      (); opened. no hidden children.
	    (setq targets (nconc
			   targets
			   (wl-thread-get-children-msgs-uncached
			    msg prefetch-marks))))
	  (forward-line))
	(setq length (length targets))
	(message "Prefetching...")
	(while targets
	  (when (if (not (wl-thread-entity-parent-invisible-p
			  (wl-thread-get-entity (car targets))))
		    (progn
		      (wl-summary-jump-to-msg (car targets))
		      (wl-summary-prefetch-msg
		       (wl-summary-message-number)))
		  (wl-summary-prefetch-msg (car targets)))
	    (message "Prefetching... %d/%d message(s)"
		     (setq count (+ 1 count)) length))
	  (setq targets (cdr targets)))
	(message "Prefetched %d/%d message(s)" count length)
	(cons count length)))))

(defun wl-summary-delete-marks-on-buffer (marks)
  (while marks
    (wl-summary-unmark (pop marks))))

(defun wl-summary-delete-copy-marks-on-buffer (copies)
  (wl-summary-delete-marks-on-buffer copies))

;;;
(defun wl-summary-delete-all-target-marks ()
  (wl-summary-delete-marks-on-buffer wl-summary-buffer-target-mark-list))

(defun wl-summary-number-list-from-region (beg end)
  (save-excursion
    (save-restriction
      (wl-summary-narrow-to-region beg end)
      (goto-char (point-min))
      (let (number-list)
	(if (eq wl-summary-buffer-view 'thread)
	    (while (not (eobp))
	      (let* ((number (wl-summary-message-number))
		     (entity (wl-thread-get-entity number)))
		(setq number-list
		      (nconc number-list
			     (if (wl-thread-entity-get-opened entity)
				 (list number)
			       (wl-thread-get-children-msgs number))))
		(forward-line)))
	  (while (not (eobp))
	    (setq number-list
		  (nconc number-list (list (wl-summary-message-number))))
	    (forward-line)))
	number-list))))

(defun wl-summary-mark-as-read-region (beg end)
  (interactive "r")
  (let ((number-list (wl-summary-number-list-from-region beg end)))
    (if (null number-list)
	(message "No message.")
      (wl-summary-mark-as-read number-list))))

(defun wl-summary-mark-as-unread-region (beg end)
  (interactive "r")
  (let ((number-list (wl-summary-number-list-from-region beg end)))
    (if (null number-list)
	(message "No message.")
      (wl-summary-mark-as-unread number-list))))

(defun wl-summary-set-flags-region (beg end &optional remove)
  (interactive "r\nP")
  (let ((number-list (wl-summary-number-list-from-region beg end)))
    (if (null number-list)
	(message "No message.")
      (wl-summary-set-flags-internal number-list nil nil remove)
      (wl-summary-count-unread)
      (wl-summary-update-modeline))))

(defun wl-summary-mark-as-answered-region (beg end &optional remove)
  (interactive "r\nP")
  (let ((number-list (wl-summary-number-list-from-region beg end))
	(remove (or remove
		    (elmo-message-flagged-p wl-summary-buffer-elmo-folder
					    (save-excursion
					      (goto-char beg)
					      (wl-summary-message-number))
					    'answered))))
    (if (null number-list)
	(message "No message.")
      (wl-summary-set-persistent-mark-internal remove 'answered
					       number-list
					       nil nil (interactive-p))
      (wl-summary-count-unread)
      (wl-summary-update-modeline))))

(defun wl-summary-mark-as-important-region (beg end &optional remove)
  (interactive "r\nP")
  (let ((number-list (wl-summary-number-list-from-region beg end))
	(remove (or remove
		    (elmo-message-flagged-p wl-summary-buffer-elmo-folder
					    (save-excursion
					      (goto-char beg)
					      (wl-summary-message-number))
					    'important))))
    (if (null number-list)
	(message "No message.")
      (wl-summary-set-persistent-mark-internal remove 'important number-list
					       nil nil (interactive-p))
      (wl-summary-count-unread)
      (wl-summary-update-modeline))))

(defun wl-summary-recover-messages-region (beg end)
  "Recover killed messages in region."
  (interactive "r")
  (let ((number-list (wl-summary-number-list-from-region beg end)))
    (if (null number-list)
	(message "No message.")
      (elmo-folder-recover-messages wl-summary-buffer-elmo-folder
				    number-list))))

(defun wl-summary-mark-as-read-all ()
  (interactive)
  (if (or (not (interactive-p))
	  (y-or-n-p "Mark all messages as read? "))
      (let ((folder wl-summary-buffer-elmo-folder)
	    (cur-buf (current-buffer)))
	(message "Setting all msgs as read...")
	(elmo-folder-unset-flag
	 folder
	 (elmo-folder-list-flagged folder 'unread 'in-msgdb)
	 'unread)
	(wl-folder-update-unread (wl-summary-buffer-folder-name) 0)
	(setq wl-summary-buffer-unread-count 0)
	(setq wl-summary-buffer-new-count    0)
	(wl-summary-update-modeline)
	(message "Setting all msgs as read...done"))))

(defun wl-summary-delete-cache ()
  "Delete cache of current message."
  (interactive)
  (save-excursion
    (let* ((folder wl-summary-buffer-elmo-folder)
	   number)
      (setq number (wl-summary-message-number))
      (elmo-message-set-cached folder number nil)
      (ignore-errors
	(elmo-file-cache-delete
	 (elmo-file-cache-get-path
	  (elmo-message-field wl-summary-buffer-elmo-folder
			      number
			      'message-id)))))))

(defun wl-summary-resume-cache-status ()
  "Resume the cache status of all messages in the current folder."
  (interactive)
  (let ((folder wl-summary-buffer-elmo-folder)
	number msgid)
    (message "Resuming cache status...")
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq number (wl-summary-message-number))
	(setq msgid (elmo-message-field folder number 'message-id))
	(elmo-message-set-cached folder number
				 (elmo-file-cache-exists-p msgid))
	(forward-line))
      (wl-summary-count-unread)
      (wl-summary-update-modeline)
      (message "Resuming cache status...done"))))

(defun wl-summary-delete-messages-on-buffer (msgs)
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (msgs2 msgs)
	  (len (length msgs))
	  (i 0)
	  update-list)
      (elmo-kill-buffer wl-summary-search-buf-name)
      (while msgs
	(if (eq wl-summary-buffer-view 'thread)
	    (progn
	      ;; don't use wl-append(nconc), because list is broken. ...why?
	      (setq update-list
		    (append update-list
			    (wl-thread-delete-message (car msgs))))
	      (setq update-list (delq (car msgs) update-list)))
	  (goto-char (point-min))
	  (if (wl-summary-jump-to-msg (car msgs))
	      (progn
		(delete-region (point-at-bol) (point-at-eol))
		(delete-char 1) ; delete '\n'
		(setq wl-summary-buffer-number-list
		      (delq (car msgs) wl-summary-buffer-number-list)))))
	(setq msgs (cdr msgs)))
      (when (eq wl-summary-buffer-view 'thread)
	(let ((updates (elmo-sort-uniq-number-list update-list)))
	  (elmo-with-progress-display (wl-thread-update-line (length updates))
	      "Updating deleted thread"
	    (wl-thread-update-line-msgs updates)
	    (wl-thread-cleanup-symbols msgs2))))
      (wl-summary-count-unread)
      (wl-summary-update-modeline)
      (wl-summary-folder-info-update))))

(defun wl-summary-update-status-marks (beg end &optional check)
  "Synchronize status marks on current buffer to the msgdb."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end) (not (eobp)))
      (when (or (not check)
		(wl-summary-persistent-mark-invalid-p))
	(wl-summary-update-persistent-mark))
      (forward-line))))

(defun wl-summary-update-mark-window (&optional win beg)
  "Update persistent mark in visible summary window.
This function is defined for `window-scroll-functions'"
  (with-current-buffer (window-buffer win)
    (when (eq major-mode 'wl-summary-mode)
      (let ((start (window-start win))
	    (end (condition-case nil
		     (window-end win t)	; old emacsen doesn't support 2nd arg.
		   (error (window-end win)))))
	(wl-summary-update-status-marks start end 'check)))))

(defun wl-summary-insert-message (&rest args)
  (if (eq wl-summary-buffer-view 'thread)
      (apply 'wl-summary-insert-thread args)
    (apply 'wl-summary-insert-sequential args)))

(defun wl-summary-sort (reverse)
  "Sort summary lines into the selected order; argument means descending order."
  (interactive "P")
  (let ((default-value (symbol-name wl-summary-default-sort-spec)))
    (wl-summary-rescan
     (wl-completing-read-multiple
      (format "%s by (%s): " (if reverse "Reverse sort" "Sort") default-value)
      (nconc
       (mapcar (lambda (spec) (list (symbol-name spec)))
	       wl-summary-sort-specs)
       (mapcar (lambda (spec) (list (concat "!" (symbol-name spec))))
	       wl-summary-sort-specs))
      nil t nil nil
      default-value)
     reverse)))

(defun wl-summary-get-available-flags (&optional include-specials)
  (let ((flags (elmo-uniq-list
		(append elmo-global-flags
			(copy-sequence elmo-preserved-flags))
		#'delq)))
    (if include-specials
	flags
      (delq 'new (delq 'cached flags)))))

(defun wl-summary-sync-marks ()
  "Update persistent marks in summary."
  (interactive)
  (let ((mes "Updated ")
	diff diffs)
    (message "Updating marks...")
    (dolist (flag (wl-summary-get-available-flags))
      (setq diff (elmo-list-diff (elmo-folder-list-flagged
				  wl-summary-buffer-elmo-folder
				  flag)
				 (elmo-folder-list-flagged
				  wl-summary-buffer-elmo-folder
				  flag 'in-msgdb)))
      (setq diffs (cadr diff))
      (setq mes (concat mes (format "-%d" (length diffs))))
      (when diffs
	(wl-summary-unset-persistent-mark flag diffs 'no-modeline 'no-server))
      (setq diffs (car diff)
	    mes (concat mes (format "/+%d %s " (length diffs) flag)))
      (when diffs
	(wl-summary-set-persistent-mark flag diffs 'no-modeline 'no-server)))
    (if (interactive-p) (message "%s" mes))))

(defun wl-summary-sync-update (&optional unset-cursor
					 disable-killed
					 sync-all
					 no-check)
  "Update the summary view to the newest folder status."
  (interactive)
  (let* ((folder wl-summary-buffer-elmo-folder)
	 (elmo-mime-charset wl-summary-buffer-mime-charset)
	 (inhibit-read-only t)
	 (buffer-read-only nil)
	 gc-message			; for XEmacs
	 crossed expunged mes)
    (unwind-protect
	(progn
	  (unless wl-summary-buffer-elmo-folder
	    (error "(Internal error) Folder is not set:%s" (buffer-name
							    (current-buffer))))
	  ;; Flush pending append operations (disconnected operation).
;;;	  (setq seen-list
;;;		(wl-summary-flush-pending-append-operations seen-list))
	  (goto-char (point-max))
	  (wl-folder-confirm-existence folder (elmo-folder-plugged-p folder))
	  (setq crossed (elmo-folder-synchronize folder
						 disable-killed
						 sync-all
						 no-check))
	  (if crossed
	      (let ((wl-summary-highlight
		     (and wl-summary-highlight
			  (not wl-summary-lazy-highlight)))
		    append-list delete-list
		    update-thread update-top-list
		    num diff entity)
		;; Setup sync-all
		(if sync-all (wl-summary-sync-all-init))
		(setq diff (elmo-list-diff (elmo-folder-list-messages
					    folder
					    (not disable-killed)
					    'in-msgdb)
					   wl-summary-buffer-number-list))
		(setq append-list (car diff))
		(setq delete-list (cadr diff))

		(when delete-list
		  (wl-summary-delete-messages-on-buffer delete-list))
		(unless wl-summary-lazy-update-mark
		  (wl-summary-update-status-marks (point-min) (point-max)))
		(when (and wl-summary-lazy-highlight
			   wl-summary-lazy-update-mark)
		  (let (buffer-read-only)
		    (put-text-property (point-min) (point-max) 'face nil)))
		(setq num (length append-list))
		(setq wl-summary-delayed-update nil)
		(elmo-kill-buffer wl-summary-search-buf-name)
		(elmo-with-progress-display (wl-summary-insert-line num)
		    (if (eq wl-summary-buffer-view 'thread)
			"Making thread"
		      "Inserting message")
		  (dolist (number append-list)
		    (setq entity (elmo-message-entity folder number))
		    (when (setq update-thread
				(wl-summary-insert-message
				 entity folder
				 (not sync-all)))
		      (wl-append update-top-list update-thread))
		    (if elmo-use-database
			(elmo-database-msgid-put
			 (elmo-message-entity-field entity 'message-id)
			 (elmo-folder-name-internal folder)
			 (elmo-message-entity-number entity))))
		  (while wl-summary-delayed-update
		    (message "Parent (%d) of message %d is no entity"
			     (caar wl-summary-delayed-update)
			     (elmo-message-entity-number
			      (cdar wl-summary-delayed-update)))
		    (when (setq update-thread
				(wl-summary-insert-message
				 (cdar wl-summary-delayed-update)
				 wl-summary-buffer-elmo-folder
				 (not sync-all) t))
		      (wl-append update-top-list update-thread))
		    (setq wl-summary-delayed-update
			  (cdr wl-summary-delayed-update))))
		(when (and (eq wl-summary-buffer-view 'thread)
			   update-top-list)
		  (wl-thread-update-indent-string-thread
		   (elmo-sort-uniq-number-list update-top-list)))
		(when (or delete-list append-list)
		  (wl-summary-set-message-modified))
		(when (and sync-all (eq wl-summary-buffer-view 'thread))
		  (elmo-kill-buffer wl-summary-search-buf-name)
		  (wl-thread-insert-top))
		(if elmo-use-database
		    (elmo-database-close))
		(run-hooks 'wl-summary-sync-updated-hook)
		(setq mes
		      (if (and (zerop (length delete-list))
			       (zerop num))
			  (format
			   "No updates for \"%s\"" (elmo-folder-name-internal
						    folder))
			(format "Updated (-%d/+%d) message(s)"
				(length delete-list) num))))
	    (setq mes "Quit updating")))
      ;; synchronize marks.
      (if (and crossed wl-summary-auto-sync-marks)
	  (wl-summary-sync-marks))
      ;; scoring
      (when wl-use-scoring
	(setq wl-summary-scored nil)
	(wl-summary-score-headers (and sync-all
				       (wl-summary-rescore-msgs
					wl-summary-buffer-number-list))
				  sync-all)
	(when (and wl-summary-scored
		   (setq expunged (wl-summary-score-update-all-lines)))
	  (setq mes (concat mes
			    (format " (%d expunged)"
				    (length expunged))))))
      (if (and crossed (> crossed 0))
	  (setq mes
		(if mes
		    (concat mes
			    (format " (%d crosspost)" crossed))
		  (format "%d crosspost message(s)" crossed)))
	(and mes (setq mes (concat mes "."))))
      ;; Update Folder mode
      (wl-folder-set-folder-updated
       (elmo-folder-name-internal folder)
       (list 0
	     (or (cdr (assq 'unread (wl-summary-count-unread))) 0)
	     (elmo-folder-length folder)))
      (wl-summary-update-modeline)
      ;;
      (unless unset-cursor
	(goto-char (point-min))
	(if (not (wl-summary-cursor-down t))
	    (progn
	      (goto-char (point-max))
	      (forward-line -1))
	  (when (and wl-summary-highlight
		     (not wl-summary-lazy-highlight)
		     (not (get-text-property (point) 'face)))
	    (save-excursion
	      (forward-line (- (or
				wl-summary-partial-highlight-above-lines
				wl-summary-highlight-partial-threshold)))
	      (wl-highlight-summary (point) (point-max))))))
      (wl-delete-all-overlays)
      (run-hooks 'wl-summary-buffer-window-scroll-functions)
      (set-buffer-modified-p nil)
      (if mes (message "%s" mes)))))

(defun wl-summary-set-score-mark (mark)
  (save-excursion
    (beginning-of-line)
    (let ((cur-mark (wl-summary-temp-mark)))
      (when (member cur-mark (list " "
				   wl-summary-score-below-mark
				   wl-summary-score-over-mark))
	(wl-summary-put-temp-mark mark)
	(if wl-summary-highlight
	    (wl-highlight-summary-current-line))
	(set-buffer-modified-p nil)))))

(defun wl-summary-get-score-mark (msg-num)
  (let ((score (cdr (assq msg-num wl-summary-scored))))
    (if score
	(cond ((< score wl-summary-default-score)
	       "-")
	      ((> score wl-summary-default-score)
	       "+")))))

(defun wl-summary-update-modeline ()
  (setq wl-summary-buffer-mode-line
	(funcall wl-summary-buffer-mode-line-formatter)))

(defun wl-summary-jump-to-msg (&optional number beg end)
  (interactive "NJump to Message (No.): ")
  (when number
    (let ((pos (point))
	  regexp)
      (setq regexp (concat "\r" (number-to-string number) "[^0-9]"))
      (if (and beg end (or (< pos beg) (< end pos)))
	  (progn
	    (goto-char beg)
	    (if (re-search-forward regexp end t)
		(progn (backward-char) (beginning-of-line) t)
	      (goto-char pos)
	      nil))
	(beginning-of-line)
	(if (or (and (re-search-forward regexp end t)
		     (progn (backward-char) t))
		(re-search-backward regexp beg t))
	    (progn (beginning-of-line) t)
	  nil)))))

(defun wl-summary-highlight-msgs (msgs)
  (save-excursion
    (elmo-with-progress-display (wl-summary-highlight-line (length msgs))
	"Hilighting"
      (while msgs
	(if (wl-summary-jump-to-msg (car msgs))
	    (wl-highlight-summary-current-line))
	(setq msgs (cdr msgs))
	(elmo-progress-notify 'wl-summary-highlight-line)))))

(defun wl-summary-message-number ()
  (save-excursion
    (beginning-of-line)
    (if (or (re-search-forward "\r\\(-?[0-9]+\\)" (point-at-eol) t)
	    (re-search-forward "^ *\\(-?[0-9]+\\)" (point-at-eol) t))
	(string-to-number (wl-match-buffer 1))
      nil)))

(defun wl-summary-delete-all-msgs ()
  (interactive)
  (let ((cur-buf (current-buffer))
	(dels (elmo-folder-list-messages wl-summary-buffer-elmo-folder)))
    (set-buffer cur-buf)
    (if (null dels)
	(message "No message to delete.")
      (if (y-or-n-p (format "%s has %d message(s).  Delete all? "
			    (wl-summary-buffer-folder-name)
			    (length dels)))
	  (progn
	    (message "Deleting...")
	    (elmo-folder-move-messages wl-summary-buffer-elmo-folder dels
				       'null)
	    (wl-summary-set-message-modified)
	    (wl-folder-set-folder-updated (wl-summary-buffer-folder-name)
					  (list 0 0 0))
;;; for thread.
;;;	    (setq wl-thread-top-entity '(nil t nil nil))
	    (setq wl-summary-buffer-unread-count 0)
	    (setq wl-summary-buffer-new-count    0)
	    (wl-summary-update-modeline)
	    (set-buffer cur-buf)
	    (let ((inhibit-read-only t)
		  (buffer-read-only nil))
	      (erase-buffer))
;;;	    (if wl-summary-cache-use (wl-summary-save-view-cache))
	    (message "Deleting...done")
	    t)
	nil))))

(defun wl-summary-toggle-thread (&optional arg)
  "Toggle thread status (T)hread and (S)equential.
If ARG, without confirm."
  (interactive "P")
  (when (or arg
	    (y-or-n-p (format "Toggle threading? (y=%s): "
			      (if (eq wl-summary-buffer-view 'thread)
				  "\"off\"" "\"on\""))))
    (if (eq wl-summary-buffer-view 'thread)
	(setq wl-summary-buffer-view 'sequence)
      (setq wl-summary-buffer-view 'thread))
    (wl-summary-update-modeline)
    (force-mode-line-update)
    (wl-summary-rescan nil nil nil t)))

(defun wl-summary-load-file-object (filename)
  "Load lisp object from dir."
  (with-temp-buffer
    (let (insert-file-contents-pre-hook	; To avoid autoconv-xmas...
	  insert-file-contents-post-hook
	  ret-val)
      (if (not (file-readable-p filename))
	  ()
	(as-binary-input-file (insert-file-contents filename))
	(condition-case nil
	    (read (current-buffer))
	  (error (error "Reading failed")))))))

(defun wl-summary-goto-folder (&optional arg)
  (interactive "P")
  (wl-summary-goto-folder-subr nil nil nil nil t nil arg))

(defun wl-summary-goto-folder-sticky ()
  (interactive)
  (wl-summary-goto-folder-subr nil nil nil t t))

(defun wl-summary-goto-last-visited-folder ()
  (interactive)
  (let ((entity
	 (wl-folder-search-entity-by-name wl-summary-last-visited-folder
					  wl-folder-entity
					  'folder)))
    (if entity (wl-folder-set-current-entity-id
		(wl-folder-get-entity-id entity))))
  (wl-summary-goto-folder-subr wl-summary-last-visited-folder nil nil nil t))

(defun wl-summary-sticky-p (&optional folder)
  (if folder
      (get-buffer (wl-summary-sticky-buffer-name
		   (elmo-folder-name-internal folder)))
    (not (string= wl-summary-buffer-name (buffer-name)))))

(defun wl-summary-always-sticky-folder-p (folder)
  (or (eq t wl-summary-always-sticky-folder-list)
      (wl-string-match-member
       (elmo-folder-name-internal folder)
       wl-summary-always-sticky-folder-list)))

(defun wl-summary-stick (&optional force)
  "Make current summary buffer sticky."
  (interactive "P")
  (if (wl-summary-sticky-p)
      (message "Current summary buffer is already sticky.")
    (when (or force (y-or-n-p "Stick current summary buffer? "))
      (wl-summary-toggle-disp-msg 'off)
      (wl-summary-switch-to-clone-buffer
       (wl-summary-sticky-buffer-name
	(wl-summary-buffer-folder-name)))
;;; ???hang up
;;;      (rename-buffer (wl-summary-sticky-buffer-name
;;;		      (wl-summary-buffer-folder-name))))
      (message "Folder `%s' is now sticky." (wl-summary-buffer-folder-name)))))

(defun wl-summary-switch-to-clone-buffer (buffer-name)
  (let ((cur-buf (current-buffer))
	(msg (wl-summary-message-number))
	(buf (get-buffer-create buffer-name))
	(folder wl-summary-buffer-elmo-folder)
	(copy-variables
	 (append '(wl-summary-buffer-view
		   wl-summary-buffer-temp-mark-list
		   wl-summary-buffer-target-mark-list
		   wl-summary-buffer-elmo-folder
		   wl-summary-buffer-number-column
		   wl-summary-buffer-temp-mark-column
		   wl-summary-buffer-persistent-mark-column
		   wl-summary-buffer-message-modified
		   wl-summary-buffer-thread-modified
		   wl-summary-buffer-number-list
		   wl-summary-buffer-persistent-mark-version
		   wl-summary-buffer-folder-name
		   wl-summary-buffer-line-formatter)
		 (and (eq wl-summary-buffer-view 'thread)
		      '(wl-thread-entity-hashtb
			wl-thread-entities
			wl-thread-entity-list))
		 (and wl-use-scoring
		      '(wl-summary-scored
			wl-summary-default-score
			wl-summary-important-above
			wl-summary-target-above
			wl-summary-mark-below
			wl-summary-expunge-below))
		 (and (featurep 'wl-score)
		      '(wl-current-score-file
			wl-score-alist)))))
    (set-buffer buf)
    (wl-summary-mode)
    (wl-summary-buffer-set-folder folder)
    (let ((buffer-read-only nil))
      (insert-buffer-substring cur-buf))
    (set-buffer-modified-p nil)
    (while copy-variables
      (set (car copy-variables)
	   (with-current-buffer cur-buf
	     (symbol-value (car copy-variables))))
      (setq copy-variables (cdr copy-variables)))
    (switch-to-buffer buf)
    (kill-buffer cur-buf)
    (wl-summary-count-unread)
    (wl-summary-update-modeline)
    (if msg
	(if (eq wl-summary-buffer-view 'thread)
	    (wl-thread-jump-to-msg msg)
	  (wl-summary-jump-to-msg msg))
      (goto-char (point-max))
      (beginning-of-line))))

(defun wl-summary-get-buffer (folder)
  (and folder
       (or (get-buffer (wl-summary-sticky-buffer-name folder))
	   (let ((buffer (get-buffer wl-summary-buffer-name)))
	     (and buffer
		  (with-current-buffer buffer
		    (string= (wl-summary-buffer-folder-name) folder))
		  buffer)))))

(defun wl-summary-get-buffer-create (name &optional force-sticky)
  (if force-sticky
      (get-buffer-create
       (wl-summary-sticky-buffer-name name))
    (or (get-buffer (wl-summary-sticky-buffer-name name))
	(get-buffer-create wl-summary-buffer-name))))

(defun wl-summary-make-number-list ()
  (save-excursion
    (goto-char (point-min))
    (setq wl-summary-buffer-number-list nil)
    (while (not (eobp))
      (setq wl-summary-buffer-number-list
	    (cons (wl-summary-message-number)
		  wl-summary-buffer-number-list))
      (forward-line))
    (setq wl-summary-buffer-number-list
	  (nreverse wl-summary-buffer-number-list))))

(defun wl-summary-auto-select-msg-p (unread-msg)
  (and unread-msg
       (not (elmo-message-has-global-flag-p
	     wl-summary-buffer-elmo-folder unread-msg))))

(defsubst wl-summary-open-folder (folder)
  ;; Select folder
  (let ((elmo-mime-charset wl-summary-buffer-mime-charset))
    (unwind-protect
	(elmo-folder-open folder 'load-msgdb)
      ;; For compatibility
      (setq wl-summary-buffer-folder-name (elmo-folder-name-internal
					   folder)))))

(defun wl-summary-goto-folder-subr (&optional name scan-type other-window
					      sticky interactive scoring
					      force-exit)
  "Display target folder on summary."
  (interactive)
  (let* ((keep-cursor (memq this-command
			    wl-summary-keep-cursor-command))
	 (name (or name (wl-summary-read-folder wl-default-folder)))
	 (cur-fld wl-summary-buffer-elmo-folder)
	 folder buf mes hilit reuse-buf
	 retval entity)
    (if (string= name "")
	(setq name wl-default-folder))
    (setq folder (wl-folder-get-elmo-folder name))
    (when (and (not (string=
		     (and cur-fld (elmo-folder-name-internal cur-fld))
		     (elmo-folder-name-internal folder))) ; folder is moved.
	       (eq major-mode 'wl-summary-mode)) ; called in summary.
      (setq wl-summary-last-visited-folder (wl-summary-buffer-folder-name))
      (run-hooks 'wl-summary-exit-pre-hook)
      (let ((discard-contents (or force-exit (not (wl-summary-sticky-p)))))
	(when discard-contents
	  (wl-summary-cleanup-temp-marks))
	(wl-summary-save-view)
	(if discard-contents
	    (elmo-folder-close wl-summary-buffer-elmo-folder)
	  (elmo-folder-commit wl-summary-buffer-elmo-folder)))
      (if (and (wl-summary-sticky-p) force-exit)
	  (kill-buffer (current-buffer))))
    (setq buf (wl-summary-get-buffer-create (elmo-folder-name-internal folder)
					    sticky))
    (setq reuse-buf
	  (with-current-buffer buf
	    (string= (elmo-folder-name-internal folder)
		     (wl-summary-buffer-folder-name))))
    (unwind-protect
	(if reuse-buf
	    (if interactive
		(switch-to-buffer buf)
	      (set-buffer buf))
	  (if other-window
	      (delete-other-windows))
	  (set-buffer buf)
	  (unless (eq major-mode 'wl-summary-mode)
	    (wl-summary-mode))
	  (wl-summary-buffer-set-folder folder)
	  (setq wl-summary-buffer-display-mime-mode
		(if (wl-summary-no-mime-p wl-summary-buffer-elmo-folder)
		    'as-is
		  'mime))
	  (setq wl-summary-buffer-disp-msg nil)
	  (setq wl-summary-buffer-message-ring nil)
	  (setq wl-summary-buffer-current-msg nil)
	  (setq wl-summary-buffer-persistent-mark-version 0)
	  (let ((inhibit-read-only t)
		(buffer-read-only nil))
	    (erase-buffer)
	    ;; Resume summary view
	    (if wl-summary-cache-use
		(let* ((dir (elmo-folder-msgdb-path folder))
		       (cache (expand-file-name wl-summary-cache-file dir))
		       (view (expand-file-name wl-summary-view-file dir)))
		  (when (file-exists-p cache)
		    (insert-file-contents-as-binary cache)
		    (set-buffer-multibyte
		     default-enable-multibyte-characters)
		    (decode-mime-charset-region
		     (point-min)(point-max)
		     wl-summary-buffer-mime-charset 'LF))
		  (if (file-exists-p view)
		      (setq wl-summary-buffer-view
			    (wl-summary-load-file-object view))
		    (setq wl-summary-buffer-view
			  (or (wl-get-assoc-list-value
			       wl-summary-default-view-alist
			       (elmo-folder-name-internal folder))
			      wl-summary-default-view)))
		  (wl-thread-resume-entity folder)
		  (wl-summary-open-folder folder)
		  (wl-summary-detect-mark-position))
	      (setq wl-summary-buffer-view
		    (wl-summary-load-file-object
		     (expand-file-name wl-summary-view-file
				       (elmo-folder-msgdb-path folder))))
	      (wl-summary-open-folder folder)
	      (wl-summary-detect-mark-position)
	      (wl-summary-rescan))
	    (wl-summary-count-unread)
	    (wl-summary-update-modeline)))
      (unless (eq wl-summary-buffer-view 'thread)
	(wl-summary-make-number-list))
      (when (and wl-summary-cache-use
		 (or (and wl-summary-check-line-format
			  (wl-summary-line-format-changed-p))
		     (wl-summary-view-old-p)))
	(wl-summary-rescan))
      (wl-summary-toggle-disp-msg (if wl-summary-buffer-disp-msg 'on 'off))
      (unless (and reuse-buf keep-cursor)
	(unwind-protect
	    (let ((wl-use-scoring
		   (if (or scoring interactive) wl-use-scoring)))
	      (if (and (not scan-type)
		       interactive
		       (not wl-ask-range))
		  (setq scan-type (wl-summary-get-sync-range folder)))
	      (cond
	       ((eq scan-type nil)
		(wl-summary-sync 'unset-cursor))
	       ((eq scan-type 'all)
		(wl-summary-sync 'unset-cursor "all"))
	       ((eq scan-type 'no-sync))
	       ((eq scan-type 'rescan)
		(wl-summary-rescan))
	       ((or (eq scan-type 'force-update)
		    (eq scan-type 'update))
		(setq mes (wl-summary-sync-force-update
			   'unset-cursor)))))
	  (if interactive
	      (switch-to-buffer buf)
	    (set-buffer buf))
	  ;; stick always-sticky-folder
	  (when (wl-summary-always-sticky-folder-p folder)
	    (or (wl-summary-sticky-p) (wl-summary-stick t)))
	  (run-hooks 'wl-summary-prepared-pre-hook)
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (if (wl-summary-cursor-down t)
	      (let ((unreadp (wl-summary-next-message
			      (wl-summary-message-number)
			      'down t)))
		(cond ((and wl-auto-select-first
			    (wl-summary-auto-select-msg-p unreadp))
		       ;; wl-auto-select-first is non-nil and
		       ;; unreadp is non-nil but not flagged
		       (setq retval 'disp-msg))
		      ((and wl-auto-prefetch-first
			    (wl-summary-auto-select-msg-p unreadp))
		       ;; wl-auto-select-first is non-nil and
		       ;; unreadp is non-nil but not flagged
		       (setq retval 'prefetch-msg))
		      ((not (wl-summary-auto-select-msg-p unreadp))
		       ;; unreadp is nil or flagged
		       (setq retval 'more-next))))
	    (goto-char (point-max))
	    (if (elmo-folder-plugged-p folder)
		(forward-line -1)
	      (wl-summary-prev))
	    (setq retval 'more-next))
	  (if (and wl-summary-highlight
		   (not wl-summary-lazy-highlight)
		   (not reuse-buf))
	      (if (and wl-summary-highlight-partial-threshold
		       (> (count-lines (point-min) (point-max))
			  wl-summary-highlight-partial-threshold))
		  (save-excursion
		    (forward-line (-
				   0
				   (or
				    wl-summary-partial-highlight-above-lines
				    wl-summary-highlight-partial-threshold)))
		    (wl-highlight-summary (point) (point-max)))
		(wl-highlight-summary (point-min) (point-max))))
	  (if (eq retval 'disp-msg)
	      (wl-summary-redisplay))
	  (if (eq retval 'prefetch-msg)
	      (wl-message-buffer-prefetch
	       folder
	       (wl-summary-message-number)
	       (min (or wl-message-buffer-prefetch-depth 0)
		    (1- wl-message-buffer-cache-size))
	       (current-buffer)
	       wl-summary-buffer-mime-charset))
	  (if mes (message "%s" mes))
	  (if (and interactive wl-summary-recenter)
	      (recenter (/ (- (window-height) 2) 2))))))
    ;; set current entity-id
    (when (and folder
	       (setq entity
		     (wl-folder-search-entity-by-name
		      (elmo-folder-name-internal folder)
		      wl-folder-entity
		      'folder)))
      ;; entity-id is unknown.
      (wl-folder-set-current-entity-id
       (wl-folder-get-entity-id entity)))
    (when (and wl-summary-buffer-window-scroll-functions
	       wl-on-xemacs)
      (sit-for 0))
    (when (or (eq t wl-summary-force-prefetch-folder-list)
	      (wl-string-match-member
	       (elmo-folder-name-internal wl-summary-buffer-elmo-folder)
	       wl-summary-force-prefetch-folder-list))
      (wl-summary-force-prefetch))
    (unwind-protect
	(run-hooks 'wl-summary-prepared-hook)
      (set-buffer-modified-p nil))
    retval))

(defun wl-summary-goto-previous-message-beginning ()
  (end-of-line)
  (re-search-backward "\r\\(-?[0-9]+\\)" nil t)
  (beginning-of-line))

(defun wl-summary-goto-top-of-current-thread ()
  (wl-summary-jump-to-msg
   (wl-thread-entity-get-number
    (wl-thread-entity-get-top-entity (wl-thread-get-entity
				      (wl-summary-message-number))))))

(defun wl-summary-goto-bottom-of-sub-thread (&optional depth)
  (interactive)
  (let ((depth (or depth
		   (wl-thread-get-depth-of-current-line))))
    (forward-line)
    (while (and (not (eobp))
		(>= (wl-thread-get-depth-of-current-line)
		    depth))
      (forward-line))
    (beginning-of-line)))

(defun wl-summary-insert-line (line)
  "Insert LINE in the Summary."
  (if wl-use-highlight-mouse-line
      ;; remove 'mouse-face of current line.
      (put-text-property
       (point-at-bol) (point-at-eol)
       'mouse-face nil))
  (insert line "\n")
  (save-excursion
    (forward-line -1)
    (let* ((number (wl-summary-message-number))
	   (mark-info (wl-summary-registered-temp-mark number)))
      (when (and mark-info (nth 2 mark-info))
	(wl-summary-print-argument number (nth 2 mark-info)))))
  (if wl-use-highlight-mouse-line
      ;; remove 'mouse-face of current line.
      (put-text-property
       (point-at-bol) (point-at-eol)
       'mouse-face nil))
  (elmo-progress-notify 'wl-summary-insert-line)
  (ignore-errors
    (run-hooks 'wl-summary-line-inserted-hook)))

(defun wl-summary-insert-sequential (entity folder &rest args)
  (when entity
    (let ((inhibit-read-only t)
	  (number (elmo-message-entity-number entity))
	  buffer-read-only)
      (goto-char (point-max))
      (wl-summary-insert-line
       (wl-summary-create-line entity nil nil
			       (elmo-message-status folder number)))
      (setq wl-summary-buffer-number-list
	    (wl-append wl-summary-buffer-number-list
		       (list (elmo-message-entity-number entity))))
      nil)))

(defun wl-summary-default-subject-filter (subject)
  (setq subject (elmo-replace-in-string
		 subject "\\(\\(re\\|was\\)[:>]\\|[ \t]+\\)+" ""))
  (if (string-match "^\\[[^]]*\\]" subject)
      (substring subject (match-end 0))
    subject))

(defun wl-summary-subject-equal (subject1 subject2)
  (string= (funcall wl-summary-subject-filter-function subject1)
	   (funcall wl-summary-subject-filter-function subject2)))

(defmacro wl-summary-put-alike (alike count)
  `(elmo-set-hash-val (format "#%d" ,count)
		      ,alike
		      wl-summary-alike-hashtb))

(defsubst wl-summary-get-alike ()
  (elmo-get-hash-val (format "#%d" (wl-count-lines))
		     wl-summary-alike-hashtb))

(defun wl-summary-insert-headers (folder func &optional mime-decode)
  (let ((numbers (elmo-folder-list-messages folder 'visible t))
	(count (wl-count-lines))
	ov this last alike)
    (buffer-disable-undo (current-buffer))
    (make-local-variable 'wl-summary-alike-hashtb)
    (setq wl-summary-alike-hashtb (elmo-make-hash (* (length numbers) 2)))
    (when mime-decode
      (set-buffer-multibyte default-enable-multibyte-characters))
    (mapc (lambda (number)
	    (setq ov (elmo-message-entity folder number))
	    (setq this (funcall func ov))
	    (if (equal last this)
		(setq alike (cons ov alike))
	      (when last
		(wl-summary-put-alike alike count)
		(insert last ?\n)
		(setq count (1+ count)))
	      (setq alike (list ov)
		    last this)))
	  numbers)
    (when (null (eq last this))
      (wl-summary-put-alike alike count)
      (insert last ?\n))
    (when mime-decode
      (decode-mime-charset-region (point-min) (point-max)
				  elmo-mime-charset)
      (when (eq mime-decode 'mime)
	(eword-decode-region (point-min) (point-max))))
    (run-hooks 'wl-summary-insert-headers-hook)))

(defun wl-summary-search-by-subject (entity folder)
  (let ((summary-buf (current-buffer))
	(buf (get-buffer-create wl-summary-search-buf-name))
	(folder-name (wl-summary-buffer-folder-name))
	match founds result)
    (with-current-buffer buf
      (let ((case-fold-search t))
	(when (or (not (string= wl-summary-search-buf-folder-name folder-name))
		  (zerop (buffer-size)))
	  (setq wl-summary-search-buf-folder-name folder-name)
	  (message "Creating subject cache...")
	  (wl-summary-insert-headers
	   folder
	   (lambda (x)
	     (funcall wl-summary-subject-filter-function
		      (elmo-message-entity-field x 'subject))))
	  (message "Creating subject cache...done"))
	(setq match (funcall wl-summary-subject-filter-function
			     (elmo-message-entity-field entity 'subject)))
	(if (string= match "")
	    (setq match "\n"))
	(goto-char (point-max))
	(while (and (null result)
		    (not (= (point) (point-min)))
		    (search-backward match nil t))
	  ;; check exactly match
	  (when (and (bolp) (= (point-at-eol)(match-end 0)))
	    (setq founds (wl-summary-get-alike))
	    (with-current-buffer summary-buf
	      (while founds
		(when (and
		       ;; the first element of found-entity list exists on
		       ;; thread tree.
		       (wl-thread-get-entity
			(elmo-message-entity-number (car founds)))
		       ;; message id is not same as myself.
		       (not (string=
			     (elmo-message-entity-field entity 'message-id)
			     (elmo-message-entity-field (car founds)
							'message-id)))
		       ;; not a descendant.
		       (not (wl-thread-descendant-p
			     (elmo-message-entity-number entity)
			     (elmo-message-entity-number (car founds)))))
		  (setq result (car founds)
			founds nil))
		(setq founds (cdr founds))))))
	result))))

(defun wl-summary-insert-thread (entity folder update
					&optional force-insert)
  (let ((depth 0)
	this-id	parent-entity parent-number
	number cur-entity linked retval delayed-entity
	update-list entity-stack thread-entity)
    (while entity
      (setq this-id (elmo-message-entity-field entity 'message-id)
	    number (elmo-message-entity-number entity))
      (if (and wl-thread-saved-entity-hashtb-internal
	       (setq thread-entity
		     (elmo-get-hash-val
		      (format "#%d" (elmo-message-entity-number entity))
		      wl-thread-saved-entity-hashtb-internal)))
	  (setq parent-entity
		(elmo-message-entity
		 folder
		 (wl-thread-entity-get-parent thread-entity))
		linked (wl-thread-entity-get-linked thread-entity))
	(setq parent-entity (elmo-message-entity-parent folder entity)
	      linked nil))
      (setq parent-number (and parent-entity
			       (elmo-message-entity-number parent-entity)))
      ;; If thread loop detected, set parent as nil.
      (let ((cur entity)
	    anumber relatives)
	(while cur
	  (when (setq anumber
		      (elmo-message-entity-number
		       (setq cur (elmo-message-entity-parent folder cur))))
	    (if (memq anumber relatives)
		(setq parent-number nil
		      cur nil))
	    (setq relatives (cons anumber relatives)))))
      (if (and parent-number
	       (not (wl-thread-get-entity parent-number))
	       (not force-insert))
	  ;; parent exists in overview, but not in wl-thread-entities
	  (progn
	    (wl-append wl-summary-delayed-update
		       (list (cons parent-number entity)))
	    (setq entity nil)) ;; exit loop
	;; Search parent by subject.
	(when (and (null parent-number)
		   wl-summary-search-parent-by-subject-regexp
		   (string-match
		    wl-summary-search-parent-by-subject-regexp
		    (elmo-message-entity-field entity 'subject)))
	  (let ((found (wl-summary-search-by-subject entity folder)))
	    (when (and found
		       (not (member found wl-summary-delayed-update)))
	      (setq parent-entity found)
	      (setq parent-number
		    (elmo-message-entity-number parent-entity))
	      (setq linked t))))
	;; If subject is change, divide thread.
	(if (and parent-number
		 wl-summary-divide-thread-when-subject-changed
		 (not (wl-summary-subject-equal
		       (or (elmo-message-entity-field entity 'subject) "")
		       (or (elmo-message-entity-field parent-entity
						      'subject) ""))))
	    (setq parent-number nil))
	(when (setq retval (wl-thread-insert-message
			    entity number parent-number update linked))
	  (wl-append update-list (list retval)))
	(elmo-progress-notify 'wl-summary-insert-line)
	(setq entity nil) ; exit loop
	(while (setq delayed-entity (assq number wl-summary-delayed-update))
	  (setq wl-summary-delayed-update
		(delq delayed-entity wl-summary-delayed-update))
	  ;; update delayed message
	  (wl-append entity-stack (list (cdr delayed-entity)))))
      (if (and (not entity)
	       entity-stack)
	  (setq entity (pop entity-stack))))
    update-list))

(defun wl-summary-update-thread (entity
				 thr-entity
				 parent-entity)
  (let* ((this-id (elmo-message-entity-field entity 'message-id))
	 (overview-entity entity)
	 (parent-id (elmo-message-entity-field parent-entity 'message-id))
	 (number (elmo-message-entity-number entity))
	 (parent-number (elmo-message-entity-number parent-entity))
	 insert-line)
    (cond
     ((or (not parent-id)
	  (string= this-id parent-id))
      (goto-char (point-max))
      (beginning-of-line)
      (setq insert-line t))
     ;; parent already exists in buffer.
     ((wl-summary-jump-to-msg parent-number)
      (wl-thread-goto-bottom-of-sub-thread)
      (setq insert-line t)))
    (when insert-line
      (let (buffer-read-only)
	(wl-summary-insert-line
	 (wl-summary-create-line
	  entity
	  parent-entity
	  nil
	  (wl-summary-message-status number)
	  (wl-thread-maybe-get-children-num number)
	  (wl-thread-make-indent-string thr-entity)
	  (wl-thread-entity-get-linked thr-entity)))))))

(defun wl-summary-target-mark-msgs (msgs)
  "Return the number of marked messages."
  (let ((i 0))
    (dolist (number msgs)
      (when (wl-summary-target-mark number)
	(setq i (1+ i))))
    i))

(defun wl-summary-pick (&optional from-list delete-marks)
  (interactive "i\nP")
  (save-excursion
    (let* ((condition (car (elmo-parse-search-condition
			    (wl-read-search-condition
			     wl-summary-pick-field-default))))
	   (result (elmo-folder-search wl-summary-buffer-elmo-folder
				       condition
				       (or from-list t)))
	   num)
      (if delete-marks
	  (let ((mlist wl-summary-buffer-target-mark-list))
	    (while mlist
	      (when (wl-summary-jump-to-msg (car mlist))
		(wl-summary-unmark))
	      (setq mlist (cdr mlist)))
	    (setq wl-summary-buffer-target-mark-list nil)))
      (if (and result
	       (setq num (wl-summary-target-mark-msgs result))
	       (> num 0))
	  (if (= num (length result))
	      (message "%d message(s) are picked." num)
	    (message "%d(%d) message(s) are picked." num
		     (- (length result) num)))
	(message "No message was picked.")))))

(defun wl-summary-unvirtual ()
  "Exit from current virtual folder."
  (interactive)
  (if (eq 'filter
	  (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
      (wl-summary-goto-folder-subr
       (elmo-folder-name-internal
	(elmo-filter-folder-target-internal
	 wl-summary-buffer-elmo-folder))
       'update nil nil t)
    (error "This folder is not filtered")))

(defun wl-summary-virtual (&optional arg)
  "Goto virtual folder.
If ARG, exit virtual folder."
  (interactive "P")
  (if arg
      (wl-summary-unvirtual)
    (wl-summary-goto-folder-subr (concat "/"
					 (wl-read-search-condition
					  wl-summary-pick-field-default)
					 "/"
					 (wl-summary-buffer-folder-name))
				 'update nil nil t)
    (run-hooks 'wl-summary-virtual-hook)))

(defun wl-summary-delete-all-temp-marks (&optional no-msg force)
  "Erase all temp marks from buffer."
  (interactive)
  (when (or wl-summary-buffer-target-mark-list
	    wl-summary-buffer-temp-mark-list
	    wl-summary-scored)
    (save-excursion
      (goto-char (point-min))
      (unless no-msg
	(message "Unmarking..."))
      (while (not (eobp))
	(wl-summary-unset-mark nil nil force)
	(forward-line))
      (unless no-msg
	(message "Unmarking...done"))
      (setq wl-summary-buffer-target-mark-list nil)
      (setq wl-summary-buffer-temp-mark-list nil))))

(defsubst wl-summary-temp-mark (&optional number)
  "Return temp-mark string of current line."
  (let ((number (or number (wl-summary-message-number)))
	info)
    (or (and (wl-summary-have-target-mark-p number)
	     "*")
	(and (setq info (wl-summary-registered-temp-mark number))
	     (nth 1 info))
	(wl-summary-get-score-mark number)
	" ")))

(defun wl-summary-persistent-mark-invalid-p ()
  (not
   (equal
    ;; mey be nil.
    (get-text-property (point) 'wl-summary-persistent-mark-version)
    wl-summary-buffer-persistent-mark-version)))

(defun wl-summary-validate-persistent-mark (beg end)
  (let ((inhibit-read-only t)
	(buffer-read-only nil))
    (put-text-property beg end
		       'wl-summary-persistent-mark-version
		       wl-summary-buffer-persistent-mark-version)
    (set-buffer-modified-p nil)))

(defun wl-summary-validate-persistent-mark-string (string)
  (put-text-property 0 (length string)
		     'wl-summary-persistent-mark-version
		     wl-summary-buffer-persistent-mark-version
		     string))

(defun wl-summary-invalidate-persistent-mark ()
  (setq wl-summary-buffer-persistent-mark-version
	(1+ wl-summary-buffer-persistent-mark-version)))

(defsubst wl-summary-persistent-mark-string (folder status)
  "Return the persistent mark string.
The mark is decided according to the FOLDER and STATUS."
  (let ((priorities wl-summary-persistent-mark-priority-list)
	(flags (elmo-message-status-flags status))
	(cached (elmo-message-status-cached-p status))
	mark)
    (while (and (null mark) priorities)
      (let ((flag (car priorities)))
	(cond
	 ((eq flag 'flag)
	  (let ((flags (elmo-get-global-flags flags 'ignore-preserved))
		(specs wl-summary-flag-alist)
		spec)
	    (when flags
	      (while (setq spec (car specs))
		(if (memq (car spec) flags)
		    (setq mark (or (nth 2 spec) wl-summary-flag-mark)
			  specs nil)
		  (setq specs (cdr specs))))
	      (unless mark
		(setq mark wl-summary-flag-mark)))))
	 ((eq flag 'killed)
	  (when (elmo-message-status-killed-p status)
	    (setq mark wl-summary-killed-mark)))
	 ((memq flag flags)
	  (setq mark
		(let ((var (intern-soft
			    (format
			     (if cached
				 "wl-summary-%s-cached-mark"
			       "wl-summary-%s-uncached-mark")
			     flag))))
		  (or (and var (boundp var) (symbol-value var))
		      (funcall (if cached #'downcase #'upcase)
			       (substring (symbol-name flag) 0 1)))))))
	(setq priorities (cdr priorities))))
    (or mark
	(if (or cached (elmo-folder-local-p folder))
	    nil
	  wl-summary-uncached-mark))))

(defsubst wl-summary-message-mark (folder number &optional status)
  "Return mark of the message."
  (ignore-errors
    (wl-summary-persistent-mark-string
     folder
     (or status (elmo-message-status folder number)))))

(defsubst wl-summary-persistent-mark (&optional number status)
  "Return persistent-mark string of current line."
  (or (wl-summary-message-mark wl-summary-buffer-elmo-folder
			       (or number (wl-summary-message-number))
			       status)
      " "))

(defun wl-summary-put-temp-mark (mark)
  "Put temp MARK on current line."
  (when wl-summary-buffer-temp-mark-column
    (save-excursion
      (beginning-of-line)
      (let ((inhibit-read-only t)
	    (buffer-read-only nil))
	(move-to-column wl-summary-buffer-temp-mark-column)
	(delete-char -1)
	(insert mark)))))

(defun wl-summary-next-buffer ()
  "Switch to next summary buffer."
  (interactive)
  (let ((buffers (sort (wl-collect-summary)
		       (lambda (buffer1 buffer2)
			 (string-lessp (buffer-name buffer1)
				       (buffer-name buffer2))))))
    (switch-to-buffer
     (or (cadr (memq (current-buffer) buffers))
	 (car buffers)))))

(defun wl-summary-previous-buffer ()
  "Switch to previous summary buffer."
  (interactive)
  (let ((buffers (sort (wl-collect-summary)
		       (lambda (buffer1 buffer2)
			 (not (string-lessp (buffer-name buffer1)
					    (buffer-name buffer2)))))))
    (switch-to-buffer
     (or (cadr (memq (current-buffer) buffers))
	 (car buffers)))))

(defun wl-summary-check-target-mark ()
  (when (null wl-summary-buffer-target-mark-list)
    (error "No marked message")))

(defun wl-summary-target-mark-mark-as-read ()
  (interactive)
  (wl-summary-check-target-mark)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-summary-mark-as-read wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

(defun wl-summary-target-mark-mark-as-unread ()
  (interactive)
  (wl-summary-check-target-mark)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-summary-mark-as-unread wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

(defun wl-summary-target-mark-operation (flag &optional inverse)
  (wl-summary-check-target-mark)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-summary-set-persistent-mark-internal
       inverse flag wl-summary-buffer-target-mark-list)
      (wl-summary-delete-all-target-marks))))

(defun wl-summary-target-mark-mark-as-important (&optional remove)
  (interactive "P")
  (wl-summary-target-mark-operation 'important remove))

(defun wl-summary-target-mark-mark-as-answered (&optional remove)
  (interactive "P")
  (wl-summary-target-mark-operation 'answered remove))

(defun wl-summary-target-mark-set-flags (&optional remove)
  (interactive "P")
  (wl-summary-check-target-mark)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-summary-set-flags-internal wl-summary-buffer-target-mark-list
				     nil nil remove)
      (wl-summary-delete-all-target-marks)
      (wl-summary-count-unread)
      (wl-summary-update-modeline))))

(defun wl-summary-target-mark-recover ()
  "Recover killed messages which have target mark."
  (interactive)
  (wl-summary-check-target-mark)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (elmo-folder-recover-messages wl-summary-buffer-elmo-folder
				    wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

(defun wl-summary-target-mark-save ()
  (interactive)
  (wl-summary-check-target-mark)
  (let ((wl-save-dir
	 (wl-read-directory-name "Save to directory: "
				 wl-temporary-file-directory))
	number)
    (if (null (file-exists-p wl-save-dir))
	(make-directory wl-save-dir))
    (while (setq number (car wl-summary-buffer-target-mark-list))
      (wl-thread-jump-to-msg number)
      (wl-summary-save t wl-save-dir)
      (wl-summary-unmark))))

(defun wl-summary-target-mark-pick ()
  (interactive)
  (wl-summary-check-target-mark)
  (wl-summary-pick wl-summary-buffer-target-mark-list 'delete))

(defun wl-summary-update-persistent-mark (&optional number)
  "Synch up persistent mark of current line with msgdb's.
Return non-nil if the mark is updated"
  (interactive)
  (let ((status (wl-summary-message-status number)))
    (prog1
	(when wl-summary-buffer-persistent-mark-column
	  (save-excursion
	    (move-to-column wl-summary-buffer-persistent-mark-column)
	    (let ((inhibit-read-only t)
		  (buffer-read-only nil)
		  (mark (buffer-substring (- (point) 1) (point)))
		  (new-mark (wl-summary-persistent-mark number status)))
	      (prog1
		  (unless (string= new-mark mark)
		    (delete-char -1)
		    (insert new-mark)
		    (wl-summary-set-message-modified)
		    t)
		(wl-summary-validate-persistent-mark (point-at-bol)
						     (point-at-eol))))))
      (when wl-summary-highlight
	(wl-highlight-summary-current-line number status))
      (set-buffer-modified-p nil))))

(defsubst wl-summary-mark-as-read-internal (inverse
					    number-or-numbers
					    no-folder-mark
					    no-modeline-update)
  (save-excursion
    (let ((folder wl-summary-buffer-elmo-folder)
	  unread-message number
	  number-list)
      (setq number-list (cond ((numberp number-or-numbers)
			       (setq unread-message
				     (elmo-message-flagged-p
				      folder
				      number-or-numbers
				      'unread))
			       (list number-or-numbers))
			      ((and (not (null number-or-numbers))
				    (listp number-or-numbers))
			       number-or-numbers)
			      ((setq number (wl-summary-message-number))
			       ;; interactive
			       (setq unread-message
				     (elmo-message-flagged-p
				      folder
				      number
				      'unread))
			       (list number))))
      (if (null number-list)
	  (message "No message.")
	(if inverse
	    (elmo-folder-set-flag folder number-list 'unread no-folder-mark)
	  (elmo-folder-unset-flag folder number-list 'unread no-folder-mark))
	(when (and unread-message
		   (not inverse))
	  (dolist (number number-list)
	    (wl-summary-jump-to-msg number)
	    (run-hooks 'wl-summary-unread-message-hook)))
	(unless no-modeline-update
	  ;; Update unread numbers.
	  (wl-summary-count-unread)
	  (wl-summary-update-modeline)
	  (wl-folder-update-unread
	   (wl-summary-buffer-folder-name)
	   wl-summary-buffer-unread-count))))))

(defun wl-summary-mark-as-read (&optional number-or-numbers
					  no-folder-mark
					  no-modeline-update)
  (interactive)
  (wl-summary-mark-as-read-internal nil
				    number-or-numbers
				    no-folder-mark
				    no-modeline-update))

(defun wl-summary-mark-as-unread (&optional number-or-numbers
					    no-folder-mark
					    no-modeline-update)
  (interactive)
  (wl-summary-mark-as-read-internal 'inverse
				    number-or-numbers
				    no-folder-mark
				    no-modeline-update))

(defsubst wl-summary-set-persistent-mark-internal (inverse
						   flag
						   &optional number-or-numbers
						   no-modeline-update
						   no-server
						   interactive)
  "Set persistent mark."
  (save-excursion
    (let ((folder wl-summary-buffer-elmo-folder)
	  number number-list)
      (setq number-list (cond ((numberp number-or-numbers)
			       (list number-or-numbers))
			      ((and (not (null number-or-numbers))
				    (listp number-or-numbers))
			       number-or-numbers)
			      ((setq number (wl-summary-message-number))
			       ;; interactive
			       (list number))))
      (if (null number-list)
	  (message "No message.")
	;; XXX Only the first element of the list is checked.
	(if (elmo-message-flag-available-p folder (car number-list) flag)
	    (progn
	      (if inverse
		  (elmo-folder-unset-flag folder number-list flag no-server)
		(elmo-folder-set-flag folder number-list flag no-server))
	      (unless no-modeline-update
		;; Update unread numbers.
		;; should elmo-flag-mark-as-read return unread numbers?
		(wl-summary-count-unread)
		(wl-summary-update-modeline)
		(wl-folder-update-unread
		 (wl-summary-buffer-folder-name)
		 wl-summary-buffer-unread-count)))
	  (if interactive
	      (error "Flag `%s' is not available in this folder" flag)))))))

(defun wl-summary-unset-persistent-mark (&optional flag
						   number-or-numbers
						   no-modeline-update
						   no-server)
  "Unset persistent mark."
  (interactive)
  (when (interactive-p)
    (let ((completion-ignore-case t))
      (setq flag (intern (downcase
			  (completing-read
			   "Mark name: "
			   (mapcar (lambda (flag)
				     (list (capitalize (symbol-name flag))))
				   (wl-summary-get-available-flags))
			   nil
			   'require-match))))))
  (wl-summary-set-persistent-mark-internal 'inverse
					   flag
					   number-or-numbers
					   no-modeline-update
					   no-server
					   (interactive-p)))

(defun wl-summary-set-persistent-mark (&optional flag
						 number-or-numbers
						 no-modeline-update
						 no-server)
  "Set persistent mark."
  (interactive)
  (when (interactive-p)
    (let ((completion-ignore-case t))
      (setq flag (intern (downcase
			  (completing-read
			   "Mark name: "
			   (mapcar (lambda (flag)
				     (list (capitalize (symbol-name flag))))
				   (wl-summary-get-available-flags))
			   nil
			   'require-match))))))
  (wl-summary-set-persistent-mark-internal nil
					   flag
					   number-or-numbers
					   no-modeline-update
					   no-server
					   (interactive-p)))

(defun wl-summary-toggle-persistent-mark (&optional force)
  "Toggle persistent mark."
  (interactive "P")
  (let ((completion-ignore-case t)
	flag)
    (setq flag (intern (downcase
			(completing-read
			 "Mark name: "
			 (mapcar (lambda (flag)
				   (list (capitalize (symbol-name flag))))
				 (wl-summary-get-available-flags))
			 nil
			 'require-match))))
    (if (and (elmo-message-flagged-p wl-summary-buffer-elmo-folder
				     (wl-summary-message-number)
				     flag)
	     (not force))
	(wl-summary-unset-persistent-mark flag)
      (wl-summary-set-persistent-mark flag))))

(defun wl-summary-mark-as-answered (&optional number-or-numbers
					      no-modeline-update)
  (interactive)
  (wl-summary-set-persistent-mark-internal
   (and (interactive-p)
	(elmo-message-flagged-p wl-summary-buffer-elmo-folder
				(wl-summary-message-number)
				'answered))
   'answered
   number-or-numbers
   no-modeline-update
   nil
   (interactive-p)))

(defun wl-summary-mark-as-unanswered (&optional number-or-numbers
						no-modeline-update)
  (wl-summary-set-persistent-mark-internal
   'inverse
   'answered
   number-or-numbers
   no-modeline-update))

(defun wl-summary-decide-flag (folder number)
  (let ((flags (elmo-get-global-flags (elmo-message-flags
				       folder number)))
	(completion-ignore-case t)
	new-flags)
    (setq new-flags
	  (delq nil
		(mapcar
		 (lambda (flag)
		   (and (> (length flag) 0)
			(intern (downcase flag))))
		 (wl-completing-read-multiple
		  "Flags: "
		  (mapcar (lambda (flag)
			    (list (capitalize (symbol-name flag))))
			  elmo-global-flags)
		  nil nil (mapconcat (lambda (flag)
				       (capitalize (symbol-name flag)))
				     flags
				     ",")))))
    (dolist (flag new-flags)
      (unless (memq flag elmo-global-flags)
	(when (elmo-local-flag-p flag)
	  (error "Cannot treat `%s'." flag))
	(unless (elmo-flag-valid-p flag)
	  (error "Invalid char in `%s'" flag))
	(if (y-or-n-p (format "Flag `%s' is not registered yet. Register?"
			      (capitalize (symbol-name flag))))
	    (setq elmo-global-flags (append
				     elmo-global-flags
				     (list flag)))
	  (error "Stopped"))))
    new-flags))

(defsubst wl-summary-set-flags-internal (&optional
					number-or-numbers
					flags
					local
					remove-all)
  (save-excursion
    (let ((folder wl-summary-buffer-elmo-folder)
	  number number-list)
      (setq number-list (cond ((numberp number-or-numbers)
			       (list number-or-numbers))
			      ((and (not (null number-or-numbers))
				    (listp number-or-numbers))
			       number-or-numbers)
			      ((setq number (wl-summary-message-number))
			       ;; interactive
			       (list number))))
      (if remove-all
	  (setq flags nil)
	(unless flags
	  (setq flags (wl-summary-decide-flag folder (car number-list)))))
      (if (null number-list)
	  (message "No message.")
	(dolist (number number-list)
	  (elmo-message-set-global-flags folder number flags local)))
      flags)))

(defun wl-summary-set-flags (&optional remove)
  (interactive "P")
  (wl-summary-set-flags-internal nil nil nil remove))

(defun wl-summary-mark-as-important (&optional prompt)
  (interactive "P")
  (if prompt
      (wl-summary-set-flags-internal)
    (wl-summary-set-persistent-mark-internal
     (and (interactive-p)
	  (elmo-message-flagged-p wl-summary-buffer-elmo-folder
				  (wl-summary-message-number)
				  'important))
     'important
     nil nil nil (interactive-p))))

(defun wl-summary-recover-message (number)
  "Recover current message if it is killed."
  (interactive (list (wl-summary-message-number)))
  (if (null number)
      (message "No message.")
    (elmo-folder-recover-messages wl-summary-buffer-elmo-folder
				  (list number))))

;;; Summary line.
(defvar wl-summary-line-formatter nil)

(defun wl-summary-view-old-p ()
  "Return non-nil when summary view cache has old format."
  (when wl-summary-buffer-number-list
    (save-excursion
      (goto-char (point-min))
      (not (re-search-forward "\r-?[0-9]+" (point-at-eol) t)))))

(defun wl-summary-line-format-changed-p ()
  "Return non-nil when summary line format is changed."
  (not (string=
	wl-summary-buffer-line-format
	(or (elmo-object-load (expand-file-name
			       wl-summary-line-format-file
			       (elmo-folder-msgdb-path
				wl-summary-buffer-elmo-folder))
			      wl-summary-buffer-mime-charset)
	    wl-summary-buffer-line-format))))

(defun wl-summary-line-format-save ()
  "Save current summary line format."
  (elmo-object-save
   (expand-file-name wl-summary-line-format-file
		     (elmo-folder-msgdb-path
		      wl-summary-buffer-elmo-folder))
   wl-summary-buffer-line-format
   wl-summary-buffer-mime-charset))

(defun wl-summary-line-number ()
  (wl-set-string-width
   (- wl-summary-buffer-number-column)
   (number-to-string
    (elmo-message-entity-number wl-message-entity))))

(defun wl-summary-line-year ()
  (aref wl-datevec 0))
(defun wl-summary-line-month ()
  (format "%02d" (aref wl-datevec 1)))
(defun wl-summary-line-day ()
  (format "%02d" (aref wl-datevec 2)))
(defun wl-summary-line-day-of-week ()
  (condition-case nil
      (elmo-date-get-week (aref wl-datevec 0)
			  (aref wl-datevec 1)
			  (aref wl-datevec 2))
    (error "??")))
(defun wl-summary-line-hour ()
  (format "%02d" (aref wl-datevec 3)))
(defun wl-summary-line-minute ()
  (format "%02d" (aref wl-datevec 4)))

(defun wl-summary-line-size ()
  (let ((size (elmo-message-entity-field wl-message-entity 'size)))
    (if size
	(cond
	 ((<= 1 (/ size 1048576))
	  (format "%.0fM" (/ size 1048576.0)))
	 ((<= 1 (/ size 1024))
	  (format "%.0fK" (/ size 1024.0)))
	 (t (format "%dB" size)))
      "")))

(defun wl-summary-line-subject ()
  (let (no-parent subject parent-raw-subject parent-subject)
    (if (string= wl-thr-indent-string "")
	(setq no-parent t)) ; no parent
    (setq subject
	  (elmo-delete-char ?\n
			    (or (elmo-message-entity-field
				 wl-message-entity
				 'subject)
				wl-summary-no-subject-message)))
    (setq parent-raw-subject
	  (elmo-message-entity-field wl-parent-message-entity 'subject))
    (setq parent-subject
	  (if parent-raw-subject
	      (elmo-delete-char ?\n parent-raw-subject)))
    (if (or no-parent
	    (null parent-subject)
	    (not (wl-summary-subject-equal subject parent-subject)))
	(funcall wl-summary-subject-function subject)
      "")))

(defun wl-summary-line-from ()
  (elmo-delete-char ?\n
		    (funcall wl-summary-from-function
			     (elmo-message-entity-field
			      wl-message-entity
			      'from))))

(defun wl-summary-line-list-info ()
  (let ((list-info (wl-summary-get-list-info wl-message-entity)))
    (if (car list-info)
	(format (if (cdr list-info) "(%s %05.0f)" "(%s)")
		(car list-info) (cdr list-info))
      "")))

(defun wl-summary-line-list-count ()
  (let ((ml-count (cdr (wl-summary-get-list-info wl-message-entity))))
    (if ml-count
	(format "%.0f" ml-count)
      "")))

(defun wl-summary-line-attached ()
  (let ((content-type (elmo-message-entity-field
		       wl-message-entity 'content-type))
	(case-fold-search t))
    (if (and content-type
	     (string-match "multipart/mixed" content-type))
	"@"
      "")))

;;; For future use.
;;;(defun wl-summary-line-cached ()
;;;  (if (elmo-message-cached-p wl-summary-buffer-elmo-folder
;;;			     (elmo-message-entity-number wl-message-entity))
;;;      " "
;;;    "u"))

(defun wl-summary-create-line (wl-message-entity
			       wl-parent-message-entity
			       wl-temp-mark
			       wl-status
			       &optional
			       wl-thr-children-number
			       wl-thr-indent-string
			       wl-thr-linked)
  "Create a summary line."
  (let ((wl-mime-charset wl-summary-buffer-mime-charset)
	(wl-persistent-mark (wl-summary-persistent-mark-string
			     wl-summary-buffer-elmo-folder
			     wl-status))
	(elmo-mime-charset wl-summary-buffer-mime-charset)
	(elmo-lang wl-summary-buffer-weekday-name-lang)
	(wl-datevec (or (elmo-time-to-datevec
			 (elmo-message-entity-field wl-message-entity 'date)
			 wl-summary-fix-timezone)
			(make-vector 7 0)))
	(entity wl-message-entity) ; backward compatibility.
	line mark)
    (if (and wl-thr-indent-string
	     wl-summary-indent-length-limit
	     (< wl-summary-indent-length-limit
		(string-width wl-thr-indent-string)))
	(setq wl-thr-indent-string (wl-set-string-width
				    wl-summary-indent-length-limit
				    wl-thr-indent-string)))
    (setq line (funcall wl-summary-buffer-line-formatter))
    (if wl-summary-width (setq line
			       (wl-set-string-width
				(- wl-summary-width 1) line nil
				'ignore-invalid)))
    (setq line (concat line
		       "\r"
		       (number-to-string
			(elmo-message-entity-number
			 wl-message-entity))))
    (wl-summary-validate-persistent-mark-string line)
    (if wl-summary-highlight
	(wl-highlight-summary-line-string
	 (elmo-message-entity-number wl-message-entity)
	 line
	 wl-status
	 wl-temp-mark
	 wl-thr-indent-string))
    line))

(defsubst wl-summary-proc-wday (wday-str year month mday)
  (save-match-data
    (if (string-match "\\([A-Z][a-z][a-z]\\).*" wday-str)
	(wl-match-string 1 wday-str)
      (elmo-date-get-week year month mday))))

(defvar wl-summary-move-spec-alist
  '((new . ((t . nil)
	    (p . new)
	    (p . unread)
	    (p . digest)))
    (unread . ((t . nil)
	       (p . unread)
	       (p . digest)))))

(defsubst wl-summary-next-message (num direction hereto)
  (if wl-summary-buffer-next-message-function
      (funcall wl-summary-buffer-next-message-function num direction hereto)
    (let ((cur-spec (cdr (assq wl-summary-move-order
			       wl-summary-move-spec-alist)))
	  (nums (memq num (if (eq direction 'up)
			      (reverse wl-summary-buffer-number-list)
			    wl-summary-buffer-number-list)))
	  flagged-list nums2)
      (unless hereto (setq nums (cdr nums)))
      (setq nums2 nums)
      (if cur-spec
	  (catch 'done
	    (while cur-spec
	      (setq nums nums2)
	      (cond ((eq (car (car cur-spec)) 'p)
		     (if (setq flagged-list
			       (elmo-folder-list-flagged
				wl-summary-buffer-elmo-folder
				(cdr (car cur-spec)) t))
			 (while nums
			   (if (and (memq (car nums) flagged-list)
				    (elmo-message-accessible-p
				     wl-summary-buffer-elmo-folder
				     (car nums)))
			       (throw 'done (car nums)))
			   (setq nums (cdr nums)))))
		    ((eq (car (car cur-spec)) 't)
		     (if wl-summary-buffer-target-mark-list
			 (while nums
			   (if (memq (car nums)
				     wl-summary-buffer-target-mark-list)
			       (throw 'done (car nums)))
			   (setq nums (cdr nums))))))
	      (setq cur-spec (cdr cur-spec))))
	(car nums)))))

(defsubst wl-summary-cursor-move (direction hereto)
  (when (and (eq direction 'up)
	     (eobp))
    (forward-line -1)
    (setq hereto t))
  (let (num)
    (when (setq num (wl-summary-next-message (wl-summary-message-number)
					     direction hereto))
      (if (numberp num)
	  (wl-thread-jump-to-msg num))
      t)))
;;
;; Goto unread or global flag message
;; returns t if next message exists in this folder.
(defun wl-summary-cursor-down (&optional hereto)
  (interactive "P")
  (wl-summary-cursor-move 'down hereto))

(defun wl-summary-cursor-up (&optional hereto)
  (interactive "P")
  (wl-summary-cursor-move 'up hereto))

(defun wl-summary-save-view-cache ()
  (save-excursion
    (let* ((dir (elmo-folder-msgdb-path wl-summary-buffer-elmo-folder))
	   (cache (expand-file-name wl-summary-cache-file dir))
	   (view (expand-file-name wl-summary-view-file dir))
	   (save-view wl-summary-buffer-view)
	   (mark-list (copy-sequence wl-summary-buffer-target-mark-list))
	   (temp-list (copy-sequence wl-summary-buffer-temp-mark-list))
	   (tmp-buffer (get-buffer-create " *wl-summary-save-view-cache*"))
	   (temp-column wl-summary-buffer-temp-mark-column)
	   (charset wl-summary-buffer-mime-charset))
      (when dir
	(if (file-directory-p dir)
	    (); ok.
	  (if (file-exists-p dir)
	      (error "File %s already exists" dir)
	    (elmo-make-directory dir)))
	(if (eq save-view 'thread)
	    (wl-thread-save-entity dir))
	(when wl-summary-check-line-format
	  (wl-summary-line-format-save))
	(unwind-protect
	    (progn
	      (when (file-writable-p cache)
		(copy-to-buffer tmp-buffer (point-min) (point-max))
		(with-current-buffer tmp-buffer
		  (widen)
		  (make-local-variable 'wl-summary-highlight)
		  (setq wl-summary-highlight nil
			wl-summary-buffer-target-mark-list mark-list
			wl-summary-buffer-temp-mark-list temp-list
			wl-summary-buffer-temp-mark-column temp-column)
		  (wl-summary-delete-all-temp-marks 'no-msg 'force)
		  (encode-coding-region
		   (point-min) (point-max)
		   (or (and wl-on-mule
			    ;; one in mcs-ltn1(apel<10.4) cannot take 2 arg.
			    (mime-charset-to-coding-system charset 'LF))
		       ;; Mule 2 doesn't have `*ctext*unix'.
		       (mime-charset-to-coding-system charset)))
		  (write-region-as-binary (point-min)(point-max)
					  cache nil 'no-msg)))
	      (when (file-writable-p view) ; 'thread or 'sequence
		(with-temp-buffer
		  (prin1 save-view (current-buffer))
		  (princ "\n" (current-buffer))
		  (write-region (point-min) (point-max) view nil 'no-msg))))
	  ;; kill tmp buffer.
	  (kill-buffer tmp-buffer))))))

(defsubst wl-summary-get-sync-range (folder)
  (intern (or (and
	       (elmo-folder-plugged-p folder)
	       (wl-get-assoc-list-value
		wl-folder-sync-range-alist
		(elmo-folder-name-internal folder)
		'function))
	      wl-default-sync-range)))

;; redefined for wl-summary-sync-update
(defun wl-summary-input-range (folder)
  "returns update or all or rescan."
  ;; for the case when parts are expanded in the bottom of the folder
  (let ((input-range-list '("no-sync"
			    "first:"
			    "last:"
			    "cache-status"
			    "mark"
			    "rescan"
			    "rescan-noscore"
			    "rescan-thread"
			    "update"
			    "update-entirely"
			    "all"
			    "all-entirely"))
	(default (or (wl-get-assoc-list-value
		      wl-folder-sync-range-alist
		      folder
		      'function)
		     wl-default-sync-range))
	range)
    (setq range
	  (completing-read (format "Range (%s): " default)
			   (mapcar
			    (lambda (x) (cons x x))
			    input-range-list) nil nil nil nil default))))

(defun wl-summary-toggle-disp-folder (&optional arg)
  (interactive)
  (let ((cur-buf (current-buffer))
	(summary-win (get-buffer-window (current-buffer)))
	fld-buf fld-win)
    (cond
     ((eq arg 'on)
      (setq wl-summary-buffer-disp-folder t)
      ;; hide your folder window
      (if (setq fld-buf (get-buffer wl-folder-buffer-name))
	  (if (setq fld-win (get-buffer-window fld-buf))
	      (delete-window fld-win))))
     ((eq arg 'off)
      (setq wl-summary-buffer-disp-folder nil)
      ;; hide your wl-message window!
      (when (buffer-live-p wl-message-buffer)
	(wl-message-select-buffer wl-message-buffer)
	(delete-window))
      (select-window (get-buffer-window cur-buf))
      ;; display wl-folder window!!
      (if (setq fld-buf (get-buffer wl-folder-buffer-name))
	  (if (setq fld-win (get-buffer-window fld-buf))
	      ;; folder win is already displayed.
	      (select-window fld-win)
	    ;; folder win is not displayed.
	    (switch-to-buffer fld-buf))
	;; no folder buf
	(wl-folder))
      ;; temporarily delete summary-win.
      (if summary-win
	  (delete-window summary-win))
      (split-window-horizontally wl-folder-window-width)
      (other-window 1)
      (switch-to-buffer cur-buf))
     (t
      (if (setq fld-buf (get-buffer wl-folder-buffer-name))
	  (if (setq fld-win (get-buffer-window fld-buf))
	      (setq wl-summary-buffer-disp-folder nil)
	    (setq wl-summary-buffer-disp-folder t)))
      (if (not wl-summary-buffer-disp-folder)
	  ;; hide message window
	  (let ((mes-win (and wl-message-buffer
			      (get-buffer-window wl-message-buffer)))
		(wl-stay-folder-window t))
	    (if mes-win (delete-window mes-win))
	    ;; hide your folder window
	    (if (setq fld-buf (get-buffer wl-folder-buffer-name))
		(if (setq fld-win (get-buffer-window fld-buf))
		    (progn
		      (delete-window (get-buffer-window cur-buf))
		      (select-window fld-win)
		      (switch-to-buffer cur-buf))))
	    (run-hooks 'wl-summary-toggle-disp-folder-off-hook)
	    ;; resume message window.
	    (when mes-win
	      (wl-message-select-buffer wl-message-buffer)
	      (run-hooks 'wl-summary-toggle-disp-folder-message-resumed-hook)
	      (select-window (get-buffer-window cur-buf))))
	;; hide message window
	(let ((wl-stay-folder-window t)
	      (mes-win (and wl-message-buffer
			    (get-buffer-window wl-message-buffer))))
	  (if mes-win (delete-window mes-win))
	  (select-window (get-buffer-window cur-buf))
	  ;; display wl-folder window!!
	  (if (setq fld-buf (get-buffer wl-folder-buffer-name))
	      (if (setq fld-win (get-buffer-window fld-buf))
		  ;; folder win is already displayed.
		  (select-window fld-win)
		;; folder win is not displayed...occupy all.
		(switch-to-buffer fld-buf))
	    ;; no folder buf
	    (wl-folder))
	  (split-window-horizontally wl-folder-window-width)
	  (other-window 1)
	  (switch-to-buffer cur-buf)
	  ;; resume message window.
	  (run-hooks 'wl-summary-toggle-disp-folder-on-hook)
	  (when mes-win
	    (wl-message-select-buffer wl-message-buffer)
	    (run-hooks 'wl-summary-toggle-disp-folder-message-resumed-hook)
	    (select-window (get-buffer-window cur-buf))))))))
  (run-hooks 'wl-summary-toggle-disp-folder-hook))

(defun wl-summary-toggle-disp-msg (&optional arg)
  (interactive)
  (let ((cur-buf (current-buffer))
	fld-buf fld-win
	summary-win)
    (cond
     ((eq arg 'on)
      (setq wl-summary-buffer-disp-msg t)
      (save-excursion
	;; hide your folder window
	(if (and (not wl-stay-folder-window)
		 (setq fld-buf (get-buffer wl-folder-buffer-name)))
	    (if (setq fld-win (get-buffer-window fld-buf))
		(unless (one-window-p fld-win)
		  (delete-window fld-win))))))
     ((eq arg 'off)
      (wl-delete-all-overlays)
      (setq wl-summary-buffer-disp-msg nil)
      (save-excursion
	(when (buffer-live-p wl-message-buffer)
	  (wl-message-select-buffer wl-message-buffer)
	  (delete-window)
	  (and (get-buffer-window cur-buf)
	       (select-window (get-buffer-window cur-buf))))
	(run-hooks 'wl-summary-toggle-disp-off-hook)))
     (t
      (if (and wl-message-buffer
	       (get-buffer-window wl-message-buffer)) ; already displayed
	  (setq wl-summary-buffer-disp-msg nil)
	(setq wl-summary-buffer-disp-msg t))
      (if wl-summary-buffer-disp-msg
	  (progn
	    (wl-summary-redisplay)
;;; hide your folder window
;;;	    (setq fld-buf (get-buffer wl-folder-buffer-name))
;;;	    (if (setq fld-win (get-buffer-window fld-buf))
;;;		(delete-window fld-win)))
	    (run-hooks 'wl-summary-toggle-disp-on-hook))
	(wl-delete-all-overlays)
	(save-excursion
	  (wl-message-select-buffer wl-message-buffer)
	  (delete-window)
	  (select-window (get-buffer-window cur-buf))
	  (setq wl-message-buffer nil)
	  (run-hooks 'wl-summary-toggle-disp-off-hook))
;;;	(switch-to-buffer cur-buf)
	)))
    (run-hooks 'wl-summary-buffer-window-scroll-functions)))

(defun wl-summary-enter-handler (&optional arg)
  "A command for `enter' key in the summary.
Basically, it shows next line of the message.
If optional argument ARG is specified, behave as followed.
If ARG is number, jump to the message.
Otherwise it shows previous line of the message."
  (interactive "P")
  (cond ((numberp arg)
	 (unless (wl-thread-jump-to-msg arg)
	   (message "Message (#%d) was not found." arg)))
	(arg
	 (wl-summary-prev-line-content))
	(t
	 (wl-summary-next-line-content))))

(defun wl-summary-next-line-content ()
  "Show next line of the message."
  (interactive)
  (let ((cur-buf (current-buffer)))
    (wl-summary-toggle-disp-msg 'on)
    (when (wl-summary-set-message-buffer-or-redisplay 'ignore-original)
      (set-buffer cur-buf)
      (wl-message-next-page 1))))

(defun wl-summary-prev-line-content ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (wl-summary-toggle-disp-msg 'on)
    (when (wl-summary-set-message-buffer-or-redisplay 'ignore-original)
      (set-buffer cur-buf)
      (wl-message-prev-page 1))))

(defun wl-summary-next-page ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (wl-summary-toggle-disp-msg 'on)
    (when (wl-summary-set-message-buffer-or-redisplay 'ignore-original)
      (set-buffer cur-buf)
      (wl-message-next-page))))

(defun wl-summary-prev-page ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (wl-summary-toggle-disp-msg 'on)
    (when (wl-summary-set-message-buffer-or-redisplay 'ignore-original)
      (set-buffer cur-buf)
      (wl-message-prev-page))))

(defsubst wl-summary-no-mime-p (folder)
  (wl-string-match-member (elmo-folder-name-internal folder)
			  wl-summary-no-mime-folder-list))

(defun wl-summary-set-message-buffer-or-redisplay (&rest args)
  "Set message buffer.
If message is not displayed yet, display it.
Return t if message exists."
  (let ((folder wl-summary-buffer-elmo-folder)
	(number (wl-summary-message-number))
	cur-folder cur-number message-last-pos)
    (when (buffer-live-p wl-message-buffer)
      (save-window-excursion
	(setq wl-current-summary-buffer (current-buffer))
	(wl-message-select-buffer wl-message-buffer)
	(setq cur-folder wl-message-buffer-cur-folder)
	(setq cur-number wl-message-buffer-cur-number)))
    (if (and (string= (elmo-folder-name-internal folder) (or cur-folder ""))
	     (eq number (or cur-number 0)))
	(progn
	  (set-buffer wl-message-buffer)
	  t)
      (wl-summary-redisplay-internal folder number)
      (when (buffer-live-p wl-message-buffer)
	(set-buffer wl-message-buffer))
      nil)))

(defun wl-summary-target-mark-forward (&optional arg)
  (interactive "P")
  (wl-summary-check-target-mark)
  (let ((mlist (nreverse (copy-sequence wl-summary-buffer-target-mark-list)))
	(summary-buf (current-buffer))
	(wl-draft-forward t)
	start-point
	draft-buf)
    (wl-summary-jump-to-msg (car mlist))
    (wl-summary-forward t)
    (setq start-point (point))
    (setq draft-buf (current-buffer))
    (setq mlist (cdr mlist))
    (save-window-excursion
      (when mlist
	(while mlist
	  (set-buffer summary-buf)
	  (wl-summary-jump-to-msg (car mlist))
	  (wl-summary-redisplay)
	  (set-buffer draft-buf)
	  (goto-char (point-max))
	  (wl-draft-insert-message)
	  (setq mlist (cdr mlist)))
	(wl-draft-body-goto-top)
	(wl-draft-enclose-digest-region (point) (point-max)))
      (goto-char start-point)
      (with-current-buffer summary-buf
	(wl-summary-delete-all-target-marks)))
    (run-hooks 'wl-mail-setup-hook)))

(defun wl-summary-target-mark-reply-with-citation (&optional arg)
  (interactive "P")
  (wl-summary-check-target-mark)
  (let ((mlist (nreverse (copy-sequence wl-summary-buffer-target-mark-list)))
	(summary-buf (current-buffer))
	change-major-mode-hook
	start-point
	draft-buf)
    (wl-summary-jump-to-msg (car mlist))
    (when (wl-summary-reply arg t)
      (goto-char (point-max))
      (setq start-point (point-marker))
      (setq draft-buf (current-buffer))
      (save-window-excursion
	(while mlist
	  (set-buffer summary-buf)
	  (delete-other-windows)
	  (wl-summary-jump-to-msg (car mlist))
	  (wl-summary-redisplay)
	  (set-buffer draft-buf)
	  (goto-char (point-max))
	  (wl-draft-yank-original)
	  (setq mlist (cdr mlist)))
	(goto-char start-point)
	(with-current-buffer summary-buf
	  (wl-summary-delete-all-target-marks)))
      (wl-draft-reply-position wl-draft-reply-default-position)
      (run-hooks 'wl-mail-setup-hook))))

(defun wl-summary-reply-with-citation (&optional arg)
  (interactive "P")
  (when (wl-summary-reply arg t)
    (goto-char (point-max))
    (wl-draft-yank-original)
    (wl-draft-reply-position wl-draft-reply-default-position)
    (run-hooks 'wl-mail-setup-hook)))

(defun wl-summary-jump-to-msg-by-message-id (&optional id)
  (interactive)
  (let* ((original (wl-summary-message-number))
	 (msgid (elmo-string (or id (read-from-minibuffer "Message-ID: "))))
	 (entity (elmo-message-entity wl-summary-buffer-elmo-folder msgid))
	 msg otherfld schar
	 (errmsg (format "No message with id \"%s\" in the folder." msgid)))
    (if (setq msg (elmo-message-entity-number entity))
	(progn
	  (wl-thread-jump-to-msg msg)
	  t)
      ;; for XEmacs!
      (if (and elmo-use-database
	       (setq errmsg
		     (format
		      "No message with id \"%s\" in the database." msgid))
	       (setq otherfld (elmo-database-msgid-get msgid)))
	  (if (cdr (wl-summary-jump-to-msg-internal
		    (car otherfld) (nth 1 otherfld) 'no-sync))
	      t ; succeed.
	    ;; Back to original.
	    (wl-summary-jump-to-msg-internal
	     (wl-summary-buffer-folder-name) original 'no-sync))
	(cond ((eq wl-summary-search-via-nntp 'confirm)
	       (require 'elmo-nntp)
	       (message "Search message in nntp server \"%s\" <y/n/s(elect)>? "
			elmo-nntp-default-server)
	       (setq schar (let ((cursor-in-echo-area t)) (read-char)))
	       (cond ((eq schar ?y)
		      (wl-summary-jump-to-msg-by-message-id-via-nntp msgid))
		     ((eq schar ?s)
		      (wl-summary-jump-to-msg-by-message-id-via-nntp
		       msgid
		       (read-from-minibuffer "NNTP Server: ")))
		     (t
		      (message "%s" errmsg)
		      nil)))
	      ((or (eq wl-summary-search-via-nntp 'force)
		   (and
		    (eq (elmo-folder-type-internal wl-summary-buffer-elmo-folder)
			'nntp)
		    wl-summary-search-via-nntp))
	       (wl-summary-jump-to-msg-by-message-id-via-nntp msgid))
	      (t
	       (message "%s" errmsg)
	       nil))))))

(defun wl-summary-jump-to-msg-by-message-id-via-nntp (&optional id server-spec)
  (interactive)
  (let* ((msgid (elmo-string (or id (read-from-minibuffer "Message-ID: "))))
	 newsgroups folder ret
	 user server port type spec)
    (if server-spec
	(if (string-match "^-" server-spec)
	    (setq spec (wl-folder-get-elmo-folder server-spec)
		  user (elmo-net-folder-user-internal spec)
		  server (elmo-net-folder-server-internal spec)
		  port (elmo-net-folder-port-internal spec)
		  type (elmo-net-folder-stream-type-internal spec))
	  (setq server server-spec)))
    (when (setq ret (elmo-nntp-get-newsgroup-by-msgid
		     msgid
		     (or server elmo-nntp-default-server)
		     (or user elmo-nntp-default-user)
		     (or port elmo-nntp-default-port)
		     (or type elmo-nntp-default-stream-type)))
      (setq newsgroups (elmo-nntp-parse-newsgroups ret))
      (setq folder (concat "-" (car newsgroups)
			   (elmo-nntp-folder-postfix user server port type)))
      (catch 'found
	(while newsgroups
	  (if (wl-folder-entity-exists-p (car newsgroups)
					 wl-folder-newsgroups-hashtb)
	      (throw 'found
		     (setq folder (concat "-" (car newsgroups)
					  (elmo-nntp-folder-postfix
					   user server port type)))))
	  (setq newsgroups (cdr newsgroups)))))
    (if ret
	(wl-summary-jump-to-msg-internal folder nil 'update msgid)
      (message "No message id \"%s\" in nntp server \"%s\"."
	       msgid (or server elmo-nntp-default-server))
      nil)))

(defun wl-summary-jump-to-msg-internal (folder msg scan-type &optional msgid)
  (let (wl-auto-select-first entity)
    (if (or (string= folder (wl-summary-buffer-folder-name))
	    (y-or-n-p
	     (format
	      "Message was found in the folder \"%s\". Jump to it? "
	      folder)))
	(progn
	  (unwind-protect
	      (wl-summary-goto-folder-subr
	       folder scan-type nil nil t)
	    (if msgid
		(setq msg
		      (elmo-message-entity-number
		       (elmo-message-entity
			wl-summary-buffer-elmo-folder
			msgid))))
	    (setq entity (wl-folder-search-entity-by-name folder
							  wl-folder-entity
							  'folder))
	    (if entity
		(wl-folder-set-current-entity-id
		 (wl-folder-get-entity-id entity))))
	  (if (null msg)
	      (message "Message was not found currently in this folder.")
	    (setq msg (and (wl-thread-jump-to-msg msg) msg)))
	  (cons folder msg)))))

(defun wl-summary-jump-to-parent-message (arg)
  (interactive "P")
  (let ((cur-buf (current-buffer))
	(disp-msg wl-summary-buffer-disp-msg)
	(number (wl-summary-message-number))
	(regexp "\\(<[^<>]*>\\)[ \t]*$")
	(i -1) ;; xxx
	msg-id msg-num ref-list ref irt)
    (if (null number)
	(message "No message.")
      (when (eq wl-summary-buffer-view 'thread)
	(cond ((and arg (not (numberp arg)))
	       (setq msg-num
		     (wl-thread-entity-get-number
		      (wl-thread-entity-get-top-entity
		       (wl-thread-get-entity number)))))
	      ((and arg (numberp arg))
	       (setq i 0)
	       (setq msg-num number)
	       (while (< i arg)
		 (setq msg-num
		       (wl-thread-entity-get-number
			(wl-thread-entity-get-parent-entity
			 (wl-thread-get-entity msg-num))))
		 (setq i (1+ i))))
	      (t (setq msg-num
		       (wl-thread-entity-get-number
			(wl-thread-entity-get-parent-entity
			 (wl-thread-get-entity number)))))))
      (when (null msg-num)
	(wl-summary-set-message-buffer-or-redisplay)
	(set-buffer (wl-message-get-original-buffer))
	(message "Searching parent message...")
	(setq ref (std11-field-body "References")
	      irt (std11-field-body "In-Reply-To"))
	(cond
	 ((and arg (not (numberp arg)) ref (not (string= ref ""))
	       (string-match regexp ref))
	  ;; The first message of the thread.
	  (setq msg-id (wl-match-string 1 ref)))
	 ;; "In-Reply-To:" has only one msg-id.
	 ((and (null arg) irt (not (string= irt ""))
	       (string-match regexp irt))
	  (setq msg-id (wl-match-string 1 irt)))
	 ((and (or (null arg) (numberp arg)) ref (not (string= ref ""))
	       (string-match regexp ref))
	  ;; "^" searching parent, "C-u 2 ^" looking for grandparent.
	  (while (string-match regexp ref)
	    (setq ref-list
		  (append (list
			   (wl-match-string 1 ref))
			  ref-list))
	    (setq ref (substring ref (match-end 0)))
	    (setq i (1+ i)))
	  (setq msg-id
		(if (null arg) (nth 0 ref-list) ;; previous
		  (if (<= arg i) (nth (1- arg) ref-list)
		    (nth i ref-list))))))
	(set-buffer cur-buf)
	(or disp-msg (wl-summary-toggle-disp-msg 'off)))
      (cond ((and (null msg-id) (null msg-num))
	     (message "No parent message!")
	     nil)
	    ((and msg-id (wl-summary-jump-to-msg-by-message-id msg-id))
	     (if wl-summary-buffer-disp-msg (wl-summary-redisplay))
	     (message "Searching parent message...done")
	     t)
	    ((and msg-num (wl-summary-jump-to-msg msg-num))
	     (if wl-summary-buffer-disp-msg (wl-summary-redisplay))
	     (message "Searching parent message...done")
	     t)
	    (t ; failed.
	     (message "Parent message was not found.")
	     nil)))))

(defun wl-summary-reply (&optional arg without-setup-hook)
  "Reply to current message. See also `wl-draft-reply'."
  (interactive "P")
  (let ((folder wl-summary-buffer-elmo-folder)
	(number (wl-summary-message-number))
	(summary-buf (current-buffer))
	(winconf (current-window-configuration))
	mes-buf)
    (when number
      (save-excursion
	(wl-summary-set-message-buffer-or-redisplay))
      (wl-message-select-buffer wl-message-buffer)
      (condition-case err
	  (when (setq mes-buf (wl-message-get-original-buffer))
	    (wl-draft-reply mes-buf arg summary-buf number)
	    (wl-draft-reply-position wl-draft-reply-default-position)
	    (unless without-setup-hook
	      (run-hooks 'wl-mail-setup-hook)))
	(error (set-window-configuration winconf)
	       (signal (car err)(cdr err))))
      (with-current-buffer summary-buf (run-hooks 'wl-summary-reply-hook))
      t)))

(defun wl-summary-write (folder)
  "Write a new draft from Summary."
  (interactive (list (wl-summary-buffer-folder-name)))
  (wl-draft (list (cons 'To "")) nil nil nil nil folder)
  (run-hooks 'wl-mail-setup-hook)
  (mail-position-on-field "To"))

(defvar wl-summary-write-current-folder-functions
  '(wl-folder-get-newsgroups
    wl-folder-guess-mailing-list-by-refile-rule
    wl-folder-guess-mailing-list-by-folder-name)
  "Newsgroups or Mailing List address guess functions list.
Call from `wl-summary-write-current-folder'.
When guess function return nil, challenge next guess-function.")

(defun wl-summary-write-current-folder (folder)
  "Write message to current FOLDER's newsgroup or mailing-list.
Use function list is `wl-summary-write-current-folder-functions'."
  (interactive (list (wl-summary-buffer-folder-name)))
  (let ((func-list wl-summary-write-current-folder-functions)
	guess-list guess-func)
    (while func-list
      (setq guess-list (funcall (car func-list) folder))
      (if (null guess-list)
	  (setq func-list (cdr func-list))
	(setq guess-func (car func-list))
	(setq func-list nil)))
    (if (null guess-func)
	(wl-summary-write folder)
      (unless (or (stringp (nth 0 guess-list))
		  (stringp (nth 1 guess-list))
		  (stringp (nth 2 guess-list)))
	(error "Invalid value return guess function `%s'"
	       (symbol-name guess-func)))
      (wl-draft (list (cons 'To (nth 0 guess-list))
		      (cons 'Cc (nth 1 guess-list))
		      (cons 'Newsgroups (nth 2 guess-list)))
		nil nil nil nil folder)
      (run-hooks 'wl-mail-setup-hook)
      (mail-position-on-field "Subject"))))

(defun wl-summary-forward (&optional without-setup-hook)
  ""
  (interactive)
  (let ((folder wl-summary-buffer-elmo-folder)
	(number (wl-summary-message-number))
	(summary-buf (current-buffer))
	(wl-draft-forward t)
	entity subject num)
    (if (null number)
	(message "No message.")
      (if (and (elmo-message-use-cache-p folder number)
	       (eq (elmo-file-cache-status
		    (elmo-file-cache-get
		     (elmo-message-field folder number 'message-id)))
		   'section))
	  ;; Reload.
	  (wl-summary-redisplay-internal nil nil 'force-reload)
	(wl-summary-redisplay-internal folder number))
      (wl-message-select-buffer wl-message-buffer)
      (setq subject (with-current-buffer
			wl-message-buffer-original-buffer
		      (std11-field-body "Subject")))
      (wl-draft-forward subject summary-buf number)
      (with-current-buffer summary-buf (run-hooks 'wl-summary-forward-hook))
      (unless without-setup-hook
	(run-hooks 'wl-mail-setup-hook)))))

(defun wl-summary-click (e)
  (interactive "e")
  (mouse-set-point e)
  (wl-summary-read))

(defun wl-summary-read ()
  "Proceed reading message in the summary buffer."
  (interactive)
  (let ((cur-buf (current-buffer)))
    (wl-summary-toggle-disp-msg 'on)
    (when (wl-summary-set-message-buffer-or-redisplay 'ignore-original)
      (set-buffer cur-buf)
      (if (wl-message-next-page)
	  (wl-summary-down t)))))

(defsubst wl-summary-cursor-move-surface (downward interactive)
  (if wl-summary-move-direction-toggle
      (setq wl-summary-move-direction-downward downward))
  (let ((start (point))
	(skip-tmark-regexp (wl-regexp-opt wl-summary-skip-mark-list))
	(skip t)
	(column (current-column))
	goto-next next-entity finfo)
    (beginning-of-line)
    (while (and skip
		(not (if downward (eobp) (bobp))))
      (if downward
	  (forward-line)
	(forward-line -1))
      (setq skip (or (string-match skip-tmark-regexp
				   (wl-summary-temp-mark))
		     (not (and (wl-summary-message-number)
			       (elmo-message-accessible-p
				wl-summary-buffer-elmo-folder
				(wl-summary-message-number)))))))
    (if (if downward (eobp) (and (bobp) skip)) (setq goto-next t))
    (if (or (eobp) (and (bobp) skip))
	(goto-char start))
    (move-to-column column)

    (if (not goto-next)
	(if wl-summary-buffer-disp-msg
	    (wl-summary-redisplay))
      (if interactive
	  (cond
	   ((and (not downward) wl-summary-buffer-prev-folder-function)
	    (funcall wl-summary-buffer-prev-folder-function))
	   ((and downward wl-summary-buffer-next-folder-function)
	    (funcall wl-summary-buffer-next-folder-function))
	   (t
	    (when wl-auto-select-next
	      (setq next-entity
		    (if downward
			(wl-summary-get-next-folder)
		      (wl-summary-get-prev-folder)))
	      (if next-entity
		  (setq finfo (wl-folder-get-entity-info next-entity))))
	    (wl-ask-folder
	     '(lambda () (wl-summary-next-folder-or-exit next-entity))
	     (format
	      "No more messages. Type SPC to go to %s."
	      (wl-summary-entity-info-msg next-entity finfo)))))))))

(defun wl-summary-prev (&optional interactive)
  (interactive)
  (wl-summary-cursor-move-surface nil (or interactive (interactive-p))))

(defun wl-summary-next (&optional interactive)
  (interactive)
  (wl-summary-cursor-move-surface t (or interactive (interactive-p))))

(defun wl-summary-up (&optional interactive skip-no-unread)
  ""
  (interactive)
  (if wl-summary-move-direction-toggle
      (setq wl-summary-move-direction-downward nil))
  (if (wl-summary-cursor-up)
      (if wl-summary-buffer-disp-msg
	  (wl-summary-redisplay))
    (if (or interactive
	    (interactive-p))
	(if wl-summary-buffer-prev-folder-function
	    (funcall wl-summary-buffer-prev-folder-function)
	  (let (next-entity finfo)
	    (when wl-auto-select-next
	      (progn
		(setq next-entity (wl-summary-get-prev-unread-folder))
		(if next-entity
		    (setq finfo (wl-folder-get-entity-info next-entity)))))
	    (if (and skip-no-unread
		     (eq wl-auto-select-next 'skip-no-unread))
		(wl-summary-next-folder-or-exit next-entity t)
	      (wl-ask-folder
	       '(lambda () (wl-summary-next-folder-or-exit next-entity t))
	       (format
		"No more unread messages. Type SPC to go to %s."
		(wl-summary-entity-info-msg next-entity finfo)))))))))

(defun wl-summary-get-prev-folder ()
  (let ((folder-buf (get-buffer wl-folder-buffer-name)))
    (when folder-buf
      (wl-folder-get-prev-folder
       (with-current-buffer folder-buf
	 wl-folder-buffer-cur-entity-id)))))

(defun wl-summary-get-next-folder ()
  (let ((folder-buf (get-buffer wl-folder-buffer-name)))
    (when folder-buf
      (wl-folder-get-next-folder
       (with-current-buffer folder-buf
	 wl-folder-buffer-cur-entity-id)))))

(defun wl-summary-get-next-unread-folder ()
  (let ((folder-buf (get-buffer wl-folder-buffer-name)))
    (when folder-buf
      (wl-folder-get-next-folder
       (with-current-buffer folder-buf
	 wl-folder-buffer-cur-entity-id)
       'unread))))

(defun wl-summary-get-prev-unread-folder ()
  (let ((folder-buf (get-buffer wl-folder-buffer-name)))
    (when folder-buf
      (wl-folder-get-prev-folder
       (with-current-buffer folder-buf
	 wl-folder-buffer-cur-entity-id)
       'unread))))

(defun wl-summary-down (&optional interactive skip-no-unread)
  (interactive)
  (if wl-summary-move-direction-toggle
      (setq wl-summary-move-direction-downward t))
  (if (wl-summary-cursor-down)
      (if wl-summary-buffer-disp-msg
	  (wl-summary-redisplay))
    (if (or interactive
	    (interactive-p))
	(if wl-summary-buffer-next-folder-function
	    (funcall wl-summary-buffer-next-folder-function)
	  (let (next-entity finfo)
	    (when wl-auto-select-next
	      (setq next-entity (wl-summary-get-next-unread-folder)))
	    (if next-entity
		(setq finfo (wl-folder-get-entity-info next-entity)))
	    (if (and skip-no-unread
		     (eq wl-auto-select-next 'skip-no-unread))
		(wl-summary-next-folder-or-exit next-entity)
	      (wl-ask-folder
	       '(lambda () (wl-summary-next-folder-or-exit next-entity))
	       (format
		"No more unread messages. Type SPC to go to %s."
		(wl-summary-entity-info-msg next-entity finfo)))))))))

(defun wl-summary-pop-to-last-message ()
  "Jump to last displayed message, and pop a new massage off the ring."
  (interactive)
  (let ((number (wl-summary-pop-message (wl-summary-message-number))))
    (unless number
      (error "Empty message ring"))
    (wl-summary-jump-to-msg number)
    (when wl-summary-buffer-disp-msg
      (let (wl-summary-buffer-message-ring)
	(wl-summary-redisplay)))))

(defun wl-summary-goto-last-displayed-msg (&optional arg)
  "Jump to last displayed message."
  (interactive "P")
  (cond
   ((eq last-command 'wl-summary-pop-to-last-message)
    (setq this-command 'wl-summary-pop-to-last-message)
    (wl-summary-pop-to-last-message))
   (arg
    (setq this-command 'wl-summary-pop-to-last-message)
    (wl-summary-pop-to-last-message))
   (t
    (let ((current (wl-summary-message-number))
	  (number (wl-summary-pop-message)))
      (if number
	  (progn
	    (wl-summary-jump-to-msg number)
	    (if wl-summary-buffer-disp-msg
		(wl-summary-redisplay)
	      (wl-summary-push-message current)))
	(message "No last message."))))))

(defun wl-summary-message-display-type ()
  (when (and wl-summary-buffer-disp-msg
	     (buffer-live-p wl-message-buffer)
	     wl-summary-buffer-current-msg
	     (wl-summary-message-number)
	     (= (wl-summary-message-number) wl-summary-buffer-current-msg))
    (wl-message-buffer-display-type wl-message-buffer)))

(defun wl-summary-buffer-display-mime-mode ()
  (or (wl-message-display-type-property (wl-summary-message-display-type)
					:mime)
      wl-summary-buffer-display-mime-mode))

(defun wl-summary-buffer-display-header-mode ()
  (or (wl-message-display-type-property (wl-summary-message-display-type)
					:header)
      wl-summary-buffer-display-header-mode))

(defun wl-summary-toggle-mime (&optional arg)
  "Toggle MIME decoding.
If ARG is non-nil, ask coding-system to display the message in the current
MIME analysis mode.

If ARG is numeric number, decode message as following:
1: Enable MIME analysis.
2: Enable MIME analysis only for headers.
3: Disable MIME analysis."
  (interactive "P")
  (let ((mime-mode (wl-summary-buffer-display-mime-mode))
	(elmo-mime-display-as-is-coding-system
	 elmo-mime-display-as-is-coding-system))
    (if (and (consp arg) (> (prefix-numeric-value arg) 4))
	(progn
	  (setq wl-summary-buffer-display-mime-mode mime-mode)
	  (wl-summary-update-modeline))
      (cond
       ((numberp arg)
	(setq mime-mode (case arg
			  (1 'mime)
			  (2 'header-only)
			  (3 'as-is)
;;;			  (4 'decode-only)
			  (5 'no-merge))))
       (arg
	;; Specify coding-system (doesn't change the MIME mode).
	(setq elmo-mime-display-as-is-coding-system
	      (if (and arg
		       (not (wl-message-mime-analysis-p
			     (wl-summary-message-display-type))))
		  (or (read-coding-system "Coding system: ")
		      elmo-mime-display-as-is-coding-system)
		elmo-mime-display-as-is-coding-system)))
       (t
	;; Change the MIME mode.
	(setq mime-mode (or (cadr (memq mime-mode
					wl-summary-display-mime-mode-list))
			    (car wl-summary-display-mime-mode-list)))))
      (wl-summary-redisplay-internal nil nil arg mime-mode))
    (message "MIME decoding: %s%s"
	     (upcase (symbol-name mime-mode))
	     (if (and (not (eq mime-mode 'mime))
		      (not (eq elmo-mime-display-as-is-coding-system
			       wl-cs-autoconv)))
		 (concat " ("
			 (symbol-name elmo-mime-display-as-is-coding-system)
			 ")")
	       ""))))

(defun wl-summary-redisplay (&optional arg)
  "Redisplay message."
  (interactive "P")
  (apply #'wl-summary-redisplay-internal nil nil arg
	 (unless (and (consp arg) (> (prefix-numeric-value arg) 4))
	   (list wl-summary-buffer-display-mime-mode
		 wl-summary-buffer-display-header-mode))))

(defun wl-summary-toggle-all-header (&optional arg)
  "Toggle displaying message with all header."
  (interactive "P")
  (let ((header-mode (wl-summary-buffer-display-header-mode)))
    (if (and (consp arg) (> (prefix-numeric-value arg) 4))
	(setq wl-summary-buffer-display-header-mode header-mode)
      (wl-summary-redisplay-internal
       nil nil arg nil
       (if (eq header-mode 'all) 'partial 'all)))))

(defun wl-summary-redisplay-internal (&optional folder number force-reload
						mime-mode header-mode)
  (let* ((folder (or folder wl-summary-buffer-elmo-folder))
	 (num (or number (wl-summary-message-number)))
	 (wl-mime-charset      wl-summary-buffer-mime-charset)
	 (default-mime-charset wl-summary-buffer-mime-charset)
	 fld-buf fld-win thr-entity
	 (elmo-message-fetch-confirm (or elmo-message-fetch-confirm
					 (and force-reload
					      elmo-message-fetch-threshold))))
    (if (and wl-thread-open-reading-thread
	     (eq wl-summary-buffer-view 'thread)
	     (not (wl-thread-entity-get-opened
		   (setq thr-entity (wl-thread-get-entity
				     num))))
	     (wl-thread-entity-get-children thr-entity))
	(wl-thread-force-open))
    (if num
	(progn
	  (setq wl-summary-buffer-disp-msg t)
	  (wl-summary-push-message wl-summary-buffer-current-msg)
	  ;; hide folder window
	  (if (and (not wl-stay-folder-window)
		   (setq fld-buf (get-buffer wl-folder-buffer-name)))
	      (if (setq fld-win (get-buffer-window fld-buf))
		  (delete-window fld-win)))
	  (setq wl-current-summary-buffer (current-buffer))
	  (wl-message-redisplay folder num
				(wl-message-make-display-type
				 (or mime-mode
				     (wl-summary-buffer-display-mime-mode))
				 (or header-mode
				     (wl-summary-buffer-display-header-mode)))
				(or force-reload
				    (string= (elmo-folder-name-internal folder)
					     wl-draft-folder)))
	  (when (elmo-message-use-cache-p folder num)
	    (elmo-message-set-cached folder num t))
	  (ignore-errors
	    (if (elmo-message-flagged-p wl-summary-buffer-elmo-folder
					num
					'unread)
		(wl-summary-mark-as-read num)
	      (wl-summary-count-unread)
	      (wl-summary-update-modeline)
	      (wl-folder-update-unread
	       (wl-summary-buffer-folder-name)
	       wl-summary-buffer-unread-count)))
	  (setq wl-summary-buffer-current-msg num)
	  (when wl-summary-recenter
	    (recenter (/ (- (window-height) 2) 2))
	    (if (not wl-summary-indent-length-limit)
		(wl-horizontal-recenter)))
	  (wl-highlight-summary-displaying)
	  (wl-message-buffer-prefetch-next
	   folder num
	   (min (or wl-message-buffer-prefetch-depth 0)
		(1- wl-message-buffer-cache-size))
	   (current-buffer)
	   wl-summary-buffer-mime-charset)
	  (run-hooks 'wl-summary-redisplay-hook))
      (message "No message to display."))))

(defun wl-summary-jump-to-current-message ()
  "Jump into Message buffer."
  (interactive)
  (let (message-buf message-win)
    (if (setq message-buf wl-message-buffer)
	(if (setq message-win (get-buffer-window message-buf))
	    (select-window message-win)
	  (wl-message-select-buffer wl-message-buffer))
      (wl-summary-redisplay)
      (wl-message-select-buffer wl-message-buffer))))

(defun wl-summary-cancel-message ()
  "Cancel an article on news."
  (interactive)
  (if (null (wl-summary-message-number))
      (message "No message.")
    (let ((summary-buf (current-buffer))
	  message-buf)
      (wl-summary-set-message-buffer-or-redisplay)
      (if (setq message-buf (wl-message-get-original-buffer))
	  (set-buffer message-buf))
      (unless (wl-message-news-p)
	(set-buffer summary-buf)
	(if (and (eq (elmo-folder-type-internal wl-summary-buffer-elmo-folder)
		     'nntp)
		 (y-or-n-p "Cannot get Newsgroups. Fetch again? "))
	    (progn
	      (wl-summary-redisplay t)
	      (wl-summary-supersedes-message))
	  (error "This is not a news article; supersedes is impossible")))
      (when (yes-or-no-p "Do you really want to cancel this article? ")
	(let (from newsgroups message-id distribution buf)
	  (save-excursion
	    (setq from (std11-field-body "from")
		  newsgroups (std11-field-body "newsgroups")
		  message-id (elmo-get-message-id-from-buffer)
		  distribution (std11-field-body "distribution"))
	    ;; Make sure that this article was written by the user.
	    (unless (wl-address-user-mail-address-p
		     (wl-address-header-extract-address
		      (car (wl-parse-addresses from))))
	      (error "This article is not yours"))
	    ;; Make control message.
	    (setq buf (set-buffer (get-buffer-create " *message cancel*")))
	    (setq wl-draft-buffer-cur-summary-buffer summary-buf)
	    (buffer-disable-undo (current-buffer))
	    (erase-buffer)
	    (insert "Newsgroups: " newsgroups "\n"
		    "From: " (wl-address-header-extract-address
			      wl-from) "\n"
			      "Subject: cmsg cancel " message-id "\n"
			      "Control: cancel " message-id "\n"
			      (if distribution
				  (concat "Distribution: " distribution "\n")
				"")
			      mail-header-separator "\n"
			      wl-summary-cancel-message)
	    (message "Canceling your message...")
	    (wl-draft-raw-send t t) ; kill when done, force-pre-hooks.
	    (message "Canceling your message...done")))))))

(defun wl-summary-supersedes-message ()
  "Supersede current message."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  (let ((summary-buf (current-buffer))
	message-buf from)
    (wl-summary-set-message-buffer-or-redisplay)
    (if (setq message-buf (wl-message-get-original-buffer))
	(set-buffer message-buf))
    (unless (wl-message-news-p)
      (set-buffer summary-buf)
      (if (and (eq (elmo-folder-type-internal wl-summary-buffer-elmo-folder)
		   'nntp)
	       (y-or-n-p "Cannot get Newsgroups. Fetch again? "))
	  (progn
	    (wl-summary-redisplay t)
	    (wl-summary-supersedes-message))
	(error "This is not a news article; supersedes is impossible")))
    (save-excursion
      (setq from (std11-field-body "from"))
      ;; Make sure that this article was written by the user.
      (unless (wl-address-user-mail-address-p
	       (wl-address-header-extract-address
		(car (wl-parse-addresses from))))
	(error "This article is not yours"))
      (let* ((message-id (elmo-get-message-id-from-buffer))
	     (followup-to (std11-field-body "followup-to"))
	     (mail-default-headers
	      (concat mail-default-headers
		      "Supersedes: " message-id "\n"
		      (and followup-to
			   (concat "Followup-To: " followup-to "\n")))))
	(if message-buf (set-buffer message-buf))
	(wl-draft-edit-string (buffer-substring (point-min) (point-max)))))))

(defun wl-summary-display-raw (&optional arg)
  "Display current message in raw format."
  (interactive)
  (let ((number (wl-summary-message-number))
	(folder wl-summary-buffer-elmo-folder))
    (if number
	(let ((raw (elmo-message-fetch-string
		    folder number
		    (elmo-find-fetch-strategy folder number)))
	      (raw-buffer (get-buffer-create "*wl:raw message*"))
	      (raw-mode-map (make-sparse-keymap)))
	  (with-current-buffer raw-buffer
	    (toggle-read-only -1)
	    (erase-buffer)
	    (princ raw raw-buffer)
	    (toggle-read-only t)
	    (goto-char (point-min))
	    (switch-to-buffer-other-window raw-buffer)
	    (define-key raw-mode-map "l" 'toggle-truncate-lines)
	    (define-key raw-mode-map "q" 'kill-buffer-and-window)
	    (define-key raw-mode-map "," 'kill-buffer-and-window)
	    (use-local-map raw-mode-map)))
      (message "No message to display."))
    number))

(defun wl-summary-save (&optional arg wl-save-dir)
  "Save current message to disk."
  (interactive)
  (let ((filename)
	(num (wl-summary-message-number)))
    (unless wl-save-dir
      (setq wl-save-dir wl-temporary-file-directory))
    (if num
	(save-excursion
	  (setq filename (concat (number-to-string num) wl-summary-save-file-suffix))
	  (if (or (null arg)
                  (file-exists-p (expand-file-name filename wl-save-dir)))
              (setq filename (expand-file-name (read-file-name "Save to file: " wl-save-dir nil nil filename)))
            (setq filename (expand-file-name filename wl-save-dir)))
	  (wl-summary-set-message-buffer-or-redisplay)
	  (set-buffer (wl-message-get-original-buffer))
	  (when (or arg
		    (not (file-exists-p filename))
		    (y-or-n-p "File already exists.  override it? "))
	    (write-region-as-binary (point-min) (point-max) filename)))
      (message "No message to save."))
    num))

(defun wl-summary-save-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (wl-summary-narrow-to-region beg end)
      (goto-char (point-min))
      (let ((wl-save-dir
	     (wl-read-directory-name "Save to directory: "
				     wl-temporary-file-directory)))
	(if (null (file-exists-p wl-save-dir))
	    (make-directory wl-save-dir))
	(if (eq wl-summary-buffer-view 'thread)
	    (progn
	      (while (not (eobp))
		(let* ((number (wl-summary-message-number))
		       (entity (wl-thread-get-entity number)))
		  (if (wl-thread-entity-get-opened entity)
		      (wl-summary-save t wl-save-dir)
		    ;; closed
		    (wl-summary-save t wl-save-dir))
		  (forward-line))))
	  (while (not (eobp))
	    (wl-summary-save t wl-save-dir)
	    (forward-line)))))))

;; mew-summary-pipe-message()
(defun wl-summary-pipe-message (prefix command)
  "Send this message via pipe."
  (interactive (list current-prefix-arg nil))
  (if (null (wl-summary-message-number))
      (message "No message.")
    (setq command (wl-read-shell-command "Shell command on message: "
					 wl-summary-shell-command-last))
    (if (y-or-n-p "Send this message to pipe? ")
	(wl-summary-pipe-message-subr prefix command))))

(defun wl-summary-target-mark-pipe (prefix command)
  "Send each marked messages via pipe."
  (interactive (list current-prefix-arg nil))
  (if (null wl-summary-buffer-target-mark-list)
      (message "No marked message.")
    (setq command (wl-read-shell-command
		   "Shell command on each marked message: "
		   wl-summary-shell-command-last))
    (when (y-or-n-p "Send each marked message to pipe? ")
      (while (car wl-summary-buffer-target-mark-list)
	(let ((num (car wl-summary-buffer-target-mark-list)))
	  (wl-thread-jump-to-msg num)
	  (wl-summary-pipe-message-subr prefix command)
	  (wl-summary-unmark))))))

(defun wl-summary-pipe-message-subr (prefix command)
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (set-buffer (wl-message-get-original-buffer))
    (if (string= command "")
	(setq command wl-summary-shell-command-last))
    (goto-char (point-min)) ; perhaps this line won't be necessary
    (if prefix
	(search-forward "\n\n"))
    (shell-command-on-region (point) (point-max) command nil)
    (setq wl-summary-shell-command-last command)))

(defun wl-summary-print-message (&optional arg)
  (interactive "P")
  (if (null (wl-summary-message-number))
      (message "No message.")
    (save-excursion
      (wl-summary-set-message-buffer-or-redisplay)
      (if (or (not (interactive-p))
	      (y-or-n-p "Print ok? "))
	  (progn
	    (let ((buffer (generate-new-buffer " *print*")))
	      (copy-to-buffer buffer (point-min) (point-max))
	      (set-buffer buffer)
	      (funcall wl-print-buffer-function)
	      (kill-buffer buffer)))
	(message "")))))

(defun wl-summary-print-message-with-ps-print (&optional filename)
  "Print message via ps-print."
  (interactive)
  (if (null (wl-summary-message-number))
      (message "No message.")
    (setq filename (ps-print-preprint current-prefix-arg))
    (if (or (not (interactive-p))
	    (y-or-n-p "Print ok? "))
	(let ((summary-buffer (current-buffer))
	      wl-break-pages)
	  (save-excursion
	    (wl-summary-set-message-buffer-or-redisplay)
	    (let* ((buffer (generate-new-buffer " *print*"))
		   (entity (progn
			     (set-buffer summary-buffer)
			     (elmo-message-entity
			      wl-summary-buffer-elmo-folder
			      (wl-summary-message-number))))
		   (wl-ps-subject
		    (or (elmo-message-entity-field entity 'subject 'string)
			""))
		   (wl-ps-from
		    (or (elmo-message-entity-field entity 'from 'string)
			""))
		   (wl-ps-date
		    (or (elmo-message-entity-field entity 'date 'string)
			"")))
	      (run-hooks 'wl-ps-preprint-hook)
	      (set-buffer wl-message-buffer)
	      (copy-to-buffer buffer (point-min) (point-max))
	      (set-buffer buffer)
	      (unwind-protect
		  (let ((ps-left-header
			 (list (concat "(" wl-ps-subject ")")
			       (concat "(" wl-ps-from ")")))
			(ps-right-header
			 (list "/pagenumberstring load"
			       (concat "(" wl-ps-date ")"))))
		    (run-hooks 'wl-ps-print-hook)
		    (funcall wl-ps-print-buffer-function filename))
		(kill-buffer buffer)))))
      (message ""))))

(if (featurep 'ps-print) ; ps-print is available.
    (fset 'wl-summary-print-message 'wl-summary-print-message-with-ps-print))

(defun wl-summary-target-mark-print ()
  (interactive)
  (wl-summary-check-target-mark)
  (when (y-or-n-p "Print all marked messages. OK? ")
    (while (car wl-summary-buffer-target-mark-list)
      (let ((num (car wl-summary-buffer-target-mark-list)))
	(wl-thread-jump-to-msg num)
	(wl-summary-print-message)
	(wl-summary-unmark)))))

(defun wl-summary-folder-info-update ()
  (wl-folder-set-folder-updated
   (elmo-string (wl-summary-buffer-folder-name))
   (list 0
	 wl-summary-buffer-unread-count
	 (elmo-folder-length
	  wl-summary-buffer-elmo-folder))))

(defun wl-summary-get-original-buffer ()
  "Get original buffer for the current summary."
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (wl-message-get-original-buffer)))

(defun wl-summary-pack-number (&optional arg)
  (interactive "P")
  (elmo-folder-pack-numbers wl-summary-buffer-elmo-folder)
  (let (wl-use-scoring)
    (wl-summary-rescan nil nil nil t)))

(defun wl-summary-target-mark-uudecode ()
  (interactive)
  (wl-summary-check-target-mark)
  (let ((mlist (reverse wl-summary-buffer-target-mark-list))
	(summary-buf (current-buffer))
	(tmp-buf (get-buffer-create "*WL UUENCODE*"))
	orig-buf i k filename rc errmsg)
    (setq i 1)
    (setq k (length mlist))
    (set-buffer tmp-buf)
    (erase-buffer)
    (save-window-excursion
      (while mlist
	(set-buffer summary-buf)
	(wl-summary-jump-to-msg (car mlist))
	(wl-summary-redisplay)
	(set-buffer (setq orig-buf (wl-summary-get-original-buffer)))
	(goto-char (point-min))
	(cond ((= i 1) ; first
	       (if (setq filename (wl-message-uu-substring
				   orig-buf tmp-buf t
				   (= i k)))
		   nil
		 (error "Can't find begin line")))
	      ((< i k)
	       (wl-message-uu-substring orig-buf tmp-buf))
	      (t ; last
	       (wl-message-uu-substring orig-buf tmp-buf nil t)))
	(setq i (1+ i))
	(setq mlist (cdr mlist)))
      (set-buffer tmp-buf)
      (message "Exec %s..." wl-prog-uudecode)
      (unwind-protect
	  (let ((decode-dir wl-temporary-file-directory))
	    (if (not wl-prog-uudecode-no-stdout-option)
		(setq filename (expand-file-name (read-file-name "Save to file: " wl-temporary-file-directory nil nil (elmo-safe-filename))))
	      (setq decode-dir
		    (wl-read-directory-name "Save to directory: "
					    wl-temporary-file-directory))
	      (setq filename (expand-file-name filename decode-dir)))
	    (if (file-exists-p filename)
		(or (yes-or-no-p (format "File %s exists. Save anyway? "
					 filename))
		    (error "")))
	    (elmo-bind-directory
	     decode-dir
	     (setq rc
		   (as-binary-process
		    (apply 'call-process-region (point-min) (point-max)
			   wl-prog-uudecode t (current-buffer) nil
			   wl-prog-uudecode-arg))))
	    (when (not (= 0 rc))
	      (setq errmsg (buffer-substring (point-min)(point-max)))
	      (error "Uudecode error: %s" errmsg))
	    (if (not wl-prog-uudecode-no-stdout-option)
		(let (file-name-handler-alist) ;; void jka-compr
		  (as-binary-output-file
		   (write-region (point-min) (point-max)
				 filename nil 'no-msg))))
	    (with-current-buffer summary-buf
	      (wl-summary-delete-all-target-marks))
	    (if (file-exists-p filename)
		(message "Saved as %s" filename)))
	(kill-buffer tmp-buf)))))

;;; Someday
;;;(defun wl-summary-drop-unsync ()
;;;  "Drop all unsync messages."
;;;  (interactive)
;;;  (if (elmo-folder-pipe-p (wl-summary-buffer-folder-name))
;;;      (error "You cannot drop unsync messages in this folder"))
;;;  (if (or (not (interactive-p))
;;;	  (y-or-n-p "Drop all unsync messages? "))
;;;      (let* ((folder-list (elmo-folder-get-primitive-folder-list
;;;			   (wl-summary-buffer-folder-name)))
;;;	     (is-multi (elmo-multi-p (wl-summary-buffer-folder-name)))
;;;	     (sum 0)
;;;	     (multi-num 0)
;;;	     pair)
;;;	(message "Dropping...")
;;;	(while folder-list
;;;	  (setq pair (elmo-folder-message-numbers (car folder-list)))
;;;	  (when is-multi ;; dirty hack...
;;;	    (incf multi-num)
;;;	    (setcar pair (+ (* multi-num elmo-multi-divide-number)
;;;			    (car pair))))
;;;	  (elmo-msgdb-set-number-alist
;;;	   (wl-summary-buffer-msgdb)
;;;	   (nconc
;;;	    (elmo-msgdb-get-number-alist (wl-summary-buffer-msgdb))
;;;	    (list (cons (car pair) nil))))
;;;	  (setq sum (+ sum (cdr pair)))
;;;	  (setq folder-list (cdr folder-list)))
;;;	(wl-summary-set-message-modified)
;;;	(wl-folder-set-folder-updated (wl-summary-buffer-folder-name)
;;;				      (list 0
;;;					    (+ wl-summary-buffer-unread-count
;;;					       wl-summary-buffer-new-count)
;;;					    sum))
;;;	(message "Dropping...done"))))

(defun wl-summary-previous-message-number (msg)
  "Return a message number previous to the message specified by MSG."
  (let ((list wl-summary-buffer-number-list)
	previous)
    (while (and list (not (eq msg (car list))))
      (setq previous (car list))
      (setq list (cdr list)))
    previous))

(defun wl-summary-next-message-number (msg)
  "Return a message number next to the message specified by MSG."
  (cadr (memq msg wl-summary-buffer-number-list)))

(defun wl-summary-default-get-next-msg (msg)
  (or (wl-summary-next-message msg
			       (if wl-summary-move-direction-downward 'down
				 'up)
			       nil)
      (if wl-summary-move-direction-downward
	  (wl-summary-next-message-number msg)
	(wl-summary-previous-message-number msg))))

(defun wl-summary-save-current-message ()
  "Save current message for `wl-summary-yank-saved-message'."
  (interactive)
  (let ((number (wl-summary-message-number)))
    (setq wl-summary-buffer-saved-message number)
    (and number (message "No: %s is saved." number))))

(defun wl-summary-yank-saved-message ()
  "Set current message as a parent of the saved message."
  (interactive)
  (if wl-summary-buffer-saved-message
      (let ((number (wl-summary-message-number)))
	(if (eq wl-summary-buffer-saved-message number)
	    (message "Cannot set itself as a parent.")
	  (save-excursion
	    (wl-thread-jump-to-msg wl-summary-buffer-saved-message)
	    (wl-thread-set-parent number)
	    (wl-summary-set-thread-modified))
	  (setq  wl-summary-buffer-saved-message nil)))
    (message "There's no saved message.")))

(defun wl-summary-toggle-header-narrowing ()
  "Toggle message header narrowing."
  (interactive)
  (when wl-message-use-header-narrowing
    (save-selected-window
      (let* ((mbuf wl-message-buffer)
	     (mwin (when mbuf (get-buffer-window mbuf)))
	     (wpos (when mwin (window-start mwin))))
	(when mbuf
	  (set-buffer mbuf)
	  (wl-message-header-narrowing-toggle)
	  (and wpos (set-window-start mwin wpos)))))))

(defun wl-summary-toggle-mime-buttons ()
  "Toggle visibility of mime buttons."
  (interactive)
  (customize-set-value 'mime-view-buttons-visible (not mime-view-buttons-visible))
  (wl-message-buffer-cache-clean-up)
  (wl-summary-redisplay))

(require 'product)
(product-provide (provide 'wl-summary) (require 'wl-version))

;;; wl-summary.el ends here
