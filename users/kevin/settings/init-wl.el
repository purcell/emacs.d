;;; init-wl.el --- Configuration file for the Wanderlust Email client
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Thu Jul 17 09:49:48 2008 (+0100)
;; Version: 0.1
;; Last-updated: Sun Apr 12 15:41:18 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 3
;; URL: http://www.emacswiki.org/emacs/hgw-init-wl.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; This is my complete configuration file for Wanderlust, the excellent Email
;; client, which has been cobbled together from the examples and documentation
;; supplied with Wanderlust, reading the source-code, and scanning the mailing
;; lists wl-en@lists.airs.net, wl@lists.airs.net and of course from the web.
;; Mailing list postings by Ron Isaacson, Vitaly Mayatskikh, Yoichi NAKAYAMA
;; have been particularly useful as have configuration files posted on the web
;; by Jared Rhine, Angus Lees and YAMASHITA Junji.
;;
;; ----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;;
;; ----------------------------------------------------------------------------
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ----------------------------------------------------------------------------
;;
;;; Code:

;; ----------------------------------------------------------------------------
;;; Autoload Wanderlust on command "wl"

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." nil)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose with Wanderlust." t)


;; default setting for wanderlust
(setq
 wl-forward-subject-prefix "Fwd: " )    ;; use "Fwd: " not "Forward: "


;; Header From:
;;(setq wl-from "Lu Jianmei <lu.jianmei@trs.com.cn>")

;; If (system-name) does not return FQDN,
;; set following as a local domain name without hostname.
;;(setq wl-local-domain "trs.com.cn")

;;(setq wl-subscribed-mailing-list t)
;;(setq wl-draft-delete-myself-from-cc t)
;; Mail-Followup-To:’ field is automatically inserted in the draft buffer
;; (setq wl-insert-mail-followup-to t)
;; ‘Mail-Reply-To:’ field is automatically inserted in the draft buffer.
(setq wl-insert-mail-reply-to t)

;; The initial setting is full. Style of draft buffer window (except for replying and forwarding). keep is to use current window, full is to use full frame window, split is to split current window and use it.
;;(setq wl-draft-buffer-style full)
;;(setq wl-draft-reply-buffer-style full)
;; Open draft in a frame
;;(setq wl-draft-use-frame t)


;; suggested by Masaru Nomiya on the WL mailing list
;;It's not uncommon to forget
;;to add a subject or an attachment when you send a mail (or at least, when I
;;send a mail…). However, using wl-mail-send-pre-hook we can let Wanderlust warn
;;us when something like that happens.
(defun djcb-wl-draft-subject-check ()
  "check whether the message has a subject before sending"
  (if (and (< (length (std11-field-body "Subject")) 1)
           (null (y-or-n-p "No subject! Send current draft?")))
      (error "Abort.")))


;; note, this check could cause some false positives; anyway, better
;; safe than sorry...
(defun djcb-wl-draft-attachment-check ()
  "if attachment is mention but none included, warn the the user"
  (save-excursion
    (goto-char 0)
    (unless ;; don't we have an attachment?

        (re-search-forward "^Content-Disposition: attachment" nil t)
      (when ;; no attachment; did we mention an attachment?
          (re-search-forward "attach" nil t)
        (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
          (error "Abort."))))))

(add-hook 'wl-mail-send-pre-hook 'djcb-wl-draft-subject-check)
(add-hook 'wl-mail-send-pre-hook 'djcb-wl-draft-attachment-check)

;; [[ SEMI Setting ]]

;; Disable inline display of HTML part.
;; Put before load `mime-setup'
(setq mime-setup-enable-inline-html nil)

;; Don't split large message.
(setq mime-edit-split-message nil)

;; If lines of message are larger than this value, treat it as `large'.
(setq mime-edit-message-default-max-lines 1000)
(require 'signature)


;; --------------------------
;; comment because there is no such metamail package of mimencode
;; solve some encode issue
;;(setq mel-b-ccl-module nil)
;;(setq mel-q-ccl-module nil)
;;(setq base64-external-encoder '("mimencode"))
;;(setq base64-external-decoder '("mimencode" "-u"))
;;(setq base64-external-decoder-option-to-specify-file '("-o"))
;;(setq quoted-printable-external-encoder '("mimencode" "-q"))
;;(setq quoted-printable-external-decoder '("mimencode" "-q" "-u"))
;;(setq quoted-printable-external-decoder-option-to-specify-file '("-o"))
;;(setq base64-internal-decoding-limit 0)
;;(setq base64-internal-encoding-limit 0)
;;(setq quoted-printable-internal-decoding-limit 0)
;;(setq quoted-printable-internal-encoding-limit 0)

;;(setq-default mime-transfer-level 8)
;;(setq mime-header-accept-quoted-encoded-words t)



;; ----------------------------------------------------------------------------
;;; w3m octet configuration for handling attachments

(require 'mime-w3m)
(require 'octet)
(require 'w3m-util)
(octet-mime-setup)

;;------------------------
;;  when a new e-mail has arrived, or when there's a meeting in 15 minutes you should attend.
;;  Pop-up a warning
(defun djcb-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
               (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "

                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text only version

    (message (concat title ": " msg))))


(add-hook 'wl-biff-notify-hook
          (lambda()
            (djcb-popup "Wanderlust" "You have new mail!"
                        "/usr/share/icons/gnome/32x32/status/mail-unread-new.png"
                        "/usr/share/sounds/freedesktop/stereo/phone-incoming-call.oga")))

;;----------------
;;LOADING dired-dd, you can check the
;; http://www.asahi-net.or.jp/~pi9s-nnb/dired-dd-home.html
;;----------------
;;Include something like

;;(add-hook
;; 'dired-load-hook
;; (function
;;  (lambda ()
;;    (load "dired-x")
;;    ;; Set dired-x variables here.
;;    ;; To and flo...
;;    (if window-system (require 'dired-dd)))))

;; ----------------------------------------------------------------------------
;;; Basic configuration

(setq user-full-name "Lu Jianmei")
(setq user-mail-address "lu.jianmei@trs.com.cn")

;; Example: imap4.exaple.org as main IMAP server
(setq elmo-imap4-default-user "lu.jianmei@trs.com.cn")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-server  "imap.qiye.163.com")
(setq elmo-imap4-default-port 143)
;;(setq elmo-imap4-default-stream-type 'ssl)


(setq wl-icon-directory "~/.emacs.d/site-lisp/wl/etc/icons"
      elmo-msgdb-directory "~/mails/Wanderlust/Elmo"
      elmo-split-log-file "~/mails/Wanderlust/Elmo/split-log"

      ;; config passwd for imap, need to use M-x call function: "elmo-passwd-alist-name" after login&sent a mail
      ;; then wl will encry your password and save into this file for cache.
      ;; the file's content would be like this:
      ;; (("IMAP:user/clear@imap_server:143" . "passwd encrypt") ("SMTP:user/PLAIN@smtp_server" . "passwd encrypted"))
      ;; eg. (("IMAP:lu.jianmei@trs.com.cn/clear@imap.qiye.163.com:143" . "passwd encrypt") ("SMTP:lu.jianmei@trs.com.cn/PLAIN@smtp.qiye.163.com" . "passwd encrypted"))

      elmo-passwd-alist-file-name "~/mails/passwd"
      elmo-passwd-life-time 100000

      ;; Offline and synchronization
      wl-plugged t
      elmo-imap4-use-modified-utf7 t
      elmo-imap4-use-cache t
      elmo-nntp-use-cache t
      elmo-pop3-use-cache t
      wl-ask-range nil

      ;; do not need confirm when fetch
      elmo-message-fetch-confirm nil
      elmo-message-fetch-threshold 250000
      ;;elmo-network-session-idle-timeout 30

      wl-fcc ".Sent"
      wl-fcc-force-as-read t
      wl-from (concat user-full-name " <" user-mail-address ">")
      ;;      wl-from (concat user-full-name "< lu.jianmei@trs.com.cn > ")
      wl-organization "TRS Ltd."

      ;; Automatic signature insertion
      signature-file-name "~/mails/Signatures/TRSAddress"
      signature-insert-at-eof t
      signature-delete-blank-lines-at-eof t

      ;; User Email addresses
      wl-user-mail-address-list t

      wl-draft-reply-buffer-style 'keep
      wl-interactive-send nil
      wl-interactive-exit nil

      ;; Windows and decoration
      wl-folder-use-frame t
      wl-highlight-body-too t
      wl-use-highlight-mouse-line nil
      wl-show-plug-status-on-modeline t
      wl-message-window-size '(1 . 4)
      )

;; Set mail-icon to be shown universally in the modeline.
(setq global-mode-string
      (cons
       '(wl-modeline-biff-status
         wl-modeline-biff-state-on
         wl-modeline-biff-state-off)
       global-mode-string))

;; Use wanderlust for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; ----------------------------------------------------------------------------
;;; Folders

(setq wl-stay-folder-window t
      wl-folder-window-width 30
      wl-folder-desktop-name "Email"
      wl-default-folder "%inbox"
      my-wl-default-filing-folder "~/mail/.wl/"
      wl-default-spec "~/mails/.wl/Customers/"
      wl-draft-folder ".Drafts"
      wl-trash-folder ".Trash"
      wl-interactive-save-folders nil

      ;;wl-dispose-folder-alist '(("^." . remove))
      wl-use-petname t
      wl-folder-petname-alist nil
      wl-fldmgr-make-backup  t
      wl-fldmgr-sort-group-first  t

      elmo-folder-update-confirm t
      elmo-folder-update-threshold 1000

      wl-folder-check-async  t
      wl-auto-check-folder-name 'none
      wl-auto-check-folder-list '("^\\.")
      wl-auto-uncheck-folder-list nil

      wl-folder-notify-deleted t
      wl-fldmgr-add-complete-with-current-folder-list t
      wl-folder-info-save t
      wl-folder-many-unsync-threshold  100
      wl-highlight-folder-by-numbers 1
      )

;; ----------------------------------------------------------------------------
;;; Local mail directory path

(setq my-maildir-path "~/mails"
      wl-folders-file (concat my-maildir-path "/folders")
      elmo-localdir-folder-path my-maildir-path
      elmo-maildir-folder-path my-maildir-path
      elmo-search-namazu-default-index-path my-maildir-path
      elmo-archive-folder-path my-maildir-path)

;; BUILD the folder tree automatically
;; Note: if you change the hierarchy and want to rebuild the tree do
;; rm -rf ~/Emacs/Wanderlust/Elmo/folder
(setq wl-folder-hierarchy-access-folders
      '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
    "^-[^.]*\\(:\\|@\\|$\\)"
    "^@$"
   "^'$"))

;; ----------------------------------------------------------------------------
;;; Summary

(setq wl-auto-select-next 'unread
      wl-summary-width nil
      wl-summary-weekday-name-lang "en"
      wl-summary-showto-folder-regexp ".Sent.*"
      ;;wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
      wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"

      ;; Summary threads
      wl-thread-insert-opened t
      wl-thread-open-reading-thread t
     )

;; ----------------------------------------------------------------------------
;;; Draft:

(setq wl-draft-config-alist
      '(
        ((string-match ".*@imap\\.qiye\\.163.com.*" wl-draft-parent-folder)
         ("From" . "lu jianmei <lu.jianmei@trs.com.cn>")
         ("Bcc" . "lu jianmei <lu.jianmei@trs.com.cn>")
         ("Organization" . nil)
         ("X-Attribution" . "lujianmei")
         (signature . "~/mails/Signatures/TRSAddress"))
        )

      wl-draft-reply-without-argument-list
      '(("Followup-To" .
         (("Mail-Followup-To" "Mail-Reply-To" "Reply-To") nil ("Followup-To")))
        ("Mail-Followup-To" .
         (("Mail-Followup-To") nil nil))
        ("Newsgroups" .
         (("Mail-Reply-To" "Reply-To" "To") ("Newsgroups")))
        ("Mail-Reply-To" .
         (("Mail-Reply-To" "Reply-To") nil nil))
        ("Reply-To" .
         (("Reply-To") nil nil))
        (wl-draft-self-reply-p . (("To") nil))
        ("From" . (("From") nil nil)))
      )

;;;  boxquote
;; provides a set of functions for using a text quoting style that
;; partially boxes in the left hand side of an area of text, such a marking
;; style might be used to show externally included text or example code.
;;
;; ,----
;; | The default style looks like this.
;; `----
;; using e.g. `boxquote-region'
;; `boxquote-unbox' removes the box
(require 'boxquote)

;; ----------------------------------------------------------------------------
;;; Message:

(setq mime-view-mailcap-files '("~/mails/Wanderlust/mailcap")
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^X-Attribution:"
        "^\\(Posted\\|Date\\):"
        )
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc"))

(eval-after-load "mime-view"
  '(progn
     (ctree-set-calist-strictly
      'mime-acting-condition
      '((mode . "play")
        (type . application)(subtype . pdf)
        (method . my-mime-save-content-find-file)))))

;; ----------------------------------------------------------------------------
;;; Biff: Check for new mail


;; You should set this variable if you use multiple e-mail addresses.
(setq wl-user-mail-address-list  '("lu.jianmei@trs.com.cn"))

(setq wl-biff-check-folder-list
      '("%INBOX: lu.jianmei/user@imap.qiye.163.com:143"
        "%TRS: lu.jianmei/user@imap.qiye.163.com:143"
        "%Haier: lu.jianmei/user@imap.qiye.163.com:143"
        "%PM: lu.jianmei/user@imap.qiye.163.com:143"
        ;;        "&anysky133/user@imap.163.com:143"
        ;;        "&xxxxxxx+enquiries/user@mail.plus.net:110!direct"
        ;;        "%inbox:hxxxxxx0/clear@imap.gmail.com:993!"
        ;;        "-gmane.emacs.cvs@news.gmane.org"
        ;;        "-gmane.emacs.devel@news.gmane.org"
        ;;        "-gmane.emacs.orgmode@news.gmane.org"
        ;;        "-gmane.emacs.emms.user@news.gmane.org"
        ;;        "-gmane.emacs.sources@news.gmane.org"
        ;;        "-gmane.mail.wanderlust.general@news.gmane.org"
        ;;        "-gmane.mail.wanderlust.general.japanese@news.gmane.org"
        ;;        "-gmane.comp.window-managers.stumpwm.devel@news.gmane.org"
        ;;        "-gmane.comp.mozilla.conkeror@news.gmane.org"
        )
      wl-biff-check-interval 10
      wl-biff-use-idle-timer t)

;; ----------------------------------------------------------------------------
;;; Refiling:
;; See also `bbdb-wl-refile-alist' and `wl-init-hook'

(defcustom wl-general-refile-rule-alist nil
  "General rule alist which may be extended to include the `From' `folder'
entries defined in the BBDB by `bbdb-wl-refile-alist'.
e.g.
'((\"From\"
   (\"teranisi@isl.ntt.co.jp\" . \"+teranisi\"))
  (\"x-ml-name\"
   (\"^Wanderlust\"    . \"+wl\")
   (\"^Elips\" . \"+elips\")))"
  :type '(repeat (list (string :tag "Field")
                       (repeat :inline t
                               (cons (regexp :tag "Value")
                                     (string :tag "Folder")))))
  :group 'wl-pref)

;; Set the default value of wl-refile-rule-alist
(setq wl-refile-rule-alist wl-general-refile-rule-alist)

;; ----------------------------------------------------------------------------
;;; Configure BBDB to manage Email addresses
(require 'bbdb)
(bbdb-initialize)

(require 'bbdb-wl)
(bbdb-wl-setup)

(setq bbdb-use-pop-up t ;; Allow pop-ups
      bbdb-pop-up-target-lines 1
      bbdb/mail-auto-create-p t ;; auto collection
      bbdb-wl-ignore-folder-regexp "^@" ;; folders without auto collection
      bbdb-north-american-phone-numbers-p nil
      bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))
      bbdb-dwim-net-address-allow-redundancy t
      bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

      bbdb-offer-save 1 ;; 1 means save-without-asking
      ;; shows the name of bbdb in the summary

      bbdb-electric-p 1 ;; be disposable with SPC

      ;; Not with wl-summary-showto-folder-regexp
      ;;wl-summary-from-function 'bbdb-wl-from-func
      ;; Use the default:
      wl-summary-from-function 'wl-summary-default-from

      ;; Using BBDB for pet names is OK
      wl-summary-get-petname-function 'bbdb-wl-get-petname

      bbdb-always-add-address t            ;; add new addresses to existing...
      ;; ...contacts automatically
      bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

      bbdb-completion-type nil                 ;; complete on anything

      bbdb-complete-name-allow-cycling t       ;; cycle through matches
      ;; this only works partially

      bbbd-message-caching-enabled t           ;; be fast
      bbdb-use-alternate-names t               ;; use AKA


      bbdb-elided-display t                    ;; single-line addresses

      ;; auto-create addresses from mail
      bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
      bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
      '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))


      )
;; http://bbdb.sourceforge.net/bbdb.html#SEC33
;; (define-key wl-draft-mode-map "<C-tab>" 'bbdb-complete-name)

 (add-hook 'wl-draft-mode-hook
           (lambda ()
             (define-key wl-draft-mode-map "<C-tab>" 'bbdb-complete-name)))


;; ----------------------------------------------------------------------------
;;; Configure recently used Email addresses

(require 'recent-addresses)
(setq recent-addresses-file
      (expand-file-name "~/mails/recent-addresses"))

;; ----------------------------------------------------------------------------
;;; Configure supercite to manage citations

(autoload 'sc-cite-original "supercite" nil t)
(setq sc-nested-citation-p t
      sc-citation-leader ""
      sc-auto-fill-region-p nil
      sc-confirm-always-p nil)

(defun my-sc-header ()
  "Insert `Dear <sc-author>,' at the beginning of replies."
  (let ((sc-mumble "")
        (whofrom (sc-whofrom)))
    (if whofrom
        (insert "Dear "
                (sc-hdr "" (sc-mail-field "sc-author"))
                ","))))

;;(add-to-list 'sc-rewrite-header-list '(my-sc-header) t)

(setq sc-preferred-header-style 8)

;; ----------------------------------------------------------------------------
;;; Sending

;; Don't split large messages
(setq mime-edit-split-message nil)

(defun my-wl-gmail-smtp-server ()
  "Configure the use of the GMail SMTP server for sending"
  (setq wl-smtp-connection-type 'stats
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-port 587
        wl-smtp-posting-user "jmlu.java@gmail.com"
        wl-smtp-posting-server "smtp.gmail.com"
        wl-local-domain "gmail.com"))
(defun my-wl-trs-smtp-server ()
  "Configure the use of the TRS SMTP server for sending"
  (setq ;;wl-smtp-connection-type 'stats
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-port 25 ;;25, 465, 597
        wl-smtp-posting-user "lu.jianmei@trs.com.cn"
        wl-smtp-posting-server "smtp.qiye.163.com"
        wl-local-domain "mail.trs.com.cn"))

(defun my-wl-163-smtp-server ()
  "Configure the use of the Zen SMTP server for sending"
  (setq wl-smtp-connection-type nil
        wl-smtp-authenticate-type nil
        wl-smtp-posting-port 25
        wl-smtp-posting-user "anysky130"
        wl-smtp-posting-server "smtp.163.com"
        wl-local-domain "163.com"))

;; ----------------------------------------------------------------------------
;;; Add hooks

(add-hook
 'wl-init-hook
 '(lambda ()
    (set-frame-position (selected-frame) 663 0)
    (set-frame-height (selected-frame) 70)
    (set-frame-width (selected-frame) 114)
    ;;    (my-wl-163-smtp-server) ;; Set the default smtp server to zen
    (my-wl-trs-smtp-server) ;; Set the default smtp server to trs
    (my-bbdb-wl-refile-alist) ;; Add the BBDB refiling folders
    (run-with-idle-timer 30 t 'my-wl-auto-save-draft-buffers)

    ;; Add support for (signature . "filename")
    (unless (assq 'signature wl-draft-config-sub-func-alist)
      (wl-append wl-draft-config-sub-func-alist
                 '((signature . wl-draft-config-sub-signature))))

    (defun mime-edit-insert-signature (&optional arg)
      "Redefine to insert a signature file directly, not as a tag."
      (interactive "P")
      (insert-signature arg))

    ;; Keep track of recently used Email addresses
    (recent-addresses-mode 1)
    ))

(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    (local-set-key "\M-m" 'mairix-search)
    ))

(add-hook
 'wl-summary-mode-hook
 '(lambda ()
    (hl-line-mode t)

    ;; Key bindings
    (local-set-key "D" 'wl-thread-delete)
    (local-set-key "b" 'wl-summary-resend-bounced-mail)
    (local-set-key "\C-d" 'my-wl-summary-delete-and-move-prev)
    (local-set-key "\C-cQ" 'my-wl-delete-whole-folder)
    (local-set-key "\C-cb" 'my-bbdb-wl-refile-alist)
    (local-set-key "\C-a"
                   '(lambda ()
                      (interactive)
                      (wl-summary-reply-with-citation 1)))
    (local-set-key "\M-m" 'mairix-search)
    ))

(add-hook
 'wl-summary-exec-hook
 '(lambda ()
    ;; Synchronise the folder with the server after executing the summary
    ;; operation
    (wl-summary-sync-update)
    ))

(add-hook
 'wl-message-buffer-created-hook
 '(lambda ()
    (setq truncate-lines nil) ;; Fold over-lenght lines
    ))

(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Key bindings
    (local-set-key "\C-c\C-k" 'my-wl-draft-kill-force)
    ))

;; Add supercite support
(add-hook 'mail-citation-hook 'sc-cite-original)

;; Add lots of goodies to the mail setup
(add-hook 'wl-mail-setup-hook 'my-mail-setup)

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    ))

(add-hook
 'wl-biff-notify-hook
 '(lambda ()
    (my-wl-update-current-summaries)
    ))

;; Automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))

(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; ----------------------------------------------------------------------------
;;; Extension Functions

(defun wl-draft-config-sub-signature (content)
  "Insert the signature at the end of the MIME message."
  (let ((signature-insert-at-eof nil)
        (signature-file-name content))
    (goto-char (mime-edit-content-end))
    (insert-signature)))

(defun my-bbdb-wl-refile-alist ()
  "Add the `From To' refile to `folder' entries from the BBDB to the
`wl-refile-rule-alist'."
  (interactive)
  (let ((from-rule-alist (list '("From" "To")))
        (records (bbdb-records))
        (record))
    (while records
      (setq record (car records))
      (let ((email-addrs (bbdb-record-net record))
            (folder (bbdb-record-getprop record 'folder))
            (email-addr))
        (if folder
            (progn
              (while email-addrs
                (setq email-addr (car email-addrs))
                (setq from-rule-alist
                      (append from-rule-alist (list (cons email-addr folder))))
                (setq email-addrs (cdr email-addrs))))))
      (setq records (cdr records)))
    (setq wl-refile-rule-alist
          (append wl-general-refile-rule-alist (list from-rule-alist)))
    ))

;; ----------------------------------------------------------------------------
;;; User Functions

(defun my-wl-draft-kill-force ()
  (interactive)
  (wl-draft-kill t))

(defun my-wl-delete-whole-folder ()
  (interactive)
  (wl-summary-target-mark-all)
  (wl-summary-target-mark-delete)
  (wl-summary-exec)
  (wl-summary-exit))

(defun my-wl-check-mail-primary ()
  (interactive)
  (unless (get-buffer wl-folder-buffer-name)
    (wl))
  (delete-other-windows)
  (switch-to-buffer wl-folder-buffer-name)
  (goto-char (point-min))
  (next-line 1)
  (wl-folder-jump-to-current-entity))

(defun my-wl-auto-save-draft-buffers ()
  (let ((buffers (wl-collect-draft)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (if (buffer-modified-p) (wl-draft-save))
        (setq buffers (cdr buffers))))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(defun my-wl-summary-delete-and-move-prev ()
  (interactive)
  (let (wl-summary-move-direction-downward)
    (call-interactively 'wl-summary-delete)))

(defun my-wl-summary-goto-to-folder (folder)
  "Goto FOLDER from the summary buffer after closing it."
  (wl-summary-exit t)
  (set-buffer (get-buffer wl-folder-buffer-name))
  (wl-folder-goto-folder-subr folder))

(defun my-wl-goto-to-folder (folder)
  "Goto FOLDER from either the folders or summary buffer after closing it."
  (if (string= (buffer-name) wl-summary-buffer-name)
      (my-wl-summary-goto-to-folder search-folder)
    (wl-folder-goto-folder-subr search-folder)))

(defun my-clean-mime-reply ()
  "Clean-up the citation in replies, removing unnecessary entities."
  (interactive)
  ;; Find and strip the first tag, indicating the start of the
  ;; cited message
  (when (re-search-forward "^> \\[1" nil t)
    (beginning-of-line)
    (delete-lines 1)
    (while (or (looking-at "^> *$")
               (looking-at "^> \\[[1-9]"))
      (delete-lines 1))
    (when (re-search-forward "^> \\[[1-9][\\. ]" nil t)
      (beginning-of-line)
      (let ((pt (point)))
        (re-search-forward "^$")
        (delete-region pt (point)))))
  ;; Now find the tag that ends the first section, and strip off
  ;; everything from there to the end of the message (including any
  ;; other sections that got cited)
  (goto-char (point-max))
  (when (re-search-backward "^> +[^ ]" nil t)
    (beginning-of-line)
    (let ((pt (point)))
      (goto-char (point-max))
      (if (re-search-backward "^> *$" pt t)
          (progn
            (beginning-of-line)
            (while (looking-at "^> *$")
              (delete-lines 1)
              (forward-line -1))
            (forward-line 1)
            (delete-lines 1))
        (goto-char (point-max))
        (re-search-backward "^$")
        (delete-lines 1)))))

(defun wl-rehilight ()
  "Re-highlight message."
  (let ((beg (point-min))
        (end (point-max)))
    (put-text-property beg end 'face nil)
    (wl-highlight-message beg end t)))

(defun my-mail-setup ()
  "Set up appropriate modes for writing Email
and clean-up citation for replies."
  (interactive)
  ;; Fold over-lenght lines
  (setq truncate-lines nil)
  (turn-on-auto-fill)
  ;;  (flyspell-mode t)
  (wl-draft-config-exec)
  ;; Switch on the completion selection mode
  ;; and set the default completion-selection to bbdb
  ;;(completion-selection-mode t)
  ;;(completion-selection-set 'complete-bbdb)
  ;; Clean up reply citation
  (save-excursion
    ;; Goto the beginning of the message body
    (mail-text)
    ;; If the message body starts with "Dear " assume it is a reply
    ;; and clean the citation
    (when (looking-at "^Dear ")
      (my-clean-mime-reply))))

(defun my-mime-save-content-find-file (entity &optional situation)
  "Save the attached mime ENTITY and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (let* ((name (or (mime-entity-safe-filename entity)
                   (format "%s" (mime-entity-media-type entity))))
         (dir (if (eq t mime-save-directory)
                  default-directory
                mime-save-directory))
         (filename (expand-file-name
                    (file-name-nondirectory name) temporary-file-directory)))
    (mime-write-entity-content entity filename)
    (select-frame (make-frame))
    (find-file filename)
    ))

(defun my-mime-view-emacs-mode (entity &optional situation)
  "Internal method for mime-view to display the mime ENTITY in a buffer with an
appropriate emacs mode."
  (let ((buf (get-buffer-create
              (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      ;;(mule-caesar-region (point-min) (point-max))
      ;; Set emacs mode here
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
          (select-window (or win (get-largest-window)))
          ))
    (view-buffer buf)
    (goto-char (point-min))
    ))

(defun my-mime-find-file-current-entity ()
  "Save the current mime entity and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
        (my-mime-save-content-find-file entity)))
  )

(require 'elmo)

(defun my-bbdb-insert-folder ()
  "Interactively select the destination folder and store in BBDB."
  (interactive)
  (let* ((record (or (bbdb-current-record t)
                     (error "current record unexists!")))
         (name (wl-summary-read-folder my-wl-default-filing-folder))
         (folder-path (wl-folder-get-elmo-folder name)))
    (bbdb-insert-new-field
     record 'folder (elmo-folder-name-internal folder-path)))
  ;; Update the wl refiling database
  (my-bbdb-wl-refile-alist))

(defun my-mime-button-exec ()
  "Execute the button under point without using the mouse."
  (interactive)
  (let (buf point func data)
    (save-window-excursion
      (setq buf (current-buffer)
            point (point)
            func (get-text-property (point) 'mime-button-callback)
            data (get-text-property (point) 'mime-button-data)
            ))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
          (apply func data))
      )))

;; ----------------------------------------------------------------------------
;;; Key-bindings

(global-set-key "\C-xm" 'my-wl-check-mail-primary)
(define-key bbdb-mode-map [(control f)] 'my-bbdb-insert-folder)

;; ----------------------------------------------------------------------------
;;; Simple mairix interface

(setq my-mairix "mairix")
(setq my-mairix-search-folder ".Search")

(defun my-mairix-search (args)
  "Mairix search"
  (interactive "sSearch: ")
  (let ((buf (get-buffer-create "*mairix*"))
        (msg))
    (set-buffer buf)
    (erase-buffer)
    (apply 'call-process my-mairix nil buf nil (split-string args))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq msg (buffer-substring (point-min) (point))))
    (kill-buffer buf)
    (if (string-match "Matched \\([0-9]+\\) messages" msg)
        (let ((cnt (match-string 1 msg)))
          (if (not (string= cnt "0"))
              (my-wl-goto-to-folder my-mairix-search-folder))
          (message msg))
      (error msg))))



;; ----------------------------------------------------------------------------
;; reformat the email content, by pressing M-q
(require 'filladapt)

;; from a WL mailing list post by Per b. Sederber
;; Re-fill messages that arrive poorly formatted
(defun wl-summary-refill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (set-buffer wl-message-buffer)
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

;;(define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)
 (add-hook 'wl-summary-mode-hook
           (lambda ()
             (define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)))


;; ----------------------------------------------------------------------------
;;; General mairix interface

(require 'mairix)

(add-to-list 'mairix-display-functions '(wl mairix-wl-display))
(add-to-list 'mairix-get-mail-header-functions '(wl mairix-wl-fetch-field))

(setq mairix-file-path "~/mails/"
      mairix-search-file "Search"
      mairix-mail-program 'wl
      mairix-wl-search-folder-prefix ".")

(defun mairix-wl-display (folder)
  "Display FOLDER using Wanderlust."
  ;; If Wanderlust is running (Folder buffer exists)...
  (if (get-buffer wl-folder-buffer-name)
      ;; Check if we are in the summary buffer, close it and
      ;; goto the Folder buffer
      (if (string= (buffer-name) wl-summary-buffer-name)
          (progn
            (wl-summary-exit t)
            (set-buffer (get-buffer wl-folder-buffer-name))))
    ;; Otherwise Wanderlust is not running so start it
    (wl))
  ;; From the Folder buffer goto FOLDER first stripping off mairix-file-path
  ;; to leave the wl folder name
  (when (string-match
         (concat (regexp-quote (expand-file-name mairix-file-path)) "\\(.*\\)")
         folder)
    (wl-folder-goto-folder-subr
     (concat mairix-wl-search-folder-prefix (match-string 1 folder)))))


(defun mairix-wl-fetch-field (field)
  "Get mail header FIELD for current message using Wanderlust."
  (when wl-summary-buffer-elmo-folder
    (elmo-message-field
     wl-summary-buffer-elmo-folder
     (wl-summary-message-number)
     (intern (downcase field)))))

(defvar my-mairix-map
  (let ((map (make-sparse-keymap)))
    ;;    (define-key my-map "m" map)
    map)
  "Sub-keymap in the my keymap for the mairix commands")


(define-key my-mairix-map "m" 'mairix-search)

(define-key my-mairix-map "w" 'mairix-widget-search)

(define-key my-mairix-map "u" 'mairix-update-database)

(define-key my-mairix-map "f" 'mairix-search-from-this-article)

(define-key my-mairix-map "t" 'mairix-search-thread-this-article)

(define-key my-mairix-map "b" 'mairix-widget-search-based-on-article)

(define-key my-mairix-map "s" 'mairix-save-search)

(define-key my-mairix-map "i" 'mairix-use-saved-search)

(define-key my-mairix-map "e" 'mairix-edit-saved-searches)


;;How can I stop [[CategoryBBDB|BBDB]] checking for name mismatches?
(remove-hook 'wl-message-redisplay-hook 'bbdb-wl-get-update-record)
;; ----------------------------------------------------------------------------

;;; init-wl.el ends here
