;;; wanderlust-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "elmo" "elmo.el" (21816 37685 154447 315000))
;;; Generated autoloads from elmo.el

(autoload 'elmo-make-folder "elmo" "\
Make an ELMO folder structure specified by NAME.
If optional argument NON-PERSISTENT is non-nil, the folder msgdb is not saved.
If optional argument MIME-CHARSET is specified, it is used for
encode and decode a multibyte string.

\(fn NAME &optional NON-PERSISTENT MIME-CHARSET)" nil nil)

;;;***

;;;### (autoloads nil "elmo-split" "elmo-split.el" (21816 37684 497780
;;;;;;  626000))
;;; Generated autoloads from elmo-split.el

(autoload 'elmo-split "elmo-split" "\
Split messages in the `elmo-split-folder' according to `elmo-split-rule'.
If prefix argument ARG is specified, do a reharsal (no harm).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "wl" "wl.el" (21816 37678 874447 98000))
;;; Generated autoloads from wl.el

(autoload 'wl "wl" "\
Start Wanderlust -- Yet Another Message Interface On Emacsen.
If ARG (prefix argument) is specified, folder checkings are skipped.

\(fn &optional ARG)" t nil)

(autoload 'wl-other-frame "wl" "\
Pop up a frame to read messages via Wanderlust.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "wl-addrmgr" "wl-addrmgr.el" (21816 37683 641113
;;;;;;  929000))
;;; Generated autoloads from wl-addrmgr.el

(autoload 'wl-addrmgr "wl-addrmgr" "\
Start an Address manager.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "wl-draft" "wl-draft.el" (21816 37682 981113
;;;;;;  907000))
;;; Generated autoloads from wl-draft.el

(autoload 'wl-draft "wl-draft" "\
Write and send mail/news message with Wanderlust.

\(fn &optional HEADER-ALIST CONTENT-TYPE CONTENT-TRANSFER-ENCODING BODY EDIT-AGAIN PARENT-FOLDER PARENT-NUMBER)" t nil)

(autoload 'wl-user-agent-compose "wl-draft" "\
Support the `compose-mail' interface for wl.
Only support for TO, SUBJECT, and OTHER-HEADERS has been implemented.
Support for CONTINUE, YANK-ACTION, SEND-ACTIONS and RETURN-ACTION has not
been implemented yet.  Partial support for SWITCH-FUNCTION now supported.

\(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-FUNCTION YANK-ACTION SEND-ACTIONS RETURN-ACTION)" nil nil)

;;;***

;;;### (autoloads nil nil ("acap.el" "elmo-access.el" "elmo-archive.el"
;;;;;;  "elmo-cache.el" "elmo-date.el" "elmo-dop.el" "elmo-file.el"
;;;;;;  "elmo-filter.el" "elmo-flag.el" "elmo-imap4.el" "elmo-internal.el"
;;;;;;  "elmo-localdir.el" "elmo-localnews.el" "elmo-maildir.el"
;;;;;;  "elmo-map.el" "elmo-mime.el" "elmo-msgdb.el" "elmo-multi.el"
;;;;;;  "elmo-net.el" "elmo-nntp.el" "elmo-null.el" "elmo-pipe.el"
;;;;;;  "elmo-pop3.el" "elmo-search.el" "elmo-sendlog.el" "elmo-shimbun.el"
;;;;;;  "elmo-signal.el" "elmo-spam.el" "elmo-util.el" "elmo-vars.el"
;;;;;;  "elmo-version.el" "elsp-bogofilter.el" "elsp-bsfilter.el"
;;;;;;  "elsp-sa.el" "elsp-spamfilter.el" "elsp-spamoracle.el" "mmimap.el"
;;;;;;  "modb-entity.el" "modb-legacy.el" "modb-standard.el" "modb.el"
;;;;;;  "pldap.el" "slp.el" "wanderlust-pkg.el" "wl-acap.el" "wl-action.el"
;;;;;;  "wl-address.el" "wl-batch.el" "wl-demo.el" "wl-e21.el" "wl-expire.el"
;;;;;;  "wl-fldmgr.el" "wl-folder.el" "wl-highlight.el" "wl-message.el"
;;;;;;  "wl-mime.el" "wl-refile.el" "wl-score.el" "wl-spam.el" "wl-summary.el"
;;;;;;  "wl-template.el" "wl-thread.el" "wl-util.el" "wl-vars.el"
;;;;;;  "wl-version.el") (21816 37685 536964 151000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wanderlust-autoloads.el ends here
