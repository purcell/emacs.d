;;; alert-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "alert" "alert.el" (23009 21893 0 0))
;;; Generated autoloads from alert.el

(autoload 'alert-add-rule "alert" "\
Programmatically add an alert configuration rule.

Normally, users should custoimze `alert-user-configuration'.
This facility is for module writers and users that need to do
things the Lisp way.

Here is a rule the author currently uses with ERC, so that the
fringe gets colored whenever people chat on BitlBee:

\(alert-add-rule :status   \\='(buried visible idle)
                :severity \\='(moderate high urgent)
                :mode     \\='erc-mode
                :predicate
                #\\='(lambda (info)
                    (string-match (concat \"\\\\`[^&].*@BitlBee\\\\\\='\")
                                  (erc-format-target-and/or-network)))
                :persistent
                #\\='(lambda (info)
                    ;; If the buffer is buried, or the user has been
                    ;; idle for `alert-reveal-idle-time' seconds,
                    ;; make this alert persistent.  Normally, alerts
                    ;; become persistent after
                    ;; `alert-persist-idle-time' seconds.
                    (memq (plist-get info :status) \\='(buried idle)))
                :style \\='fringe
                :continue t)

\(fn &key SEVERITY STATUS MODE CATEGORY TITLE MESSAGE PREDICATE ICON (style alert-default-style) PERSISTENT CONTINUE NEVER-PERSIST APPEND)" nil nil)

(autoload 'alert "alert" "\
Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

\(alert \"This is a message\"
       :severity \\='high          ;; The default severity is `normal'
       :title \"Title\"           ;; An optional title
       :category \\='example       ;; A symbol to identify the message
       :mode \\='text-mode         ;; Normally determined automatically
       :buffer (current-buffer) ;; This is the default
       :data nil                ;; Unused by alert.el itself
       :persistent nil          ;; Force the alert to be persistent;
                                ;; it is best not to use this
       :never-persist nil       ;; Force this alert to never persist
       :style \\='fringe)          ;; Force a given style to be used;
                                ;; this is only for debugging!

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity \\='high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity \\='trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category \\='some-category-or-other)

\(fn MESSAGE &key (severity (quote normal)) TITLE ICON CATEGORY BUFFER MODE DATA STYLE PERSISTENT NEVER-PERSIST)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; alert-autoloads.el ends here
