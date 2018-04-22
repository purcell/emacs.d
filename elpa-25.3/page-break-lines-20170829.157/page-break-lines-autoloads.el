;;; page-break-lines-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "page-break-lines" "page-break-lines.el" (23009
;;;;;;  21843 0 0))
;;; Generated autoloads from page-break-lines.el

(defvar page-break-lines-char 9472 "\
Character used to render page break lines.")

(custom-autoload 'page-break-lines-char "page-break-lines" t)

(defvar page-break-lines-lighter " PgLn" "\
Mode-line indicator for `page-break-lines-mode'.")

(custom-autoload 'page-break-lines-lighter "page-break-lines" t)

(defvar page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode) "\
Modes in which to enable `page-break-lines-mode'.")

(custom-autoload 'page-break-lines-modes "page-break-lines" t)

(defface page-break-lines '((t :inherit font-lock-comment-face :bold nil :italic nil)) "\
Face used to colorize page break lines.
If using :bold or :italic, please ensure `page-break-lines-char'
is available in that variant of your font, otherwise it may be
displayed as a junk character." :group (quote page-break-lines))

(autoload 'page-break-lines-mode "page-break-lines" "\
Toggle Page Break Lines mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'turn-on-page-break-lines-mode 'page-break-lines-mode)

(autoload 'page-break-lines-mode-maybe "page-break-lines" "\
Enable `page-break-lines-mode' in the current buffer if desired.
When `major-mode' is listed in `page-break-lines-modes', then
`page-break-lines-mode' will be enabled.

\(fn)" nil nil)

(defvar global-page-break-lines-mode nil "\
Non-nil if Global Page-Break-Lines mode is enabled.
See the `global-page-break-lines-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-page-break-lines-mode'.")

(custom-autoload 'global-page-break-lines-mode "page-break-lines" nil)

(autoload 'global-page-break-lines-mode "page-break-lines" "\
Toggle Page-Break-Lines mode in all buffers.
With prefix ARG, enable Global Page-Break-Lines mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Page-Break-Lines mode is enabled in all buffers where
`page-break-lines-mode-maybe' would do it.
See `page-break-lines-mode' for more information on Page-Break-Lines mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; page-break-lines-autoloads.el ends here
