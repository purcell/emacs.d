;;; darcsum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "darcsum" "darcsum.el" (23009 21851 0 0))
;;; Generated autoloads from darcsum.el

(autoload 'darcsum-changes "darcsum" "\
Show the changes in another buffer.
Optional argument HOW-MANY limits the number of changes shown,
counting from the most recent changes.

\(fn &optional HOW-MANY)" t nil)

(autoload 'darcsum-whatsnew "darcsum" "\
Run `darcs whatsnew' in DIRECTORY, displaying the output in `darcsum-mode'.

When invoked interactively, prompt for the directory to display changes for.
With prefix arg LOOK-FOR-ADDS, run darcs with argument `--look-for-adds'.
Display the buffer unless NO-DISPLAY is non-nil.
Show context around changes if SHOW-CONTEXT is non-nil.

\(fn DIRECTORY &optional LOOK-FOR-ADDS NO-DISPLAY SHOW-CONTEXT)" t nil)

(autoload 'darcsum-view "darcsum" "\
View the contents of the current buffer as a darcs changeset for DIRECTORY.
More precisely, search forward from point for the next changeset-like region,
and attempt to parse that as a darcs patch.

When invoked interactively, prompt for a directory; by default, the current
working directory is assumed.

\(fn DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil nil ("50darcsum.el" "darcsum-pkg.el") (23009
;;;;;;  21851 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; darcsum-autoloads.el ends here
