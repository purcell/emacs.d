;;; rake-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "rake" "rake.el" (23009 25336 0 0))
;;; Generated autoloads from rake.el

(autoload 'rake-compile "rake" "\
Runs TASK-NAME from the directory returned by `rake--root'.
The optional MODE can be passed to specify
which mode the compilation buffer should run in.

\(fn TASK-NAME &optional MODE)" nil nil)

(autoload 'rake-rerun "rake" "\
Re-runs the last task

\(fn)" t nil)

(autoload 'rake-regenerate-cache "rake" "\
Regenerates the rake's cache for the current project.

\(fn)" t nil)

(autoload 'rake-find-task "rake" "\
Finds a rake task.

\(fn ARG)" t nil)

(autoload 'rake "rake" "\
Runs rake command.

\(fn ARG &optional COMPILATION-MODE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rake-autoloads.el ends here
