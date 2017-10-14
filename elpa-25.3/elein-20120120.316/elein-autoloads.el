;;; elein-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "elein" "elein.el" (23009 25365 0 0))
;;; Generated autoloads from elein.el

(autoload 'elein-swank "elein" "\
Launch lein swank and connect slime to it.  Interactively, a
PREFIX means launch a standalone swank session without a
project.

\(fn &optional PREFIX)" t nil)

(autoload 'elein-kill-swank "elein" "\
Kill swank process started by lein swank.

\(fn)" t nil)

(autoload 'elein-reswank "elein" "\
Kill current lisp, restart lein swank and connect slime to it.

\(fn)" t nil)

(autoload 'elein-run-cmd "elein" "\
Run 'lein ARGS' using `compile' in the project root directory.

\(fn ARGS)" t nil)

(autoload 'elein-run-task "elein" "\
Run 'lein TASK' using `compile' in the project root directory.

\(fn TASK)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elein-autoloads.el ends here
