;;; json-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "json-mode" "json-mode.el" (23009 23266 0 0))
;;; Generated autoloads from json-mode.el

(defconst json-mode-standard-file-ext '(".json" ".jsonld") "\
List of JSON file extensions.")

(defsubst json-mode--update-auto-mode (filenames) "\
Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry" (let* ((new-regexp (rx-to-string (\` (seq (eval (cons (quote or) (append json-mode-standard-file-ext (quote (\, filenames))))) eot)))) (new-entry (cons new-regexp (quote json-mode))) (old-entry (when (boundp (quote json-mode--auto-mode-entry)) json-mode--auto-mode-entry))) (setq auto-mode-alist (delete old-entry auto-mode-alist)) (add-to-list (quote auto-mode-alist) new-entry) new-entry))

(defvar json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock") "\
List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected.")

(custom-autoload 'json-mode-auto-mode-list "json-mode" nil)

(defvar json-mode--auto-mode-entry (json-mode--update-auto-mode json-mode-auto-mode-list) "\
Regexp generated from the `json-mode-auto-mode-list'.")

(autoload 'json-mode "json-mode" "\
Major mode for editing JSON files

\(fn)" t nil)

(autoload 'json-mode-show-path "json-mode" "\


\(fn)" t nil)

(autoload 'json-mode-kill-path "json-mode" "\


\(fn)" t nil)

(autoload 'json-mode-beautify "json-mode" "\
Beautify / pretty-print the active region (or the entire buffer if no active region).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; json-mode-autoloads.el ends here
