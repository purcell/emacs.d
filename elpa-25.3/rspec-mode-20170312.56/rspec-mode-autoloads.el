;;; rspec-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "rspec-mode" "rspec-mode.el" (23009 25331 0
;;;;;;  0))
;;; Generated autoloads from rspec-mode.el

(autoload 'rspec-mode "rspec-mode" "\
Minor mode for RSpec files

\\{rspec-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'rspec-verifiable-mode "rspec-mode" "\
Minor mode for Ruby files that have specs

\\{rspec-verifiable-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'rspec-dired-mode "rspec-mode" "\
Minor mode for Dired buffers with spec files

\\{rspec-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'rspec-buffer-is-spec-p "rspec-mode" "\
Return true if the current buffer is a spec.

\(fn)" nil nil)

(autoload 'rspec-enable-appropriate-mode "rspec-mode" "\


\(fn)" nil nil)

(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook)) (add-hook hook 'rspec-enable-appropriate-mode))

(add-hook 'rails-minor-mode-hook 'rspec-verifiable-mode)

;;;***

;;;### (autoloads nil nil ("rspec-mode-pkg.el") (23009 25331 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rspec-mode-autoloads.el ends here
