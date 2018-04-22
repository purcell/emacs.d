;;; inf-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "inf-ruby" "inf-ruby.el" (23009 25332 0 0))
;;; Generated autoloads from inf-ruby.el

(defvar ruby-source-modes '(ruby-mode enh-ruby-mode) "\
Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `ruby-load-file'.
Used by these commands to determine defaults.")

(autoload 'inf-ruby-setup-keybindings "inf-ruby" "\
Hook up `inf-ruby-minor-mode' to each of `ruby-source-modes'.

\(fn)" nil nil)

(autoload 'inf-ruby-minor-mode "inf-ruby" "\
Minor mode for interacting with the inferior process buffer.

The following commands are available:

\\{inf-ruby-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inf-ruby "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use.

If there is a Ruby process running in an existing buffer, switch
to that buffer. Otherwise create a new buffer.

\(fn &optional IMPL)" t nil)

(autoload 'run-ruby "inf-ruby" "\
Run an inferior Ruby process, input and output in a new buffer.

The consecutive buffer names will be:
`*NAME*', `*NAME*<2>', `*NAME*<3>' and so on.

NAME defaults to \"ruby\".

Runs the hooks `comint-mode-hook' and `inf-ruby-mode-hook'.

\(Type \\[describe-mode] in the process buffer for the list of commands.)

\(fn COMMAND &optional NAME)" nil nil)

(autoload 'inf-ruby-switch-setup "inf-ruby" "\
Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `inf-ruby-switch-from-compilation' to `С-x C-q'.

\(fn)" nil nil)

(autoload 'inf-ruby-console-auto "inf-ruby" "\
Run the appropriate Ruby console command.
The command and the directory to run it from are detected
automatically.

\(fn)" t nil)

(autoload 'inf-ruby-console-zeus "inf-ruby" "\
Run Rails console in DIR using Zeus.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-rails "inf-ruby" "\
Run Rails console in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-gem "inf-ruby" "\
Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction.

\(fn DIR)" t nil)

(autoload 'inf-ruby-auto-enter "inf-ruby" "\
Switch to `inf-ruby-mode' if the breakpoint pattern matches the current line.

\(fn)" nil nil)

(autoload 'inf-ruby-auto-exit "inf-ruby" "\
Return to the previous compilation mode if INPUT is a debugger exit command.

\(fn INPUT)" nil nil)

(autoload 'inf-ruby-console-script "inf-ruby" "\
Run custom bin/console, console or console.rb in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-default "inf-ruby" "\
Run Pry, or bundle console, in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-file-contents-match "inf-ruby" "\


\(fn FILE REGEXP &optional MATCH-GROUP)" nil nil)
 (dolist (mode ruby-source-modes) (add-hook (intern (format "%s-hook" mode)) 'inf-ruby-minor-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; inf-ruby-autoloads.el ends here
