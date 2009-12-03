;;; slime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (slime-hyperspec-lookup slime-connect slime slime-mode
;;;;;;  slime-lisp-mode-hook) "slime" "slime.el" (19223 50752))
;;; Generated autoloads from slime.el

(defvar slime-lisp-modes '(lisp-mode))

(defvar slime-setup-contribs nil)

(defun slime-setup (&optional contribs) "\
Setup Emacs so that lisp-mode buffers always use SLIME.
 CONTRIBS is a list of contrib packages to load." (when (member (quote lisp-mode) slime-lisp-modes) (add-hook (quote lisp-mode-hook) (quote slime-lisp-mode-hook))) (setq slime-setup-contribs contribs) (slime-setup-contribs))

(autoload 'slime-lisp-mode-hook "slime" "\
Not documented

\(fn)" nil nil)

(autoload 'slime-mode "slime" "\
\\<slime-mode-map> SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

 Commands to compile the current buffer's source file and visually
 highlight any resulting compiler notes and warnings:
 \\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
 \\[slime-compile-file]	- Compile (but not load) the current buffer's file.
 \\[slime-compile-defun]	- Compile the top-level form at point.

 Commands for visiting compiler notes:
 \\[slime-next-note]	- Goto the next form with a compiler note.
 \\[slime-previous-note]	- Goto the previous form with a compiler note.
 \\[slime-remove-notes]	- Remove compiler-note annotations in buffer.

 Finding definitions:
 \\[slime-edit-definition]	- Edit the definition of the function called at point.
 \\[slime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

 Documentation commands:
 \\[slime-describe-symbol]	- Describe symbol.
 \\[slime-apropos]	- Apropos search.
 \\[slime-disassemble-symbol]	- Disassemble a function.

 Evaluation commands:
 \\[slime-eval-defun]	- Evaluate top-level from containing point.
 \\[slime-eval-last-expression]	- Evaluate sexp before point.
 \\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

 Full set of commands:
 \\{slime-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'slime "slime" "\
Start an inferior^_superior Lisp and connect to its Swank server.

\(fn &optional COMMAND CODING-SYSTEM)" t nil)

(autoload 'slime-connect "slime" "\
Connect to a running Swank server. Returns the connection.

\(fn HOST PORT &optional CODING-SYSTEM)" t nil)

(autoload 'slime-hyperspec-lookup "slime" "\
A wrapper for `hyperspec-lookup'

\(fn SYMBOL-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("slime-pkg.el") (19223 50752 325694))

;;;***

(provide 'slime-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; slime-autoloads.el ends here
