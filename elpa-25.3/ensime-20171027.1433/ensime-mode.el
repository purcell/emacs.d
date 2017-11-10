;;; ensime-mode.el --- ensime mode

(eval-when-compile
  (require 'ensime-macros))

(require 'cl) ;; needs to be interpreter until we catch all uses

(require 'arc-mode)
(require 'comint)
(require 'dash)
(require 'easymenu)
(require 'flymake)
(require 'font-lock)
(require 'hideshow)
(require 'pp)
(require 's)
(require 'scala-mode)
(require 'thingatpt)
(require 'timer)
(require 'tooltip)
(require 'url-gw)

(require 'ensime-client)
(require 'ensime-util)
(require 'ensime-vars)
(require 'ensime-config)
(require 'ensime-completion-util)

(require 'ensime-inf)
(require 'ensime-stacktrace)
(require 'ensime-debug)
(require 'ensime-editor)
(require 'ensime-company)
(require 'ensime-eldoc)
(require 'ensime-model)
(require 'ensime-notes)
(require 'ensime-popup)
(require 'ensime-refactor)
(require 'ensime-startup)
(require 'ensime-undo)
(require 'ensime-search)
(require 'ensime-doc)
(require 'ensime-semantic-highlight)
(require 'ensime-ui)
(require 'ensime-http)
(require 'timer)

;; should really be optional
(require 'ensime-sbt)

(defvar ensime-source-buffer-saved-hook nil
  "Hook called whenever an ensime source buffer is saved.")

(defvar ensime-source-buffer-loaded-hook nil
  "Hook called whenever an ensime source buffer is loaded.")

(defvar ensime-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))

      (define-key prefix-map (kbd "C-v r") 'ensime-show-uses-of-symbol-at-point)
      (define-key prefix-map (kbd "C-v h") 'ensime-show-hierarchy-of-type-at-point)

      (define-key prefix-map (kbd "C-v s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-v z") 'ensime-inf-switch)
      (define-key prefix-map (kbd "C-v f") 'ensime-format-source)
      (define-key prefix-map (kbd "C-v u") 'ensime-undo-peek)
      (define-key prefix-map (kbd "C-v v") 'ensime-search)
      (define-key prefix-map (kbd "C-v d") 'ensime-show-doc-for-symbol-at-point)
      (define-key prefix-map (kbd "C-v D") 'ensime-project-docs)
      (define-key prefix-map (kbd "C-v t") 'ensime-type-at-point)
      (define-key prefix-map (kbd "C-v T") 'ensime-type-at-point-full-name)
      (define-key prefix-map (kbd "C-v e") 'ensime-print-errors-at-point)
      (define-key prefix-map (kbd "C-v .") 'ensime-expand-selection-command)

      (define-key prefix-map (kbd "C-v C-r") 'ensime-inf-eval-region)
      (define-key prefix-map (kbd "C-v b") 'ensime-inf-eval-buffer)
      (define-key prefix-map (kbd "C-v l") 'ensime-inf-load-file)

      (define-key prefix-map (kbd "C-c c") 'ensime-typecheck-current-buffer)
      (define-key prefix-map (kbd "C-c r") 'ensime-reload-open-files)

      (define-key prefix-map (kbd "C-d a") 'ensime-db-attach)
      (define-key prefix-map (kbd "C-d b") 'ensime-db-set-break)
      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d o") 'ensime-db-step-out)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d i") 'ensime-db-inspect-value-at-point)
      (define-key prefix-map (kbd "C-d t") 'ensime-db-backtrace)
      (define-key prefix-map (kbd "C-d a") 'ensime-db-clear-all-breaks)

      (define-key prefix-map (kbd "C-b s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-b C-j") 'ensime-sbt-send-eol)
      (define-key prefix-map (kbd "C-b S") 'ensime-stacktrace-switch)
      (define-key prefix-map (kbd "C-b c") 'ensime-sbt-do-compile)
      (define-key prefix-map (kbd "C-b C") 'ensime-sbt-do-compile-only)
      (define-key prefix-map (kbd "C-b f") 'ensime-sbt-do-scalariform-only)
      (define-key prefix-map (kbd "C-b n") 'ensime-sbt-do-clean)
      (define-key prefix-map (kbd "C-b E") 'ensime-sbt-do-ensime-config)
      (define-key prefix-map (kbd "C-b o") 'ensime-sbt-do-test-only-dwim)
      (define-key prefix-map (kbd "C-b p") 'ensime-sbt-do-package)
      (define-key prefix-map (kbd "C-b r") 'ensime-sbt-do-run)
      (define-key prefix-map (kbd "C-b T") 'ensime-sbt-do-test)
      (define-key prefix-map (kbd "C-b t") 'ensime-sbt-do-test-dwim)
      (define-key prefix-map (kbd "C-b q") 'ensime-sbt-do-test-quick-dwim)

      (define-key prefix-map (kbd "C-r a") 'ensime-refactor-add-type-annotation)
      (define-key prefix-map (kbd "C-r r") 'ensime-refactor-diff-rename)
      (define-key prefix-map (kbd "C-r o") 'ensime-refactor-diff-organize-imports)
      (define-key prefix-map (kbd "C-r l") 'ensime-refactor-diff-extract-local)
      (define-key prefix-map (kbd "C-r m") 'ensime-refactor-diff-extract-method)
      (define-key prefix-map (kbd "C-r i") 'ensime-refactor-diff-inline-local)
      (define-key prefix-map (kbd "C-r e") 'ensime-refactor-expand-match-cases)
      (define-key prefix-map (kbd "C-r t") 'ensime-import-type-at-point)

      (define-key map ensime-mode-key-prefix prefix-map)

      ;; Prefix-less shortcuts bindings...
      (define-key map (kbd "M-.") 'ensime-edit-definition)
      (define-key map (kbd "M-,") 'ensime-pop-find-definition-stack)

      (define-key map (kbd "M-n") 'ensime-forward-note)
      (define-key map (kbd "M-p") 'ensime-backward-note)

      (define-key map [C-down-mouse-1] 'ignore)
      (define-key map [C-up-mouse-1] 'ignore)
      (define-key map [C-down-mouse-3] 'ignore)
      (define-key map [C-up-mouse-3] 'ignore)
      (define-key map [C-mouse-1] 'ensime-control-mouse-1-single-click)
      (define-key map [C-mouse-3] 'ensime-control-mouse-3-single-click)
      )

    map)
  "Keymap for ENSIME mode."
  )

(defun ensime-run-after-save-hooks ()
  "Things to run whenever a source buffer is saved."
  (when (ensime-source-file-p)
    (when (and (ensime-connected-p) (ensime-analyzer-ready))
      (condition-case err-info
          (run-hooks 'ensime-source-buffer-saved-hook)
        (error
         (message
          "Error running ensime-source-buffer-saved-hook: %s"
          err-info))))))

(defun ensime-run-find-file-hooks ()
  "Things to run whenever a source buffer is opened."
  (when (ensime-source-file-p)
    (when (and (ensime-connected-p) (ensime-analyzer-ready))
      (condition-case err-info
          (run-hooks 'ensime-source-buffer-loaded-hook)
        (error
         (message
          "Error running ensime-source-buffer-loaded-hook: %s"
          err-info))))))

(defun ensime-save-buffer-no-hooks ()
  "Just save the buffer per usual, don't type-check!"
  (let ((after-save-hook nil)
        (before-save-hook nil))
    (save-buffer)))

(defun ensime-delete-buffer-and-file ()
  "Kill the current buffer and delete the corresponding file!"
  (interactive)
  (ensime-assert-buffer-saved-interactive
   (with-current-buffer (or (buffer-base-buffer) (current-buffer))
     (let ((f (buffer-file-name-with-indirect)))
       (ensime-rpc-remove-file f)
       (delete-file f)
       (kill-buffer nil)
       ))
   ))

(easy-menu-define ensime-mode-menu ensime-mode-map
  "Menu for ENSIME mode"
  '("ENSIME"
    ("Test")

    ("Typecheck"
     ["Typecheck file" ensime-typecheck-current-buffer]
     ["Reload typechecker" ensime-reload-open-files])

    ("Refactor"
     ["Add type annotation" (ensime-refactor-add-type-annotation)]
     ["Organize imports" (ensime-refactor-diff-organize-imports)]
     ["Import type at point" ensime-import-type-at-point]
     ["Rename" (ensime-refactor-diff-rename)]
     ["Extract local val" (ensime-refactor-diff-extract-local)]
     ["Extract method" (ensime-refactor-diff-extract-method)]
     ["Inline local val" (ensime-refactor-diff-inline-local)]
     ["Expand match cases" (ensime-refactor-expand-match-cases)])

    ("Navigation"
     ["Search" ensime-search]
     ["Show Usages" ensime-show-uses-of-symbol-at-point]
     ["Show Hierarchy" ensime-show-hierarchy-of-type-at-point]
     ["Lookup definition" ensime-edit-definition]
     ["Pop definition stack" ensime-pop-find-definition-stack]

     ["Expand selection" ensime-expand-selection-command]
     )

    ("Documentation"
     ["Browse documentation of symbol" ensime-show-doc-for-symbol-at-point]
     ["Browse all documentation" ensime-project-docs])

    ("SBT"
     ["Start or switch to" ensime-sbt-switch]
     ["Compile" ensime-sbt-do-compile]
     ["Compile only" ensime-sbt-do-compile-only]
     ["Clean" ensime-sbt-do-clean]
     ["Test" ensime-sbt-do-test]
     ["Test module/suite" ensime-sbt-do-test-dwim]
     ["Test quick" ensime-sbt-do-test-quick-dwim]
     ["Test current class" ensime-sbt-do-test-only-dwim]
     ["Format source" ensime-sbt-do-scalariform-only]
     ["Run" ensime-sbt-do-run]
     ["Package" ensime-sbt-do-package])

    ("Debugger"
     ["Attach" ensime-db-attach]
     ["Set break point" ensime-db-set-break]
     ["Clear breakpoint" ensime-db-clear-break]
     ["Clear all breakpoints" ensime-db-clear-all-breaks]
     ["Step" ensime-db-step]
     ["Next" ensime-db-next]
     ["Run" ensime-db-run]
     ["Continue" ensime-db-continue]
     ["Quit" ensime-db-quit]
     ["Show Backtrace" ensime-db-backtrace]
     ["Inspect value at point" ensime-db-inspect-value-at-point]
     )

    "---"
    ["Go to SBT console" ensime-sbt-switch]
    ["Go to stacktrace buffer" ensime-stacktrace-switch]
    ["Go to Scala REPL" ensime-inf-switch]
    ["Shutdown ENSIME server" ensime-shutdown]
    ["Troubleshooting" ensime-troubleshooting]
    ))

;;;###autoload
(define-minor-mode ensime-mode
  "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).
\\{ensime-mode-map}"
  nil
  nil
  ensime-mode-map

  (if ensime-mode
      (progn

        (pcase ensime-completion-style
          (`company
           (ensime-company-enable))
          (`auto-complete
           (ensime-ac-enable)
           (add-hook 'completion-at-point-functions
                     'ensime-completion-at-point-function nil t))
          (_ t))

        (easy-menu-add ensime-mode-menu ensime-mode-map)

        (add-hook 'after-save-hook 'ensime-run-after-save-hooks nil t)

	(add-hook 'find-file-hook 'ensime-run-find-file-hooks nil t)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-typecheck-current-buffer)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-typecheck-current-buffer)

        (add-hook 'after-change-functions
                  'ensime-after-change-function nil t)

        (add-hook 'window-configuration-change-hook
                  'ensime-show-left-margin-hook)

        (ensime-idle-typecheck-set-timer)

        (when ensime-tooltip-hints
          (add-hook 'tooltip-functions 'ensime-tooltip-handler)
          (make-local-variable 'track-mouse)
          (setq track-mouse t)
          (make-local-variable 'tooltip-delay)
          (setq tooltip-delay 1.0)
          (define-key ensime-mode-map [mouse-movement] 'ensime-mouse-motion))

        (ensime-refresh-all-note-overlays)

        (when ensime-eldoc-hints
          (setq-local eldoc-documentation-function 'ensime-eldoc-info))

	(when (equal major-mode 'scala-mode)
	  (ensime--setup-imenu)))
    (progn
      (pcase ensime-completion-style
        (`auto-complete
         (ensime-ac-disable))
        (_ t))

      (remove-hook 'after-save-hook 'ensime-run-after-save-hooks t)

      (remove-hook 'find-file-hook 'ensime-run-find-file-hooks t)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-typecheck-current-buffer)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-typecheck-current-buffer)

      (remove-hook 'after-change-functions
                   'ensime-after-change-function t)

      (remove-hook 'window-configuration-change-hook
                   'ensime-show-left-margin-hook)

      (remove-hook 'tooltip-functions 'ensime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil)

      (when (equal major-mode 'scala-mode)
	(ensime--unset-imenu)))))

;;;###autoload
(add-hook 'scala-mode-hook
          (lambda () (when (fboundp 'ensime) (ensime-mode))))

;;;;;; Mouse handlers

(defun ensime-control-mouse-1-single-click (event)
  "Command handler for control+clicks of mouse button 1.
   If control is held, jump to definition of symbol under
   point."
  (interactive "e")
  (mouse-set-point event)
  (ensime-edit-definition))

(defun ensime-mouse-motion (event)
  "Command handler for mouse movement events in `ensime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))

;;;;;; imenu

(defun ensime-imenu-index-function ()
  "Function to be used for `imenu-create-index-function'."
  (-flatten
   (-map
    (lambda (x) (ensime-flatten-structure-view x))
    (plist-get (ensime-rpc-structure-view) :view))))

(defun ensime-flatten-structure-view (member-plist &optional result parent)
  (ensime-plist-bind
   (name keyword members position) member-plist
   (-when-let* ((offset (plist-get position :offset))
                (new-parent (if parent (format "%s.%s" parent name) name))
                (imenu-item (cons
                             (format "%s:%s" keyword (if parent new-parent name))
                             (ensime-internalize-offset offset))))
     (if members
         (-concat
          (cons imenu-item result)
          (-map
           (lambda (x) (ensime-flatten-structure-view x result new-parent))
           members))
       (cons imenu-item result)))))

(defun ensime--setup-imenu ()
  "Setup imenu function and make imenu rescan index with every call."
  (when (boundp 'imenu-auto-rescan)
    (set (make-local-variable 'backup-imenu-auto-rescan) imenu-auto-rescan))
  (set (make-local-variable 'backup-imenu-create-index-function) imenu-create-index-function)
  (set (make-local-variable 'imenu-auto-rescan) t)
  (set (make-local-variable 'imenu-create-index-function) #'ensime-imenu-index-function))

(defun ensime--unset-imenu ()
  "Revert ensime specific imenu settings."
  (when (boundp 'backup-imenu-auto-rescan)
    (setq imenu-auto-rescan backup-imenu-auto-rescan))
  (setq imenu-create-index-function backup-imenu-create-index-function))

;;;;;; Tooltips


(defun ensime-tooltip-show-message (msg)
  "Display tooltip, respecting ensime tooltip options."
  (if ensime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun ensime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error
   or warning overlay exists at point, show the description
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."

  (when (and (eventp event)
             ensime-mode
             (ensime-connected-p)
             (ensime-analyzer-ready)
             (posn-point (event-end event)))


    (let* ((point (posn-point (event-end event)))
           (external-pos (ensime-externalize-offset point))
           (ident (tooltip-identifier-from-point point))
           (note-overlays (ensime-overlays-at point))
           (sem-high-overlays (ensime-sem-high-sym-types-at-point point))
           (val-at-pt (ensime-db-tooltip point)))

      (cond

       ;; If debugger is active and we can get the value of the symbol
       ;; at the point, show it in the tooltip.
       (val-at-pt (ensime-tooltip-show-message val-at-pt) t)

       ;; If error or warning overlays exist,
       ;; show that message..
       (note-overlays (progn
                        (ensime-tooltip-show-message
                         (overlay-get (car note-overlays) 'help-echo))
                        t))

       ;; Show implicit conversions if present
       ((or (member 'implicitConversion sem-high-overlays)
            (member 'implicitParams sem-high-overlays))
        (ensime-tooltip-show-message
         (mapconcat 'identity (ensime-implicit-notes-at point) "\n")))

       ;; Otherwise show a type hint..
       ((and ident ensime-tooltip-type-hints)
        (progn
          (ensime-eval-async
           `(swank:type-at-point ,(buffer-file-name-with-indirect) ,external-pos)
           #'(lambda (type)
               (when type
                 (let ((msg (ensime-type-full-name-with-args type)))
                   (ensime-tooltip-show-message msg)
                   ))))
          t
          )))
      )))




;;;;;; Modeline

;; Setup the custom ensime modeline handler
(add-to-list 'minor-mode-alist
             '(ensime-mode (:eval (ensime-modeline-string))))

(defun ensime-modeline-string ()
  "The string to display in the modeline.
\"ENSIME\" only appears if we aren't connected. If connected,
include connection-name, and possibly some state information."
  (when ensime-mode
    (s-wrap
     (condition-case err
         (let ((conn (ensime-connection-or-nil)))
           (cond ((not conn)
                  (if (ensime-owning-server-process-for-source-file (buffer-file-name-with-indirect))
                      "ENSIME: Starting"
                    "ENSIME: Disconnected"))
                 ((ensime-connected-p conn)
                  (let ((config (ensime-config conn)))
                    (or (plist-get config :name)
                        "ENSIME: unknown project")))
                 (t "ENSIME: Dead Connection")))
       (error "ENSIME: Error"))
     "[" "]")))

(provide 'ensime-mode)



;;;;;; Troubleshooting


(defun ensime-troubleshooting ()
  "Information about emacs and ensime installation."
  (interactive)
  (let ((info (format
               (concat "\n### READ THIS\n"
                       "Have you read all the documentation at [Ensime Emacs](http://ensime.org/editors/emacs/)? Please do, we put a lot of effort into it.\n"
                       "Most problems can be resolved easily by following a simple process, follow through our\n"
                       "[Troubleshooting guide](http://ensime.org/editors/emacs/troubleshooting/) and share the debugging information below as a gist if you need to escalate.\n"
                       "Do not paste this directly into the gitter chat room as it is very verbose.\n"
                       "*****************\n"
                       "\n## System Information - Ensime troubleshoot report\n\n"
                       "- OS: `%s`\n"
                       "- Emacs: `%s`\n"
                       "- ensime-sbt-command: `%s`\n"
                       "- debug-on-error: `%s`\n"
                       "- exec-path: `%s`\n"
                       "- Backtrace:\n```\n%s\n```\n"
                       "- .ensime file content:\n```\n%s\n```\n")
               system-type
               emacs-version
               (bound-and-true-p ensime-sbt-command)
               (bound-and-true-p debug-on-error)
               exec-path
               (if (get-buffer "*Backtrace*")
                   (with-current-buffer "*Backtrace*"
                     (buffer-substring-no-properties
                      (point-min) (min (point-max) 1000)))
                 "Backtrace not available or not found")
               (if (y-or-n-p "Do you want to include .ensime file content? (if yes please choose .ensime file)")
                   (progn
                     (let ((filePath (read-file-name "Enter .ensime file path:")))
                       (with-temp-buffer
                         (insert-file-contents filePath)
                         (buffer-string))))
                 (progn "Not provided")))))
    (with-output-to-temp-buffer "*ensime-troubleshooting*"
      (switch-to-buffer "*ensime-troubleshooting*")
      (insert info))))

;; Local Variables:
;; End:
