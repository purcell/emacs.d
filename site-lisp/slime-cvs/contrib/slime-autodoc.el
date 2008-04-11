;;; slime-autodoc.el --- show fancy arglist in echo area
;;
;; Authors: Luke Gorrie  <luke@bluetail.com>
;;          Lawrence Mitchell  <wence@gmx.li>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-autodoc)))
;;

(require 'slime-parse)

(defvar slime-use-autodoc-mode t
  "When non-nil always enable slime-autodoc-mode in slime-mode.")

(defun slime-fontify-string (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create " *slime-fontify*")
    (erase-buffer)
    (if (not (eq major-mode 'lisp-mode))
        (lisp-mode))
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (when (re-search-forward "===> \\(\\(.\\|\n\\)*\\) <===" nil t)
      (let ((highlight (match-string 1)))
        ;; Can't use (replace-match highlight) here -- broken in Emacs 21
        (delete-region (match-beginning 0) (match-end 0))
	(slime-insert-propertized '(face highlight) highlight)))
    (buffer-substring (point-min) (point-max))))

(defun slime-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slime-read-symbol-name "Arglist of: ")))
  (slime-eval-async 
   `(swank:arglist-for-echo-area (quote (,name)))
   (lambda (arglist)
     (if arglist
         (message "%s" (slime-fontify-string arglist))
       (error "Arglist not available")))))



;;;; Autodocs (automatic context-sensitive help)

(defvar slime-autodoc-mode nil
  "*When non-nil, print documentation about symbols as the point moves.")

(defvar slime-autodoc-cache-type 'last
  "*Cache policy for automatically fetched documentation.
Possible values are:
 nil  - none.
 last - cache only the most recently-looked-at symbol's documentation.
        The values are stored in the variable `slime-autodoc-cache'.

More caching means fewer calls to the Lisp process, but at the risk of
using outdated information.")

(defvar slime-autodoc-cache nil
  "Cache variable for when `slime-autodoc-cache-type' is 'last'.
The value is (SYMBOL-NAME . DOCUMENTATION).")

(defun slime-autodoc-mode (&optional arg)
  "Enable `slime-autodoc'."
  (interactive "P")
  (cond ((< (prefix-numeric-value arg) 0) (setq slime-autodoc-mode nil))
        (arg (setq slime-autodoc-mode t))
        (t (setq slime-autodoc-mode (not slime-autodoc-mode))))
  (if slime-autodoc-mode
      (progn 
        (slime-autodoc-start-timer)
        (add-hook 'pre-command-hook 
                  'slime-autodoc-pre-command-refresh-echo-area t))
    (slime-autodoc-stop-timer)))

(defvar slime-autodoc-last-message "")

(defun slime-autodoc ()
  "Print some apropos information about the code at point, if applicable."
  (destructuring-bind (cache-key retrieve-form) (slime-autodoc-thing-at-point)
    (let ((cached (slime-get-cached-autodoc cache-key)))
      (if cached 
          (slime-autodoc-message cached)
        ;; Asynchronously fetch, cache, and display documentation
        (slime-eval-async 
         retrieve-form
         (slime-rcurry 
	  (lambda (doc cache-key)
	    (let ((doc (if doc (slime-fontify-string doc) "")))
	      (slime-update-autodoc-cache cache-key doc)
	      (slime-autodoc-message doc)))
	  cache-key))))))

(defcustom slime-autodoc-use-multiline-p nil
  "If non-nil, allow long autodoc messages to resize echo area display."
  :type 'boolean
  :group 'slime-ui)

(defvar slime-autodoc-message-function 'slime-autodoc-show-message)

(defun slime-autodoc-message (doc)
  "Display the autodoc documentation string DOC."
  (funcall slime-autodoc-message-function doc))

(defun slime-autodoc-show-message (doc)
  (unless slime-autodoc-use-multiline-p
    (setq doc (slime-oneliner doc)))
  (setq slime-autodoc-last-message doc)
  (message "%s" doc))

(defvar slime-autodoc-dimensions-function nil)

(defun slime-autodoc-message-dimensions ()
  "Return the available width and height for pretty printing autodoc
messages."
  (cond
   (slime-autodoc-dimensions-function
    (funcall slime-autodoc-dimensions-function))
   (slime-autodoc-use-multiline-p 
    ;; Use the full width of the minibuffer;
    ;; minibuffer will grow vertically if necessary
    (values (window-width (minibuffer-window))
            nil))
   (t
    ;; Try to fit everything in one line; we cut off when displaying
    (values 1000 1))))

(defun slime-autodoc-pre-command-refresh-echo-area ()
  (unless (string= slime-autodoc-last-message "")
    (if (slime-autodoc-message-ok-p)
        (message "%s" slime-autodoc-last-message)
      (setq slime-autodoc-last-message ""))))

(defun slime-autodoc-thing-at-point ()
  "Return a cache key and a swank form."
  (let ((global (slime-autodoc-global-at-point)))
    (if global
        (values (slime-qualify-cl-symbol-name global)
                `(swank:variable-desc-for-echo-area ,global))
      (multiple-value-bind (operators arg-indices points)
          (slime-enclosing-form-specs)
        (values (mapcar* (lambda (designator arg-index)
                           (cons
                            (if (symbolp designator)
                                (slime-qualify-cl-symbol-name designator)
                              designator)
                            arg-index))
                         operators arg-indices)
                (multiple-value-bind (width height)
                    (slime-autodoc-message-dimensions)
                  `(swank:arglist-for-echo-area ',operators
                                                :arg-indices ',arg-indices
                                                :print-right-margin ,width
                                                :print-lines ,height)))))))

(defun slime-autodoc-global-at-point ()
  "Return the global variable name at point, if any."
  (when-let (name (slime-symbol-name-at-point))
    (if (slime-global-variable-name-p name) name)))

(defcustom slime-global-variable-name-regexp "^\\(.*:\\)?\\([*+]\\).+\\2$"
  "Regexp used to check if a symbol name is a global variable.

Default value assumes +this+ or *that* naming conventions."
  :type 'regexp
  :group 'slime)

(defun slime-global-variable-name-p (name)
  "Is NAME a global variable?
Globals are recognised purely by *this-naming-convention*."
  (and (< (length name) 80) ; avoid overflows in regexp matcher
       (string-match slime-global-variable-name-regexp name)))

(defun slime-get-cached-autodoc (symbol-name)
  "Return the cached autodoc documentation for SYMBOL-NAME, or nil."
  (ecase slime-autodoc-cache-type
    ((nil) nil)
    ((last)
     (when (equal (car slime-autodoc-cache) symbol-name)
       (cdr slime-autodoc-cache)))
    ((all)
     (when-let (symbol (intern-soft symbol-name))
       (get symbol 'slime-autodoc-cache)))))

(defun slime-update-autodoc-cache (symbol-name documentation)
  "Update the autodoc cache for SYMBOL with DOCUMENTATION.
Return DOCUMENTATION."
  (ecase slime-autodoc-cache-type
    ((nil) nil)
    ((last)
     (setq slime-autodoc-cache (cons symbol-name documentation)))
    ((all)
     (put (intern symbol-name) 'slime-autodoc-cache documentation)))
  documentation)


;;;;; Asynchronous message idle timer

(defvar slime-autodoc-idle-timer nil
  "Idle timer for the next autodoc message.")

(defvar slime-autodoc-delay 0.2
  "*Delay before autodoc messages are fetched and displayed, in seconds.")

(defun slime-autodoc-start-timer ()
  "(Re)start the timer that prints autodocs every `slime-autodoc-delay' seconds."
  (interactive)
  (when slime-autodoc-idle-timer
    (cancel-timer slime-autodoc-idle-timer))
  (setq slime-autodoc-idle-timer
        (run-with-idle-timer slime-autodoc-delay slime-autodoc-delay
                             'slime-autodoc-timer-hook)))

(defun slime-autodoc-stop-timer ()
  "Stop the timer that prints autodocs.
See also `slime-autodoc-start-timer'."
  (when slime-autodoc-idle-timer
    (cancel-timer slime-autodoc-idle-timer)
    (setq slime-autodoc-idle-timer nil)))

(defun slime-autodoc-timer-hook ()
  "Function to be called after each Emacs becomes idle.
When `slime-autodoc-mode' is non-nil, print apropos information about
the symbol at point if applicable."
  (when (slime-autodoc-message-ok-p)
    (condition-case err
        (slime-autodoc)
      (error
       (setq slime-autodoc-mode nil)
       (message "Error: %S; slime-autodoc-mode now disabled." err)))))

(defun slime-autodoc-message-ok-p ()
  "Return true if printing a message is currently okay (shouldn't
annoy the user)."
  (and (or slime-mode (eq major-mode 'slime-repl-mode) 
           (eq major-mode 'sldb-mode))
       slime-autodoc-mode
       (or (null (current-message)) 
           (string= (current-message) slime-autodoc-last-message))
       (not executing-kbd-macro)
       (not (and (boundp 'edebug-active) (symbol-value 'edebug-active)))
       (not cursor-in-echo-area)
       (not (active-minibuffer-window))
       (not (eq (selected-window) (minibuffer-window)))
       (slime-background-activities-enabled-p)))


;;; Initialization

(defun slime-autodoc-init ()
  (setq slime-echo-arglist-function 'slime-autodoc)
  (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
    (add-hook h 'slime-autodoc-maybe-enable)))

(defun slime-autodoc-maybe-enable ()
  (when slime-use-autodoc-mode 
    (slime-autodoc-mode 1)))

(defun slime-autodoc-unload ()
  (setq slime-echo-arglist-function 'slime-show-arglist)
  (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
    (remove-hook h 'slime-autodoc-maybe-enable)))

(slime-require :swank-arglists)

(provide 'slime-autodoc)
