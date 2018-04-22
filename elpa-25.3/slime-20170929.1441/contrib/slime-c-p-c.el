(require 'slime)
(require 'cl-lib)

(defvar slime-c-p-c-init-undo-stack nil)

(define-slime-contrib slime-c-p-c
  "ILISP style Compound Prefix Completion."
  (:authors "Luke Gorrie  <luke@synap.se>"
            "Edi Weitz  <edi@agharta.de>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-parse slime-editing-commands slime-autodoc)
  (:swank-dependencies swank-c-p-c)
  (:on-load
   (push
    `(progn
       (remove-hook 'slime-completion-at-point-functions
		    #'slime-c-p-c-completion-at-point)
       (remove-hook 'slime-connected-hook 'slime-c-p-c-on-connect)
       ,@(when (featurep 'slime-repl)
               `((define-key slime-mode-map "\C-c\C-s"
                   ',(lookup-key slime-mode-map "\C-c\C-s"))
                 (define-key slime-repl-mode-map "\C-c\C-s"
                   ',(lookup-key slime-repl-mode-map "\C-c\C-s")))))
    slime-c-p-c-init-undo-stack)
   (add-hook 'slime-completion-at-point-functions
	     #'slime-c-p-c-completion-at-point)
   (define-key slime-mode-map "\C-c\C-s" 'slime-complete-form)
   (when (featurep 'slime-repl)
     (define-key slime-repl-mode-map "\C-c\C-s" 'slime-complete-form)))
  (:on-unload
   (while slime-c-p-c-init-undo-stack
     (eval (pop slime-c-p-c-init-undo-stack)))))

(defcustom slime-c-p-c-unambiguous-prefix-p t
  "If true, set point after the unambigous prefix.
If false, move point to the end of the inserted text."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-complete-symbol*-fancy nil
  "Use information from argument lists for DWIM'ish symbol completion."
  :group 'slime-mode
  :type 'boolean)


;; FIXME: this is the old code to display completions.  Remove it once
;; `slime-complete-symbol*' and `slime-fuzzy-complete-symbol' can be
;; used together with `completion-at-point'.

(defvar slime-completions-buffer-name "*Completions*")

;; FIXME: can probably use quit-window instead
(make-variable-buffer-local
 (defvar slime-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar slime-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `slime-complete-saved-window-configuration'."))

(defun slime-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or slime-complete-saved-window-configuration
              (get-buffer-window slime-completions-buffer-name))
    (setq slime-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun slime-complete-delay-restoration ()
  (add-hook 'pre-command-hook
            'slime-complete-maybe-restore-window-configuration
            'append
            'local))

(defun slime-complete-forget-window-configuration ()
  (setq slime-complete-saved-window-configuration nil)
  (setq slime-completions-window nil))

(defun slime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (when (and slime-complete-saved-window-configuration
             (slime-completion-window-active-p))
    (save-excursion (set-window-configuration
                     slime-complete-saved-window-configuration))
    (setq slime-complete-saved-window-configuration nil)
    (when (buffer-live-p slime-completions-buffer-name)
      (kill-buffer slime-completions-buffer-name))))

(defun slime-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((cl-find last-command-event "()\"'`,# \r\n:")
             (slime-complete-restore-window-configuration))
            ((not (slime-completion-window-active-p))
             (slime-complete-forget-window-configuration))
            (t
             (slime-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-complete-restore-window-configuration: %S"
              err))))

(defun slime-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p slime-completions-window)
       (equal (buffer-name (window-buffer slime-completions-window))
              slime-completions-buffer-name)))

(defun slime-display-completion-list (completions base)
  (let ((savedp (slime-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer slime-completions-buffer-name
      (display-completion-list completions)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-position offset)
          (set-syntax-table lisp-mode-syntax-table))))
    (when savedp
      (setq slime-completions-window
            (get-buffer-window slime-completions-buffer-name)))))

(defun slime-display-or-scroll-completions (completions base)
  (cond ((and (eq last-command this-command)
              (slime-completion-window-active-p))
         (slime-scroll-completions))
        (t
         (slime-display-completion-list completions base)))
  (slime-complete-delay-restoration))

(defun slime-scroll-completions ()
  (let ((window slime-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun slime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defun slime-maybe-complete-as-filename ()
   "If point is at a string starting with \", complete it as filename.
 Return nil if point is not at filename."
   (when (save-excursion (re-search-backward "\"[^ \t\n]+\\="
                                            (max (point-min)
                                                 (- (point) 1000)) t))
     (let ((comint-completion-addsuffix '("/" . "\"")))
      (comint-replace-by-expanded-filename)
      t)))


(defun slime-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (or (slime-maybe-complete-as-filename)
      (slime-expand-abbreviations-and-complete)))

(defun slime-c-p-c-completion-at-point ()
  #'slime-complete-symbol*)

;; FIXME: factorize
(defun slime-expand-abbreviations-and-complete ()
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-result (slime-contextual-completions beg end))
         (completion-set (cl-first completion-result))
         (completed-prefix (cl-second completion-result)))
    (if (null completion-set)
        (progn (slime-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
               (ding)
               (slime-complete-restore-window-configuration))
      ;; some XEmacs issue makes this distinction necessary
      (cond ((> (length completed-prefix) (- end beg))
	     (goto-char end)
	     (insert-and-inherit completed-prefix)
	     (delete-region beg end)
	     (goto-char (+ beg (length completed-prefix))))
	    (t nil))
      (cond ((and (member completed-prefix completion-set)
                  (slime-length= completion-set 1))
             (slime-minibuffer-respecting-message "Sole completion")
             (when slime-complete-symbol*-fancy
               (slime-complete-symbol*-fancy-bit))
             (slime-complete-restore-window-configuration))
            ;; Incomplete
            (t
             (when (member completed-prefix completion-set)
               (slime-minibuffer-respecting-message 
                "Complete but not unique"))
	     (when slime-c-p-c-unambiguous-prefix-p
	       (let ((unambiguous-completion-length
		      (cl-loop for c in completion-set
			       minimizing (or (cl-mismatch completed-prefix c)
                                              (length completed-prefix)))))
		 (goto-char (+ beg unambiguous-completion-length))))
             (slime-display-or-scroll-completions completion-set 
                                                  completed-prefix))))))

(defun slime-complete-symbol*-fancy-bit ()
  "Do fancy tricks after completing a symbol.
\(Insert a space or close-paren based on arglist information.)"
  (let ((arglist (slime-retrieve-arglist (slime-symbol-at-point))))
    (unless (eq arglist :not-available)
      (let ((args
             ;; Don't intern these symbols
             (let ((obarray (make-vector 10 0)))
               (cdr (read arglist))))
            (function-call-position-p
             (save-excursion
               (backward-sexp)
               (equal (char-before) ?\())))
        (when function-call-position-p
          (if (null args)
              (execute-kbd-macro ")")
              (execute-kbd-macro " ")
              (when (and (slime-background-activities-enabled-p)
                         (not (minibuffer-window-active-p (minibuffer-window))))
                (slime-echo-arglist))))))))

(cl-defun slime-contextual-completions (beg end)
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((and (< beg (point-max))
           (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (let ((completions 
             (slime-completions-for-keyword token
                                            (save-excursion 
                                              (goto-char beg)
                                              (slime-parse-form-upto-point)))))
        (when (cl-first completions)
          (cl-return-from slime-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (cl-subseq token 0 2) "#\\"))
      ;; Character name completion
      (cl-return-from slime-contextual-completions
        (slime-completions-for-character token))))
    ;; Regular symbol completion
    (slime-completions token)))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix ',(slime-current-package))))

(defun slime-completions-for-keyword (prefix buffer-form)
  (slime-eval `(swank:completions-for-keyword ,prefix ',buffer-form)))

(defun slime-completions-for-character (prefix)
  (cl-labels ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (slime-eval `(swank:completions-for-character
                                ,(cl-subseq prefix 2)))))
      (when (car result)
        (list (mapcar #'append-char-syntax (car result))
              (append-char-syntax (cadr result)))))))


;;; Complete form

(defun slime-complete-form ()
  "Complete the form at point.  
This is a superset of the functionality of `slime-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let ((buffer-form (slime-parse-form-upto-point)))
    (let ((result (slime-eval `(swank:complete-form ',buffer-form))))
      (if (eq result :not-available)
          (error "Could not generate completion for the form `%s'" buffer-form)
          (progn
            (just-one-space (if (looking-back "\\s(" (1- (point)))
                                0
                                1))
            (save-excursion
              (insert result)
              (let ((slime-close-parens-limit 1))
                (slime-close-all-parens-in-sexp)))
            (save-excursion
              (backward-up-list 1)
              (indent-sexp)))))))

(provide 'slime-c-p-c)

