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
       (setq slime-complete-symbol-function ',slime-complete-symbol-function)
       (remove-hook 'slime-connected-hook 'slime-c-p-c-on-connect)
       ,@(when (featurep 'slime-repl)
               `((define-key slime-mode-map "\C-c\C-s"
                   ',(lookup-key slime-mode-map "\C-c\C-s"))
                 (define-key slime-repl-mode-map "\C-c\C-s"
                   ',(lookup-key slime-repl-mode-map "\C-c\C-s")))))
    slime-c-p-c-init-undo-stack)
   (setq slime-complete-symbol-function 'slime-complete-symbol*)
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

(defun slime-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (or (slime-maybe-complete-as-filename)
      (slime-expand-abbreviations-and-complete)))

;; FIXME: factorize
(defun slime-expand-abbreviations-and-complete ()
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-result (slime-contextual-completions beg end))
         (completion-set (first completion-result))
         (completed-prefix (second completion-result)))
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
		      (loop for c in completion-set
			    minimizing (or (mismatch completed-prefix c)
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

(defun* slime-contextual-completions (beg end) 
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
        (when (first completions)
          (return-from slime-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (subseq token 0 2) "#\\"))
      ;; Character name completion
      (return-from slime-contextual-completions
        (slime-completions-for-character token))))
    ;; Regular symbol completion
    (slime-completions token)))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix ',(slime-current-package))))

(defun slime-completions-for-keyword (prefix buffer-form)
  (slime-eval `(swank:completions-for-keyword ,prefix ',buffer-form)))

(defun slime-completions-for-character (prefix)
  (flet ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (slime-eval `(swank:completions-for-character
                                ,(subseq prefix 2)))))
      (when (car result)
        (list (mapcar 'append-char-syntax (car result))
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

;;; Tests

(def-slime-test complete-symbol*
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" nil)
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-output"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value"))
      ("common-lisp" (("common-lisp-user:" "common-lisp:") "common-lisp")))
  (let ((completions (slime-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test complete-form
    (buffer-sexpr wished-completion &optional skip-trailing-test-p)
    ""
    '(("(defmethod arglist-dispatch *HERE*"
       "(defmethod arglist-dispatch (operator arguments) body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct (*HERE*"
       "(with-struct (conc-name names...)" t)
      ("(with-struct (foo. bar baz *HERE*"
       "(with-struct (foo. bar baz names...)" t))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (setq slime-buffer-package "SWANK")
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-complete-form)
    (slime-check-completed-form buffer-sexpr wished-completion)

    ;; Now the same but with trailing `)' for paredit users...
    (unless skip-trailing-test-p
      (erase-buffer)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (delete-region (match-beginning 0) (match-end 0))
      (insert ")") (backward-char)
      (slime-complete-form)
      (slime-check-completed-form (concat buffer-sexpr ")") wished-completion))
    ))

(defun slime-check-completed-form (buffer-sexpr wished-completion)
  (slime-test-expect (format "Completed form for `%s' is as expected"
                              buffer-sexpr)
                     wished-completion
                     (buffer-string)
                     'equal))

(provide 'slime-c-p-c)

