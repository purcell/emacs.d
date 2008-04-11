;;; slime-c-p-c.el --- ILISP style Compound Prefix Completion
;;
;; Authors: Luke Gorrie  <luke@synap.se>
;;          Edi Weitz  <edi@agharta.de>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de> 
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (slime-setup '(slime-c-p-c ... possibly other packages ...))
;;



(require 'slime)
(require 'slime-parse)
(require 'slime-editing-commands)

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
  (let ((arglist (slime-get-arglist (slime-symbol-name-at-point))))
    (when arglist
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
              (insert-and-inherit ")")
            (insert-and-inherit " ")
            (when (and slime-space-information-p
                       (slime-background-activities-enabled-p)
                       (not (minibuffer-window-active-p (minibuffer-window))))
              (slime-echo-arglist))))))))

(defun slime-get-arglist (symbol-name)
  "Return the argument list for SYMBOL-NAME."
  (slime-eval `(swank:arglist-for-echo-area (quote (,symbol-name)))))

(defun* slime-contextual-completions (beg end) 
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((and (< beg (point-max))
               (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (multiple-value-bind (operator-names arg-indices points)
          (save-excursion 
            (goto-char beg)
            (slime-enclosing-form-specs))
        (when operator-names
          (let ((completions 
                 (slime-completions-for-keyword operator-names token
                                                arg-indices)))
            (when (first completions)
              (return-from slime-contextual-completions completions))
            ;; If no matching keyword was found, do regular symbol
            ;; completion.
            ))))
     ((and (> beg 2)
           (string= (buffer-substring-no-properties (- beg 2) beg) "#\\"))
      ;; Character name completion
      (return-from slime-contextual-completions
        (slime-completions-for-character token))))
    ;; Regular symbol completion
    (slime-completions token)))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix ',(slime-current-package))))

(defun slime-completions-for-keyword (operator-designator prefix
                                                          arg-indices)
  (slime-eval `(swank:completions-for-keyword ',operator-designator
					      ,prefix
					      ',arg-indices)))

(defun slime-completions-for-character (prefix)
  (slime-eval `(swank:completions-for-character ,prefix)))


;;; Complete form

(defun slime-complete-form ()
  "Complete the form at point.  
This is a superset of the functionality of `slime-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let ((form-string (slime-incomplete-form-at-point)))
    (let ((result (slime-eval `(swank:complete-form ',form-string))))
      (if (eq result :not-available)
          (error "Could not generate completion for the form `%s'" form-string)
          (progn
            (just-one-space)
            (save-excursion
              ;; SWANK:COMPLETE-FORM always returns a closing
              ;; parenthesis; but we only want to insert one if it's
              ;; really necessary (thinking especially of paredit.el.)
              (insert (substring result 0 -1))
              (let ((slime-close-parens-limit 1))
                (slime-close-all-parens-in-sexp)))
            (save-excursion
              (backward-up-list 1)
              (indent-sexp)))))))

;;; Initialization

(defvar slime-c-p-c-init-undo-stack nil)

(defun slime-c-p-c-init ()
  (slime-require :swank-arglists)
  ;; save current state for unload
  (push 
   `(progn
      (setq slime-complete-symbol-function ',slime-complete-symbol-function)
      (remove-hook 'slime-connected-hook 'slime-c-p-c-on-connect)
      (define-key slime-mode-map "\C-c\C-s"
	',(lookup-key slime-mode-map "\C-c\C-s"))
      (define-key slime-repl-mode-map "\C-c\C-s"
	',(lookup-key slime-repl-mode-map "\C-c\C-s")))
   slime-c-p-c-init-undo-stack)
  (setq slime-complete-symbol-function 'slime-complete-symbol*)
  (define-key slime-mode-map "\C-c\C-s" 'slime-complete-form)
  (define-key slime-repl-mode-map "\C-c\C-s" 'slime-complete-form))

(defun slime-c-p-c-unload ()
  (while slime-c-p-c-init-undo-stack
    (eval (pop slime-c-p-c-init-undo-stack))))

(provide 'slime-c-p-c)
