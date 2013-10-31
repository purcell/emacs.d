
(define-slime-contrib slime-fontifying-fu
  "Additional fontification tweaks:
Fontify WITH-FOO, DO-FOO, DEFINE-FOO like standard macros.
Fontify CHECK-FOO like CHECK-TYPE."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load
   (font-lock-add-keywords
    'lisp-mode slime-additional-font-lock-keywords)
   (when slime-highlight-suppressed-forms
     (slime-activate-font-lock-magic)))
  (:on-unload
   ;; FIXME: remove `slime-search-suppressed-forms', and remove the
   ;; extend-region hook.
   (font-lock-remove-keywords 
    'lisp-mode slime-additional-font-lock-keywords)))

;;; Fontify WITH-FOO, DO-FOO, and DEFINE-FOO like standard macros.
;;; Fontify CHECK-FOO like CHECK-TYPE.
(defvar slime-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\|without-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face) 
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)
   ("(\\(assert-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)))


;;;; Specially fontify forms suppressed by a reader conditional.

(defcustom slime-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defface slime-reader-conditional-face
  (if (slime-face-inheritance-possible-p)
    '((t (:inherit font-lock-comment-face)))
  '((((background light)) (:foreground "DimGray" :bold t))
    (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

(defvar slime-search-suppressed-forms-match-data (list nil nil))

(defun slime-search-suppressed-forms-internal (limit)
  (when (search-forward-regexp slime-reader-conditionals-regexp limit t)
    (let ((start (match-beginning 0))   ; save match data
          (state (slime-current-parser-state)))
      (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
          (slime-search-suppressed-forms-internal limit)
        (let* ((char (char-before))
               (expr (read (current-buffer)))
               (val  (slime-eval-feature-expression expr)))
          (when (<= (point) limit)
            (if (or (and (eq char ?+) (not val))
                    (and (eq char ?-) val))
                ;; If `slime-extend-region-for-font-lock' did not
                ;; fully extend the region, the assertion below may
                ;; fail. This should only happen on XEmacs and older
                ;; versions of GNU Emacs.
                (ignore-errors
                  (forward-sexp) (backward-sexp)
                  ;; Try to suppress as far as possible.
                  (slime-forward-sexp)
                  (assert (<= (point) limit))
                  (let ((md (match-data nil slime-search-suppressed-forms-match-data)))
                    (setf (first md) start)
                    (setf (second md) (point))
                    (set-match-data md)
                    t))
                (slime-search-suppressed-forms-internal limit))))))))

(defun slime-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slime-highlight-suppressed-forms
             (slime-connected-p))
    (let ((result 'retry))
      (while (and (eq result 'retry) (<= (point) limit))
        (condition-case condition
            (setq result (slime-search-suppressed-forms-internal limit))
          (end-of-file                        ; e.g. #+(
           (setq result nil)) 
          ;; We found a reader conditional we couldn't process for
          ;; some reason; however, there may still be other reader
          ;; conditionals before `limit'.
          (invalid-read-syntax                ; e.g. #+#.foo
           (setq result 'retry))
          (scan-error                         ; e.g. #+nil (foo ...
           (setq result 'retry)) 
          (slime-incorrect-feature-expression ; e.g. #+(not foo bar)
           (setq result 'retry))
          (slime-unknown-feature-expression   ; e.g. #+(foo)
           (setq result 'retry)) 
          (error
           (setq result nil)
           (slime-display-warning
            (concat "Caught error during fontification while searching for forms\n"
                    "that are suppressed by reader-conditionals. The error was: %S.")
            condition))))
      result)))


(defun slime-search-directly-preceding-reader-conditional ()
  "Search for a directly preceding reader conditional. Return its
position, or nil."
  ;;; We search for a preceding reader conditional. Then we check that
  ;;; between the reader conditional and the point where we started is
  ;;; no other intervening sexp, and we check that the reader
  ;;; conditional is at the same nesting level.
  (condition-case nil
      (let* ((orig-pt (point)))
        (when-let (reader-conditional-pt 
                   (search-backward-regexp slime-reader-conditionals-regexp
                                           ;; We restrict the search to the
                                           ;; beginning of the /previous/ defun.
                                           (save-excursion (beginning-of-defun) (point))
                                           t))
          (let* ((parser-state 
                  (parse-partial-sexp (progn (goto-char (+ reader-conditional-pt 2))
                                             (forward-sexp) ; skip feature expr.
                                             (point))
                                      orig-pt))
                 (paren-depth  (car  parser-state))
                 (last-sexp-pt (caddr  parser-state)))
            (if (and paren-depth (not (plusp paren-depth)) ; no opening parenthesis in between?
                     (not last-sexp-pt))                   ; no complete sexp in between?
                reader-conditional-pt
                nil))))
    (scan-error nil)))                                     ; improper feature expression


;;; We'll push this onto `font-lock-extend-region-functions'. In past,
;;; we didn't do so which made our reader-conditional font-lock magic
;;; pretty unreliable (it wouldn't highlight all suppressed forms, and
;;; worked quite non-deterministic in general.)
;;;
;;; Cf. _Elisp Manual_, 23.6.10 Multiline Font Lock Constructs.
;;;
;;; We make sure that `font-lock-beg' and `font-lock-end' always point
;;; to the beginning or end of a toplevel form. So we never miss a
;;; reader-conditional, or point in mid of one.
(defun slime-extend-region-for-font-lock ()
  (when slime-highlight-suppressed-forms
    (condition-case c
        (let (changedp)
          (multiple-value-setq (changedp font-lock-beg font-lock-end)
            (slime-compute-region-for-font-lock font-lock-beg font-lock-end))
          changedp)
      (error
       (slime-display-warning
        (concat "Caught error when trying to extend the region for fontification.\n"
                "The error was: %S\n"
                "Further: font-lock-beg=%d, font-lock-end=%d.")
        c font-lock-beg font-lock-end)))))

(when (fboundp 'syntax-ppss-toplevel-pos)
  (defun slime-beginning-of-tlf ()
    (when-let (pos (syntax-ppss-toplevel-pos (slime-current-parser-state)))
      (goto-char pos))))

(unless (fboundp 'syntax-ppss-toplevel-pos)
  (defun slime-beginning-of-tlf ()
    (let* ((state (slime-current-parser-state))
           (comment-start (nth 8 state)))
      (when comment-start               ; or string
        (goto-char comment-start)
        (setq state (slime-current-parser-state)))
      (let ((depth (nth 0 state)))
        (when (plusp depth)
          (ignore-errors (up-list (- depth)))) ; ignore unbalanced parentheses
        (when-let (upper-pt (nth 1 state)) 
          (goto-char upper-pt)
          (while (when-let (upper-pt (nth 1 (slime-current-parser-state)))
                   (goto-char upper-pt))))))))

(defun slime-compute-region-for-font-lock (orig-beg orig-end)
  (let ((beg orig-beg)
        (end orig-end))
    (goto-char beg)
    (inline (slime-beginning-of-tlf))
    (assert (not (plusp (nth 0 (slime-current-parser-state)))))
    (setq beg (let ((pt (point)))
                (cond ((> (- beg pt) 20000) beg)
                      ((slime-search-directly-preceding-reader-conditional))
                      (t pt))))
    (goto-char end)
    (while (search-backward-regexp slime-reader-conditionals-regexp beg t)
      (setq end (max end (save-excursion 
                           (ignore-errors (slime-forward-reader-conditional))
                           (point)))))
    (values (or (/= beg orig-beg) (/= end orig-end)) beg end)))


(defun slime-activate-font-lock-magic ()
  (if (featurep 'xemacs)
      (let ((pattern `((slime-search-suppressed-forms
                        (0 slime-reader-conditional-face t)))))
        (dolist (sym '(lisp-font-lock-keywords
                       lisp-font-lock-keywords-1
                       lisp-font-lock-keywords-2))
          (set sym (append (symbol-value sym) pattern))))
      (font-lock-add-keywords
       'lisp-mode
       `((slime-search-suppressed-forms 0 ,''slime-reader-conditional-face t)))

      (add-hook 'lisp-mode-hook 
                #'(lambda () 
                    (add-hook 'font-lock-extend-region-functions
                              'slime-extend-region-for-font-lock t t)))))

(let ((byte-compile-warnings '())) 
  (mapc #'byte-compile
        '(slime-extend-region-for-font-lock
          slime-compute-region-for-font-lock
          slime-search-directly-preceding-reader-conditional
          slime-search-suppressed-forms
          slime-beginning-of-tlf)))

;;; Tests
(def-slime-test font-lock-magic (buffer-content)
    "Some testing for the font-lock-magic. *YES* should be
    highlighted as a suppressed form, *NO* should not."

    '(("(defun *NO* (x y) (+ x y))")
      ("(defun *NO*")
      ("*NO*) #-(and) (*YES*) (*NO* *NO*")
      ("\(
\(defun *NO*")
      ("\)
\(defun *NO*
    \(
\)")
      ("#+#.foo
\(defun *NO* (x y) (+ x y))")
      ("#+#.foo
\(defun *NO* (x ")
      ("#+(
\(defun *NO* (x ")
      ("#+(test)
\(defun *NO* (x ")

      ("(eval-when (...)
\(defun *NO* (x ")

      ("(eval-when (...)
#+(and)
\(defun *NO* (x ")

      ("#-(and) (defun *YES* (x y) (+ x y))")
      ("
#-(and) (defun *YES* (x y) (+ x y))
#+(and) (defun *NO* (x y) (+ x y))")

      ("#+(and) (defun *NO* (x y) #-(and) (+ *YES* y))")
      ("#| #+(or) |# *NO*")
      ("#| #+(or) x |# *NO*")
      ("*NO* \"#| *NO* #+(or) x |# *NO*\" *NO*")
      ("#+#.foo (defun foo (bar))
#-(and) *YES* *NO* bar
")
      ("#+(foo) (defun foo (bar))
#-(and) *YES* *NO* bar")
      ("#| #+(or) |# *NO* foo
#-(and) *YES* *NO*")
      ("#- (and)
\(*YES*)
\(*NO*)
#-(and)
\(*YES*)
\(*NO*)")
      ("#+nil (foo)

#-(and)
#+nil (
       asdf *YES* a
            fsdfad)

\( asdf *YES*

       )
\(*NO*)

")
      ("*NO*

#-(and) \(progn
   #-(and)
   (defun *YES* ...)

   #+(and)
   (defun *YES* ...)

   (defun *YES* ...)

   *YES*

   *YES*

   *YES*

   *YES*
\)

*NO*")
      ("#-(not) *YES* *NO*

*NO*

#+(not) *NO* *NO*

*NO*

#+(not a b c) *NO* *NO*

*NO*"))
  (slime-check-top-level)
  (with-temp-buffer
    (insert buffer-content)
    (slime-initialize-lisp-buffer-for-test-suite
     :autodoc t :font-lock-magic t)
    ;; Can't use `font-lock-fontify-buffer' because for the case when
    ;; `jit-lock-mode' is enabled. Jit-lock-mode fontifies only on
    ;; actual display.
    (font-lock-default-fontify-buffer)
    (when (search-backward "*NO*" nil t)
      (slime-test-expect "Not suppressed by reader conditional?"
                         'slime-reader-conditional-face
                         (get-text-property (point) 'face)
                         #'(lambda (x y) (not (eq x y)))))
    (goto-char (point-max))
    (when (search-backward "*YES*" nil t)
      (slime-test-expect "Suppressed by reader conditional?"
                         'slime-reader-conditional-face
                         (get-text-property (point) 'face)))))

(defun* slime-initialize-lisp-buffer-for-test-suite 
    (&key (font-lock-magic t) (autodoc t))
  (let ((hook lisp-mode-hook))
    (unwind-protect
         (progn 
           (set (make-local-variable 'slime-highlight-suppressed-forms)
                font-lock-magic)
           (setq lisp-mode-hook nil)
           (lisp-mode)
           (slime-mode 1)
           (when (boundp 'slime-autodoc-mode)
             (if autodoc
                 (slime-autodoc-mode 1)
                 (slime-autodoc-mode -1))))
      (setq lisp-mode-hook hook))))

(provide 'slime-fontifying-fu)
