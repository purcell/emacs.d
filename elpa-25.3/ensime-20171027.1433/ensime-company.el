;;; ensime-company.el --- ENSIME support for company-mode

;; Copyright (C) 2003 - 2015 the SLIME and ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;; The `ensime-company' function provides a `company' backend that
;; will serve completion candidates asynchronously.
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'ensime-client)
(require 'ensime-completion-util)
(require 'ensime-util)
(require 'ensime-model)

(require 'company)
(require 'yasnippet)
(require 'scala-mode-syntax)
(require 's)
(require 'dash)

(defcustom ensime-company-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defun ensime--yasnippet-escape (s)
  "Return a string with special yasnippet chars escaped in `S'."
  (s-replace "$" "\\$" s))

(defun ensime--build-yasnippet-for-call
    (param-sections &optional infix pass-function-block)
  "Return a yasnippet template for a method call defined by `PARAM-SECTIONS'.
Each argument is a tab-stop.

Non-nil `INFIX' to omit parenthesis, `PASS-FUNCTION-BLOCK' to use
block notation for the final parameter."
  (let ((tab-stop 0)
        (section-count 0))
    (mapconcat
     (lambda (sect)
       (incf section-count)
       (let* ((params (plist-get sect :params)))
         (if (and pass-function-block
                  (= section-count (length param-sections)))

             ;; If requested, expand the last section as an inline block.
             (let* ((type-info (cadar params))
                    (block-params (plist-get (car (plist-get type-info :param-sections)) :params))
                    (result-type (plist-get type-info :result-type)))
               (if (ensime-type-is-by-name-p type-info) " { $0 }"
                 (concat
                  " { "
                  (let ((param-list
                         (mapconcat
                          (lambda (name-and-type)
                            (cl-destructuring-bind (name type) name-and-type
                              (let ((param-name (ensime--yasnippet-escape name))
                                    (type-name (ensime--yasnippet-escape (plist-get type :name))))
                                (format "${%s:%s: %s}" (incf tab-stop) param-name type-name))))
                          block-params
                          ", ")))
                    (cond ((> (length block-params) 1) (format "(%s)" param-list)) ;; (Int, String) => ...
                          ((= (length block-params) 1) param-list) ;; Int => ...
                          ((= (length block-params) 0) "()"))) ;; () => ...
                  (let ((result-type-name (ensime--yasnippet-escape
                                           (plist-get result-type :name))))
                    (format " => ${%s:%s} }$0" (incf tab-stop) result-type-name)))))

           ;; Otherwise build template for a standard parameter list.
           (concat (if infix " " "(")
                   (mapconcat
                    (lambda (nm-and-tp)
                      (let ((param-name (ensime--yasnippet-escape (car nm-and-tp)))
                            (type-name (ensime--yasnippet-escape
                                        (plist-get (cadr nm-and-tp) :name))))
                        (format "${%s:%s: %s}"
                                (incf tab-stop)
                                param-name type-name)))
                    params ", ")
                   (if infix "" ")")))))
     param-sections
     "")))

(defun ensime--company-try-completion ()
  "Attempts a company-mode completion at point. Returns nil if
 completion is not available at point."
  (when company-mode
    (let ((unique-candidate (ensime-unique-candidate-at-point)))
      (cond
       ;; If the identifier is already complete, we must invoke parameter
       ;; expansion manually.
       (unique-candidate
	(ensime--yasnippet-complete-action unique-candidate)
	t)

       ((company-manual-begin)
	(company-complete-common)
	t)

       (t nil)))))

(defun ensime-company-complete-or-indent ()
  "Try to complete, falling back to indentation."
  (interactive)
  (when (or (ensime-at-bol-p)
	    (not (ensime--company-try-completion)))
    (if mark-active
        (indent-region (region-beginning) (region-end))
      (indent-according-to-mode))))

(defcustom ensime-company-idle-delay 0
  "The idle delay in seconds until completion starts automatically when using company-mode."
  :type 'number
  :group 'ensime-ui)

(defcustom company-minimum-prefix-length 2
  "Minimum prefix length until completion starts automatically when using company-mode."
  :type 'number
  :group 'ensime-ui)

;;;###autoload
(defun ensime-company-enable ()
  (make-local-variable 'company-backends)
  (push #'ensime-company company-backends)
  (company-mode t)

  (set (make-local-variable 'company-idle-delay) ensime-company-idle-delay)
  (set (make-local-variable 'company-minimum-prefix-length) company-minimum-prefix-length)

  ;; https://github.com/joaotavora/yasnippet/issues/708#issuecomment-222517433
  (yas-minor-mode t)
  (make-local-variable 'yas-minor-mode-map)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (if (window-system)
      (local-set-key [tab] #'ensime-company-complete-or-indent)
    (local-set-key (kbd "TAB") #'ensime-company-complete-or-indent)))

(defun ensime--yasnippet-complete-action (&optional candidate-in force-block)
  "Side-effect yasnippet completion for the candidate.
The candidate is provided as a string object with text properties
`TO-INSERT' (string) and `TYPE-INFO' (ensime-type-info) in either
`CANDIDATE-IN' or the dynamic scope `CANDIDATE'.

`FORCE-BLOCK' is an optional character to use to open the
bracketing.

This is typically called after the base candidate contents have
been inserted immediately prior to the point."
  (let* ((candidate (or candidate-in candidate)) ;; auto-complete uses dynamic variables
         (name candidate) ;; clean string without the text properties
         (to-insert (get-text-property 0 'to-insert candidate))
         (type-info (get-text-property 0 'type-info candidate))
         (is-callable (plist-get type-info :arrow-type))
         (name-start-point (- (point) (length name)))
         (is-scala (ensime-scala-file-p (buffer-file-name-with-indirect)))

         (param-sections
          ;; ignore implicit sections
          (when is-callable
            (-filter
             (lambda (sect)
               (not (plist-get sect :is-implicit)))
             (plist-get type-info :param-sections))))

         (is-infix
          ;; move this logic to the server https://github.com/ensime/ensime-server/issues/1475
          (and is-callable
               (= 1 (length param-sections))
               (= 1 (length (plist-get
                             (car param-sections) :params)))
               (null (string-match "[A-z]" name))))

         (is-field-assigner
          (s-ends-with? "_=" name))

         (is-nullary
          ;; could also move to the server
          (and is-scala
               (or
                (null param-sections)
                (and
                 ;; special case hack
                 (null (plist-get (car param-sections) :params))
                 (s-starts-with-p "get" candidate))))))

    ;; Some thoughts: any `delete-char' code needs to be really
    ;; careful about what it's deleting. We're making a lot of
    ;; assumptions about what is at point and we'd be safer doing
    ;; `looking-back' queries.
    ;;
    ;; These different cases all appear to be mutually exclusive, so
    ;; we should perhaps treat them in a condition case instead of
    ;; procedurally.

    ;; assumes the user typed `_=' or ` ='
    (when is-field-assigner
      (delete-char (- 2))
      (insert " ="))

    ;; assumes the user typed `.<name>' or ` <name>' where name is a one character symbol
    (when is-infix
      (delete-char (- (+ 1 (length name))))
      (insert " ")
      (insert name))

    ;; `to-insert' should trump what the user typed. We're assuming the user typed `name'
    ;; (this sounds wrong!)
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If we're modifying an existing method identifier, delete the
    ;; `name' part that was there before and ignore the parameters
    ;; (which may already exist in the buffer)
    (-when-let (suffix (ensime-completion-suffix-at-point))
      (delete-char (length suffix))
      (setq is-nullary t))

    ;; adds the parameters with yasnippet
    (when (and is-callable type-info (not is-nullary))
      (let* ((maybe-braces (ensime-param-section-accepts-block-p
                            (car (last param-sections))))
             (pass-function-block
              (and maybe-braces
                   (equal
                    ?\{
                    (or force-block
                        (save-excursion (read-char-choice "{ or (" '(?\{ ?\()))))))
             (snippet
              (ensime--build-yasnippet-for-call
               param-sections
               (or is-infix is-field-assigner)
               pass-function-block)))
        (yas-expand-snippet snippet (point) (point))))))

(defun ensime-company (command &optional arg &rest rest)
  "Ensime backend for `company-mode'.
`COMMAND' `ARG' and `REST' are defined by the `company-mode' public API."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'ensime-company))

    (`prefix (if (ensime-connected-p)
                 (ensime-completion-prefix-at-point)
               nil))

    (`candidates
     ;; Just ignore if there's no connection.
     (when (and (ensime-connected-p) (ensime-analyzer-ready))
       (let ((max-results 1000000)  ;; We want *all* candidates.
             (case-sense ensime-company-case-sensitive))
	 `(:async . (lambda (callback)
		      (ensime-get-completions-async
		       ,max-results ,case-sense callback))))))

    ;; Don't do client-side sorting (preserve server-side rankings).
    (`sorted t)

    ;; We handle dup removal on the server.
    (`duplicates nil)

    ;; We request *all* completions, so it's ok to let company manage caching.
    (`no-cache nil)

    ;; Show an inline signature for callable completions.
    (`annotation
     (let* ((type-info (get-text-property 0 'type-info arg))
            (is-callable (plist-get type-info :arrow-type)))
       (concat (if is-callable "" ": ")
               (plist-get type-info :name))))

    ;; Expand function formal parameters if we've completed a call.
    (`post-completion (ensime--yasnippet-complete-action arg))

    (`ignore-case t)
    (`require-match `never)
    (`doc-buffer nil) ;; TODO for docs!
    (`meta nil) ;; TODO for short docs!
    (`location nil) ;; TODO Maybe use at some point to link to definitions.
    (_ nil)
    ))

(provide 'ensime-company)

;;; ensime-company.el ends here
