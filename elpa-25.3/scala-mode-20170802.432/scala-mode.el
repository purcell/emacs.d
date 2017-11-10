;;; scala-mode.el --- Major mode for editing Scala

;; Copyright (c) 2012 Heikki Vesalainen

;; Homepage: https://github.com/ensime/emacs-scala-mode
;; Keywords: languages
;; Package-Version:  0.23
;; Package-Requires: ()

;;; Commentary:
;;
;;  Documentation at http://ensime.org/editors/emacs/scala-mode/
;;
;;; Code:

(require 'scala-mode-lib)
(require 'scala-mode-syntax)
(require 'scala-mode-paragraph)
(require 'scala-mode-indent)
(require 'scala-mode-fontlock)
(require 'scala-mode-map)
(require 'scala-mode-imenu)
(require 'scala-mode-prettify-symbols)

;; Tested only for emacs 24
(unless (<= 24 emacs-major-version)
  (error
   (format "The Scala mode has been tested only on Emacs version 24.2 (and not your Emacs version %s.%s)"
           emacs-major-version  emacs-minor-version)))

(defgroup scala nil
  "A programming mode for the Scala language 2.9"
  :group 'languages)

(defmacro scala-mode:make-local-variables (&rest quoted-names)
  (cons 'progn (mapcar #'(lambda (quoted-name) `(make-local-variable ,quoted-name)) quoted-names)))

(defun scala-mode:find-tag ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let (from to)
    (when (and (progn
                 ;; Look at text around `point'.
                 (save-excursion
                   (if (< 0 (skip-chars-backward scala-syntax:opchar-group))
                       (if (= (char-before) ?_)
                           (skip-syntax-backward "w_"))
                     (skip-syntax-backward "w_"))
                   (setq from (point)))
                 (save-excursion
                   (skip-syntax-forward "w_.") (setq to (point)))
                 (save-excursion
                   (ignore-errors (scala-syntax:backward-sexp)) (setq from (max from (point))))
                 (save-excursion
                   (goto-char from)
                   (ignore-errors (scala-syntax:forward-sexp)) (setq to (min to (point))))
                 (> to from))
               (save-excursion
                 (goto-char from)
                 (and (looking-at scala-syntax:id-re)
                      (not (looking-at scala-syntax:keywords-unsafe-re)))))
      (buffer-substring-no-properties from to))))


(defun scala-mode:forward-sexp-function (&optional count)
  (unless count (setq count 1))
  (if (< count 0)
      (dotimes (n (abs count))
        (scala-syntax:backward-sexp))
    (dotimes (n count)
      (scala-syntax:forward-sexp))))

;;;###autoload
(defun scala-mode:set-scala-syntax-mode ()
  "Sets the syntax-table and other related variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table."
  (set-syntax-table scala-syntax:syntax-table)
  (scala-mode:make-local-variables
   'syntax-propertize-function
   'parse-sexp-lookup-properties
   'forward-sexp-function)

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t
        forward-sexp-function           'scala-mode:forward-sexp-function))

;;;###autoload
(defun scala-mode:goto-start-of-code ()
  "Go to the start of the real code in the file: object, class or trait."
  (interactive)
  (let* ((case-fold-search nil))
    (search-forward-regexp "\\([[:space:]]+\\|^\\)\\(class\\|object\\|trait\\)" nil t)
    (move-beginning-of-line nil)))

;;;###autoload
(define-derived-mode scala-mode prog-mode "Scala"
  "Major mode for editing scala code.

When started, runs `scala-mode-hook'.

\\{scala-mode-map}"
  :syntax-table scala-syntax:syntax-table
;  :group
;  :abbrev

  (scala-mode:make-local-variables
   'post-self-insert-hook
   'syntax-propertize-function
   'font-lock-syntactic-face-function
   'font-lock-defaults
   'paragraph-start
   'paragraph-separate
   'parse-sexp-lookup-properties
   'fill-paragraph-function
   'adaptive-fill-function
   'adaptive-fill-first-line-regexp
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
   'forward-sexp-function
   'find-tag-default-function
   'indent-line-function
   'fixup-whitespace
   'delete-indentation
   'indent-tabs-mode
   'imenu-create-index-function
   'beginning-of-defun-function
   'end-of-defun-function)

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq scala-mode:debug-messages       nil

        syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        font-lock-defaults              '(scala-font-lock:keywords
                                          nil)
        font-lock-syntactic-face-function 'scala-font-lock:syntactic-face-function

        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        paragraph-start                 scala-paragraph:paragraph-start-re
        paragraph-separate              scala-paragraph:paragraph-separate-re
        fill-paragraph-function         'scala-paragraph:fill-paragraph
        adaptive-fill-function          'scala-paragraph:fill-function
        adaptive-fill-first-line-regexp scala-paragraph:fill-first-line-re
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)[ \t]*"
        comment-column                  0
        comment-multi-line              t

        forward-sexp-function           'scala-mode:forward-sexp-function
        find-tag-default-function       'scala-mode:find-tag
        indent-line-function            'scala-indent:indent-line
        fixup-whitespace                'scala-indent:fixup-whitespace
        delete-indentation              'scala-indent:join-line
        indent-tabs-mode                nil
	beginning-of-defun-function     #'scala-syntax:beginning-of-definition
	end-of-defun-function           #'scala-syntax:end-of-definition
	imenu-create-index-function     #'scala-imenu:create-imenu-index)
  (use-local-map scala-mode-map)
  ;; add indent functionality to some characters
  (scala-mode-map:add-remove-indent-hook)
  (scala-mode-map:add-self-insert-hooks))

;; Attach .scala files to the scala-mode
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
  (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8))

(provide 'scala-mode)
;;; scala-mode.el ends here
