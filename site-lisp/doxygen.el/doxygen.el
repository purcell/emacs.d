;;; doxygen.el --- support for doxygen style comments

;; Copyright (C) 2000-2001 Basis Technology, Corp.

;; Author: Tom Emerson <tree@basistech.com>
;; Keywords: languages comments doxygen
;; Version: 1.1

;;; Commentary:

;; TODO:
;;
;; - better documentation
;; - key bindings, perhaps
;; - allow field insertion, a la BibTeX mode
;;
;; ACKNOWLEDGEMENTS:
;;
;; - John Sturton <john.sturton@sescoi.fr> for finding bugs in the function
;;   recognizer and sending patches to these.
;;
;; - Marcelo Matus <matus@ece.arizona.edu> for extensions to the function
;;   recognizer and to the comment insertion functions.


;;; Code:

;; default doxygen style
(defcustom doxygen-comment-begin "/**\n" "begin of general comment")
(defcustom doxygen-comment-end " */" "end of general comment")
(defcustom doxygen-comment-line-prefix " *" "prefix of comment line")
(defcustom doxygen-variable-prefix "@" "prefix of the variable")
(defcustom doxygen-comment-member-group-begin "/*@{*/\n" "begin of member group comment")
(defcustom doxygen-comment-member-group-end "/*@}*/" "end of member group comment")
(defcustom doxygen-enum-comment-begin "/**<" "begin of enum comment")
(defcustom doxygen-enum-comment-end "*/" "end of enum comment")

;; You can enable another style by copying below lines into .emacs
;; (setq doxygen-comment-begin "/*!\n")
;; (setq doxygen-comment-end "*/")
;; (setq doxygen-comment-line-prefix "")
;; (setq doxygen-variable-prefix "\\")
;; (setq doxygen-comment-member-group-begin "//@{\n")
;; (setq doxygen-comment-member-group-end "//@}")
;; (setq doxygen-enum-comment-begin "//!<")
;; (setq doxygen-enum-comment-end "")

(defvar doxygen-date-format "%Y-%m-%d"
  (format "The format used to display dates when using the %sdate command." doxygen-variable-prefix))

(defun doxygen-insert-comment()
  "Insert a generic Doxygen comment block at point, including brief
and long sections."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (point)))
        (insert (concat doxygen-comment-begin
                        doxygen-comment-line-prefix "\n"
                        doxygen-comment-end))
        (let ((end (point)))
          (indent-region start end nil)))))
  (end-of-line))

(defun doxygen-insert-file-comment ()
  "Insert a Doxygen file comment at point."
  (interactive "*")
  (let ((file-name (if (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name))
                     "untitled"))
        (date-string (format-time-string doxygen-date-format))
        (who (user-full-name)))
    (insert (format (concat doxygen-comment-begin
                            doxygen-comment-line-prefix " " doxygen-variable-prefix "file   %s\n"
                            doxygen-comment-line-prefix " " doxygen-variable-prefix "brief  \n"
                            doxygen-comment-line-prefix "\n"
                            doxygen-comment-line-prefix " <long description>\n"
                            doxygen-comment-line-prefix "\n"
                            doxygen-comment-line-prefix " " doxygen-variable-prefix "author %s\n"
                            doxygen-comment-line-prefix " " doxygen-variable-prefix "date   %s\n"
                            doxygen-comment-end)
                    file-name who date-string))))


(defun doxygen-insert-function-comment ()
  "Insert a Doxygen comment for the function at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (point))
            (str "")
            )
        (let ((args (find-arg-list)))
          (setq str (concat str
                        doxygen-comment-begin
                        doxygen-comment-line-prefix " <long-description>\n"
                        doxygen-comment-line-prefix "\n"))
          (when (cdr (assoc 'args args))
            (setq str (concat str (dump-arguments (cdr (assoc 'args args))))))
          (unless (string= "void" (cdr (assoc 'return args)))
            (setq str (concat str doxygen-comment-line-prefix " " doxygen-variable-prefix "return <ReturnValue>\n")))
          (setq str (concat str doxygen-comment-end)))
        (insert str)
        (let ((end (point)))
          (untabify start end)))))
  (end-of-line))

(defun doxygen-insert-member-group-region (start end)
  "Make the current region a member group."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
                                        ; indent-according-to-mode doesn't work well here...
    (insert doxygen-comment-member-group-begin)
    (goto-char end)
    (end-of-line)
    (insert (concat "\n" doxygen-comment-member-group-end))))

(defun doxygen-insert-compound-comment ()
  "Insert a compound comment."
  (interactive "*")
  (let ((comment-start doxygen-enum-comment-begin)
        (comment-end doxygen-enum-comment-end))
    (indent-for-comment)))


;;; internal utility functions

(defun dump-arguments (arglist)
  "Insert a comment with the Doxygen comments for a function."
  (let ((str ""))
  (mapcar (function (lambda (x)
                      (setq str (concat str doxygen-comment-line-prefix (format (concat " " doxygen-variable-prefix "param %s\n")
                                      (extract-argument-name x))))))
          arglist)
  str
  ))

(defun extract-argument-name (arg)
  "Get the argument name from the argument string 'arg'."
                                        ; this does *not* work for function pointer arguments
  (if (string-match "\\([a-zA-Z0-9_]+\\)\\s-*\\(=\\s-*.+\\s-*\\)?$" arg)
      (substring arg (match-beginning 1) (match-end 1))
    arg))

(defun find-return-type ()
  "Extract the return type of a function.
   If the function is a constructor, it returns void."
  (interactive "*")
  (save-excursion
    (let ((start (point)))
      (search-forward "(")
      (let ((bound (point)))
        (goto-char start)
        (if (re-search-forward
             (concat
              "\\(virtual \|static \|const \\)*" ; opt. modifiers
              "\\([a-zA-Z0-9_:*]+\\)\\s-+[a-zA-Z0-9_:*]+\\s-*(") ; return type
             bound t)
            (buffer-substring (match-beginning 2)(match-end 2))
          "void")
        ))))

(defun find-arg-list ()
  "Extract various bits of information from a C or C++ function declaration"
  (interactive "*")
  (let ((return-type (find-return-type)))
    (save-excursion
      (if (re-search-forward (concat
                              "\\([a-zA-Z0-9_:]+\\)\\s-*("    ; function name
                              "\\([^)]*\\))")                 ; argument list
                             nil t)
          (list (cons 'return   return-type)
                (cons 'function (buffer-substring (match-beginning 1)
                                                  (match-end 1)))
                (cons 'args     (split-string
                                 (buffer-substring (match-beginning 2)
                                                   (match-end 2)) ",")))
        nil))))

(provide 'doxygen)

;;; doxygen.el ends here
