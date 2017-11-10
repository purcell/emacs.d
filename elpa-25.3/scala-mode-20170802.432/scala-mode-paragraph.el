;;; scala-mode-paragraph.el - Major mode for editing scala, paragraph
;;; detection and fill
;;; Copyright (c) 2012 Heikki Vesalainen For information on the License,
;;; see the LICENSE file

;;; Based on Scala Language Specification (SLS) Version 2.9

;;; Provides paragraph navigation and fill for scaladocs and
;;; multi-line strings.

(defconst scala-paragraph:paragraph-line-start-re
  (concat "\\(?:\\s-*"                 ; whitespace
          "\\(?://+\\|\\*\\|/\\*+"     ; comment start
          "\\||\\)?"                   ; multi-line margin |
          "\\s-*\\)"))                 ; whitespace

(defconst scala-paragraph:scaladoc-list-start-re
  (concat "\\(?:-"                     ; unordered liststs
          "\\|[1IiAa]\\."              ; ordered lists
          "\\)\\s-*"))

(defconst scala-paragraph:fill-first-line-re
  (concat "\\s-*\\(//+\\|\\*\\||\\)?\\s-*"
          "\\(?:" scala-paragraph:scaladoc-list-start-re "\\)?"))

(defconst scala-paragraph:paragraph-start-re
  (concat scala-paragraph:paragraph-line-start-re
          "\\(?:$"                ; empty line
          "\\|==*[^=]+==*[ ]*$"   ; headings
          "\\|"
          scala-paragraph:scaladoc-list-start-re
          "\\|{{{"                ; code block start
          "\\|}}}"                ; code block end
          "\\|@[a-zA-Z]+\\>"      ; annotations
          "\\)"
          "\\|\\(?:\\s-*\\*/\\)"  ; end of comment
          ))

(defconst scala-paragraph:paragraph-separate-re
  (concat scala-paragraph:paragraph-line-start-re
          "\\(?:$\\)"
          "\\|\\(?:\\s *\\*/\\)"    ; end of comment
          ))

(defun scala-paragraph:fill-function ()
  (let (fill)
    (save-restriction
      (save-excursion
        (widen)
        (beginning-of-line)
        (cond ((looking-at "\\s-*/?\\*+\\s-*")
               (setq fill (replace-regexp-in-string
                           "/\\*+"
                           (lambda (str) (if (= (length str) 3) "  *" " *"))
                           (match-string-no-properties 0)))
               (goto-char (match-end 0))
               (when (looking-at scala-paragraph:scaladoc-list-start-re)
                 (setq fill
                       (concat fill (make-string (- (match-end 0)
                                                    (match-beginning 0)) ?\s)))))
              ((or (re-search-forward "\"\"\"|" (line-end-position) t)
                   (and (eq (nth 3 (syntax-ppss)) t)
                        (re-search-forward "^\\s-*|" (line-end-position) t)))
               (setq fill (concat (make-string (- (current-column) 1) ?\s) "|"))
               (setq fill (concat fill (make-string (skip-syntax-forward " ") ?\s)))
               (when (looking-at scala-paragraph:scaladoc-list-start-re)
                 (setq fill
                       (concat fill (make-string (- (match-end 0)
                                                    (match-beginning 0)) ?\s))))))))
    fill))

(defun scala-paragraph:fill-paragraph (&rest args)
  ;; move to inside multi-line comment or multi-line string, if outside
  (when (looking-at "\\s-*\\(?:/\\**\\|\"\"\"\\)\\s-*")
    (goto-char (match-end 0)))
  (let ((state (syntax-ppss))
        (fill-paragraph-function
         ;; Avoid infinite recursion, set fill-paragraph-function to
         ;; nil if it is 'scala-paragraph:fill-paragraph
         (unless (eq fill-paragraph-function 'scala-paragraph:fill-paragraph)
           fill-paragraph-function)))
    (cond ((integerp (nth 4 state))
           ;; mask multi-line comments and fill
           (save-restriction
             (narrow-to-region (nth 8 state)
                               (save-excursion (goto-char (nth 8 state))
                                               (if (forward-comment 1)
                                                   (point)
                                                 (point-max))))
             (apply #'fill-paragraph args))
           t)
          ((eq (nth 4 state) t)
           ;; line comment, let normal fill-function handle this
           nil)
          ((eq (nth 3 state) t)
           ;; mask multi-line strings and fill.
           (save-restriction
             (narrow-to-region (nth 8 state)
                               (save-excursion (goto-char (nth 8 state))
                                               (or (ignore-errors
                                                     (forward-sexp)
                                                     (point))
                                                   (point-max))))
             (apply #'fill-paragraph args))
           t)
          ;; TODO: fill lists
          ;; the rest should not be filled (code, etc)
          (t t))))

(provide 'scala-mode-paragraph)
