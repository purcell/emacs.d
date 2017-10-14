;;; cl-lib-highlight.el --- full cl-lib font-lock highlighting

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/cl-lib-highlight
;; Package-Version: 20140127.1312
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.3"))

;;; Commentary:

;; After load, run `cl-lib-highlight-initialize'.

;; Run `cl-lib-highlight-warn-cl-initialize' to mark deprecated cl
;; function/macro usage with with the `cl-lib-highlight-deprecated'
;; face.

;;; Code:

(require 'cl-lib)

(defgroup cl-lib-highlight nil
  "Adds special syntax highlighting to cl-lib macros."
  :group 'font-lock)

(defvar cl-lib-highlight-ignored
  '(cl-psetf cl-psetq cl-load-time-value cl-rotatef cl-pop2 cl-callf
    cl-do-all-symbols cl-multiple-value-setq cl-shiftf cl-callf2
    cl-function cl-do-symbols cl-pushnew cl-incf cl-remf cl-decf)
  "Macros that don't get any special treatment. These would show
up in an automatically generated list but shouldn't be highlighted.")

(defvar cl-lib-highlight-keywords
  '(cl-letf cl-declaim cl-etypecase cl-case cl-letf*  cl-progv cl-ecase
            cl-flet cl-symbol-macrolet cl-dotimes cl-labels cl-locally
            cl-loop cl-macrolet cl-do* cl-return cl-eval-when cl-typecase
            cl-return-from cl-do cl-block cl-the cl-flet* cl-dolist
            cl-destructuring-bind cl-multiple-value-bind cl-declare)
  "Macros that get a simple `font-lock-keyword-face'.")

(defvar cl-lib-highlight-warnings
  '(cl-assert cl-check-type)
  "Macros that get `font-lock-warning-face'.")

(defvar cl-lib-highlight-defs
  '(cl-defun cl-defmacro cl-define-compiler-macro cl-defsubst)
  "Macros who define a name, given `font-lock-function-name-face'.")

(defvar cl-lib-highlight-types
  '(cl-deftype cl-defstruct)
  "Macros who define a type, given `font-lock-type-face'.")

(defvar cl-lib-highlight-cl
  '(acons adjoin assert assoc* assoc-if assoc-if-not block caaaar caaadr
          caaar caadar caaddr caadr cadaar cadadr cadar caddar cadddr caddr
          callf callf2 case cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar
          cddadr cddar cdddar cddddr cdddr ceiling* check-type coerce
          compiler-macroexpand concatenate copy-list copy-seq count count-if
          count-if-not decf declaim define-compiler-macro define-modify-macro
          define-setf-expander define-setf-method defmacro* defsetf defstruct
          defsubst* deftype defun* delete* delete-duplicates delete-if
          delete-if-not destructuring-bind do do* do-all-symbols do-symbols
          ecase eighth endp equalp etypecase eval-when evenp every fifth fill
          find find-if find-if-not first flet floatp-safe floor* fourth
          function* gcd gensym gentemp get* getf incf intersection isqrt
          labels lcm ldiff letf letf* lexical-let lexical-let* list*
          list-length load-time-value locally loop macrolet make-random-state
          map mapcan mapcar* mapcon mapl maplist member* member-if
          member-if-not merge minusp mismatch mod* multiple-value-apply
          multiple-value-bind multiple-value-call multiple-value-list
          multiple-value-setq nintersection ninth notany notevery nreconc
          nset-difference nset-exclusive-or nsublis nsubst nsubst-if
          nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not
          nth-value nunion oddp pairlis plusp position position-if
          position-if-not proclaim progv psetf psetq pushnew random*
          random-state-p rassoc* rassoc-if rassoc-if-not reduce rem* remf
          remove* remove-duplicates remove-if remove-if-not remprop replace
          rest return return-from revappend rotatef round* search second
          set-difference set-exclusive-or seventh shiftf signum sixth some
          sort* stable-sort sublis subseq subsetp subst subst-if subst-if-not
          substitute substitute-if substitute-if-not svref symbol-macrolet
          tailp tenth the third tree-equal truncate* typecase typep union
          values values-list)
  "List of depreciated cl package functions to be warning-highlighted.")

;;;###autoload
(defun cl-lib-highlight-initialize ()
  "Add all cl-lib font lock highlighting to `emacs-lisp-mode'."
  (interactive)
  (cl-labels ((opt (syms) (regexp-opt (mapcar #'symbol-name syms) t)))
    (let ((defs (list (concat "(" (opt cl-lib-highlight-defs) "\\_>"
                              "\\s-*" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
                      '(1 font-lock-keyword-face)
                      '(2 font-lock-function-name-face nil t)))
          (types (list (concat "(" (opt cl-lib-highlight-types) "\\_>"
                               "\\s-*" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
                       '(1 font-lock-keyword-face)
                       '(2 font-lock-type-face nil t)))
          (warnings (list (concat "(" (opt cl-lib-highlight-warnings) "\\_>")
                          '(1 font-lock-warning-face)))
          (keywords (list (concat "(" (opt cl-lib-highlight-keywords) "\\_>")
                          '(1 font-lock-keyword-face))))
      (font-lock-add-keywords 'emacs-lisp-mode
                              (list defs types warnings keywords))
      (font-lock-add-keywords 'lisp-interaction-mode
                              (list defs types warnings keywords)))))

(defface cl-lib-highlight-deprecated
  '((t :inherit warning))
  "Face for deprecated cl functions and macros."
  :group 'cl-lib-highlight)

(defun cl-lib-highlight-warn-cl-initialize ()
  "Mark all of the deprecated cl functions with `cl-lib-warning'."
  (interactive)
  (let* ((opt (regexp-opt (mapcar #'symbol-name cl-lib-highlight-cl) t))
         (old (list (concat "\\(?:#'\\|(\\)" opt "\\_>")
                    '(1 'cl-lib-highlight-deprecated))))
    (font-lock-add-keywords 'emacs-lisp-mode (list old))
    (font-lock-add-keywords 'lisp-interaction-mode (list old))))

(provide 'cl-lib-highlight)

;;; cl-lib-highlight.el ends here
