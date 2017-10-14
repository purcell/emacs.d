;;; haskell-font-lock.el --- Font locking module for Haskell Mode -*- lexical-binding: t -*-

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'haskell-compat)
(require 'haskell-lexeme)
(require 'font-lock)

;;;###autoload
(defgroup haskell-appearance nil
  "Haskell Appearance."
  :group 'haskell)


(defcustom haskell-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises with regards to layout."
  :group 'haskell-appearance
  :type 'boolean)

(defcustom haskell-font-lock-symbols-alist
  '(("\\" . "λ")
    ("not" . "¬")
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("()" . "∅")
    ("==" . "≡")
    ("/=" . "≢")
    (">=" . "≥")
    ("<=" . "≤")
    ("!!" . "‼")
    ("&&" . "∧")
    ("||" . "∨")
    ("sqrt" . "√")
    ("undefined" . "⊥")
    ("pi" . "π")
    ("~>" . "⇝") ;; Omega language
    ;; ("~>" "↝") ;; less desirable
    ("-<" . "↢") ;; Paterson's arrow syntax
    ;; ("-<" "⤙") ;; nicer but uncommon
    ("::" . "∷")
    ("." "∘" ; "○"
     ;; Need a predicate here to distinguish the . used by
     ;; forall <foo> . <bar>.
     haskell-font-lock-dot-is-not-composition)
    ("forall" . "∀"))
  "Alist mapping Haskell symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the Haskell symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'haskell-appearance)

(defcustom haskell-font-lock-keywords
  ;; `as', `hiding', and `qualified' are part of the import
  ;; spec syntax, but they are not reserved.
  ;; `_' can go in here since it has temporary word syntax.
  '("case" "class" "data" "default" "deriving" "do"
    "else" "if" "import" "in" "infix" "infixl"
    "infixr" "instance" "let" "module" "mdo" "newtype" "of"
    "rec" "pattern" "proc" "then" "type" "where" "_")
  "Identifiers treated as reserved keywords in Haskell."
  :group 'haskell-appearance
  :type '(repeat string))


(defun haskell-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (null (char-before start))
              (string= " " (string (char-before start))))))))

(defvar haskell-yesod-parse-routes-mode-keywords
  '(("^\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?"
     (1 'font-lock-string-face)
     (2 'haskell-constructor-face nil lax))))

(define-derived-mode haskell-yesod-parse-routes-mode text-mode "Yesod parseRoutes mode"
  "Mode for parseRoutes from Yesod."
  (setq-local font-lock-defaults '(haskell-yesod-parse-routes-mode-keywords t t nil nil)))

(defcustom haskell-font-lock-quasi-quote-modes
  `(("hsx" . xml-mode)
    ("hamlet" . shakespeare-hamlet-mode)
    ("shamlet" . shakespeare-hamlet-mode)
    ("whamlet" . shakespeare-hamlet-mode)
    ("xmlQQ" . xml-mode)
    ("xml" . xml-mode)
    ("cmd" . shell-mode)
    ("sh_" . shell-mode)
    ("jmacro" . javascript-mode)
    ("jmacroE" . javascript-mode)
    ("r" . ess-mode)
    ("rChan" . ess-mode)
    ("sql" . sql-mode)
    ("parseRoutes" . haskell-yesod-parse-routes-mode))
  "Mapping from quasi quoter token to fontification mode.

If a quasi quote is seen in Haskell code its contents will have
font faces assigned as if respective mode was enabled."
  :group 'haskell-appearance
  :type '(repeat (cons string symbol)))

;;;###autoload
(defface haskell-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Haskell keywords."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-type-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Haskell types"
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-constructor-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Haskell constructors."
  :group 'haskell-appearance)

;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defface haskell-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight Haskell definitions."
  :group 'haskell-appearance)

;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `haskell-definition-face'.
;;;###autoload
(defface haskell-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight Haskell operators."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-pragma-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight Haskell pragmas ({-# ... #-})."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-liquid-haskell-annotation-face
  '((t :inherit haskell-pragma-face))
  "Face used to highlight LiquidHaskell annotations ({-@ ... @-})."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-literate-comment-face
  '((t :inherit font-lock-doc-face))
  "Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them."
  :group 'haskell-appearance)

(defface haskell-quasi-quote-face
  '((t :inherit font-lock-string-face))
  "Generic face for quasiquotes.

Some quote types are fontified according to other mode defined in
`haskell-font-lock-quasi-quote-modes'."
  :group 'haskell-appearance)

(defun haskell-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun haskell-font-lock-symbols-keywords ()
  (when (and haskell-font-lock-symbols
             haskell-font-lock-symbols-alist)
    `((,(regexp-opt (mapcar 'car haskell-font-lock-symbols-alist) t)
       (0 (haskell-font-lock-compose-symbol ',haskell-font-lock-symbols-alist)
          ;; In Emacs-21, if the `override' field is nil, the face
          ;; expressions is only evaluated if the text has currently
          ;; no face.  So force evaluation by using `keep'.
          keep)))))

(defun haskell-font-lock--forward-type (&optional ignore)
  "Find where does this type declaration end.

Moves the point to the end of type declaration. It should be
invoked with point just after one of type introducing keywords
like ::, class, instance, data, newtype, type."
  (interactive)
  (let ((cont t)
        (end (point))
        (token nil)
        ;; we are starting right after ::
        (last-token-was-operator t)
        (last-token-was-newline nil)
        (open-parens 0))
    (while cont
      (setq token (haskell-lexeme-looking-at-token 'newline))

      (cond
       ((null token)
        (setq cont nil))
       ((member token '(newline))
        (setq last-token-was-newline (not last-token-was-operator))
        (setq end (match-end 0))
        (goto-char (match-end 0)))
       ((member (match-string-no-properties 0)
                    '(")" "]" "}"))
        (setq open-parens (1- open-parens))
        (if (< open-parens 0)
            ;; unmatched closing parenthesis closes type declaration
            (setq cont nil)
          (setq end (match-end 0))
          (goto-char end))
        (setq last-token-was-newline nil))
       ((and (member (match-string-no-properties 0)
                     '("," ";" "|"))
             (not (member (match-string-no-properties 0) ignore)))
        (if (equal 0 open-parens)
            (setq cont nil)
          (setq last-token-was-operator t)
          (setq end (match-end 0))
          (goto-char end))
        (setq last-token-was-newline nil))
       ((and (or (member (match-string-no-properties 0)
                         '("<-" "=" "←"))
                 (member (match-string-no-properties 0) haskell-font-lock-keywords))
             (not (member (match-string-no-properties 0) ignore)))
        (setq cont nil)
        (setq last-token-was-newline nil))
       ((member (match-string-no-properties 0)
                '("(" "[" "{"))
        (if last-token-was-newline
            (setq cont nil)
          (setq open-parens (1+ open-parens))
          (setq end (match-end 0))
          (goto-char end)
          (setq last-token-was-newline nil)))
       ((member token '(qsymid char string number template-haskell-quote template-haskell-quasi-quote))
        (setq last-token-was-operator (member (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                                              '(varsym consym)))
        (if (and (not last-token-was-operator) last-token-was-newline)
            (setq cont nil)

          (goto-char (match-end 0))
          (setq end (point)))
        (setq last-token-was-newline nil))
       ((member token '(comment nested-comment literate-comment))
        (goto-char (match-end 0))
        (setq end (point)))
       (t
        (goto-char (match-end 0))
        (setq end (point))
        (setq last-token-was-newline nil))))
    (goto-char end)))


(defun haskell-font-lock--select-face-on-type-or-constructor ()
  "Private function used to select either type or constructor face
on an uppercase identifier."
  (cl-case (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
    (varid (let ((word (match-string-no-properties 0)))
             (cond
              ((member word haskell-font-lock-keywords)
               ;; Note: keywords parse as keywords only when not qualified.
               ;; GHC parses Control.let as a single but illegal lexeme.
               (when (member word '("class" "instance" "type" "data" "newtype"))
                 (save-excursion
                   (goto-char (match-end 0))
                   (save-match-data
                     (haskell-font-lock--forward-type
                      (cond
                       ((member word '("class" "instance"))
                        '("|"))
                       ((member word '("type"))
                        ;; Need to support 'type instance'
                        '("=" "instance")))))
                   (add-text-properties (match-end 0) (point) '(font-lock-multiline t haskell-type t))))
               'haskell-keyword-face)
              ((member word '("forall"))
               (when (get-text-property (match-beginning 0) 'haskell-type)
                 'haskell-keyword-face)))))
    (conid (if (get-text-property (match-beginning 0) 'haskell-type)
               'haskell-type-face
             'haskell-constructor-face))
    (varsym (unless (and (member (match-string 0) '("-" "+" "."))
                         (equal (string-to-syntax "w") (syntax-after (match-beginning 0))))
              ;; We need to protect against the case of
              ;; plus, minus or dot inside a floating
              ;; point number.
              'haskell-operator-face))
    (consym (if (not (member (match-string 1) '("::" "∷")))
                (if (get-text-property (match-beginning 0) 'haskell-type)
                    'haskell-type-face
                  'haskell-constructor-face)
              (save-excursion
                (goto-char (match-end 0))
                (save-match-data
                  (haskell-font-lock--forward-type))
                (add-text-properties (match-end 0) (point) '(font-lock-multiline t haskell-type t)))
              'haskell-operator-face))))

(defun haskell-font-lock--put-face-on-type-or-constructor ()
  "Private function used to put either type or constructor face
on an uppercase identifier."
  (let ((face (haskell-font-lock--select-face-on-type-or-constructor)))
    (when (and face
               (not (text-property-not-all (match-beginning 0) (match-end 0) 'face nil)))
      (put-text-property (match-beginning 0) (match-end 0) 'face face))))


(defun haskell-font-lock-keywords ()
  ;; this has to be a function because it depends on global value of
  ;; `haskell-font-lock-symbols'
  "Generate font lock eywords."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix "^\\(?:> ?\\)?")

         (varid "[[:lower:]_][[:alnum:]'_]*")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "'?[[:upper:]][[:alnum:]'_]*")
         (sym "\\s.+")

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\(?:\\s-*,\\s-*" varid "\\)*" "\\)"
                  ;; optionally allow for a single newline after identifier
                  "\\(\\s-+\\|\\s-*[\n]\\s-+\\)"
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-bangpat
          (concat line-prefix "\\(" varid "\\)\\s-*!"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#\\(?:[^\\\n]\\|\\\\\\(?:.\\|\n\\|\\'\\)\\)*\\(?:\n\\|\\'\\)" 0 'font-lock-preprocessor-face t)

            ,@(haskell-font-lock-symbols-keywords)

            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?\\(?:\"[^\"]*\"[\t ]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax))

            ;; Special case for `type family' and `data family'.
            ;; `family' is only reserved in these contexts.
            ("\\<\\(type\\|data\\)[ \t]+\\(family\\>\\)"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax))

            ;; Special case for `type role'
            ;; `role' is only reserved in this context.
            ("\\<\\(type\\)[ \t]+\\(role\\>\\)"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (unless (member (match-string 1) haskell-font-lock-keywords)
                                'haskell-definition-face)))
            (,topdecl-var2 (2 (unless (member (match-string 2) haskell-font-lock-keywords)
                                'haskell-definition-face)))
            (,topdecl-bangpat  (1 (unless (member (match-string 1) haskell-font-lock-keywords)
                                'haskell-definition-face)))
            (,topdecl-sym  (2 (unless (member (match-string 2) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))
            (,topdecl-sym2 (1 (unless (member (match-string 1) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 'haskell-constructor-face)
            ("\\[\\]" 0 'haskell-constructor-face)

            ("`"
             (0 (if (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
                    (parse-partial-sexp (point) (point-max) nil nil (syntax-ppss)
                                        'syntax-table)
                  (when (save-excursion
                          (goto-char (match-beginning 0))
                          (haskell-lexeme-looking-at-backtick))
                    (goto-char (match-end 0))
                    (unless (text-property-not-all (match-beginning 1) (match-end 1) 'face nil)
                      (put-text-property (match-beginning 1) (match-end 1) 'face 'haskell-operator-face))
                    (unless (text-property-not-all (match-beginning 2) (match-end 2) 'face nil)
                      (put-text-property (match-beginning 2) (match-end 2) 'face 'haskell-operator-face))
                    (unless (text-property-not-all (match-beginning 4) (match-end 4) 'face nil)
                      (put-text-property (match-beginning 4) (match-end 4) 'face 'haskell-operator-face))
                    (add-text-properties
                     (match-beginning 0) (match-end 0)
                     '(font-lock-fontified t fontified t font-lock-multiline t))))))

            (,haskell-lexeme-idsym-first-char
             (0 (if (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
                    (parse-partial-sexp (point) (point-max) nil nil (syntax-ppss)
                                        'syntax-table)
                  (when (save-excursion
                          (goto-char (match-beginning 0))
                          (haskell-lexeme-looking-at-qidsym))
                    (goto-char (match-end 0))
                    ;; note that we have to put face ourselves here because font-lock
                    ;; will use match data from the original matcher
                    (haskell-font-lock--put-face-on-type-or-constructor)))))))
    keywords))


(defun haskell-font-lock-fontify-block (lang-mode start end)
  "Fontify a block as LANG-MODE."
  (let ((string (buffer-substring-no-properties start end))
        (modified (buffer-modified-p))
        (org-buffer (current-buffer)) pos next)
    (remove-text-properties start end '(face nil))
    (with-current-buffer
        (get-buffer-create
         (concat " haskell-font-lock-fontify-block:" (symbol-name lang-mode)))
      (delete-region (point-min) (point-max))
      (insert string " ") ;; so there's a final property change
      (cl-letf (((symbol-function 'message)
                 (lambda (_fmt &rest _args))))
        ;; silence messages
        (unless (eq major-mode lang-mode) (funcall lang-mode))
        (font-lock-ensure))
      (setq pos (point-min))
      (while (setq next (next-single-property-change pos 'face))
        (put-text-property
         (+ start (1- pos)) (1- (+ start next)) 'face
         (or (get-text-property pos 'face) 'default) org-buffer)
        (setq pos next))
      (unless (equal pos (point-max))
        (put-text-property
         (+ start (1- pos)) (1- (+ start (point-max))) 'face
         'default org-buffer)))
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

(defun haskell-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Haskell."
  (cond
   ((nth 3 state)
    (if (equal ?| (nth 3 state))
        ;; find out what kind of QuasiQuote is this
        (let* ((qqname (save-excursion
                        (goto-char (nth 8 state))
                        (skip-syntax-backward "w._")
                        (buffer-substring-no-properties (point) (nth 8 state))))
               (lang-mode (cdr (assoc qqname haskell-font-lock-quasi-quote-modes))))

          (if (and lang-mode
                   (fboundp lang-mode))
              (save-excursion
                ;; find the end of the QuasiQuote
                (parse-partial-sexp (point) (point-max) nil nil state
                                    'syntax-table)
                (haskell-font-lock-fontify-block lang-mode (1+ (nth 8 state)) (1- (point)))
                ;; must return nil here so that it is not fontified again as string
                nil)
            ;; fontify normally as string because lang-mode is not present
            'haskell-quasi-quote-face))
      (save-excursion
        (let
            ((state2
              (parse-partial-sexp (point) (point-max) nil nil state
                                  'syntax-table))
             (end-of-string (point)))

          (put-text-property (nth 8 state) (point)
                             'face 'font-lock-string-face)


          (if (or (equal t (nth 3 state)) (nth 3 state2))
              ;; This is an unterminated string constant, use warning
              ;; face for the opening quote.
              (put-text-property (nth 8 state) (1+ (nth 8 state))
                                 'face 'font-lock-warning-face))

          (goto-char (1+ (nth 8 state)))
          (while (re-search-forward "\\\\" end-of-string t)

            (goto-char (1- (point)))

            (if (looking-at haskell-lexeme-string-literal-inside-item)
                (goto-char (match-end 0))

              ;; We are looking at an unacceptable escape
              ;; sequence. Use warning face to highlight that.
              (put-text-property (point) (1+ (point))
                                 'face 'font-lock-warning-face)
              (goto-char (1+ (point)))))))
      ;; must return nil here so that it is not fontified again as string
      nil))
   ;; Detect literate comment lines starting with syntax class '<'
   ((save-excursion
      (goto-char (nth 8 state))
      (equal (string-to-syntax "<") (syntax-after (point))))
    'haskell-literate-comment-face)
   ;; Detect pragmas. A pragma is enclosed in special comment
   ;; delimiters {-# .. #-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "{-#")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at-p "#-}")))
    'haskell-pragma-face)
   ;; Detect Liquid Haskell annotations enclosed in special comment
   ;; delimiters {-@ .. @-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "{-@")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at-p "@-}")))
    'haskell-liquid-haskell-annotation-face)
   ;; Haddock comment start with either "-- [|^*$]" or "{- ?[|^*$]"
   ;; (note space optional for nested comments and mandatory for
   ;; double dash comments).
   ;;
   ;; Haddock comment will also continue on next line, provided:
   ;; - current line is a double dash haddock comment
   ;; - next line is also double dash comment
   ;; - there is only whitespace between
   ;;
   ;; We recognize double dash haddock comments by property
   ;; 'font-lock-doc-face attached to newline. In case of {- -}
   ;; comments newline is outside of comment.
   ((save-excursion
      (goto-char (nth 8 state))
      (or (looking-at-p "\\(?:{- ?\\|-- \\)[|^*$]")
          (and (looking-at-p "--")            ; are we at double dash comment
               (forward-line -1)              ; this is nil on first line
               (eq (get-text-property (line-end-position) 'face)
                   'font-lock-doc-face)       ; is a doc face
               (forward-line)
               (skip-syntax-forward "-")      ; see if there is only whitespace
               (eq (point) (nth 8 state)))))  ; we are back in position
    ;; Here we look inside the comment to see if there are substrings
    ;; worth marking inside we try to emulate as much of haddock as
    ;; possible.  First we add comment face all over the comment, then
    ;; we add special features.
    (let ((beg (nth 8 state))
          (end (save-excursion
                 (parse-partial-sexp (point) (point-max) nil nil state
                                     'syntax-table)
                 (point)))
          (emphasis-open-point nil)
          (strong-open-point nil))
      (put-text-property beg end 'face 'font-lock-doc-face)

      (when (fboundp 'add-face-text-property)
        ;; `add-face-text-property' is not defined in Emacs 23

        ;; iterate over chars, take escaped chars unconditionally
        ;; mark when a construct is opened, close and face it when
        ;; it is closed

        (save-excursion
          (while (< (point) end)
            (if (looking-at "__\\|\\\\.\\|\\\n\\|[/]")
                (progn
                  (cond
                   ((equal (match-string 0) "/")
                    (if emphasis-open-point
                        (progn
                          (add-face-text-property emphasis-open-point (match-end 0)
                                                  '(:slant italic))
                          (setq emphasis-open-point nil))
                      (setq emphasis-open-point (point))))
                   ((equal (match-string 0) "__")
                    (if strong-open-point
                        (progn
                          (add-face-text-property strong-open-point (match-end 0)
                                                  '(:weight bold))
                          (setq strong-open-point nil))
                      (setq strong-open-point (point))))
                   (t
                    ;; this is a backslash escape sequence, skip over it
                    ))
                  (goto-char (match-end 0)))
              ;; skip chars that are not interesting
              (goto-char (1+ (point)))
              (skip-chars-forward "^_\\\\/" end))))))
    nil)
   (t 'font-lock-comment-face)))

(defun haskell-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Haskell."
  (setq-local font-lock-defaults
              '((haskell-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition)))))

(defun haskell-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;; Provide ourselves:

(provide 'haskell-font-lock)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 8
;; End:

;;; haskell-font-lock.el ends here
