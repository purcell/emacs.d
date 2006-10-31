;; useful colors

(cond
 ((x-display-color-p)
  (cond
   ((not (memq 'font-lock-type-face (face-list)))
    ; make the necessary faces
    (make-face 'Firebrick)
    (set-face-foreground 'Firebrick "Firebrick")
    (make-face 'RosyBrown)
    (set-face-foreground 'RosyBrown "RosyBrown")
    (make-face 'Purple)
    (set-face-foreground 'Purple "Purple")
    (make-face 'MidnightBlue)
    (set-face-foreground 'MidnightBlue "MidnightBlue")
    (make-face 'DarkGoldenRod)
    (set-face-foreground 'DarkGoldenRod "DarkGoldenRod")
    (make-face 'DarkOliveGreen)
    (set-face-foreground 'DarkOliveGreen "DarkOliveGreen4")
    (make-face 'CadetBlue)
    (set-face-foreground 'CadetBlue "CadetBlue")
    ; assign them as standard faces
    (setq font-lock-comment-face 'Firebrick)
    (setq font-lock-string-face 'RosyBrown)
    (setq font-lock-keyword-face 'Purple)
    (setq font-lock-function-name-face 'MidnightBlue)
    (setq font-lock-variable-name-face 'DarkGoldenRod)
    (setq font-lock-type-face 'DarkOliveGreen)
    (setq font-lock-reference-face 'CadetBlue)))
  ; extra faces for documention
  (make-face 'Stop)
  (set-face-foreground 'Stop "White")
  (set-face-background 'Stop "Red")
  (make-face 'Doc)
  (set-face-foreground 'Doc "Red")
  (setq font-lock-stop-face 'Stop)
  (setq font-lock-doccomment-face 'Doc)
))

; The same definition is in caml.el:
; we don't know in which order they will be loaded.
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defconst caml-font-lock-keywords
  (list
;stop special comments
   '("\\(^\\|[^\"]\\)\\((\\*\\*/\\*\\*)\\)"
     2 font-lock-stop-face)
;doccomments
   '("\\(^\\|[^\"]\\)\\((\\*\\*[^*]*\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-doccomment-face)
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
                 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char
                 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
         'font-lock-string-face)
;modules and constructors
   '("`?\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (cons (concat
          "\\<\\(a\\(nd\\|s\\)\\|c\\(onstraint\\|lass\\)"
          "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
          "\\|in\\(herit\\|itializer\\)?\\|let"
          "\\|m\\(ethod\\|utable\\|odule\\)"
          "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
          "\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>")
         'font-lock-type-face)
;blocking
   '("\\<\\(begin\\|end\\|object\\|s\\(ig\\|truct\\)\\)\\>"
     . font-lock-keyword-face)
;control
   (cons (concat
          "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|i\\(f\\|gnore\\)"
          "\\|lazy\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
          "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
          "\\|\|\\|->\\|&\\|#")
         'font-lock-reference-face)
   '("\\<raise\\>" . font-lock-comment-face)
;labels (and open)
   '("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]" 1
     font-lock-variable-name-face)
   '("\\<\\(assert\\|open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
     . font-lock-variable-name-face)))

(defconst inferior-caml-font-lock-keywords
  (append
   (list
;inferior
    '("^[#-]" . font-lock-comment-face))
   caml-font-lock-keywords))

;; font-lock commands are similar for caml-mode and inferior-caml-mode
(setq caml-mode-hook
      '(lambda ()
         (cond
          ((fboundp 'global-font-lock-mode)
           (make-local-variable 'font-lock-defaults)
           (setq font-lock-defaults
                 '(caml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
          (t
           (setq font-lock-keywords caml-font-lock-keywords)))
         (make-local-variable 'font-lock-keywords-only)
         (setq font-lock-keywords-only t)
         (font-lock-mode 1)))

(defun inferior-caml-mode-font-hook ()
  (cond
   ((fboundp 'global-font-lock-mode)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
          '(inferior-caml-font-lock-keywords
            nil nil ((?' . "w") (?_ . "w")))))
   (t
    (setq font-lock-keywords inferior-caml-font-lock-keywords)))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (font-lock-mode 1))

(add-hook 'inferior-caml-mode-hooks 'inferior-caml-mode-font-hook)

(provide 'caml-font)
