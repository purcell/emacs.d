;;----------------------------------------------------------------------------
;; Provide a version of Emacs 24's 'string-prefix-p in older emacsen
;;----------------------------------------------------------------------------
(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
    (eq t (compare-strings str1 nil nil
                           str2 0 (length str1) ignore-case))))


