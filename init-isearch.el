(defun call-with-current-isearch-string-as-regex (f)
  (let ((case-fold-search isearch-case-fold-search))
    (funcall f (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (call-with-current-isearch-string-as-regex 'occur)))

;; or fire up "all"
(define-key isearch-mode-map (kbd "C-l")
  (lambda ()
    (interactive)
    (call-with-current-isearch-string-as-regex 'all)))

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

(provide 'init-isearch)
