;;----------------------------------------------------------------------------
;; Hook slime's completion into hippie-expand
;;
;; Usage:
;;   (require 'hippie-expand-slime)
;;   (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
;;   (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand))
;;----------------------------------------------------------------------------

(defun try-expand-slime (old &optional complete-fn)
  (let ((complete-fn (or complete-fn 'slime-simple-completions)))
    (if (not old)
        (progn
          (he-init-string (slime-symbol-start-pos) (slime-symbol-end-pos))
          (message "he-search-string: %s" he-search-string)
          (if  (not (equal he-search-string ""))
              (setq he-expand-list
                    (sort (car (funcall complete-fn he-search-string))
                          'string-lessp))
            (setq he-expand-list ())))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun try-expand-slime-fuzzy (old)
  (try-expand-slime old 'slime-fuzzy-completions))


(defun set-up-slime-hippie-expand (&optional fuzzy)
  "Add an optionally-fuzzy slime completion function to the front of
`hippie-expand-try-functions-list' for the current buffer."
  (interactive)
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               (if fuzzy 'try-expand-slime-fuzzy 'try-expand-slime)
               t))


(provide 'hippie-expand-slime)