(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(defun markdown-imenu-create-index ()
  (let* ((root '(nil . nil))
         cur-alist
         (cur-level 0)
         (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
         (empty-heading "-")
         (self-heading ".")
         hashes pos level heading)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern (point-max) t)
        (cond
         ((setq hashes (match-string-no-properties 2))
          (setq heading (match-string-no-properties 3)
                pos (match-beginning 1)
                level (length hashes)))
         ((setq heading (match-string-no-properties 4))
          (setq pos (match-beginning 4)
                level 1))
         ((setq heading (match-string-no-properties 5))
          (setq pos (match-beginning 5)
                level 2)))
        (let ((alist (list (cons heading pos))))
          (cond
           ((= cur-level level)		; new sibling
            (setcdr cur-alist alist)
            (setq cur-alist alist))
           ((< cur-level level)		; first child
            (dotimes (i (- level cur-level 1))
              (setq alist (list (cons empty-heading alist))))
            (if cur-alist
                (let* ((parent (car cur-alist))
                       (self-pos (cdr parent)))
                  (setcdr parent (cons (cons self-heading self-pos) alist)))
              (setcdr root alist))		; primogenitor
            (setq cur-alist alist)
            (setq cur-level level))
           (t				; new sibling of an ancestor
            (let ((sibling-alist (last (cdr root))))
              (dotimes (i (1- level))
                (setq sibling-alist (last (cdar sibling-alist))))
              (setcdr sibling-alist alist)
              (setq cur-alist alist))
            (setq cur-level level)))))
      (cdr root))))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq imenu-create-index-function 'markdown-imenu-create-index)))

(provide 'init-markdown)
