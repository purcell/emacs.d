;;; Package --- program settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(bind-key (kbd "C-<down-mouse-1>") 'xref-find-definitions-or-usages-at-mouse 'emacs-lisp-mode-map)
;; (bind-key (kbd "C-<down-mouse-1>") 'xref-find-definitions-or-usages-at-mouse 'c-mode-map)
(bind-key (kbd "<mouse-4>") 'xref-go-back 'emacs-lisp-mode-map)
;; (bind-key (kbd "<mouse-4>") 'xref-go-back 'c-mode-map)
(bind-key (kbd "<mouse-5>") 'xref-go-forward 'emacs-lisp-mode-map)
;; (bind-key (kbd "<mouse-5>") 'xref-go-forward 'c-mode-map)

(defun xref-in-definition-p ()
  "判断当前点是否在函数定义的名称位置。"
  (when-let ((sym (symbol-at-point)))
    (save-excursion
      (beginning-of-thing 'symbol)
      (and ;; 向前搜索最近的左括号
       (condition-case nil
           (progn (up-list -1) t)
         (error nil))
       (or
        ;; 检查左括号后面的单词是否是定义形式
        (looking-at "(def\\(un\\|macro\\|generic\\|method\\|subst\\)\\s-+")
        (looking-at "(cl-def\\(un\\|macro\\|generic\\|method\\)\\s-+")
        (looking-at "(defalias\\s-+")
        (looking-at "(lambda\\s-+")
        (looking-at "(def\\(var\\|var-local\\)\\s-+")
        (looking-at "\\(?:setq\\|seq-local\\|setf\\)\\s-+.*#'?"))))))

(defun xref-find-definitions-or-usages ()
  (interactive)
  (if (xref-in-definition-p)
      (counsel-git-grep (format "%s" (symbol-at-point)))
    (xref-find-definitions (xref-backend-identifier-at-point
                            (xref-find-backend)))))

(defun xref-find-definitions-or-usages-at-mouse (event)
  (interactive "e")
  (if (xref-in-definition-p)
      (counsel-git-grep (format "%s" (symbol-at-point)))
    (xref-find-definitions-at-mouse event)))

(provide 'init-local-program)
;;; init-local-program.el ends here
