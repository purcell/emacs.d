(defun my-smartparens-config ()
  (smartparens-global-mode t)

  (setq sp-navigate-consider-sgml-tags '(html-mode
                                         nxml-mode
                                         xml-mode))
  ;; highlights matching pairs
  (show-smartparens-global-mode t)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
  (sp-with-modes '(html-mode sgml-mode nxml-mode)
    (sp-local-pair "<" ">"))

;;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "C-("))
  )

(add-hook 'prog-mode-hook (lambda ()
                            (require 'smartparens-config)
                            (my-smartparens-config)
                            ))

;; make sp-select-next-thing works even the cusor is in the open/close tag
;; like matchit in vim
;; @return t => start from open tag; nil start from close tag
(defun my-sp-select-next-thing (&optional NUM)
  (interactive "p")
  (let ((b (line-beginning-position))
        (e (line-end-position))
        (char (following-char))
        (p (point))
        rbeg
        rend
        (rlt t)
        )
    ;; "<" char code is 60
    ;; search backward
    (if (not (= char 60))
        (save-excursion
          (while (and (<= b (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (backward-char)
            )
          )
      )
    ;; search forward
    (if (not (= char 60))
        (save-excursion
          (while (and (>= e (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (forward-char)
            )
          )
      )
    ;; do the real thing
    (when (and (= char 60) (< p e))
      (goto-char p)
      (forward-char)
      (if (= (following-char) 47)
          (progn
            ;; </
            (backward-char)
            (setq rlt nil)
            )
        (progn
          ;; < , looks fine
          (backward-char)
          (setq rlt t)
          )
        )
      (condition-case nil
          (sp-select-next-thing)
        (error
         (push-mark (point) t t)
         (re-search-forward "/>" (line-end-position))
         ))

      ;; (sp-select-next-thing)
      (setq rbeg (region-beginning))
      (setq rend (region-end))

      (while (> NUM 1)
        ;; well, sp-select-next-thing is kind of wierd
        (re-search-forward "<[^!]")
        (backward-char 2)
        (condition-case nil
            (sp-select-next-thing)
          (error
           (push-mark (point) t t)
           (re-search-forward "/>" (line-end-position))
           ))
        ;; (sp-select-next-thing)
        (setq rend (region-end))
        (setq NUM (1- NUM))
        )
      (push-mark rbeg t t)
      (goto-char (1- rend))
      )
    rlt
    )
  )

(provide 'init-smartparens)