
;;# Begin # 状态栏设置 
;; 在状态栏显示日期时间
(setq display-time-day-and-date t)
(display-time)

;; 在mode-line上用彩色显示当前buffer数
(when window-system
  (copy-face 'region 'region-invert)
  (invert-face 'region-invert))
(size-indication-mode 1)
(setq-default 
 mode-line-buffer-identification 
 (propertized-buffer-identification "%b"))
(setq mode-line-format-bak mode-line-format)
(setq mode-line t)
(defun toggle-mode-line ()
  "Toggle mode-line."
  (interactive)
  (if mode-line
      (setq-default mode-line-format nil)
    (setq-default mode-line-format mode-line-format-bak))
  (setq mode-line (not mode-line)))

;; 标题栏设置
(defun frame-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let
      ((fname (or 
	       (buffer-file-name (current-buffer))
	       (buffer-name)))
       (max-len 100))
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname)))
    (if (&gt; (length fname) max-len)
	(setq fname
	      (concat "..."
		      (substring fname (- (length fname) max-len)))))
    fname)))
(setq frame-title-format 
      '(""
	(:eval (or (buffer-file-name) (buffer-name)))))
;; End 标题栏设置

;;; (setq auto-save-default nil)
;;; (setq font-lock-maximum-decoration t)
;;; (setq global-font-lock-mode t)
;;; (setq line-prefix (quote(80 . t)))
;;; (setq line-prefix (quote(80 . t)))
;;; (setq wrap-prefix (quote(80 . t)))
;;; (size-indication-mode t)
;;; (setq truncate-partial-width-windows nil)
;;; (setq visual-line-mode nil)
;;; (setq linum-mode t)

;;按Tab的时候，嘘～～，闭嘴!!
(setq visible-bell t)


(provide 'init-local)
