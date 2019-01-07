(setq user-full-name "Joseph Young")
(setq user-mail-address "2t@live.com")

(menu-bar-mode 1)

;; (blink-cursor-mode -1)


(setq visible-bell 0)

;;; pages scroll at 3 rows left
(setq scroll-margin 3
      scroll-conservatively 10000)

;;; High-light and line number
(global-set-key [f8] 'global-hl-line-mode)
;; (global-set-key [f8] 'h1AndG1mode)
;; (defun hlAndGlmode ()
;;   (interactive)
;;   (global-hl-line-mode 1)
;;   (global-linum-mode 1))

;; ;;wangyin
;; (global-set-key (kbd "#") 'match-paren)
;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))

;; big akillring
;; (setq kill-ring-max 200)


;;remember mode configuration
(autoload 'remember "remember" nil t)
(define-key global-map [f12] 'remember)
(setq remember-data-file "c:/Users/Joseph/OneDrive/org-mode/notes.org")
(global-set-key (kbd "C-<f12>") 'my-remmeber-file)
(defun my-remmeber-file()
  (interactive)
  (find-file "c:/User/Joseph/oneDrive/org-mode/notes.org"))

;;; yasnippet setting
(yas-global-mode 1)

;;; dired deleted to trash can
(setq delete-by-moving-to-trash t)

;;; solve the conflicts in Company and Yasnippet
;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)

;;; set default newline character
(setq-default buffer-file-coding-system 'utf-8-unix)

;;; utf-8 settings
(set-terminal-coding-system 'utf-8)

;;; set F9 to compile, set default compile command
(defun compile-clang ()
  (interactive)
  (let ((compile-command (format "clang \"%s\""
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively #'compile)))

(global-set-key [f9] 'compile)

(require 'init-pyim)
(require 'init-javacompile)
(provide 'init-local)
