;;; package --- init-local
;;; Commentary:
;;; personal config

;;; site-lisp
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;; c mode
;;; cedet
;; git clone http://git.code.sf.net/p/cedet/git cedet
(load-file (expand-file-name "site-lisp/cedet/cedet-devel-load.el"
                             user-emacs-directory))
(load-file (expand-file-name "site-lisp/cedet/contrib/eassist.el"
                             user-emacs-directory))

(require 'semantic)
(require 'eassist)

(require-package 'counsel-gtags)

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Remove the old element.
KEY: key
VAL: value"
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))


;;; semantic
;;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)

(defun c-mode-common-hook-setup ()
  "My c mode common hook setup."

  ;; basic
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (c-toggle-hungry-state 1)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

  ;; header
  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include"))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  ;; gtags (GNU global) stuff
  (when (and (executable-find "global")
             ;; `man global' to figure out why
             (not (string-match-p "GTAGS not found"
                                  (shell-command-to-string "global -p")))))

  ;; gtags
  (counsel-gtags-mode t)
  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol))

  (defalias 'gt-find-file 'counsel-gtags-find-file)
  (defalias 'gt-create 'counsel-gtags-create-tags)
  (defalias 'gt-update 'counsel-gtags-update-tags)
  (defalias 'gt-forward 'counsel-gtags-go-forward)
  (defalias 'gt-backward 'counsel-gtags-go-backward)

  ;; eassist mode
  (eassist-mode t)
  (local-set-key (kbd "C-c h") 'eassist-switch-h-cpp)
  (lcoal-set-key (kbd "C-c m") 'eassist-list-methods))


(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)


;;; go mode
(require-package 'go-mode)
(require-package 'go-eldoc)
(require-package 'go-autocomplete)

(require 'golint)
(require 'auto-complete-config)

(ac-config-default)

;;; Code:
(defun my-go-mode ()
  "My `go-mode' common hook."
  (linum-mode)
  (auto-complete-mode 1)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;; hook
(add-hook 'go-mode-hook 'my-go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)


;;; highlight
(require 'highlight-symbol)

(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(defalias 'hl 'highlight-symbol)


;;; scheme mode
(require 'cmuscheme)
(require-package 'paredit)

;;; repl
(setq scheme-program-name "racket")

(defvar *binding-constructs*
  '(let-values
    let*-values
    hash-for-each
    letv
    let\:
    lambda\:
    letv*
    match
    pmatch
    for
    for/list
    fun
    record))

;; scm rkt
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process, See variable `scheme-buffer'")))


(defun scheme-split-window ()
  "Split window."
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))


(defun scheme-send-last-sexp-split-window ()
  "Send Last sexp."
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))


(defun scheme-send-definition-split-window ()
  "Send definition."
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(defun my-scheme-mode ()
  "My `scheme' mode hook."
  (global-font-lock-mode 1)
  (paredit-mode)
  (linum-mode)
  ;; key
  (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
  (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)
  (mapc (lambda (x) (put x 'scheme-indent-function 1)) *binding-constructs*))

(add-hook 'scheme-mode-hook 'my-scheme-mode)


;;; misc
(defalias 'fg 'find-grep)

;;; fix osx-key
(when *is-a-mac*
  (setq mac-option-modifier 'meta))
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-d") 'dired)

;;; Paren match
(defun match-paren (arg)
  "Auto match paren.
ARG arg: parentheses"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-x [") 'match-paren)

;;; fonts
;;(defconst my-default-fonts "Source Code Pro 14")
(defconst my-default-fonts "monaco 13")
(set-frame-font my-default-fonts)
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            ""
            (select-frame new-frame)
            (set-frame-font my-default-fonts)))

;; Set selected region color.
(set-face-attribute 'region nil :background "#00ff00")

;;; mode line : default-mode-line-format
;; (setq-default mode-line-format
;;               (list mode-line-front-space
;;                     mode-line-mule-info
;;                     mode-line-client
;;                     mode-line-modified
;;                     mode-line-remote
;;                     mode-line-frame-identification
;;                     mode-line-buffer-identification
;;                     "   "
;;                     mode-line-position
;;                     ;;(vc-mode vc-mode)
;;                     "  "
;;                     ;;mode-line-modes
;;                     mode-line-misc-info
;;                     mode-line-end-spaces))

;;; unset some global keys

(provide 'init-local)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;;; init-local.el ends here
