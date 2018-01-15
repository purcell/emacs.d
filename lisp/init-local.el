;;; package --- init-local
;;; Commentary:
;;; personal config

;;; site-lisp
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;; c mode
(require-package 'counsel-gtags)

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Remove the old element.
KEY: key
VAL: value"
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun c-mode-common-hook-setup ()
  "My c mode common hook setup."
  ;; basic
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  ;; gtags
  (counsel-gtags-mode t)

  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-definition))

  (defalias 'gt-find-file 'counsel-gtags-find-file)
  (defalias 'gt-create 'counsel-gtags-create-tags)
  (defalias 'gt-update 'counsel-gtags-update-tags)
  (defalias 'gt-forward 'counsel-gtags-go-forward)
  (defalias 'gt-backward 'counsel-gtags-go-backward)
  (defalias 'gt-find-file 'counsel-gtags-find-file)

  ;; switch between .c and .h and function list
  (local-set-key (kbd "C-c h") 'ff-find-other-file)
  (local-set-key (kbd "C-c m") 'imenu))


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
  (auto-complete-mode 1)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;; hook
(add-hook 'go-mode-hook 'my-go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)


;;; which function
(require 'which-func)
(which-func-mode 1)


;;; highlight
(require 'highlight-symbol)

(defun my-highlight-next (&optional symbol)
  "My highlight function.
If symbol at current pointer is highlight return `highlight-symbol-next',
otherwise return `highlight-symbol',
SYMBOL: symbol at pointer."
  (interactive)
  (let ((symbol (or symbol
                    (highlight-symbol-get-symbol)
                    (error "No symbol at point"))))
    (if (highlight-symbol-symbol-highlighted-p symbol)
        (highlight-symbol-next-in-defun)
      (highlight-symbol symbol))))

(defun my-highlight-prev (&optional symbol)
  "My highlight function.
If symbol at current pointer is highlight return `highlight-symbol-prev',
otherwise return `highlight-symbol',
SYMBOL: symbol at pointer."
  (interactive)
  (let ((symbol (or symbol
                    (highlight-symbol-get-symbol)
                    (error "No symbol at point"))))
    (if (highlight-symbol-symbol-highlighted-p symbol)
        (highlight-symbol-prev-in-defun)
      (highlight-symbol symbol))))

(global-set-key (kbd "<f3>") 'my-highlight-next)
(global-set-key (kbd "<f4>") 'my-highlight-prev)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "S-<f4>") 'highlight-symbol-remove-all)

(defalias 'hl-remove-all 'highlight-symbol-remove-all)


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
  ;; key
  (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
  (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)
  (mapc (lambda (x) (put x 'scheme-indent-function 1)) *binding-constructs*))

(add-hook 'scheme-mode-hook 'my-scheme-mode)


;;; misc
(defalias 'fg 'find-grep)
(global-set-key (kbd "<f2>") 'find-grep)

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

;;; magit
(global-set-key (kbd "C-x p") 'magit-push)

;;; fonts
(defconst my-default-fonts "Source Code Pro")
(set-frame-font my-default-fonts)
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            ""
            (select-frame new-frame)
            (set-frame-font my-default-fonts)))

;;; org-mode
(setq org-export-backends (quote (ascii html icalendar latex md)))

;;; tab width
(setq tab-width 4)

;;; unst-keys
(global-unset-key (kbd "<M-tab>"))
(global-unset-key (kbd "C-M-i"))

;;; isearch-forward-symbol-at-point
(defalias 'search-at-point 'isearch-forward-symbol-at-point)

(provide 'init-local)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;;; init-local.el ends here
