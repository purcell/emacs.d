;; show file path in frame title
(setq-default frame-title-format "%b %f")

;; enable delete selection mode
(delete-selection-mode 1)

;; disable ido-find-file directories swich
(setq ido-auto-merge-work-directories-length -1)

;;display line number
(global-linum-mode 1)

;;tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 4 120 4))

;;auto save file
(setq auto-save-default nil)

;;auto revert file
(global-auto-revert-mode t)

;;open in same frame
(setq ns-pop-up-frames nil)


;;disable visible bell
(setq-default visible-bell nil)

;;parenthesis mode
(show-paren-mode t)

;;hightlight current line
(global-hl-line-mode 1)

;;display whitespace (comflict with auto complete)
;;(global-whitespace-mode 1)

;; init size
(setq initial-frame-alist '((width . 85) (height . 40)))

;; encoding and default font
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-8)

;;(set-face-attribute
;;  'default nil :font "Monaco-14") ; default font

(if window-system
    (set-fontset-font
     (frame-parameter nil 'font)
     'han
     (font-spec :family "STHeiti" :size 14))) ; han font


;;display column-number
(setq column-number-mode t)
(setq line-number-mode t)

;;insert final newline when save
(setq require-final-newline t)

;;delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;smooth scroll
(setq scroll-margin 2)
(setq scroll-conservatively 10000)

;;disable cua
(cua-selection-mode -1)

;; scroll-bar-mode
(scroll-bar-mode 1)

;; hide show mode
;(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;custom function
(defun untab ()
  (interactive)
   (let ((tabs tab-stop-list) (temp nil))
    (while (and tabs (> (current-column) (car tabs)))
      (setq temp tabs)
      (setq tabs (cdr tabs)))
    (progn
      (delete-horizontal-space t)
	  (indent-to (car temp)))))

(defun custom-open-newline ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun custom-open-newline-prev ()
  (interactive)
  (custom-open-newline)
  (move-text-up 1)
  (indent-according-to-mode))



;; copy word
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg))

;; toggle comment
(defun toggle-comment ()
  "comment or uncomment current line"
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; insert date
(defun date ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;; define-key
(global-set-key "\C-xf" 'find-file)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'custom-open-newline)
(global-set-key (kbd "C-S-o") 'custom-open-newline-prev)
(global-set-key (kbd "C-;") 'toggle-comment)



;;color-theme
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night-eighties)

;;yasnippet
(require 'yasnippet)
(yas/global-mode 1)

;;auto pair
(require 'autopair)
(autopair-global-mode)


(provide 'init-local)
