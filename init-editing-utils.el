;; Use evil-mode (strongly recommended) will make this file much smaller!

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file "~/.emacs.d/.bookmarks.el"
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 ;; no annoying beep on errors
 visible-bell t)

(transient-mark-mode t)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(paren-activate)     ; activating mic-paren

;;----------------------------------------------------------------------------
;; Autopair quotes and parentheses
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook (lambda ()
                            (setq autopair-autowrap t)
                            (autopair-global-mode t)
                            ))

(defun inhibit-autopair ()
  "Prevent autopair from enabling in the current buffer."
  (setq autopair-dont-activate t)
  (autopair-mode -1))

;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(global-pointback-mode)

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down
;;----------------------------------------------------------------------------
(move-text-default-bindings)

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;; comment/uncomment lines
(evilnc-default-hotkeys)

;need install browse-kill-ring
(browse-kill-ring-default-keybindings)

;; show trailing spaces in a programming mod
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; turns on auto-fill-mode, don't use text-mode-hook becasue for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'cc-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; delim-pad, control space padding around delimiters
(require 'delim-pad)
(delim-pad-mode 1)

(provide 'init-editing-utils)
