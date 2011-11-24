;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile$")
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'find-file-hooks 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(column-number-mode 1)

; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; no annoying beep on errors
(setq visible-bell t)
(setq backup-directory-alist '(("." . "~/.backups")))

(put 'narrow-to-region 'disabled nil)

(global-set-key "\C-\M-u" 'windmove-left)
(global-set-key "\C-\M-p" 'windmove-right)
(global-set-key "\C-\M-o" 'windmove-up)
(global-set-key "\C-\M-i" 'windmove-down)

; from RobinH
;Time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(global-set-key [f8] 'calendar)
(global-set-key [f12] 'list-bookmarks)

(when *win32*
  ;; resize frame
  (defun w32-maximize-frame ()
    "Maximize the current frame."
    (interactive)
    (w32-send-sys-command 61488)
    (global-set-key (kbd "C-c z") 'w32-restore-frame))

  (global-set-key (kbd "C-c z") 'w32-maximize-frame)

  (defun w32-restore-frame ()
    "Restore a minimized frame."
    (interactive)
    (w32-send-sys-command 61728)
    (global-set-key (kbd "C-c z") 'w32-maximize-frame))

  )

;; M-x ct ENTER
(defun ct (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s/TAGS -e -R %s" dir-name (directory-file-name dir-name)))
  )

;effective emacs item 3
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-s" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'tags-search)
(global-set-key "\C-x\C-n" 'find-file-other-frame) ;open new frame with a file

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;vice versa
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
           (setq i (+ i 1))
           (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;;compute the length of the marked region
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; gdb
(global-set-key "\C-x\C-a\C-g" 'gud-run)
;;; WINDOW SPLITING
;; @see http://xahlee.org/emacs/effective_emacs.html
(global-set-key (kbd "M-3") 'split-window-horizontally);was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ;was digit-argument
(global-set-key (kbd "M-1") 'delete-other-window) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(defalias 'list-buffers 'ibuffer)
;KEYBOARD SECTION
;global keyb maps
(global-set-key "\C-xc" 'clipboard-kill-ring-save)
(global-set-key "\C-cc" 'copy-region-as-kill)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)
(global-set-key [?\C-/] 'void) ;forward reference

;; @see http://www.emacswiki.org/emacs/BetterRegisters
;; This is used in the function below to make marked points visible
(defface register-marker-face '((t (:background "grey")))
      "Used to mark register positions in a buffer."
      :group 'faces)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;effiective emacs item9
(defalias 'qrr 'query-replace-regexp)

;@see http://stackoverflow.com/questions/3509919/ \
;emacs-c-opening-corresponding-header-file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(setq-default regex-tool-backend 'perl)

;make speedbar work
(if (load "mwheel" t)
  (mwheel-install))

(provide 'init-misc)
