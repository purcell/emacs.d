;;----------------------------------------------------------------------------
;; Show and edit all lines matching a regex
;;----------------------------------------------------------------------------
;(require 'all)


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)


;;----------------------------------------------------------------------------
;; Autopair quotes and parentheses
;;----------------------------------------------------------------------------
(require 'autopair)
(setq autopair-autowrap t)


;;----------------------------------------------------------------------------
;; Supercharge undo/redo
;;----------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))


;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(require 'pointback)
(global-pointback-mode)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Built-in TAB completion (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(setq cua-enable-cua-keys nil)           ;; don't add C-x,C-c,C-v
(cua-mode t)                             ;; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Conversion of line endings
;;----------------------------------------------------------------------------
;; Can also use "C-x ENTER f dos" / "C-x ENTER f unix" (set-buffer-file-coding-system)
(require 'eol-conversion)


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "M-T") 'transpose-lines)

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(global-set-key (kbd "C-c p") 'duplicate-line)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


;;----------------------------------------------------------------------------
;; Shift lines up and down
;;----------------------------------------------------------------------------
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)


;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load "cua-rect"
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name -1)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

;;----------------------------------------------------------------------------
;; Easily count words (http://emacs-fu.blogspot.com/2009/01/counting-words.html)
;;----------------------------------------------------------------------------
(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))


;; Show typing speed
(autoload 'typing-speed-mode "typing-speed-mode" "Show typing speed in modeline")
(autoload 'turn-on-typing-speed "typing-speed-mode" "Show typing speed in modeline")
;(add-hook 'text-mode-hook 'turn-on-typing-speed)



;; Get handy scratch buffers for any major mode
(autoload 'scratch "scratch" nil t)


;;----------------------------------------------------------------------------
;; Set indent width according to existing code
;;----------------------------------------------------------------------------
(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)
(diminish 'fuzzy-format-mode)


(provide 'init-editing-utils)
