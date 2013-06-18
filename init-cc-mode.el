(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
      'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

;;===== hack gud-mode begin
;; move the cursor to the end of last line if it's gud-mode
(defun hack-gud-mode ()
  (when (string= major-mode "gud-mode")
    (goto-char (point-max))))

(defadvice switch-to-buffer (after switch-to-buffer-after activate)
  (hack-gud-mode))

;; from switch-window is from 3rd party plugin switch windows.el
(defadvice switch-window (after switch-window-after activate)
  (hack-gud-mode))

;; windmove-do-window-select is from windmove.el
(defadvice windmove-do-window-select (after windmove-do-window-select-after activate)
  (hack-gud-mode))
;; ==== end

;C/C++ SECTION
(defun my-c-mode-hook ()
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (local-set-key "\M-f" 'c-forward-into-nomenclature)
  (local-set-key "\M-b" 'c-backward-into-nomenclature)
  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))
  (setq c-basic-offset 4)
  (setq c-style-variables-are-local-p nil)
  ;give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (setq compilation-window-height 8)
  (setq compilation-finish-function
        (lambda (buf str)
          (if (string-match "exited abnormally" str)
              ;;there were errors
              (message "compilation errors, press C-x ` to visit")
            ;;no errors, make the compilation window go away in 0.5 seconds
            (when (string-match "*compilation*" (buffer-name buf))
              ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
              (bury-buffer "*compilation*")
              (winner-undo)
              (message "NO COMPILATION ERRORS!")
              ))))

  ;if (0)          becomes        if (0)
  ;    {                          {
  ;       ;                           ;
  ;    }                          }
  (c-set-offset 'substatement-open 0)

  ;first arg of arglist to functions: tabbed in once
  ;(default was c-lineup-arglist-intro-after-paren)
  (c-set-offset 'arglist-intro '+)

  ;second line of arglist to functions: tabbed in once
  ;(default was c-lineup-arglist)
  (c-set-offset 'arglist-cont-nonempty '+)

  ;switch/case:  make each case line indent from switch
  (c-set-offset 'case-label '+)

  ;make the ENTER key indent next line properly
  (local-set-key "\C-m" 'newline-and-indent)

  ;syntax-highlight aggressively
  ;(setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;make open-braces after a case: statement indent to 0 (default was '+)
  (c-set-offset 'statement-case-open 0)

  ;make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  ;wxwdigets stuff
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)

  ;do not impose restriction that all lines not top-level be indented at least
  ;1 (was imposed by gnu style by default)
  (setq c-label-minimum-indentation 0)

  (setq gtags-suggested-key-mapping t)
  (gtags-mode 1)

  (require 'fic-mode)
  (add-hook 'c++-mode-hook 'turn-on-fic-mode)

  ; @see https://github.com/seanfisk/cmake-flymake
  ; make sure you project use cmake
  (flymake-mode)
  (cppcm-reload-all)

  )
;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'init-cc-mode)
