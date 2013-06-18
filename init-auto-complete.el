;; @see http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-expand-on-auto-complete nil)
(setq ac-auto-start nil)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(ac-set-trigger-key "TAB") ; AFTER input prefix, press TAB key ASAP

;; Use C-n/C-p to select candidate ONLY when completion menu is displayed
;; Below code is copied from official manual
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; extra modes auto-complete must support
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js2-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

;; clang stuff
;; @see https://github.com/brianjcj/auto-complete-clang
(defun my-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  (when (and (not *cygwin*) (not *win32*))
    ; I don't do C++ stuff with cygwin+clang
    (setq ac-sources (append '(ac-source-clang) ac-sources))
    )
  (setq clang-include-dir-str
        (cond
         (*is-a-mac* "
/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include
/usr/include/c++/4.2.1
/usr/include/c++/4.2.1/backward
/usr/local/include
/Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-darwin11/4.2.1/include
/usr/include
")
         (*cygwin* "
/usr/lib/gcc/i686-pc-cygwin/3.4.4/include/c++/i686-pc-cygwin
/usr/lib/gcc/i686-pc-cygwin/3.4.4/include/c++/backward
/usr/local/include
/usr/lib/gcc/i686-pc-cygwin/3.4.4/include
/usr/include
/usr/lib/gcc/i686-pc-cygwin/3.4.4/../../../../include/w32api
")
         (*linux* "
/usr/include
/usr/lib/wx/include/gtk2-unicode-release-2.8
/usr/include/wx-2.8
/usr/include/gtk-2.0
/usr/lib/gtk-2.0/include
/usr/include/atk-1.0
/usr/include/cairo
/usr/include/gdk-pixbuf-2.0
/usr/include/pango-1.0
/usr/include/glib-2.0
/usr/lib/glib-2.0/include
/usr/include/pixman-1
/usr/include/freetype2
/usr/include/libpng14
")
         (t "") ; other platforms
         )
        )
  (setq ac-clang-flags
        (mapcar (lambda (item) (concat "-I" item))
                (split-string clang-include-dir-str)))

  (cppcm-reload-all)
  ; fixed rinari's bug
  (remove-hook 'find-file-hook 'rinari-launch)

  (setq ac-clang-auto-save t)
  )
(add-hook 'c-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)

(ac-config-default)

(provide 'init-auto-complete)
