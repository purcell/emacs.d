;;(color-theme-sanityinc-solarized-dark)
(if (null window-system)
    (progn
      (message "tty")
      (require 'color-theme)
      ;;(color-theme-initialize)
      (color-theme-hober)
      (xterm-mouse-mode 0)
      )
  (color-theme-sanityinc-solarized-dark)
  )
(menu-bar-mode 0)
;;(tool-bar-mode 0)
;;(require 'w3m-load)
;;(setq browse-url-browser-function 'w3m-browse-url)
;;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;;; optional keyboard short-cut
;;;;(global-set-key "\C-xm" 'browse-url-at-point)
;;
;;(setq w3m-use-cookies t)
;;(setq w3m-default-display-inline-images t)
;;(setq w3m-command-arguments '("-cookie" "-F"))
;; load ibus-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/ibus")
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-SPC for Set Mark command
(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color '("red" "blue" "limegreen"))
;; use C-; to toggle IBus
(global-set-key [(control ?\;)] 'ibus-toggle)
;; Enable C-; key only for preediting
(ibus-define-common-key [(control ?\;)] nil)
(ibus-define-preedit-key [(control ?\;)] t)

;;copy
(setq x-select-enable-clipboard t)

;;org
;;(require 'org-install)
;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-log-done t)
;;(require 'org-latex)
;;(setq org-export-latex-listings t)

;;recentf
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;;(org-babel-do-load-languages
;; (quote org-babel-load-languages)
;; (quote ((emacs-lisp . t)
;;         (dot . t)
;;         (ditaa . t)
;;         (R . t)
;;         (python . t)
;;         (ruby . t)
;;         (gnuplot . t)
;;         (clojure . t)
;;         (sh . t)
;;         (ledger . t)
;;         (org . t)
;;         (plantuml . t)
;;         (latex . t))))
 
; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
;;(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
;;(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(require 'tramp)

;;(setq org-todo-keywords
;;      (quote ((sequence "TODO(t)" "STARTED(o)" "|" "DONE(d!/!)")
;;              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
;;
;;(setq org-todo-keyword-faces
;;      (quote (("TODO" :foreground "red" :weight bold)
;;              ("ONGOING" :foreground "#b58900" :weight bold)
;;              ("DONE" :foreground "forest green" :weight bold)
;;              ("WAITING" :foreground "orange" :weight bold)
;;              ("HOLD" :foreground "magenta" :weight bold)
;;              ("CANCELLED" :foreground "forest green" :weight bold))))
;;
;;(setq org-tag-alist '(
;;                      ("ShaoXia" . ?s)
;;                      ("DingQiu" . ?d)
;;                      ("HuoYong" . ?h)
;;                      ("ZiCheng" . ?z)
;;                      ("MuQian" . ?m)
;;                      ("HuangXW" . ?x)
;;                      ))

(define-skeleton org-beamer-skeleton
  "Insert org beamer header"
  ""
  "#+startup: beamer
#+LaTeX_CLASS: beamer
#+LaTeX_CLASS_OPTIONS: [bigger]
#+BEAMER_FRAME_LEVEL: 2
#+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)")

(define-skeleton org-weekly-report-entry
  "Insert Weekly Report Entry"
  ""
  "*** _PROJECT_NAME_
**** 本周进展
**** 下周安排
**** 风险
**** 资源
")
(define-skeleton org-weekly-report-header
  "Insert Weekly Report Header"
  ""
  "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"./css/fix.css\" />
#+STYLE: <script src=\"./js/jquery-1.7.2.min.js\"></script>
#+STYLE: <script src=\"./js/fix.js\"></script>")

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/remember-2.0")
;;(setq org-directory "~/org/")
;;(setq org-default-notes-file "~/.notes")
;;(setq remember-annotation-functions '(org-remember-annotation))
;;(setq remember-handler-functions '(org-remember-handler))
;;(add-hook 'remember-mode-hook 'org-remember-apply-template)
;;(define-key global-map "\C-cr" 'org-remember)
;;
;;(setq org-remember-templates
;;      '(("Todo" ?t "* TODO %? %^g\n %i\n " "~/gtd/gtd.org")
;;        ))
;;
;;
;;(defun gtd ()
;;   (interactive)
;;   (find-file "~/gtd/gtd.org")
;; )

(require 'magit)
(global-set-key [(meta f12)] 'magit-status)

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/irfc")
;;(require 'irfc)
;;(setq irfc-directory "~/workstation/rfc/")
;;(setq irfc-assoc-mode t)
;;(require 'my-org-settings)

(provide 'init-local)

