(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(setq org-completion-use-ido t)


(setq org-edit-timestamp-down-means-later t)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 7)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(eval-after-load "org"
  '(progn
     (require 'org-clock)
     (require 'org-checklist)
     (require 'org-fstree)
     (require 'org-babel-init)
     (require 'org-babel-ruby)
     (require 'org-babel-python)
     (require 'org-babel-clojure)))

(provide 'init-org)
