;;; Show line numbers.
(global-linum-mode t)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "GOPATH" "/usr/local/go")
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin" ":" "/usr/local/go/bin" ":" "/Users/justin/bin"))

;; use Meta-Return keys on Mac OS X to toggle maxmize full screen.
(global-set-key (kbd "<M-RET>") 'toggle-frame-fullscreen)
;; use C-tab to do auto complete
(global-set-key [C-tab] 'auto-complete)
