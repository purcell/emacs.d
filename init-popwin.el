(require 'popwin)

(setq popwin:special-display-config nil)
(setq display-buffer-function 'popwin:display-buffer)

;; enable popwin-mode globally is too dangerous
(add-to-list 'popwin:special-display-config '("*Help*"))
(add-to-list 'popwin:special-display-config '("*Completions*" :noselect t))
(add-to-list 'popwin:special-display-config '("*compilation*" :noselect t))
(add-to-list 'popwin:special-display-config '("*Occur*" :noselect t))
(add-to-list 'popwin:special-display-config '("*Backtrace*"))
(add-to-list 'popwin:special-display-config '("*Remember*" :stick t))
(add-to-list 'popwin:special-display-config '("*Org Agenda*"))
(add-to-list 'popwin:special-display-config '("*sdic*" :noselect))
(add-to-list 'popwin:special-display-config '("*Apropos*"))
(add-to-list 'popwin:special-display-config '("*Warnings*"))
(add-to-list 'popwin:special-display-config '(" *auto-async-byte-compile*" :noselect))

(provide 'init-popwin)
