;; golden-ratio
(require 'golden-ratio)
(setq golden-ratio-exclude-modes '("shell-mode"
                                   "calendar-mode"
                                   "gud-mode"
                                   "ediff-mode"
                                   "eshell-mode"))
(setq golden-ratio-exclude-buffer-names
      '(" *org tags*"
        " *Org todo*"
        ))
(golden-ratio-enable)

(provide 'init-golden-ratio)
