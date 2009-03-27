(require 'anything-config)

(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-recentf
        anything-c-source-occur
        anything-c-source-files-in-current-dir
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions))

(setq anything-samewindow nil)
(setq anything-input-idle-delay 0.05)

(global-set-key [\M-f10] 'anything)


(provide 'init-anything)
