(require 'emms-setup)
(emms-standard)
(emms-default-players)
;; Show the current track each time EMMS
;; starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
;; When asked for emms-play-directory,
;; always start from this one
(if (not (file-exists-p (expand-file-name "~/.emacs.d/emms")))
  (make-directory (expand-file-name "~/.emacs.d/emms"))
  )
(setq emms-source-file-default-directory "~/Music/")
(provide 'init-emms)
