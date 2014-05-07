;;; Package -- Pomodoro time management
;;; Commentary:
;;; http://pomodorotechnique.com/
;;; Code:
(require-package 'pomodoro)
(require 'pomodoro)

(setq pomodoro-break-time 5)
(setq pomodoro-long-break-time 20)
(setq pomodoro-work-time 25)
(setq pomodoro-play-sounds nil)
(setq-default mode-line-format
              (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
                    mode-line-format))

(provide 'init-pomodoro)
;;; init-pomodoro.el ends here
