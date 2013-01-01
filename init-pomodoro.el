(setq pomodoro-break-time 2)
(setq pomodoro-long-break-time 5)
(setq pomodoro-work-time 15)
(setq-default mode-line-format
              (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
                    mode-line-format))
(provide 'init-pomodoro)
