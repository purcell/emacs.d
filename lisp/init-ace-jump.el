;;; Package --- Configure ace jump
;;; Commentary:
;;; Code:

;; {{@see https://github.com/winterTTr/ace-jump-mode/wiki

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(provide 'init-ace-jump)
;;; init-ace-jump.el ends here
