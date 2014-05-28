;;; Package --- Configure key chord package
;;; Commentary:
;;; Map pairs of simultaneously pressed keys to commands
;;; Code:
(require-package 'key-chord)
(require 'key-chord)

(key-chord-mode 1)

(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "j'" 'ace-jump-mode-pop-mark)

(when (boundp 'evil-insert-state-map)
  (key-chord-define-global ",," 'evil-execute-in-normal-state))

(provide 'init-key-chord)
;;; init-key-chord.el ends here
