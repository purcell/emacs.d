(define-key global-map "\C-cx" 'sunrise)
(define-key global-map "\C-cX" 'sunrise-cd)
; reset windows layout
(define-key global-map "\C-cd" 'sr-popviewer-setup-windows)

; @see http://www.emacswiki.org/emacs/Sunrise_Commander_Tips#ReplaceDiredWithSunrise
(setq find-directory-functions (cons 'sr-dired find-directory-functions))

(eval-after-load 'sunrise-commander
  '(progn
     (require 'sunrise-x-buttons)
     (require 'sunrise-x-popviewer)

     ; Highlighting files with inappropriate permissions.
     (sr-rainbow sr-gorw-dir-face
                 (:background "misty rose"
                              :foreground "blue1"
                              :bold t)
                 "^..\\(d....\\(...\\)?w..*$\\)")
     (sr-rainbow sr-gorw-face
                 (:background "misty rose")
                 "^..\\(-....\\(...\\)?w..*$\\)")

     ;; start using the passive pane as a transient viewer is
     (setq sr-popviewer-select-viewer-action
           (lambda nil (let ((sr-running nil)) (other-window 1))))
     ))
(provide 'init-sunrise-commander)
