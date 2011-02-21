;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-z")
                (lambda ()
                  (interactive)
                  (unless (and *is-a-mac* window-system)
                    (suspend-frame))))


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq default-indicate-empty-lines t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(if (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(require 'init-maxframe)

(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (fboundp 'ns-toggle-fullscreen)
  ;; Command-Option-f to toggle fullscreen mode
  (global-set-key (kbd "M-ƒ") 'ns-toggle-fullscreen))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((prev-frame (selected-frame)))
              (select-frame frame)
              (prog1
                  (unless window-system
                    (set-frame-parameter frame 'menu-bar-lines 0))
                (select-frame prev-frame)))))




(provide 'init-gui-frames)
