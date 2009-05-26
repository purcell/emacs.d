(setq flymake-gui-warnings-enabled nil)

;; Stop flymake from breaking when ruby-mode is invoked by mmm-mode,
;; at which point buffer-file-name is nil
(eval-after-load "flymake"
  '(progn
     (defun flymake-show-error-for-current-line ()
       (let ((err (get-char-property (point) 'help-echo)))
         (when err
           (message err))))

     (defun flymake-show-next-error-in-minibuffer ()
       "Move point to the next flymake error and display the error in the minibuffer"
       (interactive)
       (flymake-goto-next-error)
       (flymake-show-error-for-current-line))

     (global-set-key (kbd "C-`") 'flymake-show-next-error-in-minibuffer)

     (defun flymake-can-syntax-check-file (file-name)
       "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
       (if (and file-name (flymake-get-init-function file-name)) t nil))

     (defadvice flymake-mode (before post-command-stuff activate compile)
       "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
       (set (make-local-variable 'post-command-hook)
            (cons 'flymake-show-error-for-current-line post-command-hook)))))


;; http://nschum.de/src/emacs/fringe-helper/
(eval-after-load "flymake"
  '(progn
     (require 'fringe-helper)

     (defvar flymake-fringe-overlays nil)
     (make-variable-buffer-local 'flymake-fringe-overlays)

     (defadvice flymake-make-overlay (after add-to-fringe first
                                            (beg end tooltip-text face mouse-face)
                                            activate compile)
       (push (fringe-helper-insert-region
              beg end
              (fringe-lib-load (if (eq face 'flymake-errline)
                                   fringe-lib-exclamation-mark
                                 fringe-lib-question-mark))
              'left-fringe 'font-lock-warning-face)
             flymake-fringe-overlays))

     (defadvice flymake-delete-own-overlays (after remove-from-fringe activate
                                                   compile)
       (mapc 'fringe-helper-remove flymake-fringe-overlays)
       (setq flymake-fringe-overlays nil))))


(provide 'init-flymake)
