(require 'pulse)

;;(setq next-error-hook nil)
(add-hook 'next-error-hook
          '(lambda ()
             (if (ycmd-next-error-at-ycmd-button (point)) (push-button)
               (let ((compilation-buffer (get-buffer "*compilation*")))
                 (when compilation-buffer
                   (save-excursion
                     (set-buffer "*compilation*")
                     (move-beginning-of-line nil)
                     (pulse-line-hook-function)))))))

(eval-after-load "ycmd"
  '(add-hook 'ycmd-mode-hook
             '(lambda ()
                (setq next-error-function 'ycmd-next-error-goto-next-error))))

(defun ycmd-next-error-at-ycmd-button (cur)
  "Returns whether CUR is at a ycmd button."
  (let* ((button (button-at cur))
         (type (and button (button-type button))))
    (or (eq type 'ycmd--error-button)
        (eq type 'ycmd--warning-button))))

(defun ycmd-next-error-goto-next-error (&optional count reset)
  "Go to next YCM-detected error in the current buffer, or stay put if none"
  (interactive)
  (when (null count) (setq count 0))
  (let ((target nil)
        (move-fn (if (< count 0) 'previous-button 'next-button))
        (next-count (if (< count -1) (1+ count) (if (> count 1) (1- count) 0)))
        (cur (if reset (point-min) (point))))
    (save-excursion
      (when (and
             (setq cur (funcall (symbol-function move-fn) cur))
             (ycmd-next-error-at-ycmd-button cur))
        (setq target cur)))
    (if (not (and count target))
        (message "Reached last error")
      (goto-char target)
      (when (not (eq next-count 0))
        (ycmd-next-error-goto-next-error next-count nil)))))

(provide 'ycmd-next-error)
