;;; mc-mark-pop.el --- Pop cursors off of the mark stack

(require 'multiple-cursors-core)

;;;###autoload
(defun mc/mark-pop ()
  "Add a cursor at the current point, pop off mark ring and jump
to the popped mark."
  (interactive)
  ;; If the mark happens to be at the current point, just pop that one off.
  (while (eql (mark) (point))
    (pop-mark))
  (mc/create-fake-cursor-at-point)
  (exchange-point-and-mark)
  (pop-mark)
  (mc/maybe-multiple-cursors-mode))

;; A good key binding for this feature is perhaps "C-S-p" ('p' for pop).

(provide 'mc-mark-pop)

;;; mc-mark-pop.el ends here
