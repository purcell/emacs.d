;; Fill column indicator
(require-package 'fill-column-indicator)
(defun sanityinc/prog-mode-fci-settings ()
  (turn-on-fci-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))

;;(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

(defun sanityinc/fci-enabled-p ()
  (bound-and-true-p fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; Regenerate fci-mode line images after switching themes
(defadvice enable-theme (after recompute-fci-face activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (sanityinc/fci-enabled-p)
        (turn-on-fci-mode)))))


(provide 'init-fci)
