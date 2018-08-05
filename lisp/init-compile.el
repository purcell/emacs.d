(setq-default compilation-scroll-output t)

(require-package 'alert)

;; Customize `alert-default-style' to get messages after compilation

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (when (buffer-live-p buf)
    (unless (catch 'is-visible
              (walk-windows (lambda (w)
                              (when (eq (window-buffer w) buf)
                                (throw 'is-visible t))))
              nil)
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

(after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish))

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(after-load 'compile
  (defadvice compilation-start (after sanityinc/save-compilation-buffer activate)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer))

  (defadvice recompile (around sanityinc/find-prev-compilation (&optional edit-command) activate)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          ad-do-it)
      ad-do-it)))

(global-set-key [f6] 'recompile)

(defadvice shell-command-on-region
    (after sanityinc/shell-command-in-view-mode
           (start end command &optional output-buffer replace &rest other-args)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))


(after-load 'compile
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))


(maybe-require-package 'cmd-to-echo)


(provide 'init-compile)
