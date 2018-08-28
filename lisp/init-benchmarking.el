(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around sanityinc/build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (list feature require-start-time time)
                       t))))))


(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun sanityinc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in sanityinc/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)


(provide 'init-benchmarking)
