(require 'ibuffer)

(require 'vc-hooks)
(defun vc-find-any-root (file-name)
  (let ((root nil))
    (loop for dir in '(".git" ".svn" "CVS" ".bzr" "_darcs")
          do (setq root (vc-find-root file-name dir))
          until root
          finally return root)))

(defun ibuffer-vc-root (buf)
  (let* ((file (buffer-local-value 'buffer-file-name buf))
         (dir (buffer-local-value 'default-directory buf))
         (ref (or file dir)))
    (vc-find-any-root ref)))

(define-ibuffer-filter vc-root
    "Toggle current view to buffers with vc root dir QUALIFIER."
  (:description "vc root dir"
                :reader (read-from-minibuffer "Filter by vc root dir (regexp): "))
  (ibuffer-awhen (ibuffer-vc-root buf)
    (message "Comparing root %s against qualifier %s" it qualifier)
    (string-match (expand-file-name qualifier) (expand-file-name it))))

(defun ibuffer-generate-filter-groups-by-vc-root ()
  "Create a set of ibuffer filter groups based on the vc root dirs of buffers"
  (mapcar (lambda (vc-dir)
                  (cons (format "%s" vc-dir) `((vc-root . ,vc-dir))))
                (ibuffer-remove-duplicates
                 (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))

(defun ibuffer-set-filter-groups-by-vc-root ()
  "Set the current filter groups to filter by vc root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-vc-root))
  (ibuffer-update nil t))

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-set-filter-groups-by-vc-root))
(add-hook 'ibuffer-mode-hook 'ibuffer-set-up-preferred-filters)



;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Org" ;; all org-related buffers
;;                 (mode . org-mode))
;;                ("Mail"
;;                 (or ;; mail-related buffers
;;                  (mode . message-mode)
;;                  (mode . mail-mode)
;;                  ;; etc.; all your mail related modes
;;                  ))
;;                ("Programming" ;; prog stuff not already in MyProjectX
;;                 (or
;;                  (mode . c-mode)
;;                  (mode . perl-mode)
;;                  (mode . python-mode)
;;                  (mode . emacs-lisp-mode)
;;                  ;; etc
;;                  ))
;;                ("ERC"   (mode . erc-mode))))))

;; (add-hook 'ibuffer-mode-hook
;;   (lambda ()
;;     (ibuffer-switch-to-saved-filter-groups "default")))


(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
