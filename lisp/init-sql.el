;;; init-sql.el --- Support for SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'sql
  ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
  (add-to-list 'sql-postgres-options "--no-psqlrc"))

(defun sanityinc/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if (and sql-buffer (buffer-live-p sql-buffer))
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (sanityinc/pop-to-sqli-buffer))))

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  (when (package-installed-p 'dash-at-point)
    (defun sanityinc/maybe-set-dash-db-docset (&rest _)
      (when (eq sql-product 'postgres)
        (setq-local dash-at-point-docset "psql")))

    (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (advice-add 'sql-set-product :after 'sanityinc/maybe-set-dash-db-docset)))

(setq-default sql-input-ring-file-name
              (locate-user-emacs-file ".sqli_history"))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)


(require-package 'sqlformat)
(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))

;; Package ideas:
;;   - PEV
(defun sanityinc/sql-explain-region-as-json (beg end &optional copy)
  "Explain the SQL between BEG and END in detailed JSON format.
This is suitable for pasting into tools such as
https://explain.dalibo.com/.

When the prefix argument COPY is non-nil, do not display the
resulting JSON, but instead copy it to the kill ring.

If the region is not active, uses the current paragraph, as per
`sql-send-paragraph'.

Connection information is taken from the special sql-* variables
set in the current buffer, so you will usually want to start a
SQLi session first, or otherwise set `sql-database' etc.

This command currently blocks the UI, sorry."
  (interactive "rP")
  (unless (eq sql-product 'postgres)
    (user-error "This command is for PostgreSQL only"))
  (unless (use-region-p)
    (setq beg (save-excursion (backward-paragraph) (point))
          end (save-excursion (forward-paragraph) (point))))
  (let ((query (buffer-substring-no-properties beg end)))
    (with-current-buffer (if (sql-buffer-live-p sql-buffer)
                             sql-buffer
                           (current-buffer))
      (let* ((process-environment
              (append (list (concat "PGDATABASE=" sql-database)
                            (concat "PGHOST=" sql-server)
                            (concat "PGUSER=" sql-user))
                      process-environment))
             (args (list "--no-psqlrc"
                         "-qAt"
                         "-w"             ; Never prompt for password
                         "-E"
                         "-c" (concat "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) " query ";")
                         ))
             (err-file (make-temp-file "sql-explain-json")))
        (with-current-buffer (get-buffer-create "*sql-explain-json*")
          (setq buffer-read-only nil)
          (delete-region (point-min) (point-max))
          (let ((retcode (apply 'call-process sql-postgres-program nil (list (current-buffer) err-file) nil args)))
            (if (zerop retcode)
                (progn
                  (json-mode)
                  (read-only-mode 1)
                  (if copy
                      (progn
                        (kill-ring-save (buffer-substring-no-properties (point-min) (point-max)))
                        (message "EXPLAIN output copied to kill-ring."))
                    (display-buffer (current-buffer))))
              (with-current-buffer (get-buffer-create "*sql-explain-errors*")
                (let ((inhibit-read-only t))
                  (insert-file-contents err-file nil nil nil t))
                (display-buffer (current-buffer))
                (user-error "EXPLAIN failed")))))))))

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'sql-mode))

(provide 'init-sql)
;;; init-sql.el ends here
