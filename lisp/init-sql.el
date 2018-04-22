(require-package 'sql-indent)
(after-load 'sql
  (require 'sql-indent))

(after-load 'sql
  ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
  (push "--no-psqlrc" sql-postgres-options))

(defun sanityinc/fix-postgres-prompt-regexp ()
  "Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22596.
Fix for the above hasn't been released as of Emacs 25.2."
  (when (eq sql-product 'postgres)
    (setq-local sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
    (setq-local sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] ")))

(add-hook 'sql-interactive-mode-hook 'sanityinc/fix-postgres-prompt-regexp)

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

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  (when (package-installed-p 'dash-at-point)
    (defun sanityinc/maybe-set-dash-db-docset ()
      (when (eq sql-product 'postgres)
        (set (make-local-variable 'dash-at-point-docset) "psql")))

    (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (defadvice sql-set-product (after set-dash-docset activate)
      (sanityinc/maybe-set-dash-db-docset))))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)


;; Package ideas:
;;   - PEV
(defun sanityinc/sql-explain-region-as-json (beg end &optional copy)
  "Explain the SQL between BEG and END in detailed JSON format.
This is suitable for pasting into tools such as
http://tatiyants.com/pev/.

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
                  (if copy
                      (progn
                        (kill-ring-save (buffer-substring-no-properties (point-min) (point-max)))
                        (message "EXPLAIN output copied to kill-ring."))
                    (view-buffer (current-buffer))))
              (with-current-buffer (get-buffer-create "*sql-explain-errors*")
                (setq buffer-read-only nil)
                (insert-file-contents err-file nil nil nil t)
                (view-buffer (current-buffer))
                (user-error "EXPLAIN failed")))))))))





(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))

(provide 'init-sql)
