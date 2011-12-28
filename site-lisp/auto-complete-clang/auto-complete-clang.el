(eval-when-compile (require 'cl))

(defvar clang-executable "clang")
(defvar clang-completion-doc-table (make-hash-table :test 'equal))

;; faces
(defface clang-completion-plain-face
  '((t (:inherit default :family "Verdana")))
  "clang completion hint base font" :group 'clang-completion-faces)

(defface clang-completion-type-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#729FCF" :weight bold :family "Verdana")))
  "clang completion hint font for types" :group 'clang-completion-faces)

(defface clang-completion-variable-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#73D216" :family "Verdana")))
  "clang completion hint font for variables" :group 'clang-completion-faces)

;; extra flags
(defvar clang-completion-pch nil)
(defvar clang-completion-flags nil)
(defvar clang-completion-suppress-error nil)

(defun clang-process-exec (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (or (eq (apply 'call-process (car command) nil '(t ".clang-completion-error") nil (cdr command)) 0) clang-completion-suppress-error)
        (let ((last-command compile-command))
          (compile "cat .clang-completion-error")
          (setq compile-command last-command))))))

(defun clang-parse-completion-line (line)
  (cond ((string-match "^COMPLETION: Pattern" line) nil) ;; exclude patterns
        ((string-match "^COMPLETION: \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
         (list (match-string 1 line) (match-string 2 line)))
        ((string-match "^OVERRIDE: \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
         (list (match-string 1 line) (match-string 2 line)))
        (t nil))
  )

(defun clang-process (buffer point)
  (unless (buffer-file-name buffer)
    (return ""))
  (let* ((filename (buffer-file-name buffer))
         (col (1+ (- point (point-at-bol))))
         (row (count-lines point (point-min)))
         (cmd (list clang-executable "-cc1"
                         filename "-fsyntax-only" "-code-completion-at"
                         (format "%s:%s:%s" filename row col))))

    ;; eval the config file under buffer locations
    (let* ((filedir (file-name-directory filename))
           (config-filename (concat filedir ".clang-completion-config.el")))
      (when (file-readable-p config-filename)
        (with-temp-buffer
          (insert-file-contents config-filename)
          (eval-buffer))))

    (when (listp clang-completion-flags)
      (setq cmd (append cmd clang-completion-flags)))
    (when (stringp clang-completion-pch)
      (setq cmd (append cmd (list "-include-pch" clang-completion-pch))))
    ;(message (format "cmd: %s" cmd))
    (message (format "complete at %s:%s:%s" filename row col))
    (clang-process-exec cmd)))

(defun clang-get-process-result (string)
  (let* ((completion-lines (split-string string "\n")))
    (delq nil (mapcar 'clang-parse-completion-line completion-lines))))

(defun clang-get-process-completion-result (string)
  (mapcar 'car (clang-get-process-result string)))

(defun clang-get-process-prototype-table (string)
  (let* ((lines (clang-get-process-result string))
         (result-table (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let* ((key (first line))
             (value (gethash key result-table)))
        (setq value (append value (list (second line))))
        (puthash key value result-table))
      )
    (setq clang-completion-doc-table result-table)))

(defun clang-get-completions (&optional buffer point)
  ;; save all modified buffers
  (or buffer (setq buffer (current-buffer)))
  (or point (setq point (point)))
  (save-some-buffers t)
  (let* ((output (clang-process buffer point)))
    (clang-get-process-prototype-table output)
    (clang-get-process-completion-result output)))

(defun filter-doc-buffer ()
  (while (re-search-backward "\\[#.*?::#\\]" nil t)
    (replace-match ""))
  (goto-char (point-max))

  (while (re-search-backward "\\[#\\|#\\]" nil t)
    (replace-match " "))
  (goto-char (point-max))
  (while (re-search-backward "{#\\|#}\\|<#\\|#>" nil t)
    (replace-match ""))
  )

(defun clang-get-doc (symbol)
  ;;(setq symbol (symbol-name (intern-soft symbol)))
  (let ((reslist (gethash symbol clang-completion-doc-table)))
    (with-temp-buffer
      (font-lock-add-keywords nil '(("\\[#\\(.*?\\)#\\]" 1
                                     'clang-completion-type-face t)))
      (font-lock-add-keywords nil '(("<#\\(.*?\\)#>" 1
                                     'clang-completion-variable-face t)))
      (font-lock-add-keywords nil '(("\\(.*\\)" 1
                                     'clang-completion-plain-face t)))
      (font-lock-mode t)

      (insert (reduce '(lambda (x y) (concat x "\n" y)) reslist))
      (font-lock-fontify-buffer)
      (filter-doc-buffer)

      (message (buffer-string))))
  ;;(with-temp-buffer
  ;; (dolist (proto reslist)
  ;; (insert proto)
  ;; (insert "\n\n"))
  ;; (filter-doc-buffer)
  ;; (buffer-string))
  ;; display nothing
  (return nil))

(defvar ac-source-clang-complete
  '((candidates . (clang-get-completions nil ac-point))
    (prefix "[^a-zA-Z0-9_]\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . clang-get-doc)
    (requires . 0)
    (symbol . "C")
    (cache)))

;;(defvar ac-source-clang-static-complete
;; '((candidates . (clang-get-completions nil ac-point))
;; (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
;; ;;(document . 'clang-get-doc)
;; (requires . 0)
;; (symbol . "M")
;; (cache)))

(defun ac-complete-clang ()
  (interactive)
  (auto-complete '(ac-source-clang-complete)))

(provide 'auto-complete-clang)
