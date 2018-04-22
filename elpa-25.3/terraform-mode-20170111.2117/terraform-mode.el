;;; terraform-mode.el --- Major mode for terraform configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-terraform-mode
;; Package-Version: 20170111.2117
;; Version: 0.06
;; Package-Requires: ((emacs "24.3") (hcl-mode "0.03"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode of terraform configuration file. terraform-mode provides
;; syntax highlighting, indentation function and formatting.

;; Format the current buffer with terraform-format-buffer. To always
;; format terraform buffers when saving, use:
;;   (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'hcl-mode)

(defgroup terraform nil
  "Major mode of Terraform configuration file."
  :group 'languages)

(defcustom terraform-indent-level 2
  "The tab width to use when indenting."
  :type 'integer)

(defconst terraform--block-regexp
  "^\\s-*\\(provider\\|resource\\|data\\|module\\|variable\\|output\\)\\s-+\"")

(defconst terraform--atlas-regexp
  "^\\s-*\\(atlas\\)\\s-*")

(defconst terraform--provisioner-regexp
  "^\\s-+\\(provisioner\\)\\s-+\"")

(defconst terraform--inner-block-regexp
  "^\\s-+\\(connection\\)\\s-+{"
  "Inner special block.")

(defvar terraform-font-lock-keywords
  `((,terraform--block-regexp 1 font-lock-function-name-face)
    (,terraform--atlas-regexp 1 font-lock-function-name-face)
    (,terraform--provisioner-regexp 1 font-lock-function-name-face)
    (,terraform--inner-block-regexp 1 font-lock-keyword-face)
    ,@hcl-font-lock-keywords))

(defun terraform-format-buffer ()
  "Rewrite current buffer in a canonical format using terraform fmt."
  (interactive)
  (let ((buf (get-buffer-create "*terraform-fmt*")))
    (if (zerop (call-process-region (point-min) (point-max)
                                    "terraform" nil buf nil "fmt" "-"))
        (let ((point (point))
              (window-start (window-start)))
          (erase-buffer)
          (insert-buffer-substring buf)
          (goto-char point)
          (set-window-start nil window-start))
      (message "terraform fmt: %s" (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)))

(define-minor-mode terraform-format-on-save-mode
  "Run terraform-format-buffer before saving current buffer."
  :lighter ""
  (if terraform-format-on-save-mode
      (add-hook 'before-save-hook #'terraform-format-buffer nil t)
    (remove-hook 'before-save-hook #'terraform-format-buffer t)))

;;;###autoload
(define-derived-mode terraform-mode hcl-mode "Terraform"
  "Major mode for editing terraform configuration file"

  (setq font-lock-defaults '((terraform-font-lock-keywords)))

  ;; indentation
  (make-local-variable 'terraform-indent-level)
  (setq hcl-indent-level terraform-indent-level)

  ;; imenu
  (setq imenu-generic-expression
        '(("resource" "^resource\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" 1)
          ("data" "^data\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" 1)
          ("provider" "^provider\\s-+\"\\([^\"]+\\)\"" 1)
          ("module" "^module\\s-+\"\\([^\"]+\\)\"" 1)
          ("variable" "^variable\\s-+\"\\([^\"]+\\)\"" 1)
          ("output" "^output\\s-+\"\\([^\"]+\\)\"" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tf\\(vars\\)?\\'" . terraform-mode))

(provide 'terraform-mode)

;;; terraform-mode.el ends here
