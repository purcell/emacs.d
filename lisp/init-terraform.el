;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Terraform

(when (maybe-require-package 'terraform-mode)
  (when (maybe-require-package 'company-terraform)
    (after-load 'terraform-mode
      (company-terraform-init)

      ;; I find formatters based on "reformatter" to be more reliable
      ;; so I redefine `terraform-format-on-save-mode' here.
      (when (maybe-require-package 'reformatter)
        (reformatter-define terraform-format
          :program "terraform" :args '("fmt" "-"))))))

(provide 'init-terraform)
;;; init-terraform.el ends here
;;; init-terraform.el ends here
