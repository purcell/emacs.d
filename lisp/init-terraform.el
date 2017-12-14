;;; Terraform

(when (maybe-require-package 'terraform-mode)
  (when (maybe-require-package 'company-terraform)
    (after-load 'terraform-mode
      (company-terraform-init))))

(provide 'init-terraform)
;;; init-terraform.el ends here
