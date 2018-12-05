;;; Terraform

(when (maybe-require-package 'terraform-mode)
  (when (maybe-require-package 'company-terraform)
    (after-load 'terraform-mode
      (company-terraform-init)
      (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))))

(provide 'init-terraform)
;;; init-terraform.el ends here
