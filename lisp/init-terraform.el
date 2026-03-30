;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Terraform

(when (maybe-require-package 'terraform-mode)
  ;; TODO: find/write a replacement for company-terraform
  (with-eval-after-load 'terraform-mode
    ;; I find formatters based on "reformatter" to be more reliable
    ;; so I redefine `terraform-format-on-save-mode' here.
    (when (maybe-require-package 'reformatter)
      (reformatter-define terraform-format
        :program "terraform" :args '("fmt" "-")))))

(with-eval-after-load 'eglot
  (push `((terraform-mode)
          . ,(eglot-alternatives
              '(("terraform-ls" "serve")
                ("tofu-ls" "serve"))))
        eglot-server-programs))

(reformatter-define tofu-fmt :program "tofu" :args '("fmt" "-"))

(provide 'init-terraform)
;;; init-terraform.el ends here
