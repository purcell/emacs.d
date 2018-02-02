(require 'init-git)

(maybe-require-package 'yagist)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)
(when (maybe-require-package 'magithub)
  (defun magithub-issues-completion-at-point ()
    (when (magithub-enabled-p)
      (when (looking-back "#\\([0-9]*\\)")
        (let ((start (match-beginning 1))
              (end (match-end 0))
              (prefix (match-string 1))
              completions)
          (dolist (i (magithub--issue-list))
            (let-alist i
              (let ((n (number-to-string .number)))
                (when (string-prefix-p prefix n)
                  (set-text-properties 0 (length n) (list :issue i) n)
                  (push n completions)))))
          (list start end completions
                :exclusive 'no
                :company-docsig (lambda (c)
                                  (let-alist (get-text-property 0 :issue c)
                                    .title))
                :company-location (lambda (c)
                                    (magithub-issue-browse (get-text-property 0 :issue c)))
                :annotation-function (lambda (c)
                                       (let-alist (get-text-property 0 :issue c)
                                         .title))
                :company-doc-buffer (lambda (c)
                                      (save-window-excursion
                                        (magithub-issue-visit (get-text-property 0 :issue c))))
                )))))

  (add-hook 'git-commit-setup-hook
            (lambda () (add-to-list 'completion-at-point-functions 'magithub-issues-completion-at-point))))


(provide 'init-github)
