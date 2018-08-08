;;; Code: make sure we turn on bazel mode for BUILD files
(when (maybe-require-package 'bazel-mode)
  (add-auto-mode 'bazel-mode "BUILD"))

(provide 'init-bazel)
