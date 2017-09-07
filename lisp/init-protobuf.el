(when (maybe-require-package 'protobuf-mode)
  (add-auto-mode 'protobuf-mode "\\.proto"))

(provide 'init-protobuf)
