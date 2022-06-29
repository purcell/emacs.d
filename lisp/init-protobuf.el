;; init-protobuf --- setting for protobuf mode


(require-package 'protobuf-mode)

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(provide 'init-protobuf)
