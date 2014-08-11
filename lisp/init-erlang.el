(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

(add-to-list 'ac-modes 'erlang-mode)

(provide 'init-erlang)
