(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(load "50darcsum")
(setq darcsum-whatsnew-switches "-l")

(eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "_darcs"))


(provide 'init-darcs)
