(require-package 'darcsum)
(require-package 'vc-darcs)


;; TODO: include this in the vc-darcs ELPA package
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(setq darcsum-whatsnew-switches "-l")

(provide 'init-darcs)
