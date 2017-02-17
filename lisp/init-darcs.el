(require-package 'darcsum)
(require-package 'vc-darcs)


;; TODO: include this in the vc-darcs ELPA package
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(after-load 'vc-darcs
  ;; This variable was removed in an Emacs 25.x snapshot, but vc-darcs
  ;; hasn't been fixed accordingly
  (unless (boundp 'vc-disable-async-diff)
    (setq vc-disable-async-diff nil)))

(setq darcsum-whatsnew-switches "-l")

(provide 'init-darcs)
