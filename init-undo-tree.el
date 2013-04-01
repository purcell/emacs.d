(autoload 'undo-tree-visualize "undo-tree" "" nil)
;; the only function I care about in undo-tree because evil-mode already use it
(global-set-key "\C-xu" 'undo-tree-visualize)
(provide 'init-undo-tree)
