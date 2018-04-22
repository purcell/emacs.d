;;; scala-mode-map.el - Major mode for editing scala, keyboard map
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'scala-mode-indent)

(defvar scala-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    ;(substitute-key-definition 'delete-indentation 'scala-indent:join-line map global-map)
    map)
  "Local key map used for scala mode")

(defun scala-mode-map:add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-parentheses)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-special-words)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-scaladoc-asterisk)
  (add-hook 'post-self-insert-hook
            'scala-indent:fix-scaladoc-close))

(defun scala-mode-map:add-remove-indent-hook ()
  (add-hook 'post-command-hook
            'scala-indent:remove-indent-from-previous-empty-line))

(provide 'scala-mode-map)
