;;; switch-window-mvborder.el --- Move border of emacs window.

;; * Header
;; Copyright (c) the unknown original author

;; I found the original code on a blog where the blog author
;; is also saying that he is not the author.
;; So, no one knows who is the author.

;; If you are the author, please send me a word.

;; Author: Unknown
;; URL: https://www.emacswiki.org/emacs/WindowResize
;;      https://www.emacswiki.org/emacs/GrowShrinkWindows
;;      https://github.com/ramnes/mvborder
;;

;;; Commentary:

;;; Code:

;; * Code                                                                 :code:
(defun switch-window--xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun switch-window--mvborder-left-or-right (arg dir)
  "General function covering switch-window-mvborder-left and switch-window-mvborder-right.
If DIR is t, then move left, otherwise move right."
  (when (null arg)
    (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (switch-window--xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun switch-window--mvborder-up-or-down (arg dir)
  "General function covering switch-window-mvborder-up and switch-window-mvborder-down.
If DIR is t, then move up, otherwise move down."
  (when (null arg)
    (setq arg 5))
  (let ((top-edge (nth 1 (window-edges))))
    (if (switch-window--xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun switch-window-mvborder-left (arg)
  (interactive "P")
  (switch-window--mvborder-left-or-right arg t))

(defun switch-window-mvborder-right (arg)
  (interactive "P")
  (switch-window--mvborder-left-or-right arg nil))

(defun switch-window-mvborder-up (arg)
  (interactive "P")
  (switch-window--mvborder-up-or-down arg t))

(defun switch-window-mvborder-down (arg)
  (interactive "P")
  (switch-window--mvborder-up-or-down arg nil))

;; * Footer
(provide 'switch-window-mvborder)

;;; switch-window-mvborder.el ends here
