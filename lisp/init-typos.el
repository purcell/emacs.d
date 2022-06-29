;;; init-typos.el --- check typos using typos  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jianshi Liu

;; (require 'simple)

(defun typos-buffer ()
  "check typos of current buffer"
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "typos -"))

;; (my-typos curser)

(provide 'init-typos)
