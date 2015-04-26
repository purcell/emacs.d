;;; -*- lexical-binding: t -*-
;;; tern-ido-complete.el --- Tern Completion by ido.el

;; Author:  <https://github.com/katspaugh>
;; Version: 0.0.1

;;; Commentary:

;; Display and refine completions in minibuffer using
;; `ido-completing-read'.

;;; Installation:

;; Add the following lines below the Tern setup code.

;; (eval-after-load 'tern
;;   '(progn
;;      (setq ido-record-commands nil)
;;      (setq ido-max-window-height 1)
;;      (local-set-key (kbd "M-<tab>") 'tern-ido-complete)))

;;; Code:

(require 'tern)
(require 'ido)


(defun tern-ido-insert (start end choice)
  (let ((completion-in-region-mode-predicate nil))
    (completion-in-region start end (list choice))))

(defun tern-ido-display (data)
  (when (eq (point) tern-last-point-pos)
    (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
          (start (+ 1 (cdr (assq 'start data))))
          (end (+ 1 (cdr (assq 'end data)))))
      (if (eq 1 (length cs))
          (tern-ido-insert start end cs)
        (run-with-idle-timer 0 nil 'tern-ido-read start end cs)))))

(defun tern-ido-read (start end cs)
  (when (eq (point) tern-last-point-pos)
    (tern-ido-insert
     start end (ido-completing-read "" cs nil nil (thing-at-point 'symbol)))))

(defun tern-ido-complete ()
  (interactive)
  (setq tern-last-point-pos (point))
  (tern-run-query 'tern-ido-display "completions" (point)))


(provide 'tern-ido-complete)
;;; tern-ido-complete.el ends here
