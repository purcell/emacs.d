;;; -*- lexical-binding: t -*-
;;; tern-auto-complete.el --- Tern Completion by auto-complete.el

;; Author:  <m.sakurai at kiwanami.net>
;; Version: 0.0.1
;; Package-Requires: ((tern "0.0.1") (auto-complete "1.4") (emacs "24"))

;;; Commentary:

;; Display completion items with its type and document.

;; If `tern-ac-on-dot' is non-nil (default), typing '.(dot)' invokes auto-complete with tern.
;; Calling the command `tern-ac-complete', you can invoke auto-complete manually.
;; This program does not provide an ac-source for arbitrary timing yet.

;;; Installation:

;; Add following lines below the tern setup code.

;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

;;; Code:

(eval-when-compile (require 'cl))
(require 'tern)
(require 'auto-complete)



;;; Completion

(defvar tern-ac-on-dot t "[AC] If t, tern enable completion by auto-completion.")

(defvar tern-ac-complete-reply nil  "[internal] tern-ac-complete-reply.")

(defvar tern-ac-complete-request-point 0
  "[internal] The point where `tern-ac-complete-request' is called.")

(defun tern-ac-complete-request (cc)
  (setq tern-last-point-pos (point))
  (setq tern-ac-complete-reply nil)
  (setq tern-ac-complete-request-point (point))
  (tern-run-query 
   (lambda (data) 
     (tern-ac-complete-response data)
     (funcall cc))
   `((type . "completions") (types . t) (docs . t))
   (point)))

(defun tern-ac-complete-response (data)
  (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 (cdr (assq 'start data))))
        (end (+ 1 (cdr (assq 'end data)))))
    (setq tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
    (setq tern-ac-complete-reply cs)))

(defun tern-ac-complete ()
  "Complete code at point by tern."
  (interactive)
  (tern-ac-complete-request
   (lambda ()
     (let ((ac-sources (cons 'ac-source-tern-completion ac-sources)))
       (ac-start)))))

(defun tern-ac-dot-complete ()
  "Insert dot and complete code at point by tern."
  (interactive)
  (insert ".")
  (tern-ac-complete))

(defvar tern-ac-completion-truncate-length 22
  "[AC] truncation length for type summary.")

(defun tern-ac-completion-matches ()
  (mapcar
   (lambda (item)
     (let ((doc (cdr (assq 'doc item)))
           (type (cdr (assq 'type item)))
           (name (cdr (assq 'name item))))
       (popup-make-item 
        name
        :symbol (if (string-match "fn" type) "f" "v")
        :summary (truncate-string-to-width 
                  type tern-ac-completion-truncate-length 0 nil "...")
        :document (concat type "\n\n" doc))))
   tern-ac-complete-reply))

(defun tern-ac-completion-prefix ()
  (or (ac-prefix-default)
      (when (= tern-ac-complete-request-point (point))
        tern-ac-complete-request-point)))

;; (makunbound 'ac-source-tern-completion)
(eval-after-load 'auto-complete
  '(progn
     (ac-define-source tern-completion
       '((candidates . tern-ac-completion-matches)
         (prefix . tern-ac-completion-prefix)
         (requires . -1)))))

;;;###autoload
(defun tern-ac-setup ()
  "Setup auto-complete for tern-mode."
  (interactive)
  (if tern-ac-on-dot
      (define-key tern-mode-keymap "." 'tern-ac-dot-complete)
    (define-key tern-mode-keymap "." nil)))


(provide 'tern-auto-complete)
;;; tern-auto-complete.el ends here
