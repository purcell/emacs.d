;;; tern-auto-complete.el --- Tern Completion by auto-complete.el -*- lexical-binding: t -*-

;; Author:  <m.sakurai at kiwanami.net>
;; Version: 0.0.1
;; Package-Version: 20170521.1235
;; Package-Requires: ((tern "0.0.1") (auto-complete "1.4") (cl-lib "0.5") (emacs "24"))

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

(require 'cl-lib)
(require 'tern)
(require 'auto-complete)



;;; Completion

(defcustom tern-ac-on-dot t
  "[AC] If t, tern enable completion by auto-completion."
  :type 'boolean
  :group 'auto-complete)

(defcustom tern-ac-sync t
  "[AC] If t, auto-complete will wait for tern canditates before starting.
This enables tern canditates to integrate automatically in auto-complete without
the need for a separate keybinding.

Remember to add ac-source-tern-completion to ac-sources."
  :type 'boolean
  :group 'auto-complete)


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
   `((type . "completions") (types . t) (docs . t) (caseInsensitive . t))
   (point)))

(defun tern-ac-complete-response (data)
  (let ((cs (cl-loop for elt across (cdr (assq 'completions data)) collect elt))
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
  (unless (nth 4 (syntax-ppss))
    (tern-ac-complete)))

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
        :symbol (if (null type) "?" (if (string-match "fn" type) "f" "v"))
        :summary (truncate-string-to-width
                  (or type "?") tern-ac-completion-truncate-length 0 nil "...")
        :document (concat type "\n\n" doc))))
   tern-ac-complete-reply))

(defun tern-ac-completion-prefix ()
  (or (ac-prefix-default)
      (when (= tern-ac-complete-request-point (point))
        tern-ac-complete-request-point)))

;; (makunbound 'ac-source-tern-completion)
(ac-define-source tern-completion
  '((candidates . tern-ac-completion-matches)
    (prefix . tern-ac-completion-prefix)
    (requires . -1)))

;;;###autoload
(defun tern-ac-setup ()
  "Setup auto-complete for tern-mode."
  (interactive)
  (if tern-ac-on-dot
      (define-key tern-mode-keymap "." 'tern-ac-dot-complete)
    (define-key tern-mode-keymap "." nil)))

(defvar tern-ac-js-major-modes '(rjsx-mode js2-mode js2-jsx-mode js-mode js-jsx-mode javascript-mode))

(defadvice auto-complete (around add-tern-ac-candidates first activate)
  "Load tern-js canditates before ac-start."
  (if (and tern-ac-sync
           (memq major-mode tern-ac-js-major-modes)
           (not (or (ac-menu-live-p) (ac-inline-live-p))))
      (tern-ac-complete-request
       'auto-complete-1)
    ad-do-it))


(provide 'tern-auto-complete)
;;; tern-auto-complete.el ends here
