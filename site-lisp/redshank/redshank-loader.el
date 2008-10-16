;;; redshank-loader.el --- Loader for Redshank

;; Copyright (C) 2008  Michael Weber

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: languages, lisp

;;; Setup
;; See file redshank.el

;;; Code
(let* ((redshank-file (or (locate-library "redshank")
                          load-file-name))
       (redshank-path (and redshank-file
                           (file-name-directory redshank-file))))
  (when redshank-path
    (add-to-list 'load-path redshank-path)))

(autoload 'redshank-mode "redshank"
  "Minor mode for editing and refactoring (Common) Lisp code."
  t)
(autoload 'turn-on-redshank-mode "redshank"
  "Turn on Redshank mode.  Please see function `redshank-mode'."
  t)

(autoload 'asdf-mode "redshank"
  "Minor mode for editing ASDF files." t)
(autoload 'turn-on-asdf-mode "redshank"
  "Turn on ASDF mode.  Please see function `asdf-mode'." t)

;;;###autoload
(defun redshank-setup (hooks &optional autoinsertp)
  "Installs `redshank-mode' on major mode hooks listed in HOOKS.
If AUTOINSERTP is non-nil and `auto-insert-mode' is available,
activate support for that, too."
  (dolist (hook hooks)
    (add-hook hook 'turn-on-redshank-mode))
  (add-to-list 'auto-mode-alist '("\\.asdf?\\'" . asdf-mode))
  (when autoinsertp
    (eval-after-load "autoinsert"
      '(progn
         (add-to-list 'auto-insert-alist
                      '(lisp-mode . [redshank-mode-line-skeleton
                                     redshank-in-package-skeleton]))
         (add-to-list 'auto-insert-alist
                      '(asdf-mode . [redshank-mode-line-skeleton
                                     redshank-asdf-defsystem-skeleton]))))))

(provide 'redshank-loader)
;;; redshank-loader.el ends here
