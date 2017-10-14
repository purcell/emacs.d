(require 'slime)
(require 'cl-lib)

;;; bits of the following taken from slime-asdf.el

(define-slime-contrib slime-quicklisp
  "Quicklisp support."
  (:authors "Matthew Kennedy <burnsidemk@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-quicklisp))

;;; Utilities

(defgroup slime-quicklisp nil
  "Quicklisp support for Slime."
  :prefix "slime-quicklisp-"
  :group 'slime)

(defvar slime-quicklisp-system-history nil
  "History list for Quicklisp system names.")



(defun slime-read-quicklisp-system-name (&optional prompt default-value)
  "Read a Quick system name from the minibuffer, prompting with PROMPT."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "Quicklisp system"))
         (quicklisp-system-names (slime-eval `(swank:list-quicklisp-systems)))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                  ": "))))
    (completing-read prompt (slime-bogus-completion-alist quicklisp-system-names)
                     nil nil nil
                     'slime-quicklisp-system-history default-value)))

(defun slime-quicklisp-quickload (system)
  "Load a Quicklisp system."
  (slime-save-some-lisp-buffers)
  (slime-display-output-buffer)
  (slime-repl-shortcut-eval-async `(ql:quickload ,system)))

;;; REPL shortcuts

(defslime-repl-shortcut slime-repl-quicklisp-quickload ("quicklisp-quickload" "ql")
  (:handler (lambda ()
              (interactive)
              (slime-quicklisp-quickload (slime-read-quicklisp-system-name))))
  (:one-liner "Load a system known to Quicklisp."))

(provide 'slime-quicklisp)
