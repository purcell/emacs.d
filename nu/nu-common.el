;; nu-common.el --- Nubank common functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank common functions

;;; Code:

(defcustom nu-home
  (getenv "NU_HOME")
  "The current Nubank home directory."
  :group 'nubank
  :type 'string)

(defun nu-home-directory ()
  "Return the Nu home as a directory."
  (if nu-home
      (file-name-as-directory (expand-file-name nu-home))
    (user-error "Please customize the `nu-home` ELisp variable or set the NU_HOME environment variable")))

(defun nu-slurp-file (filename)
  "Return the content of `FILENAME`'."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(provide 'nu-common)

;;; nu-common.el ends here
