(require 'cl)
(require 'dabbrev)
(require 'auto-complete)

(defun ac-source-dabbrev-all-buffers-candidates ()
  (interactive)
  (all-completions ac-target
                   (progn (dabbrev--reset-global-variables)
                          (let ((dabbrev-check-all-buffers t))
                            (sort (dabbrev--find-all-expansions ac-target t) #'string<)))))


(defface ac-dabbrev-all-buffers-menu-face
  '((t (:background "lightgray" :foreground "purple")))
  "Face for dabbrev candidate menu."
  :group 'auto-complete)

(defface ac-dabbrev-all-buffers-selection-face
  '((t (:background "purple" :foreground "white")))
  "Face for the dabbrev selected candidate."
  :group 'auto-complete)

(defvar ac-source-dabbrev-all-buffers
  '((candidates . ac-source-dabbrev-all-buffers-candidates)
    (candidate-face . ac-dabbrev-all-buffers-menu-face)
    (selection-face . ac-dabbrev-all-buffers-selection-face))
  "Get all the completions using dabbrev")


(provide 'ac-dabbrev-all-buffers)