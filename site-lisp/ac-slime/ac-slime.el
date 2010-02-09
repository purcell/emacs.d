;;----------------------------------------------------------------------------
;; An auto-complete source using slime completions
;;
;; Usage:
;;   (require 'ac-slime)
;;   (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;----------------------------------------------------------------------------

(eval-when-compile (require 'cl))

(defun ac-source-slime-fuzzy-candidates ()
  "Return a possibly-empty list of fuzzy completions for the symbol at point."
  (if (slime-connected-p)
      (let ((slime-fuzzy-completion-limit 50))
        (mapcar 'car (car (slime-fuzzy-completions  ac-prefix))))))

(defun ac-source-slime-simple-candidates ()
  "Return a possibly-empty list of completions for the symbol at point."
  (if (slime-connected-p)
      (car (slime-simple-completions  ac-prefix))))

(defface ac-slime-menu-face
  '((t (:background "lightgray" :foreground "darkgreen")))
  "Face for slime candidate menu."
  :group 'auto-complete)

(defface ac-slime-selection-face
  '((t (:background "darkgreen" :foreground "white")))
  "Face for the slime selected candidate."
  :group 'auto-complete)

(defvar ac-source-slime-fuzzy
  '((candidates . ac-source-slime-fuzzy-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos))
  "Source for fuzzy slime completion")

(defvar ac-source-slime-simple
  '((candidates . ac-source-slime-simple-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos))
  "Source for slime completion")


(defun set-up-slime-ac (&optional fuzzy)
  "Add an optionally-fuzzy slime completion source to the
front of `ac-sources' for the current buffer."
  (interactive)
  (setq ac-sources (add-to-list 'ac-sources
                                (if fuzzy
                                    'ac-source-slime-fuzzy
                                  'ac-source-slime-simple)
                                t)))


(provide 'ac-slime)