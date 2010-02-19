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

(defvar ac-slime-current-doc nil "Holds slime docstring for current symbol")
(defun ac-slime-documentation (symbol-name)
  (let ((symbol-name (substring-no-properties symbol-name)))
    (slime-eval `(swank:documentation-symbol ,symbol-name))))

(defun ac-slime-init ()
  (setq ac-slime-current-doc nil))

(defface ac-slime-menu-face
  '((t (:background "lightgray" :foreground "darkgreen")))
  "Face for slime candidate menu."
  :group 'auto-complete)

(defface ac-slime-selection-face
  '((t (:background "darkgreen" :foreground "white")))
  "Face for the slime selected candidate."
  :group 'auto-complete)

(defvar ac-source-slime-fuzzy
  '((init . ac-slime-init)
    (candidates . ac-source-slime-fuzzy-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos)
    (symbol . "l")
    (document . ac-slime-documentation))
  "Source for fuzzy slime completion")

(defvar ac-source-slime-simple
  '((init . ac-slime-init)
    (candidates . ac-source-slime-simple-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos)
    (symbol . "l")
    (document . ac-slime-documentation))
  "Source for slime completion")


(defun set-up-slime-ac (&optional fuzzy)
  "Add an optionally-fuzzy slime completion source to the
front of `ac-sources' for the current buffer."
  (interactive)
  (setq ac-sources (add-to-list 'ac-sources
                                (if fuzzy
                                    'ac-source-slime-fuzzy
                                  'ac-source-slime-simple))))


(provide 'ac-slime)