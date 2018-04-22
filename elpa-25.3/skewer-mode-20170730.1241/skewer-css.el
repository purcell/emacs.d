;;; skewer-css.el --- skewer support for live-interaction CSS -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This minor mode provides functionality for CSS like plain Skewer
;; does for JavaScript.

;; * C-x C-e -- `skewer-css-eval-current-declaration'
;; * C-M-x   -- `skewer-css-eval-current-rule'
;; * C-c C-k -- `skewer-css-eval-buffer'

;; These functions assume there are no comments within a CSS rule,
;; *especially* not within a declaration. In the former case, if you
;; keep the comment free of CSS syntax it should be able to manage
;; reasonably well. This may be fixed someday.

;;; Code:

(require 'css-mode)
(require 'skewer-mode)

(defun skewer-css-trim (string)
  "Trim and compress whitespace in the string."
  (let ((cleaned (replace-regexp-in-string "[\t\n ]+" " " string)))
    (replace-regexp-in-string "^[\t\n ]+\\|[\t\n ]+$" "" cleaned)))

;; Parsing

(defun skewer-css-beginning-of-rule ()
  "Move to the beginning of the current rule and return point."
  (skewer-css-end-of-rule)
  (re-search-backward "{")
  (when (re-search-backward "[}/]" nil 'start)
    (forward-char))
  (re-search-forward "[^ \t\n]")
  (backward-char)
  (point))

(defun skewer-css-end-of-rule ()
  "Move to the end of the current rule and return point."
  (if (eql (char-before) ?})
      (point)
    (re-search-forward "}")))

(defun skewer-css-end-of-declaration ()
  "Move to the end of the current declaration and return point."
  (if (eql (char-before) ?\;)
      (point)
    (re-search-forward ";")))

(defun skewer-css-beginning-of-declaration ()
  "Move to the end of the current declaration and return point."
  (skewer-css-end-of-declaration)
  (re-search-backward ":")
  (backward-sexp 1)
  (point))

(defun skewer-css-selectors ()
  "Return the selectors for the current rule."
  (save-excursion
    (let ((start (skewer-css-beginning-of-rule))
          (end (1- (re-search-forward "{"))))
      (skewer-css-trim
       (buffer-substring-no-properties start end)))))

(defun skewer-css-declaration ()
  "Return the current declaration as a pair of strings."
  (save-excursion
    (let ((start (skewer-css-beginning-of-declaration))
          (end (skewer-css-end-of-declaration)))
      (let* ((clip (buffer-substring-no-properties start end))
             (pair (split-string clip ":")))
        (mapcar #'skewer-css-trim pair)))))

;; Evaluation

(defun skewer-css (rule)
  "Add RULE as a new stylesheet."
  (skewer-eval rule nil :type "css"))

(defun skewer-css-eval-current-declaration ()
  "Evaluate the declaration at the point."
  (interactive)
  (save-excursion
    (let ((selectors (skewer-css-selectors))
          (rule (skewer-css-declaration))
          (start (skewer-css-beginning-of-declaration))
          (end (skewer-css-end-of-declaration)))
      (skewer-flash-region start end)
      (skewer-css (apply #'format "%s { %s: %s }" selectors rule)))))

(defun skewer-css-eval-current-rule ()
  "Evaluate the rule at the point."
  (interactive)
  (save-excursion
    (let* ((start (skewer-css-beginning-of-rule))
           (end (skewer-css-end-of-rule))
           (rule (buffer-substring-no-properties start end)))
      (skewer-flash-region start end)
      (skewer-css (skewer-css-trim rule)))))

(defun skewer-css-eval-buffer ()
  "Send the entire current buffer as a new stylesheet."
  (interactive)
  (skewer-css (buffer-substring-no-properties (point-min) (point-max))))

(defun skewer-css-clear-all ()
  "Remove *all* Skewer-added styles from the document."
  (interactive)
  (skewer-eval nil nil :type "cssClearAll"))

;; Minor mode definition

(defvar skewer-css-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") 'skewer-css-eval-current-declaration)
      (define-key map (kbd "C-M-x") 'skewer-css-eval-current-rule)
      (define-key map (kbd "C-c C-k") 'skewer-css-eval-buffer)
      (define-key map (kbd "C-c C-c") 'skewer-css-clear-all)))
  "Keymap for skewer-css-mode.")

;;;###autoload
(define-minor-mode skewer-css-mode
  "Minor mode for interactively loading new CSS rules."
  :lighter " skewer-css"
  :keymap skewer-css-mode-map
  :group 'skewer)

(provide 'skewer-css)

;;; skewer-css.el ends here
