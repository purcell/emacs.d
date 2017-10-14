;;; elm-mode.el --- Major mode for Elm

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015, 2016  Bogdan Popa

;; Author: Joseph Collard
;; Package-Requires: ((f "0.17") (let-alist "1.0.4") (s "1.7.0") (emacs "24.4"))
;; URL: https://github.com/jcollard/elm-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'elm-tags)
(require 'elm-format)
(require 'elm-imenu)
(require 'elm-indent)
(require 'elm-interactive)
(require 'elm-font-lock)

(defgroup elm nil
  "Support for the elm programming language."
  :link '(url-link :tag "Github" "https://github.com/jcollard/elm-mode")
  :group 'languages)

(defun elm-beginning-of-defun (&optional arg)
  "Move backward to the beginning of an ELM \"defun\".
With ARG, do it that many times.  Negative arg -N means move
forward to Nth following beginning of defun.  Returns t unless
search stops due to beginning or end of buffer.

Find the roots of this function in the c-awk-mode."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (let ((found t) ; Has the most recent regexp search found b-of-defun?
          (regexp "^[^- \t\n\r]"))
      (if (>= arg 0)
          ;; Go back one defun each time round the following loop. (For +ve arg)
          (while (and found (< 0 arg) (not (eq (point) (point-min))))
            ;; Go back one "candidate" each time round the next loop until one
            ;; is genuinely a beginning-of-defun.

            (setq found (search-backward-regexp
                         regexp (point-min) 'stop-at-limit))
            (when found
              (while (and (forward-line -1)
                          (looking-at regexp)
                          (not (= (point-min) (point)))))
              (forward-line))
            (setq arg (1- arg)))
        ;; The same for a -ve arg.
        (if (not (eq (point) (point-max))) (forward-char 1))
        (while (and found (< arg 0) (not (eq (point) (point-max)))) ; The same for -ve arg.
          (setq found (search-forward-regexp
                       regexp (point-min) 'stop-at-limit))
          (setq arg (1+ arg))))
      (eq arg 0))))

(defun elm-end-of-defun (&optional arg)
  "Move forward to the end of an ELM \"defun\".
With ARG, do it that many times.  Negative arg -N means move
forward to Nth previous end of defun.  Returns t unless
search stops due to beginning or end of buffer.

Find the roots of this function in the c-awk-mode."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (let ((found t) ; Has the most recent regexp search found b-of-defun?
          (regexp "^\n\n"))
      (if (>= arg 0)
          ;; Go back one defun each time round the following loop. (For +ve arg)
          (while (and found (< 0 arg) (not (eq (point) (point-max))))
            ;; Go back one "candidate" each time round the next loop until one
            ;; is genuinely a beginning-of-defun.
            (setq found (search-forward-regexp
                         regexp
                         (point-max) 'stop-at-limit))
            (setq arg (1- arg)))
        ;; The same for a -ve arg.
        (if (not (eq (point) (point-min))) (forward-char 1))
        (while (and found (< arg 0) (not (eq (point) (point-min)))) ; The same for -ve arg.
          (setq found (search-backward-regexp
                       regexp (point-min) 'stop-at-limit))
          (setq arg (1+ arg)))
        (if found (goto-char (match-beginning 0))))
      (eq arg 0))))


(defun elm-mode-after-save-handler ()
  "Perform various operations upon saving a buffer."
  (when elm-sort-imports-on-save
    (elm-sort-imports))
  (when elm-tags-on-save
    (elm-mode-generate-tags))
  (when elm-format-on-save
    (elm-mode-format-buffer))
  (when (or elm-sort-imports-on-save
            elm-tags-on-save
            elm-format-on-save)
    (let ((before-save-hook '())
          (after-save-hook '()))
      (basic-save-buffer))))

(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'elm-mode-format-buffer)
    (define-key map (kbd "C-c M-t") 'elm-mode-generate-tags)
    (define-key map (kbd "M-.") 'elm-mode-goto-tag-at-point)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-l") 'elm-repl-load)
    (define-key map (kbd "C-c C-p") 'elm-repl-push)
    (define-key map (kbd "C-c C-e") 'elm-repl-push-decl)
    (define-key map (kbd "C-c C-z") 'run-elm-interactive)
    (define-key map (kbd "C-c C-a") 'elm-compile-add-annotations)
    (define-key map (kbd "C-c C-r") 'elm-compile-clean-imports)
    (define-key map (kbd "C-c C-c") 'elm-compile-buffer)
    (define-key map (kbd "C-c M-c") 'elm-compile-main)
    (define-key map (kbd "C-c M-k") 'elm-package-catalog)
    (define-key map (kbd "C-c C-n") 'elm-preview-buffer)
    (define-key map (kbd "C-c C-m") 'elm-preview-main)
    (define-key map (kbd "C-c C-d") 'elm-documentation-lookup)
    (define-key map (kbd "C-c C-i") 'elm-import)
    (define-key map (kbd "C-c C-s") 'elm-sort-imports)
    (define-key map (kbd "C-c C-t") 'elm-oracle-type-at-point)
    (define-key map (kbd "C-c M-d") 'elm-oracle-doc-at-point)
    (define-key map (kbd "C-c C-v") 'elm-test-project)
    map)
  "Keymap for Elm major mode.")

;;;###autoload
(define-derived-mode elm-mode prog-mode "Elm"
  "Major mode for editing Elm source code."
  (setq-local indent-tabs-mode nil)

  ;; Elm is not generally suitable for electric indentation, since
  ;; there is no unambiguously correct indent level for any given
  ;; line.
  (when (boundp 'electric-indent-inhibit)
    (setq-local electric-indent-inhibit t))

  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local imenu-create-index-function #'elm-imenu-create-index)
  (setq-local paragraph-separate "\\(\r\t\n\\|-}\\)$")
  (setq-local beginning-of-defun-function #'elm-beginning-of-defun)
  (setq-local end-of-defun-function #'elm-end-of-defun)

  (add-function :before-until (local 'eldoc-documentation-function) #'elm-eldoc)

  (add-hook 'after-save-hook #'elm-mode-after-save-handler nil t)

  (turn-on-elm-font-lock)
  (turn-on-elm-indent))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(provide 'elm-mode)
;;; elm-mode.el ends here
