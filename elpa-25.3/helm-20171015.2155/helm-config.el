;;; helm-config.el --- Applications library for `helm.el' -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;; Require
;;
;;
(declare-function async-bytecomp-package-mode "ext:async-bytecomp.el")
(when (require 'async-bytecomp nil t)
  (and (fboundp 'async-bytecomp-package-mode)
       (async-bytecomp-package-mode 1)))


(defgroup helm-config nil
  "Various configurations for Helm."
  :group 'helm)

(defcustom helm-command-prefix-key "C-x c"
  "The key `helm-command-prefix' is bound to in the global map."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (when (and (boundp var) (symbol-value var))
      (define-key (current-global-map)
          (read-kbd-macro (symbol-value var)) nil))
    (when key
      (define-key (current-global-map)
          (read-kbd-macro key) 'helm-command-prefix))
    (set var key)))

(defcustom helm-minibuffer-history-key "C-r"
  "The key `helm-minibuffer-history' is bound to in minibuffer local maps."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (cl-dolist (map '(minibuffer-local-completion-map
                      minibuffer-local-filename-completion-map
                      minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                      minibuffer-local-isearch-map
                      minibuffer-local-map
                      minibuffer-local-must-match-filename-map ; Older Emacsen
                      minibuffer-local-must-match-map
                      minibuffer-local-ns-map))
      (when (and (boundp map) (keymapp (symbol-value map)))
        (when (and (boundp var) (symbol-value var))
          (define-key (symbol-value map)
              (read-kbd-macro (symbol-value var)) nil))
        (when key
          (define-key (symbol-value map)
              (read-kbd-macro key) 'helm-minibuffer-history))))
    (set var key)))

;;; Command Keymap
;;
;;
(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-apropos)
    (define-key map (kbd "e")         'helm-etags-select)
    (define-key map (kbd "l")         'helm-locate)
    (define-key map (kbd "s")         'helm-surfraw)
    (define-key map (kbd "r")         'helm-regexp)
    (define-key map (kbd "m")         'helm-man-woman)
    (define-key map (kbd "t")         'helm-top)
    (define-key map (kbd "/")         'helm-find)
    (define-key map (kbd "i")         'helm-semantic-or-imenu)
    (define-key map (kbd "I")         'helm-imenu-in-all-buffers)
    (define-key map (kbd "<tab>")     'helm-lisp-completion-at-point)
    (define-key map (kbd "p")         'helm-list-emacs-process)
    (define-key map (kbd "C-x r b")   'helm-filtered-bookmarks)
    (define-key map (kbd "M-y")       'helm-show-kill-ring)
    (define-key map (kbd "C-c <SPC>") 'helm-all-mark-rings)
    (define-key map (kbd "C-x C-f")   'helm-find-files)
    (define-key map (kbd "f")         'helm-multi-files)
    (define-key map (kbd "C-:")       'helm-eval-expression-with-eldoc)
    (define-key map (kbd "C-,")       'helm-calcul-expression)
    (define-key map (kbd "M-x")       'helm-M-x)
    (define-key map (kbd "M-s o")     'helm-occur)
    (define-key map (kbd "M-g a")     'helm-do-grep-ag)
    (define-key map (kbd "c")         'helm-colors)
    (define-key map (kbd "F")         'helm-select-xfont)
    (define-key map (kbd "8")         'helm-ucs)
    (define-key map (kbd "C-c f")     'helm-recentf)
    (define-key map (kbd "C-c g")     'helm-google-suggest)
    (define-key map (kbd "h i")       'helm-info-at-point)
    (define-key map (kbd "h r")       'helm-info-emacs)
    (define-key map (kbd "h g")       'helm-info-gnus)
    (define-key map (kbd "h h")       'helm-documentation)
    (define-key map (kbd "C-x C-b")   'helm-buffers-list)
    (define-key map (kbd "C-x r i")   'helm-register)
    (define-key map (kbd "C-c C-x")   'helm-run-external-command)
    (define-key map (kbd "b")         'helm-resume)
    (define-key map (kbd "M-g i")     'helm-gid)
    (define-key map (kbd "@")         'helm-list-elisp-packages)
    map))

;; Don't override the keymap we just defined with an empty
;; keymap.  This also protect bindings changed by the user.
(defvar helm-command-prefix)
(define-prefix-command 'helm-command-prefix)
(fset 'helm-command-prefix helm-command-map)
(setq  helm-command-prefix helm-command-map)


;;; Menu

(require 'helm-easymenu)


;;;###autoload
(defun helm-configuration ()
  "Customize `helm'."
  (interactive)
  (customize-group "helm"))


;;; Fontlock
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(with-helm-after-update-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-temp-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-window\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-quittable\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-current-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-show-completion\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-default-directory\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-restore-variables\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-multi-key-defun\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-while-no-input\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-aif\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-awhile\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-acond\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-aand\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-with-gensyms\\)\\>" 1 font-lock-keyword-face))))


;;; Load the autoload file
;;  It should have been generated either by
;;  package.el or the make file.

(load "helm-autoloads" nil t)

(provide 'helm-config)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-config.el ends here
