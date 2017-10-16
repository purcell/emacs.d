;;; helm-eval.el --- eval expressions from helm. -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'eldoc)
(require 'edebug)


(defgroup helm-eval nil
  "Eval related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-eldoc-in-minibuffer-show-fn
  'helm-show-info-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display."
  :group 'helm-eval
  :type  'symbol)

(defcustom helm-show-info-in-mode-line-delay 12
  "Eldoc will show info in mode-line during this delay if user is idle."
  :type  'integer
  :group 'helm-eval)


;;; Eldoc compatibility between emacs-24 and emacs-25
;;
(if (require 'elisp-mode nil t)    ; emacs-25
    ;; Maybe the eldoc functions have been
    ;; already aliased by eldoc-eval.
    (cl-loop for (f . a) in '((eldoc-current-symbol .
                               elisp--current-symbol)
                              (eldoc-fnsym-in-current-sexp .
                               elisp--fnsym-in-current-sexp) 
                              (eldoc-get-fnsym-args-string .
                               elisp-get-fnsym-args-string) 
                              (eldoc-get-var-docstring .
                               elisp-get-var-docstring))
             unless (fboundp f)
             do (defalias f a))
    ;; Emacs-24.
    (declare-function eldoc-current-symbol "eldoc")
    (declare-function eldoc-get-fnsym-args-string "eldoc" (sym &optional index))
    (declare-function eldoc-get-var-docstring "eldoc" (sym))
    (declare-function eldoc-fnsym-in-current-sexp "eldoc"))

;;; Evaluation Result
;;
;;
;; Internal
(defvar helm-eldoc-active-minibuffers-list nil)

(defvar helm-eval-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-eval-new-line-and-indent)
    (define-key map (kbd "<M-tab>")      'lisp-indent-line)
    (define-key map (kbd "<C-tab>")    'helm-lisp-completion-at-point)
    (define-key map (kbd "C-p")        'previous-line)
    (define-key map (kbd "C-n")        'next-line)
    (define-key map (kbd "<up>")       'previous-line)
    (define-key map (kbd "<down>")     'next-line)
    (define-key map (kbd "<right>")    'forward-char)
    (define-key map (kbd "<left>")     'backward-char)
    map))

(defun helm-build-evaluation-result-source ()
  (helm-build-dummy-source "Evaluation Result"
    :multiline t
    :mode-line "C-RET: nl-and-indent, M-tab: reindent, C-tab:complete, C-p/n: next/prec-line."
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (list
                                       (condition-case nil
                                           (with-helm-current-buffer
                                             (pp-to-string
                                              (if edebug-active
                                                  (edebug-eval-expression
                                                   (read helm-pattern))
                                                  (eval (read helm-pattern)))))
                                         (error "Error"))))
    :nohighlight t
    :action '(("Copy result to kill-ring" . (lambda (candidate)
                                              (kill-new
                                               (replace-regexp-in-string
                                                "\n" "" candidate))
                                              (message "Result copied to kill-ring")))
              ("copy sexp to kill-ring" . (lambda (_candidate)
                                            (kill-new helm-input)
                                            (message "Sexp copied to kill-ring"))))))

(defun helm-eval-new-line-and-indent ()
  (interactive)
  (newline) (lisp-indent-line))

(defun helm-eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `helm-eldoc-active-minibuffers-list'."
  (with-selected-window (minibuffer-window)
    (push (current-buffer) helm-eldoc-active-minibuffers-list)))

(defun helm-eldoc-show-in-eval ()
  "Return eldoc in mode-line for current minibuffer input."
  (let ((buf (window-buffer (active-minibuffer-window))))
    (condition-case err
        (when (member buf helm-eldoc-active-minibuffers-list)
          (with-current-buffer buf
            (let* ((sym     (save-excursion
                              (unless (looking-back ")\\|\"" (1- (point)))
                                (forward-char -1))
                              (eldoc-current-symbol)))
                   (info-fn (eldoc-fnsym-in-current-sexp))
                   (doc     (or (eldoc-get-var-docstring sym)
                                (eldoc-get-fnsym-args-string
                                 (car info-fn) (cadr info-fn)))))
              (when doc (funcall helm-eldoc-in-minibuffer-show-fn doc)))))
      (error (message "Eldoc in minibuffer error: %S" err) nil))))

(defun helm-show-info-in-mode-line (str)
  "Display string STR in mode-line."
  (save-selected-window
    (with-current-buffer helm-buffer
      (let ((mode-line-format (concat " " str)))
        (force-mode-line-update)
        (sit-for helm-show-info-in-mode-line-delay))
      (force-mode-line-update))))

;;; Calculation Result
;;
;;
(defvar helm-source-calculation-result
  (helm-build-dummy-source "Calculation Result"
    :filtered-candidate-transformer (lambda (_candidates _source)
                                      (list
                                       (condition-case nil
                                           (calc-eval helm-pattern)
                                         (error "error"))))
    :nohighlight t
    :action '(("Copy result to kill-ring" . (lambda (candidate)
                                              (kill-new candidate)
                                              (message "Result \"%s\" copied to kill-ring"
                                                       candidate)))
              ("Copy operation to kill-ring" . (lambda (_candidate)
                                                 (kill-new helm-input)
                                                 (message "Calculation copied to kill-ring"))))))

;;;###autoload
(defun helm-eval-expression (arg)
  "Preconfigured helm for `helm-source-evaluation-result'."
  (interactive "P")
  (helm :sources (helm-build-evaluation-result-source)
        :input (when arg (thing-at-point 'sexp))
        :buffer "*helm eval*"
        :echo-input-in-header-line nil
        :history 'read-expression-history
        :keymap helm-eval-expression-map))

(defvar eldoc-idle-delay)
;;;###autoload
(defun helm-eval-expression-with-eldoc ()
  "Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. "
  (interactive)
  (let ((timer (run-with-idle-timer
                eldoc-idle-delay 'repeat
                'helm-eldoc-show-in-eval)))
    (unwind-protect
         (minibuffer-with-setup-hook
             'helm-eldoc-store-minibuffer
           (call-interactively 'helm-eval-expression))
      (and timer (cancel-timer timer))
      (setq helm-eldoc-active-minibuffers-list
            (cdr helm-eldoc-active-minibuffers-list)))))

;;;###autoload
(defun helm-calcul-expression ()
  "Preconfigured helm for `helm-source-calculation-result'."
  (interactive)
  (helm :sources 'helm-source-calculation-result
        :buffer "*helm calcul*"))

(provide 'helm-eval)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-eval.el ends here
