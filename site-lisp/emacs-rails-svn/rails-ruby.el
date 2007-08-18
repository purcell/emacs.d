;;; rails-ruby.el --- provide features for ruby-mode

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-ruby.el $
;; $Id: rails-ruby.el 211 2007-08-17 17:10:08Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

;; setup align for ruby-mode
(require 'align)

(defconst align-ruby-modes '(ruby-mode)
  "align-perl-modes is a variable defined in `align.el'.")

(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

(add-to-list 'align-perl-modes 'ruby-mode)
(add-to-list 'align-dq-string-modes 'ruby-mode)
(add-to-list 'align-sq-string-modes 'ruby-mode)
(add-to-list 'align-open-comment-modes 'ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))

;; hideshow ruby support

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ··· %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

(eval-after-load "hideshow"
  (unless 'hs-set-up-overlay
    (setq hs-set-up-overlay 'display-code-line-counts)))

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (unless hs-set-up-overlay
              (setq hs-set-up-overlay 'display-code-line-counts))))

(defun ruby-hs-minor-mode (&optional arg)
  (interactive)
  (require 'hideshow)
  (unless (assoc 'ruby-mode hs-special-modes-alist)
    (setq
     hs-special-modes-alist
     (cons (list 'ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (&rest args) (ruby-end-of-block))
                 ;(lambda (&rest args) (ruby-beginning-of-defun))
                 )
           hs-special-modes-alist)))
  (hs-minor-mode arg))

;; flymake ruby support

(require 'flymake nil t)

(defconst flymake-allowed-ruby-file-name-masks
  '(("\\.rb\\'"      flymake-ruby-init)
    ("\\.rxml\\'"    flymake-ruby-init)
    ("\\.builder\\'" flymake-ruby-init)
    ("\\.rjs\\'"     flymake-ruby-init))
  "Filename extensions that switch on flymake-ruby mode syntax checks.")

(defconst flymake-ruby-error-line-pattern-regexp
  '("^\\([^:]+\\):\\([0-9]+\\): *\\([\n]+\\)" 1 2 nil 3)
  "Regexp matching ruby error messages.")

(defun flymake-ruby-init ()
  (condition-case er
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list rails-ruby-command (list "-c" local-file)))
    ('error ())))

(defun flymake-ruby-load ()
  (when (and (buffer-file-name)
             (string-match
              (format "\\(%s\\)"
                      (string-join
                       "\\|"
                       (mapcar 'car flymake-allowed-ruby-file-name-masks)))
              (buffer-file-name)))
    (setq flymake-allowed-file-name-masks
          (append flymake-allowed-file-name-masks flymake-allowed-ruby-file-name-masks))
    (setq flymake-err-line-patterns
          (cons flymake-ruby-error-line-pattern-regexp flymake-err-line-patterns))
    (flymake-mode t)
    (local-set-key (rails-key "d") 'flymake-display-err-menu-for-current-line)))

(when (featurep 'flymake)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

;; other stuff

(defun ruby-newline-and-indent ()
  (interactive)
  (newline)
  (ruby-indent-command))

(defun ruby-toggle-string<>simbol ()
  "Easy to switch between strings and symbols."
  (interactive)
  (let ((initial-pos (point)))
    (save-excursion
      (when (looking-at "[\"']") ;; skip beggining quote
        (goto-char (+ (point) 1))
        (unless (looking-at "\\w")
          (goto-char (- (point) 1))))
      (let* ((point (point))
             (start (skip-syntax-backward "w"))
             (end (skip-syntax-forward "w"))
             (end (+ point start end))
             (start (+ point start))
             (start-quote (- start 1))
             (end-quote (+ end 1))
             (quoted-str (buffer-substring-no-properties start-quote end-quote))
             (symbol-str (buffer-substring-no-properties start end)))
        (cond
         ((or (string-match "^\"\\w+\"$" quoted-str)
              (string-match "^\'\\w+\'$" quoted-str))
          (setq quoted-str (substring quoted-str 1 (- (length quoted-str) 1)))
          (kill-region start-quote end-quote)
          (goto-char start-quote)
          (insert (concat ":" quoted-str)))
         ((string-match "^\:\\w+$" symbol-str)
          (setq symbol-str (substring symbol-str 1))
          (kill-region start end)
          (goto-char start)
          (insert (format "'%s'" symbol-str))))))
    (goto-char initial-pos)))

(require 'inf-ruby)

(defun run-ruby-in-buffer (cmd buf)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (when (not (comint-check-proc abuf))
      (set-buffer (make-comint buf rails-ruby-command nil cmd)))
    (inferior-ruby-mode)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
          inferior-ruby-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun complete-ruby-method (prefix &optional maxnum)
  (if (capital-word-p prefix)
      (let* ((cmd "x = []; ObjectSpace.each_object(Class){|i| x << i.to_s}; x.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}")
             (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
        (el4r-ruby-eval (format cmd prefix prefix)))
    (save-excursion
      (goto-char (- (point) (+ 1 (length prefix))))
      (when (and (looking-at "\\.")
                 (capital-word-p (word-at-point))
                 (el4r-ruby-eval (format "::%s rescue nil" (word-at-point))))
        (let* ((cmd "%s.public_methods.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}")
               (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
          (el4r-ruby-eval (format cmd (word-at-point) prefix prefix)))))))


(provide 'rails-ruby)