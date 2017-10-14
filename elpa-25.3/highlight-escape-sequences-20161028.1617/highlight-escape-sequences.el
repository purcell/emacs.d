;;; highlight-escape-sequences.el --- Highlight escape sequences -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2015, 2016  Free Software Foundation, Inc.

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;;	Pavel Matcula <dev.plvlml@gmail.com>
;; URL:      https://github.com/dgutov/highlight-escape-sequences
;; Package-Version: 20161028.1617
;; Keywords: convenience
;; Version:  0.3

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This global minor mode highlights escape sequences in strings and
;; other kinds of literals with `hes-escape-sequence-face' and with
;; `hes-escape-backslash-face'. They inherit from faces
;; `font-lock-regexp-grouping-construct' and
;; `font-lock-regexp-grouping-backslash' by default, respectively.

;; It currently supports `ruby-mode', `emacs-lisp-mode', JS escape
;; sequences in both popular modes, C escapes is `c-mode', `c++-mode'
;; and `objc-mode', and Java escapes in `java-mode' and `clojure-mode'.

;; To enable it elsewhere, customize `hes-mode-alist'.

;; Put this in the init file:
;;
;; (hes-mode)

;;; Code:

(defgroup hes-mode nil
  "Highlight escape sequences."
  :group 'convenience)

(defface hes-escape-backslash-face
  '((default :inherit font-lock-regexp-grouping-backslash))
  "Face to highlight an escape backslash.")

(defface hes-escape-sequence-face
  '((default :inherit font-lock-regexp-grouping-construct))
  "Face to highlight an escape sequence.")

(defconst hes-common-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (repeat 2 xdigit))
                     (and ?u (repeat 4 xdigit))
                     (any "\"\'\\bfnrtv"))))))
  "Regexp to match the most common escape sequences.

Currently handles:
- octals (\\0 to \\777),
- hexadecimals (\\x00 to \\xFF),
- unicodes (\\u0000 to \\uFFFF),
- and backslash followed by one of \"\'\\bfnrtv.")

(defconst hes-c/c++/objc-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (1+ xdigit))
                     (and ?u (repeat 4 xdigit))
                     (and ?U (repeat 8 xdigit))
                     (any "\"\'\?\\abfnrtv"))))))
  "Regexp to match C/C++/ObjC escape sequences.

Currently handles:
- octals (\\0 to \\777),
- hexadecimals (\\x0 to \\xF..),
- unicodes (\\u0000 to \\uFFFF, \\U00000000 to \\UFFFFFFFF),
- and backslash followed by one of \"\'\?\\abfnrtv.")

(defconst hes-java-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?u (repeat 4 xdigit))
                     (any "\"\'\\bfnrt"))))))
  "Regexp to match Java escape sequences.

Currently handles:
- octals (\\0 to \\777),
- unicodes (\\u0000 to \\uFFFF),
- and backslash followed by one of \"\'\\bfnrt.")

(defconst hes-js-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (repeat 2 xdigit))
                     (and ?u (repeat 4 xdigit))
                     ;; (any "\"\'\\bfnrtv")
                     not-newline))))) ;; deprecated
  "Regexp to match JavaScript escape sequences.

Currently handles:
- octals (\\0 to \\777),
- hexadecimals (\\x00 to \\xFF),
- unicodes (\\u0000 to \\uFFFF),
- and backslash followed by anything else.")

(defconst hes-ruby-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (repeat 1 2 xdigit))
                     (and ?u
                          (or (repeat 4 xdigit)
                              (and ?{
                                   (repeat 1 6 xdigit)
                                   (0+ (1+ space)
                                       (repeat 1 6 xdigit))
                                   ?})))
                     not-newline)))))
  "Regexp to match Ruby escape sequences.

Currently handles:
- octals (\\0 to \\777),
- hexadecimals (\\x0 to \\xFF),
- unicodes (\\u0000 to \\uFFFF),
- unicodes in the \\u{} form,
- and backslash followed by anything else.

Currently doesn't handle \\C-, \\M-, etc.")

(defconst hes-ruby-escape-sequence-keywords
  `((,hes-ruby-escape-sequence-re
     (0 (let* ((state (syntax-ppss))
               (term (nth 3 state)))
          (when (or (and (eq term ?')
                         (member (match-string 2) '("\\" "'")))
                    (if (fboundp 'ruby-syntax-expansion-allowed-p)
                        (ruby-syntax-expansion-allowed-p state)
                      (memq term '(?\" ?/ ?\n ?` t))))
            ;; TODO: Switch to `add-face-text-property' when we're
            ;; fine with only supporting Emacs 24.4 and up.
            (font-lock-prepend-text-property (match-beginning 1) (match-end 1)
                                             'face 'hes-escape-backslash-face)
            (font-lock-prepend-text-property (match-beginning 2) (match-end 2)
                                             'face 'hes-escape-sequence-face)
            nil))
        prepend))))

(defconst hes-elisp-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or
                  (and ?u (repeat 4 xdigit))
                  (and ?U ?0 ?0 (repeat 6 xdigit))
                  (and ?x (+ xdigit)) ;; variable number hex digits
                  (+ (in "0-7"))      ;; variable number octal digits
                  not-newline)))))

  "Regexp to match Emacs Lisp escape sequences.

Currently handles:
- unicodes (\\uNNNN and \\U00NNNNNN)
- hexadecimal (\\x...) and octal (\\0-7), variable number of digits
- backslash followed by anything else.")

(defun hes-make-simple-escape-sequence-keywords(re)
  `((,re
     (1 (when (nth 3 (syntax-ppss))
          'hes-escape-backslash-face)
        prepend)
     (2 (when (nth 3 (syntax-ppss))
          'hes-escape-sequence-face)
        prepend))))

(defcustom hes-mode-alist
  `((c-mode          . ,hes-c/c++/objc-escape-sequence-re)
    (c++-mode        . ,hes-c/c++/objc-escape-sequence-re)
    (objc-mode       . ,hes-c/c++/objc-escape-sequence-re)
    (java-mode       . ,hes-java-escape-sequence-re)
    (clojure-mode    . ,hes-java-escape-sequence-re)
    (js-mode         . ,hes-js-escape-sequence-re)
    (js2-mode        . ,hes-js-escape-sequence-re)
    (ruby-mode       . ,hes-ruby-escape-sequence-keywords)
    (emacs-lisp-mode . ,hes-elisp-escape-sequence-re))
  "Alist of regexps or `font-lock-keywords' elements for major modes."
  :type '(repeat function)
  :set (lambda (symbol value)
         (if (bound-and-true-p hes-mode)
             (progn
               (hes-mode -1)
               (set-default symbol value)
               (hes-mode 1))
           (set-default symbol value))))

;;;###autoload
(defun turn-on-hes-mode()
  "Turn on highlighting of escape sequences."
  (interactive)
  (dolist (mode hes-mode-alist)
    (if (atom mode)
        (font-lock-add-keywords
         mode
         (hes-make-simple-escape-sequence-keywords hes-common-escape-sequence-re)
         'append)
      (when (stringp (cdr mode))
        (font-lock-add-keywords
         (car mode)
         (hes-make-simple-escape-sequence-keywords (cdr mode))
         'append))
      (when (listp (cdr mode))
        (font-lock-add-keywords (car mode) (cdr mode) 'append)))))

;;;###autoload
(defun turn-off-hes-mode()
  "Turn off highlighting of escape sequences"
  (interactive)
  (dolist (mode hes-mode-alist)
    (if (atom mode)
        (font-lock-remove-keywords
         mode
         (hes-make-simple-escape-sequence-keywords hes-common-escape-sequence-re))
      (when (stringp (cdr mode))
        (font-lock-remove-keywords
         (car mode)
         (hes-make-simple-escape-sequence-keywords (cdr mode))))
      (when (listp (cdr mode))
        (font-lock-remove-keywords (car mode) (cdr mode))))))

;;;###autoload
(define-minor-mode hes-mode
  "Toggle highlighting of escape sequences."
  :lighter "" :global t
  (if hes-mode
      (turn-on-hes-mode)
    (turn-off-hes-mode)))

(provide 'highlight-escape-sequences)

;;; highlight-escape-sequences.el ends here
