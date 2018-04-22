;;; shell-split-string.el --- Split strings using shell-like syntax

;; Author: 10sr <8.slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/shell-split-string-el
;; Package-Version: 20151224.208
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility library shell string

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; shell-split-string.el provides one function `shell-split-string'.
;; This function splits strings using shell-like syntax:
;; (shell-split-string "abc")  ->  '("abc")
;; (shell-split-string "abc  def")  ->  '("abc" "def")
;; (shell-split-string "abc \"def ghi\"")  ->  '("abc" "def ghi")
;; (shell-split-string "abc 'de f'ghi") -> '("abc" "de fghi")
;; (shell-split-string "abc \"d\\\"ef\"") -> '("abc" "d\"ef")


;;; Code:

(defvar shell-split-string-separators
  '(?  ?\f ?\t ?\n ?\r ?\v)
  "List of chars of separators.")

;;;###autoload
(defun shell-split-string (str)
  "Split string STR using shell-like syntax and return resulting list."
  (shell-split-string--1 (mapcar 'identity str) nil nil nil))

(defun shell-split-string--1 (rest current quote done)
  "Split string using shell-like syntax and return resulting list.

REST should be a list of chars that is not read yet.

CURRENT is a list of chars of word currently processing.

QUOTE is a char that quotes CURRENT word. Should be ?\", ?' or nil.
The value nil means CURRENT word is not quoted.

DONE is a list of strings that was already processed."
  (if (not rest)
      ;; If no chars to read left, return resulting list
      (cond
       (quote
        (error "Unmatched quotation"))
       (current
        `(,@done ,(apply 'string current)))
       ('otherwise
        done)
      )
    (let ((first (car rest))
          (rest (cdr rest)))
      (cond
       ((memq first shell-split-string-separators)
        (if quote
            ;; if cnrrently in inside of quotation, just append to current word
            (shell-split-string--1 rest
                                   `(,@current ,first)
                                   quote
                                   done)
          ;; if outside of quotation, reading char is end of current word
          (if current
              (shell-split-string--1 rest
                                     nil
                                     nil
                                     `(,@done ,(apply 'string current)))
            ;; if current word is nil, that means multiple separators appear
            ;; successively
            (shell-split-string--1 rest
                                   nil
                                   nil
                                   done))))

       ((eq first ?\\)
        ;; backslash
        ;; if inside of quote and next char is the quote letter, append it
        ;; if outside of quote, read next char literally
        ;; if inside of quote, just append backslash to current word
        (cond
         (quote
          (if (eq quote (car rest))
              (let ((first (car rest))
                    (rest (cdr rest)))
                (shell-split-string--1 rest
                                       `(,@current ,first)
                                       quote
                                       done))
            (shell-split-string--1 rest
                                   `(,@current ,first)
                                   quote
                                   done)))
         ('otherwise
          (let ((first (car rest))
                (rest (cdr rest)))
            (shell-split-string--1 rest
                                   `(,@current ,first)
                                   quote
                                   done)))))

       ((memq first '(?\" ?'))
        (cond
         ((eq first quote)
          ;; end of quotation
          (shell-split-string--1 rest
                                 current
                                 nil
                                 done))
         (quote
          ;; quote does not match, but still quoted with another letter
          ;; for example quoted with \", but read '
          (shell-split-string--1 rest
                                 `(,@current ,first)
                                 quote
                                 done))

         ('otherwise
          ;; if not quoted yet, start quoted word
          (shell-split-string--1 rest
                                 current
                                 first
                                 done))
         ))

       ('otherwise
        (shell-split-string--1 rest
                               `(,@current ,first)
                               quote
                               done))
       ))))

(provide 'shell-split-string)

;;; shell-split-string.el ends here
