;; haskell-c2hs.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  7 March 2016

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

;;; Commentary:
;; This mode is mostly intended for highlighting {#...#} hooks.
;;
;; Quick setup:
;; (autoload 'haskell-c2hs-mode "haskell-c2hs-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))
;;

(require 'haskell-mode)
(require 'haskell-font-lock)
(require 'haskell-utils)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(defface haskell-c2hs-hook-pair-face
  '((t (:inherit 'font-lock-preprocessor-face)))
  "Face for highlighting {#...#} pairs."
  :group 'haskell)

(defface haskell-c2hs-hook-name-face
  '((t (:inherit 'font-lock-keyword-face)))
  "Face for highlighting c2hs hook names."
  :group 'haskell)

(defvar haskell-c2hs-font-lock-keywords
  `((,(eval-when-compile
        (let* ((ws '(any ?\s ?\t ?\n ?\r))
               (anychar '(or (not (any ?#))
                             (seq "#"
                                  (not (any ?\})))))
               (any-nonquote '(or (not (any ?# ?\"))
                                  (seq "#"
                                       (not (any ?\} ?\")))))
               (cid '(seq (any (?a . ?z) (?A . ?Z) ?_)
                          (* (any (?a . ?z) (?A . ?Z) (?0 . ?9) ?_))))
               (hsid-type '(seq (? "'")
                                (any (?A . ?Z))
                                (* (any (?a . ?z) (?A . ?Z) (?0 . ?9) ?_ ?'))))
               (equals-str-val `(seq (* ,ws)
                                     "="
                                     (* ,ws)
                                     "\""
                                     (* ,any-nonquote)
                                     "\"")))
          (eval
           `(rx
             (seq
              (group-n 1 "{#")
              (* ,ws)
              (or (seq (group-n 2
                                "import"
                                (opt (+ ,ws)
                                     "qualified"))
                       (+ ,ws))
                  (seq (group-n 2
                                "context")
                       (opt (+ ,ws)
                            (group-n 3
                                     "lib")
                            ,equals-str-val)
                       (opt (+ ,ws)
                            (group-n 4
                                     "prefix")
                            ,equals-str-val)
                       (opt (+ ,ws)
                            (group-n 5
                                     "add"
                                     (+ ,ws)
                                     "prefix")
                            ,equals-str-val))
                  (seq (group-n 2
                                "type")
                       (+ ,ws)
                       ,cid)
                  (seq (group-n 2
                                "sizeof")
                       (+ ,ws)
                       ,cid)
                  (seq (group-n 2
                                "enum"
                                (+ ,ws)
                                "define")
                       (+ ,ws)
                       ,cid)
                  ;; TODO: vanilla enum fontification is incomplete
                  (seq (group-n 2
                                "enum")
                       (+ ,ws)
                       ,cid
                       (opt (+ ,ws)
                            (group-n 3
                                     "as")))
                  ;; TODO: fun hook highlighting is incompelete
                  (seq (group-n 2
                                (or "call"
                                    "fun")
                                (opt (+ ,ws)
                                     "pure")
                                (opt (+ ,ws)
                                     "unsafe"))
                       (+ ,ws)
                       ,cid
                       (opt (+ ,ws)
                            (group-n 3
                                     "as")
                            (opt (+ ,ws)
                                 (group-n 8
                                          "^"))))
                  (group-n 2
                           "get")
                  (group-n 2
                           "set")
                  (seq (group-n 2
                                "pointer")
                       (or (seq (* ,ws)
                                (group-n 3 "*")
                                (* ,ws))
                           (+ ,ws))
                       ,cid
                       (opt (+ ,ws)
                            (group-n 4 "as")
                            (+ ,ws)
                            ,hsid-type)
                       (opt (+ ,ws)
                            (group-n 5
                                     (or "foreign"
                                         "stable")))
                       (opt
                        (or (seq (+ ,ws)
                                 (group-n 6
                                          "newtype"))
                            (seq (* ,ws)
                                 "->"
                                 (* ,ws)
                                 ,hsid-type)))
                       (opt (+ ,ws)
                            (group-n 7
                                     "nocode")))
                  (group-n 2
                           "class")
                  (group-n 2
                           "alignof")
                  (group-n 2
                           "offsetof")
                  (seq (group-n 2
                                "const")
                       (+ ,ws)
                       ,cid)
                  (seq (group-n 2
                                "typedef")
                       (+ ,ws)
                       ,cid
                       (+ ,ws)
                       ,hsid-type)
                  (group-n 2
                           "nonGNU")
                  ;; TODO: default hook not implemented
                  )
              (* ,anychar)
              (group-n 9 "#}"))))))
     ;; Override highlighting for pairs in order to always distinguish them.
     (1 'haskell-c2hs-hook-pair-face t)
     (2 'haskell-c2hs-hook-name-face)
     ;; Make matches lax, i.e. do not signal error if nothing
     ;; matched.
     (3 'haskell-c2hs-hook-name-face nil t)
     (4 'haskell-c2hs-hook-name-face nil t)
     (5 'haskell-c2hs-hook-name-face nil t)
     (6 'haskell-c2hs-hook-name-face nil t)
     (7 'haskell-c2hs-hook-name-face nil t)
     (8 'font-lock-negation-char-face nil t)
     ;; Override highlighting for pairs in order to always distinguish them.
     (9 'haskell-c2hs-hook-pair-face t))
    ,@(haskell-font-lock-keywords)))

;;;###autoload
(define-derived-mode haskell-c2hs-mode haskell-mode "C2HS"
  "Mode for editing *.chs files of the c2hs haskell tool."
  (setq-local font-lock-defaults
              (cons 'haskell-c2hs-font-lock-keywords
                    (cdr font-lock-defaults))))


(provide 'haskell-c2hs)

;; haskell-c2hs.el ends here
