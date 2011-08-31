;;; wisent-debug.el --- GNU Bison for Emacs - Debugging

;; Copyright (C) 2003, 2007 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 11 February 2003
;; Keywords: syntax
;; X-RCS: $Id: wisent-debug.el,v 1.4 2007/02/19 13:38:42 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Investigating what is happening during execution of the Wisent parser.
;;
;; Wisent (the European Bison ;-) is an Elisp implementation of the
;; GNU Compiler Compiler Bison.
;;
;; For more details on the basic concepts for understanding Wisent,
;; read the Bison manual ;)
;;
;; For more details on Wisent itself read the Wisent manual.

;;; History:
;;

;;; Code:
(require 'wisent)
(require 'debug)

(defsubst wisent-debug-on-entry-p (function)
  "Return t if FUNCTION invoke debugger each time it is called.
FUNCTION must be a semantic action symbol."
  (if (memq function debug-function-list)
      t))

(defun wisent-debug-on-automaton-p (automaton)
  "Return t if there is a debug-enabled function in AUTOMATON."
  (catch 'found
    (mapatoms
     #'(lambda (function)
         (if (wisent-debug-on-entry-p function)
             (throw 'found t)))
     (aref automaton 3))))

(defsubst wisent-debug-semantic-action-source (function)
  "Return source expression of semantic action FUNCTION.
FUNCTION must be a semantic action symbol."
  (symbol-function function))

(defun wisent-debug-read-entry (flag)
  "Read a semantic action symbol from the minibuffer.
Return a list (AUTOMATON FUNCTION) suitable for an `interactive' spec.
AUTOMATON is a LALR automaton variable.  FUNCTION is a semantic action
symbol defined in AUTOMATON.  If FLAG is t, only consider
debug-enabled actions.  If nil, only consider not debug-enabled
actions.  If 'any consider all available semantic action symbols."
  (let ((vn (completing-read
             "LALR automaton name: " obarray
             (if (eq flag t)
                 #'(lambda (e)
                     (and (wisent-automaton-p e)
                          (wisent-debug-on-automaton-p
                           (symbol-value e))))
               'wisent-automaton-p)
             t))
        ob am sy)
    (unless (string-equal "" vn)
      (setq am (intern-soft vn)
            ob (aref (symbol-value am) 3)
            vn (completing-read
                (format "Semantic action symbol in `%s': " am)
                ob
                (unless (eq flag 'any)
                  #'(lambda (e)
                      (eq (wisent-debug-on-entry-p e) flag)))
                t))
      (unless (string-equal "" vn)
        (setq sy (intern-soft vn ob))))
    (list am sy)))

(defun wisent-debug-check-entry (automaton function)
  "Check that AUTOMATON owns symbol FUNCTION.
Always return a symbol FUNCTION interned in the semantic action symbol
table of AUTOMATON."
  (and (wisent-automaton-p automaton)
       function (symbolp function)
       (intern-soft (symbol-name function)
                    (aref (symbol-value automaton) 3))))

;;;###autoload
(defun wisent-debug-on-entry (automaton function)
  "Request AUTOMATON's FUNCTION to invoke debugger each time it is called.
FUNCTION must be a semantic action symbol that exists in AUTOMATON."
  (interactive (wisent-debug-read-entry nil))
  (when (setq function (wisent-debug-check-entry automaton function))
    (debug-on-entry function)))

;;;###autoload
(defun wisent-cancel-debug-on-entry (automaton function)
  "Undo effect of \\[wisent-debug-on-entry] on AUTOMATON's FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON."
  (interactive (wisent-debug-read-entry t))
  (when (setq function (wisent-debug-check-entry automaton function))
    (cancel-debug-on-entry function)))

(condition-case nil
    (require 'pprint)
  (error
   (require 'pp)))

(if (fboundp 'pprint-to-string)
    (eval-and-compile
      (defalias 'wisent-debug-pp-to-string 'pprint-to-string))
  (eval-and-compile
    (defalias 'wisent-debug-pp-to-string 'pp-to-string)))

;;;###autoload
(defun wisent-debug-show-entry (automaton function)
  "Show the source of AUTOMATON's semantic action FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON."
  (interactive (wisent-debug-read-entry 'any))
  (when (setq function (wisent-debug-check-entry automaton function))
    (with-current-buffer
        (get-buffer-create (format "*%s/%s*" automaton function))
      (erase-buffer)
      (kill-all-local-variables)
      (erase-buffer)
      (setq buffer-undo-list t
            buffer-read-only nil)
      (emacs-lisp-mode)
      (insert
       (wisent-debug-pp-to-string
        (wisent-debug-semantic-action-source function)))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'wisent-debug)

;;; wisent-debug.el ends here
