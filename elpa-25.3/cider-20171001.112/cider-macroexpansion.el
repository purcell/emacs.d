;;; cider-macroexpansion.el --- Macro expansion support -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2017 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Macro expansion support.

;;; Code:

(require 'cider-mode)
(require 'subr-x)
(require 'cider-compat)

(defconst cider-macroexpansion-buffer "*cider-macroexpansion*")
(add-to-list 'cider-ancillary-buffers cider-macroexpansion-buffer)

(defcustom cider-macroexpansion-display-namespaces 'tidy
  "Determines if namespaces are displayed in the macroexpansion buffer.
Possible values are:

  'qualified ;=> Vars are fully-qualified in the expansion
  'none      ;=> Vars are displayed without namespace qualification
  'tidy      ;=> Vars that are :refer-ed or defined in the current namespace are
                 displayed with their simple name, non-refered vars from other
                 namespaces are refered using the alias for that namespace (if
                 defined), other vars are displayed fully qualified."
  :type '(choice (const :tag "Suppress namespaces" none)
                 (const :tag "Show fully-qualified namespaces" qualified)
                 (const :tag "Show namespace aliases" tidy))
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-macroexpansion-print-metadata nil
  "Determines if metadata is included in macroexpansion results."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defun cider-sync-request:macroexpand (expander expr &optional display-namespaces)
  "Macroexpand, using EXPANDER, the given EXPR.
The default for DISPLAY-NAMESPACES is taken from
`cider-macroexpansion-display-namespaces'."
  (cider-ensure-op-supported "macroexpand")
  (thread-first `("op" "macroexpand"
                  "expander" ,expander
                  "code" ,expr
                  "ns" ,(cider-current-ns)
                  "display-namespaces" ,(or display-namespaces
                                            (symbol-name cider-macroexpansion-display-namespaces)))
    (nconc (when cider-macroexpansion-print-metadata
             '("print-meta" "true")))
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "expansion")))

(defun cider-macroexpand-undo (&optional arg)
  "Undo the last macroexpansion, using `undo-only'.
ARG is passed along to `undo-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defvar cider-last-macroexpand-expression nil
  "Specify the last macroexpansion preformed.
This variable specifies both what was expanded and the expander.")

(defun cider-macroexpand-expr (expander expr)
  "Macroexpand, use EXPANDER, the given EXPR."
  (when-let ((expansion (cider-sync-request:macroexpand expander expr)))
    (setq cider-last-macroexpand-expression expr)
    (cider-initialize-macroexpansion-buffer expansion (cider-current-ns))))

(defun cider-macroexpand-expr-inplace (expander)
  "Substitute the form preceding point with its macroexpansion using EXPANDER."
  (interactive)
  (let* ((expansion (cider-sync-request:macroexpand expander (cider-last-sexp)))
         (bounds (cons (save-excursion (clojure-backward-logical-sexp 1) (point)) (point))))
    (cider-redraw-macroexpansion-buffer
     expansion (current-buffer) (car bounds) (cdr bounds))))

(defun cider-macroexpand-again ()
  "Repeat the last macroexpansion."
  (interactive)
  (cider-initialize-macroexpansion-buffer cider-last-macroexpand-expression (cider-current-ns)))

;;;###autoload
(defun cider-macroexpand-1 (&optional prefix)
  "Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`."
  (interactive "P")
  (let ((expander (if prefix "macroexpand" "macroexpand-1")))
    (cider-macroexpand-expr expander (cider-last-sexp))))

(defun cider-macroexpand-1-inplace (&optional prefix)
  "Perform inplace \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`."
  (interactive "P")
  (let ((expander (if prefix "macroexpand" "macroexpand-1")))
    (cider-macroexpand-expr-inplace expander)))

;;;###autoload
(defun cider-macroexpand-all ()
  "Invoke \\=`macroexpand-all\\=` on the expression preceding point."
  (interactive)
  (cider-macroexpand-expr "macroexpand-all" (cider-last-sexp)))

(defun cider-macroexpand-all-inplace ()
  "Perform inplace \\=`macroexpand-all\\=` on the expression preceding point."
  (interactive)
  (cider-macroexpand-expr-inplace "macroexpand-all"))

(defun cider-initialize-macroexpansion-buffer (expansion ns)
  "Create a new Macroexpansion buffer with EXPANSION and namespace NS."
  (pop-to-buffer (cider-create-macroexpansion-buffer))
  (setq cider-buffer-ns ns)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (format "%s" expansion))
    (goto-char (point-max))
    (cider--font-lock-ensure)))

(defun cider-redraw-macroexpansion-buffer (expansion buffer start end)
  "Redraw the macroexpansion with new EXPANSION.
Text in BUFFER from START to END is replaced with new expansion,
and point is placed after the expanded form."
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (goto-char start)
      (delete-region start end)
      (insert (format "%s" expansion))
      (goto-char start)
      (indent-sexp)
      (forward-sexp))))

(declare-function cider-mode "cider-mode")

(defun cider-create-macroexpansion-buffer ()
  "Create a new macroexpansion buffer."
  (with-current-buffer (cider-popup-buffer cider-macroexpansion-buffer t)
    (clojure-mode)
    (cider-mode -1)
    (cider-macroexpansion-mode 1)
    (current-buffer)))

(defvar cider-macroexpansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'cider-macroexpand-again)
    (define-key map (kbd "q") #'cider-popup-buffer-quit-function)
    (define-key map (kbd "d") #'cider-doc)
    (define-key map (kbd "j") #'cider-javadoc)
    (define-key map (kbd ".") #'cider-find-var)
    (define-key map (kbd "m") #'cider-macroexpand-1-inplace)
    (define-key map (kbd "a") #'cider-macroexpand-all-inplace)
    (define-key map (kbd "u") #'cider-macroexpand-undo)
    (define-key map [remap undo] #'cider-macroexpand-undo)
    (easy-menu-define cider-macroexpansion-mode-menu map
      "Menu for CIDER's doc mode"
      '("Macroexpansion"
        ["Restart expansion" cider-macroexpand-again]
        ["Macroexpand-1" cider-macroexpand-1-inplace]
        ["Macroexpand-all" cider-macroexpand-all-inplace]
        ["Macroexpand-undo" cider-macroexpand-undo]
        ["Go to source" cider-find-var]
        ["Go to doc" cider-doc]
        ["Go to Javadoc" cider-docview-javadoc]
        ["Quit" cider-popup-buffer-quit-function]))
    map))

(define-minor-mode cider-macroexpansion-mode
  "Minor mode for CIDER macroexpansion.

\\{cider-macroexpansion-mode-map}"
  nil
  " Macroexpand"
  cider-macroexpansion-mode-map)

(provide 'cider-macroexpansion)

;;; cider-macroexpansion.el ends here
