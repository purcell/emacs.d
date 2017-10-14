;;; mmm-myghty.el --- MMM submode class for Myghty components
;;;     

;; Copyright (C) 2000 by Michael Abraham Shulman
;; Copyright (C) 2004 by Ben Bangert

;; Original Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>

;; Based on mmm-mason.el, trivial changes by Ben Bangert

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;;; I went to the hard (sarcasm) effort of applying two global 
;;; search/replaces, and adding a few keywords for additional
;;; blocks that Myghty introduced. Many thanks to Michael for writing
;;; the mmm-mason without which I would never have found the time
;;; to patch up for Myghty.

;;; Code:

(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-auto)

;;{{{ Python Tags

(defvar mmm-myghty-python-tags
  '("python" "init" "cleanup" "once" "filter" "shared" "global"
    "threadlocal" "requestlocal"
    "python_init" "python_cleanup" "python_once" "python_filter"))

(defvar mmm-myghty-pseudo-python-tags
  '("args" "python_args" "attr" "flags"))

(defvar mmm-myghty-non-python-tags
  '("doc" "python_doc" "text" "python_text" "def" "python_def" "method"))

(defvar mmm-myghty-python-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-myghty-python-tags t) ">")
  "Matches tags beginning Myghty sections containing Python code.
Saves the name of the tag matched.")

(defvar mmm-myghty-pseudo-python-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-myghty-pseudo-python-tags t) ">")
  "Match tags beginning Myghty sections that look like Python but aren't.
Saves the name of the tag matched.")

(defvar mmm-myghty-tag-names-regexp
  (regexp-opt (append mmm-myghty-python-tags mmm-myghty-non-python-tags) t)
  "Matches any Myghty tag name after the \"<%\". Used to verify that a
\"<%\" sequence starts an inline section.")

(defun mmm-myghty-verify-inline ()
  (not (looking-at mmm-myghty-tag-names-regexp)))

;;}}}
;;{{{ Add Classes

(mmm-add-group
 'myghty
 `((myghty-text
    :submode nil
    :front "<%text>"
    :back "</%text>"
    :insert ((?t myghty-<%text> nil @ "<%text>" @ "\n"
                 _ "\n" @ "</%text>" @)))
   (myghty-doc
    :submode text-mode
    :face mmm-comment-submode-face
    :front "<%doc>"
    :back "</%doc>"
    :face nil
    :insert ((?d myghty-<%doc> nil @ "<%doc>" @ "\n"
                 _ "\n" @ "</%doc>" @)))
   (myghty-python
    :submode python
    :match-face (("<%python>" . mmm-code-submode-face)
                 ("<%init>" . mmm-init-submode-face)
                 ("<%cleanup>" . mmm-cleanup-submode-face)
                 ("<%once>" . mmm-init-submode-face)
                 ("<%global>" . mmm-init-submode-face)
                 ("<%filter>" . mmm-special-submode-face)
                 ("<%shared>" . mmm-init-submode-face)
                 ("<%threadlocal>" . mmm-init-submode-face)
                 ("<%requestlocal>" . mmm-init-submode-face))
    :front ,mmm-myghty-python-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :match-name "~1"
    :save-name 1
    :insert ((?, myghty-<%TAG> "Python section: " @ "<%" str ">" @
                 ";\n" _ "\n" @ "</%" str ">" @)
             (?< myghty-<%TAG> ?, . nil)
             (?p myghty-<%python> ?, . "python")
             (?i myghty-<%init> ?, . "init")
             (?c myghty-<%cleanup> ?, . "cleanup")
             (?o myghty-<%once> ?, . "once")
             (?g myghty-<%global> ?, . "global")
             (?t myghty-<%threadlocal> ?, . "threadlocal")
             (?e myghty-<%requestlocal> ?, . "requestlocal")
             (?l myghty-<%filter> ?, . "filter")
             (?s myghty-<%shared> ?, . "shared")))
   (myghty-pseudo-python
    :submode python
    :face mmm-declaration-submode-face
    :front ,mmm-myghty-pseudo-python-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :insert ((?. myghty-pseudo-<%TAG> "Pseudo-python section: " @ "<%" str ">" @
                 "\n" _ "\n" @ "</%" str ">" @)
             (?> myghty-pseudo-<%TAG> ?, . nil)
             (?a myghty-<%args> ?. . "args")
             (?f myghty-<%flags> ?. . "flags")
             (?r myghty-<%attr> ?. . "attr")))
   (myghty-inline
    :submode python
    :face mmm-output-submode-face
    :front "<%"
    :front-verify mmm-myghty-verify-inline
    :back "%>"
    :insert ((?% myghty-<%-%> nil @ "<%" @ " " _ " " @ "%>" @)
             (?5 myghty-<%-%> ?% . nil)))
   (myghty-call
    :submode python
    :face mmm-special-submode-face
    :front "<&"
    :back "&>"
    :insert ((?& myghty-<&-&> nil @ "<&" @ " " _ " " @ "&>" @)
             (?7 myghty-<&-&> ?% . nil)))
   (myghty-one-line-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "^%#"
    :back "\n"
    :insert ((?# myghty-%-comment nil (mmm-myghty-start-line)
                @ "%" @ "# " _ @ '(mmm-myghty-end-line) "\n" @)
             (?3 myghty-%-comment ?# . nil)))
   (myghty-one-line
    :submode python
    :face mmm-code-submode-face
    :front "^%"
    :back "\n"
    :insert ((return myghty-%-line nil (mmm-myghty-start-line)
                     @ "%" @ " " _ @ '(mmm-myghty-end-line) "\n" @)))))

;;}}}
;;{{{ One-line Sections

(defun mmm-myghty-start-line ()
  (if (bolp)
      ""
    "\n"))

(defun mmm-myghty-end-line ()
  (if (eolp)
      (delete-char 1)))

;;}}}
;;{{{ Set Mode Line

(defun mmm-myghty-set-mode-line ()
  (setq mmm-buffer-mode-display-name "Myghty"))
(add-hook 'mmm-myghty-class-hook 'mmm-myghty-set-mode-line)

;;}}}

(provide 'mmm-myghty)

;;; mmm-myghty.el ends here
