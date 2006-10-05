;;; mmm-mason.el --- MMM submode class for Mason components

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: $Id: mmm-mason.el,v 1.13 2003/03/09 17:04:03 viritrilbia Exp $

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

;; This file contains the definition of an MMM Mode submode class for
;; editing Mason components.  See the file README.Mason for more
;; details.

;;; Code:

(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-auto)

;;{{{ Perl Tags

(defvar mmm-mason-perl-tags
  '("perl" "init" "cleanup" "once" "filter" "shared"
    "perl_init" "perl_cleanup" "perl_once" "perl_filter"))

(defvar mmm-mason-pseudo-perl-tags
  '("args" "perl_args" "attr" "flags"))

(defvar mmm-mason-non-perl-tags
  '("doc" "perl_doc" "text" "perl_text" "def" "perl_def" "method"))

(defvar mmm-mason-perl-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-mason-perl-tags t) ">")
  "Matches tags beginning Mason sections containing Perl code.
Saves the name of the tag matched.")

(defvar mmm-mason-pseudo-perl-tags-regexp
  (concat "<%" (mmm-regexp-opt mmm-mason-pseudo-perl-tags t) ">")
  "Match tags beginning Mason sections that look like Perl but aren't.
Saves the name of the tag matched.")

(defvar mmm-mason-tag-names-regexp
  (regexp-opt (append mmm-mason-perl-tags mmm-mason-non-perl-tags) t)
  "Matches any Mason tag name after the \"<%\". Used to verify that a
\"<%\" sequence starts an inline section.")

(defun mmm-mason-verify-inline ()
  (not (looking-at mmm-mason-tag-names-regexp)))

;;}}}
;;{{{ Add Classes

(mmm-add-group
 'mason
 `((mason-text
    :submode nil
    :front "<%text>"
    :back "</%text>"
    :insert ((?t mason-<%text> nil @ "<%text>" @ "\n"
                 _ "\n" @ "</%text>" @)))
   (mason-doc
    :submode text-mode
    :face mmm-comment-submode-face
    :front "<%doc>"
    :back "</%doc>"
    :face nil
    :insert ((?d mason-<%doc> nil @ "<%doc>" @ "\n"
                 _ "\n" @ "</%doc>" @)))
   (mason-perl
    :submode perl
    :match-face (("<%perl>" . mmm-code-submode-face)
                 ("<%init>" . mmm-init-submode-face)
                 ("<%cleanup>" . mmm-cleanup-submode-face)
                 ("<%once>" . mmm-init-submode-face)
                 ("<%filter>" . mmm-special-submode-face)
                 ("<%shared>" . mmm-init-submode-face))
    :front ,mmm-mason-perl-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :match-name "~1"
    :save-name 1
    :insert ((?, mason-<%TAG> "Perl section: " @ "<%" str ">" @
                 ";\n" _ "\n" @ "</%" str ">" @)
             (?< mason-<%TAG> ?, . nil)
             (?p mason-<%perl> ?, . "perl")
             (?i mason-<%init> ?, . "init")
             (?c mason-<%cleanup> ?, . "cleanup")
             (?o mason-<%once> ?, . "once")
             (?l mason-<%filter> ?, . "filter")
             (?s mason-<%shared> ?, . "shared")))
   (mason-pseudo-perl
    :submode perl
    :face mmm-declaration-submode-face
    :front ,mmm-mason-pseudo-perl-tags-regexp
    :back "</%~1>"
    :save-matches 1
    :insert ((?. mason-pseudo-<%TAG> "Pseudo-perl section: " @ "<%" str ">" @
                 "\n" _ "\n" @ "</%" str ">" @)
             (?> mason-pseudo-<%TAG> ?, . nil)
             (?a mason-<%args> ?. . "args")
             (?f mason-<%flags> ?. . "flags")
             (?r mason-<%attr> ?. . "attr")))
   (mason-inline
    :submode perl
    :face mmm-output-submode-face
    :front "<%"
    :front-verify mmm-mason-verify-inline
    :back "%>"
    :insert ((?% mason-<%-%> nil @ "<%" @ " " _ " " @ "%>" @)
             (?5 mason-<%-%> ?% . nil)))
   (mason-call
    :submode perl
    :face mmm-special-submode-face
    :front "<&"
    :back "&>"
    :insert ((?& mason-<&-&> nil @ "<&" @ " " _ " " @ "&>" @)
             (?7 mason-<&-&> ?% . nil)))
   (mason-one-line-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "^%#"
    :back "\n"
    :insert ((?# mason-%-comment nil (mmm-mason-start-line)
                @ "%" @ "# " _ @ '(mmm-mason-end-line) "\n" @)
             (?3 mason-%-comment ?# . nil)))
   (mason-one-line
    :submode perl
    :face mmm-code-submode-face
    :front "^%"
    :back "\n"
    :insert ((return mason-%-line nil (mmm-mason-start-line)
                     @ "%" @ " " _ @ '(mmm-mason-end-line) "\n" @)))))

;;}}}
;;{{{ One-line Sections

(defun mmm-mason-start-line ()
  (if (bolp)
      ""
    "\n"))

(defun mmm-mason-end-line ()
  (if (eolp)
      (delete-char 1)))

;;}}}
;;{{{ Set Mode Line

(defun mmm-mason-set-mode-line ()
  (setq mmm-buffer-mode-display-name "Mason"))
(add-hook 'mmm-mason-class-hook 'mmm-mason-set-mode-line)

;;}}}

(provide 'mmm-mason)

;;; mmm-mason.el ends here