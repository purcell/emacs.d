;;; ecb-cedet-wrapper.el -- define wrappers for all semantic funcs/vars

;; Copyright (C) 2000 - 2009 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2009

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-cedet-wrapper.el,v 1.1 2009/04/21 15:23:22 berndl Exp $

;;; Commentary:

;; This file contains wrappers for every cedet-function and -variable used
;; by ECB independent which cedet version is used. So the ECB-code is
;; independent from the fact, if cedet offers backward-compatibility or
;; not. This library offers for each used variable V of cedet a getter-function
;; named "ecb--V" and for each function F an alias named "ecb--F". V and F
;; follow the naming conventiones of cedet 1.0 but the resulting functions
;; always point to the correct variable or function of cedet independent
;; which cedet version is loaded. ECB only uses the functions exported from
;; ecb-cedet-wrapper.el!


(require 'semantic)
(require 'semantic-ctxt)
(require 'semantic-analyze)
(require 'semanticdb)
(require 'semanticdb-find)
(require 'semanticdb-mode)

(defconst ecb-semantic-2-loaded (string-match "^2" semantic-version))

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; -- getter functions for all variables of cedet currently used by ECB ---

(defsubst ecb--semantic-symbol->name-assoc-list ()
  "Return the value of `semantic-symbol->name-assoc-list'."
  (symbol-value 'semantic-symbol->name-assoc-list))

(defsubst ecb--semantic-symbol->name-assoc-list-for-type-parts ()
  "Return the value of `semantic-symbol->name-assoc-list-for-type-parts'."
  (symbol-value 'semantic-symbol->name-assoc-list-for-type-parts))

(defsubst ecb--semantic-format-tag-functions ()
  "Return value of `semantic-format-tag-functions'."
  (symbol-value 'semantic-format-tag-functions))

(defsubst ecb--semantic-orphaned-member-metaparent-type ()
  "Return the value of `semantic-orphaned-member-metaparent-type'."
  (symbol-value 'semantic-orphaned-member-metaparent-type))

(defsubst ecb--semantic-uml-colon-string ()
  "Return the value of `semantic-uml-colon-string'."
  (symbol-value 'semantic-uml-colon-string))

(defsubst ecb--semantic-format-face-alist ()
  "Return the value of `semantic-format-face-alist'."
  (symbol-value 'semantic-format-face-alist))

(defsubst ecb--semantic-after-toplevel-cache-change-hook ()
  "Return the hook-symbol `semantic-after-toplevel-cache-change-hook'."
  'semantic-after-toplevel-cache-change-hook)

(defsubst ecb--semantic-after-partial-cache-change-hook ()
  "Return the hook-symbol `semantic-after-partial-cache-change-hook'."
  'semantic-after-partial-cache-change-hook)

(defsubst ecb--semantic--before-fetch-tags-hook ()
  "Return the hook-symbol `semantic--before-fetch-tags-hook'."
  'semantic--before-fetch-tags-hook)

(defsubst ecb--ezimage-use-images ()
  (if (boundp 'ezimage-use-images)
      ezimage-use-images))

(defsubst ecb--semantic-format-use-images-flag ()
  (if (boundp 'semantic-format-use-images-flag)
      semantic-format-use-images-flag))

;; -- an alias for all functions of cedet currently used by ECB ---

;; (delq nil (mapcar (function (lambda (f)
;;                               (if (not (fboundp f))
;;                                   f)))
;;                   ecb--cedet-function-list))

(defconst ecb--cedet-function-list
  '(
    semantic--format-colorize-text
    semantic--tag-get-property
    semantic--tag-overlay-cdr
    semantic--tag-put-property
    semantic--tag-set-overlay
    semantic-active-p
    semantic-adopt-external-members
    semantic-analyze-current-context
    semantic-analyze-find-tag
    semantic-analyze-possible-completions
    semantic-analyze-tag-type
    semantic-brute-find-first-tag-by-name
    semantic-bucketize
    semantic-calculate-scope
    semantic-clear-toplevel-cache
    semantic-current-tag
    semantic-current-tag-parent
    semantic-dependency-tag-file
    semantic-documentation-for-tag
    semantic-equivalent-tag-p
    semantic-fetch-available-tags
    semantic-fetch-tags
    semantic-find-tag-by-overlay
    semantic-find-tags-by-class
    semantic-find-tags-by-name
    semantic-flatten-tags-table
    semantic-get-local-arguments
    semantic-go-to-tag
    semantic-lex-token-start
    semantic-overlay-live-p
    semantic-overlay-p
    semantic-something-to-tag-table
    semantic-tag
    semantic-tag-abstract-p
    semantic-tag-buffer
    semantic-tag-calculate-parent
    semantic-tag-children-compatibility
    semantic-tag-class
    semantic-tag-components
    semantic-tag-end
    semantic-tag-faux-p
    semantic-tag-function-arguments
    semantic-tag-function-constructor-p
    semantic-tag-function-destructor-p
    semantic-tag-function-parent
    semantic-tag-get-attribute
    semantic-tag-name
    semantic-tag-new-variable
    semantic-tag-overlay
    semantic-tag-p
    semantic-tag-protection
    semantic-tag-prototype-p
    semantic-tag-start
    semantic-tag-static-p
    semantic-tag-type
    semantic-tag-type-interfaces
    semantic-tag-type-members
    semantic-tag-type-superclasses
    semantic-tag-with-position-p
    semanticdb-brute-deep-find-tags-by-name
    semanticdb-deep-find-tags-by-name
    semanticdb-find-result-length
    semanticdb-find-result-nth
    semanticdb-find-tags-by-name
    semanticdb-full-filename
    semanticdb-minor-mode-p
    semanticdb-strip-find-results
    )
)

(defconst ecb--semantic-format-function-list
  '(
    semantic-format-tag-abbreviate
    semantic-format-tag-concise-prototype
    semantic-format-tag-name
    semantic-format-tag-prin1
    semantic-format-tag-prototype
    semantic-format-tag-summarize
    semantic-format-tag-uml-abbreviate
    semantic-format-tag-uml-concise-prototype
    semantic-format-tag-uml-prototype
    ))

;; new let us create the aliase. Each alias has the name "ecb--"<function of
;; cedet >= 1.0
(dolist (f-elem  (append ecb--cedet-function-list ecb--semantic-format-function-list))
  (defalias (intern (concat "ecb--" (symbol-name f-elem))) f-elem))

(silentcomp-provide 'ecb-cedet-wrapper)

;;; ecb-cedet-wrapper.el end here
