;;; ecb-semantic-wrapper.el -- define wrappers for all semantic funcs/vars

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2003

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

;; $Id: ecb-semantic-wrapper.el,v 1.26 2006/03/10 15:40:35 berndl Exp $

;;; Commentary:

;; This file contains wrappers for every semantic-function and -variable used
;; by ECB independent which semantic version is used. So the ECB-code is
;; independent from the fact, if semantic 2.0 offers backward-compatibility or
;; not. This library offers for each variable V of semantic a getter-function
;; named "ecb--V" and for each function F an alias named "ecb--F". V and F
;; follow the naming conventiones of semantic 2.0 but the resulting functions
;; always point to the correct variable or function of semantic independent
;; which semantic version is loaded. ECB only uses the functions exported from
;; ecb-semantic-wrapper.el!


(require 'semantic)
(require 'semantic-ctxt)
(require 'semantic-analyze)

(defconst ecb-semantic-2-loaded (string-match "^2" semantic-version))
(defconst ecb-semantic-2-beta-nr (if (and ecb-semantic-2-loaded
                                          (string-match "beta\\([1-9]\\).*"
                                                        semantic-version))
                                     (string-to-number
                                      (match-string 1 semantic-version))
                                   -1))

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; semantic 1.X does not have this
(silentcomp-defvar semanticdb-search-system-databases)
(silentcomp-defvar semantic-format-use-images-flag)
(silentcomp-defvar ezimage-use-images)
;; semantic 2.0 does not have this
(silentcomp-defvar semantic-toplevel-bovine-cache)

;; -- getter functions for all variables of semantic currently used by ECB ---

(defsubst ecb--semantic-symbol->name-assoc-list ()
  "Return the value of `semantic-symbol->name-assoc-list'."
  (symbol-value 'semantic-symbol->name-assoc-list))

(defsubst ecb--semantic-symbol->name-assoc-list-for-type-parts ()
  "Return the value of `semantic-symbol->name-assoc-list-for-type-parts'."
  (symbol-value 'semantic-symbol->name-assoc-list-for-type-parts))

(defsubst ecb--semantic-format-tag-functions ()
  "Return either the value of `semantic-format-tag-functions' or
`semantic-token->text-functions' depending which semantic version is loaded."
  (if (boundp 'semantic-format-tag-functions)
      (symbol-value 'semantic-format-tag-functions)
    (symbol-value 'semantic-token->text-functions)))

(defsubst ecb--semantic-orphaned-member-metaparent-type ()
  "Return the value of `semantic-orphaned-member-metaparent-type'."
  (symbol-value 'semantic-orphaned-member-metaparent-type))

(defsubst ecb--semantic-uml-colon-string ()
  "Return the value of `semantic-uml-colon-string'."
  (symbol-value 'semantic-uml-colon-string))

(defsubst ecb--semantic-format-face-alist ()
  "Return either the value of `semantic-format-face-alist' or
`semantic-face-alist' depending which semantic version is loaded."
  (if (boundp 'semantic-format-face-alist)
      (symbol-value 'semantic-format-face-alist)
    (symbol-value 'semantic-face-alist)))

(defsubst ecb--semantic-after-toplevel-cache-change-hook ()
  "Return the hook-symbol `semantic-after-toplevel-cache-change-hook'."
  'semantic-after-toplevel-cache-change-hook)

(defsubst ecb--semantic-after-partial-cache-change-hook ()
  "Return the hook-symbol `semantic-after-partial-cache-change-hook'."
  'semantic-after-partial-cache-change-hook)

(defsubst ecb--semantic--before-fetch-tags-hook ()
  (if (boundp 'semantic--before-fetch-tags-hook)
      'semantic--before-fetch-tags-hook
    'semantic-before-toplevel-bovination-hook))

(defsubst ecb--ezimage-use-images ()
  (if (boundp 'ezimage-use-images)
      ezimage-use-images))

(defsubst ecb--semantic-format-use-images-flag ()
  (if (boundp 'semantic-format-use-images-flag)
      semantic-format-use-images-flag))

;; -- an alias for all functions of semantic currently used by ECB ---

(defconst ecb--semantic-function-alist
  '((semantic-active-p                        . semantic-active-p)
    (semantic-token-function-args             . semantic-tag-function-arguments)
    (semantic-token-type-parts                . semantic-tag-type-members)
    (semantic-something-to-stream             . semantic-something-to-tag-table)
    (semantic-find-nonterminal-by-overlay     . semantic-find-tag-by-overlay)
    ;; here both functions return a list of tags!
    (semantic-find-nonterminal-by-token       . semantic-find-tags-by-class)
    (semantic-find-nonterminal-by-name        . semantic-brute-find-first-tag-by-name)
    (semantic-current-nonterminal-parent      . semantic-current-tag-parent)
    (semantic-adopt-external-members          . semantic-adopt-external-members)
    (semantic-bucketize                       . semantic-bucketize)
    (semantic-clear-toplevel-cache            . semantic-clear-toplevel-cache)
    (semantic-colorize-text                   . semantic--format-colorize-text)
    (semantic-current-nonterminal             . semantic-current-tag)
    (semantic-equivalent-tokens-p             . semantic-equivalent-tag-p)
    (semantic-find-dependency                 . semantic-dependency-tag-file)
    (semantic-find-documentation              . semantic-documentation-for-tag)
    (semantic-flex-start                      . semantic-lex-token-start)
    (semantic-nonterminal-children            . semantic-tag-children-compatibility)
    (semantic-nonterminal-protection          . semantic-tag-protection)
    (semantic-overlay-live-p                  . semantic-overlay-live-p)
    (semantic-overlay-p                       . semantic-overlay-p)
    (semantic-token-buffer                    . semantic-tag-buffer)
    (semantic-token-end                       . semantic-tag-end)
    (semantic-token-extra-spec                . semantic-tag-get-attribute)
    (semantic-token-function-parent           . semantic-tag-function-parent)
    (semantic-token-get                       . semantic--tag-get-property)
    (semantic-token-name                      . semantic-tag-name)
    (semantic-token-overlay                   . semantic-tag-overlay)
    (semantic-token-overlay-cdr               . semantic--tag-overlay-cdr)
    (semantic-token-p                         . semantic-tag-p)
    (semantic-token-put                       . semantic--tag-put-property)
    (semantic-token-start                     . semantic-tag-start)
    (semantic-token-token                     . semantic-tag-class)
    (semantic-token-type                      . semantic-tag-type)
    (semantic-token-type-parent-superclass    . semantic-tag-type-superclass)
    (semantic-token-type-parent-implement     . semantic-tag-type-interfaces)
    (semantic-token-with-position-p           . semantic-tag-with-position-p)
    (semantic-analyze-current-context         . semantic-analyze-current-context)
    (semantic-analyze-possible-completions    . semantic-analyze-possible-completions)
    (semantic-get-local-arguments             . semantic-get-local-arguments)
    (semantic-analyze-token-type              . semantic-analyze-tag-type))
  "Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function ECB uses from the semantic library.")

(defconst ecb--semantic-format-function-alist
  '((semantic-name-nonterminal                  . semantic-format-tag-name)
    (semantic-abbreviate-nonterminal            . semantic-format-tag-abbreviate)
    (semantic-summarize-nonterminal             . semantic-format-tag-summarize)
    (semantic-prototype-nonterminal             . semantic-format-tag-prototype)
    (semantic-concise-prototype-nonterminal     . semantic-format-tag-concise-prototype)
    (semantic-uml-abbreviate-nonterminal        . semantic-format-tag-uml-abbreviate)
    (semantic-uml-prototype-nonterminal         . semantic-format-tag-uml-prototype)
    (semantic-uml-concise-prototype-nonterminal . semantic-format-tag-uml-concise-prototype)
    (semantic-prin1-nonterminal                 . semantic-format-tag-prin1))
"Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function of `semantic-token->text-functions' (rsp. for semantic 2.X
`semantic-format-tag-functions'.")

(defconst ecb--semanticdb-function-alist
  '((semanticdb-minor-mode-p             . semanticdb-minor-mode-p)
    (semanticdb-full-filename            . semanticdb-full-filename))
  "Alist where the car is a function of semanticdb 1.X and the cdr is the
equivalent new function of semanticdb 2.X. This alist should contain every
function ECB uses from the semanticdb library.")
  
;; new let us create the aliase. Each alias has the name "ecb--"<function of
;; semantic 2.0>.
(dolist (f-elem (append ecb--semantic-function-alist
                        ecb--semantic-format-function-alist
                        ecb--semanticdb-function-alist))
  (defalias (intern (concat "ecb--" (symbol-name (cdr f-elem))))
    (if (fboundp (cdr f-elem))
        (cdr f-elem)
      (car f-elem))))


(defsubst ecb--semantic-tag (name class &rest ignore)
  "Create a new semantic tag with name NAME and tag-class CLASS."
  (if (fboundp 'semantic-tag)
      (apply 'semantic-tag name class ignore)
    (list name class nil nil nil nil)))

(defsubst ecb--semantic-tag-new-variable (name type default-value &rest attributes)
  "Create a semantic tag of class variable"
  (if (fboundp 'semantic-tag-new-variable)
      (apply 'semantic-tag-new-variable name type default-value attributes)
    (list name 'variable nil nil nil nil)))

(defsubst ecb--semantic--tag-set-overlay (tag overlay)
  "Set the overlay part of TAG with OVERLAY. OVERLAY can be an overlay or an
unloaded buffer representation."
  (let ((o-cdr (ecb--semantic--tag-overlay-cdr tag)))
    (setcar o-cdr overlay)))

(defsubst ecb--semantic-tag-calculate-parent (tag)
  "Attempt to calculate the parent-tag of TAG."
  (if (fboundp 'semantic-tag-calculate-parent)
      (apply 'semantic-tag-calculate-parent (list tag))
    (save-excursion
      (set-buffer (ecb--semantic-tag-buffer tag))
      (goto-char (ecb--semantic-tag-start tag))
      (ecb--semantic-current-tag-parent))))

(cond ((fboundp 'semantic-tag-static-p)
       (defalias 'ecb--semantic-tag-static-p 'semantic-tag-static-p))
      ((fboundp 'semantic-tag-static)
       (defalias 'ecb--semantic-tag-static-p 'semantic-tag-static))
      ((fboundp 'semantic-nonterminal-static)
       (defalias 'ecb--semantic-tag-static-p 'semantic-nonterminal-static))
      (t
       (defsubst ecb--semantic-tag-static-p (tag &optional parent)
         nil)))

(cond ((fboundp 'semantic-tag-abstract-p)
       (defalias 'ecb--semantic-tag-abstract-p 'semantic-tag-abstract-p))
      ((fboundp 'semantic-tag-abstract)
       (defalias 'ecb--semantic-tag-abstract-p 'semantic-tag-abstract))
      ((fboundp 'semantic-nonterminal-abstract)
       (defalias 'ecb--semantic-tag-abstract-p 'semantic-nonterminal-abstract))
      (t
       (defsubst ecb--semantic-tag-abstract-p (tag &optional parent)
         nil)))

(defsubst ecb--semantic-tag-prototype-p (tag)
  (ecb--semantic-tag-get-attribute tag (if (> ecb-semantic-2-beta-nr 1)
                                           :prototype-flag
                                         'prototype)))

(if (fboundp 'semantic-tag-function-constructor-p)
    (defalias 'ecb--semantic-tag-function-constructor-p
      'semantic-tag-function-constructor-p)
  (defsubst ecb--semantic-tag-function-constructor-p (tag)
    (ecb--semantic-tag-get-attribute tag (if (> ecb-semantic-2-beta-nr 1)
                                             :constructor-flag
                                           'constructor))))
    
(if (fboundp 'semantic-tag-function-destructor-p)
    (defalias 'ecb--semantic-tag-function-destructor-p
      'semantic-tag-function-destructor-p)
  (defsubst ecb--semantic-tag-function-destructor-p (tag)
    (ecb--semantic-tag-get-attribute tag (if (> ecb-semantic-2-beta-nr 1)
                                             :destructor-flag
                                           'destructor))))
    
    
(defsubst ecb--semantic-fetch-tags (&optional check-cache)
  (if (fboundp 'semantic-fetch-tags)
      (apply 'semantic-fetch-tags nil)
    (apply 'semantic-bovinate-toplevel (list check-cache))))

(if (fboundp 'semantic-fetch-available-tags)
    (defalias 'ecb--semantic-fetch-available-tags 'semantic-fetch-available-tags)
  (defsubst ecb--semantic-fetch-available-tags ()
    semantic-toplevel-bovine-cache))

(if (fboundp 'semantic-tag-components)
    (defalias 'ecb--semantic-tag-components
      'semantic-tag-components)
  (defun ecb--semantic-tag-components (tag)
    (case (ecb--semantic-tag-class tag)
      (type (ecb--semantic-tag-type-members tag))
      (function (ecb--semantic-tag-function-arguments tag))
      (otherwise nil))))

(if (fboundp 'semantic-flatten-tags-table)
    (defalias 'ecb--semantic-flatten-tags-table
      'semantic-flatten-tags-table)
  (defun ecb--semantic-flatten-tags-table (&optional table)
    "Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags."
    (let* ((table (ecb--semantic-something-to-tag-table table))
           ;; Initialize the starting list with our table.
           (lists (list table)))
      (mapc (lambda (tag)
              (let ((components (ecb--semantic-tag-components tag)))
                (if (and components
                         ;; unpositined tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here? - EL
                         (ecb--semantic-tag-with-position-p (car components)))
                    (setq lists (cons
                                 (ecb--semantic-flatten-tags-table components)
                                 lists)))))
            table)
      (apply 'append (nreverse lists))
      )))

;; Klaus Berndl <klaus.berndl@sdm.de>: Here we must make a list of tags by
;; hand for semantic 1.4!!
(if (fboundp 'semantic-find-tags-by-name)
    (defalias 'ecb--semantic-find-tags-by-name
      'semantic-find-tags-by-name)
  (defsubst ecb--semantic-find-tags-by-name (name &optional table)
    (list (ecb--semantic-brute-find-first-tag-by-name name table))))

;;; semanticdb-API Functions
;;
;; Once you have a search result, use these routines to operate
;; on the search results at a higher level

(if (fboundp 'semanticdb-find-tags-by-name)
    (defalias 'ecb--semanticdb-find-tags-by-name
      'semanticdb-find-tags-by-name)
  (defun ecb--semanticdb-find-tags-by-name (name &optional path find-file-match)
    "Runs `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is nil."
    (apply 'semanticdb-find-nonterminal-by-name
           (list name path nil nil nil find-file-match))))

(if (fboundp 'semanticdb-deep-find-tags-by-name)
    (defalias 'ecb--semanticdb-deep-find-tags-by-name
      'semanticdb-deep-find-tags-by-name)
  (defun ecb--semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
    "Runs `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is t."
    (apply 'semanticdb-find-nonterminal-by-name
           (list name path t nil nil find-file-match))))

(if (fboundp 'semanticdb-brute-deep-find-tags-by-name)
    (defalias 'ecb--semanticdb-brute-deep-find-tags-by-name
      'semanticdb-brute-deep-find-tags-by-name)
  (defun ecb--semanticdb-brute-deep-find-tags-by-name (name &optional
                                                            path find-file-match)
    "In semantic 1.4 all searches are brutish, so it runs just
    `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is t."
    (ecb--semanticdb-deep-find-tags-by-name name path find-file-match)))


(if (fboundp 'semanticdb-strip-find-results)
    (defalias 'ecb--semanticdb-strip-find-results
      'semanticdb-strip-find-results)
  (defun ecb--semanticdb-strip-find-results (results)
    "Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call."
    (apply #'append (mapcar #'cdr results))))

(if (fboundp 'semanticdb-find-result-length)
    (defalias 'ecb--semanticdb-find-result-length
      'semanticdb-find-result-length)
  (defun ecb--semanticdb-find-result-length (result)
    "Number of tags found in RESULT."
    (let ((count 0))
      (mapc (lambda (onetable)
              (setq count (+ count (1- (length onetable)))))
            result)
      count)))

(defun ecb--semanticdb-find-result-nth (result n)
  "In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current."
  (if (fboundp 'semanticdb-find-result-nth)
      (apply 'semanticdb-find-result-nth (list result n))
    (let ((ans nil)
          (anstable nil))
      ;; Loop over each single table hit.
      (while (and (not ans) result)
        ;; For each table result, get local length, and modify
        ;; N to be that much less.
        (let ((ll (length (cdr (car result))))) ;; local length
          (if (> ll n)
              ;; We have a local match.
              (setq ans (nth n (cdr (car result)))
                    anstable (car (car result)))
            ;; More to go.  Decrement N.
            (setq n (- n ll))))
        ;; Keep moving.
        (setq result (cdr result)))
      (cons ans anstable))))

(defun ecb--semanticdb-find-result-nth-with-file (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0. Returns
a cons cell with car is the searched and found tag and the cdr is the
associated full filename of this tag. If the search result is not associated
with a file, then the cdr of the result-cons is nil."
  (let ((result-nth (ecb--semanticdb-find-result-nth result n)))
    (if (and (car result-nth)
             (ecb--semantic-tag-with-position-p (car result-nth))
             (cdr result-nth))
        (cons (car result-nth)
              (ecb--semanticdb-full-filename (cdr result-nth)))
      (cons (car result-nth) nil))))

(silentcomp-provide 'ecb-semantic-wrapper)

;;; ecb-semantic-wrapper.el end here
