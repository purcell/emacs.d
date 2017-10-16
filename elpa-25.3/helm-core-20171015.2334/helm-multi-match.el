;;; helm-multi-match.el --- Multiple regexp matching methods for helm -*- lexical-binding: t -*-

;; Original Author: rubikitch

;; Copyright (C) 2008 ~ 2011 rubikitch
;; Copyright (C) 2011 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; URL: http://github.com/emacs-helm/helm

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

;;; Code:

(require 'cl-lib)
(require 'helm-lib)


(defgroup helm-multi-match nil
  "Helm multi match."
  :group 'helm)

(defcustom helm-mm-matching-method 'multi3
  "Matching method for helm match plugin.
You can set here different methods to match candidates in helm.
Here are the possible value of this symbol and their meaning:
- multi1: Respect order, prefix of pattern must match.
- multi2: Same but with partial match.
- multi3: The best, multiple regexp match, allow negation.
- multi3p: Same but prefix must match.

Default is multi3, you should keep this for a better experience.

Note that multi1 and multi3p are incompatible with fuzzy matching
in file completion and by the way fuzzy matching will be disabled there
when these options are used."
  :type  '(radio :tag "Matching methods for helm"
           (const :tag "Multiple regexp 1 ordered with prefix match"         multi1)
           (const :tag "Multiple regexp 2 ordered with partial match"        multi2)
           (const :tag "Multiple regexp 3 matching no order, partial, best." multi3)
           (const :tag "Multiple regexp 3p matching with prefix match"       multi3p))
  :group 'helm-multi-match)


;; Internal
(defvar helm-mm-default-match-functions
  '(helm-mm-exact-match helm-mm-match))
(defvar helm-mm-default-search-functions
  '(helm-mm-exact-search helm-mm-search))


;;; Build regexps
;;
;;
(defconst helm-mm-space-regexp "\\s\\\\s-"
  "Regexp to represent space itself in multiple regexp match.")

(defun helm-mm-split-pattern (pattern)
  "Split PATTERN if it contain spaces and return resulting list.
If spaces in PATTERN are escaped, don't split at this place.
i.e \"foo bar baz\"=> (\"foo\" \"bar\" \"baz\")
but \"foo\\ bar baz\"=> (\"foo\\s-bar\" \"baz\")."
  (split-string
   ;; Match spaces litteraly because candidate buffer syntax-table
   ;; doesn't understand "\s-" properly.
   (replace-regexp-in-string helm-mm-space-regexp "\\s-" pattern nil t)))

(defun helm-mm-1-make-regexp (pattern)
  "Replace spaces in PATTERN with \"\.*\"."
  (mapconcat 'identity (helm-mm-split-pattern pattern) ".*"))


;;; Exact match.
;;
;;
;; Internal.
(defvar helm-mm-exact-pattern-str nil)
(defvar helm-mm-exact-pattern-real nil)

(defun helm-mm-exact-get-pattern (pattern)
  (unless (equal pattern helm-mm-exact-pattern-str)
    (setq helm-mm-exact-pattern-str pattern
          helm-mm-exact-pattern-real (concat "\n" pattern "\n")))
  helm-mm-exact-pattern-real)


(cl-defun helm-mm-exact-match (str &optional (pattern helm-pattern))
  (if case-fold-search
      (progn
        (setq str (downcase str)
              pattern (downcase pattern))
        (string= str pattern))
      (string= str pattern)))

(defun helm-mm-exact-search (pattern &rest _ignore)
  (and (search-forward (helm-mm-exact-get-pattern pattern) nil t)
       (forward-line -1)))


;;; Prefix match
;;
;;
;; Internal
(defvar helm-mm-prefix-pattern-str nil)
(defvar helm-mm-prefix-pattern-real nil)

(defun helm-mm-prefix-get-pattern (pattern)
  (unless (equal pattern helm-mm-prefix-pattern-str)
    (setq helm-mm-prefix-pattern-str pattern
          helm-mm-prefix-pattern-real (concat "\n" pattern)))
  helm-mm-prefix-pattern-real)

(defun helm-mm-prefix-match (str &optional pattern)
  ;; In filename completion basename and basedir may be
  ;; quoted, unquote them for string comparison (Issue #1283).
  (setq pattern (replace-regexp-in-string
                 "\\\\" "" (or pattern helm-pattern)))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))

(defun helm-mm-prefix-search (pattern &rest _ignore)
  (search-forward (helm-mm-prefix-get-pattern pattern) nil t))


;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
;; Internal
(defvar helm-mm-1-pattern-str nil)
(defvar helm-mm-1-pattern-real nil)

(defun helm-mm-1-get-pattern (pattern)
  (unless (equal pattern helm-mm-1-pattern-str)
    (setq helm-mm-1-pattern-str pattern
          helm-mm-1-pattern-real
          (concat "^" (helm-mm-1-make-regexp pattern))))
  helm-mm-1-pattern-real)

(cl-defun helm-mm-1-match (str &optional (pattern helm-pattern))
  (string-match (helm-mm-1-get-pattern pattern) str))

(defun helm-mm-1-search (pattern &rest _ignore)
  (re-search-forward (helm-mm-1-get-pattern pattern) nil t))


;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
;; Internal
(defvar helm-mm-2-pattern-str nil)
(defvar helm-mm-2-pattern-real nil)

(defun helm-mm-2-get-pattern (pattern)
  (unless (equal pattern helm-mm-2-pattern-str)
    (setq helm-mm-2-pattern-str pattern
          helm-mm-2-pattern-real
          (concat "^.*" (helm-mm-1-make-regexp pattern))))
  helm-mm-2-pattern-real)

(cl-defun helm-mm-2-match (str &optional (pattern helm-pattern))
  (string-match (helm-mm-2-get-pattern pattern) str))

(defun helm-mm-2-search (pattern &rest _ignore)
  (re-search-forward (helm-mm-2-get-pattern pattern) nil t))


;;; Multiple regexp patterns 3 (permutation).
;;
;;
;; Internal
(defvar helm-mm-3-pattern-str nil)
(defvar helm-mm-3-pattern-list nil)

(defun helm-mm-3-get-patterns (pattern)
  "Return `helm-mm-3-pattern-list', a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\")).
This is done only if `helm-mm-3-pattern-str' is same as PATTERN."
  (unless (equal pattern helm-mm-3-pattern-str)
    (setq helm-mm-3-pattern-str pattern
          helm-mm-3-pattern-list
          (helm-mm-3-get-patterns-internal pattern)))
  helm-mm-3-pattern-list)

(defun helm-mm-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\"))."
  (unless (string= pattern "")
    (cl-loop for pat in (helm-mm-split-pattern pattern)
          collect (if (string= "!" (substring pat 0 1))
                      (cons 'not (substring pat 1))
                    (cons 'identity pat)))))

(cl-defun helm-mm-3-match (str &optional (pattern helm-pattern))
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mm-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mm-3-get-patterns pattern)))
    (cl-loop for (predicate . regexp) in pat
             always (funcall predicate
                             (condition-case _err
                                 ;; FIXME: Probably do nothing when
                                 ;; using fuzzy leaving the job
                                 ;; to the fuzzy fn.
                                 (string-match regexp str)
                               (invalid-regexp nil))))))

(defun helm-mm-3-search-base (pattern searchfn1 searchfn2)
  "Try to find PATTERN in `helm-buffer' with SEARCHFN1 and SEARCHFN2.
This is the search function for `candidates-in-buffer' enabled sources.
Use the same method as `helm-mm-3-match' except it search in buffer
instead of matching on a string.
i.e (identity (re-search-forward \"foo\" (point-at-eol) t)) => t."
  (cl-loop with pat = (if (stringp pattern)
                          (helm-mm-3-get-patterns pattern)
                          pattern)
           when (eq (caar pat) 'not) return
           ;; Pass the job to `helm-search-match-part'.
           (prog1 (list (point-at-bol) (point-at-eol))
             (forward-line 1))
           while (condition-case _err
                     (funcall searchfn1 (or (cdar pat) "") nil t)
                   (invalid-regexp nil))
           for bol = (point-at-bol)
           for eol = (point-at-eol)
           if (cl-loop for (pred . str) in (cdr pat) always
                       (progn (goto-char bol)
                              (funcall pred (condition-case _err
                                                (funcall searchfn2 str eol t)
                                              (invalid-regexp nil)))))
           do (goto-char eol) and return t
           else do (goto-char eol)
           finally return nil))

(defun helm-mm-3-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mm-3-get-patterns pattern)))
  (helm-mm-3-search-base
   pattern 're-search-forward 're-search-forward))

;;; mp-3 with migemo
;;
;;
(defvar helm-mm--previous-migemo-info nil
  "[Internal] Cache previous migemo query.")
(make-local-variable 'helm-mm--previous-migemo-info)

(declare-function migemo-get-pattern "ext:migemo.el")
(declare-function migemo-search-pattern-get "ext:migemo.el")

(define-minor-mode helm-migemo-mode
    "Enable migemo in helm.
It will be available in the sources handling it,
i.e the sources which have the slot :migemo with non--nil value."
  :lighter " Hmio"
  :group 'helm
  :global t
  (cl-assert (featurep 'migemo)
             nil "No feature called migemo found, install migemo.el."))

(defun helm-mm-migemo-get-pattern (pattern)
  (let ((regex (migemo-get-pattern pattern)))
    (if (ignore-errors (string-match regex "") t)
        (concat regex "\\|" pattern) pattern)))

(defun helm-mm-migemo-search-pattern-get (pattern)
  (let ((regex (migemo-search-pattern-get pattern)))
    (if (ignore-errors (string-match regex "") t)
        (concat regex "\\|" pattern) pattern)))

(defun helm-mm-migemo-string-match (pattern str)
  "Migemo version of `string-match'."
  (unless (assoc pattern helm-mm--previous-migemo-info)
    (with-helm-buffer
      (setq helm-mm--previous-migemo-info
            (push (cons pattern (helm-mm-migemo-get-pattern pattern))
                  helm-mm--previous-migemo-info))))
  (string-match (assoc-default pattern helm-mm--previous-migemo-info) str))

(cl-defun helm-mm-3-migemo-match (str &optional (pattern helm-pattern))
  (and helm-migemo-mode
       (cl-loop for (pred . re) in (helm-mm-3-get-patterns pattern)
                always (funcall pred (helm-mm-migemo-string-match re str)))))

(defun helm-mm-migemo-forward (word &optional bound noerror count)
  (with-helm-buffer
    (unless (assoc word helm-mm--previous-migemo-info)
      (setq helm-mm--previous-migemo-info
            (push (cons word (if (delq 'ascii (find-charset-string word))
                                 word
                               (helm-mm-migemo-search-pattern-get word)))
                  helm-mm--previous-migemo-info))))
  (re-search-forward
   (assoc-default word helm-mm--previous-migemo-info) bound noerror count))

(defun helm-mm-3-migemo-search (pattern &rest _ignore)
  (and helm-migemo-mode
       (helm-mm-3-search-base
        pattern 'helm-mm-migemo-forward 'helm-mm-migemo-forward)))


;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun helm-mm-3p-match (str &optional pattern)
  "Check if PATTERN match STR.
Same as `helm-mm-3-match' but more strict, matching against prefix also.
e.g \"bar foo\" will match \"barfoo\" but not \"foobar\" contrarily to
`helm-mm-3-match'."
  (let* ((pat (helm-mm-3-get-patterns (or pattern helm-pattern)))
         (first (car pat)))
    (and (funcall (car first) (helm-mm-prefix-match str (cdr first)))
         (cl-loop for (predicate . regexp) in (cdr pat)
               always (funcall predicate (string-match regexp str))))))

(defun helm-mm-3p-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mm-3-get-patterns pattern)))
  (helm-mm-3-search-base
   pattern 'helm-mm-prefix-search 're-search-forward))


;;; Generic multi-match/search functions
;;
;;
(cl-defun helm-mm-match (str &optional (pattern helm-pattern))
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-match)
               (multi2 #'helm-mm-2-match)
               (multi3 #'helm-mm-3-match)
               (multi3p #'helm-mm-3p-match))))
    (funcall fun str pattern)))

(defun helm-mm-search (pattern &rest _ignore)
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-search)
               (multi2 #'helm-mm-2-search)
               (multi3 #'helm-mm-3-search)
               (multi3p #'helm-mm-3p-search))))
    (funcall fun pattern)))


(provide 'helm-multi-match)


;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-multi-match.el ends here
