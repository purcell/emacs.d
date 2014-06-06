;;; helm-match-plugin.el --- Multiple regexp matching methods for helm -*- lexical-binding: t -*-

;; Original Author: rubikitch

;; Copyright (C) 2008 ~ 2011 rubikitch
;; Copyright (C) 2011 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'helm)
(require 'cl-lib)

;;;; Match-plugin

;; Internal
(defvar helm-mp-default-match-functions nil)
(defvar helm-mp-default-search-functions nil)
(defvar helm-mp-default-search-backward-functions nil)

(defun helm-mp-set-matching-method (var key)
  "Default function to set matching methods in helm match plugin."
  (set-default var key)
  (cl-case (symbol-value var)
    (multi1 (setq helm-mp-default-match-functions
                  '(helm-mp-exact-match helm-mp-1-match)
                  helm-mp-default-search-functions
                  '(helm-mp-exact-search helm-mp-1-search)
                  helm-mp-default-search-backward-functions
                  '(helm-mp-exact-search-backward
                    helm-mp-1-search-backward)))
    (multi2 (setq helm-mp-default-match-functions
                  '(helm-mp-exact-match helm-mp-2-match)
                  helm-mp-default-search-functions
                  '(helm-mp-exact-search helm-mp-2-search)
                  helm-mp-default-search-backward-functions
                  '(helm-mp-exact-search-backward
                    helm-mp-2-search-backward)))
    (multi3 (setq helm-mp-default-match-functions
                  '(helm-mp-exact-match helm-mp-3-match)
                  helm-mp-default-search-functions
                  '(helm-mp-exact-search helm-mp-3-search)
                  helm-mp-default-search-backward-functions
                  '(helm-mp-exact-search-backward
                    helm-mp-3-search-backward)))
    (multi3p (setq helm-mp-default-match-functions
                   '(helm-mp-exact-match helm-mp-3p-match)
                   helm-mp-default-search-functions
                   '(helm-mp-exact-search helm-mp-3p-search)
                   helm-mp-default-search-backward-functions
                   '(helm-mp-exact-search-backward
                     helm-mp-3p-search-backward)))
    (t (error "Unknown value: %s" helm-mp-matching-method))))

(defgroup helm-match-plugin nil
  "Helm match plugin."
  :group 'helm)

(defcustom helm-mp-matching-method 'multi3
  "Matching method for helm match plugin.
You can set here different methods to match candidates in helm.
Here are the possible value of this symbol and their meaning:
- multi1: Respect order, prefix of pattern must match.
- multi2: Same but with partial match.
- multi3: The best, multiple regexp match, allow negation.
- multi3p: Same but prefix must match.
Default is multi3."
  :type  '(radio :tag "Matching methods for helm"
           (const :tag "Multiple regexp 1 ordered with prefix match"         multi1)
           (const :tag "Multiple regexp 2 ordered with partial match"        multi2)
           (const :tag "Multiple regexp 3 matching no order, partial, best." multi3)
           (const :tag "Multiple regexp 3p matching with prefix match"       multi3p))
  :set   'helm-mp-set-matching-method
  :group 'helm-match-plugin)

(defface helm-match
    '((t (:inherit match)))
  "Face used to highlight matches."
  :group 'helm-match-plugin)

(defcustom helm-mp-highlight-delay 0.7
  "Highlight matches with `helm-match' face after this many seconds.
 If nil, no highlight. "
  :type  'integer
  :group 'helm-match-plugin)

(defcustom helm-mp-highlight-threshold 2
  "Minimum length of pattern to highlight.
The smaller  this value is, the slower highlight is."
  :type  'integer
  :group 'helm-match-plugin)

;;;###autoload
(define-minor-mode helm-match-plugin-mode
    "Add more flexible regexp matching for helm.
See `helm-mp-matching-method' for the behavior of each method."
  :group 'helm-match-plugin
  :require 'helm-match-plugin
  :global t
  (if helm-match-plugin-mode
      (progn
        (add-to-list 'helm-compile-source-functions 'helm-compile-source--match-plugin)
        (add-hook 'helm-update-hook 'helm-mp-highlight-match))
    (setq helm-compile-source-functions
          (delq 'helm-compile-source--match-plugin
                helm-compile-source-functions))
    (remove-hook 'helm-update-hook 'helm-mp-highlight-match)))


;;; Build regexps
;;
;;
(defvar helm-mp-space-regexp "[\\ ] "
  "Regexp to represent space itself in multiple regexp match.")

(defun helm-mp-split-pattern (pattern)
  "Split PATTERN if it contain spaces and return resulting list.
If spaces in PATTERN are escaped, don't split at this place.
i.e \"foo bar\"=> (\"foo\" \"bar\")
but \"foo\ bar\"=> (\"foobar\")."
  (if (string= pattern "")
      '("")
    (cl-loop for s in (split-string
                       (replace-regexp-in-string helm-mp-space-regexp
                                                 "\000\000" pattern)
                       " " t)
          collect (replace-regexp-in-string "\000\000" " " s))))

(defun helm-mp-1-make-regexp (pattern)
  "Replace spaces in PATTERN with \"\.*\"."
  (mapconcat 'identity (helm-mp-split-pattern pattern) ".*"))


;;; Exact match.
;;
;;
;; Internal.
(defvar helm-mp-exact-pattern-str nil)
(defvar helm-mp-exact-pattern-real nil)

(defun helm-mp-exact-get-pattern (pattern)
  (unless (equal pattern helm-mp-exact-pattern-str)
    (setq helm-mp-exact-pattern-str pattern
          helm-mp-exact-pattern-real (concat "\n" pattern "\n")))
  helm-mp-exact-pattern-real)


(defun helm-mp-exact-match (str &optional pattern)
  (string= str (or pattern helm-pattern)))

(defun helm-mp-exact-search (pattern &rest _ignore)
  (and (search-forward (helm-mp-exact-get-pattern pattern) nil t)
       (forward-line -1)))

(defun helm-mp-exact-search-backward (pattern &rest _ignore)
  (and (search-backward (helm-mp-exact-get-pattern pattern) nil t)
       (forward-line 1)))


;;; Prefix match
;;
;;
;; Internal
(defvar helm-mp-prefix-pattern-str nil)
(defvar helm-mp-prefix-pattern-real nil)

(defun helm-mp-prefix-get-pattern (pattern)
  (unless (equal pattern helm-mp-prefix-pattern-str)
    (setq helm-mp-prefix-pattern-str pattern
          helm-mp-prefix-pattern-real (concat "\n" pattern)))
  helm-mp-prefix-pattern-real)

(defun helm-mp-prefix-match (str &optional pattern)
  (setq pattern (or pattern helm-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))

(defun helm-mp-prefix-search (pattern &rest _ignore)
  (search-forward (helm-mp-prefix-get-pattern pattern) nil t))

(defun helm-mp-prefix-search-backward (pattern &rest _ignore)
  (and (search-backward (helm-mp-prefix-get-pattern pattern) nil t)
       (forward-line 1)))


;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
;; Internal
(defvar helm-mp-1-pattern-str nil)
(defvar helm-mp-1-pattern-real nil)

(defun helm-mp-1-get-pattern (pattern)
  (unless (equal pattern helm-mp-1-pattern-str)
    (setq helm-mp-1-pattern-str pattern
          helm-mp-1-pattern-real
          (concat "^" (helm-mp-1-make-regexp pattern))))
  helm-mp-1-pattern-real)

(cl-defun helm-mp-1-match (str &optional (pattern helm-pattern))
  (string-match (helm-mp-1-get-pattern pattern) str))

(defun helm-mp-1-search (pattern &rest _ignore)
  (re-search-forward (helm-mp-1-get-pattern pattern) nil t))

(defun helm-mp-1-search-backward (pattern &rest _ignore)
  (re-search-backward (helm-mp-1-get-pattern pattern) nil t))


;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
;; Internal
(defvar helm-mp-2-pattern-str nil)
(defvar helm-mp-2-pattern-real nil)

(defun helm-mp-2-get-pattern (pattern)
  (unless (equal pattern helm-mp-2-pattern-str)
    (setq helm-mp-2-pattern-str pattern
          helm-mp-2-pattern-real
          (concat "^.*" (helm-mp-1-make-regexp pattern))))
  helm-mp-2-pattern-real)

(cl-defun helm-mp-2-match (str &optional (pattern helm-pattern))
  (string-match (helm-mp-2-get-pattern pattern) str))

(defun helm-mp-2-search (pattern &rest _ignore)
  (re-search-forward (helm-mp-2-get-pattern pattern) nil t))

(defun helm-mp-2-search-backward (pattern &rest _ignore)
  (re-search-backward (helm-mp-2-get-pattern pattern) nil t))


;;; Multiple regexp patterns 3 (permutation).
;;
;;
;; Internal
(defvar helm-mp-3-pattern-str nil)
(defvar helm-mp-3-pattern-list nil)

(defun helm-mp-3-get-patterns (pattern)
  "Return `helm-mp-3-pattern-list', a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\")).
This is done only if `helm-mp-3-pattern-str' is same as PATTERN."
  (unless (equal pattern helm-mp-3-pattern-str)
    (setq helm-mp-3-pattern-str pattern
          helm-mp-3-pattern-list
          (helm-mp-3-get-patterns-internal pattern)))
  helm-mp-3-pattern-list)

(defun helm-mp-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\"))."
  (unless (string= pattern "")
    (cl-loop for pat in (helm-mp-split-pattern pattern)
          collect (if (string= "!" (substring pat 0 1))
                      (cons 'not (substring pat 1))
                    (cons 'identity pat)))))

(defun helm-mp-3-match (str &optional pattern)
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mp-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mp-3-get-patterns (or pattern helm-pattern))))
    (cl-loop for (predicate . regexp) in pat
          always (funcall predicate (string-match regexp str)))))

(defun helm-mp-3-search-base (pattern searchfn1 searchfn2)
  "Try to find PATTERN in `helm-buffer' with SEARCHFN1 and SEARCHFN2.
This is the search function for `candidates-in-buffer' enabled sources.
Use the same method as `helm-mp-3-match' except it search in buffer
instead of matching on a string.
i.e (identity (re-search-forward \"foo\" (point-at-eol) t)) => t."
  (cl-loop with pat = (if (stringp pattern)
                          (helm-mp-3-get-patterns pattern)
                        pattern)
        while (funcall searchfn1 (or (cdar pat) "") nil t)
        for bol = (point-at-bol)
        for eol = (point-at-eol)
        if (cl-loop for (pred . str) in (cdr pat) always
                 (progn (goto-char bol)
                        (funcall pred (funcall searchfn2 str eol t))))
        do (goto-char eol) and return t
        else do (goto-char eol)
        finally return nil))

(defun helm-mp-3-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 're-search-forward 're-search-forward))

(defun helm-mp-3-search-backward (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 're-search-backward 're-search-backward))


;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun helm-mp-3p-match (str &optional pattern)
  "Check if PATTERN match STR.
Same as `helm-mp-3-match' but more strict, matching against prefix also.
e.g \"bar foo\" will match \"barfoo\" but not \"foobar\" contrarily to
`helm-mp-3-match'."
  (let* ((pat (helm-mp-3-get-patterns (or pattern helm-pattern)))
         (first (car pat)))
    (and (funcall (car first) (helm-mp-prefix-match str (cdr first)))
         (cl-loop for (predicate . regexp) in (cdr pat)
               always (funcall predicate (string-match regexp str))))))

(defun helm-mp-3p-search (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 'helm-mp-prefix-search 're-search-forward))

(defun helm-mp-3p-search-backward (pattern &rest _ignore)
  (when (stringp pattern)
    (setq pattern (helm-mp-3-get-patterns pattern)))
  (helm-mp-3-search-base
   pattern 'helm-mp-prefix-search-backward 're-search-backward))


;;; source compiler
;;
;;
(defun helm-compile-source--match-plugin (source)
  (if (assoc 'no-matchplugin source)
      source
    (let* ((searchers        (if (assoc 'search-from-end source)
                                 helm-mp-default-search-backward-functions
                               helm-mp-default-search-functions))
           (defmatch         (helm-aif (assoc-default 'match source)
                                 (helm-mklist it)))
           (defmatch-strict  (helm-aif (assoc-default 'match-strict source)
                                 (helm-mklist it)))
           (defsearch        (helm-aif (assoc-default 'search source)
                                 (helm-mklist it)))
           (defsearch-strict (helm-aif (assoc-default 'search-strict source)
                                 (helm-mklist it)))
           (matchfns         (cond (defmatch-strict)
                                   (defmatch
                                    (append helm-mp-default-match-functions defmatch))
                                   (t helm-mp-default-match-functions)))
           (searchfns        (cond (defsearch-strict)
                                   (defsearch
                                    (append searchers defsearch))
                                   (t searchers))))
      `(,(if (or (assoc 'candidates-in-buffer source)
                 (equal '(identity) matchfns))
             '(match identity) `(match ,@matchfns))
         (search ,@searchfns)
         ,@source))))


;;; Highlight matches.
;;
;;
(defun helm-mp-highlight-match ()
  "Highlight matches after `helm-mp-highlight-delay' seconds."
  (unless (or (assoc 'nohighlight (helm-get-current-source))
              (not helm-mp-highlight-delay)
              (helm-empty-buffer-p)
              (string= helm-pattern ""))
    (helm-mp-highlight-match-internal (window-end (helm-window)))
    (run-with-idle-timer helm-mp-highlight-delay nil
                         'helm-mp-highlight-match-internal
                         (with-current-buffer helm-buffer (point-max)))))

(defun helm-mp-highlight-region (start end regexp face)
  (save-excursion
    (goto-char start)
    (let ((case-fold-search (helm-set-case-fold-search regexp)) me)
      (condition-case _err
          (while (and (setq me (re-search-forward regexp nil t))
                      (< (point) end)
                      (< 0 (- (match-end 0) (match-beginning 0))))
            (unless (helm-pos-header-line-p)
              (put-text-property (match-beginning 0) me 'face face)))
        (invalid-regexp nil)))))

(defun helm-mp-highlight-match-internal (end)
  (when helm-alive-p
    (set-buffer helm-buffer)
    (let ((requote (cl-loop for (pred . re) in
                         (helm-mp-3-get-patterns helm-pattern)
                         when (and (eq pred 'identity)
                                   (>= (length re)
                                       helm-mp-highlight-threshold))
                         collect re into re-list
                         finally return
                         (if (and re-list (>= (length re-list) 1))
                             (mapconcat 'identity re-list "\\|")
                           (regexp-quote helm-pattern)))))
      (when (>= (length requote) helm-mp-highlight-threshold)
        (helm-mp-highlight-region
         (point-min) end requote 'helm-match)))))

;; Enable match-plugin by default.
(helm-match-plugin-mode 1)

(provide 'helm-match-plugin)


;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-match-plugin.el ends here
