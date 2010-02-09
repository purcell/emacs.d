;;; anything-match-plugin.el --- Humane match plug-in for anything
;; $Id: anything-match-plugin.el,v 1.22 2009/03/03 10:21:45 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, matching
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Change anything.el matching algorithm humanely.
;; It gives anything.el search refinement functionality.
;; exact match -> prefix match -> multiple regexp match

;; A query of multiple regexp match is space-delimited string.
;; Anything displays candidates which matches all the regexps.
;; A regexp with "!" prefix means not matching the regexp.
;; To include spaces to a regexp, prefix "\" before space,
;; it is controlled by `anything-mp-space-regexp' variable.

;; This file highlights patterns like `occur'. Note that patterns
;; longer than `anything-mp-highlight-threshold' are highlighted. And
;; region out of screen is highlighted after
;; `anything-mp-highlight-delay' seconds.
;;
;; Highlight in Emacs is time-consuming process for slow computers. To
;; disable it is to set nil to `anything-mp-highlight-delay'.

;; Just require it to use.

;;; History:

;; $Log: anything-match-plugin.el,v $
;; Revision 1.22  2009/03/03 10:21:45  rubikitch
;; * Remove highlight.el dependency.
;; * Very faster highlight.
;;
;; Revision 1.21  2009/03/03 08:51:23  rubikitch
;; New variable: `anything-mp-highlight-threshold'
;;
;; Revision 1.20  2009/03/03 07:29:24  rubikitch
;; Highlight matches!
;;
;; Revision 1.19  2008/09/08 06:58:59  rubikitch
;; changed default `anything-mp-space-regexp' to "[\\ ] "
;;
;; Revision 1.18  2008/09/07 12:09:01  rubikitch
;; *** empty log message ***
;;
;; Revision 1.17  2008/09/07 07:48:12  rubikitch
;; Append commentary.
;; Multiple regexp match with regexp negation.
;;
;; Revision 1.16  2008/09/07 06:58:11  rubikitch
;; Added mp-3p match: permutation with prefix match
;;
;; Revision 1.15  2008/09/07 05:23:07  rubikitch
;; New variable: `anything-mp-space-regexp'
;;
;; Revision 1.14  2008/09/03 03:33:09  rubikitch
;; anything-exact-*, anything-prefix-*: memoize
;;
;; Revision 1.13  2008/09/02 10:56:50  rubikitch
;; anything-mp-3-*: MUCH MUCH FASTER
;;   changed algorithm
;;
;; Revision 1.12  2008/09/01 13:41:57  rubikitch
;; search functions for search-from-end
;;
;; Revision 1.11  2008/08/24 20:40:27  rubikitch
;; prevent the unit test from being byte-compiled.
;;
;; Revision 1.10  2008/08/24 17:48:53  rubikitch
;; Add commentary
;;
;; Revision 1.9  2008/08/24 08:23:16  rubikitch
;; Rename `anything-candidates-buffer' -> `anything-candidate-buffer'
;;
;; Revision 1.8  2008/08/22 21:25:44  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2008/08/22 21:17:58  rubikitch
;; exact, prefix match: faster
;;
;; Revision 1.6  2008/08/22 19:40:22  rubikitch
;; exact -> prefix -> mp-3 by default because of speed
;;
;; Revision 1.5  2008/08/22 19:04:53  rubikitch
;; reimplemented
;;
;; Revision 1.4  2008/08/20 00:10:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.3  2008/08/19 23:30:39  rubikitch
;; exact match support
;;
;; Revision 1.2  2008/08/19 23:02:29  rubikitch
;; candidates-in-buffer hack
;;
;; Revision 1.1  2008/08/19 19:45:11  rubikitch
;; Initial revision
;;

;;; Code:

(require 'anything)
(require 'cl)

;;;; multiple patterns
(defvar anything-use-multiple-patterns t
  "If non-nil, enable anything-use-multiple-patterns.")
(defvar anything-mp-space-regexp "[\\ ] "
  "Regexp to represent space itself in multiple regexp match.")

(defun amp-mp-make-regexps (pattern)
  (if (string= pattern "") '("")
    (loop for s in (split-string (replace-regexp-in-string anything-mp-space-regexp "\000\000" pattern) " " t)
        collect (replace-regexp-in-string "\000\000" " " s))))

(defun amp-mp-1-make-regexp (pattern)
  (mapconcat 'identity (amp-mp-make-regexps pattern) ".*"))

(defmacro amp-define-memoizer (prefix pattern-expr)
  (let ((pattern-str (intern (concat prefix "pattern-str")))
        (pattern-real (intern (concat prefix "pattern-real")))
        (get-pattern (intern (concat prefix "get-pattern"))))
    `(progn
       (defvar ,pattern-str nil)
       (defvar ,pattern-real nil)
       (defsubst ,get-pattern (pattern)
         (unless (equal pattern ,pattern-str)
           (setq ,pattern-str pattern
                 ,pattern-real ,pattern-expr))
         ,pattern-real))))

(defmacro amp-define (prefix pattern-expr)
  (let ((get-pattern (intern (concat prefix "get-pattern"))) 
        (match (intern (concat prefix "match")))
        (search (intern (concat prefix "search")))
        (search-backward (intern (concat prefix "search-backward"))))
    `(progn
       (amp-define-memoizer ,prefix ,pattern-expr)
       (defun* ,match (str &optional (pattern anything-pattern))
         (string-match (,get-pattern pattern) str))
       (defun ,search (pattern &rest ignore)
         (re-search-forward (,get-pattern pattern) nil t))
       (defun ,search-backward (pattern &rest ignore)
         (re-search-backward (,get-pattern pattern) nil t)))))
  
;; exact match
;(amp-define "anything-exact-" (concat (anything-prefix-get-pattern pattern) "$"))
(amp-define-memoizer "anything-exact-" (concat "\n" pattern "\n"))
(defun anything-exact-match (str &optional pattern)
  (string= str (or pattern anything-pattern)))
(defun anything-exact-search (pattern &rest ignore)
  (and (search-forward (anything-exact-get-pattern pattern) nil t)
       (forward-line -1)))
(defun anything-exact-search-backward (pattern &rest ignore)
  (and (search-backward (anything-exact-get-pattern pattern) nil t)
       (forward-line 1)))
;; prefix match
;;(amp-define "anything-prefix-" (concat "^" (regexp-quote pattern)))
(amp-define-memoizer "anything-prefix-" (concat "\n" pattern))
(defun anything-prefix-match (str &optional pattern)
  (setq pattern (or pattern anything-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))
(defun anything-prefix-search (pattern &rest ignore)
  (search-forward (anything-prefix-get-pattern pattern) nil t))
(defun anything-prefix-search-backward (pattern &rest ignore)
  (and (search-backward (anything-prefix-get-pattern pattern) nil t)
       (forward-line 1)))
;; multiple regexp patterns 1 (order is preserved / prefix)
(amp-define "anything-mp-1-" (concat "^" (amp-mp-1-make-regexp pattern)))
;; multiple regexp patterns 2 (order is preserved / partial)
(amp-define "anything-mp-2-" (concat "^.+" (amp-mp-1-make-regexp pattern)))

;;;; multiple regexp patterns 3 (permutation)
(defvar anything-mp-3-pattern-str nil)
(defvar anything-mp-3-pattern-list nil)
(defsubst anything-mp-3-get-patterns (pattern)
  (unless (equal pattern anything-mp-3-pattern-str)
    (setq anything-mp-3-pattern-str pattern
          anything-mp-3-pattern-list
          (loop for pat in (amp-mp-make-regexps pattern)
                collect (if (string= "!" (substring pat 0 1))
                            (cons 'not (substring pat 1))
                          (cons 'identity pat)))))
  anything-mp-3-pattern-list)
(defun* anything-mp-3-match (str &optional (pattern anything-pattern))
  (loop for (pred . re) in (anything-mp-3-get-patterns pattern)
        always (funcall pred (string-match re str))))

(defmacro anything-mp-3-search-base (searchfn1 searchfn2 b e)
  `(loop with pat = (anything-mp-3-get-patterns pattern)
         while (,searchfn1 (or (cdar pat) "") nil t)
         for bol = (point-at-bol)
         for eol = (point-at-eol)
         if (loop 
             for (pred . s) in (cdr pat)
             always (progn (goto-char ,b)
                           (funcall pred (,searchfn2 s ,e t))))
         do (goto-char ,e) (return t)
         else do
         (goto-char ,e)
         finally (return nil)))

(defun anything-mp-3-search (pattern &rest ignore)
  (anything-mp-3-search-base re-search-forward re-search-forward bol eol))
(defun anything-mp-3-search-backward (pattern &rest ignore)
  (anything-mp-3-search-base re-search-backward re-search-backward eol bol))

;; mp-3p- (multiple regexp pattern 3 with prefix search)
(defun* anything-mp-3p-match (str &optional (pattern anything-pattern))
  (destructuring-bind ((first-pred . first-re) . rest)
      (anything-mp-3-get-patterns pattern)
    (and (funcall first-pred (anything-prefix-match str first-re))
         (loop for (pred . re) in rest
               always (funcall pred (string-match re str))))))
(defun anything-mp-3p-search (pattern &rest ignore)
  (anything-mp-3-search-base anything-prefix-search re-search-forward bol eol))

(defun anything-mp-3p-search-backward (pattern &rest ignore)
  (anything-mp-3-search-base anything-prefix-search-backward re-search-backward eol bol))

;;;; Highlight matches
(defface anything-match
  '((t (:inherit match)))
  "Face used to highlight matches.")

(defvar anything-mp-highlight-delay 0.7
  "Highlight matches with `anything-match' face after this many seconds.
 If nil, no highlight. ")

(defvar anything-mp-highlight-threshold 2
  "Minimum length of pattern to highlight.
The smaller  this value is, the slower highlight is.")

(defun anything-mp-highlight-match ()
  "Highlight matches after `anything-mp-highlight-delay' seconds."
  (when (and anything-mp-highlight-delay
             (not (string= anything-pattern "")))
    (anything-mp-highlight-match-internal (window-end (anything-window)))
    (run-with-idle-timer anything-mp-highlight-delay nil
                         'anything-mp-highlight-match-internal
                         (with-current-buffer anything-buffer (point-max)))))
(add-hook 'anything-update-hook 'anything-mp-highlight-match)

(defun anything-mp-highlight-region (start end regexp face)
  (save-excursion
    (goto-char start)
    (let (me)
      (while (and (setq me (re-search-forward regexp nil t)) (< (point) end))
        (put-text-property (match-beginning 0) me 'face face)))))

(defun* anything-mp-highlight-match-internal (end)
  (when (anything-window)
    (set-buffer anything-buffer)
    (let ((requote (regexp-quote anything-pattern)))
      (when (>= (length requote) anything-mp-highlight-threshold)
        (anything-mp-highlight-region (point-min) end
                                    requote 'anything-match)))
    (loop for (pred . re) in (anything-mp-3-get-patterns anything-pattern)
          when (and (eq pred 'identity) (>= (length re) anything-mp-highlight-threshold))
          do
          (anything-mp-highlight-region (point-min) end re 'anything-match))))
                         
;;;; source compier
(defvar anything-default-match-functions
  '(anything-exact-match anything-mp-3p-match anything-mp-3-match))
(defvar anything-default-search-functions
  '(anything-exact-search anything-mp-3p-search anything-mp-3-search))
(defvar anything-default-search-backward-functions
  '(anything-exact-search-backward anything-mp-3p-search-backward anything-mp-3-search-backward))
(defun anything-compile-source--match-plugin (source)
  (let ((searchers (if (assoc 'search-from-end source)
                       anything-default-search-backward-functions
                     anything-default-search-functions)))
    `(,(if (or (assoc 'candidates-in-buffer source)
               (equal '(identity) (assoc-default 'match source)))
           '(match identity)
         `(match ,@anything-default-match-functions
                 ,@(assoc-default 'match source)))
      (search ,@searchers
              ,@(assoc-default 'search source))
      ,@source)))

(add-to-list 'anything-compile-source-functions 'anything-compile-source--match-plugin t)

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "amp-mp-make-regexps")
      (expect '("")
        (amp-mp-make-regexps ""))
      (expect '("foo" "bar")
        (amp-mp-make-regexps "foo bar"))
      (expect '("foo" "bar")
        (amp-mp-make-regexps " foo bar"))
      (expect '("foo" "bar")
        (amp-mp-make-regexps " foo bar "))
      (expect '("foo bar" "baz")
        (let ((anything-mp-space-regexp "\\\\ "))
          (amp-mp-make-regexps "foo\\ bar baz")))
      (desc "anything-exact-match")
      (expect (non-nil)
        (anything-exact-match "thunder" "thunder"))
      (expect nil
        (anything-exact-match "thunder" "fire"))
      (desc "anything-exact-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-exact-search "thunder" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "\nfire\nthunder\n")
          (goto-char 1)
          (anything-exact-search "fire" nil t)))
      (desc "anything-prefix-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-min))
          (anything-prefix-search "thund" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-min))
          (anything-prefix-search "hund" nil t)))
      (desc "anything-prefix-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-prefix-search-backward "thund" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-prefix-search-backward "hund" nil t)))
      (desc "amp-mp-1-make-regexp")
      (expect "a.*b"
        (amp-mp-1-make-regexp "a b"))
      (expect "a b"
        (let ((anything-mp-space-regexp "\\\\ "))
          (amp-mp-1-make-regexp "a\\ b")))
      (expect "a.*b c"
        (let ((anything-mp-space-regexp "\\\\ "))
          (amp-mp-1-make-regexp "a b\\ c")))
      (expect ""
        (amp-mp-1-make-regexp ""))
      (desc "anything-mp-1-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-1-search "th+ r" nil t)))
      (desc "anything-mp-2-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-2-search "h+ r" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-2-search "th+ r" nil t)))
      (desc "anything-mp-3-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "h+ r" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "th+ r" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "r th+" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "under hue" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "r th+ n" nil t)))
      (desc "anything-mp-3-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "th ders" nil t)))
      (desc "anything-mp-3-search not")
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "h !der" nil t)))
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (anything-mp-3-search "th !der" nil t)))
      (desc "anything-mp-3p-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3p-search "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (anything-mp-3p-search "h ders" nil t)))
      (desc "anything-mp-3p-search not")
      (expect t
        (with-temp-buffer
          (insert "\nthreshold\nthunder\n")
          (goto-char 1)
          (anything-mp-3p-search "th !der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (anything-mp-3p-search "h !der" nil t)))
      (desc "anything-mp-3-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3-search-backward "h der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3-search-backward "th ders" nil t)))
      (desc "anything-mp-3-search-backward not")
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3-search-backward "h !der" nil t)))
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3-search-backward "th !der" nil t)))
      (desc "anything-mp-3p-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3p-search-backward "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3p-search-backward "h der" nil t)))
      (desc "anything-mp-3p-search-backward not")
      (expect t
        (with-temp-buffer
          (insert "\nthreshold\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3p-search-backward "th !der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (anything-mp-3p-search-backward "h !der" nil t)))
      (desc "anything-mp-1-match")
      (expect (non-nil)
        (anything-mp-1-match "thunder" "th+ r"))
      (desc "anything-mp-2-match")
      (expect (non-nil)
        (anything-mp-2-match "thunder" "h+ r"))
      (expect nil
        (anything-mp-2-match "thunder" "th+ r"))
      (desc "anything-mp-3-match")
      (expect (non-nil)
        (anything-mp-3-match "thunder" "h+ r"))
      (expect (non-nil)
        (anything-mp-3-match "thunder" "th+ r"))
      (expect (non-nil)
        (anything-mp-3-match "thunder" "r th+"))
      (expect nil
        (anything-mp-3-match "thunder" "under hue"))
      (expect (non-nil)
        (anything-mp-3-match "thunder" "r th+ n"))
      (desc "anything-mp-3-match not")
      (expect (non-nil)
        (anything-mp-3-match "threshold" "th !der"))
      (desc "anything-prefix-match")
      (expect (non-nil)
        (anything-prefix-match "fobar" "fo"))
      (expect nil
        (anything-prefix-match "xfobar" "fo"))

      (desc "anything-mp-3-match")
      (expect (non-nil)
        (anything-mp-3-match "thunder" "h der"))
      (expect nil
        (anything-mp-3-match "thunder" "h ders"))
      (desc "anything-mp-3p-match")
      (expect (non-nil)
        (anything-mp-3p-match "thunder" "th der"))
      (expect nil
        (anything-mp-3p-match "thunder" "h der"))
      (desc "anything-mp-3p-match not")
      (expect (non-nil)
        (anything-mp-3p-match "threshold" "th !der"))
      (expect nil
        (anything-mp-3p-match "threshold" "h !der"))
      (desc "with identity match")
      (expect '(identity)
        (assoc-default 'match
                       (car (anything-compile-sources
                             '(((name . "FOO")
                                (candidates-in-buffer)))
                             '(anything-compile-source--candidates-in-buffer
                               anything-compile-source--match-plugin)))))
      (expect '(identity)
        (assoc-default 'match
                       (car (anything-compile-sources
                             '(((name . "FOO")
                                (match identity)))
                             '(anything-compile-source--match-plugin)))))
      (desc "functional")
      (expect '(("FOO" ("thunder")))
        (anything-test-candidates '(((name . "FOO")
                                     (candidates "fire" "thunder")))
                                  "th r"
                                  '(anything-compile-source--match-plugin)))
      (expect '(("FOO" ("one two")))
        (let ((anything-mp-space-regexp "\\\\ "))
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "one two" "three four")))
                                    "e\\ t"
                                    '(anything-compile-source--match-plugin))))
      (expect '(("FOO" ("one two")))
        (let ((anything-mp-space-regexp "  "))
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "one two" "three four")))
                                    "e  t"
                                    '(anything-compile-source--match-plugin))))
      (expect '(("FOO" ("thunder")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "fire\nthunder\nthanks\n"))))
                                     (candidates-in-buffer)))
                                  "th r"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (anything-test-candidates '(((name . "FOO")
                                     (candidates "foobar" "foo")))
                                  "foo"
                                  '(anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foo")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "foo\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foo")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "bar\nfoo\ntest\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foobar" "foo")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)))
                                  ""
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "foo"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("elisp" "elp")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "elp\nelisp\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "el p"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      (expect '(("FOO" ("elisp" )))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "elp\nelisp\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "el+ isp"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      ;; prefix multi -> multi
      (expect '(("FOO" ("elisp-info" "info.el")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "info.el\nelisp-info\n"))))
                                     (candidates-in-buffer)
                                     ))
                                  "el info"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      ;; multi not
      (expect '(("FOO" ("info.el")))
        (anything-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (anything-candidate-buffer 'global)
                                            (insert "info.el\nelisp-info\n"))))
                                     (candidates-in-buffer)
                                     ))
                                  "info !elisp"
                                  '(anything-compile-source--candidates-in-buffer
                                    anything-compile-source--match-plugin)))
      )))
;; (anything-compile-sources '(((name . "test"))) anything-compile-source-functions)
(provide 'anything-match-plugin)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-match-plugin.el")
;;; anything-match-plugin.el ends here
