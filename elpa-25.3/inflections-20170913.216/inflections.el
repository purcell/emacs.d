;;; inflections.el --- convert english words between singular and plural  -*- lexical-binding: t -*-

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Author: Dmitry Galinsky, Howard Yeh
;; URL: https://github.com/eschulte/jump.el
;; Package-Version: 20170913.216
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
;; Version: 1.1
;; Created: 2007-11-02
;; Keywords: languages, tools, wp

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:
(require 'cl-lib)

(defvar inflection-singulars    nil)
(defvar inflection-plurals      nil)
(defvar inflection-irregulars   nil)
(defvar inflection-uncountables nil)

(defmacro define-inflectors (&rest specs)
  (cons 'progn
        (cl-loop for (type . rest) in specs
                 collect (cl-case type
                           (:singular `(push (quote ,rest) inflection-singulars))
                           (:plural `(push (quote ,rest) inflection-plurals))
                           (:irregular `(push (quote ,rest) inflection-irregulars))
                           (:uncountable `(setf inflection-uncountables
                                                (append (quote ,rest) inflection-uncountables)))))))

(defmacro inflection--string=~ (regex string &rest body)
  "Regex matching similar to the =~ operator found in other languages."
  (let ((str (cl-gensym)))
    `(let ((,str ,string))
       (save-match-data
         (when (string-match ,regex ,str)
           (cl-symbol-macrolet ,(cl-loop for i to 9 collect
                                         (let ((sym (intern (concat "$" (number-to-string i)))))
                                           `(,sym (match-string ,i ,str))))
             (cl-flet (($ (i) (match-string i ,str))
                       (sub (replacement &optional (i 0) &key fixedcase literal-string)
                            (replace-match replacement fixedcase literal-string ,str i)))
               (cl-symbol-macrolet ( ;;before
                                    ($b (substring ,str 0 (match-beginning 0)))
                                    ;;match
                                    ($m (match-string 0 ,str))
                                    ;;after
                                    ($a (substring ,str (match-end 0) (length ,str))))
                 ,@body))))))))

(define-inflectors
  ;; Rules copied from https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
  (:plural "$" "s")
  (:plural "s$" "s")
  (:plural "\\(ax\\|test\\)is$" "\\1es")
  (:plural "\\(octop\\|vir\\)us$" "\\1i")
  (:plural "\\(octop\\|vir\\)i$" "\\1i")
  (:plural "\\(alias\\|status\\)$" "\\1es")
  (:plural "\\(bu\\)s$" "\\1ses")
  (:plural "\\(buffal\\|tomat\\)o$" "\\1oes")
  (:plural "\\([ti]\\)um$" "\\1a")
  (:plural "\\([ti]\\)a$" "\\1a")
  (:plural "sis$" "ses")
  (:plural "\\(?:\\([^f]\\)fe\\|\\([lr]\\)f\\)$" "\\1\\2ves")
  (:plural "\\(hive\\)$" "\\1s")
  (:plural "\\([^aeiouy]\\|qu\\)y$" "\\1ies")
  (:plural "\\(x\\|ch\\|ss\\|sh\\)$" "\\1es")
  (:plural "\\(matr\\|vert\\|ind\\)\\(ix\\|ex\\)$" "\\1ices")
  (:plural "\\(m\\|l\\)ouse$" "\\1ice")
  (:plural "\\(m\\|l\\)ice$" "\\1ice")
  (:plural "^\\(ox\\)$" "\\1en")
  (:plural "^\\(oxen\\)$" "\\1")
  (:plural "\\(quiz\\)$" "\\1zes")

  (:singular "s$" "")
  (:singular "\\(ss\\)$" "\\1")
  (:singular "\\(n\\)ews$" "\\1ews")
  (:singular "\\([ti]\\)a$" "\\1um")
  (:singular "\\(\\(a\\)naly\\|\\(b\\)a\\|\\(d\\)iagno\\|\\(p\\)arenthe\\|\\(p\\)rogno\\|\\(s\\)ynop\\|\\(t\\)he\\)\\(ses\\|sis\\)$" "\\1sis")
  (:singular "\\(^analy\\)\\(ses\\|sis\\)$" "\\1sis")
  (:singular "\\([^f]\\)ves$" "\\1fe")
  (:singular "\\(hive\\)s$" "\\1")
  (:singular "\\(tive\\)s$" "\\1")
  (:singular "\\([lr]\\)ves$" "\\1f")
  (:singular "\\([^aeiouy]\\|qu\\)ies$" "\\1y")
  (:singular "\\(s\\)eries$" "\\1eries")
  (:singular "\\(m\\)ovies$" "\\1ovie")
  (:singular "\\(x\\|ch\\|ss\\|sh\\)es$" "\\1")
  (:singular "\\(m\\|l\\)ice$" "\\1ouse")
  (:singular "\\(bus\\)\\(es\\)?$" "\\1")
  (:singular "\\(o\\)es$" "\\1")
  (:singular "\\(shoe\\)s$" "\\1")
  (:singular "\\(cris\\|test\\)\\(is\\|es\\)$" "\\1is")
  (:singular "^\\(a\\)x[ie]s$" "\\1xis")
  (:singular "\\(octop\\|vir\\)\\(us\\|i\\)$" "\\1us")
  (:singular "\\(alias\\|status\\)\\(es\\)?$" "\\1")
  (:singular "^\\(ox\\)en" "\\1")
  (:singular "\\(vert\\|ind\\)ices$" "\\1ex")
  (:singular "\\(matr\\)ices$" "\\1ix")
  (:singular "\\(quiz\\)zes$" "\\1")
  (:singular "\\(database\\)s$" "\\1")

  (:irregular "person" "people")
  (:irregular "man" "men")
  (:irregular "child" "children")
  (:irregular "sex" "sexes")
  (:irregular "move" "moves")
  (:irregular "zombie" "zombies")

  ;; Additional rules that are not in ActiveSupport::Inflections
  (:irregular "syllabus" "syllabi")
  (:irregular "radius" "radii")
  (:irregular "addendum" "addenda")
  (:irregular "cactus" "cacti")
  (:irregular "corpus" "corpora")
  (:irregular "criterion" "criteria")
  (:irregular "genus" "genera")
  (:irregular "medium" "media")

  (:uncountable "equipment" "information" "rice" "money" "species" "series" "fish" "sheep" "jeans" "police"))

;;;###autoload
(defun inflection-singularize-string (str)
  "Return the singularized version of STR."
  (when (stringp str)
    (or (car (member str inflection-uncountables))
        (caar (cl-member (downcase str) inflection-irregulars :key 'cadr :test 'equal))
        (cl-loop for (from to) in inflection-singulars
                 for singular = (inflection--string=~ from str (sub to))
                 when singular return singular)
        str)))

;;;###autoload
(define-obsolete-function-alias 'singularize-string 'inflection-singularize-string)

;;;###autoload
(defun inflection-pluralize-string (str)
  "Return the pluralized version of STR."
  (when (stringp str)
    (or (car (member str inflection-uncountables))
        (cl-cadar (cl-member (downcase str) inflection-irregulars :key 'car :test 'equal))
        (cl-loop for (from to) in inflection-plurals
                 for plurals = (inflection--string=~ from str (sub to))
                 when plurals return plurals)
        str)))

;;;###autoload
(define-obsolete-function-alias 'pluralize-string 'inflection-pluralize-string)

(provide 'inflections)
;;; inflections.el ends here
