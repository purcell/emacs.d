;;; evil-matchit-c.el --c like language (c/c++/perl/java/javascript) plugin of evil-matchit

;; Copyright (C) 2014  Chen Bin <chenbin.sh@gmail.com>

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:
(require 'evil-matchit-sdk)

;; ruby/bash/lua/vimrc
(defvar evilmi-c-match-tags
  '((("ifdef" "ifndef" "if") ("elif" "else")  "endif")
    ("switch" "case" "default"))
  "The table we look up match tags. This is a three column table.
The first column contains the open tag(s).
The second column contains the middle tag(s).
The third column contains the closed tags(s).
"
  )

(defvar evilmi-c-extract-keyword-howtos
  '(("^[ \t]*#[ \t]*\\([a-z]+\\)\\( .*\\| *\\)$" 1)
    ("^[ \t]*\\([a-z]+\\)\\([ (:].*\\| *\\)$" 1))
  "The list of HOWTO on extracting keyword from current line.
Each howto is actually a pair. The first element of pair is the regular
expression to match the current line. The second is the index of sub-matches
to extract the keyword which starts from one. The sub-match is the match defined
between '\\(' and '\\)' in regular expression.
"
  )

;;;###autoload
(defun evilmi-c-get-tag ()
  (evilmi-sdk-get-tag evilmi-c-match-tags evilmi-c-extract-keyword-howtos)
  )

;;;###autoload
(defun evilmi-c-jump (rlt NUM)
  (evilmi-sdk-jump rlt NUM evilmi-c-match-tags evilmi-c-extract-keyword-howtos)
  )

(provide 'evil-matchit-c)
