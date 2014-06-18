;;; evil-matchit-cmake.el ---cmake (ruby/lua) plugin of evil-matchit

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

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK don make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

(defvar evilmi-cmake-extract-keyword-howtos
  '(("^[ \t]*\\([a-zA-Z]+ *\\) *(.*$" 1)
    ))

;; CMake (http://www.cmake.org) syntax
(defvar evilmi-cmake-match-tags
  '((("if" "IF") ("elseif" "ELSEIF" "else" "ELSE") ("endif" "ENDIF"))
    (("foreach" "FOREACH") () ("endforeach" "ENDFOREACH"))
    (("macro" "MACRO") () ("endmacro" "ENDMACRO"))
    (("while" "WHILE") () ("endwhile" "ENDWHILE"))
    (("function" "FUNCTION") () ("endfunction" "ENDFUNCTION"))
    )
  "The table we look up match tags. This is a three column table.
The first column contains the open tag(s).
The second column contains the middle tag(s).
The third column contains the closed tags(s).
"
  )

;;;###autoload
(defun evilmi-cmake-get-tag ()
  (evilmi-sdk-get-tag evilmi-cmake-match-tags evilmi-cmake-extract-keyword-howtos)
  )

;;;###autoload
(defun evilmi-cmake-jump (rlt NUM)
  (evilmi-sdk-jump rlt NUM evilmi-cmake-match-tags evilmi-cmake-extract-keyword-howtos)
  )

(provide 'evil-matchit-cmake)
