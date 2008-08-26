;;; rdebug-regexp.el --- Ruby debugger regular expressions

;; Copyright (C) 2007, 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007, 2008 Anders Lindgren

;; $Id: rdebug-regexp.el 724 2008-02-24 16:14:52Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; Here we have regular expressions and names for matched patterns
;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(defconst gud-rdebug-marker-regexp
  "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\).*\n"
  "Regular expression used to find a file location given by rdebug.

Program-location lines look like this:
   source /tmp/gcd.rb:29:  gcd
   /tmp/gcd.rb:29:  gcd
   source /tmp/gcd.rb:29
   source C:/tmp/gcd.rb:29
   source \\sources\\capfilterscanner\\capanalyzer.rb:3:  <module>")

(defconst rdebug-annotation-end-regexp
  "\n"
  "Regular expression to match the end of an annotation.")

;; Examples of annotations:
;; ^Z^Z\n
;; ^Z^Zfoo\n
;; ^Z^Zpre-prompt\n
;; ^Z^Zsource foo.rb:10\n

(defconst rdebug-annotation-start-regexp
  "\\(\\([a-z][-a-z]*\\)?\n\\|source \\)"
  "Regular expression to match the start of an annotation.")

(defconst rdebug-breakpoint-regexp
  "^\\ +\\([0-9]+\\) \\([yn]\\) +at +\\(.+\\):\\([0-9]+\\)\\( if .*\\)?$"
  "Regexp to recognize breakpoint lines in rdebug breakpoint buffers.")

(defconst rdebug-marker-regexp-file-group 2
  "Group position in `rdebug-position-regexp' that matches the file name.")

(defconst rdebug-marker-regexp-line-group 3
  "Group position in `rdebug-position-regexp' that matches the line number.")

(defconst rdebug-position-regexp
  "\\(\\)\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)"
  "Regular expression for a rdebug position.")

(defconst rdebug-traceback-line-re
  "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\( in `.*'\\)?"
  "Regular expression that describes a Ruby traceback line.")

(defconst rdebug-dollarbang-traceback-line-re
  "^[ \t]+[[]?\\([^:]+\\):\\([0-9]+\\):in `.*'"
  "Regular expression that describes a Ruby traceback line from $! list.")

(defconst rdebug-stack-frame-1st-regexp
  "^\\(-->\\|   \\) +#\\([0-9]+\\)\\(.*\\)"
  "Regexp to match the first line of a stack frame in rdebug stack buffers.")

(defconst rdebug-stack-frame-number-group 2
  "The group position in `rdebug-stack-frame-1st-regexp' that matches the frame number.")

(defconst rdebug-stack-frame-2nd-regexp
  "\s+at line +\\([^:]+\\):\\([0-9]+\\)$"
  "Regexp to match the second line of a stack frame in rdebug stack buffers.")

(defconst rdebug-stack-frame-2nd-file-group 1
  "Group position in `rdebug-stack-frame-2nd-regexp' that matches the file name.")

(defconst rdebug-stack-frame-2nd-line-group 2
  "Group position in `rdebug-stack-frame-2nd-regexp' that matches the line number.")
(defconst rdebug-stack-frame-regexp
  (concat rdebug-stack-frame-1st-regexp rdebug-stack-frame-2nd-regexp)
  "Regexp to recognize a stack frame line in rdebug stack buffers.")

(defconst rdebug-stack-frame-file-group 4
  "Group position in `rdebug-stack-frame-regexp' that matches the file name.")

(defconst rdebug-stack-frame-line-group 5
  "Group position in `rdebug-stack-frame-regexp' that matches the line number.")

(defconst rdebug-input-prompt-regexp "(+rdb:\\([0-9]+\\|post-mortem\\)) "
  "Regular expression to recognize a rdebug prompt.  Some uses may prepend an anchor to the front.")

(provide 'rdebug-regexp)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-regexp.el ends here
