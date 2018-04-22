;;; seq.el --- Sequence manipulation functions  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 2.20
;; Package: seq

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.

;;; Code:

(if (version< emacs-version "25")
    (require 'seq-24)
  (require 'seq-25))

(provide 'seq)
;;; seq.el ends here
