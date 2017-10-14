;;; haskell-sandbox.el --- Support for sandboxes -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'haskell-session)

(defun haskell-sandbox-path (session)
  "If there is a haskell-session, return the path to the usual sandbox location."
  (concat (haskell-session-cabal-dir session)
          "/.cabal-sandbox"))

(defun haskell-sandbox-exists-p (session)
  "Is there a cabal sandbox?"
  (file-exists-p (haskell-sandbox-path session)))

(defun haskell-sandbox-pkgdb (session)
  "Get the package database of the sandbox."
  (let* ((files (directory-files (haskell-sandbox-path session)))
         (dir (car (cl-remove-if-not (lambda (file)
                                        (string-match ".conf.d$" file))
                                      files))))
    (when dir
      (concat (haskell-sandbox-path session) "/" dir))))

(provide 'haskell-sandbox)
