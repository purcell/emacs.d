;;; elm-tags.el --- etags support for Elm.

;; Copyright (C) 2016  Bogdan Popa

;; Author: Bogdan Popa
;; URL: https://github.com/jcollard/elm-mode

;; This file is not part of GNU Emacs.

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

;;; Commentary:
;;; Code:
(require 'elm-util)
(require 'f)

(defcustom elm-tags-on-save nil
  "Controls whether or not TAGS files should be generated on save."
  :group 'elm-tags
  :type 'boolean)

(defcustom elm-tags-exclude-elm-stuff t
  "Controls whether or not sources in the `elm-stuff' directory should be excluded from the TAGS file."
  :group 'elm-tags
  :type 'boolean)

(defconst elm-tags-regexps
  (f-join
   (f-dirname load-file-name)
   "elm.tags"))

;;;###autoload
(defun elm-mode-goto-tag-at-point ()
  "Go to tag at point."
  (interactive)
  (let ((tag (find-tag-default)))
    (unless tag
      (user-error "No tag candidate found around point"))
    (find-tag tag)))

;;;###autoload
(defun elm-mode-generate-tags ()
  "Generate a TAGS file for the current project."
  (interactive)
  (when (elm--has-dependency-file)
    (let* ((default-directory (elm--find-dependency-file-path))
           (find-command "find . -type f -name \"*.elm\" -print")
           (exclude-command (if elm-tags-exclude-elm-stuff
                                (concat find-command " | egrep -v elm-stuff")
                              find-command))
           (etags-command (concat
                           exclude-command
                           " | etags --language=none --regex=@"
                           (shell-quote-argument elm-tags-regexps)
                           " -")))
      (call-process-shell-command (concat etags-command "&") nil 0))))


(provide 'elm-tags)
;;; elm-tags.el ends here
