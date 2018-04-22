;;; flycheck-elm.el --- Flycheck support for the elm language

;; Copyright (c) 2015 Brian Sermons

;; Author: Brian Sermons
;; Package-Requires: ((flycheck "0.29-cvs") (emacs "24.4"))
;; Package-Version: 20160717.257
;; URL: https://github.com/bsermons/flycheck-elm

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

;; Enable this checker automatically by adding code like the following
;; to your startup files:

;;     (eval-after-load 'flycheck
;;       '(flycheck-elm-setup))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'flycheck)

(defgroup flycheck-elm nil
  "Elm support for Flycheck."
  :prefix "flycheck-elm-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/bsermons/flycheck-elm"))

(defcustom flycheck-elm-reporting-mode 'all
  "*Types of messages to show."
  :type '(choice
          (const :tag "Show warnings and errors." all)
          (const :tag "Show only errors." errors-only)
          (const :tag "Show warnings only if no errors occur." warn-after-errors))
  :group 'flycheck-elm)

(defun flycheck-elm-decode-elm-error (error checker buffer)
  (let* ((region (assoc 'region error))
         (tag (concat "[" (cdr (assoc 'tag error)) "]"))
         (overview (cdr (assoc 'overview error)))
         (details (cdr (assoc 'details error)))
         (start (assoc 'start region))
         (start-col (cdr (assoc 'column start)))
         (start-line (cdr (assoc 'line start))))
    (flycheck-error-new
     :checker checker
     :buffer buffer
     :filename (cdr (assoc 'file error))
     :line start-line
     :column start-col
     :message (mapconcat 'identity (list tag overview details) "\n")
     :level (flycheck-elm-decode-type error))))

(defun flycheck-elm-decode-type (error)
  (let ((type (cdr (assoc 'type error))))
    (pcase type
      (`"warning" 'warning)
      (`"error" 'error)
      (_ 'unknown))))

(defun flycheck-elm-read-json (str)
  (condition-case nil
      (json-read-from-string str)
    (error nil)))

(defun flycheck-elm-parse-error-data (data)
  (let* ((json-array-type 'list)
         (mapdata (mapcar
                   'flycheck-elm-read-json
                   (split-string data "\n"))))
    (append (car mapdata) (car (cdr mapdata)))))

(defun flycheck-elm-parse-errors (output checker buffer)
  "Decode elm json output errors."
  (let* ((data (flycheck-elm-parse-error-data output))
         (errors (flycheck-elm-filter-by-preference data)))
    (mapcar
     (lambda (x) (flycheck-elm-decode-elm-error x checker buffer))
     errors)))

(defun flycheck-elm-filter-by-preference (lst &optional pref)
  "Filter the lst by user preference."
  (let ((errors (flycheck-elm-filter-by-type 'error lst)))
    (or pref (set 'pref flycheck-elm-reporting-mode))
    (pcase pref
      (`errors-only errors)
      (`warn-after-errors
       (pcase (length errors)
         (0 (flycheck-elm-filter-by-type 'warning lst))
         (_ errors)))
      (_  lst))))

(defun flycheck-elm-filter-by-type (type lst)
  "Return a new LIST of errors of type TYPE."
  (cl-remove-if-not
   (lambda (x)(equal (flycheck-elm-decode-type x) type))
   lst))

(defun flycheck-elm-package-json-directory (&optional checker)
  "Find the directory in which CHECKER should run \"elm-make\"."
  (locate-dominating-file default-directory "elm-package.json"))

(flycheck-def-option-var flycheck-elm-output-file nil elm
  "The output file to compile to when performing syntax checking.

The value of this variable is either nil, or a string with the
path to the desired compilation output file.

If nil, flycheck-elm will compile to `/dev/null' so as to not
interfere with your project files. Elm-make has special logic
to handle /dev/null, hence the use of /dev/null instead of `null-device' even
on Windows.
See commit: https://github.com/elm-lang/elm-make/commit/ddcd4980fac9127c91c1de373c310155de9fa558

If a string is provided, the flycheck-elm will compile your code
to the given file each time it performs syntax checking. This can
be set to any file with a .js or .html extension. Please note
that the contents of this file will be overwritten every time
flycheck-elm successfully compiles your Elm code."
  :type '(string))


(flycheck-def-option-var flycheck-elm-main-file nil elm
  "A main elm file for flycheck-elm to compile instead of individual files.

The value of this variable is either nil, in which case
flycheck-elm will compile individual files when checking them, or
a string with the path to the main elm file within your
project. The main elm file is the .elm file which contains a
\"main\" function, for example: \"Main.elm\")."
  :type '(string))

(flycheck-define-checker elm
  "A syntax checker for elm-mode using the json output from elm-make"
  :command ("elm-make" "--report=json"
            (eval (or flycheck-elm-main-file buffer-file-name))
            (eval (concat  "--output=" (or flycheck-elm-output-file "/dev/null"))))
  :error-parser flycheck-elm-parse-errors
  :working-directory flycheck-elm-package-json-directory
  :predicate flycheck-elm-package-json-directory
  :modes elm-mode)

;;;###autoload
(defun flycheck-elm-setup ()
  "Setup Flycheck elm."
  (interactive)
  (add-to-list 'flycheck-checkers 'elm))

(provide 'flycheck-elm)
;;; flycheck-elm.el ends here
