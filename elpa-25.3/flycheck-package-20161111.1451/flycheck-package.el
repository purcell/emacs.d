;;; flycheck-package.el --- A Flycheck checker for elisp package authors

;; Copyright (C) 2014-2016  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; Keywords: lisp
;; Package-Version: 20161111.1451
;; Version: 0
;; Package-Requires: ((flycheck "0.22") (package-lint "0.2"))

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

;;; Commentary:

;; Provides feedback via flycheck about issues with the package metadata
;; of a file, e.g. the package dependencies it requires.

;; To enable, use something like this:

;;    (eval-after-load 'flycheck
;;      '(flycheck-package-setup))

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(require 'flycheck)
(require 'package-lint)

(defun flycheck-package--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (funcall callback
           'finished
           (mapcar (lambda (x)
                     (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
                   (condition-case err
                       (when (package-lint-looks-like-a-package-p)
                         (package-lint-buffer (current-buffer)))
                     (error
                      (funcall callback 'errored (error-message-string err))
                      (signal (car err) (cdr err)))))))


;;; Checker definition

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers and other packaging issues."
  :start #'flycheck-package--start
  :modes '(emacs-lisp-mode))

;;;###autoload
(defun flycheck-package-setup ()
  "Setup flycheck-package.
Add `flycheck-emacs-lisp-package' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'emacs-lisp-package t))

(provide 'flycheck-package)
;;; flycheck-package.el ends here
