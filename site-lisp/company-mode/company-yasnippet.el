;;; company-yasnippet.el --- company-mode completion back-end for Yasnippet

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Dmitry Gutov

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
;;

;;; Code:

(require 'yasnippet)

(defun company-yasnippet--candidates (prefix)
  (mapcan
   (lambda (table)
     (let ((keyhash (yas--table-hash table))
           res)
       (when keyhash
         (maphash
          (lambda (key value)
            (when (and (stringp key)
                       (string-prefix-p prefix key))
              (maphash
               (lambda (name template)
                 (push
                  (propertize key
                              'yas-annotation name
                              'yas-template template)
                  res))
               value)))
          keyhash))
       res))
   (yas--get-snippet-tables)))

;;;###autoload
(defun company-yasnippet (command &optional arg &rest ignore)
  "`company-mode' back-end for `yasnippet'.

This back-end should be used with care, because as long as there are
snippets defined for the current major mode, this back-end will always
shadow back-ends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a back-end or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other back-ends.

  (push '(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet)
"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-yasnippet))
    (prefix
     ;; Should probably use `yas--current-key', but that's bound to be slower.
     ;; How many trigger keys start with non-symbol characters anyway?
     (and yas-minor-mode
          (company-grab-symbol)))
    (annotation (concat " -> " (get-text-property 0 'yas-annotation arg)))
    (candidates (company-yasnippet--candidates arg))
    (post-completion
     (let ((template (get-text-property 0 'yas-template arg)))
       (yas-expand-snippet (yas--template-content template)
                           (- (point) (length arg))
                           (point)
                           (yas--template-expand-env template))))))

(provide 'company-yasnippet)
;;; company-yasnippet.el ends here
