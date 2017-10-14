;;; nxml-mode-expansions.el --- Nxml-specific expansions for expand-region

;; Copyright (C) 2012 Ivan Andrus

;; Author: Ivan Andrus
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; Feel free to contribute any other expansions for Nxml at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'cl-lib)
(require 'expand-region-core)
(require 'html-mode-expansions)
(require 'nxml-mode)

(defun er/mark-nxml-tag ()
  "Marks one nxml element e.g. <p>"
  (interactive)
  (cond ((looking-at "<")
         (nxml-mark-token-after))
        ((er/looking-back-exact ">")
         (backward-char 1)
         (nxml-mark-token-after))
        ((er/looking-back-max "<[^<>]*" 1000)
         (nxml-mark-token-after))))

(defun er/mark-nxml-element ()
  "Marks one nxml element e.g. <p>...</p>"
  (interactive)
  (if (not (looking-at "<[^/]"))
      (er/mark-nxml-containing-element)
    (set-mark (point))
    (nxml-forward-element)
    (exchange-point-and-mark)))

(defun er/mark-nxml-containing-element ()
  "Marks one nxml element, but always e.g. <p>...</p>"
  (interactive)
  (nxml-up-element)
  (set-mark (point))
  (nxml-backward-element))

(defun er/mark-nxml-inside-element ()
  "Marks the inside Nxml statement, eg. <p>...</p>"
  (interactive)
  (let ((nxml-sexp-element-flag nil))
    (nxml-up-element)
    (nxml-forward-balanced-item -1)
    (set-mark (point))
    (nxml-backward-up-element)
    (nxml-forward-balanced-item 1)))

(defun er/inside-nxml-attribute-string? ()
  "Returns the attribute from `xmltok-attributes' array that
point is in, or otherwise nil"
  (save-excursion 
    (forward-char 1)
    (nxml-token-before))
  (cl-find-if (lambda (att)
		(and (<= (xmltok-attribute-value-start att) (point))
		     (>= (xmltok-attribute-value-end att) (point))))
	      xmltok-attributes))

(defun er/mark-nxml-attribute-inner-string ()
  "Marks an attribute string"
  (interactive)
  (let ((attr (er/inside-nxml-attribute-string?)))
    (when attr
      (set-mark (xmltok-attribute-value-start attr))
      (goto-char (xmltok-attribute-value-end attr))
      (exchange-point-and-mark))))

(defun er/mark-nxml-attribute-string ()
  "Marks an attribute string inside quotes."
  (interactive)
  (let ((attr (er/inside-nxml-attribute-string?)))
    (when attr      
      (set-mark (1- (xmltok-attribute-value-start attr)))
      (goto-char (1+ (xmltok-attribute-value-end attr)))
      (exchange-point-and-mark))))

(defun er/add-nxml-mode-expansions ()
  "Adds Nxml-specific expansions for buffers in nxml-mode"
  (interactive)
  (set (make-local-variable 'er/try-expand-list)
       (append
        '(nxml-mark-paragraph
          ;; nxml-mark-token-after ;; Marks the current tag, etc.  It's a bit schizophrenic
          er/mark-nxml-tag
          er/mark-nxml-inside-element
          er/mark-nxml-element
          er/mark-nxml-containing-element
          er/mark-nxml-attribute-string
          er/mark-nxml-attribute-inner-string
          ;; Steal from html-mode-expansions
          er/mark-html-attribute)
        ;; some normal marks are more hindrance than help:
        (remove 'er/mark-method-call
                (remove 'er/mark-symbol-with-prefix
                        (remove 'er/mark-symbol er/try-expand-list))))))

(er/enable-mode-expansions 'nxml-mode 'er/add-nxml-mode-expansions)

(provide 'nxml-mode-expansions)

;; nxml-mode-expansions.el ends here
