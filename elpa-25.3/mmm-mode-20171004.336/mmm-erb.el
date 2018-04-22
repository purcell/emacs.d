;;; mmm-erb.el --- ERB templates editing support

;; Copyright (C) 2012, 2013 by Dmitry Gutov

;; Author: Dmitry Gutov <dgutov@yandex.ru>

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains definitions of ERB and EJS submode classes, and well as
;; support functions for proper indentation.

;; Usage:

;; (require 'mmm-auto)

;; (setq mmm-global-mode 'auto)

;; (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
;; (mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
;; (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)

;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
;; (add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

;; Optional settings:

;; (setq mmm-submode-decoration-level 2
;;       mmm-parse-when-idle t)

;; nXML as primary mode (supports only JS and CSS subregions):

;; (mmm-add-mode-ext-class 'nxml-web-mode nil 'html-js)
;; (mmm-add-mode-ext-class 'nxml-web-mode nil 'html-css)

;; (add-to-list 'auto-mode-alist '("\\.xhtml\\'" . nxml-web-mode))

;;; Code:

(require 'sgml-mode)
(eval-when-compile (require 'cl))
(require 'mmm-vars)
(require 'mmm-region)

(mmm-add-classes
 '((erb :submode ruby-mode :front "<%[#=]?" :back "-?%>"
        :match-face (("<%#" . mmm-comment-submode-face)
                     ("<%=" . mmm-output-submode-face)
                     ("<%" . mmm-code-submode-face))
        :insert ((?% erb-code nil @ "<%" @ " " _ " " @ "%>" @)
                 (?# erb-comment nil @ "<%#" @ " " _ " " @ "%>" @)
                 (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
        :creation-hook mmm-erb-mark-as-special)
   (ejs :submode js-mode :front "<%[#=]?" :back "-?%>"
        :match-face (("<%#" . mmm-comment-submode-face)
                     ("<%=" . mmm-output-submode-face)
                     ("<%" . mmm-code-submode-face))
        :insert ((?% ejs-code nil @ "<%" @ " " _ " " @ "%>" @)
                 (?# ejs-comment nil @ "<%#" @ " " _ " " @ "%>" @)
                 (?= ejs-expression nil @ "<%=" @ " " _ " " @ "%>" @))
        :creation-hook mmm-erb-mark-as-special)))

(defun mmm-erb-mark-as-special ()
  "Hook function to run in ERB and EJS tag regions."
  (overlay-put mmm-current-overlay 'mmm-special-tag t))

;;;###autoload
(define-derived-mode html-erb-mode html-mode "ERB-HTML"
  (setq sgml-unclosed-tags nil) ; Simplifies indentation logic.
  (set (make-local-variable 'mmm-indent-line-function) 'mmm-erb-indent-line)
  (add-hook 'mmm-after-syntax-propertize-functions
            'html-erb-after-syntax-propertize nil t))

(defun html-erb-after-syntax-propertize (overlay mode beg end)
  (when overlay
    (with-silent-modifications
      (funcall
       (syntax-propertize-rules ("<\\|>" (0 ".")))
       beg end))))

(defun mmm-erb-indent-line ()
  "Indent the current line intelligently."
  (interactive)
  (let ((offset (- (current-column) (current-indentation))))
    (back-to-indentation)
    (mmm-update-submode-region)
    (if (and mmm-current-overlay mmm-current-submode
             (< (overlay-start mmm-current-overlay) (point-at-bol)))
        ;; Region starts before the current line (and contains indentation).
        ;; If it starts on the current line, then either first part of the line
        ;; is in primary mode, or we're on the first line of a script or style
        ;; tag contents. In the latter case, better to also indent it according
        ;; to the primary mode (as text): `js-indent-line' ignores narrowing,
        ;; gets confused by the angle bracket on the previous line and thus
        ;; breaks our "top level" heuristic.
        (mmm-erb-indent-line-submode)
      (mmm-erb-indent-line-primary))
    (when (> offset 0) (forward-char offset))))

(defun mmm-erb-indent-line-submode ()
  "Indent line within a submode."
  (let (added-whitespace)
    (if (<= (overlay-end mmm-current-overlay)
            (save-excursion (back-to-indentation) (point)))
        ;; We're at a closing tag.
        (mmm-erb-indent-to-region-start)
      (save-restriction
        (save-excursion
          (goto-char (overlay-start mmm-current-overlay))
          (when (not (looking-at "^\\|\\s-*$"))
            ;; Submode region has text on the same line as the opening tag,
            ;; pad it with whitespace to make the following lines line up.
            (setq added-whitespace (current-column))
            (insert-char ?\s added-whitespace)))
        (narrow-to-region (overlay-start mmm-current-overlay)
                          (overlay-end mmm-current-overlay))
        (funcall (mmm-erb-orig-indent-function mmm-current-submode))
        (when added-whitespace
          ;; Remove the padding.
          (save-excursion
            (goto-char (overlay-start mmm-current-overlay))
            (delete-char added-whitespace))))
      ;; If submode indent function moved us to bol,
      ;; we're on the top level, indent according to the primary mode.
      (when (zerop (current-indentation))
        (mmm-erb-indent-to-region-start
         (mmm-erb-indent-offset mmm-primary-mode))))))

(defun mmm-erb-indent-to-region-start (&optional additional-offset)
  "Indent line to match start of region, possibly adding ADDITIONAL-OFFSET."
  (let ((indent (current-indentation)))
    (indent-line-to
     (save-excursion
       (goto-char (1- (overlay-start mmm-current-overlay)))
       (+ (current-indentation)
          (or additional-offset 0))))))

(defun mmm-erb-indent-line-primary ()
  "Indent line in primary mode."
  (let* ((here (point))
         ;; Go before previous line's tag.
         (start (progn (forward-line -1)
                       (back-to-indentation)
                       (let ((lcon (sgml-lexical-context)))
                         (when (eq (car lcon) 'tag)
                           ;; Tag spreads several lines.
                           (goto-char (cdr lcon))
                           (back-to-indentation)))
                       (point)))
         (regions (mmm-regions-in start here))
         (n 0))
    ;; Collect indent modifier depending on type of tags.
    (loop for region in regions
          for type = (mmm-erb-scan-region region)
          when type do
          (if (eq type 'close)
              (when (plusp n) (decf n))
            (incf n (if (eq type 'close) 0 1))))
    (let ((eol (progn (goto-char here) (end-of-line 1) (point))))
      ;; Look for "else" and "end" instructions to adjust modifier.
      ;; If a block start instruction comes first, abort.
      (loop for region in (mmm-regions-in here eol)
            for type = (mmm-erb-scan-region region)
            until (eq type 'open)
            when (memq type '(middle close)) do (decf n)))
    (goto-char here)
    (funcall (mmm-erb-orig-indent-function mmm-primary-mode))
    (let* ((indent (current-indentation))
           (indent-step (mmm-erb-indent-offset mmm-primary-mode)))
      (indent-line-to (+ indent (if n (* indent-step n) 0))))))

(defun mmm-erb-scan-region (region)
  (when region ; Can be nil if a line is empty, for example.
    (destructuring-bind (submode beg end ovl) region
      (let ((scan-fn (plist-get '(ruby-mode mmm-erb-scan-erb
                                  js-mode   mmm-erb-scan-ejs)
                                submode)))
        (and scan-fn
             (overlay-get ovl 'mmm-special-tag)
             (save-excursion
               (goto-char beg)
               (skip-syntax-forward "-")
               (funcall scan-fn end)))))))

(defconst mmm-erb-ruby-close-re "\\<end\\>\\|}"
  "Regexp to match the end of a Ruby block.")

(defun mmm-erb-scan-erb (limit)
  (cond ((looking-at "\\(?:if\\|unless\\|for\\|while\\)\\b") 'open)
        ((looking-at "\\(?:else\\|elsif\\)\\b") 'middle)
        ((looking-at mmm-erb-ruby-close-re) 'close)
        ((and (re-search-forward (concat "\\(?: +do +\\| *{ *\\)"
                                         "\\(?:|[A-Za-z0-9_, ]*|\\)? *")
                                 limit t)
              (let ((pt (point)))
                (not (when (< pt limit)
                       (goto-char limit)
                       (skip-syntax-backward "-")
                       (looking-back mmm-erb-ruby-close-re pt)))))
         'open)))

(defun mmm-erb-scan-ejs (limit)
  (cond ((looking-at "\\(?:if\\|for\\|while\\)\\b") 'open)
        ((looking-at "} *else\\b") 'middle)
        ((looking-at "}") 'close)
        ((re-search-forward " *{ *" limit t) 'open)))

(defun mmm-erb-orig-indent-function (mode)
  (get mode 'mmm-indent-line-function))

(defvar mmm-erb-offset-var-alist
  '((html-erb-mode . sgml-basic-offset)
    (nxml-web-mode . nxml-child-indent)))

(defun mmm-erb-indent-offset (mode)
  (let ((name (cdr (assoc mode mmm-erb-offset-var-alist))))
    (when name (symbol-value name))))

;;;###autoload
(define-derived-mode nxml-web-mode nxml-mode "nXML-Web"
  (set (make-local-variable 'mmm-indent-line-function) 'mmm-erb-indent-line))

(provide 'mmm-erb)

;;; mmm-erb.el ends here
