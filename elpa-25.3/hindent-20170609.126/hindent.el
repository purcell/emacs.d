;;; hindent.el --- Indent haskell code using the "hindent" program

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/hindent
;; Package-Version: 20170609.126
;; Package-Requires: ((cl-lib "0.5"))

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

;; Provides a minor mode and commands for easily using the "hindent"
;; program to reformat Haskell code.

;; Add `hindent-mode' to your `haskell-mode-hook' and use the provided
;; keybindings as needed.  Set `hindent-reformat-buffer-on-save' to
;; `t' globally or in local variables to have your code automatically
;; reformatted.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization properties

(defgroup hindent nil
  "Integration with the \"hindent\" reformatting program."
  :prefix "hindent-"
  :group 'haskell)

(defcustom hindent-style
  nil
  "The style to use for formatting.

For hindent versions lower than 5, you must set this to a non-nil string."
  :group 'hindent
  :type 'string
  :safe #'stringp)

(make-obsolete-variable 'hindent-style nil "hindent 5")


(defcustom hindent-process-path
  "hindent"
  "Location where the hindent executable is located."
  :group 'hindent
  :type 'string
  :safe #'stringp)

(defcustom hindent-reformat-buffer-on-save nil
  "Set to t to run `hindent-reformat-buffer' when a buffer in `hindent-mode' is saved."
  :group 'hindent
  :type 'boolean
  :safe #'booleanp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode

(defvar hindent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap indent-region] #'hindent-reformat-region)
    (define-key map [remap fill-paragraph] #'hindent-reformat-decl-or-fill)
    map)
  "Keymap for `hindent-mode'.")

;;;###autoload
(define-minor-mode hindent-mode
  "Indent code with the hindent program.

Provide the following keybindings:

\\{hindent-mode-map}"
  :init-value nil
  :keymap hindent-mode-map
  :lighter " HI"
  :group 'hindent
  :require 'hindent
  (if hindent-mode
      (add-hook 'before-save-hook 'hindent--before-save nil t)
    (remove-hook 'before-save-hook 'hindent--before-save t)))

(defun hindent--before-save ()
  "Optionally reformat the buffer on save."
  (when hindent-reformat-buffer-on-save
    (hindent-reformat-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

;;;###autoload
(defun hindent-reformat-decl ()
  "Re-format the current declaration.

The declaration is parsed and pretty printed.  Comments are
preserved, although placement may be funky."
  (interactive)
  (let ((start-end (hindent-decl-points)))
    (when start-end
      (let ((beg (car start-end))
            (end (cdr start-end)))
        (hindent-reformat-region beg end t)))))

;;;###autoload
(defun hindent-reformat-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (hindent-reformat-region (point-min)
                           (point-max)))

;;;###autoload
(defun hindent-reformat-decl-or-fill (justify)
  "Re-format current declaration, or fill paragraph.

Fill paragraph if in a comment, otherwise reformat the current
declaration.  When filling, the prefix argument JUSTIFY will
cause the text to be justified, as per `fill-paragraph'."
  (interactive (progn
                 ;; Copied from `fill-paragraph'
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'full))))
  (if (hindent-in-comment)
      (fill-paragraph justify t)
    (hindent-reformat-decl)))

;;;###autoload
(defun hindent-reformat-region (beg end &optional drop-newline)
  "Reformat the region from BEG to END, accounting for indentation.

If DROP-NEWLINE is non-nil, don't require a newline at the end of
the file."
  (interactive "r")
  (if (= (save-excursion (goto-char beg)
                         (line-beginning-position))
         beg)
      (hindent-reformat-region-as-is beg end drop-newline)
    (let* ((column (- beg (line-beginning-position)))
           (string (buffer-substring-no-properties beg end))
           (new-string (with-temp-buffer
                         (insert (make-string column ? ) string)
                         (hindent-reformat-region-as-is (point-min)
                                                        (point-max)
                                                        drop-newline)
                         (delete-region (point-min) (1+ column))
                         (buffer-substring (point-min)
                                           (point-max)))))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert new-string)))))

;;;###autoload
(define-obsolete-function-alias 'hindent/reformat-decl 'hindent-reformat-decl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal library

(defun hindent-reformat-region-as-is (beg end &optional drop-newline)
  "Reformat the given region from BEG to END as-is.

This is the place where hindent is actually called.

If DROP-NEWLINE is non-nil, don't require a newline at the end of
the file."
  (let* ((original (current-buffer))
         (orig-str (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (let ((temp (current-buffer)))
        (with-current-buffer original
          (let ((ret (apply #'call-process-region
                            (append (list beg
                                          end
                                          hindent-process-path
                                          nil ; delete
                                          temp ; output
                                          nil)
                                    (hindent-extra-arguments)))))
            (cond
             ((= ret 1)
              (let ((error-string
                     (with-current-buffer temp
                       (let ((string (progn (goto-char (point-min))
                                            (buffer-substring (line-beginning-position)
                                                              (line-end-position)))))
                         string))))
                (if (string= error-string "hindent: Parse error: EOF")
                    (message "language pragma")
                  (error error-string))))
             ((= ret 0)
              (let* ((last-decl (= end (point-max)))
                     (new-str (with-current-buffer temp
                                (when (and drop-newline (not last-decl))
                                  (goto-char (point-max))
                                  (when (looking-back "\n" (1- (point)))
                                    (delete-char -1)))
                                (buffer-string))))
                (if (not (string= new-str orig-str))
                    (let ((line (line-number-at-pos))
                          (col (current-column)))
                      (delete-region beg
                                     end)
                      (let ((new-start (point)))
                        (insert new-str)
                        (let ((new-end (point)))
                          (goto-char (point-min))
                          (forward-line (1- line))
                          (goto-char (+ (line-beginning-position) col))
                          (when (looking-back "^[ ]+" (line-beginning-position))
                            (back-to-indentation))
                          (delete-trailing-whitespace new-start new-end)))
                      (message "Formatted."))
                  (message "Already formatted.")))))))))))

(defun hindent-decl-points ()
  "Get the start and end position of the current declaration.

This assumes that declarations start at column zero and that the
rest is always indented by one space afterwards, so Template
Haskell uses with it all being at column zero are not expected to
work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (hindent-in-comment)
         (save-excursion (goto-char (line-beginning-position))
                         (hindent-in-comment)))
    nil)
   ((save-excursion
      (goto-char (line-beginning-position))
      (or (looking-at "^-}$")
          (looking-at "^{-$")))
    nil)
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start
             (or (cl-letf
                     (((symbol-function 'jump)
                       #'(lambda ()
                           (search-backward-regexp "^[^ \n]" nil t 1)
                           (cond
                            ((save-excursion (goto-char (line-beginning-position))
                                             (looking-at "|]"))
                             (jump))
                            (t (unless (or (looking-at "^-}$")
                                           (looking-at "^{-$"))
                                 (point)))))))
                   (goto-char (line-end-position))
                   (jump))
                 0))
            (end
             (progn
               (goto-char (1+ (point)))
               (or (cl-letf
                       (((symbol-function 'jump)
                         #'(lambda ()
                             (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                               (cond
                                ((save-excursion (goto-char (line-beginning-position))
                                                 (looking-at "|]"))
                                 (jump))
                                (t (forward-char -1)
                                   (search-backward-regexp "[^\n ]" nil t)
                                   (forward-char)
                                   (point)))))))
                     (jump))
                   (point-max)))))
        (cons start end))))))

(defun hindent-in-comment ()
  "Are we currently in a comment?"
  (save-excursion
    (when (and (= (line-end-position)
                  (point))
               (/= (line-beginning-position) (point)))
      (forward-char -1))
    (and
     (elt (syntax-ppss) 4)
     ;; Pragmas {-# SPECIALIZE .. #-} etc are not to be treated as
     ;; comments, even though they are highlighted as such
     (not (save-excursion (goto-char (line-beginning-position))
                          (looking-at "{-# "))))))

(defun hindent-extra-arguments ()
  "Extra command line arguments for the hindent invocation."
  (append
   (when (boundp 'haskell-language-extensions)
     haskell-language-extensions)
   (when hindent-style
     (list "--style" hindent-style))))

(provide 'hindent)

;;; hindent.el ends here
