;;; json-mode.el --- Major mode for editing JSON files.

;; Copyright (C) 2011-2014 Josh Johnston

;; Author: Josh Johnston
;; URL: https://github.com/joshwnj/json-mode
;; Package-Version: 20170719.2205
;; Version: 1.6.0
;; Package-Requires: ((json-reformat "0.0.5") (json-snatcher "1.0.0"))

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

;; extend the builtin js-mode's syntax highlighting

;;; Code:

(require 'js)
(require 'rx)
(require 'json-snatcher)
(require 'json-reformat)

(defgroup json-mode '()
  "Major mode for editing JSON files."
  :group 'js)

;;;###autoload
(defconst json-mode-standard-file-ext '(".json" ".jsonld")
  "List of JSON file extensions.")

;; This is to be sure the customization is loaded.  Otherwise,
;; autoload discards any defun or defcustom.
;;;###autoload
(defsubst json-mode--update-auto-mode (filenames)
  "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
  (let* ((new-regexp
          (rx-to-string
           `(seq (eval
                  (cons 'or
                        (append json-mode-standard-file-ext
                                ',filenames))) eot)))
         (new-entry (cons new-regexp 'json-mode))
         (old-entry (when (boundp 'json-mode--auto-mode-entry)
                      json-mode--auto-mode-entry)))
    (setq auto-mode-alist (delete old-entry auto-mode-alist))
    (add-to-list 'auto-mode-alist new-entry)
    new-entry))

;;;###autoload
(defcustom json-mode-auto-mode-list '(
                                      ".babelrc"
                                      ".bowerrc"
                                      "composer.lock"
                                      )
  "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
  :group 'json-mode
  :type '(repeat string)
  :set (lambda (symbol value)
         "Update SYMBOL with a new regexp made from VALUE.

This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
         (set-default symbol value)
         (setq json-mode--auto-mode-entry (json-mode--update-auto-mode value))))

;; Autoload needed to initalize the the `auto-list-mode' entry.
;;;###autoload
(defvar json-mode--auto-mode-entry (json-mode--update-auto-mode json-mode-auto-mode-list)
  "Regexp generated from the `json-mode-auto-mode-list'.")

(defconst json-mode-quoted-string-re
  (rx (group (char ?\")
             (zero-or-more (or (seq ?\\ ?\\)
                               (seq ?\\ ?\")
                               (seq ?\\ (not (any ?\" ?\\)))
                               (not (any ?\" ?\\))))
             (char ?\"))))
(defconst json-mode-quoted-key-re
  (rx (group (char ?\")
             (zero-or-more (or (seq ?\\ ?\\)
                               (seq ?\\ ?\")
                               (seq ?\\ (not (any ?\" ?\\)))
                               (not (any ?\" ?\\))))
             (char ?\"))
      (zero-or-more blank)
      ?\:))
(defconst json-mode-number-re (rx (group (one-or-more digit)
                                         (optional ?\. (one-or-more digit)))))
(defconst json-mode-keyword-re  (rx (group (or "true" "false" "null"))))

(defconst json-font-lock-keywords-1
  (list
   (list json-mode-quoted-key-re 1 font-lock-keyword-face)
   (list json-mode-quoted-string-re 1 font-lock-string-face)
   (list json-mode-keyword-re 1 font-lock-constant-face)
   (list json-mode-number-re 1 font-lock-constant-face)
   )
  "Level one font lock.")

;;;###autoload
(define-derived-mode json-mode javascript-mode "JSON"
  "Major mode for editing JSON files"
  (set (make-local-variable 'font-lock-defaults) '(json-font-lock-keywords-1 t)))

;; Well formatted JSON files almost always begin with “{” or “[”.
(add-to-list 'magic-mode-alist '("^[{[]$" . json-mode))

;;;###autoload
(defun json-mode-show-path ()
  (interactive)
  (let ((temp-name "*json-path*"))
    (with-output-to-temp-buffer temp-name (jsons-print-path))

    (let ((temp-window (get-buffer-window temp-name)))
      ;; delete the window if we have one,
      ;; so we can recreate it in the correct position
      (if temp-window
          (delete-window temp-window))

      ;; always put the temp window below the json window
      (set-window-buffer (if (fboundp 'split-window-below)
                             (split-window-below)
                           (split-window-vertically)) temp-name))
    ))

(define-key json-mode-map (kbd "C-c C-p") 'json-mode-show-path)

;;;###autoload
(defun json-mode-kill-path ()
  (interactive)
    (kill-new (jsons-print-path)))

(define-key json-mode-map (kbd "C-c P") 'json-mode-kill-path)

;;;###autoload
(defun json-mode-beautify ()
  "Beautify / pretty-print the active region (or the entire buffer if no active region)."
  (interactive)
  (let ((json-reformat:indent-width js-indent-level)
        (json-reformat:pretty-string? t))
    (if (use-region-p)
        (json-reformat-region (region-beginning) (region-end))
      (json-reformat-region (buffer-end -1) (buffer-end 1)))))

(define-key json-mode-map (kbd "C-c C-f") 'json-mode-beautify)

(defun json-toggle-boolean ()
  "If point is on `true' or `false', toggle it."
  (interactive)
  (unless (nth 8 (syntax-ppss)) ; inside a keyword, string or comment
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (string (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
           (pt (point)))
      (when (and bounds (member string '("true" "false")))
        (delete-region (car bounds) (cdr bounds))
        (cond
         ((string= "true" string)
          (insert "false")
          (goto-char (if (= pt (cdr bounds)) (1+ pt) pt)))
         (t
          (insert "true")
          (goto-char (if (= pt (cdr bounds)) (1- pt) pt))))))))

(define-key json-mode-map (kbd "C-c C-t") 'json-toggle-boolean)

(defun json-nullify-sexp ()
  "Replace the sexp at point with `null'."
  (interactive)
  (let ((syntax (syntax-ppss)) symbol)
    (cond
     ((nth 4 syntax) nil)               ; inside a comment
     ((nth 3 syntax)                    ; inside a string
      (goto-char (nth 8 syntax))
      (when (save-excursion (forward-sexp) (skip-chars-forward "[:space:]") (eq (char-after) ?:))
        ;; sexp is an object key, so we nullify the entire object
        (goto-char (nth 1 syntax)))
      (kill-sexp)
      (insert "null"))
     ((setq symbol (bounds-of-thing-at-point 'symbol))
      (cond
       ((looking-at-p "null"))
       ((save-excursion (skip-chars-backward "[0-9.]") (looking-at json-mode-number-re))
        (kill-region (match-beginning 0) (match-end 0))
        (insert "null"))
       (t (kill-region (car symbol) (cdr symbol)) (insert "null"))))
     ((< 0 (nth 0 syntax))
      (goto-char (nth 1 syntax))
      (kill-sexp)
      (insert "null"))
     (t nil))))

(define-key json-mode-map (kbd "C-c C-k") 'json-nullify-sexp)

(defun json-increment-number-at-point (&optional delta)
  "Add DELTA to the number at point; DELTA defaults to 1."
  (interactive)
  (when (save-excursion (skip-chars-backward "[0-9.]") (looking-at json-mode-number-re))
    (let ((num (+ (or delta 1)
                  (string-to-number (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
          (pt (point)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (number-to-string num))
      (goto-char pt))))

(define-key json-mode-map (kbd "C-c C-i") 'json-increment-number-at-point)

(defun json-decrement-number-at-point ()
  "Decrement the number at point."
  (interactive)
  (json-increment-number-at-point -1))

(define-key json-mode-map (kbd "C-c C-d") 'json-decrement-number-at-point)

(provide 'json-mode)
;;; json-mode.el ends here
