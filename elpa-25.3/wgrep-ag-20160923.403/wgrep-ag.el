;;; wgrep-ag.el --- Writable ag buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; Package-Version: 20160923.403
;; Package-Requires: ((wgrep "2.1.5") (cl-lib "0.5"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.9

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wgrep-ag allows you to edit a ag buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install ag.el
;;
;;   https://github.com/Wilfred/ag.el

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-ag-setup "wgrep-ag")
;;     (add-hook 'ag-mode-hook 'wgrep-ag-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)
(require 'cl-lib)

(defun wgrep-ag-prepare-header/footer ()
  (save-excursion
    (goto-char (point-min))
    (when (not (get-text-property (point) 'compilation-message))
      (let ((first-result (next-single-property-change (point)
                                                       'compilation-message)))
        ;; Maybe ag was run with --group?  Pedantry demands that I not
        ;; mark the first "File:" line as part of the wgrep-header.
        (when first-result
          (goto-char first-result)
          (when (and (zerop (forward-line -1))
                     (looking-at-p "^File: "))
             (setq first-result (point))))
        (add-text-properties (point-min) (or first-result (point-max))
                             '(read-only t wgrep-header t))))
    (goto-char (point-max))
    (forward-line 0)
    (when (not (or (get-text-property (point) 'compilation-message)
                   (get-text-property (point) 'wgrep-header)))
      (goto-char (previous-single-property-change (point) 'compilation-message))
      (forward-line 1)
      (add-text-properties (point) (point-max)
                           '(read-only t wgrep-footer t)))))

(defun wgrep-ag-parse-command-results ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop
       with last-file-name
       for ref-start = (point) then (next-single-property-change
                                     (point)
                                     'compilation-message)
       while ref-start
       do
         (goto-char ref-start)
         (let ((compile-msg (get-text-property (point)
                                               'compilation-message)))
           ;; This should always be true, but just in case:
           (when compile-msg
             (let ((ref-end (next-single-property-change (point)
                                                         'compilation-message)))
               (goto-char ref-end)
               ;; We expect to be able to find the end of this
               ;; compilation message before running into another one.
               (unless (get-text-property (point) 'compilation-message)
                 (let* ((loc (compilation--message->loc compile-msg))
                        (line-num (compilation--loc->line loc))
                        (file-name (car (compilation--file-struct->file-spec
                                         (compilation--loc->file-struct loc)))))
                   (unless (string= file-name last-file-name)
                     (put-text-property ref-start ref-end
                                        (wgrep-construct-filename-property
                                         file-name)
                                        file-name)
                     (setq last-file-name file-name))
                   (add-text-properties ref-start ref-end
                                        (list 'wgrep-line-filename
                                              file-name
                                              'wgrep-line-number
                                              line-num))))))))))

;;;###autoload
(defun wgrep-ag-setup ()
  (set (make-local-variable 'wgrep-header/footer-parser)
       'wgrep-ag-prepare-header/footer)
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-ag-parse-command-results)
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; For `unload-feature'
(defun wgrep-ag-unload-function ()
  (remove-hook 'ag-mode-hook 'wgrep-ag-setup))

(provide 'wgrep-ag)

;;; wgrep-ag.el ends here
