;;; wgrep-helm.el --- Writable helm-grep-mode buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.1.1"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-helm.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.5

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

;; wgrep-helm allows you to edit a helm-grep-mode buffer and apply those
;; changes to the file buffer.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep-helm)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

(declare-function helm-grep-split-line "helm-grep")

;;;###autoload
(defun wgrep-helm-setup ()
  (set (make-local-variable 'wgrep-header/footer-parser)
       'wgrep-helm-prepare-header/footer)
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-helm-parse-command-results)
  (wgrep-setup-internal))

(defun wgrep-helm-prepare-header/footer ()
  (let (beg end)
    ;; Set read-only grep result header
    (setq beg (point-min))
    (setq end (next-single-property-change
               (point-min) 'helm-realvalue))
    (put-text-property beg end 'read-only t)
    (put-text-property beg end 'wgrep-header t)
    ;; helm-grep-mode have NO footer.
    ))

(defun wgrep-helm-parse-command-results ()
  (while (not (eobp))
    (when (looking-at wgrep-line-file-regexp)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (dispname (match-string 1))
             (namelen (length dispname)))
        (let* ((value (get-text-property (point) 'helm-realvalue))
               (data  (helm-grep-split-line value))
               (fn    (or (get-text-property (point) 'buffer-name)
                          (get-text-property (point) 'helm-grep-fname)))
               (line  (string-to-number (nth 1 data)))
               (fprop (wgrep-construct-filename-property fn)))
          (put-text-property start end 'wgrep-line-filename fn)
          (put-text-property start end 'wgrep-line-number line)
          (put-text-property start (+ start namelen) fprop fn))))
    (forward-line 1)))

;;;###autoload
(add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)

;;;###autoload
(add-hook 'helm-moccur-mode-hook 'wgrep-helm-setup)

;; For `unload-feature'
(defun wgrep-helm-unload-function ()
  (remove-hook 'helm-grep-mode-hook 'wgrep-helm-setup)
  (remove-hook 'helm-moccur-mode-hook 'wgrep-helm-setup))

(provide 'wgrep-helm)

;;; wgrep-helm.el ends here
