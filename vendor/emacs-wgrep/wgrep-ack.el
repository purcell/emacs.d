;;; wgrep-ack.el --- Writable ack-and-a-half buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Contributor: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.1.1"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ack.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.3

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

;; wgrep-ack allows you to edit a ack-and-a-half buffer and apply those
;; changes to the file buffer.

;;; Install:

;; 1. Install ack-and-a-half.el
;;
;;   https://github.com/jhelwig/ack-and-a-half

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep-ack)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;;;###autoload
(defun wgrep-ack-and-a-half-setup ()
  ;; ack-and-a-half-mode prints a column number too, so we catch that
  ;; if it exists.  Here \2 is a colon + whitespace separator.  This
  ;; might need to change if (caar grep-regexp-alist) does.
  (set (make-local-variable 'wgrep-line-file-regexp)
       (concat
        wgrep-default-line-header-regexp
        "\\(?:\\([1-9][0-9]*\\)\\2\\)?"))
  (wgrep-setup-internal))

;;;###autoload
(defun wgrep-ack-setup ()
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-ack-prepare-command-results)
  (wgrep-setup-internal))

(defun wgrep-ack-prepare-command-results ()
  (let (fprop fn)
    (while (not (eobp))
      (cond
       ((null fn)
        ;; index of filename
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (when (/= bol eol)
            (setq fn (buffer-substring-no-properties bol eol))
            (setq fprop (wgrep-construct-filename-property fn))
            (put-text-property bol eol fprop fn)
            (put-text-property bol eol 'wgrep-ignore t))))
       ((looking-at "^\\([0-9]+\\)[:-]")
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (line (string-to-number (match-string 1))))
          (put-text-property start end 'wgrep-line-filename fn)
          (put-text-property start end 'wgrep-line-number line)))
       ((looking-at "^$")
        (setq fn nil)))
      (forward-line 1))))

;;;###autoload
(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)

;;;###autoload
(add-hook 'ack-mode-hook 'wgrep-ack-setup)

;; For `unload-feature'
(defun wgrep-ack-unload-function ()
  (remove-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)
  (remove-hook 'ack-mode-hook 'wgrep-ack-setup))

(provide 'wgrep-ack)

;;; wgrep-ack.el ends here
