;;; skewer-less.el --- Skewer support for live LESS stylesheet updates

;; Copyright (C) 2013-2016  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages, tools
;; Package-Version: 20160828.1321
;; Version: 0
;; Package-Requires: ((skewer-mode "1.5.3"))

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

;; Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

;; Note that this is intended for use in place of `skewer-css-mode',
;; which does not work with lesscss.

;; Enable `skewer-less-mode' in a ".less" buffer.  Hit "C-c C-k" just
;; like in `skewer-css-mode'.  To reload the buffer when you save it,
;; use code like the following:

;; (add-hook 'skewer-less-mode
;;           (lambda ()
;;             (add-hook 'after-save-hook 'skewer-less-eval-buffer nil t)))

;;; Code:

(require 'skewer-css)

(defvar skewer-less-mode-map
  (let ((m (make-sparse-keymap)))
    ;; for consistency with skewer-css
    (define-key m (kbd "C-c C-k") 'skewer-less-eval-buffer)
    m)
  "Keymap for `skewer-less-mode'.")

;;;###autoload
(define-minor-mode skewer-less-mode
  "Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

For this to work properly, the lessc command must be available on
`exec-path', and `skewer' must be running."
  nil
  " skewer-less"
  skewer-less-mode-map)

;;;###autoload
(defun skewer-less-eval-buffer ()
  "When skewer appears to be active, ask for a reload."
  (interactive)
  (skewer-less-eval-region (point-min) (point-max)))

;;;###autoload
(defun skewer-less-eval-region (beg end)
  "Process the region from BEG to END with \"lessc\", and pass it to `skewer-css'."
  (interactive "r")
  (let ((cssbuf "*skewer-less-output*")
        (errbuf "*skewer-less-errors*"))
    (if (save-window-excursion (equal 0 (shell-command-on-region beg end "lessc -" cssbuf nil errbuf)))
        (with-current-buffer cssbuf
          (skewer-css (buffer-substring-no-properties (point-min) (point-max)))
          (message "lessc output sent.")
          ;; Make the output nice to look at
          (css-mode))
      (with-current-buffer errbuf
        ;; Append STDOUT contents
        (goto-char (point-max))
        (insert-buffer cssbuf))
      (display-buffer errbuf))))


(provide 'skewer-less)
;;; skewer-less.el ends here
