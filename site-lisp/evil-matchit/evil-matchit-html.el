;;; evil-matchit-html.el ---html plugin of evil-matchit

;; Copyright (C) 2014  Chen Bin <chenbin.sh@gmail.com>

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'evil-matchit)

(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

;;;###autoload
(defun evilmi-html-get-tag ()
  (let ((b (line-beginning-position))
        (e (line-end-position))
        (html-tag-char (string-to-char "<"))
        (char (following-char))
        (p (point))
        (found_tag -1)
        (rlt nil)
        )

    (save-excursion
      ;; search backward
      (if (not (= char html-tag-char))
          (while (and (<= b (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (backward-char)
            )
        )
      ;; search forward
      (if (not (= char html-tag-char))
          (save-excursion
            (while (and (>= e (point)) (not (= char 60)))
              (setq char (following-char))
              (setq p (point))
              (forward-char)
              )
            )
        )

      ;; is end tag?
      (when (and (= char html-tag-char) (< p e))
        (goto-char p)
        (forward-char)
        (if (= (following-char) 47)
            (progn
              ;; </
              (skip-chars-forward "^>")
              (forward-char)
              (setq p (point))
              (setq found_tag 1)
              )
          (progn
            ;; < , looks fine
            (backward-char)
            (setq found_tag 0)
            )
          )
        )
      )
    (setq rlt (list p found_tag ""))
    rlt
    )
  )

;;;###autoload
(defun evilmi-html-jump (rlt NUM)
  (let ((p (nth 0 rlt))
        (tag-type (nth 1 rlt))
        (tag-keyword (nth 2 rlt))
        )

    (if (=  1 tag-type) (sgml-skip-tag-backward NUM))
    (if (=  0 tag-type) (sgml-skip-tag-forward NUM))
    (point)
    )
  )

(provide 'evil-matchit-html)
