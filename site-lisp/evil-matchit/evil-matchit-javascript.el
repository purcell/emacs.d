;;; evil-matchit-javascript.el --- simple match plugin of evil-matchit

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

;; TODO, fn.then().({}, find the second (
(defun evilmi--javascript-find-open-brace (cur-line)
  (let (rlt)
    ;; javascript code line "(function(...) { ..."
    ;; C code line "} else {"
    (if (or (string-match "^[ \t]*[\(\}]?[$_a-zA-Z0-9]+.*{ *\\(\/\/.*\\)?$" cur-line)
            (string-match "^[ \t]*[\(\}]?[$_a-zA-Z0-9]+.*{ *\\(\/\*[^/]*\*\/\\)?$" cur-line))
        (setq rlt 1)
      (save-excursion
        (forward-line)
        (setq cur-line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
        (if (string-match "^[ \t]*{ *$" cur-line)
            (setq rlt 2)
          )
        )
      )
    rlt
    )
  )

;;;###autoload
(defun evilmi-javascript-get-tag ()
  (let (p
        forward-line-num
        rlt
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
        )
    (message "evilmi-javascript-get-tag called")
    ;; only handle open tag
    (if (not (memq (following-char) (string-to-list "{[(}}])")))
        (if (setq forward-line-num (evilmi--javascript-find-open-brace cur-line))
            (when forward-line-num
              (setq p (line-beginning-position))
              (forward-line (1- forward-line-num))
              (search-forward "{" nil nil)
              (backward-char)
              (setq rlt (list p))
              )
          )
      (setq rlt (list (point)))
      )
    rlt
    )
  )

;;;###autoload
(defun evilmi-javascript-jump (rlt NUM)
  (let (cur-line)
    (when rlt
      (evil-jump-item)

      (setq cur-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
      ;; hack for javascript
      (message "cur-line=%s" cur-line)
      (if (or (string-match "^[ \t]*}\)\(.*\)\; *$" cur-line)
              (string-match "^[ \t]*}\(.*\))\; *$" cur-line)
              (string-match "^[ \t]*}\])\; *$" cur-line))
          (line-end-position)
        (1+ (point))
        )
      )
    ))

(provide 'evil-matchit-javascript)
