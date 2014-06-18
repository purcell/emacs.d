;;; evil-matchit-python.el ---python plugin of evil-matchit

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

;; @return number of indent
(defun evilmi--python-calculate-indent (line)
  (let (prefix )
    (if (string-match "^[ \t]*$" line)
        ;; empty line
        999
      (if (string-match "^\\([ \t]+\\).*$" line)
          (progn
            (setq prefix (match-string 1 line))
            ;; char code of tab is 9
            (if (= (elt prefix 0) 9)
                (length prefix)
              (/ (length prefix) 4)
              )
            )
        0
        )
      )
    )
  )

;; jump from else to if, jump from finally to try
;; only python need this hack, a wierd language
(defun evilmi--python-move-to-first-open-tag (cur-indent)
  (let (out-of-loop
        keyword
        where-to-go
        regexp
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        )

    ;; extract keyword from current line
    (if (string-match "^[ \t]*\\([a-z]+\\) *.*:$" cur-line)
        (setq keyword (match-string 1 cur-line))
      )

    (cond
     ((string= keyword "else")
      (setq regexp "^[ \t]*\\(if\\) *.*:$")
      )
     ((or (string= keyword "finally") (string= keyword "except"))
       (setq regexp "^[ \t]*\\(try\\) *.*:$")
       )
     )

    (when regexp
      (save-excursion
        (while (not out-of-loop)
          (forward-line -1)
          (setq cur-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))

          (when (and (= cur-indent (evilmi--python-calculate-indent cur-line))
                     (string-match regexp cur-line)
                     )
            (setq where-to-go (line-beginning-position))
            (setq out-of-loop t)
            )

          ;; if it's first line, we need get out of loop
          (if (= (point-min) (line-beginning-position))
              (setq out-of-loop t)
            )
          )
        )
      (when where-to-go
        (goto-char where-to-go)
        (skip-chars-forward " \t")
        )
      )
    )
  )

(defun evilmi--python-move-to-next-open-tag (keyword cur-indent)
  (let (out-of-loop
        where-to-go
        regexp
        cur-line
        )
    (cond
     ((string= keyword "try")
      (setq regexp "^[ \t]*\\(except\\) *.*:$")
      )
     ((string= keyword "except")
      (setq regexp "^[ \t]*\\(except\\|finally\\) *.*:$")
      )
     ( (or (string= keyword "elif") (string= keyword "if"))
       (setq regexp "^[ \t]*\\(elif\\|else\\) *.*:$")
       )
     )

    (save-excursion
      (while (not out-of-loop)
        (forward-line)
        (setq cur-line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))

        (when (= cur-indent (evilmi--python-calculate-indent cur-line))
          (if (and regexp (string-match regexp cur-line))
              (setq where-to-go (line-beginning-position))
            )
          (setq out-of-loop t)
          )
        ;; if it's last line, we need get out of loop
        (if (= (point-max) (line-end-position))
            (setq out-of-loop t)
          )
        )
      )
    (when where-to-go
      (goto-char where-to-go)
      (skip-chars-forward " \t")
      )
    )
  )

;;;###autoload
(defun evilmi-python-get-tag ()
  (let (rlt
        (regexp "^[ \t]*\\([a-z]+\\) *.*:$")
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        cur-indent
        (tag-type 0)
        keyword
        p
        cur-indent
        )

    (setq cur-indent (evilmi--python-calculate-indent cur-line))
    ;; move cursor to the beginning of line
    (if (string-match regexp cur-line)
        ;; open tag
        (progn
          (setq keyword (match-string 1 cur-line))
          (setq p (line-beginning-position))
          (setq tag-type 0)
          )
      ;; closed tag
      (progn
        (setq keyword "")
        (setq tag-type 1)
        (setq p (line-end-position))
        (save-excursion
          ;; find the limit end from current line
          (while (and (>= (evilmi--python-calculate-indent
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))) cur-indent)
                      (not (= p (point-max)))
                      )
            (forward-line 1)
            (setq p (line-end-position))
            )
          ;; scroll back, skip all the empty lines
          (let ((scrolling-back t)
                my-indent
                )
            (while scrolling-back
              (setq my-indent (evilmi--python-calculate-indent
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))

              (if (or (= my-indent 999)
                      (< my-indent cur-indent)
                      (= (line-beginning-position) (point-min))
                      )
                  (forward-line -1)
                (setq scrolling-back nil)
                )
              )
            )
          (setq p (line-end-position))
          )
        )
      )

    (setq rlt (list p tag-type keyword))
    rlt
    )
  )

;;;###autoload
(defun evilmi-python-jump (rlt NUM)
  (let ((p (nth 0 rlt))
        (tag-type (nth 1 rlt))
        (keyword (nth 2 rlt))
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        cur-indent
        dendent
        where-to-jump-in-theory
        )
    (setq cur-indent (evilmi--python-calculate-indent cur-line))

    ;; start from closed tag
    (cond
     ((=  1 tag-type)
      (goto-char p)
      ;; jump to open tag
      (while (not dendent)
        (forward-line -1)
        ;; first line
        (setq cur-line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))

        ;; skip empty lines
        (when (not (string-match "^[ \t]*$" cur-line))
          (if (< (evilmi--python-calculate-indent cur-line) cur-indent)
              (progn
                (setq dendent t)
                (skip-chars-forward " \t")
                (setq dendent t)
                (evilmi--python-move-to-first-open-tag (1- cur-indent))
                (setq where-to-jump-in-theory (point))
                )
            )
          )
        )
      )

     ;; start from open tag
     ((=  0 tag-type)
      ;; jump to closed tag
      (while (not dendent)
        (forward-line)
        (setq cur-line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))

        ;; just skip empty line
        (when (not (string-match "^[ \t]*$" cur-line))
          (if (<= (evilmi--python-calculate-indent cur-line) cur-indent)
              (progn
                (setq dendent t)
                )
            (progn
              ;; record the latest indented line info
              (setq where-to-jump-in-theory (line-end-position))
              )
            )
          )
        ;; last line
        (if (= (point-max) (line-end-position)) (setq dendent t))
        )

      (if where-to-jump-in-theory (goto-char where-to-jump-in-theory))

      (evilmi--python-move-to-next-open-tag keyword cur-indent)
      )
     )
    where-to-jump-in-theory
    )
  )

(provide 'evil-matchit-python)
