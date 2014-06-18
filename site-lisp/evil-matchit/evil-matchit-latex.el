;;; evil-matchit-latex.el ---latex plugin of evil-matchit

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
(require 'evil-matchit-sdk)

(defvar evilmi-latex-regexp "\\\\\\([a-zA-Z]+\\(\{[a-zA-Z]+\}\\)?\\)")

(defvar evilmi-latex-match-tags
  '((("if[a-zA-Z]+" "if") "else" "fi")
    ("left" nil "right")
    ("begin[a-z]+" nil "end[a-z]+")
    ("begin\{[a-z]+\}" nil "end\{[a-z]+\}"))
  "The table we look up match tags. This is a three column table.
The first column contains the open tag(s).
The second column contains the middle tag(s).
The third column contains the closed tags(s).")


;;;###autoload
(defun evilmi-latex-get-tag ()
  (let (rlt
        keyword
        tag-info
        cursor-pos)

    (save-excursion
      (skip-chars-backward "a-zA-Z \t{}")
      ;; move cursor to the beginning of tag
      (unless (bolp)
        (
         backward-char 1)
        )
      ;; extract keyword, several keyword could be in one line
      (re-search-forward evilmi-latex-regexp (line-end-position) t)
      (setq keyword (match-string 1))

      (when (evilmi-sdk-member keyword evilmi-latex-match-tags)
        (setq cursor-pos (point))
        (setq tag-info (evilmi-sdk-get-tag-info keyword evilmi-latex-match-tags))
        (setq rlt (list
                   (if (= 2 (nth 1 tag-info))
                       (point)
                     (save-excursion
                       (backward-char (1+ (length keyword)))
                       (point)
                       )
                     )
                   tag-info
                   ))))
    (if rlt (goto-char cursor-pos))
    rlt
    ))

;;;###autoload
(defun evilmi-latex-jump (rlt NUM)
  (let ((orig-tag-type (nth 1 (nth 1 rlt)))
        (orig-tag-info (nth 1 rlt))
        cur-tag-type
        cur-tag-info
        level
        keyword
        found
        where-to-jump-in-theory
        fn-reg-search
        )

    (setq level (if (= orig-tag-type 2) 0 1))
    (setq fn-reg-search (if (= orig-tag-type 2) 're-search-backward 're-search-forward))
    (while (not found)
      (if (not (funcall fn-reg-search evilmi-latex-regexp
                        (if (= orig-tag-type 2) (point-min) (point-max))
                        t))
          (setq found t)
        ;; nothing found
        (progn
          (setq keyword (match-string 1))
          (when (evilmi-sdk-member keyword evilmi-latex-match-tags)
            (setq cur-tag-info (evilmi-sdk-get-tag-info keyword evilmi-latex-match-tags))
            (setq cur-tag-type (nth 1 cur-tag-info))
            ;; we need more strict tag match strategy because tex is really wierd
            (when (= (car cur-tag-info) (car orig-tag-info))
              (cond
               ;; handle open tag
               ;; open (0) -> mid (1)  found when level is one else ignore
               ((and (= orig-tag-type 0) (= cur-tag-type 1))
                (when (= 1 level)
                  (setq where-to-jump-in-theory (point))
                  (setq found t)
                  )
                )
               ;; open (0) -> closed (2) found when level is zero, level--
               ((and (= orig-tag-type 0) (= cur-tag-type 2))
                (setq level (1- level))
                (when (= 0 level)
                  (setq where-to-jump-in-theory (point))
                  (setq found t)
                  )
                )
               ;; open (0) -> open (0) level++
               ((and (= orig-tag-type 0) (= cur-tag-type 0))
                (setq level (1+ level))
                )

               ;; now handle mid tag
               ;; mid (1) -> mid (1) found when level is zero else ignore
               ((and (= orig-tag-type 1) (= cur-tag-type 1))
                (when (= 1 level)
                  (setq where-to-jump-in-theory (point))
                  (setq found t)
                  )
                )
               ;; mid (1) -> closed (2) found when level is zero, level --
               ((and (= orig-tag-type 1) (= cur-tag-type 2))
                (setq level (1- level))
                (when (= 0 level)
                  (setq where-to-jump-in-theory (point))
                  (setq found t)
                  )
                )
               ;; mid (1) -> open (0) level++
               ((and (= orig-tag-type 1) (= cur-tag-type 0))
                (setq level (1+ level))
                )

               ;; now handle closed tag
               ;; closed (2) -> mid (1) ignore,impossible
               ((and (= orig-tag-type 2) (= cur-tag-type 1))
                (message "impossible to be here (latex-mode)")
                )
               ;; closed (2) -> closed (2) level++
               ((and (= orig-tag-type 2) (= cur-tag-type 2))
                (setq level (1+ level))
                )
               ;; closed (2) -> open (0) found when level is zero, level--
               ((and (= orig-tag-type 2) (= cur-tag-type 0))
                (setq level (1- level))
                (when (= 0 level)
                  (setq where-to-jump-in-theory (point))
                  (setq found t)
                  )
                )
               (t (message "why here?"))
               )
              )))))))

(provide 'evil-matchit-latex)
