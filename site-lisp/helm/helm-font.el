;;; helm-font --- Font and ucs selection for Helm -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)

(defvar helm-ucs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-backspace>") 'helm-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'helm-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'helm-ucs-persistent-forward)
    (define-key map (kbd "<C-return>")    'helm-ucs-persistent-insert)
    (define-key map (kbd "C-c ?")         'helm-ucs-help)
    map)
  "Keymap for `helm-ucs'.")


;;; Xfont selection
;;
;;
(defun helm-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar helm-xfonts-cache nil)
(defvar helm-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless helm-xfonts-cache
                (setq helm-xfonts-cache
                      (x-list-fonts "*")))))
    (candidates . helm-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . helm-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar helm-ucs-max-len 0)
(defun helm-calculate-ucs-max-len ()
  "Calculate the length of longest `ucs-names' candidate."
  (cl-loop with count = 0
        for (n . v) in (ucs-names)
        for len = (length n)
        if (> len count)
        do (setq count len)
        finally return count))

(defun helm-ucs-init ()
  "Initialize an helm buffer with ucs symbols.
Only math* symbols are collected."
  (unless (> helm-ucs-max-len 0)
    (setq helm-ucs-max-len
          (helm-calculate-ucs-max-len)))
  (with-current-buffer (helm-candidate-buffer
                        (get-buffer-create "*helm ucs*"))
    ;; `ucs-names' fn will not run again, data is cached in
    ;; var `ucs-names'.
    (cl-loop for (n . v) in (ucs-names)
          for len = (length n)
          for diff = (+ (- helm-ucs-max-len len) 2)
          unless (string= "" n)
          do (progn (insert (concat
                             n ":"
                             (make-string
                              diff ? )))
                    (if (fboundp 'ucs-insert)
                        (ucs-insert v)
                      ;; call `insert-char' with nil nil
                      ;; to shutup byte compiler in 24.1.
                      (insert-char v nil nil))
                    (insert "\n")))))

(defun helm-ucs-forward-char (_candidate)
  (with-helm-current-buffer
    (forward-char 1)))

(defun helm-ucs-backward-char (_candidate)
  (with-helm-current-buffer
    (forward-char -1)))

(defun helm-ucs-delete-backward (_candidate)
  (with-helm-current-buffer
    (delete-char -1)))

(defun helm-ucs-insert-char (candidate)
  (with-helm-current-buffer
    (insert
     (replace-regexp-in-string
      " " ""
      (cadr (split-string candidate ":"))))))

(defun helm-ucs-persistent-insert ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'action-insert 'helm-ucs-insert-char)
    (helm-execute-persistent-action 'action-insert)))

(defun helm-ucs-persistent-forward ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'action-forward 'helm-ucs-forward-char)
    (helm-execute-persistent-action 'action-forward)))

(defun helm-ucs-persistent-backward ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'action-back 'helm-ucs-backward-char)
    (helm-execute-persistent-action 'action-back)))

(defun helm-ucs-persistent-delete ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'action-delete 'helm-ucs-delete-backward)
    (helm-execute-persistent-action 'action-delete)))

(defvar helm-source-ucs
  '((name . "Ucs names")
    (init . helm-ucs-init)
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (mode-line . helm-ucs-mode-line-string)
    (action . (("Insert" . helm-ucs-insert-char)
               ("Forward char" . helm-ucs-forward-char)
               ("Backward char" . helm-ucs-backward-char)
               ("Delete char backward" . helm-ucs-delete-backward))))
  "Source for collecting `ucs-names' math symbols.")

;;;###autoload
(defun helm-select-xfont ()
  "Preconfigured `helm' to select Xfont."
  (interactive)
  (helm-other-buffer 'helm-source-xfonts "*helm select* xfont"))

;;;###autoload
(defun helm-ucs ()
  "Preconfigured helm for `ucs-names' math symbols."
  (interactive)
  (helm :sources 'helm-source-ucs
        :keymap  helm-ucs-map))

(provide 'helm-font)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-font.el ends here
