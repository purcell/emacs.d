;;; tide-lv.el --- Other echo area

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides `tide-lv-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo "Я тихо-тихо пiдглядаю,
;;     І тiшуся собi, як бачу то,
;;     Шо страшить i не пiдпускає,
;;     А iншi п’ють тебе, як воду пiсок." L.V.
;;
;;
;; Modified version of lv.el from github.com/abo-abo/hydra

;;; Code:

(defvar tide-lv-buffer-name " *tide-LV*")

(defvar tide-lv-wnd nil
  "Holds the current LV window.")

(defun tide-lv-window ()
  "Ensure that LV window is live and return it."
  (if (window-live-p tide-lv-wnd)
      tide-lv-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq tide-lv-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))))
        (if (setq buf (get-buffer tide-lv-buffer-name))
            (switch-to-buffer buf)
          (switch-to-buffer tide-lv-buffer-name)
          (set-window-hscroll tide-lv-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (set-window-dedicated-p tide-lv-wnd t)
          (set-window-parameter tide-lv-wnd 'no-other-window t))
        (select-window ori)))))

(defun tide-lv-message (format-string &rest args)
  "Set LV window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (tide-lv-window)
      (unless (string= (buffer-string) str)
        (delete-region (point-min) (point-max))
        (insert str)
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun tide-lv-delete-window ()
  "Delete LV window and kill its buffer."
  (when (window-live-p tide-lv-wnd)
    (let ((buf (window-buffer tide-lv-wnd)))
      (delete-window tide-lv-wnd)
      (kill-buffer buf))))

(provide 'tide-lv)

;;; tide-lv.el ends here
