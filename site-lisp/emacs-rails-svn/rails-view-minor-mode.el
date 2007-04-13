;;; rails-view-minor-mode.el --- minor mode for RubyOnRails views

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-view-minor-mode.el $
;; $Id: rails-view-minor-mode.el 173 2007-04-09 15:15:02Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defun rails-view-minor-mode:create-partial-from-selection ()
  "Create a partial from current buffer selection."
  (interactive)
  (if mark-active
      (save-excursion
        (let ((name (read-string "Partial name (without _ and extension)? "))
              (content (buffer-substring-no-properties (region-beginning) (region-end)))
              (modified (buffer-modified-p)))
          (unless (string-not-empty name)
            (progn
              (message "Empty partial name") (return)))
          (kill-region (region-beginning) (region-end))
          (insert (concat "<%= render :partial => \"" name "\" %>"))
          (mmm-parse-region (line-beginning-position) (line-end-position))
          (insert  "\n")
          (split-window-vertically)
          (other-window 1)
          (find-file (concat "_" name ".rhtml"))
          (goto-char (point-min))
          (erase-buffer)
          (insert content)
          (save-buffer)
          (fit-window-to-buffer)
          (other-window -1)
          (unless modified (save-buffer))
          (message "type `C-x +` to balance windows")))))

(defun rails-view-minor-mode:create-helper-from-block (&optional helper-name)
  "Create a helper function from current ERb block (<% .. %>)."
  (interactive)
  (let ((current-pos (point))
        (file buffer-file-name)
        begin-pos
        end-pos)
    (save-excursion
      (setq begin-pos (search-backward "<%" nil t))
      (setq end-pos (search-forward "%>" nil t)))
    (if (and begin-pos
             end-pos
             (> current-pos begin-pos)
             (< current-pos end-pos))
        (let* ((helper-file (concat (rails-project:root) (rails-core:helper-file (rails-core:current-controller))))
               (content (replace-regexp-in-string "\\(<%=?\\|-?%>\\)" ""
                                                  (buffer-substring-no-properties begin-pos end-pos)))
               (helper-defination (if helper-name helper-name
                                     (read-string "Type helper function defination (without `def` keyword): "))))
           (if (file-exists-p helper-file)
               (let ((modified (buffer-modified-p))
                     (helper-func-def (concat "def " helper-defination)))
                 (kill-region begin-pos end-pos)
                 (insert (concat "<%= " helper-defination " -%>" ))
                 (mmm-parse-region (line-beginning-position) (line-end-position))
                 (insert "\n")
                 (split-window-vertically)
                 (other-window 1)
                 (find-file helper-file)
                 (goto-char (point-min))
                 (search-forward-regexp "module +[a-zA-Z0-9:]+")
                 (end-of-line)
                 (newline)
                 (ruby-indent-command)
                 (save-excursion
                   (insert (concat helper-func-def "\n" content "\nend\n")))
                 (ruby-indent-exp)
                 (fit-window-to-buffer)
                 (save-buffer)
                 (other-window -1)
                 (unless modified (save-buffer))
                 (message "Type `C-x +` to balance windows"))
             (message "helper not found")))
       (message "block not found"))))

(define-minor-mode rails-view-minor-mode
  "Minor mode for RubyOnRails views."
  :lighter " View"
  :keymap (rails-controller-layout:keymap :view)
  (setq rails-primary-switch-func 'rails-controller-layout:toggle-action-view)
  (setq rails-secondary-switch-func 'rails-controller-layout:menu)
  (if (boundp 'mmm-mode-map)
      (progn
        (define-key mmm-mode-map (rails-key "p") 'rails-view-minor-mode:create-partial-from-selection)
        (define-key mmm-mode-map (rails-key "b") 'rails-view-minor-mode:create-helper-from-block))
    (progn
      (local-set-key (rails-key "p") 'rails-view-minor-mode:create-partial-from-selection)
      (local-set-key (rails-key "b") 'rails-view-minor-mode:create-helper-from-block))))

(provide 'rails-view-minor-mode)