;;; rails-for-view.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-for-view.el $
;; $Id: rails-for-view.el 77 2007-01-27 17:44:21Z dimaexe $

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

(defun rails-view:create-partial-from-selection ()
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

(defun rails-view:create-helper-from-block (&optional helper-name)
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
        (let* ((helper-file (concat (rails-core:root) (rails-core:helper-file (rails-core:current-controller))))
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

(defun rails-view:switch-to-action ()
  "Switch to the current action."
  (interactive)
  (rails-core:open-controller+action :controller
                                     (rails-core:current-controller)
                                     (rails-core:current-action)))

(defun rails-view:switch-with-menu ()
  "Switch to various files related to this view using a menu."
  (interactive)
  (let ((menu (rails-core:menu-of-views (rails-core:current-controller) t))
        (functional-test (rails-core:file (rails-core:functional-test-file (rails-core:current-controller))))
        (helper (rails-core:file (rails-core:helper-file (rails-core:current-controller))))
        item)
    (when (file-exists-p functional-test)
      (add-to-list 'menu (list "Functional Test" functional-test)))
    (when (file-exists-p helper)
      (add-to-list 'menu (list "Helper" helper)))
    (add-to-list 'menu (list "Controller" 'rails-view:switch-to-action))
    (setq item
          (rails-core:menu
           (list (concat "View "
                         (rails-core:current-controller)
                         "#"
                         (rails-core:current-action))
                 (cons "Please select.." menu))))

    (when item
      (if (symbolp item)
          (apply item nil)
        (when (file-exists-p item)
          (find-file item))))))

(defun rails-for-view ()
  "Enable RHTML configurations."
  (interactive)
  (setq rails-primary-switch-func 'rails-view:switch-to-action)
  (setq rails-secondary-switch-func 'rails-view:switch-with-menu)
  (if (boundp 'mmm-mode-map)
      (progn
        (define-key mmm-mode-map (kbd "\C-c p") 'rails-view:create-partial-from-selection)
        (define-key mmm-mode-map (kbd "\C-c b") 'rails-view:create-helper-from-block))
    (progn
      (local-set-key (kbd "\C-c p") 'rails-view:create-partial-from-selection)
      (local-set-key (kbd "\C-c b") 'rails-view:create-helper-from-block))))

(provide 'rails-for-view)