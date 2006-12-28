;;; rails-for-rhtml.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-for-rhtml.el $
;; $Id: rails-for-rhtml.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun rails-rhtml:create-partial-from-selection ()
  "Create a partial from current buffer selection."
  (interactive)
  (if mark-active
      (save-excursion
        (let ((name (read-string "Partial name? "))
              (content (buffer-substring-no-properties (region-beginning) (region-end))))
          (kill-region (region-beginning) (region-end))
          (insert (concat "<%= render :partial => \"" name "\" %>"))
          (split-window-vertically)
          (other-window 1)
          (find-file (concat "_" name ".rhtml"))
          (goto-char (point-min))
          (erase-buffer)
          (insert content)
          (other-window -1)
          (mmm-parse-region (line-beginning-position) (line-end-position))))))

(defun rails-rhtml:create-helper-from-block (&optional helper-name)
  "Create a helper function from current ERb block (<% .. %>)."
  (interactive)
  (rails-core:with-root
   (root)
   (let ((current-pos (point))
         (file buffer-file-name)
         begin-pos end-pos)
     (save-excursion
       (setq begin-pos (search-backward "<%" nil t))
       (setq end-pos (search-forward "%>" nil t)))
     (if (and begin-pos
              end-pos
              (> current-pos begin-pos)
              (< current-pos end-pos)
              (string-match "app/views/\\(.*\\)/\\([a-z0-9_]+\\)\.[a-z]+$" file))
         (let* ((helper-file (concat root "app/helpers/" (match-string 1 file) "_helper.rb"))
                (content (buffer-substring-no-properties begin-pos end-pos))
                (helper-alist (if helper-name helper-name (read-string "Enter helper function name with args: ")))
                (helper-alist (split-string helper-alist)))
           (if (file-exists-p helper-file)
               (let (start-point-in-helper helper-func-name)
                 (setq helper-func-name (concat "def " (car helper-alist) " ("))
                 (setq helper-alist (cdr helper-alist))
                 (mapcar (lambda (arg) (setq helper-func-name (concat helper-func-name arg ", "))) helper-alist)
                 (setq helper-func-name (concat (substring helper-func-name 0 -2) ")" ))
                 (kill-region begin-pos end-pos)
                 (insert (concat "<%= " helper-func-name " -%>" ))
                 (mmm-parse-region (line-beginning-position) (line-end-position))
                 (split-window-vertically)
                 (other-window 1)
                 (find-file helper-file)
                 (goto-char (point-min))
                 (search-forward-regexp "module +[a-zA-Z0-9:]+")
                 (newline)
                 (setq start-point-in-helper (point))
                 (insert helper-func-name)
                 (ruby-indent-command)
                 (newline)
                 (insert content)
                 (insert "\nend\n")
;;                  (while (and (re-search-forward "\\(<%=?\\|-?%>\\)" nil t)
;;                              (< (point) start-point-in-helper))
;;                    (replace-match "" nil nil))
                 (replace-regexp "\\(<%=?\\|-?%>\\)" "" nil start-point-in-helper (point))
                 (goto-char start-point-in-helper)
                 (ruby-indent-exp)
                 (other-window -1))
             (message "helper not found")))
       (message "block not found")))))

(defun rails-rhtml:get-current-controller-and-action ()
  "Return a list containing the current controller and action."
  (let ((file buffer-file-name)
        controller action)
    (if (string-match "app/views/\\(.*\\)/\\([a-z0-9_]+\\)\.[a-z]+$" file)
        (progn
          (setq controller (match-string-no-properties 1 file))
          (setq action (match-string-no-properties 2 file))))
    (list controller action)))

(defun rails-rhtml:switch-to-action ()
  "Switch to the current action."
  (interactive)
  (let ((file buffer-file-name))
    (rails-core:with-root
     (root)
     (let* ((calist (rails-rhtml:get-current-controller-and-action))
            (controller (nth 0 calist))
            (action (nth 1 calist)))
       (if (and controller action)
           (progn
             (setq controller (concat root "app/controllers/" controller "_controller.rb"))
             (if (file-exists-p controller)
                 (let ((controller-class-name (rails-core:class-by-file controller)))
                   (find-file controller)
                   (goto-char (point-min))
                   (if (search-forward-regexp (concat "^[ ]*def[ ]*" action) nil t)
                       (progn
                         (message (concat controller-class-name "#" action))
                         (recenter))
                     (message controller-class-name))))))))))

(defun rails-rhtml:switch-to-helper ()
  "Switch to the current helper."
  (let* ((root (rails-core:root))
         (calist (rails-rhtml:get-current-controller-and-action))
         (controller (nth 0 calist)))
    (find-file (concat root (rails-core:helper-file (rails-core:class-by-file controller))))))

(defun rails-rhtml:switch-with-menu ()
  "Switch to various files related to this view using a menu."
  (interactive)
  (let ((root (rails-core:root))
        (menu (list))
        item)
    (add-to-list 'menu (list "Helper" 'rails-rhtml:switch-to-helper))
    (add-to-list 'menu (list "Controller" 'rails-rhtml:switch-to-action))
    (setq item
          (rails-core:menu
           (list "Please select.." (cons "Please select.." menu))))
    (if item
        (apply item nil))))

(defun rails-for-rhtml ()
  "Enable RHTML configurations."
  (interactive)
  (setq rails-primary-switch-func 'rails-rhtml:switch-to-action)
  (setq rails-secondary-switch-func 'rails-rhtml:switch-with-menu)
  (local-set-key (kbd "\C-c p") 'rails-rhtml:create-partial-from-selection)
  (local-set-key (kbd "\C-c b") 'rails-rhtml:create-helper-from-block))

;;;;;; Open file from file

(defun rails-for-rhtml:switch-to-controller-action ()
  "Switch to the controller and action corresponding the this
file."
  (rails-core:open-controller+action
   :controller (rails-core:current-controller) (rails-core:current-action)))

(provide 'rails-for-rhtml)