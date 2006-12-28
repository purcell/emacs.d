;;; rails-for-controller.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-for-controller.el $
;; $Id: rails-for-controller.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun rails-controller:get-current-controller-and-action ()
  "Return a list containing the current controller and action."
  (save-excursion
    (let (action controller)
      (goto-char (line-end-position))
      (if (search-backward-regexp "^[ ]*def \\([a-z0-9_]+\\)" nil t)
    (setq action (match-string-no-properties 1)))
      (if (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_:]+\\)[ ]+<" nil t)
    (setq controller (match-string-no-properties 1)))
      (list controller action))))

(defun rails-controller:switch-to-view()
  "Switch to the view corresponding to the current action."
  (interactive)
  (let* ((calist (rails-controller:get-current-controller-and-action))
         (controller (nth 0 calist))
         (action (nth 1 calist))
         file tmp)
    (if action
        (let ((root (rails-core:root))
              (files (rails-core:get-view-files controller action)))
;;
;; DO NOT UNCOMMENT AND DELETE, WAIT FIXING BUG IN CVS EMACS
;;
;;           (if (> 1 (list-length files)) ;; multiple views
;;               (let ((items (list))
;;                     (tmp files))
;;                     file)
;;                 (while (car tmp)
;;                   (add-to-list 'items (cons (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "\\2" (car tmp)) (car tmp)))
;;                   (setq tmp (cdr tmp)))
;;                 (setq file
;;                       (rails-core:menu
;;                        (list "Please select.." (cons "Please select.." files))))
;;                 (if file
;;                     (progn
;;                       (find-file file)
;;                       (message (concat controller "#" action)))))
          (if (= 1 (list-length files)) ;; one view
              (progn
                (find-file (car files))
                (message (concat controller "#" action))))
          (if (= 0 (list-length files)) ;; view not found
              (if (y-or-n-p (format "View for %s#%s not found, create %s.rhtml? " controller action action))
                  (let ((file (concat root "app/views/"
                                      (replace-regexp-in-string "_controller" ""
                                                                (rails-core:file-by-class controller t)))))
                    (make-directory file t)
                    (find-file (format "%s/%s.rhtml" file action)))))))))

(defun rails-controller:switch-with-menu ()
  "Switch to various files related to the current action using a
menu."
  (interactive)
  (let* ((root (rails-core:root))
         (menu (list))
         (views (list))
         (calist (rails-controller:get-current-controller-and-action))
         (controller (nth 0 calist))
         (action (nth 1 calist))
         (files (rails-core:get-view-files controller nil))
         (helper (rails-core:helper-file controller))
         (test (rails-core:functional-test-file controller))
         file)
    (while (car files)
      (add-to-list 'menu
                   (list
                    (replace-regexp-in-string
                     "\\(.*/\\)\\([^/]+\\)$"
                     (concat
                      (if (string-match "^\_" (file-name-nondirectory (car files))) "Partial" "View") "\: \\2")
                     (car files)) (car files)))
      (setq files (cdr files)))
    (unless (rails-use-text-menu)
      (add-to-list 'menu (list "--" "--")))
    (add-to-list 'menu (list "Functional test" (concat root test)))
    (if action
        (add-to-list 'menu (list "Current action" (car (rails-core:get-view-files controller action)))))
    (add-to-list 'menu (list "Helper" (concat root helper)))
    (setq file
          (rails-core:menu
           (list "Please select.." (cons "Please select.." menu))))
    (if file
        (find-file file))))

(defun rails-for-controller ()
  "Enable controller configurations."
  (interactive)
  (setq rails-secondary-switch-func 'rails-controller:switch-with-menu)
  (setq rails-primary-switch-func 'rails-controller:switch-to-view))

;;;;;;;; Open file from file stuff, please do not delete, while open file from file works fine

(defun rails-for-controller:views-for-current-action ()
  "Return a list of views for the current action."
  (mapcar (lambda (view-file)
      (list (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "View\: \\2" view-file)
      (lexical-let ((file view-file))
        (lambda () (interactive) (find-file file)))))
    (rails-core:get-view-files (rails-core:current-controller)
             (rails-core:current-action))))

(defun rails-for-controller:switch-by-current-controller (to-what file-func)
  "Switch by the current controller position."
  (let ((controller (rails-core:current-controller)))
    (rails-core:find-or-ask-to-create
     (format "%s for controller %s does not exist, create it? " to-what controller)
     (funcall file-func controller))))

(defun rails-for-controller:switch-to-functional-test ()
  "Switch to the functional test correspoding to the current controller."
  (rails-for-controller:switch-by-current-controller
   "Functional test" 'rails-core:functional-test-file))

(defun rails-for-controller:switch-to-helper ()
  "Switch to the helper correspoding to the current controller."
  (rails-for-controller:switch-by-current-controller
   "Helper file" 'rails-core:helper-file))

(defun rails-for-controller:switch-to-view2 ()
  "Switch to the view correspoding to the current action and
controller."
  (rails-core:open-controller+action
   :view (rails-core:current-controller) (rails-core:current-action)))

(defun rails-for-controller:switch-to-controller ()
  "Switch to the controller."
  (rails-core:open-controller+action
   :controller (rails-core:current-controller) nil))

(defun rails-for-controller:switch-to-views ()
  "Switch to the views."
  (rails-core:open-controller+action
   :view (rails-core:current-controller) nil))

(provide 'rails-for-controller)