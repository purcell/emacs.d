;;; helm-elisp-package.el --- helm interface for package.el -*- lexical-binding: t -*-

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
(require 'package)

;; internals vars
(defvar helm-el-package--show-only 'all)
(defvar helm-el-package--initialized-p nil)

(defun helm-el-package--init ()
  (when (null package-alist)
    (setq helm-el-package--show-only 'all))
  (save-selected-window
    (list-packages helm-el-package--initialized-p)
    (setq helm-el-package--initialized-p t)
    (message nil))
  (helm-init-candidates-in-buffer
      'global
    (with-current-buffer (get-buffer "*Packages*")
      (buffer-string)))
  (setq helm-el-package--show-only 'all)
  (kill-buffer "*Packages*"))

(defun helm-el-package-describe (candidate)
  (let ((id (get-text-property 0 'tabulated-list-id candidate)))
    (describe-package (if (fboundp 'package-desc-name)
                          (package-desc-name id)
                        (car id)))))

(defun helm-el-package-install (_candidate)
  (cl-loop with mkd = (helm-marked-candidates)
        for p in mkd
        for id = (get-text-property 0 'tabulated-list-id p)
        do (package-install
            (if (fboundp 'package-desc-name)
                (package-desc-name id)
              (car id)))
        and collect (if (fboundp 'package-desc-full-name)
                        id
                      (car id))
        into installed-list
        finally do (if (fboundp 'package-desc-full-name)
                       (message (format "%d packages installed:\n(%s)"
                                        (length installed-list)
                                        (mapconcat #'package-desc-full-name
                                                   installed-list ", ")))
                     (message (format "%d packages installed:\n(%s)"
                                      (length installed-list)
                                      (mapconcat 'symbol-name installed-list ", "))))))

(defun helm-el-package-uninstall (_candidate)
  (cl-loop with mkd = (helm-marked-candidates)
        for p in mkd
        for id = (get-text-property 0 'tabulated-list-id p)
        do
        (condition-case-unless-debug err
            (with-no-warnings
              (if (fboundp 'package-desc-full-name)
                  ;; emacs 24.4
                  (package-delete id)
                ;; emacs 24.3
                (package-delete (symbol-name (car id))
                                (package-version-join (cdr id)))))
          (error (message (cadr err))))
        and collect (if (fboundp 'package-desc-full-name)
                        id
                      (cons (symbol-name (car id))
                            (package-version-join (cdr id))))
        into delete-list
        finally do (if (fboundp 'package-desc-full-name)
                       ;; emacs 24.4
                       (message (format "%d packages deleted:\n(%s)"
                                        (length delete-list)
                                        (mapconcat #'package-desc-full-name
                                                   delete-list ", ")))
                     ;; emacs 24.3
                     (message (format "%d packages deleted:\n(%s)"
                                      (length delete-list)
                                      (mapconcat (lambda (x)
                                                   (concat (car x) "-" (cdr x)))
                                                 delete-list ", ")))
                     ;; emacs 24.3 doesn't update
                     ;; its `package-alist' after deleting.
                     (cl-loop for p in package-alist
                           when (assq (symbol-name (car p)) delete-list)
                           do (setq package-alist (delete p package-alist))))))

(defun helm-el-package--transformer (candidates _source)
  (cl-loop for c in candidates
        for id = (get-text-property 0 'tabulated-list-id c) 
        for installed-p = (assq (if (fboundp 'package-desc-name)
                                    (package-desc-name id)
                                  (car id))
                                package-alist)
        for cand = (cons c (car (split-string c)))
        when (or (and installed-p
                      (eq helm-el-package--show-only 'installed))
                 (and (not installed-p)
                      (eq helm-el-package--show-only 'uninstalled)) 
                 (eq helm-el-package--show-only 'all))
        collect cand))

(defun helm-el-package-show-installed ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'installed)
    (helm-update)))

(defun helm-el-package-show-all ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'all)
    (helm-update)))

(defun helm-el-package-show-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'uninstalled)
    (helm-update)))

(defvar helm-el-package-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I") 'helm-el-package-show-installed)
    (define-key map (kbd "M-U") 'helm-el-package-show-uninstalled)
    (define-key map (kbd "M-A") 'helm-el-package-show-all)
    (define-key map (kbd "C-c ?") 'helm-el-package-help)
    map))

(defvar helm-source-list-el-package
  `((name . "list packages")
    (init . helm-el-package--init)
    (get-line . buffer-substring)
    (match-part . (lambda (c) (car (split-string c))))
    (filtered-candidate-transformer . helm-el-package--transformer)
    (candidates-in-buffer)
    (mode-line . helm-el-package-mode-line)
    (keymap . ,helm-el-package-map)
    (candidate-number-limit . 9999)
    (action . (("Describe" . helm-el-package-describe)
               ("Install" . helm-el-package-install)
               ("Uninstall" . helm-el-package-uninstall)))))

;;;###autoload
(defun helm-list-elisp-packages (arg)
  (interactive "P")
  (when arg (setq helm-el-package--initialized-p nil))
  (helm :sources 'helm-source-list-el-package
        :buffer "*helm list packages*"))

(provide 'helm-elisp-package)

;;; helm-elisp-package.el ends here
