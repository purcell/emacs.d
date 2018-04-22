;;; helm-elisp-package.el --- helm interface for package.el -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-help)
(require 'package)

(defgroup helm-el-package nil
  "helm elisp packages."
  :group 'helm)

(defcustom helm-el-package-initial-filter 'all
  "Show only installed, upgraded or all packages at startup."
  :group 'helm-el-package
  :type '(radio :tag "Initial filter for elisp packages"
          (const :tag "Show all packages" all)
          (const :tag "Show installed packages" installed)
          (const :tag "Show not installed packages" uninstalled)
          (const :tag "Show upgradable packages" upgrade)))

(defcustom helm-el-truncate-lines t
  "Truncate lines in helm-buffer when non--nil."
  :group 'helm-el-package
  :type 'boolean)

;; internals vars
(defvar helm-el-package--show-only 'all)
(defvar helm-el-package--initialized-p nil)
(defvar helm-el-package--tabulated-list nil)
(defvar helm-el-package--upgrades nil)
(defvar helm-el-package--removable-packages nil)

;; Shutup bytecompiler for emacs-24*
(defvar package-menu-async) ; Only available on emacs-25.
(declare-function async-byte-recompile-directory "ext:async-bytecomp.el")

(defun helm-el-package--init ()
  (let (package-menu-async
        (inhibit-read-only t))
    (when (null package-alist)
      (setq helm-el-package--show-only 'all))
    (when (and (fboundp 'package--removable-packages)
               (setq helm-el-package--removable-packages
                     (package--removable-packages))
               (fboundp 'package-autoremove))
      (package-autoremove))
    (unwind-protect
         (progn
           (save-selected-window
             (if (and helm-el-package--initialized-p
                      (fboundp 'package-show-package-list))
                 ;; Use this as `list-packages' doesn't work
                 ;; properly (empty buffer) when called from lisp
                 ;; with 'no-fetch (emacs-25 WA).
                 (package-show-package-list)
               (when helm--force-updating-p (message "Refreshing packages list..."))  
               (list-packages helm-el-package--initialized-p))
             (setq helm-el-package--initialized-p t)
             (message nil))
           (helm-init-candidates-in-buffer
               'global
             (with-current-buffer (get-buffer "*Packages*")
               (setq helm-el-package--tabulated-list tabulated-list-entries)
               (remove-text-properties (point-min) (point-max)
                                       '(read-only button follow-link category))
               (buffer-string)))
           (setq helm-el-package--upgrades (helm-el-package-menu--find-upgrades))
           (if helm--force-updating-p
               (if helm-el-package--upgrades
                   (message "Refreshing packages list done, [%d] package(s) to upgrade"
                            (length helm-el-package--upgrades))
                 (message "Refreshing packages list done, no upgrades available"))
             (setq helm-el-package--show-only (if helm-el-package--upgrades
                                                  'upgrade
                                                helm-el-package-initial-filter))))
      (kill-buffer "*Packages*"))))

(defun helm-el-package-describe (candidate)
  (let ((id (get-text-property 0 'tabulated-list-id candidate)))
    (describe-package (if (fboundp 'package-desc-name)
                          (package-desc-name id)
                        (car id)))))

(defun helm-el-package-visit-homepage (candidate)
  (let* ((id (get-text-property 0 'tabulated-list-id candidate))
         (pkg (if (fboundp 'package-desc-name) (package-desc-name id)
                (car id)))
         (desc (cadr (assoc pkg package-archive-contents)))
         (extras (package-desc-extras desc))
         (url (and (listp extras) (cdr-safe (assoc :url extras)))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage"
               (propertize (symbol-name pkg)
                           'face 'font-lock-keyword-face)))))

(defun helm-el-run-visit-homepage ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-visit-homepage)))
(put 'helm-el-run-visit-homepage 'helm-only t)

(defun helm-el-package-install-1 (pkg-list)
  (cl-loop with mkd = pkg-list
           for p in mkd
           for id = (get-text-property 0 'tabulated-list-id p)
           do (package-install
               (if (fboundp 'package-desc-name) id (car id)))
           collect (if (fboundp 'package-desc-full-name) id (car id))
           into installed-list
           finally do (if (fboundp 'package-desc-full-name)
                          (message (format "%d packages installed:\n(%s)"
                                           (length installed-list)
                                           (mapconcat #'package-desc-full-name
                                                      installed-list ", ")))
                          (message (format "%d packages installed:\n(%s)"
                                           (length installed-list)
                                           (mapconcat 'symbol-name installed-list ", "))))))

(defun helm-el-package-install (_candidate)
  (helm-el-package-install-1 (helm-marked-candidates)))

(defun helm-el-run-package-install ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-install)))
(put 'helm-el-run-package-install 'helm-only t)

(defun helm-el-package-uninstall-1 (pkg-list &optional force)
  (cl-loop with mkd = pkg-list
        for p in mkd
        for id = (get-text-property 0 'tabulated-list-id p)
        do
        (condition-case-unless-debug err
            (with-no-warnings
              (if (fboundp 'package-desc-full-name)
                  ;; emacs 24.4
                  (condition-case nil
                      (package-delete id force)
                    (wrong-number-of-arguments
                     (package-delete id)))
                ;; emacs 24.3
                (package-delete (symbol-name (car id))
                                (package-version-join (cdr id)))))
          (error (message (cadr err))))
        unless (assoc (elt id 1) package-alist)
        collect (if (fboundp 'package-desc-full-name)
                        id
                      (cons (symbol-name (car id))
                            (package-version-join (cdr id))))
        into delete-list
        finally do (if delete-list
                       (if (fboundp 'package-desc-full-name)
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
                                    do (setq package-alist (delete p package-alist))))
                       "No package deleted")))

(defun helm-el-package-uninstall (_candidate)
  (helm-el-package-uninstall-1 (helm-marked-candidates) helm-current-prefix-arg))

(defun helm-el-run-package-uninstall ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-uninstall)))
(put 'helm-el-run-package-uninstall 'helm-only t)

(defun helm-el-package-menu--find-upgrades ()
  (cl-loop for entry in helm-el-package--tabulated-list
           for pkg-desc = (car entry)
           for status = (package-desc-status pkg-desc)
           when (member status '("installed" "unsigned" "dependency"))
           collect pkg-desc
           into installed
           when (member status '("available" "new"))
           collect (cons (package-desc-name pkg-desc) pkg-desc)
           into available
           finally return
           (cl-loop for pkg in installed
                    for avail-pkg = (assq (package-desc-name pkg) available)
                    when (and avail-pkg
                              (version-list-< (package-desc-version pkg)
                                              (package-desc-version
                                               (cdr avail-pkg))))
                    collect avail-pkg)))

(defun helm-el-package-upgrade-1 (pkg-list)
  (cl-loop for p in pkg-list
           for pkg-desc = (car p)
           for upgrade = (cdr (assq (package-desc-name pkg-desc)
                                    helm-el-package--upgrades))
           do
           (cond ((null upgrade)
                  (ignore))
                 ((equal pkg-desc upgrade)
                  ;;Install.
                  (with-no-warnings
                    (if (boundp 'package-selected-packages)
                        (package-install pkg-desc t)
                        (package-install pkg-desc))))
                 (t
                  ;; Delete.
                  (if (boundp 'package-selected-packages)
                      (with-no-warnings
                        (package-delete pkg-desc t t))
                      (package-delete pkg-desc))))))

(defun helm-el-package-upgrade (_candidate)
  (helm-el-package-upgrade-1
   (cl-loop with pkgs = (helm-marked-candidates)
            for p in helm-el-package--tabulated-list
            for pkg = (car p)
            if (member (symbol-name (package-desc-name pkg)) pkgs)
            collect p)))

(defun helm-el-run-package-upgrade ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-upgrade)))
(put 'helm-el-run-package-upgrade 'helm-only t)

(defun helm-el-package-upgrade-all ()
  (if helm-el-package--upgrades
      (with-helm-display-marked-candidates
        helm-marked-buffer-name (mapcar (lambda (x) (symbol-name (car x)))
                                        helm-el-package--upgrades)
        (when (y-or-n-p "Upgrade all packages? ")
          (helm-el-package-upgrade-1 helm-el-package--tabulated-list)))
      (message "No packages to upgrade actually!")))

(defun helm-el-package-upgrade-all-action (_candidate)
  (helm-el-package-upgrade-all))

(defun helm-el-run-package-upgrade-all ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-upgrade-all-action)))
(put 'helm-el-run-package-upgrade-all 'helm-only t)

(defun helm-el-package--transformer (candidates _source)
  (cl-loop for c in candidates
           for id = (get-text-property 0 'tabulated-list-id c)
           for name = (if (fboundp 'package-desc-name)
                          (and id (package-desc-name id))
                          (car id))
           for desc = (package-desc-status id)
           for built-in-p = (and (package-built-in-p name)
                                 (not (member desc '("available" "new"
                                                     "installed" "dependency"))))
           for installed-p = (member desc '("installed" "dependency"))
           for upgrade-p = (assq name helm-el-package--upgrades)
           for user-installed-p = (and (boundp 'package-selected-packages)
                                       (memq name package-selected-packages))
           do (when user-installed-p (put-text-property 0 2 'display "S " c))
           do (when (memq name helm-el-package--removable-packages)
                (put-text-property 0 2 'display "U " c)
                (put-text-property
                 2 (+ (length (symbol-name name)) 2)
                 'face 'font-lock-variable-name-face c))
           for cand = (cons c (car (split-string c)))
           when (or (and built-in-p
                         (eq helm-el-package--show-only 'built-in))
                    (and upgrade-p
                         (eq helm-el-package--show-only 'upgrade))
                    (and installed-p
                         (eq helm-el-package--show-only 'installed))
                    (and (not installed-p)
                         (not built-in-p)
                         (eq helm-el-package--show-only 'uninstalled))
                    (eq helm-el-package--show-only 'all))
           collect cand))

(defun helm-el-package-show-built-in ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'built-in)
    (helm-update)))
(put 'helm-el-package-show-built-in 'helm-only t)

(defun helm-el-package-show-upgrade ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'upgrade)
    (helm-update)))
(put 'helm-el-package-show-upgrade 'helm-only t)

(defun helm-el-package-show-installed ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'installed)
    (helm-update)))
(put 'helm-el-package-show-installed 'helm-only t)

(defun helm-el-package-show-all ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'all)
    (helm-update)))
(put 'helm-el-package-show-all 'helm-only t)

(defun helm-el-package-show-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'uninstalled)
    (helm-update)))
(put 'helm-el-package-show-uninstalled 'helm-only t)

(defvar helm-el-package-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-el-package-show-installed)
    (define-key map (kbd "M-O")   'helm-el-package-show-uninstalled)
    (define-key map (kbd "M-U")   'helm-el-package-show-upgrade)
    (define-key map (kbd "M-B")   'helm-el-package-show-built-in)
    (define-key map (kbd "M-A")   'helm-el-package-show-all)
    (define-key map (kbd "C-c i") 'helm-el-run-package-install)
    (define-key map (kbd "C-c r") 'helm-el-run-package-reinstall)
    (define-key map (kbd "C-c d") 'helm-el-run-package-uninstall)
    (define-key map (kbd "C-c u") 'helm-el-run-package-upgrade)
    (define-key map (kbd "C-c U") 'helm-el-run-package-upgrade-all)
    (define-key map (kbd "C-c @") 'helm-el-run-visit-homepage)
    map))

(defvar helm-source-list-el-package nil)
(defclass helm-list-el-package-source (helm-source-in-buffer)
  ((init :initform 'helm-el-package--init)
   (get-line :initform 'buffer-substring)
   (filtered-candidate-transformer :initform 'helm-el-package--transformer)
   (action-transformer :initform 'helm-el-package--action-transformer)
   (help-message :initform 'helm-el-package-help-message)
   (keymap :initform helm-el-package-map)
   (update :initform 'helm-el-package--update)
   (candidate-number-limit :initform 9999)
   (action :initform '(("Describe package" . helm-el-package-describe)
                       ("Visit homepage" . helm-el-package-visit-homepage)))
   (group :initform 'helm-el-package)))

(defun helm-el-package--action-transformer (actions candidate)
  (let* ((pkg-desc (get-text-property 0 'tabulated-list-id candidate))
         (status (package-desc-status pkg-desc))
         (pkg-name (package-desc-name pkg-desc))
         (built-in (and (package-built-in-p pkg-name)
                        (not (member status '("available" "new"
                                              "installed" "dependency")))))
         (acts (if helm-el-package--upgrades
                   (append actions '(("Upgrade all packages"
                                      . helm-el-package-upgrade-all-action)))
                   actions)))
    (cond (built-in '(("Describe package" . helm-el-package-describe)))
          ((and (package-installed-p pkg-name)
                (cdr (assq pkg-name helm-el-package--upgrades))
                (member status '("installed" "dependency")))
           (append '(("Upgrade package(s)" . helm-el-package-upgrade)
                     ("Uninstall package(s)" . helm-el-package-uninstall))
                   acts))
          ((and (package-installed-p pkg-name)
                (cdr (assq pkg-name helm-el-package--upgrades))
                (string= status "available"))
           (append '(("Upgrade package(s)" . helm-el-package-upgrade))
                   acts))
          ((and (package-installed-p pkg-name)
                (or (null (package-built-in-p pkg-name))
                    (and (package-built-in-p pkg-name)
                         (assq pkg-name package-alist))))
           (append acts '(("Reinstall package(s)" . helm-el-package-reinstall)
                          ("Recompile package(s)" . helm-el-package-recompile)
                          ("Uninstall package(s)" . helm-el-package-uninstall))))
          (t (append acts '(("Install packages(s)" . helm-el-package-install)))))))

(defun helm-el-package--update ()
  (setq helm-el-package--initialized-p nil))

(defun helm-el-package-recompile (_pkg)
  (cl-loop for p in (helm-marked-candidates)
           for pkg-desc = (get-text-property 0 'tabulated-list-id p)
           for name = (package-desc-name pkg-desc) 
           for dir = (package-desc-dir pkg-desc)
           do (if (fboundp 'async-byte-recompile-directory)
                  (async-byte-recompile-directory dir)
                  (when (y-or-n-p (format "Really recompile `%s' while already loaded ?" name))
                    (byte-recompile-directory dir 0 t)))))

(defun helm-el-package-reinstall (_pkg)
  (cl-loop for p in (helm-marked-candidates)
           for pkg-desc = (get-text-property 0 'tabulated-list-id p)
           for name = (package-desc-name pkg-desc)
           do (if (boundp 'package-selected-packages)
                  (with-no-warnings
                    (package-delete pkg-desc 'force 'nosave)
                    ;; pkg-desc contain the description
                    ;; of the installed package just removed
                    ;; and is BTW no more valid.
                    ;; Use the entry in package-archive-content
                    ;; which is the non--installed package entry.
                    ;; For some reason `package-install'
                    ;; need a pkg-desc (package-desc-p) for the build-in
                    ;; packages already installed, the name (as symbol)
                    ;; fails with such packages.
                    (package-install
                     (cadr (assq name package-archive-contents)) t))
                  (package-delete pkg-desc)
                  (package-install name))))

(defun helm-el-run-package-reinstall ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-reinstall)))
(put 'helm-el-run-package-reinstall 'helm-only t)

;;;###autoload
(defun helm-list-elisp-packages (arg)
  "Preconfigured helm for listing and handling emacs packages."
  (interactive "P")
  (when arg (setq helm-el-package--initialized-p nil))
  (unless helm-source-list-el-package
    (setq helm-source-list-el-package
          (helm-make-source "list packages" 'helm-list-el-package-source)))
  (helm :sources 'helm-source-list-el-package
        :truncate-lines helm-el-truncate-lines
        :full-frame t
        :buffer "*helm list packages*"))

;;;###autoload
(defun helm-list-elisp-packages-no-fetch (arg)
  "Preconfigured helm for emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on remote.
Called with a prefix ARG always fetch packages on remote."
  (interactive "P")
  (let ((helm-el-package--initialized-p (null arg)))
    (helm-list-elisp-packages nil)))

(provide 'helm-elisp-package)

;;; helm-elisp-package.el ends here
