;;; helm-apt.el --- Helm interface for Debian/Ubuntu packages (apt-*) -*- lexical-binding: t -*-

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
(require 'helm-utils)
(require 'helm-external)

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")


(defgroup helm-apt nil
  "Apt related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-apt-cache-show-function 'helm-apt-cache-show-1
  "Function of one argument used to show apt package.
Default is `helm-apt-cache-show-1' but you can use `apt-utils-show-package-1'
from `apt-utils.el' to have something more enhanced.
If nil default `helm-apt-cache-show-1' will be used."
  :type 'function
  :group 'helm-apt)

(defface helm-apt-installed
    '((t (:foreground "green")))
  "Face used for apt installed candidates."
  :group 'helm-apt)

(defface helm-apt-deinstalled
    '((t (:foreground "DimGray")))
  "Face used for apt deinstalled candidates."
  :group 'helm-apt)


(defvar helm-apt-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?") 'helm-apt-help)
    (define-key map (kbd "M-I")   'helm-apt-show-only-installed)
    (define-key map (kbd "M-U")   'helm-apt-show-only-not-installed)
    (define-key map (kbd "M-D")   'helm-apt-show-only-deinstalled)
    (define-key map (kbd "M-A")   'helm-apt-show-all)
    map))


(defvar helm-source-apt
  `((name . "APT")
    (init . helm-apt-init)
    (candidates-in-buffer)
    (candidate-transformer . helm-apt-candidate-transformer)
    (display-to-real . helm-apt-display-to-real)
    (update . helm-apt-refresh)
    (keymap . ,helm-apt-map)
    (mode-line . helm-apt-mode-line)
    (action
     ("Show package description" . helm-apt-cache-show)
     ("Install package" . helm-apt-install)
     ("Reinstall package" . helm-apt-reinstall)
     ("Remove package" . helm-apt-uninstall)
     ("Purge package" . helm-apt-purge))
    (persistent-action . helm-apt-persistent-action)
    (persistent-help . "Show package description")))

(defvar helm-apt-search-command "apt-cache search '%s'")
(defvar helm-apt-show-command "apt-cache show '%s'")
(defvar helm-apt-installed-packages nil)
(defvar helm-apt-all-packages nil)
(defvar helm-apt-input-history nil)
(defvar helm-apt-show-only 'all)
(defvar helm-apt-term-buffer nil)

(defun helm-apt-refresh ()
  "Refresh installed candidates list."
  (setq helm-apt-installed-packages nil)
  (setq helm-apt-all-packages nil))

(defun helm-apt-persistent-action (candidate)
  "Persistent action for APT source."
  (helm-apt-cache-show candidate))

(defun helm-apt-candidate-transformer (candidates)
  "Show installed CANDIDATES and the ones to deinstall in a different color."
  (cl-loop for cand in candidates
        for name = (helm-apt-display-to-real cand)
        for deinstall = (string=
                         (assoc-default name helm-apt-installed-packages)
                         "deinstall")
        for install = (string=
                       (assoc-default name helm-apt-installed-packages)
                       "install")
        for show = (cond ((and deinstall
                               (memq helm-apt-show-only '(all deinstalled)))
                          (propertize cand 'face 'helm-apt-deinstalled))
                         ((and install
                               (memq helm-apt-show-only '(all installed)))
                          (propertize cand 'face 'helm-apt-installed))
                         ((and (eq helm-apt-show-only 'noinstalled)
                               (not install)) cand)
                         ((eq helm-apt-show-only 'all) cand))
        when show collect show))

(defun helm-apt-show-only-installed ()
  (interactive)
  (when helm-alive-p
    (setq helm-apt-show-only 'installed)
    (helm-update)))

(defun helm-apt-show-only-not-installed ()
  (interactive)
  (when helm-alive-p
    (setq helm-apt-show-only 'noinstalled)
    (helm-update)))

(defun helm-apt-show-only-deinstalled ()
  (interactive)
  (when helm-alive-p
    (setq helm-apt-show-only 'deinstalled)
    (helm-update)))

(defun helm-apt-show-all ()
  (interactive)
  (when helm-alive-p
    (setq helm-apt-show-only 'all)
    (helm-update)))

(defun helm-apt-init ()
  "Initialize list of debian packages."
  (let ((query ""))
    (setq helm-apt-show-only 'all)
    (unless (and helm-apt-installed-packages
                 helm-apt-all-packages)
      (message "Loading package list...")
      (setq helm-apt-installed-packages
            (with-temp-buffer
              (call-process-shell-command "dpkg --get-selections"
                                          nil (current-buffer))
              (cl-loop for i in (split-string (buffer-string) "\n" t)
                    for p = (split-string i)
                    collect (cons (car p) (cadr p)))))
      (helm-init-candidates-in-buffer
          'global
        (setq helm-apt-all-packages
              (with-temp-buffer
                (call-process-shell-command
                 (format helm-apt-search-command query)
                 nil (current-buffer))
                (buffer-string))))
      (message "Loading package list done")
      (sit-for 0.5))))

(defun helm-apt-display-to-real (line)
  "Return only name of a debian package.
LINE is displayed like:
package name - description."
  (car (split-string line " - ")))

(defvar helm-apt-show-current-package nil)
(define-derived-mode helm-apt-show-mode
    special-mode "helm-apt-show"
    "Mode to display infos on apt packages.")

(defun helm-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (if (and (functionp helm-apt-cache-show-function)
           (not (eq helm-apt-cache-show-function
                    'helm-apt-cache-show)))
      ;; A function, call it.
      (funcall helm-apt-cache-show-function package)
    ;; nil or whatever use default.
    (helm-apt-cache-show-1 package)))

(defun helm-apt-cache-show-1 (package)
  (let* ((command (format helm-apt-show-command package))
         (buf     (get-buffer-create "*helm apt show*")))
    (helm-switch-to-buffer buf)
    (unless (string= package helm-apt-show-current-package)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (call-process-shell-command
           command nil (current-buffer) t))))
    (helm-apt-show-mode)
    (set (make-local-variable 'helm-apt-show-current-package)
         package)))

(defun helm-apt-install (_package)
  "Run 'apt-get install' shell command on PACKAGE."
  (helm-apt-generic-action :action 'install))

(defun helm-apt-reinstall (_package)
  "Run 'apt-get install --reinstall' shell command on PACKAGE."
  (helm-apt-generic-action :action 'reinstall))

(defun helm-apt-uninstall (_package)
  "Run 'apt-get remove' shell command on PACKAGE."
  (helm-apt-generic-action :action 'uninstall))

(defun helm-apt-purge (_package)
  "Run 'apt-get purge' shell command on PACKAGE."
  (helm-apt-generic-action :action 'purge))

(cl-defun helm-apt-generic-action (&key action)
  "Run 'apt-get ACTION'.
Support install, remove and purge actions."
  (if (and helm-apt-term-buffer
           (buffer-live-p (get-buffer helm-apt-term-buffer)))
      (switch-to-buffer helm-apt-term-buffer)
    (ansi-term (getenv "SHELL") "term apt")
    (setq helm-apt-term-buffer (buffer-name)))
  (term-line-mode)
  (let ((command   (cl-case action
                     (install   "sudo apt-get install ")
                     (reinstall "sudo apt-get install --reinstall ")
                     (uninstall "sudo apt-get remove ")
                     (purge     "sudo apt-get purge ")
                     (t          (error "Unknown action"))))
        (beg       (point))
        end
        (cand-list (mapconcat #'(lambda (x) (format "'%s'" x))
                              (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command cand-list))
    (setq end (point))
    (if (y-or-n-p (format "%s package(s)" (symbol-name action)))
        (progn
          (setq helm-external-commands-list nil)
          (setq helm-apt-installed-packages nil)
          (term-char-mode) (term-send-input))
      (delete-region beg end))))

;;;###autoload
(defun helm-apt (arg)
  "Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache."
  (interactive "P")
  (let ((query (read-string "Search Package: " nil 'helm-apt-input-history)))
    (when arg (helm-apt-refresh))
    (helm :sources 'helm-source-apt
          :prompt "Search Package: "
          :input query
          :buffer "*helm apt*"
          :history 'helm-apt-input-history)))


(provide 'helm-apt)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-apt.el ends here
