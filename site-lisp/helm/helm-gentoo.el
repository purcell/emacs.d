;;; helm-gentoo.el --- Helm UI for gentoo portage. -*- lexical-binding: t -*-

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

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")


(defgroup helm-gentoo nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defface helm-gentoo-match '((t (:foreground "red")))
  "Face for helm-gentoo installed packages."
  :group 'traverse-faces)


;;; Internals
(defvar helm-gentoo-use-flags nil)
(defvar helm-gentoo-buffer "*helm-gentoo-output*")
(defvar helm-cache-gentoo nil)
(defvar helm-cache-world nil)
(defvar helm-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create helm-gentoo-buffer)
              (unless helm-cache-gentoo
                (helm-gentoo-setup-cache))
              (unless helm-cache-world
                (setq helm-cache-world (helm-gentoo-get-world)))
              (helm-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (helm-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm helm-cache-world)
                                       (helm-gentoo-eshell-action elm "genlop -qe")
                                     (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (let ((urls (helm-gentoo-get-url elm)))
                                        (browse-url (helm-comp-read "Url: " urls :must-match t)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm helm-cache-world)
                                           (helm-gentoo-eshell-action elm "genlop -qi")
                                         (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (helm-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (helm-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (helm-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (helm-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (helm-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (helm-gentoo-default-action elm "equery" "files")))
               ("Refresh" . (lambda (elm)
                              (helm-gentoo-setup-cache)
                              (setq helm-cache-world (helm-gentoo-get-world))))))))


(cl-defun helm-gentoo-install (_candidate &key action)
  (setq helm-external-commands-list nil)
  (ansi-term (getenv "SHELL") "Gentoo emerge")
  (term-line-mode)
  (let ((command (cl-case action
                   (install "sudo emerge -av ")
                   (uninstall "sudo emerge -avC ")
                   (t (error "Unknown action"))))
        (elms (mapconcat 'identity (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command elms)) 
    (term-char-mode) (term-send-input)))

(defun helm-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `helm-gentoo-buffer'."
  (if (member elm helm-cache-world)
      (progn
        (helm-switch-to-buffer helm-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
    (message "No infos on packages not yet installed")))

(defvar helm-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless helm-gentoo-use-flags
                (helm-gentoo-setup-use-flags-cache))
              (helm-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-highlight-local-use)
    (action . (("Description"
                . (lambda (elm)
                    (helm-switch-to-buffer helm-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))
               ("Enable"
                . (lambda (elm)
                    (helm-gentoo-eshell-action elm "*sudo -p Password: euse -E")))
               ("Disable"
                . (lambda (elm)
                    (helm-gentoo-eshell-action elm "*sudo -p Password: euse -D")))
               ("Remove"
                . (lambda (elm)
                    (helm-gentoo-eshell-action elm "*sudo -p Password: euse -P")))
               ("Show which dep use this flag"
                . (lambda (elm)
                    (helm-switch-to-buffer helm-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))))))



(defun helm-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*helm-gentoo*"))
         (buf (helm-candidate-buffer portage-buf)))
    (with-current-buffer buf
      (cl-dolist (i helm-cache-gentoo)
        (insert (concat i "\n"))))))

(defun helm-gentoo-setup-cache ()
  "Set up `helm-cache-gentoo'"
  (setq helm-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun helm-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name helm-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
      (eshell-command (format "%s %s" command elm)))))

(defun helm-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*helm-gentoo-use*"))
         (buf (helm-candidate-buffer use-buf)))
    (with-current-buffer buf
      (cl-dolist (i helm-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun helm-gentoo-setup-use-flags-cache ()
  "Setup `helm-gentoo-use-flags'"
  (setq helm-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun helm-gentoo-get-url (elm)
  "Return a list of urls from eix output."
  (cl-loop with url-list = (split-string
                            (with-temp-buffer
                              (call-process "eix" nil t nil
                                            elm "--format" "<homepage>\n")
                              (buffer-string)))
        for i in url-list
        when (and (string-match "^http://.*" i)
                  all
                  (not (member i all)))
        collect i into all
        finally return all))

(defun helm-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun helm-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))


(defun helm-highlight-world (eix)
  "Highlight all installed package."
  (cl-loop for i in eix
        if (member i helm-cache-world)
        collect (propertize i 'face 'helm-gentoo-match)
        else
        collect i))

(defun helm-highlight-local-use (use-flags)
  (let ((local-uses (helm-gentoo-get-local-use)))
    (cl-loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'helm-gentoo-match)
          else
          collect i)))

;;;###autoload
(defun helm-gentoo ()
  "Preconfigured `helm' for gentoo linux."
  (interactive)
  (helm-other-buffer '(helm-source-gentoo
                       helm-source-use-flags)
                     "*helm gentoo*"))


(provide 'helm-gentoo)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gentoo.el ends here
