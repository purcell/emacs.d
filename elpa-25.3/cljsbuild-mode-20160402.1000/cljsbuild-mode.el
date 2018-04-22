;;; cljsbuild-mode.el --- A minor mode for the ClojureScript 'lein cljsbuild' command

;; Copyright 2012-2016 Kototama

;; Authors: Kototama <kototamo gmail com>
;; Version: 0.4.1
;; Package-Version: 20160402.1000
;; Package-X-Original-version: 0.4.1
;; Keywords: clojure, clojurescript, leiningen, compilation
;; URL: http://github.com/kototama/cljsbuild-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
;; that will automatically watch the compilation buffer, pops it when the
;; compilation failed and (optionally) hides it when the compilation
;; succeed.

;; Installation:
;;
;; Packages are available in the Marmalade and MELPA repositories.
;; Install the mode with "M-x package-install RET cljsbuild-mode".
;;
;; Usage:
;;
;; M-x cljsbuild-start
;;
;;; Code:

(require 'ansi-color)
(require 'compile)

(defgroup cljsbuild-mode nil
  "A helper mode for running 'lein cljsbuild' within Emacs."
  :prefix "cljsbuild-"
  :group 'applications)

;;;###autoload
(define-minor-mode cljsbuild-mode
  "ClojureScript Build mode"
  :init-value nil
  :lighter " Cljs-Build"
  :group 'cljsbuild-mode
  :after-hook (cljsbuild-init-mode))

(defcustom cljsbuild-verbose t
  "When non-nil, provide progress feedback in the minibuffer."
  :type 'boolean
  :group 'cljsbuild-mode)

(defcustom cljsbuild-show-buffer-on-failure t
  "When non-nil, pop up the build buffer when failures are seen."
  :type 'boolean
  :group 'cljsbuild-mode)

(defcustom cljsbuild-hide-buffer-on-success nil
  "When non-nil, hide the build buffer when a build succeeds."
  :type 'boolean
  :group 'cljsbuild-mode)

(defcustom cljsbuild-show-buffer-on-warnings t
  "When non-nil, pop up the build buffer when warnings are seen."
  :type 'boolean
  :group 'cljsbuild-mode)

(defun cljsbuild-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS through to `message' if `cljsbuild-verbose' is non-nil."
  (when cljsbuild-verbose
    (apply #'message format-string args)))

(defcustom cljsbuild-compile-command
  "lein cljsbuild auto"
  "Default build command to use for `cljsbuild-compile'."
  :type 'string
  :group 'cljsbuild-mode)

(defconst cljsbuild-compilation-error-regexp-alist
  '(("^Caused by: .+{:column \\([0-9]+\\), :line \\([0-9]+\\), :file \"\\(.+\\)\""
     3 2 1 nil 3)
    ("^ERROR: .+ error at \\(.+\\) line \\([0-9]+\\) : \\([0-9]+\\)"
     1 2 3 nil 3)
    ("^WARNING: .+ at line \\([0-9]+\\) \\(.+\\.cljs\\)"
     2 1 1 2 2)
    ("^WARNING: .+ at line \\([0-9]+\\) file:\\(.+\\)"
     2 1 1 1 2))
  "Regexps used for matching ClojureScript compile messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar cljsbuild-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    (define-key map (kbd "g") 'cljsbuild-start)
    map)
  "Keymap for `cljsbuild-compilation-mode' buffers.")

(defun cljsbuild-on-buffer-change
  (beginning end &optional len)
  (let ((inserted (buffer-substring-no-properties beginning end))
        (buffer-visible (get-buffer-window (buffer-name) 'visible)))
    (cond ((string-match "^Compiling .+\.\.\.$" inserted)
           (with-current-buffer (buffer-name)
             (delete-region (point-min) (1- beginning))))
          ((string-match "^Successfully compiled" inserted)
           (cljsbuild-message "Cljsbuild compilation success")
           (when cljsbuild-hide-buffer-on-success
             ;; hides the compilation buffer
             (delete-windows-on (buffer-name))))
          ((string-match "^Compiling.+failed.$" inserted)
           (cljsbuild-message "Cljsbuild compilation failure")
           (when (and (not buffer-visible) cljsbuild-show-buffer-on-failure)
             ;; if the compilation buffer is not visible, shows it
             (switch-to-buffer-other-window (buffer-name) t)))
          ((string-match "^WARNING:" inserted)
           (cljsbuild-message "Cljsbuild compilation warning")
           (when (and (not buffer-visible) cljsbuild-show-buffer-on-warnings)
             (switch-to-buffer-other-window (buffer-name) t))))))

(defun cljsbuild-init-mode ()
  "Initialize the minor mode and register a change hook on the
compilation buffer"
  (remove-hook 'after-change-functions 'cljsbuild-on-buffer-change)
  (add-hook 'after-change-functions 'cljsbuild-on-buffer-change nil t))

(defun cljsbuild-process-sentinel
  (process event)
  "Display a message when a change to the process occurs."
  (message "Cljsbuild: %s" event))

;; Functions using compile-mode as cljsbuild output:
(defun cljsbuild-compilation-filter-hook ()
  "Local `compilation-filter-hook' for `cljsbuild-compilation-mode'."
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (cljsbuild-on-buffer-change compilation-filter-start (point-max)))

(define-compilation-mode cljsbuild-compilation-mode "cljsbuild"
  "ClojureScript `compilation-mode'."
  (set (make-local-variable 'compilation-error-regexp-alist)
       cljsbuild-compilation-error-regexp-alist)

  (add-hook 'compilation-filter-hook
            'cljsbuild-compilation-filter-hook nil t))

(defun cljsbuild-do-compile (command)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (compilation-start command 'cljsbuild-compilation-mode))

;;;###autoload
(defun cljsbuild-start (command)
  "Runs cljsbuild."
  (interactive
   (list (read-string "cljsbuild command: "
		      cljsbuild-compile-command
		      nil
		      '("lein cljsbuild once" "lein cljsbuild clean"))))
  (let ((project-dir (locate-dominating-file default-directory "project.clj")))
    (if project-dir
	(cd project-dir)
      (error "Not inside a Leiningen project")))
  (cljsbuild-do-compile command))

(provide 'cljsbuild-mode)

;;; cljsbuild-mode.el ends here
