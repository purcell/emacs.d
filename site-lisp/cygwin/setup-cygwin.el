;;; setup-cygwin.el --- Set up Emacs for using Cygwin
;;
;; Filename: setup-cygwin.el
;; Description:
;; Author: Markus Hoenika
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2012, Drew Adams, all rights reserved.
;; Created: Thu Jan 15 11:13:38 2004
;; Version: 21.0
;; Last-Updated: Wed Jun 27 10:09:18 2012 (-0700)
;;           By: dradams
;;     Update #: 108
;; URL: http://www.emacswiki.org/cgi-bin/wiki/setup-cygwin.el
;; Keywords: os, unix, cygwin
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `ange-ftp', `backquote', `comint', `cygwin-mount', `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Set up Emacs for using Cygwin.  From Markus Hoenika's paper "SGML
;;   for Windows NT" <hoenika_markus@compuserve.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/08/11 dadams
;;     Made settings that are based on Cygwin install directory conditional, per input from Tom Popovich.
;; 2011/01/04 dadams
;;     Added autoload cookies for commands.
;; 2009/10-15 dadams
;;     Set ediff-shell to shell-file-name.
;; 2007/12/08 dadams
;;     Use absolute file name for shell-file-name.
;; 2006/11/16 dadams
;;     Replace add-to-list by setq, for Emacs -q prior to Emacs 21.
;; 2006/08/14 dadams
;;     Append, not prepend "c:/cygwin/usr/info/" to Info-default-directory-list.
;; 2004/10/01 dadams
;;     Changed Info-directory-list to Info-default-directory-list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cygwin-mount)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make Cygwin paths accessible
(cygwin-mount-activate)

;;; Follow Cygwin symlinks.
;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
(defun follow-cygwin-symlink ()
  "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (re-search-forward
           "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
          (find-alternate-file (match-string 1)))
      (if (looking-at "!<symlink>")
          (progn
            (re-search-forward "!<symlink>\\(.*\\)\0")
            (find-alternate-file (match-string 1))))
      )))
(add-hook 'find-file-hooks 'follow-cygwin-symlink)

;;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'undecided-unix)


;;; Add Cygwin Info pages
(setq Info-default-directory-list (append Info-default-directory-list (list "c:/cygwin/usr/info/")))


;;; TO DO: have an option for the Cygwin installation directory, instead of fiddling this way.
(unless (or (file-directory-p "C:/cygwin/bin") (file-directory-p "C:/bin"))
  (error "Edit `setup-cygwin.el' - not known where Cygwin is installed"))

;;; Use `bash' as the default shell in Emacs.
(setq exec-path  (cons (if (file-directory-p "C:/cygwin/bin") "C:/cygwin/bin" "C:/bin")
                       exec-path))
(setq shell-file-name  (concat (if (file-directory-p "C:/cygwin/bin") "C:/cygwin/bin" "C:/bin")
                               "/bash.exe")) ; Subprocesses invoked via the shell.
(setenv "SHELL" shell-file-name)
(setenv "PATH" (concat (getenv "PATH") (if (file-directory-p "C:/cygwin/bin") ";C:\\cygwin\\bin" ";C:\\bin" )))
(setq explicit-shell-file-name  shell-file-name) ; Interactive shell
(setq ediff-shell               shell-file-name)    ; Ediff shell
(setq explicit-shell-args       '("--login" "-i"))

;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?
(setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

;; These don't seem to be needed.
;; They were recommended by http://www.khngai.com/emacs/cygwin.php
;;;;; (add-hook 'comint-output-filter-functions
;;;;;     'shell-strip-ctrl-m nil t)
;;;;; ;; Removes unsightly ^M characters that would otherwise appear in output of java applications.
;;;;; (add-hook 'comint-output-filter-functions
;;;;;     'comint-watch-for-password-prompt nil t)
;;;;; (setq explicit-shell-file-name "bash.exe")
;;;;; ;; For subprocesses invoked via the shell
;;;;; ;; (e.g., "shell -c command")
;;;;; (setq shell-file-name explicit-shell-file-name)


;;;###autoload
(defun bash ()
  "Start `bash' shell."
  (interactive)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (shell)))

(setq process-coding-system-alist
      (cons '("bash" . (raw-text-dos . raw-text-unix)) process-coding-system-alist))


;; From: http://www.dotfiles.com/files/6/235_.emacs
;;;###autoload
(defun set-shell-bash()
  "Enable on-the-fly switching between the bash shell and DOS."
  (interactive)
  ;; (setq binary-process-input t)
  (setq shell-file-name "bash")
  (setq shell-command-switch "-c")      ; SHOULD IT BE (setq shell-command-switch "-ic")?
  (setq explicit-shell-file-name "bash")
  (setenv "SHELL" explicit-shell-file-name)
  ;;;;;(setq explicit-sh-args '("-login" "-i")) ; Undefined?
  (setq w32-quote-process-args ?\") ;; "
  ;;;;;(setq mswindows-quote-process-args t)) ; Undefined?
  )

;;;###autoload
(defun set-shell-cmdproxy()
  "Set shell to `cmdproxy'."
  (interactive)
  (setq shell-file-name "cmdproxy")
  (setq explicit-shell-file-name "cmdproxy")
  (setenv "SHELL" explicit-shell-file-name)
  ;;;;;(setq explicit-sh-args nil)           ; Undefined?
  (setq w32-quote-process-args nil))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-cygwin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-cygwin.el ends here
