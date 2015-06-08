;;; sunrise-x-w32-addons --- MS-Windows-specific utilities for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2011, 2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 14 May 2011
;; Version: 1
;; RCS Version: $Rev: 456 $
;; Keywords: sunrise commander, w32, ms windows
;; URL: http://www.emacswiki.org/emacs/sunrise-x-w32-addons.el
;; Compatibility: GNU Emacs 23+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension implements a listing of all Windows drives and special folders
;; inside the Sunrise Commander, as well as support for *.lnk shortcuts for all
;; file system operations inside Sunrise. Only standard Windows shortcuts are
;; currently supported.

;; *WARNING!* this extension is targeted at Emacs ports that run directly on the
;; Windows file system (like EmacsW32 and... are there any others?) if yours is
;; running on top of some simulation layer (like Cygwin does) that maps the file
;; system this code is of no use to you.

;; It was developed on EmacsW32 (version 23.1.50.1, patched) for Windows, during
;; the Hackergarten session at the GeeCon 2011 conference in Cracow. Thanks go
;; to Andreas Ames for beta-testing this code.

;;; Installation:

;; (These are generic installation instructions -- if you installed via ELPA you
;; don't need to follow them).

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise‐x‐w32-addons) expression to your .emacs file after
;; the (require 'sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;;; Usage:

;; * The "Windows Drives and Special Folders" pane can be accessed in two ways:
;; 1) by pressing "C-c w" anywhere in the file system, or
;; 2) by navigating "up" (J) from a  top-level directory in any drive or network
;; share.

;; * Windows shortcuts are resolved automatically and, as long as a shortcut can
;; be resolved to an existing file, all operations (*INCLUDING DELETION!!!*) are
;; performed directly on that file. If you want to operate on shortcuts you must
;; first disable shortcut resolution by customizing the
;; `sr-w32-follow-shortcuts' flag and setting it to false.

;; * Virtual directories (i.e. directories containing a "target.lnk" shortcut to
;; another directory) are also dereferenced automatically. If you need to modify
;; the properties (Desktop.ini) of such a folder use the
;; `sr-w32-follow-shortcuts' flag as described above.

;; * Use Shift-W to copy the full paths of all marked files and dirs to the kill
;; ring in windows-compatible form (i.e. using backslash as the file separator).

;; Enjoy ;-)

;;; Code:

(require 'sunrise-commander)

(defcustom sr-w32-follow-shortcuts t
  "Controls the shortcut resolution mechanism.
When set, all operations executed on a Windows shortcut directly
affect the target of the shortcut."
  :group 'sunrise
  :type 'boolean)

(defvar sr-w32-local-map (let ((map (make-sparse-keymap)))
			   (set-keymap-parent map sr-virtual-mode-map)
			   (define-key map "s" 'ignore)
			   (define-key map "r" 'ignore)
			   (define-key map "l" 'ignore)
			   (define-key map "d" 'ignore)
			   map)
  "Local keymap used inside the \"Windows Drives and Special Folders\" pane.")

(define-key sr-mode-map "\C-cw" 'sr-w32-virtual-entries)
(define-key sr-mode-map "\S-w" 'sr-w32-copy-paths-as-kill)

(defadvice sr-dired-prev-subdir
  (around sr-w32-advice-sr-dired-prev-subdir (&optional count))
  "Bring up the drivers pane when navigating up from a topmost directory."
  (if (sr-equal-dirs default-directory (expand-file-name ".."))
      (sr-w32-virtual-entries)
    ad-do-it))
(ad-activate 'sr-dired-prev-subdir)

(defadvice sr-find-file
  (before sr-w32-advice-sr-find-file (filename &optional wildcards))
  "Implement virtual folder resolution on Windows."
  (when sr-w32-follow-shortcuts
    (let ((info) (target (format "%s/target.lnk" filename)))
      (if (file-readable-p target)
          (setq info (sr-w32-resolve-lnk target)))
      (if (< 0 (length info))
          (setq filename info)))))
(ad-activate 'sr-find-file)

(defadvice dired-get-filename
  (after sr-w32-advice-dired-get-filename (&optional LOCALP NO-ERROR))
  "Implement standard Windows shortcut resolution."
  (when sr-w32-follow-shortcuts
    (let ((filename (or ad-return-value "")))
      (if (string-match "\\.lnk\\'" filename)
          (setq filename (sr-w32-resolve-lnk filename)))
      (if (< 0 (length filename))
          (setq ad-return-value filename)))))
(ad-activate 'dired-get-filename)

(defun sr-w32-goto-dir (dir)
  "`sr-goto-dir' replacement for the \"Windows Drives and Special Folders\" pane."
  (let ((sr-goto-dir-function nil))
    (if (not (sr-equal-dirs dir default-directory))
	(sr-goto-dir dir)
      (sr-virtual-dismiss)
      (sr-beginning-of-buffer))))

(defun sr-w32-resolve-lnk (link)
  "Use the provided VBScript script to resolve standard Windows shortcuts."
  (let* ((script (sr-w32-create-drivers-script))
	 (command (format "cscript /nologo \"%s\" /l \"%s\"" script link))
	 (info (shell-command-to-string command))
	 (info (replace-regexp-in-string "\\\\" "/" info))
	 (info (replace-regexp-in-string "\n" "" info)))
    (if (file-exists-p info) info link)))

(defun sr-w32-virtual-entries(&optional _ignore-auto _no-confirm)
  "Build a Sunrise pane containing all the Windows drives currently ready.
Also includes some selected special folders."
  (interactive)
  (let* ((script (sr-w32-create-drivers-script))
         (command (format "cscript /nologo \"%s\"" script))
         (info (car (read-from-string (sr-w32-execute-command command)))))
    (sr-switch-to-clean-buffer
     (generate-new-buffer-name "*W32 Drives & Folders*"))
    (insert "Windows Drives and Special Folders: \n")
    (insert "- \n") (sr-w32-entry-overlay (- (point) 3) (1- (point)))
    (sr-w32-display-drives info)
    (insert "- \n") (sr-w32-entry-overlay (- (point) 3) (1- (point)))
    (sr-w32-display-folders info)
    (sr-virtual-mode)
    (sr-beginning-of-buffer)
    (mapc 'make-local-variable '( revert-buffer-function
				  sr-goto-dir-function))
    (setq revert-buffer-function 'sr-w32-virtual-entries
	  sr-goto-dir-function 'sr-w32-goto-dir)
    (use-local-map sr-w32-local-map)))

(defun sr-w32-execute-command (command)
  "Safely execute the given shell command and return its output as a string."
  (condition-case nil 
      (shell-command-to-string command)
    (error
     (progn
       (sr-goto-dir "~")
       (shell-command-to-string command)))))

(defun sr-w32-display-drives (info)
  "Insert a list of all currently ready Windows drives into the current pane."
  (let ((inhibit-read-only t))
    (dolist (drive (cdr (assoc 'drives info)))
      (insert (format "drwxrwxrwx 0 x x 0 0000-00-00 %s:/\n" drive))
      (sr-w32-mask-drive))))

(defun sr-w32-mask-drive ()
  "Remove unnecesary information from the listing of a drive."
  (save-excursion
    (forward-line -1)
    (sr-w32-entry-overlay (point) (+ 30 (point)))))

(defun sr-w32-display-folders (info)
  "Insert a list of Windows special folders into the current pane."
  (dolist (folder (cdr (assoc 'folders info)))
    (when (and (< 0 (length folder)) (file-directory-p folder))
      (insert (format "drwxrwxrwx 0 x x 0 0000-00-00 %s\n" folder))
      (sr-w32-mask-folder))))

(defun sr-w32-mask-folder ()
  "Remove unnecesary details from the listing of a special folder."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (search-backward "/")
    (sr-w32-entry-overlay (1+ (point)) (point-at-bol))))

(defun sr-w32-entry-overlay (start end)
  "Create an invisible, tangible overlay from start to end."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string "  ")))

(defun sr-w32-create-drivers-script ()
  "Return the path of the VBScript file used for Windows-specific operations.
Creates it first if necessary."
  (let* ((script-name "sunrise-x-w32-addons.vbs")
         (script-dir (file-name-directory (symbol-file 'sunrise-x-w32-addons)))
         (script-path (concat script-dir script-name)))
    (unless (file-exists-p script-path)
      (with-temp-buffer
        (insert "Set objArgs = WScript.Arguments
If objArgs.Count = 0 Then
  info()
Else
  If objArgs(0) = \"/l\" Then
    resolve_lnk(objArgs(1))
  End If
End If

Function info()
  Dim filesys, drv, drvcoll, w32info, shell, folder
  Dim folders(7)
  folders(0) = \"Desktop\"
  folders(1) = \"Programs\"
  folders(2) = \"MyDocuments\"
  folders(3) = \"Favorites\"
  folders(4) = \"PrintHood\"
  folders(5) = \"NetHood\"
  folders(6) = \"AllUsersDesktop\"
  folders(7) = \"AllUsersPrograms\"

  Set filesys = CreateObject(\"Scripting.FileSystemObject\")
  Set drvcoll = filesys.Drives

  w32info = \"((drives . (\"
  For Each drv in drvcoll
    If drv.IsReady Then
      w32info = w32info & \"\"\"\" & drv.DriveLetter & \"\"\" \"
    End If
  Next
  w32info = w32info & \")) (folders . (\"

  Set shell = CreateObject(\"WScript.Shell\")
  For Each folder in folders
    folder = Replace(shell.SpecialFolders(folder), \"\\\", \"/\")
    w32info = w32info & \"\"\"\" & folder & \"\"\" \"
  Next
  w32info = w32info & \")))\"

  Wscript.Echo w32info
End Function

Function resolve_lnk(linkFile)
  Set link = WScript.CreateObject(\"WScript.Shell\").CreateShortcut(linkFile)
  WScript.Echo link.TargetPath
End Function")
        (write-file script-path)))
    script-path))

(defun sr-w32-copy-paths-as-kill ()
  "Copy windows paths of marked (or next ARG) files into the kill ring.
In all paths copied slash characters are replaced with backslashes.
The names are separated by a space."
  (interactive)
  (let* ((unix (dired-get-marked-files))
         (winz (mapcar (lambda (x) (replace-regexp-in-string "/" "\\\\" x)) unix))
         (string (mapconcat #'identity winz " ")))
    (kill-new string t)
    (message "%s" string)))

(defun sunrise-x-w32-addons-unload-function ()
  (sr-ad-disable "^sr-w32-"))

(provide 'sunrise-x-w32-addons)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-w32-addons))

;;; sunrise-x-w32-addons.el ends here
