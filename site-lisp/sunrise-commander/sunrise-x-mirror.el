;;; sunrise-x-mirror.el --- full read/write access to compressed archives for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 4 May 2008
;; Version: 2
;; RCS Version: $Rev: 423 $
;; Keywords: sunrise commander, archives read/write
;; URL: http://www.emacswiki.org/emacs/sunrise-x-mirror.el
;; Compatibility: GNU Emacs 22+

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

;; This is an extension for the Sunrise Commander file manager (for more details
;; visit http://www.emacswiki.org/emacs/Sunrise_Commander), that allows browsing
;; compressed archives in full read-write mode. Sunrise does offer means for
;; transparent browsing archives (using AVFS), but they just provide read-only
;; navigation -- if you want to edit a file inside the virtual filesystem, copy,
;; remove, or rename anything, you still have to uncompress the archive, do the
;; stuff and compress it back yourself.

;; It uses one or unionfs-fuse or funionfs  to create a writeable overlay on top
;; of the read-only filesystem provided by AVFS. You can freely add, remove or
;; modify anything inside the resulting union filesystem (a.k.a. the "mirror
;; area"), and then commit all modifications (or not) to the original archive
;; with a single keystroke. There is no preliminary uncompressing of the archive
;; and nothing happens if you don't make changes (or if you don't commit them).
;; On commit, the contents of the union fs are compressed to create an updated
;; archive to replace the original one (optionally after making a backup copy of
;; it, just in case).

;; Navigating outside a mirror area will automatically close it, so if you do it
;; you may be asked whether to commit or not to the archive all your changes. In
;; nested archives (e.g. a jar inside a zip inside a tgz), partial modifications
;; are committed silently on the fly if moving out from a modified archive to
;; one that contains it. Only if you leave the topmost mirror area you will be
;; asked for confirmation whether to modify the resulting archive.

;; Be warned, though, that this method may be impractical for very large or very
;; deeply nested archives with strong compression, since the uncompressing
;; happens in the final stage and requires multiple access operations through
;; AVFS. What this means is that probably you'll have to wait a looooong time if
;; you try to commit changes to a tar.bz2 file with several hundreds of
;; megabytes in size, or under five or six other layers of strong compression.

;; For this extension to work you must have:

;; 1) FUSE + AVFS support in your Sunrise Commander. If you can navigate (read-
;; only) inside compressed archives you already have this.

;; 2) One of unionfs-fuse or funionfs. Debian squeeze (stable) offers a package
;; for the first, which is currently the recommended implementation.

;; 3) Programs required for repacking archives -- at least zip and tar.

;; 4) Your AVFS mount point (and the value of variable `sr-avfs-root') must be
;; in a directory where you have writing access.

;; All this means is that most probably this extension will work out-of-the-box
;; on Linux (or MacOS, or other unices), but you'll have a hard time to make it
;; work on Windows. It was written on GNU Emacs 23 on Linux and tested on GNU
;; Emacs 22 and 23 for Linux.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-x-mirror) to your .emacs file, anywhere after the
;; (require 'sunrise-commander) sexp.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) Customize the variable `sr-mirror-unionfs-impl' and select your preferred
;; unionfs implementation (either unionfs-fuse or funionfs).

;; 5) Run the Sunrise Commander (M-x sunrise), select (or navigate inside) any
;; compressed directory in the active pane and press C-c C-b. This will
;; automatically take you to the mirror area for the selected archive. You can
;; make any modifications you want to the contents of the archive, or navigate
;; inside directories or other compressed archives inside it. When you're done,
;; press again C-c C-b anywhere inside the mirror area, or simply navigate out
;; of it. If there are any changes to commit (*and* if you confirm) the original
;; archive will be replaced with a new one with the contents of the mirror area
;; you've just been working on. If you don't change the defaults, the original
;; will be renamed with a ".bak" extension added.

;; 6) You can add support for new archive formats by adding new entries to the
;; `sr-mirror-pack-commands-alist' custom variable, which contains a regular
;; expression to match against the name of the archive and a string containing
;; the shell command to execute for packing back the mirror area into a
;; compressed archive.

;; 7) Once you've gained enough confidence using this extension you can reset
;; the `sr-mirror-keep-backups' flag to get rid of all the backup copies
;; produced by it.

;; 8) Enjoy ;)

;;; Code:

(require 'sunrise-commander)
(eval-when-compile (require 'cl))

(defcustom sr-mirror-keep-backups t
  "If non-nil, keep backup files when committing changes to read-only archives."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-mirror-pack-commands-alist
  '(
    ("\\.\\(?:zip\\|xpi\\|apk\\)$" . "zip -r   %f *")
    ("\\.[jwesh]ar$"               . "zip -r   %f *")
    ("\\.tar$"                     . "tar cvf  %f *")
    ("\\.\\(?:tar\\.gz\\|tgz\\)$"  . "tar cvzf %f *")
    ("\\.tar\\.bz2$"               . "tar cvjf %f *")
    ("\\.\\(?:tar\\.xz\\|txz\\)$"  . "tar cvJf %f *")
   )
  "List of shell commands to repack particular archive contents.
Used when repacking contents from a mirror area into a compressed
archive of the appropriate type. Use %f as a placeholder for the
name of the resulting archive. If no repacking command has been
registered here for a file (usually a file extension), Sunrise
will refuse to create a mirror area for it even if it is normally
browseable through AVFS."
  :group 'sunrise
  :type 'alist)

(defcustom sr-mirror-unionfs-impl 'unionfs-fuse
  "Implementation of unionfs to use for creating mirror areas."
  :group 'sunrise
  :type '(choice (const :tag "unionfs-fuse" unionfs-fuse)
         (const :tag "funionfs" funionfs)))

(defface sr-mirror-path-face
  '((t (:background "blue" :foreground "yellow" :bold t :height 120)))
  "Face of the directory path inside mirror areas."
  :group 'sunrise)

(defvar sr-mirror-home nil
  "Root directory of all mirror areas.
Set automatically by the function `sr-mirror-enable' and reset by
`sr-mirror-disable' to keep the mirror home path, as well as to
indicate mirroring support is on/off. Do not mess with it
directly - if you need to change the name of your mirror home
dir, modify `sr-mirror-enable'.")

(defvar sr-mirror-divert-goto-dir t
  "Internal variable used to avoid infinite recursion.
Used when diverting `sr-goto-dir' calls to `sr-mirror-goto-dir'.
Do not touch, or else.")

(if (boundp 'sr-mode-map)
    (define-key sr-mode-map "\C-c\C-b" 'sr-mirror-toggle))

(defun sr-mirror-enable ()
  "Enable Sunrise mirror support.
Sets the variable `sr-mirror-home' to a non-nil value and
activates all advice necessary for mirror operations. This method
is called every time a new mirror area is created."
  (unless sr-mirror-home
    (setq sr-mirror-home (concat sr-avfs-root "#mirror#/"))
    (ad-activate 'make-directory)
    (ad-activate 'save-buffer)
    (ad-activate 'sr-goto-dir)))

(defun sr-mirror-disable ()
  "Disable Sunrise mirror support.
Resets `sr-mirror-home' and deactivates all advice used in mirror
operations. This method is called after the last mirror area in
the current mirror home is closed."
  (when sr-mirror-home
    (setq sr-mirror-home nil)
    (ad-deactivate 'make-directory)
    (ad-deactivate 'save-buffer)
    (ad-deactivate 'sr-goto-dir)))

(defun sr-mirror-open ()
  "Set up a mirror area in the current pane.
Uses unionfs-fuse to create a writeable filesystem overlay over the AVFS virtual
filesystem of the selected compressed archive and displays it in the current
pane. The result is a mirror of the contents of the original archive that is
fully writeable."
  (interactive)
  (let ((path (or (dired-get-filename nil t)
                  (concat (expand-file-name (dired-current-directory)) "/.")))
        (sr-mirror-divert-goto-dir nil)
        (sr-avfs-root (expand-file-name sr-avfs-root))
        fname vpaths)
    (if (sr-overlapping-paths-p sr-avfs-root path)
        (unless (and sr-mirror-home (sr-overlapping-paths-p sr-mirror-home path))
          (setq path (substring path (length sr-avfs-root))
                vpaths (split-string path "#[^/]*/")
                path (car vpaths)
                vpaths (cdr vpaths))))
    (setq fname (file-name-nondirectory path))
    (if (null (assoc-default fname sr-mirror-pack-commands-alist 'string-match))
        (error (concat "Sunrise: sorry, no packer was registered for " fname)))
    (sr-mirror-enable)
    (unless (file-exists-p sr-mirror-home)
      (make-directory sr-mirror-home))
    (if vpaths
        (mapc (lambda (x)
                (let ((sr-mirror-divert-goto-dir nil))
                  (sr-goto-dir (sr-mirror-mount path))
                  (sr-follow-file x)
                  (setq path (dired-get-filename))))
              vpaths)
      (sr-goto-dir (sr-mirror-mount path)))
    (sr-graphical-highlight 'sr-mirror-path-face)
    (add-hook 'kill-buffer-hook 'sr-mirror-on-kill-buffer)
    t ))

(defun sr-mirror-mount (path)
  "Create and mount (if necessary) all the directories needed to mirror PATH.
PATH identifies the compressed archive. Returns the path to the
corresponding mirror area."
  (let* ((base (sr-mirror-mangle path))
         (virtual (sr-mirror-full-demangle path))
         (mirror (concat sr-mirror-home base))
         (overlay (concat sr-mirror-home "." base))
         (command
          (case sr-mirror-unionfs-impl
            (unionfs-fuse
             (concat "cd ~; unionfs-fuse -o cow,kernel_cache -o allow_other "
                     overlay "=RW:" virtual "=RO " mirror))

            (funionfs
             (concat "cd ~; funionfs " overlay " " mirror
                     " -o dirs=" virtual "=ro")))))
    (if (null virtual)
        (error (concat "Sunrise: sorry, don't know how to mirror " path)))
    (unless (file-directory-p mirror)
      (make-directory mirror)
      (make-directory overlay)
      (shell-command-to-string command))
    mirror))

(defun sr-mirror-close (&optional do-commit local-commit moving)
  "Destroy the current mirror area.
Unmounts and deletes the directories it was built upon. Tries to
automatically repack the mirror and substitute the original archive
with a new one containing the modifications made to the mirror.

If optional argument DO-COMMIT is set, then all changes made to the
mirror are unconditionally committed to the archive. If
LOCAL-COMMIT is set, then the commit is considered local (changes
effect a mirror nested inside another mirror). MOVING means that
this operation was triggered by the user moving outside of the
current mirror area (the current buffer will be killed soon)."
  (interactive)
  (unless sr-mirror-home
    (error (concat "Sunrise: sorry, can't mirror " (dired-get-filename))))

  (let ((here (dired-current-directory))
        (sr-mirror-divert-goto-dir nil)
        (pos) (mirror) (overlay) (vroot) (vpath) (committed))

    (unless (sr-overlapping-paths-p sr-mirror-home here)
      (error (concat "Sunrise: sorry, that's not a mirror area: " here)))

    (setq pos (string-match "\\(?:/\\|$\\)" here (length sr-mirror-home))
          mirror (substring here (length sr-mirror-home) pos)
          overlay (concat "." mirror )
          vpath (substring here (1+ pos))
          do-commit (and (sr-mirror-files (concat sr-mirror-home overlay))
                         (or do-commit
                             (y-or-n-p "Sunrise: commit changes in mirror? "))))

    (unless local-commit
      (sr-unhighlight 'sr-mirror-path-face))

    (remove-hook 'kill-buffer-hook 'sr-mirror-on-kill-buffer)
    (sr-follow-file (sr-mirror-demangle mirror))
    (setq vroot (dired-get-filename 'no-dir))

    (if do-commit (setq committed (sr-mirror-commit mirror)))
    (sr-mirror-unmount mirror overlay)

    (unless local-commit
      (if (sr-overlapping-paths-p sr-mirror-home (dired-current-directory))
          (sr-mirror-close committed))
      (unless moving
        (sr-find-file (expand-file-name (concat default-directory vroot)))
        (if (< 0 (length vpath)) (sr-goto-dir vpath)))))

  (sr-highlight)
  (if (and sr-mirror-home
           (null (directory-files sr-mirror-home nil "^[^.]")))
      (sr-mirror-disable))
  t)

(defun sr-mirror-commit (mirror)
  "Commit all modifications made to MIRROR in directory OVERLAY.
Replaces the mirrored archive with a new one built with the
current contents of the mirror. Keeps a backup of the original
archive if the variable `sr-mirror-backup' is non-nil (the
default)."
  (condition-case err
      (let ((repacked (sr-mirror-repack mirror))
            (target (dired-get-filename)))
        (if (and sr-mirror-keep-backups
                 (not (sr-overlapping-paths-p sr-mirror-home target)))
            (rename-file target (concat target ".bak") 1)
          (delete-file target))
        (copy-file repacked (dired-current-directory) t nil nil)
        (delete-file repacked)
        t)
    (error (progn
             (setq err (cadr err))
             (if (not (yes-or-no-p (concat err ". OK to continue? ")))
                 (error err))))))

(defun sr-mirror-unmount (mirror overlay)
  "Unmount and delete all directories used for mirroring a compressed archive.
MIRROR is the union of the AVFS directory that holds the contents
of the archive (read-only) with OVERLAY, which contains all the
modifications made to the union in the current session."
  (let* ((command (concat "cd ~; fusermount -u " sr-mirror-home mirror))
         (err (shell-command-to-string command)))
    (if (or (null err) (string= err ""))
        (progn
          (dired-delete-file (concat sr-mirror-home mirror) 'always)
          (dired-delete-file (concat sr-mirror-home overlay) 'always)
          (revert-buffer))
      (error (concat "Sunrise: error unmounting mirror: " err)))))

(defun sr-mirror-toggle ()
  "Open new or destroy the current mirror area, depending on context."
  (interactive)
  (let ((open-ok) (close-ok) (err-msg))
    (condition-case err1
        (setq open-ok (sr-mirror-open))
      (error (condition-case err2
                 (progn
                   (setq close-ok (sr-mirror-close))
                   (setq err-msg (cadr err1)))
               (error
                  (setq err-msg (cadr err2))) )) )
    (if (and (not open-ok) (not close-ok))
        (error err-msg)
      (sr-highlight))))

(defun sr-mirror-repack (mirror)
  "Try to repack the given MIRROR.
On success, returns a string containing the full path to the newly
packed archive, otherwise throws an error."
  (message "Sunrise: repacking mirror, please wait...")
  (let* ((target-home (concat sr-mirror-home ".repacked/"))
         (archive (replace-regexp-in-string "#[a-z0-9#]*$" "" mirror))
         (target (replace-regexp-in-string
                  "/?$" ""
                  (car (last (split-string archive "+")))))
         (command (assoc-default archive sr-mirror-pack-commands-alist 'string-match)))

    (if (null command)
        (error (concat "Sunrise: sorry, don't know how to repack " mirror)))

    (if (not (file-exists-p target-home))
        (make-directory target-home))
    (setq target (concat target-home target))
    (setq command (replace-regexp-in-string "%f" target command))
    (setq command (concat "cd " sr-mirror-home mirror "; " command))
    (shell-command-to-string command)
    target))

(defun sr-mirror-mangle (path)
  "Transform PATH into a string naming a new mirror area."
  (let ((handler (assoc-default path sr-avfs-handlers-alist 'string-match)))
    (if (eq ?/ (string-to-char path))
        (setq path (substring path 1)))
    (concat (replace-regexp-in-string
             "/" "+"
             (replace-regexp-in-string "\\+" "{+}" path)) handler)))

(defun sr-mirror-demangle (path)
  "Transform the given mirror area name into a regular filesystem path.
Opposite of `sr-mirror-mangle'."
  (concat "/"
          (replace-regexp-in-string
           "{\\+}" "+" (replace-regexp-in-string
                        "\\+\\([^}]\\)" "/\\1" (replace-regexp-in-string
                                                "#[a-z0-9#]*$" "" path)))))

(defun sr-mirror-full-demangle (path)
  "Demangle PATH recursively to obtain the current path of the original archive.
This is necessary because reflecting an archive that is itself a
reflection causes deadlocks in FUSE."
  (let ((reflected path)
        (home-len (length sr-mirror-home))
        (handler (assoc-default path sr-avfs-handlers-alist 'string-match))
        (prev-path))
    (while (and (not (string= reflected prev-path))
                (sr-overlapping-paths-p sr-mirror-home reflected))
      (setq prev-path reflected)
      (setq reflected (substring reflected home-len)
            reflected (sr-mirror-demangle reflected)))
    (setq reflected (concat sr-avfs-root reflected handler))
    reflected))

(defun sr-mirror-files (directory)
  "Return list of pathnames constituting mirror modifications inside overlay DIRECTORY."
  (if (not (file-directory-p directory))
      (ignore)
    (let ((files (directory-files directory)))
      (mapc (lambda (x) (setq files (delete x files)))
              '("." ".." "._funionfs_control~"))
      files)))

(defun sr-mirror-overlay-redir (dirname &optional force-root)
  "Adjust DIRNAME for use with a mirror filesystem.
Analyses the given directory path and rewrites it (if necessary)
to play nicely with the mirror fs the given path belongs to. If
the path is not inside any mirror fs, it is returned unmodified."
  (if (null sr-avfs-root)
      dirname
    (let ((xpdir (expand-file-name dirname))
          (mirror) (pos) (target))
      (if (sr-overlapping-paths-p sr-mirror-home xpdir)
          (progn
            (setq mirror (substring xpdir (length sr-mirror-home)))
            (setq pos (string-match "/\\|$" mirror))
            (if pos
                (progn
                  (setq target (replace-regexp-in-string "^/" "" (substring mirror pos)))
                  (setq mirror (substring mirror 0 pos))))
            (if (and target
                     (or (> (length target) 0) force-root)
                     (not (eq ?. (string-to-char mirror))))
                (concat sr-mirror-home "." mirror "/" target)
              dirname))
        dirname))))

(defun sr-mirror-surface (dir)
  "Return the topmost parent of DIR under `sr-mirror-home', if any."
  (if (and sr-mirror-home
           (sr-overlapping-paths-p sr-mirror-home dir)
           (not (sr-equal-dirs sr-mirror-home dir)))
      (let ((local-dir (dired-make-relative dir sr-mirror-home)))
        (string-match "^\\([^/]*\\)" local-dir)
        (match-string 1 local-dir))))

(defun sr-mirror-overlapping-p (mirror1 mirror2)
  "Return non-nil if the surface of MIRROR2 maps an archive nested
inside the archive mapped by the surface of MIRROR1."
  (let ((surface1 (sr-mirror-surface mirror1))
        (surface2 (sr-mirror-surface mirror2))
        top)
    (when (and surface1 surface2)
      (setq top (sr-mirror-demangle surface1))
      (sr-overlapping-paths-p top (sr-mirror-demangle surface2)))))

(defun sr-mirror-goto-dir (target)
  "Enhance `sr-goto-dir' with transparent navigation inside mirror areas.
All calls to `sr-goto-dir' are diverted to this function."
  (let* ((here (expand-file-name default-directory))
         (target (expand-file-name (or target ".")))
         (surface-here (sr-mirror-surface here))
         (sr-mirror-divert-goto-dir nil)
         surface-target)
    (cond
     ((null surface-here) (sr-goto-dir target))
     ((sr-overlapping-paths-p sr-avfs-root target) (sr-mirror-open))
     (t
      (progn
        (if (sr-equal-dirs target sr-mirror-home)
            (setq target (expand-file-name
                          (concat (sr-mirror-demangle surface-here) "/.."))
                  surface-target (sr-mirror-surface (sr-mirror-mangle target)))
          (setq surface-target (sr-mirror-surface target)))
        (unless (equal surface-here surface-target)
          (if (and surface-target
                   (sr-overlapping-paths-p sr-mirror-home target)
                   (sr-mirror-overlapping-p surface-target surface-here))
              (sr-mirror-close t t)
            (sr-mirror-close nil nil t)))
        (unless (or (not (file-directory-p target))
                    (sr-equal-dirs target (dired-current-directory)))
          (sr-goto-dir target)))))
    (sr-highlight)))

(defun sr-mirror-on-kill-buffer ()
  "Handle navigation out of a mirror area other than through `sr-goto-dir'.
This includes e.g. bookmark jumps and pane synchronizations."
  (when (and sr-mirror-home (eq major-mode 'sr-mode)
           (null (sr-mirror-surface sr-this-directory))
           (sr-mirror-surface (dired-current-directory)))
      (sr-mirror-goto-dir sr-this-directory)
      (sr-unhighlight 'sr-mirror-path-face)))

(defadvice sr-goto-dir
  (around sr-mirror-advice-sr-goto-dir (dir))
  "Divert all `sr-goto-dir' calls to `sr-mirror-goto-dir'."
  (if sr-mirror-divert-goto-dir
      (sr-mirror-goto-dir dir)
    ad-do-it))

(defadvice sr-clone-files
  (around sr-mirror-advice-sr-clone-files
          (file-path-list target-dir clone-op progress &optional do-overwrite))
"Redirect all `sr-copy' operations to the right path under the
overlay directory."
  (if (null sr-mirror-home)
      ad-do-it
    (let ((orig target-dir))
      (setq target-dir (sr-mirror-overlay-redir target-dir t))
      (if (> (length target-dir) (length orig))
          (make-directory target-dir))
      ad-do-it)))
(ad-activate 'sr-clone-files)

(defadvice make-directory
  (around sr-mirror-advice-make-directory (dirname &optional parents))
  "Redirect directory creation operations to the right path under
the overlay directory."
  (setq dirname (sr-mirror-overlay-redir dirname))
  (setq parents t)
  ad-do-it)

(defadvice save-buffer
  (around sr-mirror-advice-save-buffer (&optional args))
  "Create all the subdirectories (and set their permissions)
needed for enabling the redirection of buffer saving operations
to the right path under the overlay directory."
  (let* ((orig (buffer-file-name))
         (target (sr-mirror-overlay-redir orig)))
    (if (> (length target) (length orig))
        (let ((default-directory "~/")
              (target-dir (file-name-directory target)))
          (make-directory target-dir)
          (shell-command-to-string (concat dired-chmod-program " a+x " target-dir))
          (write-file target nil))
      ad-do-it)))

(defun sr-mirror-toggle-read-only ()
  "Toggle the read-only flag in all buffers opened inside a mirror area,
so they are always writeable by default."
  (if sr-mirror-home
      (let* ((orig (buffer-file-name))
             (target (sr-mirror-overlay-redir orig)))
        (if (> (length target) (length orig))
            (setq buffer-read-only nil)))))
(add-hook 'find-file-hook 'sr-mirror-toggle-read-only)

(defun sunrise-x-mirror-unload-function ()
  (sr-ad-disable "^sr-mirror-"))

(provide 'sunrise-x-mirror)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-mirror))

;;; sunrise-x-mirror.el ends here
