;;; bundler.el --- Interact with Bundler from Emacs

;; Copyright (c) 2011 Tobias Svensson <tob@tobiassvensson.co.uk>

;; Author: Tobias Svensson <tob@tobiassvensson.co.uk>
;; URL: http://github.com/endofunky/bundler.el
;; Package-Version: 20160815.215
;; Keywords: bundler ruby
;; Created: 31 Dec 2011
;; Version: 1.1.1
;; Package-Requires: ((inf-ruby "2.1") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Interact with Bundler from Emacs.
;;
;; 1) bundle-open
;;
;;    Wraps 'bundle open' which, if the given gem is installed and has been
;;    required correctly, will open the gem's source directory with dired.
;;
;; 2) bundle-console
;;
;;    Starts an inferior ruby process in the context of the current bundle
;;    using 'bundle console' (requires inf-ruby to be installed).
;;
;; 3) bundle-install, bundle-update, bundle-check
;;
;;    Runs the corresponding Bundler command with async-shell-command and
;;    *Bundler* as the target buffer. This exists so the output won't mess
;;    with the default buffer used by M-& and async-shell-command.

;;; Install

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/endofunky/bundler.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/bundler.el")
;; (require 'bundler)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'inf-ruby)

;;;###autoload
(defun bundle-open (gem-name)
  "Queries for a gem name and opens the location of the gem in dired."
  (interactive (list (completing-read "Bundled gem: " (bundle-list-gems-cached))))
    (if (= (length gem-name) 0)
        (message "No gem name given.")
      (let ((gem-location (bundle-gem-location gem-name)))
        (cond
         ((eq gem-location 'no-gemfile)
          (message "Could not find Gemfile"))
         (gem-location
          (dired gem-location))
         (t
          (message "Gem '%s' not found" gem-name))))))

;;;###autoload
(defun bundle-console ()
  "Run an inferior Ruby process in the context of the current bundle."
  (interactive)
  (run-ruby "bundle console"))

;;;###autoload
(defun bundle-check ()
  "Run bundle check for the current bundle."
  (interactive)
  (bundle-command "bundle check"))

;;;###autoload
(defun bundle-install ()
  "Run bundle install for the current bundle."
  (interactive)
  (bundle-command "bundle install"))

;;;###autoload
(defun bundle-update (&optional update-cmd-args)
  "Run bundle update for the current bundle."
  (interactive "P")
  (let ((command "bundle update"))
    ;; For customization of the command with prefix arg.
    (setq command (if update-cmd-args
                      (read-string "Run: " (concat command " "))
                    command))

    (bundle-command command)))

;;;###autoload
(defun bundle-exec (command)
  (interactive "sBundle Exec: ")
  (run-bundled-command command))

;;;###autoload
(defun bundle-gemfile (&optional gemfile)
  "Set BUNDLE_GEMFILE environment variable."
  (interactive
   (list
    (let ((default-p
            (let ((gemfile-dir (bundle-locate-gemfile)))
              (if (not gemfile-dir)
                  "Gemfile"
                (concat gemfile-dir "Gemfile")))))
    (read-string (format "Gemfile (%s): " default-p)
                 default-p nil default-p))))
  (if gemfile
      (if (file-readable-p gemfile)
          (progn
            (setq bundle-gem-list-cache (make-hash-table))
            (setenv "BUNDLE_GEMFILE" gemfile)
            (message "BUNDLE_GEMFILE set to: %s." gemfile))
        (message "Warning: couldn't read file \"%s\". BUNDLE_GEMFILE unchanged." gemfile))
    (setenv "BUNDLE_GEMFILE")))

;;;###autoload
(defun bundle-outdated ()
  "List installed gems with newer versions available."
  (interactive)
  (bundle-command "bundle outdated"))

;;;###autoload
(defun bundle-show ()
  "Shows all gems that are part of the bundle, or the path to a given gem."
  (interactive)
  (bundle-command "bundle show"))

;;;###autoload
(defun bundle-version ()
  "Prints version information."
  (interactive)
  (shell-command "bundle version"))

(defun bundle-command (cmd)
  "Run cmd in an async buffer."
  (async-shell-command cmd "*Bundler*"))

(defun run-bundled-command (cmd &rest args)
  "Run bundle exec for the given command, optionally with args"
  (interactive)
  (let (command)
    (setq command
          (if args
              (concat "bundle exec " cmd " "(mapconcat 'identity args " "))
            (concat "bundle exec " cmd)))
    (bundle-command command)))

(defun bundle-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-gemfile if the
Gemfile could not be found, or nil if the Gem could not be
found."
  (let ((bundler-stdout
         (shell-command-to-string
          (format "bundle show %s" (shell-quote-argument gem-name))))
        (remote (file-remote-p default-directory)))
    (cond
     ((string-match "Could not locate Gemfile" bundler-stdout)
      'no-gemfile)
     ((string-match "Could not find " bundler-stdout)
      nil)
     (t
      (concat remote
              (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               bundler-stdout)
              "/")))))

(defvar bundle-gem-list-cache
  (make-hash-table)
  "Holds a hash table of gem lists per directory.")

(cl-defun bundle-locate-gemfile (&optional (dir default-directory))
         (let ((has-gemfile (directory-files dir nil "^Gemfile$"))
               (is-root (equal dir "/")))
           (cond
            (has-gemfile dir)
            (is-root
             (print (format
                     "No Gemfile found in either %s or any parent directory!"
                     default-directory))
             nil)
            ((bundle-locate-gemfile (expand-file-name ".." dir))))))

(defun bundle-list-gems-cached ()
  (let* ((gemfile-dir (bundle-locate-gemfile))
         (gem-list (gethash gemfile-dir bundle-gem-list-cache)))
    (if (not gemfile-dir)
        nil
      (unless gem-list
        (print (format "Don't have directory %s in cache yet, updating." gemfile-dir))
        (setq gem-list (bundle-list-gems))
        (puthash gemfile-dir gem-list bundle-gem-list-cache))
      gem-list)))

(defun bundle-list-gems ()
  (save-excursion
    (let* ((cmd "bundle list")
           (bundle-out (shell-command-to-string cmd))
           (bundle-lines (split-string bundle-out "\n")))

      (defun parse-bundle-list-line (line)
        (cond
         ((string-match "^  \\* \\([^\s]+\\).*$" line)
          (match-string 1 line))
         ((string-match "Could not \\(find\\|locate\\)" line)
          (message line) nil)
         ((string-match "Gems included by the bundle:\\|^ *$" line)
          nil)
         (t
          (message "Warning: couldn't parse line from \"%s\":\n%s"
                   cmd line)
          nil)))

      (remq nil (mapcar 'parse-bundle-list-line bundle-lines)))))

(defun bundle-list-gem-paths ()
  (save-excursion
    (let* ((cmd "bundle list --paths")
           (bundle-out (shell-command-to-string cmd)))
      (split-string bundle-out "\n"))))

(provide 'bundler)
;;; bundler.el ends here.
