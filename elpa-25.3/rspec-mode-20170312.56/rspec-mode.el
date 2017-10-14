;;; rspec-mode.el --- Enhance ruby-mode for RSpec

;; Copyright (C) 2008-2015 Peter Williams <http://barelyenough.org> and others
;; Author: Peter Williams, et al.
;; URL: http://github.com/pezra/rspec-mode
;; Created: 2011
;; Version: 1.16
;; Keywords: rspec ruby
;; Package-Requires: ((ruby-mode "1.0") (cl-lib "0.4"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; This minor mode provides some enhancements to ruby-mode in the
;; contexts of RSpec specifications.  Refer to the README for a
;; summary of keybindings and their descriptions.
;;
;; You can choose whether to run specs using 'rake spec' or the 'rspec'
;; command. Use the customization interface (customize-group
;; rspec-mode) or override using (setq rspec-use-rake-when-possible TVAL).
;;
;; Options will be loaded from spec.opts or .rspec if it exists and
;; rspec-use-opts-file-when-available is not set to nil, otherwise it
;; will fallback to defaults.
;;
;; You can also launch specs from Dired buffers, to do that, add this:
;;
;;   (add-hook 'dired-mode-hook 'rspec-dired-mode)
;;
;; It has almost the same keybindings, but there's no toggle-spec
;; command, and `rspec-dired-verify-single' runs all marked files, or
;; the file at point.
;;
;; Dependencies:
;;
;; If `rspec-use-rvm` is set to true `rvm.el' is required.
;;
;;; Change Log:
;;
;; 1.16 - Add `rspec-yank-last-command' function (Sergiy Kukunin)
;; 1.15 - Add option to run spec commands in a Docker container
;;        through "docker exec".
;; 1.14 - Add option to run spec commands in a Vagrant box through
;;        "vagrant ssh -c".
;; 1.13 - Add a variable to autosave current buffer where it makes sense
;; 1.12 - Run specs for single method (Renan Ranelli)
;; 1.11 - Switching between method, its specs and back (Renan Ranelli)
;; 1.9 - Support for RSpec 3.
;; 1.8 - Support for Capybara's acceptance test DSL (Ales Guzik)
;; 1.7 - Support for Spring (Tomy Kaira)
;;     - New commands: `rspec-verify-matching', `rspec-verify-continue'
;;       (Jean-Louis Giordano)
;;     - Run specs from Dired (Adam Sokolnicki)
;;     - Include Yasnippet snippets collection (Dmitry Gutov)
;; 1.6 - Improved keymaps and compile buffer (Dmitry Gutov)
;; 1.5 - Allow key prefix to be customized (`rspec-key-command-prefix')
;; 1.4 - Allow .rspec/spec.opts files to be ignored (user option
;;       `rspec-use-opts-file-when-available')
;; 1.3 - Bundler support (JD Huntington)
;; 1.2 - Rspec2 compatibility  (Anantha Kumaran)
;; 1.1 - Run verification processes from project root directory (Joe Hirn)
;; 1.0 - Advance to end of compilation buffer even if it not the other window (byplayer)
;; 0.8 - RVM support (Peter Williams)
;; 0.7 - follow RoR conventions for file in lib directory (Tim Harper)
;; 0.6 - support for arbitrary spec and rake commands (David Yeu)
;; 0.5 - minor changes from Tim Harper
;; 0.4 - ansi colorization of compliation buffers (teaforthecat)
;; 0.3 - Dave Nolan implements respect for spec.opts config and
;;       custom option to use 'rake spec' task or 'spec' command
;; 0.2 - Tim Harper implemented support for imenu to generate a basic
;;       tag outline
;; 0.1 - Pezra's version in master

;;; Code:
(require 'ruby-mode)
(require 'ansi-color)
(require 'compile)
(require 'cl-lib)

(define-prefix-command 'rspec-verifiable-mode-keymap)
(define-prefix-command 'rspec-mode-keymap)

(define-key rspec-verifiable-mode-keymap (kbd "v") 'rspec-verify)
(define-key rspec-verifiable-mode-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-verifiable-mode-keymap (kbd "t") 'rspec-toggle-spec-and-target)
(define-key rspec-verifiable-mode-keymap (kbd "e") 'rspec-toggle-spec-and-target-find-example)
(define-key rspec-verifiable-mode-keymap (kbd "4 t") 'rspec-find-spec-or-target-other-window)
(define-key rspec-verifiable-mode-keymap (kbd "4 e") 'rspec-find-spec-or-target-find-example-other-window)
(define-key rspec-verifiable-mode-keymap (kbd "r") 'rspec-rerun)
(define-key rspec-verifiable-mode-keymap (kbd "y") 'rspec-yank-last-command)
(define-key rspec-verifiable-mode-keymap (kbd "m") 'rspec-verify-matching)
(define-key rspec-verifiable-mode-keymap (kbd "c") 'rspec-verify-continue)
(define-key rspec-verifiable-mode-keymap (kbd "s") 'rspec-verify-method)
(define-key rspec-verifiable-mode-keymap (kbd "f") 'rspec-run-last-failed)

(set-keymap-parent rspec-mode-keymap rspec-verifiable-mode-keymap)

(define-key rspec-mode-keymap (kbd "s") 'rspec-verify-single)
(define-key rspec-mode-keymap (kbd "d") 'rspec-toggle-example-pendingness)

(define-prefix-command 'rspec-dired-mode-keymap)
(define-key rspec-dired-mode-keymap (kbd "v") 'rspec-dired-verify)
(define-key rspec-dired-mode-keymap (kbd "s") 'rspec-dired-verify-single)
(define-key rspec-dired-mode-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-dired-mode-keymap (kbd "r") 'rspec-rerun)

(defgroup rspec-mode nil
  "RSpec minor mode."
  :group 'languages)

(defcustom rspec-use-rake-when-possible nil
  "When non-nil and Rakefile is present, run specs via rake spec task."
  :tag "RSpec runner command"
  :type '(radio (const :tag "Use 'rake spec' task" t)
                (const :tag "Use 'rspec' command" nil))
  :group 'rspec-mode)

(define-obsolete-variable-alias 'rspec-use-rake-flag
  'rspec-use-rake-when-possible "1.7")

(defcustom rspec-rake-command "rake"
  "The command for rake."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-spec-command "rspec"
  "The command for spec."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-use-rvm nil
  "When t, use RVM. Requires rvm.el."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-chruby nil
  "When t, use chruby. Requires chruby.el."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-docker-command "docker-compose run"
  "Docker command to run."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-docker-container "rspec-container-name"
  "Name of the docker container to run rspec in."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-docker-cwd "/app/"
  "Working directory when running inside Docker.  Use trailing slash."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-vagrant-cwd "/vagrant/"
  "Working directory when running inside Vagrant. Use trailing slash."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-use-bundler-when-possible t
  "When t and Gemfile is present, run specs with 'bundle exec'.
Not used when running specs using Zeus or Spring."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-docker-when-possible nil
  "When t and Dockerfile is present, run specs inside Docker container using 'docker exec'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-vagrant-when-possible nil
  "When t and Vagrant file is present, run specs inside Vagrant box using 'vagrant ssh -c'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-zeus-when-possible t
  "When t and .zeus.sock is present, run specs with 'zeus'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-spring-when-possible t
  "When t and spring.pid is present, run specs with 'spring'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-opts-file-when-available t
  "When t, RSpec should use .rspec/spec.opts."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-key-command-prefix  (kbd "C-c ,")
  "The prefix for all rspec related key commands."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-command-options "--format documentation"
  "Default options used with rspec-command."
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-snippets-fg-syntax nil
  "Defines whether to use the full or concise FactoryGirl syntax in snippets.
When the value is neither `full', nor `concise', use the concise syntax if
there's an `include FactoryGirl::Syntax::Methods' statement in spec_helper."
  :type '(choice
          (const full)
          (const concise)
          (const nil))
  :group 'rspec-mode)

(defcustom rspec-compilation-skip-threshold 2
  "Compilation motion commands skip less important messages.
The value can be either 2 -- skip anything less than error, 1 --
skip anything less than warning or 0 -- don't skip any messages.
Note that all messages not positively identified as warning or
info, are considered errors."
  :type '(choice (const :tag "Skip warnings and info" 2)
                 (const :tag "Skip info" 1)
                 (const :tag "No skip" 0))
  :group 'rspec-mode)

(defcustom rspec-expose-dsl-globally nil
  "Defines whether the RSpec DSL is assumed to be exposed
  globally, and so prepend snippets at the top level with
  'RSpec.'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-primary-source-dirs '("app" "lib")
  "List of directories whose names should be omitted when looking
for spec files corresponding to files inside them."
  :type '(repeat string)
  :safe 'listp
  :group 'rspec-mode)

(defcustom rspec-autosave-buffer nil
  "If t save the current buffer when running
`rspec-verify', `rspec-verify-single', `rspec-verify-matching',
`rspec-verify-continue' & `rspec-run-last-failed'."
  :type 'boolean
  :group 'rspec-mode)

;;;###autoload
(define-minor-mode rspec-mode
  "Minor mode for RSpec files

\\{rspec-mode-map}"
  :lighter " RSpec" :keymap `((,rspec-key-command-prefix . rspec-mode-keymap))
  (if rspec-mode
      (progn
        (rspec-set-imenu-generic-expression)
        (when (boundp 'yas-extra-modes)
          (if (fboundp 'yas-activate-extra-mode)
              ;; Yasnippet 0.8.1+
              (yas-activate-extra-mode 'rspec-mode)
            (make-local-variable 'yas-extra-modes)
            (add-to-list 'yas-extra-modes 'rspec-mode)
            (yas--load-pending-jits))))
    (setq imenu-create-index-function 'ruby-imenu-create-index)
    (setq imenu-generic-expression nil)
    (when (boundp 'yas-extra-modes)
      (setq yas-extra-modes (delq 'rspec-mode yas-extra-modes)))))

;;;###autoload
(define-minor-mode rspec-verifiable-mode
  "Minor mode for Ruby files that have specs

\\{rspec-verifiable-mode-map}"
  :lighter "" :keymap `((,rspec-key-command-prefix . rspec-verifiable-mode-keymap)))

;;;###autoload
(define-minor-mode rspec-dired-mode
  "Minor mode for Dired buffers with spec files

\\{rspec-dired-mode-map}"
  :lighter "" :keymap `((,rspec-key-command-prefix . rspec-dired-mode-keymap)))

(defconst rspec-imenu-generic-expression
  '(("Examples"  "^\\( *\\(its?\\|specify\\|example\\|describe\\|context\\|feature\\|scenario\\) +.+\\)" 1))
  "The imenu regex to parse an outline of the rspec file")

(defconst rspec-spec-file-name-re "\\(_\\|-\\)spec\\.rb\\'"
  "The regex to identify spec files")

(defun rspec-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression rspec-imenu-generic-expression))

(defvar rspec-snippets-dir
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "snippets" (file-name-directory current)))
  "The directory containing rspec snippets.")

(defun rspec-install-snippets ()
  "Add `rspec-snippets-dir' to `yas-snippet-dirs' and load snippets from it."
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs rspec-snippets-dir t)
  (yas-load-directory rspec-snippets-dir))

(defun rspec-class-from-file-name ()
  "Guess the name of the class the spec is for."
  (let* ((name (file-relative-name (buffer-file-name)
                                   (rspec-spec-directory (buffer-file-name))))
         (rules `((,rspec-spec-file-name-re . "") ("/" . "::") ("_" . "")))
         (class (capitalize name)))
    (dolist (rule rules)
      (setq class (replace-regexp-in-string (car rule) (cdr rule) class t t)))
    class))

(defun rspec-top-level-desc-p ()
  "Return t if point is on the first \"describe\" block opener."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (not (catch 'found
             (while (re-search-backward "\\_<\\(describe\\|feature\\)\\_>" nil t)
               (unless (nth 8 (syntax-ppss))
                 (throw 'found t))))))))

(defun rspec-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char
     (save-excursion
       (end-of-line)
       (unless (and (re-search-backward "^[[:space:]]*\\(it\\|scenario\\)[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))

(defun rspec-example-pending-p ()
  "True if the example under point is pending. Otherwise false."
  (interactive)
  (save-excursion
    (rspec-beginning-of-example)
    (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)) t)))

(defun rspec-toggle-example-pendingness ()
  "Disable active examples and enables pending examples."
  (interactive)
  (if (rspec-example-pending-p)
      (rspec-enable-example)
    (rspec-disable-example)))

(defun rspec-disable-example ()
  "Disable the example at point."
  (interactive)
  (when (not (rspec-example-pending-p))
    (save-excursion
      (rspec-beginning-of-example)
      (end-of-line)
      (insert "\npending")
      (indent-for-tab-command))))

(defun rspec-enable-example ()
  "Enable the example at point."
  (interactive)
  (when (rspec-example-pending-p)
    (save-excursion
      (rspec-beginning-of-example)
      (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)"
                         (save-excursion (ruby-end-of-block) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point))
                     (save-excursion (forward-line 1) (point))))))

(defun rspec-verify ()
  "Run the specified spec, or the spec file for the current buffer."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name))
                         (rspec-core-options)))

(defun rspec-verify-matching ()
  "Run the specs related to the current buffer. This is more fuzzy that a simple verify."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (rspec-run-multiple-files (rspec-all-related-spec-files (buffer-file-name))
                            (rspec-core-options)))

(defun rspec-run-last-failed ()
  "Run just the specs that failed during the last invocation."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (rspec-run-multiple-files rspec-last-failed-specs (rspec-core-options)))

(defun rspec-verify-continue ()
  "Run the current spec file and the spec files located after it.
This is most useful in combination with the option `--fail-fast',
in long-running test suites."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (let ((current-spec-file (rspec-compress-spec-file
                            (rspec-spec-file-for (buffer-file-name)))))
    (rspec-run-multiple-files
     (cl-loop for file in (rspec-all-spec-files (buffer-file-name))
              when (not (string-lessp file current-spec-file))
              collect file)
     (rspec-core-options))))

(defun rspec-verify-single ()
  "Run the specified example at point."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (rspec-run-single-file
   (cons
    (rspec-spec-file-for (buffer-file-name))
    (save-restriction
      (widen)
      (number-to-string (line-number-at-pos))))
   (rspec-core-options)))

(defun rspec-dired-verify ()
  "Run all specs in the current directory."
  (interactive)
  (rspec-run-single-file (dired-current-directory) (rspec-core-options)))

(defun rspec-dired-verify-single ()
  "Run marked specs or spec at point (works with directories too)."
  (interactive)
  (rspec-compile (rspec-runner-target (dired-get-marked-files))
                 (rspec-core-options)))

(defun rspec-verify-all ()
  "Run the 'spec' rake task for the project of the current file."
  (interactive)
  (rspec-run (rspec-core-options)))

(defun rspec-toggle-spec-and-target ()
  "Switch to the spec or the target file for the current buffer.
If the current buffer is visiting a spec file, switches to the
target, otherwise the spec."
  (interactive)
  (find-file (rspec-spec-or-target)))

(defun rspec-verify-method ()
  "Just like `rspec-verify-single' but tries to find examples for
the method at point."
  (interactive)
  (save-excursion
    (when (rspec--toggle-spec-and-target-find-method
           (lambda () (set-buffer (find-file-noselect (rspec-spec-or-target)))))
      (rspec-verify-single))))

(defun rspec--toggle-spec-and-target-find-method (toggle-function)
  (cl-labels
      ((get-spec-name ()
                      (save-excursion
                        (end-of-line)
                        (or
                         (re-search-backward "\\(?:describe\\|context\\)\s*(?[\s\n]*['\"][#\\.]\\([a-zA-Z_?!]*\\)['\"].*[\n\s)]* ?do" nil t)
                         (error "No method spec before point"))
                        (match-string 1)))
       (get-method-name ()
                        (save-excursion
                          (end-of-line)
                          (or
                           (re-search-backward "def \\(?:self\\)?\\(.?[a-zA-Z_?!]+\\)" nil t)
                           (error "No method definition before point"))
                          (match-string 1))))
    (let* ((spec-p (rspec-buffer-is-spec-p))
           (target-regexp (if spec-p
                              (format "def \\(self\\)?\\.?%s" (regexp-quote (get-spec-name)))
                            (format "\\(describe\\|context\\)[\s(\n]+['\"]#?%s['\"]" (regexp-quote (get-method-name))))))
      (funcall toggle-function)
      (if (string-match-p target-regexp (buffer-string))
          (progn
            (goto-char (point-min))
            (re-search-forward target-regexp))
        (message "No matching %s" (if spec-p "method" "spec"))
        nil))))

(defun rspec-toggle-spec-and-target-find-example ()
  "Just like `rspec-toggle-spec-and-target' but tries to toggle between
the method and its corresponding examples."
  (interactive)
  (rspec--toggle-spec-and-target-find-method 'rspec-toggle-spec-and-target))

(defun rspec-find-spec-or-target-other-window ()
  "Find in the other window the spec or the target file.
If the current buffer is visiting a spec file, finds the target,
otherwise the spec."
  (interactive)
  (find-file-other-window (rspec-spec-or-target)))

(defun rspec-find-spec-or-target-find-example-other-window ()
  "Find in the other window the spec or the target file, and try
to navigate to the example or method corresponding to point."
  (interactive)
  (rspec--toggle-spec-and-target-find-method 'rspec-find-spec-or-target-other-window))

(defun rspec-spec-or-target ()
  (if (rspec-buffer-is-spec-p)
      (rspec-target-file-for (buffer-file-name))
    (rspec-spec-file-for (buffer-file-name))))

(defun rspec-spec-file-for (a-file-name)
  "Find spec for the specified file."
  (if (rspec-spec-file-p a-file-name)
      a-file-name
    (let ((replace-regex (if (rspec-target-in-holder-dir-p a-file-name)
                             "^\\.\\./[^/]+/"
                           "^\\.\\./"))
          (relative-file-name (file-relative-name a-file-name (rspec-spec-directory a-file-name))))
      (rspec-specize-file-name (expand-file-name (replace-regexp-in-string replace-regex "" relative-file-name)
                                                 (rspec-spec-directory a-file-name))))))

(defun rspec-target-in-holder-dir-p (a-file-name)
  (string-match (concat "^" (concat
                             (regexp-quote
                              (rspec-project-root a-file-name))
                             (regexp-opt rspec-primary-source-dirs)
                             "/"))
                a-file-name))

(defun rspec-target-file-for (a-spec-file-name)
  "Find the target for A-SPEC-FILE-NAME."
  (cl-loop for extension in (list "rb" "rake")
           for candidate = (rspec-targetize-file-name a-spec-file-name
                                                       extension)
           for filename = (cl-loop for dir in (cons "."
                                                    rspec-primary-source-dirs)
                                   for target = (replace-regexp-in-string
                                                 "/spec/"
                                                 (concat "/" dir "/")
                                                 candidate)
                                   if (file-exists-p target)
                                   return target)
           if filename
           return filename))

(defun rspec-specize-file-name (a-file-name)
  "Return A-FILE-NAME but converted in to a spec file name."
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.\\(rb\\|rake\\)\\)?$" "_spec.rb" (file-name-nondirectory a-file-name))))

(defun rspec-targetize-file-name (a-file-name extension)
  "Return A-FILE-NAME but converted into a non-spec file name with EXTENSION."
  (concat (file-name-directory a-file-name)
          (rspec-file-name-with-default-extension
           (replace-regexp-in-string "_spec\\.rb" (concat "." extension)
                                     (file-name-nondirectory a-file-name)))))

(defun rspec-file-name-with-default-extension (a-file-name)
  "Add .rb file extension to A-FILE-NAME if it does not already have an extension."
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))

(defun rspec-parent-directory (a-directory)
  "Returns the directory of which A-DIRECTORY is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun rspec-root-directory-p (a-directory)
  "Return t if A-DIRECTORY is the root."
  (equal a-directory (rspec-parent-directory a-directory)))

(defun rspec-spec-directory (a-file)
  "Return the nearest spec directory that could contain specs for A-FILE."
  (if (file-directory-p a-file)
      (or
       (car (directory-files a-file t "^spec$"))
       (if (rspec-root-directory-p a-file)
           nil
         (rspec-spec-directory (rspec-parent-directory a-file))))
    (rspec-spec-directory (rspec-parent-directory a-file))))

(defun rspec-all-related-spec-files (a-file)
  (let* ((expected-name (file-name-nondirectory (rspec-spec-file-for a-file)))
         (expected-spec-file (concat "/" expected-name)))
    (cl-loop for file in (rspec-all-spec-files a-file)
             when (string-match-p expected-spec-file file)
             collect file)))

(defun rspec-all-files-under-directory (dir)
  (let ((files (file-expand-wildcards (concat dir "/*") nil)))
    (if (null files)
        files
      (delete-dups
       (append files
               (rspec-all-files-under-directory (concat dir "/*")))))))

(defun rspec-compress-spec-file (a-file)
  (file-relative-name a-file (rspec-project-root)))

(defun rspec-all-spec-files (a-file)
  (mapcar 'rspec-compress-spec-file
          (sort (cl-loop for file in (rspec-all-files-under-directory
                                      (rspec-spec-directory a-file))
                         when (rspec-spec-file-p file)
                         collect file)
                'string-lessp)))

(defun rspec-spec-file-p (a-file-name)
  "Return true if the specified A-FILE-NAME is a spec."
  (numberp (string-match rspec-spec-file-name-re a-file-name)))

(defun rspec-core-options ()
  "Return string of options that instructs spec to use options
file if it exists, or sensible defaults otherwise."
  (cond ((and rspec-use-opts-file-when-available
              (file-readable-p (rspec-spec-opts-file)))
         (concat "--options " (rspec--shell-quote-local (rspec-spec-opts-file))))
        (t rspec-command-options)))

(defun rspec-bundle-p ()
  (and rspec-use-bundler-when-possible
       (file-readable-p (concat (rspec-project-root) "Gemfile"))))

(defun rspec-docker-p ()
  (and rspec-use-docker-when-possible
       (file-readable-p (concat (rspec-project-root) "Dockerfile"))))

(defun rspec-vagrant-p ()
  (and rspec-use-vagrant-when-possible
       (file-readable-p (concat (rspec-project-root) "Vagrantfile"))))

(defun rspec-zeus-file-path ()
  (or (getenv "ZEUSSOCK")
      (concat (rspec-project-root) ".zeus.sock")))

(defun rspec-zeus-p ()
  (and rspec-use-zeus-when-possible
       (file-exists-p (rspec-zeus-file-path))))

(defun rspec-rake-p ()
  (and rspec-use-rake-when-possible
       ;; Looks inefficient, but the calculation of the root is quite
       ;; fast. Unless this is used over TRAMP, I suppose.
       (not (or (rspec-spring-p) (rspec-zeus-p)))
       (file-exists-p (concat (rspec-project-root) "Rakefile"))))

(defun rspec-spring-p ()
  (and rspec-use-spring-when-possible
       (let ((root (directory-file-name (rspec-project-root))))
         (or
          ;; Older versions
          (file-exists-p (format "%s/tmp/spring/spring.pid" root))
          ;; 0.9.2+
          (file-exists-p (format "%s/spring/%s.pid" temporary-file-directory (md5 root)))
          ;; 1.2.0+
          (let* ((path (or (getenv "XDG_RUNTIME_DIR") temporary-file-directory))
                 (ruby-version (shell-command-to-string "ruby -e 'print RUBY_VERSION'"))
                 (application-id (md5 (concat ruby-version root))))
            (or
             (file-exists-p (format "%s/spring/%s.pid" path application-id))
             ;; 1.5.0+
             (file-exists-p (format "%s/spring-%s/%s.pid" path (user-real-uid) application-id))))))))

(defun rspec2-p ()
  (or (string-match "rspec" rspec-spec-command)
      (file-readable-p (concat (rspec-project-root) ".rspec"))))

(defun rspec-spec-opts-file ()
  "Return filename of spec opts file."
  (if (rspec2-p)
      (expand-file-name ".rspec" (rspec-project-root))
    (expand-file-name "spec.opts" (rspec-spec-directory (rspec-project-root)))))

(defun rspec--shell-quote-local (file)
  (let ((remote (file-remote-p file))
        (docker (rspec-docker-p))
        (vagrant (rspec-vagrant-p)))
    (shell-quote-argument
     (cond
      (remote (substring file (length remote)))
      (docker (replace-regexp-in-string (regexp-quote (rspec-project-root))
                                         rspec-docker-cwd file))
      (vagrant (replace-regexp-in-string (regexp-quote (rspec-project-root))
                                         rspec-vagrant-cwd file))
      (t  file)))))

(defun rspec--docker-wrapper (command)
  (if (rspec-docker-p)
      (format "%s %s bash -c \"%s\""
              rspec-docker-command
              rspec-docker-container
              command)
    command))

(defun rspec--vagrant-wrapper (command)
  (if (rspec-vagrant-p)
      (format "vagrant ssh -c 'cd %s; %s'"
              (shell-quote-argument rspec-vagrant-cwd)
              command)
    command))

(defun rspec-runner ()
  "Return command line to run rspec."
  (let ((bundle-command (if (rspec-bundle-p) "bundle exec " ""))
        (zeus-command (if (rspec-zeus-p) "zeus " nil))
        (spring-command (if (rspec-spring-p) "spring " nil)))
    (concat (or zeus-command spring-command bundle-command)
            (if (rspec-rake-p)
                (concat rspec-rake-command " spec")
              rspec-spec-command))))

(defun rspec-runner-options (&optional opts)
  "Return string of options from OPTS for command line."
  (let ((opts (if (listp opts)
                  opts
                (list opts)))
        (use-rake (rspec-rake-p)))
    (concat (when use-rake "SPEC_OPTS=\'")
            (mapconcat 'identity opts " ")
            (when use-rake "\'"))))

(defun rspec-runner-target (target)
  "Processes TARGET to pass it to the runner.
TARGET can be a file, a directory, a list of such,
or a cons (FILE . LINE), to run one example."
  (let ((use-rake (rspec-rake-p)))
    (concat (when use-rake "SPEC=\'")
            (if (listp target)
                (if (listp (cdr target))
                    (mapconcat #'rspec--shell-quote-local target " ")
                  (concat (rspec--shell-quote-local (car target))
                          ":"
                          (cdr target)))
              (rspec--shell-quote-local target))
            (when use-rake "\'"))))

;;;###autoload
(defun rspec-buffer-is-spec-p ()
  "Return true if the current buffer is a spec."
  (and (buffer-file-name)
       (rspec-spec-file-p (buffer-file-name))))

(defun rspec-run (&optional opts)
  "Run spec with the specified options OPTS."
  (rspec-compile (rspec-runner-target
                  (rspec-spec-directory (rspec-project-root)))
                 opts))

(defun rspec-run-single-file (spec-file &rest opts)
  "Run spec on SPEC-FILE with the specified options OPTS."
  (rspec-compile (rspec-runner-target spec-file) opts))

(defun rspec-run-multiple-files (spec-files &rest opts)
  "Run spec on a list of SPEC-FILES with the specified options OPTS."
  (if (null spec-files)
      (message "No spec files found!")
    (rspec-compile (rspec-runner-target spec-files) opts)))

(defvar rspec-last-failed-specs nil
  "The file and line number of the specs that failed during the last run.")

(defvar rspec-last-directory nil
  "Directory the last spec process ran in.")

(defvar rspec-last-arguments nil
  "Arguments passed to `rspec-compile' at the last invocation.")

(defun rspec-rerun ()
  "Re-run the last RSpec invocation."
  (interactive)
  (if (not rspec-last-directory)
      (error "No previous verification")
    (let ((default-directory rspec-last-directory))
      (apply #'rspec-compile rspec-last-arguments))))

(defun rspec-yank-last-command ()
  "Yank the last RSpec command to the clipboard."
  (interactive)
  (if (not rspec-last-directory)
      (error "No previous verification")
    (let ((default-directory rspec-last-directory))
      (kill-new (apply #'rspec-compile-command rspec-last-arguments)))))

(defun rspec-compile (target &optional opts)
  "Run a compile for TARGET with the specified options OPTS."
  (setq rspec-last-directory default-directory
        rspec-last-arguments (list target opts))

  (if rspec-use-rvm
      (rvm-activate-corresponding-ruby))

  (if rspec-use-chruby
      (chruby-use-corresponding))

  (let ((default-directory (or (rspec-project-root) default-directory)))
    (compile
     (rspec-compile-command target opts)
     'rspec-compilation-mode)))

(defun rspec-compile-command (target &optional opts)
  "Composes RSpec command line for the compile function"
  (rspec--vagrant-wrapper
    (rspec--docker-wrapper
    (mapconcat 'identity `(,(rspec-runner)
                            ,(rspec-runner-options opts)
                            ,target) " "))))

(defvar rspec-compilation-mode-font-lock-keywords
  '((compilation--ensure-parse)
    ("^\\(Pending\\|Failures\\):$"
     (0 font-lock-function-name-face))
    ("^[0-9]+ examples?, 0 failures.*$"
     (0 compilation-info-face))
    ("^[0-9]+ examples?, \\([0-9]+ failures?\\)"
     (1 compilation-error-face))))

(defvar rspec-compilation-error-regexp-alist-alist
  '((rspec-capybara-html "screenshot: file://\\([0-9A-Za-z@_./\:-]+\\.html\\)" 1 nil nil 0 1)
    (rspec-capybara-screenshot "screenshot: file://\\([0-9A-Za-z@_./\:-]+\\.png\\)" 1 nil nil 0 1)
    (rspec "^ +# \\([0-9A-Za-z@_./:-]+\\.rb\\):\\([0-9]+\\):in" 1 2 nil 2 1)
    (rspec-pendings "^ +# \\([0-9A-Za-z@_./:-]+\\.rb\\):\\([0-9]+\\)" 1 2 nil 1 1)
    (rspec-summary "^rspec \\([0-9A-Za-z@_./:-]+\\.rb\\):\\([0-9]+\\)" 1 2 nil 2 1)))

(defvar rspec-compilation-error-regexp-alist
  (mapcar 'car rspec-compilation-error-regexp-alist-alist))

(define-compilation-mode rspec-compilation-mode "RSpec Compilation"
  "Compilation mode for RSpec output."
  (add-hook 'compilation-filter-hook 'rspec-colorize-compilation-buffer nil t)
  (add-hook 'compilation-finish-functions 'rspec-store-failures nil t)
  (add-hook 'compilation-finish-functions 'rspec-handle-error nil t))

(defun rspec-store-failures (&rest ignore)
  "Store the file and line number of the failed examples from this run."
  (let (failures)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^rspec \\([0-9A-Za-z@_./:-]+\\.rb:[0-9]+\\)" nil t)
        (push (match-string-no-properties 1) failures)))
    (setq rspec-last-failed-specs (reverse failures))))

(defun rspec-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun rspec-handle-error (&rest ignore)
  (save-excursion
    (goto-char (point-max))
    (when (save-excursion
            (forward-line -10)
            (search-forward "`+' for LL():Rake::Scope::EmptyScope" nil t))
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t)
            (url "https://github.com/pezra/rspec-mode/issues/84"))
        (insert (format "\n%s\n"
                        (propertize
                         "You seem to be using Rake 0.9. Rake 10 is recommended."
                         'font-lock-face 'error)))
        (insert "See ")
        (insert-text-button url 'type 'help-url 'help-args (list url))
        (insert ".\n")))))

(defun rspec-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((rspec-root-directory-p directory)
           (error "Could not determine the project root."))
          ((file-exists-p (expand-file-name "Rakefile" directory)) (expand-file-name directory))
          ((file-exists-p (expand-file-name "Gemfile" directory)) (expand-file-name directory))
          ((file-exists-p (expand-file-name "Berksfile" directory)) (expand-file-name directory))
          (t (rspec-project-root (file-name-directory (directory-file-name directory)))))))

(defun rspec--include-fg-syntax-methods-p ()
  "Check whether FactoryGirl::Syntax::Methods is included in rails_helper or spec_helper."
  (cl-case rspec-snippets-fg-syntax
    (full nil)
    (concise t)
    (t
     (cl-find-if
      (lambda (path)
        (let ((expanded-path (expand-file-name path (rspec-project-root))))
          (when (file-exists-p expanded-path)
            (with-temp-buffer
              (insert-file-contents expanded-path)
              (ruby-mode)
              (when (re-search-forward "include +FactoryGirl::Syntax::Methods" nil t)
                (not (nth 4 (syntax-ppss))))))))
      '("spec/rails_helper.rb" "spec/spec_helper.rb")))))

(defun rspec--autosave-buffer-maybe ()
  "Saves the current buffer if `rspec-autosave-buffer' is t and
the buffer is a spec or a target file."
  (when (and rspec-autosave-buffer (rspec-spec-or-target))
    (save-buffer)))

(defun rspec-snippets-fg-method-call (method)
  "Return FactoryGirl method call for METHOD, for use in snippets.
Looks at FactoryGirl::Syntax::Methods usage in spec_helper."
  (if (rspec--include-fg-syntax-methods-p)
      method
    (concat "FactoryGirl." method)))

;;;###autoload
(defun rspec-enable-appropriate-mode ()
  (if (rspec-buffer-is-spec-p)
      (rspec-mode)
    (rspec-verifiable-mode)))

;; Hook up all Ruby buffers.
;;;###autoload
(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
  (add-hook hook 'rspec-enable-appropriate-mode))

;; Add verify related spec keybinding to rails minor mode buffers
;;;###autoload
(add-hook 'rails-minor-mode-hook 'rspec-verifiable-mode)

(provide 'rspec-mode)
;;; rspec-mode.el ends here
