;;; eproject.el --- assign files to projects, programatically
;;
;; Copyright (C) 2008, 2009 Jonathan Rockway <jon@jrock.us>
;;
;; Author: Jonathan Rockway <jon@jrock.us>
;; Maintainer: Jonathan Rockway <jon@jrock.us>
;; Created: 20 Nov 2008
;; Version: 1.5
;; Package-Requires: ((helm "1.6.4"))
;; Keywords: programming, projects
;;
;; This file is not a part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Eproject is an extension that lets you group related files together
;; as projects.  It aims to be as unobtrusive as possible -- no new
;; files are created (or required to exist) on disk, and buffers that
;; aren't a member of a project are not affected in any way.
;;
;; The main starting point for eproject is defining project types.
;; There is a macro for this, define-project-type, that accepts four
;; arguments, the type name (a symbol), a list of supertypes (for
;; inheriting properties), a form that is executed to determine
;; whether a file is a member of a project, and then a free-form
;; property list.  An example will clear things up.
;;
;; Let's create a "perl" project type, for Perl projects that have a
;; Makefile.PL.
;;
;; (define-project-type perl (generic)
;;   (look-for "Makefile.PL")
;;   :relevant-files ("\\.pm$" "\\.t$"))
;;
;; Now when you open a file and somewhere above in the directory tree
;; there is a Makefile.PL, it will be a "perl project".
;;
;; There are a few things you get with this.  A hook called
;; perl-project-file-visit-hook will be run, and the buffer will have
;; the "eproject-mode" minor-mode turned on.  You can also read and
;; set metadata via the eproject-attribute and
;; eproject-add-project-metadatum calls.
;;
;; (This is mostly helpful to Lisp programmers rather than end-users;
;; if you want tools for visiting and managing projects (and ibuffer
;; integration), load `eproject-extras'.  These extras are great
;; examples of the eproject API in action, so please take a look even
;; if you don't want those exact features.)
;;
;; Let's look at the mechanics of the define-project-type call.  The
;; first argument is the name of the project type -- it can be any
;; symbol.  The next argument is a list of other projects types that
;; this project will inherit from.  That means that if you call
;; eproject-get-project-metadatum and the current project doesn't
;; define a value, we'll look at the supertypes until we get something
;; non-nil.  Usually you will want to set this to (generic), which
;; will make your type work correctly even if you don't define any of
;; your own metadata.
;;
;; The next argument is a form that will be executed with the filename
;; that was just opened bound to FILE.  It is expected to return the
;; project root, or nil if FILE is not in a project of this type.  The
;; look-for function will look up the directory tree for a file that
;; is named the same as its argument (see the docstring for
;; `eproject--look-for-impl' for all the details).  You can write any
;; Lisp here you like; we'll see some more examples later.  (You only
;; get one form, so if you need to execute more than one, just wrap it
;; in a progn.)
;;
;; The final (&rest-style) argument is a property list of initial project
;; metadata.  You can put anything you want here, as long as it is in the
;; form of a property list (keyword, value, keyword, value, ...).
;;
;; After this form runs, eproject will be able to recognize files in
;; the type of the project you defined.  It also creates a hook named
;; <type>-project-file-visit-hook.  You can do anything you want here,
;; including access (eproject-type) and (eproject-root).
;;
;; As an example, in my perl-project-file-visit-hook, I do this:
;;
;; (lambda ()
;;   (ignore-errors
;;     (stylish-repl-eval-perl
;;      (format "use lib '%s'" (car (perl-project-includes)))))))
;;
;; This will add the library directory of this project to my current
;; stylish-repl session, so that I can use my project in the REPL
;; immediately.  (I do something similar for Lisp + SLIME projects)
;;
;; That's basically all there is.  eproject is designed to be minimal and
;; extensible, so I hope it meets your needs.
;;
;; Please e-mail me or find me on #emacs (jrockway) if you have
;; questions.  If you'd like to send a patch (always appreciated),
;; please diff against the latest git version, available by running:
;;
;; $ git clone git://github.com/jrockway/eproject
;;
;; Share and enjoy.

;;; Public API:

;; eproject-root (&optional buffer)
;;
;; - returns the project root for the project that buffer is a member
;;   of. defaults to the current buffer

;; eproject-attribute (key &optional root)
;;
;; - returns the value of key for the project that buffer is a member
;;   of.  root defaults to the current buffer's eproject-root

;; eproject-list-project-files

;; define-project-type

;; define-project-attribute

;; eproject-projects

;; Everything else is mostly used internally, and may change.

;;; Public commands:

;; eproject-maybe-turn-on
;;
;; - turn on eproject for the current buffer, if possible
;;   (if it's turned on, the hooks will be run)

;; eproject-reinitialize-project
;;
;; - re-read config for the current project, then run
;;   eproject-maybe-turn-on
;;
;;   this is bound to C-c C-c when editing .eproject files, which is
;;   very convenient for testing.

;; See eproject-extras.el for more interesting / useful commands.
;; This file is mostly "plumbing".

;;; Bugs:
;;
;; You can't forward reference supertypes -- this will mess things up
;; internally, but you won't get a warning.  This can be easily fixed
;; by using a smarter algorithm for eproject--all-types.
;;
;; The "linearized isa" (i.e. "class precedence list") is computed
;; with a depth-first search.  This is bad; we should really use the
;; C3 ordering.

;;; Website:
;;
;; The latest version is on github at
;; http://github.com/jrockway/eproject/tree/master
;;
;; The wiki has lots more documentation:
;; http://wiki.github.com/jrockway/eproject
;;

;;; The Changelog section documents major changes.  Minor non-breaking
;;; updates are regularly committed to git.

;;;  Changelog:
;;
;; 1.6 (Sat Aug 28 22:21:39 CDT 2010)
;;
;; * Remove eproject-project-names variable and add some proper
;;   introspection for project sets.
;;
;; 1.5 (Thu May 28 21:38:08 MST 2009)
;;
;; * Split out the non-core stuff into eproject-extras.el.
;;   (slime-contrib style)
;;
;; 1.4 (Thu May 28 02:21:40 MST 2009)
;;
;; * Add support for "instance" metadata, instead of "class" (project)
;;   metadata
;;
;; 1.3 (Wed May 27 20:47:48 MST 2009)
;;
;; * Officially support w32
;;
;; 1.2 (Thu May  7 02:18:01 CDT 2009)
;;
;; * Add ibuffer support
;;
;; 1.1 (Sat Jan 31 20:03:56 CST 2009)
;;
;; * Make the completing-read function customizable
;;
;; 1.0 (Nov 28 2008)
;;
;; * Initial release
;;

;;; Code:

(require 'cl)
(require 'eshell) ;; For portable path handling

(defgroup eproject nil
  "Eproject; provide support for grouping files and buffers into projects"
  :prefix "eproject-"
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "eproject.el")
  :link '(emacs-library-link :tag "Optional extras" "eproject-extras.el")
  :link '(url-link :tag "Github wiki" "http://wiki.github.com/jrockway/eproject"))

(defcustom eproject-keybind-prefix
  "C-c"
  "The keybind prefix for eproject"
  :type 'string
  :group 'eproject)

(defvar eproject-root nil
  "A buffer-local variable set to the root of its eproject
  project.  NIL if it isn't in an eproject.  Your code should
  call the function `eproject-root` instead of accessing this
  variable directly.  It should also not set it; only
  `eproject-maybe-turn-on' can do that.")

(make-variable-buffer-local 'eproject-root)

(defvar eproject-project-types nil
  "An alist of project type name to (supertypes selector metadata-plist) pairs.")

(defvar eproject-extra-attributes nil
  "A list of pairs used to assign attributes to projects.

Each entry can be in the form of `(FUNCTION (ATTRIBUTES))'
or `((KEY . TYPE) (ATTRIBUTES))'.

If FUNCTION is specified, it will be evaluated for each project
root.  If it returns a non-nil value, ATTRIBUTES will be added to
the project attributes.

If `(KEY . TYPE)' is specified, then TYPE is either
`:root-regexp' or `:project-name' and KEY is interpreted
accordingly.  If KEY matches a project root, its ATTRIBUTES are
applied.

ATTRIBUTES is a plist of attributes.")

(defvar eproject-attributes-alist nil
  "An alist of project root -> plist of project metadata.")

(defvar eproject-first-buffer-hook nil
  "Hook to run when the first buffer in a new project is opened.
  Called after the project is initialized, so it's safe to call
  eproject functions.")

(defvar eproject-projects-hook nil
  "Hook that's run when a list of projects is requested.  Hook may return a list of new (name . root) pairs to be added to eproject's internal list.")

(defvar eproject-project-change-hook nil
  "Hook that's run when a project is changed; currently this means when a file in the project is saved.")

(defun define-project-attribute (key attributes)
  "Define extra attributes to be applied to projects.

See `eproject-extra-attributes' for details on the format of KEY
and ATTRIBUTES."
  (check-type key (or function cons))
  (check-type attributes list)
  (add-to-list 'eproject-extra-attributes (list key attributes)))

(defmacro define-project-type (type supertypes selector &rest metadata)
  "Define a new project type TYPE that inherits from SUPERTYPES.

SELECTOR is a form that is given a filename FILE and returns the
project root if it is of this type of project, or NIL otherwise.

Optional argument METADATA is a plist of metadata that will
become project attributes."
  `(progn
     (defvar ,(intern (format "%s-project-file-visit-hook" type)) nil
       ,(format "Hooks that will be run when a file in a %s project is opened." type))
       (setq eproject-project-types
             (nconc (assq-delete-all ',type eproject-project-types)
                    (list
                     (list ',type ',supertypes
                           (lambda (file) ,selector)
                           ',metadata))))))

(defun eproject--build-parent-candidates (start-at)
  "Given directory START-AT, return a list of parent directories, including START-AT."
    (loop for x on (reverse (eshell-split-path start-at)) by #'cdr
          ;; i think eshell-split-path guarantees the
          ;; file-name-as-directory application, but i don't want to
          ;; debug it if it doesn't :)
          collect (file-name-as-directory (apply #'concat (reverse x)))))

(defun eproject--scan-parents-for (start-at predicate)
  "Call PREDICATE with each parent directory of START-AT, returning the path to the first directory where PREDICATE returns T."
  (find-if predicate (eproject--build-parent-candidates
                          (file-name-as-directory start-at))))

(defun eproject--find-file-named (start-at filename)
  "Starting in directory START-AT, recursively check parent directories for a file named FILENAME.  Return the directory where the file is first found; return NIL otherwise."
  (eproject--scan-parents-for start-at
   (lambda (directory) ; note that directory always has the path separator on the end
     (file-exists-p (concat directory filename)))))

;; TODO: sugar around lambda/lambda, which is ugly
(define-project-type generic () nil
  :relevant-files (".*")
  :irrelevant-files ("^[.]" "^[#]")
  :file-name-map (lambda (root) (lambda (root file) file))
  :local-variables (lambda (root) (lambda (root file) nil))
  :config-file ".eproject")

(define-project-type generic-eproject (generic) (look-for ".eproject"))

(define-project-type generic-git (generic) (look-for ".git")
  :irrelevant-files ("^[.]" "^[#]" ".git/"))

(define-project-type generic-hg (generic) (look-for ".hg")
  :irrelevant-files ("^[.]" "^[#]" ".hg/"))

(define-project-type generic-bzr (generic) (look-for ".bzr")
  :irrelevant-files ("^[.]" "^[#]" ".bzr/"))

(define-project-type generic-darcs (generic) (look-for "_darcs")
  :irrelevant-files ("^[.]" "^[#]" "_darcs/"))

(defun eproject--type-info (type)
  (or
   (assoc type eproject-project-types)
   (error "No type %s" type)))

(defun eproject--project-supertypes (type)
  (nth 1 (eproject--type-info type)))

(defun eproject--project-selector (type)
  (nth 2 (eproject--type-info type)))

(defun* eproject--look-for-impl (file expression &optional (type :filename))
  "Implements the LOOK-FOR function that is flet-bound during
`eproject--run-project-selector'.  EXPRESSION and TYPE specify
what to look for.  Some examples:

   (look-for \"Makefile.PL\") ; look up the directory tree for a file called Makefile.PL
   (look-for \"*.PL\" :glob) ; look for a file matching *.PL
"
  (case type
    (:filename (eproject--find-file-named file expression))
    (:glob (eproject--scan-parents-for (file-name-directory file)
             (lambda (current-directory)
               (let ((default-directory current-directory))
                 (and (not (equal file current-directory))
                      (> (length (file-expand-wildcards expression)) 0))))))
    (otherwise (error "Don't know how to handle %s in LOOK-FOR!" type))))

(defun eproject--buffer-file-name ()
  (or (buffer-file-name) (and (derived-mode-p 'dired-mode)
                              (expand-file-name (if (consp dired-directory)
                                                    (car dired-directory)
                                                  dired-directory)))))

(defun* eproject--run-project-selector (type &optional (file (eproject--buffer-file-name)))
  "Run the selector associated with project type TYPE."
  (when (not file)
    (error "Buffer '%s' has no file name" (current-buffer)))
  (flet ((look-for (expr &optional (expr-type :filename))
                   (funcall #'eproject--look-for-impl file expr expr-type)))
    (funcall (eproject--project-selector type) file)))

(defun eproject--linearized-isa (type &optional include-self)
  (delete-duplicates
   (append
    (if include-self (list type))
    (eproject--project-supertypes type)
    (loop for stype in (eproject--project-supertypes type)
          append (eproject--linearized-isa stype)))))

(defun eproject--all-types ()
  ;; this should be most specific to least specific, as long as nothing
  ;; is forward-referenced.
 (reverse (mapcar #'car eproject-project-types)))

;; metadata vs. attributes:
;; * metadata is per-project-type
;; * attributes are per-project-root (and includes the project-type metadata)
(defun eproject--compute-all-applicable-metadata (type)
  (loop for next-type in (eproject--linearized-isa type t)
        append (nth 3 (eproject--type-info next-type))))

(defun eproject-get-project-metadatum (type key)
  (getf (eproject--compute-all-applicable-metadata type) key))

(defun eproject-add-project-metadatum (type key value)
  (setf (getf (nth 3 (assoc type eproject-project-types)) key) value))

(defmacro* eproject--do-in-buffer ((buffer) &body forms)
  `(with-current-buffer ,buffer
     (when (not eproject-mode)
       (error "Buffer is not an eproject buffer!"))
     ,@forms))

(defun* eproject-root (&optional (buffer (current-buffer)))
  "Return the value of the eproject variable root.
BUFFER defaults to the current buffer"
  (eproject--do-in-buffer (buffer) eproject-root))

(defun* eproject-attribute (key &optional (root (eproject-root)))
  "Lookup the attribute KEY for the eproject ROOT
ROOT defaults to the current buffer's project-root."
  (getf (cdr (assoc root eproject-attributes-alist)) key))

(defun eproject--known-project-roots ()
  "Return a list of projects roots that have been visisted this session."
  (loop for (key . value) in eproject-attributes-alist collect key))

(defmacro define-eproject-accessor (variable)
  "Create a function named eproject-VARIABLE that return the value of VARIABLE in the context of the current project."
  (let ((sym (intern (format "eproject-%s" variable))))
  `(defun* ,sym
       (&optional (buffer (current-buffer)))
     ,(format "Return the value of the eproject variable %s.  BUFFER defaults to the current buffer." variable)
     (eproject-attribute ,(intern (format ":%s" variable))))))

(define-eproject-accessor type)
(define-eproject-accessor name)

(defun eproject-reinitialize-project ()
  "Forget all project settings for the current eproject, then reload them."
  (interactive)
  (let ((root (eproject-root)))
    (setf eproject-attributes-alist
          (delete-if (lambda (x) (equal (car x) root))
                     eproject-attributes-alist)))
  (eproject-maybe-turn-on)
  (if (ignore-errors (eproject-root))
      (message "Project `%s' reinitialized successfully." (eproject-name))
    (message "Error reinitializing project!")))

(defun eproject--maybe-reinitialize ()
  "Run by `eproject-project-change-hook' to reinit the project after .eproject is modified."
  (when (and (eq major-mode 'dot-eproject-mode)
             (boundp 'eproject-root) eproject-root)
    (eproject-reinitialize-project)))

(defun eproject--eval-user-data (project-name root)
  "Interpret EPROJECT-EXTRA-ATTRIBUTES for PROJECT-NAME (in ROOT)."
  (loop for (key attributes) in eproject-extra-attributes append
        (cond ((functionp key)
               (if (funcall key root) attributes nil))
              ((not (listp key))
               (error "Bad eproject user data (%s %s), %s must be a list/function"
                      key attributes key))
              ((and (eq (cdr key) :project-name)
                    (equal (car key) project-name))
               attributes)
              ((and (eq (cdr key) :root-regexp)
                    (string-match (car key) root))
               attributes)
              (t nil))))

(defun eproject--interpret-metadata (data root)
  "Interpret DATA with respect to ROOT.

This mostly means evaluating functions and passing everything
else through unchanged."
  (loop for i in data collect (if (functionp i) (funcall i root) i)))

(defun eproject--init-attributes (root type)
  "Update the EPROJECT-ATTRIBUTES-ALIST for the project rooted at ROOT (of TYPE)."
  (let ((project-data (assoc root eproject-attributes-alist)))
    (when (null project-data)
      (let* ((class-data (eproject--interpret-metadata
                          (eproject--compute-all-applicable-metadata type)
                          root))

             ;; read the .eproject (or whatever) file
             (config-file
              (concat root (getf class-data :config-file ".eproject")))
             (config-file-contents
              (with-temp-buffer
                (ignore-errors (insert-file-contents config-file nil nil nil t))
                (buffer-substring-no-properties (point-min) (point-max))))
             (config-file-sexp
              (read (format "(list %s)" config-file-contents)))
             (data-is-unsafe (unsafep config-file-sexp))
             (config-file-data
              (cond (data-is-unsafe
                     (warn "Config file %s contains unsafe data (%s), ignoring!"
                      config-file data-is-unsafe)
                     nil)
                    (t (let ((data (eval config-file-sexp)))
                         (if data (nconc
                                   (list :loaded-from-config-file config-file)
                                   data)
                           nil)))))

             ;; combine class and config data; config overriding class
             (class-and-config-data (cond
                    ;; ensure that the config-file-data is really a plist
                    ((evenp (length config-file-data))
                     (nconc config-file-data class-data))
                    (t class-data)))

             ;; calculate the project name, as it's used by "user
             ;; data"

             ;; backcompat note: not sure why i looked in
             ;; :project-name for the value to set the :name attribute
             ;; to.  so now we look in both, preferring the new way.
             (name (or (getf class-and-config-data :name)
                       (getf class-and-config-data :project-name)
                       (directory-file-name
                        (elt (reverse (eshell-split-path root)) 0))))

             ;; finally, merge in the "user data"
             (user-data
              (eproject--interpret-metadata
               (eproject--eval-user-data name root) root))

             ;; now compute the final list of attributes
             (data (nconc user-data class-and-config-data)))

        (add-to-list 'eproject-attributes-alist
                     (cons root (nconc (list :type type :name name) data)))))))

(defvar eproject-mode-map (make-sparse-keymap)
  "Keybindings while in eproject-mode")

(define-minor-mode eproject-mode
  "A minor mode for buffers that are a member of an eproject project."
  nil " Project" eproject-mode-map
  (when (null eproject-root)
    (error "Please do not use this directly.  Call eproject-maybe-turn-on instead.")))

(defun eproject-maybe-turn-on ()
  "Turn on eproject for the current buffer, if it is in a project."
  (interactive)
  (let (bestroot besttype (set-before (mapcar #'car eproject-attributes-alist)))
    (loop for type in (eproject--all-types)
          do (let ((root (eproject--run-project-selector type)))
               (when (and root
                          (or (not bestroot)
                              ;; longest filename == best match (XXX:
                              ;; need to canonicalize?)
                              (> (length root) (length bestroot))))
                 (setq bestroot root)
                 (setq besttype type))))
    (when bestroot
      (setq eproject-root (file-name-as-directory bestroot))

      ;; read .eproject file (etc.) and initialize at least :name and
      ;; :type
      (condition-case e
          (eproject--init-attributes eproject-root besttype)
        (error (display-warning 'warning
            (format "There was a problem setting up the eproject attributes for this project: %s" e))))

      ;; with :name and :type set, it's now safe to turn on eproject
      (eproject-mode 1)

      ;; initialize buffer-local variables that the project defines
      ;; (called after we turn on eproject-mode, so we can call
      ;; eproject-* functions cleanly)
      (condition-case e
          (eproject--setup-local-variables)
        (error (display-warning 'warning
          (format "Problem initializing project-specific local-variables in %s: %s"
                  (eproject--buffer-file-name) e))))

      ;; run the first-buffer hooks if this is the first time we've
      ;; seen this particular project root.
      (when (not (member eproject-root set-before))
        (run-hooks 'eproject-first-buffer-hook))

      ;; run project-type hooks, which may also call into eproject-*
      ;; functions
      (mapc (lambda (x) (run-hooks (intern (format "%s-project-file-visit-hook" x))))
            (eproject--project-supertypes besttype))
      (run-hooks (intern (format "%s-project-file-visit-hook" besttype)))

      ;; return the project root; it's occasionally useful for the caller
      bestroot)))

(defun eproject--setup-local-variables ()
  "Setup local variables as specified by the project attribute :local-variables."
  (let* ((var-maker (eproject-attribute :local-variables))
         (vars (cond ((functionp var-maker)
                      (funcall var-maker
                               (eproject-root)
                               (file-relative-name (eproject--buffer-file-name)
                                                   (eproject-root))))
                     ((listp var-maker) var-maker))))
    (loop for (name val) on vars by #'cddr do
          (set (make-local-variable name) val))))

(defun eproject--search-directory-tree (directory file-regexp ignore-regexp)
  (loop for file in (directory-files (file-name-as-directory directory) t "^[^.]" t)
        unless (string-match ignore-regexp file)
          if (not (file-directory-p file))
            when (and (not (string-match ignore-regexp
                                         (file-name-nondirectory file)))
                      (string-match file-regexp file))
              collect file into files end
          else
            collect file into directories
        finally return
          (nconc files
                 (loop for dir in directories
                       nconc (eproject--search-directory-tree dir file-regexp
                                                              ignore-regexp)))))

(defun eproject-assert-type (type)
  "Assert that the current buffer is in a project of type TYPE."
  (when (not (memq type (eproject--linearized-isa (eproject-type) t)))
    (error (format "%s is not in a project of type %s!"
                   (current-buffer) type))))

(defun eproject--combine-regexps (regexp-list)
  "Combine regexps like `regexp-opt', but without quoting anything.
Argument REGEXP-LIST is a list of regexps to combine."
  (format "\\(?:%s\\)"
          (reduce (lambda (a b) (concat a "\\|" b))
                  (mapcar (lambda (f) (format "\\(?:%s\\)" f)) regexp-list))))

;;; TODO: cache this?
(defun eproject--file-check-regexps (root)
  "Return a pair (matcher . ignore) for the project in ROOT."
  (let ((matcher (eproject--combine-regexps
                  (eproject-attribute :relevant-files root)))
        (ignore (eproject--combine-regexps
                 (cons
                  (concat (regexp-opt completion-ignored-extensions t) "$")
                  (eproject-attribute :irrelevant-files root)))))
    (cons matcher ignore)))

(defun* eproject-classify-file (file &optional (root (eproject-root)))
  "Return T if FILE would belong to the project in ROOT.

No check is done to ensure that the root subsumes FILE or even
that FILE is an absolute path."
  ;; XXX: this logic is sort of copied from search-directory-tree.
  ;; maybe combine?
  (destructuring-bind (matcher . ignore) (eproject--file-check-regexps root)
    (and (not (string-match ignore file))
         (not (string-match ignore (file-name-nondirectory file)))
         (string-match matcher file)
         t)))

(defun* eproject-list-project-files (&optional (root (eproject-root)))
  "Return a list of all project files in PROJECT-ROOT."
  (destructuring-bind (matcher . ignore) (eproject--file-check-regexps root)
    (eproject--search-directory-tree root matcher ignore)))

(defun* eproject-list-project-files-relative (&optional (root (eproject-root)))
  (mapcar (lambda (file)
            (file-relative-name file root))
          (eproject-list-project-files root)))

;;; dot-eproject mode

(define-derived-mode dot-eproject-mode emacs-lisp-mode "dot-eproject"
  "Major mode for editing .eproject files."
  (define-key dot-eproject-mode-map (kbd (concat eproject-keybind-prefix " C-c")) #'eproject-reinitialize-project))

;; introspect sets of projects
(defun eproject-projects ()
  "Return a list of (name . root) pairs of all known eproject projects."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for f in eproject-projects-hook do
          (loop for (name . root) in (funcall f)
                do (puthash name root hash)))
    (loop for (root . rest)
          in eproject-attributes-alist
          do (puthash (or (getf rest :name) (getf rest :project-name))
                      root hash))
    (loop for name being each hash-key in hash
          collect (cons name (gethash name hash)))))

(defun eproject-project-names ()
  "Return a list of project names known to eproject."
  (mapcar #'car (eproject-projects)))

;; Finish up
(defun eproject--after-change-major-mode-hook ()
  (when (and (buffer-file-name)
             (not eproject-root))
    (eproject-maybe-turn-on)))

(defun eproject--after-save-hook ()
  ;; TODO: perhaps check against relevant-files or irrelevant-files
  ;; regex?  I'm avoiding this now because I'd rather not force the
  ;; speed hit -- if the user wants to do something slow after save,
  ;; fine... but I'd rather not make the decision for him.
  (when (and (boundp 'eproject-root) eproject-root)
    (run-hooks 'eproject-project-change-hook)))

(add-hook 'find-file-hook #'eproject-maybe-turn-on)
(add-hook 'dired-mode-hook #'eproject-maybe-turn-on)
(add-hook 'after-change-major-mode-hook #'eproject--after-change-major-mode-hook)
(add-hook 'after-save-hook #'eproject--after-save-hook)

(add-hook 'eproject-project-change-hook #'eproject--maybe-reinitialize)

(add-to-list 'auto-mode-alist '("\\.eproject\\'" . dot-eproject-mode))

(provide 'eproject)
;;; eproject.el ends here
