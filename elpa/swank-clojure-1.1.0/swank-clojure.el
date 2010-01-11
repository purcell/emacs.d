;;; swank-clojure.el --- slime adapter for clojure
;;
;; Copyright (C) 2008, 2009 Jeffrey Chu and Phil Hagelberg
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;          Phil Hagelberg <technomancy@gmail.com>
;;
;; URL: http://github.com/technomancy/swank-clojure
;; Version: 1.1.0
;; Keywords: languages, lisp
;; Package-Requires: ((slime-repl "20091016") (clojure-mode "1.6"))
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).
;;
;;; Commentary:
;;
;; The purpose of this file is to set up `slime-lisp-implementations'
;; to allow SLIME to communicate with the Swank server implemented in
;; Clojure. There are four ways to launch a session:
;;
;; 1. Standalone: If you just hit M-x slime, swank-clojure will
;;    download the jars for Clojure, contrib, and swank-clojure,
;;    launch an instance, and connect to it. If you just want to try
;;    out Clojure, this is all you need. Just get Swank Clojure
;;    through ELPA (http://tromey.com/elpa) and stop reading here.
;;
;; 2. Custom classpath: If you want to hack on Clojure or Contrib, set
;;    swank-clojure-classpath to a list of paths to the jars you want to
;;    use and then hit M-x slime.
;;
;; 3. Project: Put your project's dependencies in the lib/ directory,
;;    (either manually or using Leiningen or Maven) then launch
;;    M-x swank-clojure-project. Note that you must have
;;    swank-clojure.jar in the lib/ directory, it will not
;;    automatically add itself to the classpath as it did in past
;;    versions that had to run from a checkout.
;;
;; 4. Standalone Server: Users of leiningen or clojure-maven-plugin
;;    can launch a server from a shell
;;    (http://wiki.github.com/technomancy/leiningen/emacs-integration)
;;    and connect to it from within Emacs using M-x slime-connect.
;;
;;; Code:
;;

(require 'slime)
(require 'clojure-mode)

(defgroup swank-clojure nil
  "SLIME/swank support for clojure"
  :prefix "swank-clojure-"
  :group 'applications)

(defcustom swank-clojure-java-path "java"
  "The location of the java executable"
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-jar-home "~/.swank-clojure/"
  "The directory where the jars necessary to run swank-clojure are kept."
  :type 'string
  :group 'swank-clojure)

(defun swank-clojure-default-classpath ()
  (append
   (when (file-directory-p "~/.clojure")
     (directory-files "~/.clojure" t ".jar$"))
   (when (file-directory-p swank-clojure-jar-home)
     (directory-files swank-clojure-jar-home t ".jar$"))))

(defcustom swank-clojure-classpath
  (swank-clojure-default-classpath)
  "The classpath from which clojure will load from (passed into
java as the -cp argument). On default, it includes all jar files
within ~/.clojure/ and ~/.swank-clojure"
  :type 'list
  :group 'swank-clojure)

;; For backwards-compatibility:
(defvaralias 'swank-clojure-extra-classpaths 'swank-clojure-classpath)

(defcustom swank-clojure-library-paths nil
  "The library paths used when loading shared libraries,
used to set the java.library.path property"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-extra-vm-args nil
  "Extra arguments to be passed to the Java VM when starting clojure.
For example -Xmx512m or -Dsun.java2d.noddraw=true"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-binary nil
  "Used as a binary executable (instead of swank-clojure-java-path) if non-nil."
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-init-files nil
  "If provided, will be used to initialize the REPL environment."
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-compile-p nil
  "Whether to instruct swank-clojure to compile files. Set to nil
  if it's causing you problems."
  :type 'boolean
  :group 'swank-clojure)

(defcustom swank-clojure-deps
  (list (concat "http://repo.technomancy.us/"
                "swank-clojure-1.1.0.jar")
        (concat "http://build.clojure.org/snapshots/org/"
                "clojure/clojure/1.1.0-master-SNAPSHOT/"
                "clojure-1.1.0-master-20091202.150145-1.jar")
        (concat "http://build.clojure.org/snapshots/org/"
                "clojure/clojure-contrib/1.1.0-master-SNAPSHOT/"
                "clojure-contrib-1.1.0-master-20091212.205045-1.jar"))
  "A list of urls of jars required to run swank-clojure. If they
don't exist in `swank-clojure-jar-home' and
`swank-clojure-classpath' is not set, the user will be prompted
to download them when invoking `slime'.

Due to a bug in url-retrieve-synchronously, they must be
downloaded in order of size (ascending), so if you customize
this, keep that in mind."
  :type 'list
  :group 'swank-clojure)

(defface swank-clojure-dim-trace-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'slime-ui)

;;;###autoload
(defun swank-clojure-init (file encoding)
  (concat
   (when swank-clojure-compile-p
     "(require 'swank.loader)\n\n(swank.loader/init)\n\n")
   "(require 'swank.swank)\n\n"
   (when (boundp 'slime-protocol-version)
     (format "(swank.swank/ignore-protocol-version %S)\n\n"
             slime-protocol-version))
   ;; Hacked in call to get the localhost address to work around a bug
   ;; where the REPL doesn't pop up until the user presses Enter.
   "(do (.. java.net.InetAddress getLocalHost getHostAddress) nil)"
   (format "(swank.swank/start-server %S :encoding %S)\n\n"
           file (format "%s" (slime-coding-system-cl-name encoding)))))

(defun swank-clojure-find-package ()
  (let ((regexp "^(\\(clojure.core/\\)?\\(in-\\)?ns\\s-+[:']?\\([^()\" \t\n]+\\>\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 3)))))

;;;###autoload
(defun swank-clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function)
       'swank-clojure-find-package))

(defun swank-clojure-update-indentation (sym indent)
  (put sym 'clojure-indent-function indent))

(defun swank-clojure-concat-paths (paths)
  "Concatenate given list of `paths' using `path-separator'. (`expand-file-name'
will be used over paths too.)"
  (mapconcat 'identity (mapcar 'expand-file-name paths) path-separator))

(defun swank-clojure-parse-jar-name (url)
  (car (last (split-string url "/"))))

(defun swank-clojure-download-jar (url)
  (let ((jar-name (swank-clojure-parse-jar-name url)))
    (message "Downloading %s..." jar-name)
    (let ((download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (condition-case e
            (progn
              (set-buffer download-buffer)
              (re-search-forward "HTTP/[0-9]\.[0-9] 200 OK")
              (re-search-forward "^$" nil 'move)
              (delete-region (point-min) (+ 1 (point)))
              (write-file (concat swank-clojure-jar-home "/" jar-name))
              (kill-buffer nil))    
          (error
           ;; no recursive directory deletion on emacs 22 =(
           (dolist (j (directory-files swank-clojure-jar-home t "[^.]+$"))
             (delete-file j))
           (delete-directory swank-clojure-jar-home)
           (error "Failed to download Clojure jars.")))))))

(defun swank-clojure-dep-exists-p (jar-url)
  "True if the jar file in `jar-url' exists in `swank-clojure-jar-home'."
  (file-exists-p (expand-file-name (swank-clojure-parse-jar-name jar-url)
                                   swank-clojure-jar-home)))

(defun swank-clojure-check-install ()
  "Prompt to install Clojure if it's not already present."
  (when (and (not swank-clojure-classpath)
             (or (not (file-exists-p swank-clojure-jar-home))
                 (> (count-if-not 'swank-clojure-dep-exists-p swank-clojure-deps)
                    0))
             (y-or-n-p "It looks like Clojure is not installed. Install now? "))
    (make-directory swank-clojure-jar-home t)
    (dolist (j swank-clojure-deps)
      (swank-clojure-download-jar j))
    (setq swank-clojure-classpath (swank-clojure-default-classpath))))

;;;###autoload
(defun swank-clojure-cmd ()
  "Create the command to start clojure according to current settings."
  (swank-clojure-check-install)
  (if swank-clojure-binary
      (if (listp swank-clojure-binary)
          swank-clojure-binary
        (list swank-clojure-binary))
    (delete-if
     'null
     (append
      (list swank-clojure-java-path)
      swank-clojure-extra-vm-args
      (list
       (when swank-clojure-library-paths
         (concat "-Djava.library.path="
                 (swank-clojure-concat-paths swank-clojure-library-paths)))
       "-classpath"
       (swank-clojure-concat-paths swank-clojure-classpath)
       "clojure.main")
      (let ((init-opts '()))
        ;; TODO: cleanup
        (dolist (init-file swank-clojure-init-files init-opts)
          (setq init-opts (append init-opts (list "-i" init-file))))
        init-opts)
      (list "--repl")))))

;;;###autoload
(defadvice slime-read-interactive-args (before add-clojure)
  ;; Unfortunately we need to construct our Clojure-launching command
  ;; at slime-launch time to reflect changes in the classpath. Slime
  ;; has no mechanism to support this, so we must resort to advice.
  (require 'assoc)
  (aput 'slime-lisp-implementations 'clojure
        (list (swank-clojure-cmd) :init 'swank-clojure-init)))

;; Change the repl to be more clojure friendly
(defun swank-clojure-slime-repl-modify-syntax ()
  (when (string-match "\\*slime-repl clojure\\*" (buffer-name))
    ;; modify syntax
    (modify-syntax-entry ?~ "'   ")
    (modify-syntax-entry ?, "    ")
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (modify-syntax-entry ?^ "'")
    (modify-syntax-entry ?= "'")

    ;; set indentation function (already local)
    (setq lisp-indent-function 'clojure-indent-function)

    ;; set paredit keys
    (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
      (define-key slime-repl-mode-map "{" 'paredit-open-curly)
      (define-key slime-repl-mode-map "}" 'paredit-close-curly))))

;; Debugger

(defun swank-clojure-dim-font-lock ()
  "Dim irrelevant lines in Clojure debugger buffers."
  (if (string-match "clojure" (buffer-name))
      (font-lock-add-keywords
       nil `((,(concat " [0-9]+: " (regexp-opt '("clojure.core"
                                                 "clojure.lang"
                                                 "swank." "java."))
                       ;; TODO: regexes ending in .* are ignored by
                       ;; font-lock; what gives?
                       "[a-zA-Z0-9\\._$]*")
              . font-lock-comment-face)) t)))

(add-hook 'sldb-mode-hook 'swank-clojure-dim-font-lock)

(defvar swank-clojure-project-hook nil
  "A hook to run when a new SLIME session starts via `swank-clojure-project'.
The `path' variable is bound to the project root when these functions run.")

(defun swank-clojure-javadoc (classname)
  "Show the javadoc for classname using clojure.contrib.repl-utils/javadoc"
  (interactive (list (read-from-minibuffer "Javadoc for: " (slime-sexp-at-point))))
  (slime-eval
   `(swank:eval-and-grab-output
     ,(concat "(try
       (require 'clojure.contrib.repl-utils)
       (@(ns-resolve 'clojure.contrib.repl-utils 'javadoc) " classname ")
       (catch Throwable t (.getMessage t)))"))))

(defun directoryp (path)
  "Return t is path is a directory or a symlink pointing to a directory."
  (let ((first-attr (car (file-attributes path))))
    (if (stringp first-attr)
      (directoryp first-attr)
      first-attr)))

;;;###autoload
(defun swank-clojure-project (path)
  "Setup classpath for a clojure project and starts a new SLIME session.
  Kills existing SLIME session, if any."
  (interactive (list
                (read-directory-name
                 "Project root: "
                 (if (functionp 'locate-dominating-file) ; Emacs 23 only
                     (locate-dominating-file default-directory "src")
                   default-directory))))
  ;; TODO: allow multiple SLIME sessions per Emacs instance
  (when (get-buffer "*inferior-lisp*") (kill-buffer "*inferior-lisp*"))

  (let ((slime-lisp-implementations (copy-list slime-lisp-implementations))
        (swank-clojure-extra-vm-args (copy-list swank-clojure-extra-vm-args))
        (swank-clojure-binary nil)
        (swank-clojure-classpath (let ((l (expand-file-name "lib" path)))
                                   (if (file-directory-p l)
				       (append
					(directory-files l t ".jar$")
					(remove-if-not 'directoryp
					  (directory-files l t "^[^\\.]")))))))

    (add-to-list 'swank-clojure-classpath (expand-file-name "classes/" path))
    (add-to-list 'swank-clojure-classpath (expand-file-name "src/" path))
    (add-to-list 'swank-clojure-classpath (expand-file-name "test/" path))

    ;; For Maven style project layouts
    (when (file-exists-p (expand-file-name "pom.xml" path))
      (dolist (d '("src/main/clojure/" "src/test/clojure/"
                   "target/test-classes/" "target/classes/" "target/dependency/"))
        (add-to-list 'swank-clojure-classpath (expand-file-name d path) t))
      (dolist (d (let ((l (expand-file-name "target/dependency/" path)))
                   (if (file-directory-p l)
                       (directory-files l t ".jar$"))))
        (add-to-list 'swank-clojure-classpath (expand-file-name d path) t))
      (add-to-list 'swank-clojure-extra-vm-args
                   (format "-Dclojure.compile.path=%s"
                           (expand-file-name "target/classes/" path))))
    (run-hooks 'swank-clojure-project-hook)

    (save-window-excursion
      (slime))))

(provide 'swank-clojure)
;;; swank-clojure.el ends here
