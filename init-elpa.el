;;------------------------------------------------------------------------------
;; Find and load the correct package.el
;;------------------------------------------------------------------------------

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/package")))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)


;;------------------------------------------------------------------------------
;; Patch up annoying package.el quirks
;;------------------------------------------------------------------------------

(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))


;;------------------------------------------------------------------------------
;; Add support to package.el for pre-filtering available packages
;;------------------------------------------------------------------------------

(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
      (funcall package-filter-function
         (car package)
         (funcall (if (fboundp 'package-desc-version)
          'package--ac-desc-version
        'package-desc-vers)
            (cdr package))
         archive))
    ad-do-it))
;;------------------------------------------------------------------------------
;; On-demand installation of packages
;;------------------------------------------------------------------------------

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;;------------------------------------------------------------------------------
;; Standard package repositories
;;------------------------------------------------------------------------------

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))


;; use packages from melpa only, even packages in elpa.gnu.org are ignored
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://hiddencameras.milkbox.net/packages/")
                         ))

;; Un-comment below line if you download zip file from https://github.com/redguardtoo/myelpa/archive/master.zip and extract its content into ~/myelpa/
;; (setq package-archives '(("myelpa" . "~/myelpa")))

;; Or Un-comment below line if you prefer installing package from https://github.com/redguardtoo/myelpa/ directly
;; (setq package-archives '(("myelpa" . "https://raw.github.com/redguardtoo/myelpa/master/")))

(defvar melpa-include-packages
  '(bbdb
    xml-rpc
    json-rpc
    kv
    color-theme
    anaconda-mode
    wgrep
    robe
    inf-ruby
    yari
    dsvn
    move-text
    findr
    mwe-log-commands
    dired-details
    yaml-mode
    noflet
    db
    creole
    web
    elnode
    sass-mode
    idomenu
    pointback
    buffer-move
    regex-tool
    csharp-mode
    switch-window
    cmake-mode
    sr-speedbar
    quack
    iedit
    legalese
    htmlize
    scratch
    mic-paren
    session
    crontab-mode
    bookmark+
    flymake-lua
    multi-term
    dired+
    inflections
    dropdown-list
    lua-mode
    pomodoro
    helm
    auto-compile
    packed
    gitconfig-mode
    project-local-variables
    org-fstree
    textile-mode
    pretty-mode
    auto-complete-clang
    w3m
    fakir
    erlang
    fancy-narrow)
  "Don't install any Melpa packages except these packages")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (and (string-equal archive "melpa") (memq package melpa-include-packages))
             (not (string-equal archive "melpa")))
         )))

;; un-comment below code if you prefer use all the package on melpa (unstable) without limitation
;; (setq package-filter-function nil)

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)

(require-package 'cl-lib '(0 0 5) nil)
(require-package 'xml-rpc)
(require-package 'kv '(0 0 19) nil)
(require-package 'dash '(2 5 0) nil)
; color-theme 6.6.1 in elpa is buggy
(require-package 'color-theme)
(require-package 'auto-compile)
(require-package 'ace-jump-mode)
(require-package 'expand-region '(0 8 0) nil)
(require-package 'fringe-helper)
(require-package 'haskell-mode '(13 7 0) nil)
(require-package 'magit '(1 2 0) nil)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'wgrep)
(require-package 'json)
(require-package 'lua-mode)
(require-package 'project-local-variables)
(require-package 'ruby-mode)
(require-package 'robe)
(require-package 'inf-ruby '(2 3 0) nil)
(require-package 'yari)
(require-package 'yaml-mode)
(require-package 'paredit)
(require-package 'erlang '(20120612 0 0) nil)
(require-package 'browse-kill-ring)
(require-package 'findr)
(if *emacs24* (require-package 'jump '(2 3 0) nil))
(require-package 'haml-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'markdown-mode)
(require-package 'dired+)
(require-package 'maxframe)
(require-package 'org-fstree)
(require-package 'htmlize)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mic-paren)
(require-package 'rainbow-delimiters)
(require-package 'textile-mode)
(require-package 'pretty-mode)
(if *emacs24* (require-package 'coffee-mode))
(require-package 'crontab-mode)
(require-package 'dsvn)
(require-package 'exec-path-from-shell)
(require-package 'flymake-coffee)
(require-package 'flymake-css)
(require-package 'flymake-haml)
(require-package 'flymake-jslint)
(require-package 'flymake-python-pyflakes)
(require-package 'flymake-ruby)
(require-package 'flymake-sass)
(require-package 'flymake-shell)
(require-package 'hl-sexp)
(require-package 'ibuffer-vc)
(require-package 'less-css-mode)
(require-package 'move-text)
(require-package 'mwe-log-commands)
(require-package 'page-break-lines)
(require-package 'pointback)
(require-package 'regex-tool)
(require-package 'rinari)
(require-package 'ruby-compilation)
(require-package 'csharp-mode)
(require-package 'cmake-mode)
(require-package 'emmet-mode)
(require-package 'session)
;; (require-package 'tidy)
(require-package 'unfill)
(require-package 'w3m)
(require-package 'idomenu)
(if *emacs24* (require-package 'ggtags))
(require-package 'buffer-move)
(require-package 'switch-window)
(require-package 'maxframe)
(require-package 'cpputils-cmake '(0 4 8) nil)
(require-package 'flyspell-lazy)
(require-package 'bbdb '(20130421 1145 0) nil)
(require-package 'iedit)
(require-package 'pomodoro '(20130114 1543 0) nil)
(require-package 'flymake-lua)
(require-package 'dropdown-list)
(require-package 'yasnippet '(0 8 0) nil)
;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'bookmark+)
(require-package 'multi-term)
(require-package 'json-mode)
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 1))
    (require-package 'js2-mode '(20140114 0 0) nil)
  )
(require-package 'tagedit)
(require-package 'fancy-narrow)
(require-package 'sr-speedbar)
;; company-mode drop emacs 23 support
(if (and (>= emacs-major-version 24)) (require-package 'company '(0 8 0) nil))
(require-package 'legalese)
(require-package 'string-edit)
(require-package 'dired-details)
;; (require-package 'git-gutter '(0 63) nil)
(require-package 'fakir)
(require-package 'f)
(require-package 'elnode) ;; elnode dependent on f
(when *emacs24*
  (require-package 'anaconda-mode))
(require-package 'quack) ;; for scheme

;; (require-package 'command-frequency)

(provide 'init-elpa)
