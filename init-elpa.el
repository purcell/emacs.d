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
                     (package-desc-vers (cdr package))
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

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))


;;------------------------------------------------------------------------------
;; Also use Melpa for some packages built straight from VC
;;------------------------------------------------------------------------------

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar melpa-exclude-packages
  ;; I'm happy my packages included in melpa. But need time to switch to melpa finally
  '(slime evil-nerd-commenter company evil auto-complete)
  "Don't install Melpa versions of these packages.")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (not (string-equal archive "melpa"))
             (not (memq package melpa-exclude-packages))))))


;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)

(require-package 'all)
(require-package 'xml-rpc)
(require-package 'ido-ubiquitous)
(require-package 'dash '(20130513 1618 0) nil)
; color-theme 6.6.1 in elpa is buggy
(when (< emacs-major-version 24)
  (require-package 'color-theme))
(require-package 'auto-compile)
(require-package 'ace-jump-mode)
(require-package 'multiple-cursors)
(require-package 'expand-region '(20130302 1954 0) nil)
(require-package 'fringe-helper)
(require-package 'gnuplot)
(require-package 'haskell-mode '(20130523 10 0) nil)
(require-package 'cl-lib '(0 3 0) nil)
(require-package 'magit '(20130523 125 0) nil)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'wgrep)
(require-package 'flymake-cursor)
(require-package 'csv-mode)
(require-package 'csv-nav)
(require-package 'json)
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 1))
  (require-package 'js2-mode)
  )
(require-package 'lua-mode '(20120919 1821 0) nil)
(require-package 'project-local-variables)
(require-package 'ruby-mode)
(require-package 'robe)
(require-package 'inf-ruby '(20120722 0 0) nil)
(require-package 'yari)
(require-package 'yaml-mode)
(require-package 'paredit)
(require-package 'eldoc-eval)
(require-package 'erlang)
(require-package 'slime)
(require-package 'slime-fuzzy)
(require-package 'slime-repl)
(require-package 'browse-kill-ring)
(require-package 'findr)
(require-package 'jump '(20120820 1951 0) nil)
(require-package 'anything)
;; (require-package 'gist)
(require-package 'haml-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'elein)
(require-package 'markdown-mode)
(require-package 'smex '(20120915 2041 0) nil)
(require-package 'dired+)
(require-package 'rainbow-mode '(0 6 0) nil)
(require-package 'maxframe)
;; org-mode is very important to me, so I always use the latest ersion
;; org 8.0 makes org2blog broken, so I just downgrade to the 7.8 and wait
(require-package 'org '(20130805 0 0) t)
;; org-mime is not upgrade in repository. I put right version in elpa
;; (require-package 'org-mime)
(require-package 'org-fstree)
;; I don't care mac
;;(when *is-a-mac*
;;  (require-package 'org-mac-link-grabber)
;;  (require-package 'org-mac-iCal))
(require-package 'htmlize)
(require-package 'org2blog '(20130704 908 0) nil)
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'cljsbuild-mode)
(require-package 'nrepl)
(require-package 'slamhound)
(require-package 'diminish)
(require-package 'js-comint)
(require-package 'php-mode)
(require-package 'smarty-mode)
(require-package 'scratch)
(require-package 'mic-paren)
(require-package 'rainbow-delimiters)
(require-package 'todochiku)
(require-package 'marmalade)
(require-package 'textile-mode)
(require-package 'pretty-mode)

;; I maintain this chunk:
(require-package 'ac-slime)
(require-package 'ac-nrepl)
(require-package 'coffee-mode)
(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'zenburn-theme)
(require-package 'crontab-mode)
(require-package 'dsvn)
(require-package 'elisp-slime-nav)
(require-package 'exec-path-from-shell)
(require-package 'flymake-coffee)
(require-package 'flymake-css)
(require-package 'flymake-haml)
(require-package 'flymake-jslint)
(require-package 'flymake-php)
(require-package 'flymake-python-pyflakes)
(require-package 'flymake-ruby)
(require-package 'flymake-sass)
(require-package 'flymake-shell)
(require-package 'hippie-expand-slime)
(require-package 'hl-sexp)
(require-package 'ibuffer-vc)
(require-package 'less-css-mode)
(require-package 'lively)
(require-package 'mmm-mode '(20130210 133 0) nil)
(require-package 'move-text)
(require-package 'mwe-log-commands)
(require-package 'page-break-lines)
(require-package 'pointback)
(require-package 'regex-tool)
(require-package 'rinari '(20130202 1020 0) nil) ; use latest rinari
(require-package 'ruby-compilation)
(require-package 'iy-go-to-char)
(require-package 'csharp-mode)
(require-package 'cmake-mode)
(require-package 'keyfreq)
(require-package 'fuzzy)
(require-package 'auto-complete) ;auto-complete is dependent on fuzzy
(require-package 'auto-complete-clang)
(require-package 'emmet-mode)
(require-package 'session)
(require-package 'tidy)
(require-package 'unfill)
(require-package 'whole-line-or-region)
(require-package 'undo-tree '(20121124 2207 0) nil)
(require-package 'auctex)
(require-package 'etags-select '(1 13 0) nil) ;; evil may need it
;;evil-20120725 requires ert
(require-package 'evil '(1 0 3) nil)
(require-package 'evil-numbers '(20120712 1933 0) nil)
(require-package 'w3m)
(require-package 'idomenu)
(require-package 'ctags)
(require-package 'buffer-move)
(require-package 'go-mode)
(require-package 'switch-window)
(require-package 'maxframe)
(require-package 'cpputils-cmake '(0 3 4) t)
(require-package 'flyspell-lazy)
(require-package 'gtags)
(require-package 'bbdb '(20130421 1145 0) nil)
(require-package 'twittering-mode)
(require-package 'iedit)
(require-package 'wxwidgets-help '(0 0 3) nil)
(if *is-a-mac* (require-package 'emms))
(require-package 'pomodoro '(20130114 1543 0) nil)
(require-package 'flymake-lua)
(require-package 'evil-nerd-commenter '(0 0 8) nil)
(require-package 'surround '(20121022 1257 0) nil)
(require-package 'dropdown-list)
(require-package 'yasnippet '(20130505 2115 0) nil)
(require-package 'workgroups)
;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'bookmark+)
(require-package 'json-mode)
(require-package 'tagedit)
(require-package 'web-mode)
(require-package 'sr-speedbar)
(require-package 'requirejs-mode)
(require-package 'smartparens)
(require-package 'company '(0 6 10) nil)
(require-package 'legalese)
(require-package 'string-edit)
(require-package 'dired-details)
(require-package 'popwin)
(require-package 'elnode)
(require-package 'confluence-edit)
;;(require-package 'git-messenger '(20130613 1222 0) nil)
(require-package 'issue-tracker '(0 0 1) nil)

(when *emacs24*
  (require-package 'helm '(20130409 1040 0) nil)
  (require-package 'helm-ls-git '(20130310 1401 0) nil)
  (require-package 'helm-gtags)
  (require-package 'helm-c-yasnippet)
  )
;; (require-package 'command-frequency)

(provide 'init-elpa)
