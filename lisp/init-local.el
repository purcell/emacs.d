;;; This is the init-local of ritsch_master

;; ede
(global-ede-mode)

;; c-mode & c++-mode
(add-hook 'c-mode-hook (lambda ()
                         (local-set-key (kbd "<f8>") 'ff-get-other-file)))
(add-hook 'c++-mode-hook (lambda ()
                           (local-set-key (kbd "<f8>") 'ff-get-other-file)))

;; ac-c-headers
(require-package 'ac-c-headers)
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; YASnippet
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

;; js-comint
(require-package 'js-comint)
(require 'js-comint)
(setq inferior-js-program-command "node --interactive")
(add-hook 'js2-mode-hook '(lambda ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)))
(setenv "NODE_NO_READLINE" "1")

;; cmake-mode
(require-package 'cmake-mode)
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists.txt$" . cmake-mode))

;; ;; cmake-project
;; (require-package 'cmake-project)
;; (require 'cmake-project)
;; (defun maybe-cmake-project-hook ()
;;   (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
;; (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
;; (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;; undo-tree
(global-set-key (kbd "<f9>") 'undo-tree-undo)
(global-set-key (kbd "<f10>") 'undo-tree-redo)

;; cpputil-cmake
(require-package 'cpputils-cmake)
(require 'cpputils-cmake)
(add-hook 'c-mode-hook (lambda () (cppcm-reload-all)))
(add-hook 'c++-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, somebody reported that they can use this package with Fortran
(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))

;; comments
(global-set-key (kbd "<f11>") 'comment-or-uncomment-region)

;; compile
(global-set-key (kbd "<f5>") 'recompile)

;; w3m
(require-package 'w3m)
(require 'w3m)
; use w3m as standard browser of emacs:
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; linum
(require-package 'linum)
(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)

;; gnus

(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-auth-supported (list mech)
                                              smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password &optional key
                            cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-starttls-credentials (list (list
                                                                                              server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL
enabled.)" server port user))

; This function will complain if you fill the from field with
; an account not present in smtp-accounts.
(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from) do (cond
                                               ((memq auth-mech '(cram-md5 plain login))
                                                (return (apply 'set-smtp (cons auth-mech auth-spec))))
                                               ((eql auth-mech 'ssl)
                                                (return (apply 'set-smtp-ssl auth-spec)))
                                               (t (error "Unrecognized SMTP auth. mechanism:
`%s'." auth-mech))) finally (error "Cannot infer SMTP
information."))))

(defvar %smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (with-current-buffer smtpmail-text-buffer
    (change-smtp))
  (funcall (symbol-value '%smtpmail-via-smtp) recipient
           smtpmail-text-buffer))


;; erc
(require 'erc)
(setq erc-prompt-for-password nil)
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 6697
           :nick "ritsch_master"))

;; malabar-mode
(require-package 'malabar-mode)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "/usr/share/groovy")
(defun start-malabar ()
  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode)))

;; emacs-eclim
(require-package 'emacs-eclim)
(defun start-eclim ()
  (interactive)
  (require 'eclim)
  (require 'eclimd)
  (require 'ac-emacs-eclim-source)
  (ac-emacs-eclim-config))

;; octave-mode
(require 'octave-mod)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; ac-octave
(require-package 'ac-octave)
(require 'ac-octave)
(add-to-list 'ac-sources 'ac-source-octave)
(add-hook 'octave-mode-hook '(lambda () (ac-octave-mode-setup)))

;; auctex
(require-package 'auctex)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; games
(setq tetris-score-file "~/.emacs.d/tetris-scores")
(setq snake-score-file "~/.emacs.d/snake-scores")


;;; other packages in this kit:
;; 2048
;(load-file "~/.emacs.d/packages/emacs-2048/2048.el")

;;; other configuration
;; semantic set up
(require 'semantic/ia)
(require 'semantic/bovine/c)
(semantic-mode 1)
(setq-mode-local c-mode semanticdb-find-default-throttle
                      '(project local unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                      '(project local unloaded system recursive))

; semantic autocomplete integration
(add-to-list 'ac-sources 'ac-source-semantic)

; enable some modes
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;  semantic uses gnu
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

; add Qt to c++-mode
(defvar qt-base-dir "/usr/include")
(if (file-readable-p qt-base-dir)
    (progn
      (semantic-add-system-include qt-base-dir 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/Qt") 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/QtGui") 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/QtCore") 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/QtTest") 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/QtNetwork") 'c++-mode)
      (semantic-add-system-include (concat qt-base-dir "/QtSvg") 'c++-mode)
      (add-to-list 'auto-mode-alist (cons qt-base-dir 'c++-mode))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt-base-dir "/Qt/qconfig.h"))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt-base-dir "/Qt/qconfig-large.h"))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt-base-dir "/Qt/qglobal.h"))))

; add Boost to c++-mode
(defvar boost-base-dir "/usr/include/boost")
(if (file-readable-p boost-base-dir)
    (progn
      (semantic-add-system-include boost-base-dir 'c++-mode)
      (add-to-list 'auto-mode-alist (cons boost-base-dir 'c++-mode))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat boost-base-dir "/config.hpp"))))

(provide 'init-local)
