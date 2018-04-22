#!/bin/sh


## Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Commentary:

# Preconfigured `emacs -Q' with a basic Helm configuration.
# Run it from this directory or symlink it somewhere in your PATH.

# If TEMP env var exists, use it, otherwise declare it.
test -z "$TEMP" && TEMP="/tmp"

CONF_FILE="$TEMP/helm-cfg.el"
EMACS=emacs

case $1 in
    -P)
        shift 1
        EMACS=$1
        shift 1
        ;;
    -h)
        echo "Usage: ${0##*/} [-P} Emacs path [-h} help [--] EMACS ARGS"
        exit 1
        ;;
esac

LOAD_PATH=$($EMACS -q -batch --eval "(prin1 load-path)")

cd "${0%/*}" || exit 1

# Check if autoload file exists.
# It may be in a different directory if emacs-helm.sh is a symlink.
TRUENAME=$(find . -samefile "$0" -printf "%l")
if [ ! -z "$TRUENAME" ]; then
    AUTO_FILE="${TRUENAME%/*}/helm-autoloads.el"
else
    AUTO_FILE="helm-autoloads.el"
fi
if [ ! -e "$AUTO_FILE" ]; then
    echo No autoloads found, please run make first to generate autoload file
    exit 1
fi


cat > $CONF_FILE <<EOF
(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\
;; This Emacs is Powered by \`HELM' using\n\
;; emacs program \"$EMACS\".\n\
;; This is a minimal \`helm' configuration to discover \`helm' or debug it.\n\
;; You can retrieve this minimal configuration in \"$CONF_FILE\".\n\
;; Some original Emacs commands are replaced by their \`helm' counterparts:\n\n\
;; - \`find-file'(C-x C-f)            =>\`helm-find-files'\n\
;; - \`occur'(M-s o)                  =>\`helm-occur'\n\
;; - \`list-buffers'(C-x C-b)         =>\`helm-buffers-list'\n\
;; - \`completion-at-point'(M-tab)    =>\`helm-lisp-completion-at-point'[1]\n\
;; - \`dabbrev-expand'(M-/)           =>\`helm-dabbrev'\n\n\
;; - \`execute-extended-command'(M-x) =>\`helm-M-x'\n\n
;; Some other Emacs commands are \"helmized\" by \`helm-mode'.\n\
;; [1] Coming with emacs-24.4, \`completion-at-point' is \"helmized\" by \`helm-mode'\n\
;; which provides Helm completion in many places like \`shell-mode'.\n\
;; Find context help for most Helm commands with \`C-h m'.\n\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))

(setq load-path (quote $LOAD_PATH))
(require 'package)
;; User may be using a non standard \`package-user-dir'.
;; Modify \`package-directory-list' instead of \`package-user-dir'
;; in case the user starts Helm from a non-ELPA installation.
(unless (file-equal-p package-user-dir "~/.emacs.d/elpa")
  (add-to-list 'package-directory-list (directory-file-name
                                        (file-name-directory
                                         (directory-file-name default-directory)))))

(setq package-load-list '((helm-core t) (helm t) (async t) (popup t)))
(package-initialize)
(add-to-list 'load-path (file-name-directory (file-truename "$0")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(blink-cursor-mode -1)
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))
EOF

$EMACS -Q -l "$CONF_FILE" "$@"
