;;; helm-info.el --- Browse info index with helm -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-plugin)
(require 'helm-net)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))


(defgroup helm-info nil
  "Info related Applications and libraries for Helm."
  :group 'helm)

;;; Build info-index sources with info-index plug-in.
;;
;;
(defun helm-build-info-index-command (name doc source buffer)
  "Define an helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-source-info-<NAME>' and BUFFER a string buffer name."
  (eval (list 'defun name nil doc
              (list 'interactive)
              (list 'helm
                    :sources source
                    :buffer buffer
                    :candidate-number-limit 1000))))

(defun helm-define-info-index-sources (var-value &optional commands)
  "Define helm sources named helm-source-info-<NAME>.
Sources are generated for all entries of `helm-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `helm-info<NAME>'.
Where NAME is one of `helm-default-info-index-list'."
  (cl-loop with symbols = (cl-loop for str in var-value
                                collect
                                (intern (concat "helm-source-info-" str)))
        for sym in symbols
        for str in var-value
        do (set sym (list (cons 'name (format "Info index: %s" str))
                          (cons 'info-index str)))
        when commands
        do (let ((com (intern (concat "helm-info-" str))))
             (helm-build-info-index-command
              com (format "Predefined helm for %s info." str)
              sym (format "*helm info %s*" str)))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-define-info-index-sources value t))

(defcustom helm-default-info-index-list
  '("elisp" "cl" "org" "gnus" "tramp" "ratpoison"
    "zsh" "bash" "coreutils" "fileutils"
    "find" "sh-utils" "textutils" "libc"
    "make" "automake" "autoconf" "eintr"
    "emacs" "elib" "eieio" "gauche-refe" "guile"
    "guile-tut" "goops" "screen" "latex" "gawk"
    "sed" "m4" "wget" "binutils" "as" "bfd" "gprof"
    "ld" "diff" "flex" "grep" "gzip" "libtool"
    "texinfo" "info" "gdb" "stabs" "cvsbook" "cvs"
    "bison" "id-utils" "global")
  "Info Manual entries to use for building helm info index commands."
  :group 'helm-info
  :type  '(repeat (choice string))
  :set   'helm-info-index-set)

(defcustom helm-info-default-sources
  '(helm-source-info-elisp
    helm-source-info-cl
    helm-source-info-eieio
    helm-source-info-pages)
  "The default sources to use in `helm-info-at-point'."
  :group 'helm-info
  :type '(repeat (choice symbol)))


;;; Info pages
(defvar helm-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-info-pages
      helm-info-pages
    (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
          topics)
      (require 'info)
      (with-temp-buffer
        (Info-find-node "dir" "top")
        (goto-char (point-min))
        (while (re-search-forward info-topic-regexp nil t)
          (push (match-string-no-properties 1) topics))
        (kill-buffer))
      (setq helm-info-pages topics))))

(defvar helm-source-info-pages
  `((name . "Info Pages")
    (init . helm-info-pages-init)
    (candidates . helm-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))

;;;###autoload
(defun helm-info-at-point ()
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive)
  (let ((helm-google-suggest-default-function
         'helm-google-suggest-emacs-lisp))
    (helm :sources helm-info-default-sources
          :buffer "*helm info*")))

(provide 'helm-info)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-info.el ends here
