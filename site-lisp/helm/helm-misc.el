;;; helm-misc.el --- Various functions for helm -*- lexical-binding: t -*-

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


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-time-zone-home-location "Paris"
  "The time zone of your home"
  :group 'helm-misc
  :type 'string)

(defcustom helm-mini-default-sources '(helm-source-buffers-list
                                       helm-source-recentf
                                       helm-source-buffer-not-found)
  "Default sources list used in `helm-mini'."
  :group 'helm-misc
  :type '(repeat (choice symbol)))

(defface helm-time-zone-current
    '((t (:foreground "green")))
  "Face used to colorize current time in `helm-world-time'."
  :group 'helm-misc)

(defface helm-time-zone-home
    '((t (:foreground "red")))
  "Face used to colorize home time in `helm-world-time'."
  :group 'helm-misc)


;;; Latex completion
(defvar LaTeX-math-menu)
(defun helm-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (cl-loop for i in (cddr LaTeX-math-menu)
        for elm = (cl-loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-helm-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . helm-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar helm-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar helm-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar helm-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar helm-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar helm-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar helm-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Eev anchors
(defvar helm-source-eev-anchor
  '((name . "Anchors")
    (candidates
     . (lambda ()
         (ignore-errors
           (with-helm-current-buffer
             (cl-loop initially (goto-char (point-min))
                   while (re-search-forward
                          (format ee-anchor-format "\\([^\.].+\\)") nil t)
                   for anchor = (match-string-no-properties 1)
                   collect (cons (format "%5d:%s"
                                         (line-number-at-pos (match-beginning 0))
                                         (format ee-anchor-format anchor))
                                 anchor))))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (helm-highlight-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . ee-to)))))

;;; Jabber Contacts (jabber.el)
(defun helm-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (cl-loop for item in (jabber-concat-rosters)
          when (get item 'connected)
          collect
          (if (get item 'name)
              (cons (get item 'name) item)
            (cons (symbol-name item) item)))))

(defvar helm-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (helm-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (helm-jabber-online-contacts)))))))))

;;; World time
;;
(defun helm-time-zone-transformer (candidates _source)
  (cl-loop for i in candidates
        collect
        (cond ((string-match (format-time-string "%H:%M" (current-time)) i)
               (propertize i 'face 'helm-time-zone-current))
              ((string-match helm-time-zone-home-location i)
               (propertize i 'face 'helm-time-zone-home))
              (t i))))

(defvar helm-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (require 'time)
              (let ((helm-buffer (helm-candidate-buffer 'global)))
                (with-current-buffer helm-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-time-zone-transformer)))

;;; LaCarte
;;
;;
(defun helm-create-lacarte-source (name &optional maps)
  "Create lacarte source named NAME for MAPS.
MAPS is like in `lacarte-get-overall-menu-item-alist'.
See
    http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el"
  `((name . ,name)
    (init . (lambda () (require 'lacarte)))
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (delete '(nil) (lacarte-get-overall-menu-item-alist ,@maps)))))
    (candidate-transformer . helm-lacarte-candidate-transformer)
    (candidate-number-limit . 9999)
    (type . command)))

(defun helm-lacarte-candidate-transformer (cands)
  (mapcar (lambda (cand)
            (let* ((item (car cand))
                   (match (string-match "[^>] \\((.*)\\)$" item)))
              (when match
                (put-text-property (match-beginning 1) (match-end 1)
                                   'face 'helm-M-x-key item))
              cand))
          cands))

(defvar helm-source-lacarte (helm-create-lacarte-source "Lacarte")
  "Helm interface for lacarte.el.
See
    http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")

;;;###autoload
(defun helm-browse-menubar ()
  "Helm interface to the menubar using lacarte.el."
  (interactive)
  (require 'lacarte)
  (helm :sources 'helm-source-lacarte :buffer "*helm lacarte*"))

(defun helm-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-stringify cmd-or-name)
              (delete (helm-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
      (setq this-command cmd)
      (call-interactively cmd))))

;;; Minibuffer History
;;
;;
(defvar helm-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name)
                     (format "%s (%s)" name minibuffer-history-variable)))
    (candidates
     . (lambda ()
         (let ((history (cl-loop for i in
                              (symbol-value minibuffer-history-variable)
                              unless (string= "" i) collect i)))
           (if (consp (car history))
               (mapcar 'prin1-to-string history)
             history))))
    (migemo)
    (multiline)
    (action . (lambda (candidate)
                (delete-minibuffer-contents)
                (insert candidate)))))

;;; Shell history
;;
;;
(defun helm-comint-input-ring-action (candidate)
  "Default action for comint history."
  (with-helm-current-buffer
    (delete-region (comint-line-beginning-position) (point-max))
    (insert candidate)))

(defvar helm-source-comint-input-ring
  '((name . "Comint history")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (ring-elements comint-input-ring))))
    (action . helm-comint-input-ring-action))
  "Source that provide helm completion against `comint-input-ring'.")


;;; Helm ratpoison UI
;;
;;
(defvar helm-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . helm-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-ratpoison-commands-execute))
    (display-to-real . helm-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

(defun helm-ratpoison-commands-init ()
  (unless (helm-candidate-buffer)
    (with-current-buffer (helm-candidate-buffer 'global)
      ;; with ratpoison prefix key
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "<ratpoison> \\1: \\2"))
      (goto-char (point-max))
      ;; direct binding
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help top"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "\\1: \\2")))))

(defun helm-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))

;;; Helm stumpwm UI
;;
;;
(defvar helm-source-stumpwm-commands
  '((name . "Stumpwm Commands")
    (init . helm-stumpwm-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-stumpwm-commands-execute))
    (candidate-number-limit)))

(defun helm-stumpwm-commands-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (save-excursion
      (call-process "stumpish" nil (current-buffer) nil "commands"))
    (while (re-search-forward "[ ]*\\([^ ]+\\)[ ]*\n?" nil t)
      (replace-match "\n\\1\n"))
    (delete-blank-lines)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-max))))

(defun helm-stumpwm-commands-execute (candidate)
  (call-process "stumpish" nil nil nil  candidate))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time."
  (interactive)
  (helm-other-buffer 'helm-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-eev-anchors ()
  "Preconfigured `helm' for eev anchors."
  (interactive)
  (helm-other-buffer 'helm-source-eev-anchor "*Helm eev anchors*"))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-stumpwm-commands()
  (interactive)
  (helm-other-buffer 'helm-source-stumpwm-commands
                     "*helm stumpwm commands*"))


;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer helm-mini-default-sources "*helm mini*")))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-source-minibuffer-history
                       "*helm minibuffer-history*")))

;;;###autoload
(defun helm-comint-input-ring ()
  "Predefined `helm' that provide completion of `comint' history."
  (interactive)
  (when (derived-mode-p 'comint-mode)
    (helm :sources 'helm-source-comint-input-ring
          :input (buffer-substring-no-properties (comint-line-beginning-position)
                                                 (point-at-eol))
          :buffer "*helm comint history*")))


(provide 'helm-misc)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-misc.el ends here
