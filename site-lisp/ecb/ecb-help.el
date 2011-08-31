;;; ecb-help.el --- online help for ECB and bug reporting

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2001

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-help.el,v 1.117 2009/05/09 15:23:50 berndl Exp $

;;; Commentary:
;;
;; Contains all online-help for ECB (stolen something from recentf.el)

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-layout)
(require 'ecb-util)

;; XEmacs and Emacs 20.X
(silentcomp-defvar browse-url-new-window-p)
(silentcomp-defun browse-url)
;; Emacs 21
(silentcomp-defvar browse-url-new-window-flag)
;; JDE
(silentcomp-defvar jde-version)
;; mail and reporter
(silentcomp-defun mail-subject)
(silentcomp-defun mail-text)
(silentcomp-defun reporter-submit-bug-report)

(defconst ecb-help-info-start-file "ecb.info")
(defconst ecb-help-html-start-file "ecb.html")
(defconst ecb-help-info-subdir "./info-help/")
(defconst ecb-help-html-subdir "./html-help/")

(defgroup ecb-help nil
  "Settings for the ECB online help"
  :group 'ecb)

(defcustom ecb-show-help-format 'info
  "*The format `ecb-show-help' shows its online help.
Allowed values are 'info \(for the Info format) and 'html \(for HTML format).
If the value is 'html then `browse-url-browser-function' says which browser is
used.

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included."
  :group 'ecb-help
  :group 'ecb-most-important
  :type '(choice :tag "Online-help format" :menu-tag "Online-help format"
                 (const :tag "Info" :value info)
                 (const :tag "Html" :value html)))


(defcustom ecb-help-info-path (concat
                               (if ecb-running-xemacs
                                   (if (file-exists-p
                                        (concat ecb-ecb-dir
                                                ecb-help-info-subdir
                                                ecb-help-info-start-file))
                                       ecb-help-info-subdir
                                     "../../info/")
                                 ecb-help-info-subdir)
                               ecb-help-info-start-file)
  "*Path where the ECB online help in info format resides.
This must be the location of the file \"ecb.info\" which comes with the ECB
distribution. If is installed by unpacking the archive available on the ECB
web-site then this is the subdir `ecb-help-info-subdir' of the installation
directory of ECB. If it is installed as XEmacs-package \(e.g. via the package
manager of XEmacs) then this is probably the directory \"../../info/\"
\(relativ to the Elisp directory of ECB).

The path can either be an absolute path or a path relative to the directory
where the Elisp files of ECB are.

Normally there should be no need to change this option!"
  :group 'ecb-help
  :type 'file)

(defcustom ecb-help-html-path
  (if (not ecb-running-xemacs)
      (concat ecb-help-html-subdir ecb-help-html-start-file)
    (cond ((file-exists-p
            (concat ecb-ecb-dir
                    ecb-help-html-subdir
                    ecb-help-html-start-file))
           (concat ecb-help-html-subdir ecb-help-html-start-file))
          ((file-exists-p
            (concat ecb-ecb-dir
                    "../../html/"
                    ecb-help-html-start-file))
           (concat "../../html/" ecb-help-html-start-file))
          ((file-exists-p
            (concat ecb-ecb-dir
                    "../../html/ecb/index.html"))
           "../../html/ecb/index.html")
          (t
           (concat "../../etc/ecb/html/" ecb-help-html-start-file))))
  "*Path where the ECB online help in HTML format resides.
This must be the location of the file \"index.html\" which comes with the ECB
distribution. If is installed by unpacking the archive available on the ECB
web-site then this is the subdir `ecb-help-html-subdir' of the installation
directory of ECB. If it is installed as XEmacs-package \(e.g. via the package
manager of XEmacs) then this is probably either the directory \"../../html/\" or
\"../../etc/ecb/html/\" \(both relative to the Elisp directory of ECB).

The path can either be an absolute path or a path relative to the directory
where the Elisp files of ECB are.

Normally there should be no need to change this option!"
  :group 'ecb-help
  :type 'file)


(defun ecb-info (info-file &optional no-file-not-exist-err)
  "Starts `info' with INFO-FILE. If INFO-FILE does not exists then nil is
returned otherwise true. If NO-FILE-NOT-EXIST-ERR is not nil then just nil is
returned if INFO-FILE does not exist otherwise an error is reported."
  (if (file-exists-p info-file)
      (prog1 t
        (info info-file))
    (unless no-file-not-exist-err
      (ecb-error "Info file %s does not exists!" info-file))
    nil))

(defun ecb-browse-html-file (html-file &optional no-file-not-exist-err)
  "Opens HTML-FILE in the standard-webbrowser with `browse-url'. If INFO-FILE
does not exists then nil is returned otherwise true. If NO-FILE-NOT-EXIST-ERR
is not nil then just nil is returned if HTML-FILE does not exist otherwise an
error is reported."
  (if (file-exists-p html-file)
      (prog1 t
        (if (and (locate-library "browse-url")
                 (require 'browse-url)
                 (fboundp 'browse-url))
            (browse-url (concat "file://" html-file)
                        (if (boundp 'browse-url-new-window-flag)
                            browse-url-new-window-flag
                          browse-url-new-window-p))
          (ecb-error "Function 'browse-url needed for displaying HTML!")))
    (unless no-file-not-exist-err
      (ecb-error "HTML file %s does not exists!" html-file))
    nil))

;;;###autoload
(defun ecb-show-help (&optional format)
  "Shows the online help of ECB in Info or HTML-format.
The format depends on the setting in `ecb-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help \(Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `ecb-help-info-start-file' and `ecb-help-html-start-file'!

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included."
  (interactive "P")
  (let ((f (if format
               (intern (ecb-query-string "Choose format of online-help:"
                                         (if (equal 'ecb-show-help-format
                                                    'html)
                                             '("info" "html")
                                           '("html" "info"))))
             ecb-show-help-format))
        (info-path-abs (expand-file-name
                        (save-match-data
                          (if (or (string-match "^\\." ecb-help-info-path)
                                  (string-match (concat "^"
                                                        (regexp-quote
                                                         ecb-help-info-start-file))
                                                ecb-help-info-path))
                              (concat ecb-ecb-dir ecb-help-info-path)
                            ecb-help-info-path))))
        (html-path-abs (expand-file-name
                        (save-match-data
                          (if (or (string-match "^\\." ecb-help-html-path)
                                  (string-match (concat "^"
                                                        (regexp-quote
                                                         ecb-help-html-start-file))
                                                ecb-help-html-path))
                              (concat ecb-ecb-dir ecb-help-html-path)
                            ecb-help-html-path)))))
    (if (equal f 'info)
        (ecb-info info-path-abs)
      (message "Opening ECB online-help in a web-browser...")
      (ecb-browse-html-file html-path-abs))))


;;
;; Problem reporting functions stolen from JDEE
;;
(defvar ecb-problem-report-mail-address "ecb-list@lists.sourceforge.net" )

(defconst ecb-problem-report-message
  "Please enter the details of your bug report here")

(defun ecb-submit-problem-report()
  "Submit a problem report for the ECB to the ECB mailing-list.
This command generates in the edit-window a problem-report which contains
already the current values of all ECB options, the current backtrace-buffer if
there is any and the current message-buffer. You will be asked for a
problem-report subject and then you must insert a description of the problem.
Please describe the problem as detailed as possible!

*IMPORTANT*: Cause of extra appearance of SPAM in the mailing-lists,
SourceForge has changed its policy: Now it is only possible to post to the
mailing-list for users who have subscribed this mailing-list. So please be
aware you will not be able to send comments, bug reports and improvement
suggestions before you have subscribed the ECB-mailing-list. See the section
\"Mailing-list\" at the ECB-website at http://ecb.sourceforge.net how to do
this."

  (interactive)
  (when (or ecb-minor-mode
            (y-or-n-p "ECB should be active when submitting a problem-report. Force report? "))
    (if (and (equal ecb-frame (selected-frame))
             (not (ecb-point-in-edit-window-number)))
        (ecb-select-edit-window))
    (if (not (locate-library "reporter"))
        (ecb-error "You need the reporter.el package to submit a bugreport for ECB!")
      (require 'reporter)
      (progn
        (message "Preparing problem report...")
        ;;prepare the basic buffer
        (reporter-submit-bug-report
         ecb-problem-report-mail-address
         (format "ECB: %s, CEDET: %s, semantic: %s, eieio: %s, speedbar: %s, JDEE: %s"
                 ecb-version
                 cedet-version
                 semantic-version
                 eieio-version
                 speedbar-version
                 (if (boundp 'jde-version)
                     jde-version
                   "No JDEE"))
         (ecb-problem-report-list-all-variables)
         nil
         'ecb-problem-report-post-hook
         ecb-problem-report-message)
        (if (equal ecb-frame (selected-frame))
            (ecb-redraw-layout))
        (mail-subject)
        (insert (read-string "Problem report subject: "
                             (format "ECB-%s -- " ecb-version)))
        (mail-text)
        (search-forward ecb-problem-report-message)
        (end-of-line)
        (message "Preparing bug report...done")))))

(defun ecb-problem-report-post-hook()
  "Function run the reporter package done its work. It looks for a message- and
a backtrace-buffer and inserts the contents of that."
  (save-excursion
    (goto-char (point-min))
    ;; if the mail-packages has already inserted a signature we must not go to
    ;; the buffer-end but just before the signature
    (if (re-search-forward "^--[ \t]*$" nil t)
        (progn
          (beginning-of-line)
          (insert "\n\n\n")
          (forward-line -2))
      (goto-char (point-max))
      (insert "\n\n"))
    ;; ecb-faces
    (let ((ecb-face-list (delq nil (mapcar (function
                                            (lambda (f)
                                              (if (save-match-data
                                                    (string-match "^ecb-"
                                                                  (symbol-name f)))
                                                  f
                                                nil)))
                                           (face-list)))))
      (insert "\n\n-----------------------------------------------------\n")
      (insert "The attributes of the ECB-faces are:\n\n")
      (dolist (f ecb-face-list)
        (when f
          (insert (format "%s: %s\n"
                          (symbol-name f)
                          (funcall (if ecb-running-xemacs
                                       'face-custom-attributes-get
                                     'custom-face-attributes-get)
                                   f ecb-frame)))))
      (insert "\n-----------------------------------------------------\n\n"))
    (let* ((messages-buffer 
	    (get-buffer
	     (if ecb-running-xemacs " *Message-Log*" "*Messages*")))
	   (backtrace-buffer (get-buffer "*Backtrace*"))
           (tag-dump-buffer (get-buffer "*ecb-tag-dump*")))

      ;;insert the contents of the tag-dump buffer if it is there. 
      (insert "\n\n-----------------------------------------------------\n")
      (if tag-dump-buffer
          (progn
            (insert "The contents of the *ecb-tag-dump* buffer were\n\n")
	    (insert-buffer-substring tag-dump-buffer)
            ;; we must force the mark
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *ecb-tag-dump* buffer" ))
        (insert "There was no *ecb-tag-dump* buffer" ))
      (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the trace-output buffer if it is there. 
;;       (insert "\n\n-----------------------------------------------------\n")
;;       (if tag-dump-buffer
;;           (progn
;;             (insert "The contents of the *ecb-tag-dump* buffer were\n\n")
;; 	    (insert-buffer-substring tag-dump-buffer)
;;             ;; we must force the mark
;; 	    (goto-char (mark t))
;;             (insert "\nEnd Insert *ecb-tag-dump* buffer" ))
;;         (insert "There was no *ecb-tag-dump* buffer" ))
;;       (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the backtrace buffer if it is there. 
      (insert "\n\n-----------------------------------------------------\n")
      (if backtrace-buffer
          (progn
            (insert "The contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer-substring backtrace-buffer)
            ;; we must force the mark
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *Backtrace* buffer" ))
        (insert "There was no *Backtrace* buffer" ))
      (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the messages buffer if it is there. 
      (insert "-----------------------------------------------------\n")
      (if messages-buffer
          (progn
            (insert "The contents of the *Messages* buffer were\n\n")
	    (insert-buffer-substring messages-buffer)
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *Messages* buffer" ))
        (insert "There was no *Messages* buffer" ))
      (insert  "\n-----------------------------------------------------\n\n"))))


(defun ecb-problem-report-list-all-variables()
  "List all variables starting with `ecb-' and some other variables which
could be interesting for support."
  (let ((emacs-vars (sort (delete nil
                                  `(pre-command-hook
                                    post-command-hook
                                    after-save-hook
                                    help-mode-hook
                                    compilation-mode-hook
                                    truncate-partial-width-windows
                                    truncate-lines
                                    ,(if (boundp 'compilation-window-height)
                                         'compilation-window-height)
                                    ,(if (boundp 'temp-buffer-max-height)
                                         'temp-buffer-max-height)
                                    auto-mode-alist
                                    ,(if (boundp 'c-mode-hook)
                                         'c-mode-hook)
                                    ,(if (boundp 'c++-mode-hook)
                                         'c++-mode-hook)
                                    ,(if (boundp 'c-mode-common-hook)
                                         'c-mode-common-hook)
                                    ,(if (boundp 'java-mode-hook)
                                         'java-mode-hook)
                                    ,(if (boundp 'jde-mode-hook)
                                         'jde-mode-hook)
                                    system-type
                                    window-system
                                    max-specpdl-size
                                    max-lisp-eval-depth
                                    ,(if (boundp 'ediff-quit-hook)
                                         'ediff-quit-hook)))
                          (function (lambda (l r)
                                      (ecb-string< (symbol-name l)
                                                   (symbol-name r))))))
        (semantic-vars (sort (delete nil
                                     `(semantic-after-toplevel-cache-change-hook
                                       semantic-after-partial-cache-change-hook
                                       semantic-format-face-alist
                                       semantic-uml-colon-string
                                       semantic-orphaned-member-metaparent-type))
                             (function (lambda (l r)
                                         (ecb-string< (symbol-name l)
                                                      (symbol-name r))))))
        (speedbar-vars (sort '(speedbar-dynamic-tags-function-list
                               speedbar-tag-hierarchy-method
                               speedbar-tag-group-name-minimum-length
                               speedbar-tag-split-minimum-length
                               speedbar-tag-regroup-maximum-length
                               speedbar-fetch-etags-command
                               speedbar-fetch-etags-arguments
                               speedbar-fetch-etags-parse-list)
                             (function (lambda (l r)
                                         (ecb-string< (symbol-name l)
                                                      (symbol-name r))))))
        (ecb-options (mapcar
                      'intern
                      (sort
                       (let (completion-ignore-case)
                         (all-completions "ecb-" obarray 'user-variable-p))
                       'ecb-string<)))
        (ecb-internal-vars (sort '(ecb-path-selected-directory
                                   ecb-path-selected-source
                                   ecb-use-semantic-grouping
                                   ecb-autocontrol/sync-fcn-register
                                   ecb-idle-timer-alist
                                   ecb-post-command-hooks
                                   ecb-pre-command-hooks
                                   ecb-max-specpdl-size-old
                                   ecb-max-lisp-eval-depth-old
                                   ecb-minor-mode
                                   ecb-adviced-function-sets
                                   ecb-adviced-functions
                                   ecb-last-window-config-before-deactivation
                                   ecb-edit-area-creators
                                   ecb-stealthy-function-list
                                   ecb-stealthy-function-state-alist
                                   ecb-windows-hidden
                                   ecb-toggle-layout-state
                                   ecb-tree-buffer-creators
                                   ecb-tree-buffers
                                   ecb-buffer-setfunction-registration
                                   ecb-current-maximized-ecb-buffer-name
                                   ecb-special-ecb-buffers-of-current-layout)
                                 (function (lambda (l r)
                                             (ecb-string< (symbol-name l)
                                                          (symbol-name r)))))))
    (append emacs-vars semantic-vars speedbar-vars
            ecb-internal-vars ecb-options)))


(silentcomp-provide 'ecb-help)

;; ecb-help.el ends here
