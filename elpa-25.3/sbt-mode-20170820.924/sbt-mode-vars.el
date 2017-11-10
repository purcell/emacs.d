;;; sbt-mode-vars.el --- Custom variables for sbt-mode

;; Copyright (C) 2013 Heikki Vesalainen
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  All the user-customisable variables.
;;
;;; Code:

(defcustom sbt:program-name "sbt"
  "Program invoked by the `sbt:run-sbt' command."
  :type 'string
  :group 'sbt)

(defcustom sbt:program-options
  (when (eql system-type 'windows-nt)
    '("-Djline.terminal=jline.UnsupportedTerminal"))
  "Options passed to sbt by the `sbt:run-sbt' command."
  :type '(repeat string)
  :group 'sbt)

(defcustom sbt:default-command "test:compile"
  "The default command to run with sbt-command."
  :type 'string
  :group 'sbt)

(defcustom sbt:save-some-buffers t
  "Whether to run `save-some-buffers' before running a command."
  :type 'boolean
  :group 'sbt)

(defcustom sbt:clear-buffer-before-command t
  "Whether to clear the sbt buffer before running a command."
  :type 'boolean
  :group 'sbt)

(defcustom sbt:display-command-buffer t
  "Whether to display the buffer when running a command."
  :type 'boolean
  :group 'sbt)

(defcustom sbt:prefer-nested-projects nil
  "When finding sbt root directories, prefer nested projects.
If nil, outer projects are preferred."
  :type 'boolean
  :group 'sbt)

(defface sbt:error
  '((t :inherit error))
  "Face for displaying some sbt error messages"
  :group 'sbt)

(defface sbt:info
  '((t :inherit success))
  "A face for displaying some sbt info messages"
  :group 'sbt)

(defface sbt:warning
  '((t :inherit warning))
  "A face for displaying some sbt warning messages"
  :group 'sbt)

(defgroup sbt nil
  "Support for sbt build REPL."
  :group 'sbt
  :prefix "sbt:")

(provide 'sbt-mode-vars)

;;; sbt-mode-vars.el ends here
