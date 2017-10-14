;;; log4e.el --- provide logging framework for elisp

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: log
;; Package-Version: 20170401.604
;; URL: https://github.com/aki2o/log4e
;; Version: 0.3.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides logging framework for elisp.

;;; Dependency:
;; 
;; Nothing.

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your elisp file.
;; 
;; (require 'log4e)

;;; Configuration:
;; 
;; See <https://github.com/aki2o/log4e/blob/master/README.md>
;; Otherwise, eval following sexp.
;; (describe-function 'log4e:deflogger)

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "log4e:" :docstring t)
;; `log4e:next-log'
;; Move to start of next log on log4e-mode.
;; `log4e:previous-log'
;; Move to start of previous log on log4e-mode.
;; `log4e:insert-start-log-quickly'
;; Insert logging statment for trace level log at start of current function/macro.
;; 
;;  *** END auto-documentation
;; 
;; For detail, see <https://github.com/aki2o/log4e/blob/master/README.md>
;; 
;; [Note] Other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK


;; Enjoy!!!


;;; Code:
(eval-when-compile (require 'cl))
(require 'rx)


(defconst log4e-log-level-alist '((fatal . 6)
                                  (error . 5)
                                  (warn  . 4)
                                  (info  . 3)
                                  (debug . 2)
                                  (trace . 1))
  "Alist of log level value.")

(defconst log4e-default-logging-function-name-alist '((fatal . "log-fatal")
                                                      (error . "log-error")
                                                      (warn  . "log-warn")
                                                      (info  . "log-info")
                                                      (debug . "log-debug")
                                                      (trace . "log-trace"))
  "Alist of logging function name at default.")


(defmacro log4e--def-symmaker (symnm)
  `(progn
     (defsubst ,(intern (concat "log4e--make-symbol-" symnm)) (prefix)
       (intern (concat ,(format "log4e--%s-" symnm) prefix)))))

(log4e--def-symmaker "log-buffer")
(log4e--def-symmaker "msg-buffer")
(log4e--def-symmaker "log-template")
(log4e--def-symmaker "time-template")
(log4e--def-symmaker "min-level")
(log4e--def-symmaker "max-level")
(log4e--def-symmaker "toggle-logging")
(log4e--def-symmaker "toggle-debugging")
(log4e--def-symmaker "buffer-coding-system")
(log4e--def-symmaker "author-mail-address")

(defmacro log4e--def-level-logger (prefix suffix level)
  (let ((argform (if suffix
                     '(msg &rest msgargs)
                   '(level msg &rest msgargs)))
        (buff (log4e--make-symbol-log-buffer prefix))
        (codsys (log4e--make-symbol-buffer-coding-system prefix))
        (logtmpl (log4e--make-symbol-log-template prefix))
        (timetmpl (log4e--make-symbol-time-template prefix))
        (minlvl (log4e--make-symbol-min-level prefix))
        (maxlvl (log4e--make-symbol-max-level prefix))
        (logging-p (log4e--make-symbol-toggle-logging prefix)))
    `(progn

       ;; Define logging function
       (defun ,(intern (concat prefix "--" (or suffix "log"))) ,argform
         ,(format "Do logging for %s level log.
%sMSG/MSGARGS are passed to `format'."
                  (or (eval level) "any")
                  (if suffix "" "LEVEL is symbol as a log level in '(trace debug info warn error fatal).\n"))
         (let ((log4e--current-msg-buffer ,(log4e--make-symbol-msg-buffer prefix)))
           (apply 'log4e--logging ,buff ,codsys ,logtmpl ,timetmpl ,minlvl ,maxlvl ,logging-p ,(if suffix level 'level) msg msgargs)))
       
       ;; Define logging macro
       (defmacro ,(intern (concat prefix "--" (or suffix "log") "*")) ,argform
         ,(format "Do logging for %s level log.
%sMSG/MSGARGS are passed to `format'.
Evaluation of MSGARGS is invoked only if %s level log should be printed."
                  (or (eval level) "any")
                  (if suffix "" "LEVEL is symbol as a log level in '(trace debug info warn error fatal).\n")
                  (or (eval level) "the"))
         (let ((prefix ,prefix)
               (suffix ,suffix)
               (level ',level)
               (msg msg)
               (msgargs msgargs)
               (buff (log4e--make-symbol-log-buffer ,prefix))
               (codsys (log4e--make-symbol-buffer-coding-system ,prefix))
               (logtmpl (log4e--make-symbol-log-template ,prefix))
               (timetmpl (log4e--make-symbol-time-template ,prefix))
               (minlvl (log4e--make-symbol-min-level ,prefix))
               (maxlvl (log4e--make-symbol-max-level ,prefix))
               (logging-p (log4e--make-symbol-toggle-logging ,prefix)))
           `(let ((log4e--current-msg-buffer ,(log4e--make-symbol-msg-buffer prefix)))
              (when (and ,logging-p
                         (log4e--logging-level-p ,minlvl ,maxlvl ,level))
                (log4e--logging ,buff ,codsys ,logtmpl ,timetmpl ,minlvl ,maxlvl ,logging-p ,(if suffix level 'level) ,msg ,@msgargs)))))
       
       )))

(defsubst log4e--logging-level-p (minlevel maxlevel currlevel)
  (let ((minlvlvalue (or (assoc-default minlevel log4e-log-level-alist)
                         1))
        (maxlvlvalue (or (assoc-default maxlevel log4e-log-level-alist)
                         6))
        (currlvlvalue (or (assoc-default currlevel log4e-log-level-alist)
                          0)))
    (and (>= currlvlvalue minlvlvalue)
         (<= currlvlvalue maxlvlvalue))))

(defsubst log4e--get-or-create-log-buffer (buffnm &optional codesys)
  (or (get-buffer buffnm)
      (let ((buff (get-buffer-create buffnm)))
        (with-current-buffer buff
          (log4e-mode)
          (when codesys
            (setq buffer-file-coding-system codesys)))
        buff)))

(defvar log4e--regexp-msg-format
  (rx-to-string `(and "%"
                      (* (any "+#-0"))        ; flags
                      (* (any "0-9"))         ; width
                      (? "." (+ (any "0-9"))) ; precision
                      (any "a-zA-Z"))))

(defsubst log4e--insert-log (logtmpl timetmpl level msg msgargs propertize-p)
  (let ((timetext (format-time-string timetmpl))
        (lvltext (format "%-05s" (upcase (symbol-name level))))
        (buffer-read-only nil))
    (when propertize-p
      (put-text-property 0 (length timetext) 'face 'font-lock-doc-face timetext)
      (put-text-property 0 (length lvltext) 'face 'font-lock-keyword-face lvltext))
    (let* ((logtext logtmpl)
           (logtext (replace-regexp-in-string "%t" timetext logtext))
           (logtext (replace-regexp-in-string "%l" lvltext logtext))
           (logtext (replace-regexp-in-string "%m" msg logtext))
           (begin (point)))
      (insert logtext "\n")
      (when propertize-p
        (put-text-property begin (+ begin 1) 'log4e--level level))
      (loop initially (goto-char begin)
            while (and msgargs
                       (re-search-forward log4e--regexp-msg-format nil t))
            for currtype = (match-string-no-properties 0)
            for currarg = (pop msgargs)
            for failfmt = nil
            for currtext = (condition-case e
                               (format currtype currarg)
                             (error (setq failfmt t)
                                    (format "=%s=" (error-message-string e))))
            if propertize-p
            do (ignore-errors
                 (cond (failfmt (put-text-property 0 (length currtext) 'face 'font-lock-warning-face currtext))
                       (t       (put-text-property 0 (length currtext) 'face 'font-lock-string-face currtext))))
            do (replace-match currtext t t))
      (goto-char begin))))

(defvar log4e--current-msg-buffer nil)

;; We needs this signature be stay for other compiled plugins using old version
(defun log4e--logging (buffnm codsys logtmpl timetmpl minlevel maxlevel logging-p level msg &rest msgargs)
  (when (and logging-p
             (log4e--logging-level-p minlevel maxlevel level))
    (save-match-data
      (with-current-buffer (log4e--get-or-create-log-buffer buffnm codsys)
        (goto-char (point-max))
        (let* ((buffer-read-only nil)
               (begin (point))
               (currlog (progn
                          (log4e--insert-log logtmpl timetmpl level msg msgargs t)
                          (goto-char (point-max))
                          (buffer-substring-no-properties begin (point))))
               (msgbuf (or (when (and log4e--current-msg-buffer
                                      (not (eq log4e--current-msg-buffer t)))
                             (ignore-errors (get-buffer log4e--current-msg-buffer)))
                           log4e--current-msg-buffer)))
          (when msgbuf
            (let ((standard-output (if (buffer-live-p msgbuf)
                                       msgbuf
                                     standard-output)))
              (princ currlog))))
        nil))))

(defun log4e--get-current-log-line-level ()
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'log4e--level)))

;; We needs this signature be stay for other plugins compiled with this old version
(defun log4e--clear-log (buffnm)
  (with-current-buffer (log4e--get-or-create-log-buffer buffnm)
    (setq buffer-read-only nil)
    (erase-buffer)))

;; We needs this signature be stay for other plugins compiled with this old version
(defun log4e--open-log (buffnm)
  (let* ((buff (get-buffer buffnm)))
    (if (not (buffer-live-p buff))
        (message "[Log4E] Not exist log buffer.")
      (with-current-buffer buff
        (setq buffer-read-only t))
      (pop-to-buffer buff))))

;; We needs this signature be stay for other plugins compiled with this old version
(defun log4e--open-log-if-debug (buffnm dbg)
  (when dbg
    (log4e--open-log buffnm)))

;; (defun log4e--send-report-if-not-debug (buffnm dbg addr prefix)
;;   (let* ((buff (get-buffer buffnm)))
;;     (when (and (not dbg)
;;                (stringp addr)
;;                (buffer-live-p buff))
;;       (reporter-submit-bug-report addr prefix nil nil nil nil))))


(defmacro log4e:deflogger (prefix msgtmpl timetmpl &optional log-function-name-custom-alist)
  "Define the functions of logging for your elisp.

Specification:
 After eval this, you can use the functions for supporting about logging. They are the following ...
 - do logging for each log level. Log level are trace, debug, info, warn, error and fatal.
 - set max and min log level.
 - switch logging.
 - switch debugging.
 - open and clear log buffer.
 - send bug report for you.
 For details, see Functions section.

Argument:
 - PREFIX is string as your elisp prefix.
 - MSGTMPL is string as format of log. The following words has a special meaning.
   - %t ... Replaced with time string. About it, see TIMETMPL argument.
   - %l ... Replaced with log level. They are 'TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'.
   - %m ... Replaced with log message that passed by you.
 - TIMETMPL is string as format of time. This value is passed to `format-time-string'.
 - LOG-FUNCTION-NAME-CUSTOM-ALIST is alist as the function name of logging.
   - If this value is nil, define the following functions.
      yourprefix--log-trace
      yourprefix--log-debug
      ...
      yourprefix--log-fatal
   - If you want to custom the name of them, give like the following value.
      '((fatal . \"fatal\")
        (error . \"error\")
        (warn  . \"warn\")
        (info  . \"info\")
        (debug . \"debug\")
        (trace . \"trace\"))
     Then, define the following functions.
      yourprefix--trace
      yourprefix--debug
      ...
      yourprefix--fatal

Functions:
 List all functions defined below. PREFIX is your prefix.
 - PREFIX--log-fatal    ... #1
 - PREFIX--log-error    ... #1
 - PREFIX--log-warn     ... #1
 - PREFIX--log-info     ... #1
 - PREFIX--log-debug    ... #1
 - PREFIX--log-trace    ... #1
 - PREFIX--log-fatal*   ... #2
 - PREFIX--log-error*   ... #2
 - PREFIX--log-warn*    ... #2
 - PREFIX--log-info*    ... #2
 - PREFIX--log-debug*   ... #2
 - PREFIX--log-trace*   ... #2
 - PREFIX--log
 - PREFIX--log-set-level
 - PREFIX--log-enable-logging            ... #3
 - PREFIX--log-disable-logging           ... #3
 - PREFIX--log-enable-messaging          ... #3
 - PREFIX--log-disable-messaging         ... #3
 - PREFIX--log-enable-debugging          ... #3
 - PREFIX--log-disable-debugging         ... #3
 - PREFIX--log-debugging-p
 - PREFIX--log-set-coding-system
 - PREFIX--log-set-author-mail-address
 - PREFIX--log-clear-log                 ... #3
 - PREFIX--log-open-log                  ... #3
 - PREFIX--log-open-log-if-debug

 #1 : You can customize this name
 #2 : Name is a #1 name + \"*\"
 #3 : This is command

Example:
;; If you develop elisp that has prefix \"hoge\", write and eval the following sexp in your elisp file.

 (require 'log4e)
 (log4e:deflogger \"hoge\" \"%t [%l] %m\" \"%H:%M:%S\")

;; Eval the following
 (hoge--log-enable-logging)

;; Then, write the following

 (defun hoge-do-hoge (hoge)
   (if (not (stringp hoge))
       (hoge--log-fatal \"failed do hoge : hoge is '%s'\" hoge)
     (hoge--log-debug \"start do hoge about '%s'\" hoge)
     (message \"hoge!\")
     (hoge--log-info \"done hoge about '%s'\" hoge)))

;; Eval the following
 (hoge-do-hoge \"HOGEGE\")

;; Do M-x hoge--log-open-log
;; Open the buffer which name is \" *log4e-hoge*\". The buffer string is below
12:34:56 [INFO ] done hoge about 'HOGEGE'

;; Eval the following
 (hoge--log-set-level 'trace)
 (hoge-do-hoge \"FUGAGA\")

;; Do M-x hoge--log-open-log
;; Open the buffer. its string is below
12:34:56 [INFO ] done hoge about 'HOGEGE'
12:35:43 [DEBUG] start do hoge about 'FUGAGA'
12:35:43 [INFO ] done hoge about 'FUGAGA'
 
"
  (declare (indent 0))
  (if (or (not (stringp prefix))   (string= prefix "")
          (not (stringp msgtmpl))  (string= msgtmpl "")
          (not (stringp timetmpl)) (string= timetmpl ""))
      (message "[LOG4E] invalid argument of deflogger")
    (let* ((bufsym (log4e--make-symbol-log-buffer prefix))
           (msgbufsym (log4e--make-symbol-msg-buffer prefix))
           (logtmplsym (log4e--make-symbol-log-template prefix))
           (timetmplsym (log4e--make-symbol-time-template prefix))
           (minlvlsym (log4e--make-symbol-min-level prefix))
           (maxlvlsym (log4e--make-symbol-max-level prefix))
           (tglsym (log4e--make-symbol-toggle-logging prefix))
           (dbgsym (log4e--make-symbol-toggle-debugging prefix))
           (codsyssym (log4e--make-symbol-buffer-coding-system prefix))
           (addrsym (log4e--make-symbol-author-mail-address prefix))
           (funcnm-alist (loop with custom-alist = (car (cdr log-function-name-custom-alist))
                                  for lvl in '(fatal error warn info debug trace)
                                  for lvlpair = (assq lvl custom-alist)
                                  for fname = (or (cdr-safe lvlpair) "")
                                  collect (or (if (string-match "\*" fname)
                                                  (progn
                                                    (message "[LOG4E] ignore %s level name in log-function-name-custom-alist. can't use '*' for the name." lvl)
                                                    nil)
                                                lvlpair)
                                              (assq lvl log4e-default-logging-function-name-alist)))))
      `(progn

         ;; Define variable for prefix
         (defvar ,bufsym (format " *log4e-%s*" ,prefix))
         (defvar ,logtmplsym ,msgtmpl)
         (defvar ,timetmplsym ,timetmpl)
         (defvar ,minlvlsym 'info)
         (defvar ,maxlvlsym 'fatal)
         (defvar ,tglsym nil)
         (defvar ,msgbufsym nil)
         (defvar ,dbgsym nil)
         (defvar ,codsyssym nil)
         (defvar ,addrsym nil)

         ;; Define level set function
         (defun ,(intern (concat prefix "--log-set-level")) (minlevel &optional maxlevel)
           "Set range for doing logging.

MINLEVEL is symbol of lowest level for doing logging. its default is 'info.
MAXLEVEL is symbol of highest level for doing logging. its default is 'fatal."
           (setq ,minlvlsym minlevel)
           (setq ,maxlvlsym maxlevel))

         ;; Define logging toggle function
         (defun ,(intern (concat prefix "--log-enable-logging")) ()
           "Enable logging by logging functions."
           (interactive)
           (setq ,tglsym t))
         (defun ,(intern (concat prefix "--log-disable-logging")) ()
           "Disable logging by logging functions."
           (interactive)
           (setq ,tglsym nil))

         ;; Define messaging toggle function
         (defun ,(intern (concat prefix "--log-enable-messaging")) (&optional buffer)
           "Enable dump the log into other buffer by logging functions.

BUFFER is a buffer dumped log into. nil means *Messages* buffer."
           (interactive)
           (setq ,msgbufsym (or buffer t)))
         (defun ,(intern (concat prefix "--log-disable-messaging")) ()
           "Disable dump the log into other buffer by logging functions."
           (interactive)
           (setq ,msgbufsym nil))

         ;; Define debugging toggle function
         (defun ,(intern (concat prefix "--log-enable-debugging")) ()
           "Enable debugging and logging.

`PREFIX--log-debugging-p' will return t."
           (interactive)
           (setq ,tglsym t)
           (setq ,dbgsym t))
         (defun ,(intern (concat prefix "--log-disable-debugging")) ()
           "Disable debugging.

`PREFIX--log-debugging-p' will return nil."
           (interactive)
           (setq ,dbgsym nil))
         (defun ,(intern (concat prefix "--log-debugging-p")) ()
           ,dbgsym)

         ;; Define coding system set funtion
         (defun ,(intern (concat prefix "--log-set-coding-system")) (coding-system)
           "Set charset and linefeed of LOG-BUFFER.

CODING-SYSTEM is symbol for setting to `buffer-file-coding-system'.
LOG-BUFFER is a buffer which name is \" *log4e-PREFIX*\"."
           (setq ,codsyssym coding-system))

         ;;          ;; Define author mail set function
         ;;          (defun ,(intern (concat prefix "--log-set-author-mail-address")) (before-atmark after-atmark)
         ;;            "Set mail address of author for elisp that has PREFIX. This value is used SEND-REPORT.

         ;; BEFORE-ATMARK is string as part of mail address. If your address is \"hoge@example.co.jp\", it is \"hoge\".
         ;; AFTER-ATMARK is string as part of mail address. If your address is \"hoge@example.co.jp\", it is \"example.co.jp\".
         ;; SEND-REPORT is `PREFIX--log-send-report-if-not-debug'."
         ;;            (setq ,addrsym (concat before-atmark "@" after-atmark)))

         ;; Define log buffer handle function
         (defun ,(intern (concat prefix "--log-clear-log")) ()
           "Clear buffer string of buffer which name is \" *log4e-PREFIX*\"."
           (interactive)
           (log4e--clear-log ,bufsym))
         (defun ,(intern (concat prefix "--log-open-log")) ()
           "Open buffer which name is \" *log4e-PREFIX*\"."
           (interactive)
           (log4e--open-log ,bufsym))
         (defun ,(intern (concat prefix "--log-open-log-if-debug")) ()
           "Open buffer which name is \" *log4e-PREFIX*\" if debugging is enabled."
           (log4e--open-log-if-debug ,bufsym ,dbgsym))

         ;;          ;; Define report send function
         ;;          (defun ,(intern (concat prefix "--log-send-report-if-not-debug")) ()
         ;;            "Send bug report to author if debugging is disabled.

         ;; The author mailaddress is set by `PREFIX--log-set-author-mail-address'.
         ;; About the way of sending bug report, see `reporter-submit-bug-report'."
         ;;            (log4e--send-report-if-not-debug ,bufsym ,dbgsym ,addrsym ,prefix))

         ;; Define each level logging function
         (log4e--def-level-logger ,prefix nil nil)
         (log4e--def-level-logger ,prefix ,(assoc-default 'fatal funcnm-alist) 'fatal)
         (log4e--def-level-logger ,prefix ,(assoc-default 'error funcnm-alist) 'error)
         (log4e--def-level-logger ,prefix ,(assoc-default 'warn  funcnm-alist) 'warn)
         (log4e--def-level-logger ,prefix ,(assoc-default 'info  funcnm-alist) 'info)
         (log4e--def-level-logger ,prefix ,(assoc-default 'debug funcnm-alist) 'debug)
         (log4e--def-level-logger ,prefix ,(assoc-default 'trace funcnm-alist) 'trace)
         
         ))))


;;;###autoload
(define-derived-mode log4e-mode view-mode "Log4E"
  "Major mode for browsing a buffer made by log4e.

\\<log4e-mode-map>
\\{log4e-mode-map}"
  (define-key log4e-mode-map (kbd "J") 'log4e:next-log)
  (define-key log4e-mode-map (kbd "K") 'log4e:previous-log))

(defun log4e:next-log ()
  "Move to start of next log on log4e-mode."
  (interactive)
  (let* ((level))
    (while (and (not level)
                (< (point) (point-max)))
      (forward-line 1)
      (setq level (log4e--get-current-log-line-level)))
    level))

(defun log4e:previous-log ()
  "Move to start of previous log on log4e-mode."
  (interactive)
  (let* ((level))
    (while (and (not level)
                (> (point) (point-min)))
      (forward-line -1)
      (setq level (log4e--get-current-log-line-level)))
    level))

;;;###autoload
(defun log4e:insert-start-log-quickly ()
  "Insert logging statment for trace level log at start of current function/macro."
  (interactive)
  (let* ((fstartpt (when (re-search-backward "(\\(?:defun\\|defmacro\\|defsubst\\)\\*? +\\([^ ]+\\) +(\\([^)]*\\))" nil t)
                     (point)))
         (fncnm (when fstartpt (match-string-no-properties 1)))
         (argtext (when fstartpt (match-string-no-properties 2)))
         (prefix (save-excursion
                   (goto-char (point-min))
                   (loop while (re-search-forward "(log4e:deflogger[ \n]+\"\\([^\"]+\\)\"" nil t)
                            for prefix = (match-string-no-properties 1)
                            for currface = (get-text-property (match-beginning 0) 'face)
                            if (not (eq currface 'font-lock-comment-face))
                            return prefix))))
    (when (and fstartpt prefix)
      (let* ((fncnm (replace-regexp-in-string (concat "\\`" prefix "[^a-zA-Z0-9]+") "" fncnm))
             (fncnm (replace-regexp-in-string "-" " " fncnm))
             (argtext (replace-regexp-in-string "\n" " " argtext))
             (argtext (replace-regexp-in-string "^ +" "" argtext))
             (argtext (replace-regexp-in-string " +$" "" argtext))
             (args (split-string argtext " +"))
             (args (loop for arg in args
                            if (and (not (string= arg ""))
                                    (not (string-match "\\`&" arg)))
                            collect arg))
             (logtext (loop with ret = (format "start %s." fncnm)
                               for arg in args
                               do (setq ret (concat ret " " arg "[%s]"))
                               finally return ret))
             (sexpformat (loop with ret = "(%s--log 'trace \"%s\""
                                  for arg in args
                                  do (setq ret (concat ret " %s"))
                                  finally return (concat ret ")")))
             (inserttext (apply 'format sexpformat prefix logtext args)))
        (forward-char)
        (forward-sexp 3)
        (when (re-search-forward "\\=[ \n]+\"" nil t)
          (forward-char -1)
          (forward-sexp))
        (newline-and-indent)
        (insert inserttext)))))


(provide 'log4e)
;;; log4e.el ends here
