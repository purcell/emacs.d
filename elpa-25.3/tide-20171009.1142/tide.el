;;; tide.el --- Typescript Interactive Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/tide
;; Version: 2.5.3
;; Keywords: typescript
;; Package-Requires: ((dash "2.10.0") (s "1.11.0") (flycheck "27") (typescript-mode "0.1") (cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'typescript-mode)
(require 'etags)
(require 'json)
(require 'cl-lib)
(require 'eldoc)
(require 'dash)
(require 's)
(require 'flycheck)
(require 'imenu)
(require 'thingatpt)
(require 'tide-lv)

;; Silence compiler warnings

(defvar js2-basic-offset)
(defvar js-indent-level)
(defvar js3-indent-level)
(defvar web-mode-code-indent-offset)
(defvar sgml-basic-offset)
(defvar company-backends)

(declare-function company-grab-symbol-cons "company.el" (idle-begin-after-re &optional max-len))
(declare-function company-begin-backend "company.el" (backend &optional callback))
(declare-function company-in-string-or-comment "company.el" nil)

(defgroup tide nil
  "TypeScript Interactive Development Environment."
  :prefix "tide-"
  :group 'tools)

(defcustom tide-sync-request-timeout 2
  "The number of seconds to wait for a sync response."
  :type 'integer
  :group 'tide)

(defcustom tide-tsserver-process-environment '()
  "List of extra environment variables to use when starting tsserver."
  :type '(repeat string)
  :group 'tide)

(defcustom tide-tsserver-executable nil
  "Name of tsserver executable to run instead of the bundled tsserver.

This may either be an absolute path or a relative path. Relative
paths are resolved against the project root directory.

Note that this option only works with TypeScript version 2.0 and
above."
  :type '(choice (const nil) string)
  :group 'tide)

(defcustom tide-node-executable "node"
  "Name or path of the node executable binary file."
  :type '(choice (const nil) string)
  :group 'tide)

(defcustom tide-post-code-edit-hook nil
  "Hook run after code edits are applied in a buffer."
  :type 'hook
  :group 'tide)

(defvar tide-format-options '()
  "Format options plist.")

(defvar tide-completion-ignore-case nil
  "CASE will be ignored in completion if set to non-nil.")

(defvar tide-completion-detailed nil
  "Completion dropdown will contain detailed method information if set to non-nil.")

(defface tide-file
  '((t (:inherit dired-header)))
  "Face for file names in references output."
  :group 'tide)

(defface tide-line-number
  '((t (:inherit compilation-line-number)))
  "Face for line numbers in references output."
  :group 'tide)

(defface tide-match
  '((t (:inherit match)))
  "Face for matched symbol in references output."
  :group 'tide)

(defface tide-imenu-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for type in imenu list."
  :group 'tide)

(defface tide-choice-face
  '((t (:inherit font-lock-warning-face)))
  "Face for choices used in popup window."
  :group 'tide)

(defcustom tide-jump-to-definition-reuse-window t
  "Reuse existing window when jumping to definition."
  :type 'boolean
  :group 'tide)

(defcustom tide-imenu-flatten nil
  "Imenu index will be flattened if set to non-nil."
  :type 'boolean
  :group 'tide)

(defcustom tide-allow-popup-select '(code-fix refactor)
  "The list of commands where popup selection is allowed."
  :type '(set (const code-fix) (const jump-to-implementation) (const refactor))
  :group 'tide)

(defcustom tide-always-show-documentation nil
  "Show the documentation window even if only type information is available."
  :type 'boolean
  :group 'tide)

(defcustom tide-server-max-response-length 102400
  "Maximum allowed response length from tsserver. Any response greater than this would be ignored."
  :type 'integer
  :group 'tide)

(defcustom tide-tsserver-locator-function #'tide-tsserver-locater-npmlocal-projectile-npmglobal
  "Function used by tide to locate tsserver."
  :type 'function
  :group 'tide)

(defmacro tide-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defvar tide-supported-modes '(typescript-mode web-mode js-mode js2-mode js2-jsx-mode js3-mode rjsx-mode))

(defvar tide-server-buffer-name "*tide-server*")
(defvar tide-request-counter 0)
(defvar tide-project-configs (make-hash-table :test 'equal))
(defvar tide-max-response-length-error-message "Reponse length from tsserver is greater than maximum allowed response.")

(tide-def-permanent-buffer-local tide-project-root nil)
(tide-def-permanent-buffer-local tide-buffer-dirty nil)
(tide-def-permanent-buffer-local tide-buffer-tmp-file nil)
(tide-def-permanent-buffer-local tide-active-buffer-file-name nil)

(defvar tide-servers (make-hash-table :test 'equal))
(defvar tide-response-callbacks (make-hash-table :test 'equal))

(defvar tide-source-root-directory (file-name-directory (or load-file-name buffer-file-name)))
(defvar tide-tsserver-directory (expand-file-name "tsserver" tide-source-root-directory))

(defun tide-project-root ()
  "Project root folder determined based on the presence of tsconfig.json."
  (or
   tide-project-root
   (let ((root (or (locate-dominating-file default-directory "tsconfig.json")
                   (locate-dominating-file default-directory "jsconfig.json"))))
     (unless root
       (message (tide-join (list "Couldn't locate project root folder with a tsconfig.json or jsconfig.json file. Using '" default-directory "' as project root.")))
       (setq root default-directory))
     (let ((full-path (expand-file-name root)))
       (setq tide-project-root full-path)
       full-path))))

(defun tide-project-name ()
  (let ((full-path (directory-file-name (tide-project-root))))
    (concat (file-name-nondirectory full-path) "-" (substring (md5 full-path) 0 10))))

;;; Compatibility

(defvar tide-tsserver-unsupported-commands (make-hash-table :test 'equal))

(defun tide-mark-as-unsupported (command)
  (puthash
   (tide-project-name)
   (cl-pushnew
    command
    (gethash (tide-project-name) tide-tsserver-unsupported-commands '()))
   tide-tsserver-unsupported-commands))

(defun tide-unsupported-p (command)
  (member command (gethash (tide-project-name) tide-tsserver-unsupported-commands '())))

(defmacro tide-fallback-if-not-supported (new-command new old cb)
  `(if (tide-unsupported-p ,new-command)
       (,old ,cb)
     (,new
      (lambda (response)
        (if (tide-command-unknown-p response)
            (progn
              (tide-mark-as-unsupported ,new-command)
              (,old ,cb))
          (funcall ,cb response))))))

;;; Helpers

(defun tide-safe-json-read-file (filename)
  (condition-case nil
      (let ((json-object-type 'plist))
        (json-read-file filename))
    (error '())))

(defun tide-plist-get (list &rest args)
  (cl-reduce
   (lambda (object key)
     (when object
       (plist-get object key)))
   args
   :initial-value list))

(defun tide-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists. Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun tide-response-success-p (response)
  (and response (equal (plist-get response :success) t)))

(defun tide-command-unknown-p (response)
  (and response (string-equal (plist-get response :command) "unknown")))

(defun tide-tsserver-version-not-supported ()
  (error "Only tsserver 2.0 or greater is supported. Upgrade your tsserver or use older version of tide."))

(defun tide-tsserver-feature-not-supported (min-version)
  (error "tsserver %S or greater is required for this feature." min-version))

(defmacro tide-on-response-success (response ignore-empty-response &rest body)
  (declare (indent 2))
  `(if (tide-response-success-p ,response)
       (progn
         ,@body)
     (-when-let (msg (plist-get response :message))
       (unless (and ,ignore-empty-response (string-equal msg "No content available."))
         (message "%s" msg)))
     nil))

(defmacro tide-on-response-success-callback (response ignore-empty-response &rest body)
  (declare (indent 2))
  `(lambda (,response)
     (tide-on-response-success ,response ,ignore-empty-response
       ,@body)))

(defun tide-join (list)
  (mapconcat 'identity list ""))

(defun tide-each-buffer (project-name fn)
  "Callback FN for each buffer within PROJECT-NAME with tide-mode enabled."
  (-each (buffer-list)
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p tide-mode)
                   (equal (tide-project-name) project-name))
          (funcall fn))))))

(defun tide-line-number-at-pos (&optional pos)
  (let ((p (or pos (point))))
    (if (= (point-min) 1)
        (line-number-at-pos p)
      (save-excursion
        (save-restriction
          (widen)
          (line-number-at-pos p))))))

(defun tide-current-offset ()
  "Number of characters present from the begining of line to cursor in current line.

offset is one based."
  (1+ (- (point) (line-beginning-position))))

(defun tide-offset (pos)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (tide-current-offset))))

(defun tide-column (line offset)
  "Return column number corresponds to the LINE and OFFSET.

LINE is one based, OFFSET is one based and column is zero based"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (beginning-of-line)
      (while (> offset 1)
        (forward-char)
        (cl-decf offset))
      (1+ (current-column)))))

(defun tide-span-to-position (span)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- (plist-get span :line)))
      (beginning-of-line)
      (forward-char (1- (plist-get span :offset)))
      (point))))

(defun tide-doc-buffer (string)
  (with-current-buffer (get-buffer-create "*tide-documentation*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when string
        (save-excursion
          (tide-insert string))))
    (local-set-key (kbd "q") #'quit-window)
    (current-buffer)))


(defvar tide-alphabets '(?a ?s ?d ?f ?j ?k ?l))

(defun tide-popup-select-item (prompt list)
  (let ((hints (-map-indexed
                (lambda (i item)
                  (concat (propertize (char-to-string (nth i tide-alphabets)) 'face 'tide-choice-face)
                          "  "
                          item))
                list)))
    (unwind-protect
        (progn
          (tide-lv-message (mapconcat 'identity hints "\n"))
          (let ((selected (read-char-choice prompt (-take (length list) tide-alphabets))))
            (nth (-find-index (lambda (char) (eql selected char)) tide-alphabets) list)))
        (tide-lv-delete-window))))

(defun tide-completing-read-select-item (prompt list)
  (completing-read prompt list nil t))

(defun tide-can-use-popup-p (feature)
  (member feature tide-allow-popup-select))

(defun tide-select-item-from-list (prompt list label-fn allow-popup)
  (let ((collection (make-hash-table :test 'equal)))
    (dolist (item list)
      (puthash (funcall label-fn item) item collection))
    (let ((selected-text
           (if (and (<= (length list) (length tide-alphabets)) allow-popup)
               (tide-popup-select-item prompt (hash-table-keys collection))
             (tide-completing-read-select-item prompt (hash-table-keys collection)))))
      (gethash selected-text collection))))

;;; Events

(defvar tide-event-listeners (make-hash-table :test 'equal))

(defun tide-set-event-listener (listener)
  (puthash (tide-project-name) (cons (current-buffer) listener) tide-event-listeners))

(defun tide-clear-event-listener ()
  (remhash (tide-project-name) tide-event-listeners))

;;; Server

(defun tide-current-server ()
  (gethash (tide-project-name) tide-servers))

(defun tide-next-request-id ()
  (number-to-string (cl-incf tide-request-counter)))

(defun tide-dispatch-response (response)
  (let* ((request-id (plist-get response :request_seq))
         (callback (gethash request-id tide-response-callbacks)))
    (when callback
      (let ((buffer (car callback)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (apply (cdr callback) (list response)))))
      (remhash request-id tide-response-callbacks))))

(defun tide-dispatch-event (event)
  (-when-let (listener (gethash (tide-project-name) tide-event-listeners))
    (with-current-buffer (car listener)
      (apply (cdr listener) (list event)))))

(defun tide-dispatch (response)
  (cl-case (intern (plist-get response :type))
    ('response (tide-dispatch-response response))
    ('event (tide-dispatch-event response))))

(defun tide-send-command (name args &optional callback)
  (when (not (tide-current-server))
    (error "Server does not exist. Run M-x tide-restart-server to start it again"))

  (tide-sync-buffer-contents)

  (let* ((request-id (tide-next-request-id))
         (command `(:command ,name :seq ,request-id :arguments ,args))
         (json-encoding-pretty-print nil)
         (encoded-command (json-encode command))
         (payload (concat encoded-command "\n")))
    (process-send-string (tide-current-server) payload)
    (when callback
      (puthash request-id (cons (current-buffer) callback) tide-response-callbacks))))

(defun tide-send-command-sync (name args)
  (let* ((start-time (current-time))
         (response nil))
    (tide-send-command name args (lambda (resp) (setq response resp)))
    (while (not response)
      (accept-process-output nil 0.01)
      (when (> (cadr (time-subtract (current-time) start-time))
               tide-sync-request-timeout)
        (error "Sync request timed out %s" name)))
    response))

(defun tide-net-filter (process data)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data))
  (tide-decode-response process))

(defun tide-net-sentinel (process message)
  (let ((project-name (process-get process 'project-name)))
    (message "(%s) tsserver exits: %s." project-name (string-trim message))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (tide-cleanup-project project-name)))

(defun tide--npm-local ()
  "Return a single path to the package-local typescript package directory or nil."

  (-when-let (packages-folder (locate-dominating-file default-directory "package.json"))
    (concat packages-folder "node_modules/typescript/lib/")))

(defun tide--npm-global ()
  "Return a single path to the global typescript package directory or nil."

  (if (eq system-type 'windows-nt)
      (concat (getenv "appdata") "\\npm\\node_modules")
    "/usr/lib/node_modules/typescript/lib/"))

(defun tide--npm-global-usrlocal ()
  "Return a single path to the global typescript package directory or nil."

  ;; this check does not apply to windows.
  (if (eq system-type 'windows-nt)
      nil
    "/usr/local/lib/node_modules/typescript/lib/"))

(defun tide--project-package ()
  "Return the package's node_module bin directory using projectile's project root or nil."
  (when (and (functionp 'projectile-project-p)
             (functionp 'projectile-project-root)
             (projectile-project-p))
    (concat (projectile-project-root) "node_modules/typescript/lib")))

(defconst tide--tsserver "tsserver.js"
  "File-name of the file that node executes to start the typescript server.")

(defun tide--locate-tsserver (path)
  "Locate the typescript server in PATH.
Return a string representing the existing full path or nil."
  (let ((exe (expand-file-name tide--tsserver path)))
    (when (file-exists-p exe) exe)))

(defun tide-tsserver-locater-npmlocal-projectile-npmglobal ()
  "Locate tsserver through project-local or global system settings."
  (or
   (tide--locate-tsserver (tide--npm-local))
   (tide--locate-tsserver (tide--project-package))
   (tide--locate-tsserver (tide--npm-global))
   (tide--locate-tsserver (tide--npm-global-usrlocal))))

(defun tide-locate-tsserver-executable ()
  "Locate the typescript server executable.
If TIDE-TSSERVER-EXECUTABLE is set by the user use it.  Otherwise check in the npm local package directory, in the project root as defined by projectile, and in the npm global installation.  If nothing is found use the bundled version."
  (or
   (and tide-tsserver-executable (expand-file-name tide-tsserver-executable))
   (funcall tide-tsserver-locator-function)
   (expand-file-name tide--tsserver tide-tsserver-directory)))

(defun tide-start-server ()
  (when (tide-current-server)
    (error "Server already exist"))

  (message "(%s) Starting tsserver..." (tide-project-name))
  (let* ((default-directory (tide-project-root))
         (process-environment (append tide-tsserver-process-environment process-environment))
         (buf (generate-new-buffer tide-server-buffer-name))
         (tsserverjs (tide-locate-tsserver-executable))
         ;; Use a pipe to communicate with the subprocess. This fixes a hang
         ;; when a >1k message is sent on macOS.
         (process-connection-type nil)
         (process
          (start-file-process "tsserver" buf tide-node-executable tsserverjs)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (set-process-filter process #'tide-net-filter)
    (set-process-sentinel process #'tide-net-sentinel)
    (set-process-query-on-exit-flag process nil)
    (with-current-buffer (process-buffer process)
      (buffer-disable-undo))
    (process-put process 'project-name (tide-project-name))
    (puthash (tide-project-name) process tide-servers)
    (message "(%s) tsserver server started successfully." (tide-project-name))))

(defun tide-cleanup-buffer-callbacks ()
  (let ((error-response `(:success ,nil)))
    (maphash
     (lambda (id callback)
       (when (equal (current-buffer) (car callback))
         (funcall (cdr callback) error-response)
         (remhash id tide-response-callbacks)))
     tide-response-callbacks)))

(defun tide-cleanup-project (project-name)
  (tide-each-buffer project-name
                    (lambda ()
                      (tide-cleanup-buffer-callbacks)))
  (remhash project-name tide-servers)
  (remhash project-name tide-tsserver-unsupported-commands)
  (remhash project-name tide-project-configs))

(defun tide-start-server-if-required ()
  (when (not (tide-current-server))
    (tide-start-server)))

(defun tide-decode-response-legth ()
  (goto-char (point-min))
  (when (re-search-forward "Content-Length: \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun tide-enough-response-p (length)
  (save-excursion
    (when (search-forward "{" nil t)
      (backward-char 1)
      (>= (- (position-bytes (point-max)) (position-bytes (point))) (1- length)))))

(defun tide-decode-response (process)
  (with-current-buffer (process-buffer process)
    (let ((length (tide-decode-response-legth))
          (json-object-type 'plist)
          (json-array-type 'list))
      (when (and length (tide-enough-response-p length))
        (search-forward "{")
        (backward-char 1)
        (let ((response (if (> length tide-server-max-response-length)
                            (let ((seq (when (re-search-forward "\"request_seq\":\"\\([0-9]+\\)\"" nil t)
                                         (match-string 1))))
                              (forward-line 1)
                              (when seq
                                `(:success :json-false :type "response" :message ,tide-max-response-length-error-message :request_seq ,seq)))
                          (json-read-object))))
          (delete-region (point-min) (point))
          (when response
            (tide-dispatch response)))
        (when (>= (buffer-size) 16)
          (tide-decode-response process))))))

;;; Initialization

(defun tide-file-format-options ()
  (tide-combine-plists
   `(:tabSize ,tab-width :indentSize ,(tide-current-indentsize))
   tide-format-options
   (tide-tsfmt-options)))

(defun tide-tsfmt-options ()
  (let ((config-file (expand-file-name "tsfmt.json" (tide-project-root))))
    (when (file-exists-p config-file)
      (tide-safe-json-read-file config-file))))

(defun tide-current-indentsize ()
  (pcase major-mode
    (`typescript-mode typescript-indent-level)
    (`js2-mode js2-basic-offset)
    (`js-mode js-indent-level)
    (`js3-mode js3-indent-level)
    (`web-mode web-mode-code-indent-offset)
    (`js2-jsx-mode sgml-basic-offset)
    (`rjsx-mode sgml-basic-offset)
    (_ standard-indent)))

(defun tide-command:configure ()
  (tide-send-command "configure" `(:hostInfo ,(emacs-version) :file ,buffer-file-name :formatOptions ,(tide-file-format-options))))

(defun tide-command:projectInfo (cb &optional need-file-name-list)
  (tide-send-command "projectInfo" `(:file ,buffer-file-name :needFileNameList ,need-file-name-list) cb))

(defun tide-command:openfile ()
  (tide-send-command "open"
                     (append `(:file ,buffer-file-name)
                             (let ((extension (upcase (file-name-extension buffer-file-name))))
                               (if (member extension '("TS" "JS" "TSX" "JSX"))
                                   `(:scriptKindName ,extension)
                                 nil)))))

(defun tide-command:closefile ()
  (tide-send-command "close" `(:file ,buffer-file-name)))

;;; Jump to definition

(defun tide-command:definition (cb)
  (tide-send-command
   "definition"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))
   cb))

(defun tide-command:typeDefinition (cb)
  (tide-send-command
   "typeDefinition"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))
   cb))

(defun tide-jump-to-definition (&optional arg)
  "Jump to the definition of the symbol at point.

If pointed at an abstract member-declaration, will proceed to look for
implementations.  When invoked with a prefix arg, jump to the type definition."
  (interactive "P")
  (let ((cb (lambda (response)
              (tide-on-response-success response nil
                (-when-let (filespan (car (plist-get response :body)))
                  ;; if we're still at the same location...
                  ;; maybe we're a abstract member which has impementations?
                  (if (and (not arg)
                           (tide-filespan-is-current-location-p filespan))
                      (tide-jump-to-implementation)
                    (tide-jump-to-filespan filespan tide-jump-to-definition-reuse-window)))))))
    (if arg
        (tide-command:typeDefinition cb)
      (tide-command:definition cb))))

(defun tide-filespan-is-current-location-p (filespan)
  (let* ((location (plist-get filespan :start))
         (new-file-name (plist-get filespan :file)))
    (and (string-equal new-file-name buffer-file-name)
         (equal (tide-location-to-point location) (point)))))

(defun tide-move-to-location (location)
  (let* ((line (plist-get location :line))
         (offset (plist-get location :offset)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    (forward-char (1- offset))))

(defun tide-location-to-point (location)
  (save-excursion
    (tide-move-to-location location)
    (point)))

(defun tide-jump-to-filespan (filespan &optional reuse-window no-marker)
  (let ((file (plist-get filespan :file)))
    (unless no-marker
      (ring-insert find-tag-marker-ring (point-marker)))
    (if reuse-window
        (pop-to-buffer (find-file-noselect file) '((display-buffer-reuse-window display-buffer-same-window)))
      (pop-to-buffer (find-file-noselect file)))
    (tide-move-to-location (plist-get filespan :start))))

(defalias 'tide-jump-back 'pop-tag-mark)

;;; Jump to implementation

(defun tide-command:implementation ()
  (tide-send-command-sync "implementation" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-jump-to-implementation-format-item (item)
  (let* ((file-name (plist-get item :file))
         (line (save-excursion
                 (with-current-buffer (find-file-noselect file-name)
                   (tide-move-to-location (plist-get item :start))
                   (replace-regexp-in-string "\n" "" (thing-at-point 'line)))))
         (file-pos (concat
                    (propertize (file-name-nondirectory file-name)
                                'face 'tide-file)
                    ":"
                    (propertize (number-to-string (tide-plist-get item :start :line))
                                'face 'tide-line-number))))
    (concat
     line
     " "
     file-pos)))

(defun tide-jump-to-implementation ()
  "Jump to a implementation of the symbol at point."
  (interactive)
  (let ((response (tide-command:implementation)))
    (tide-on-response-success response nil
      (let ((impls (plist-get response :body)))
        (cond ((= 0 (length impls)) (message "No implementations available."))
              ((= 1 (length impls)) (tide-jump-to-filespan (car impls)))
              (t (tide-jump-to-filespan
                  (tide-select-item-from-list "Select implementation: " impls #'tide-jump-to-implementation-format-item (tide-can-use-popup-p 'jump-to-implementation)))))))))

;;; Navigate to named member

(defun tide-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun tide-get-symbol-at-point ()
  "Returns the symbol found at point, if not deemed 'noise'.
Noise can be anything like braces, reserved keywords, etc."

  (when (not (or (tide-in-string-p)
                 (member (face-at-point) '(font-lock-keyword-face))))
    ;; we could have used symbol-at-point here, but that leaves us unable to
    ;; differentiate between a symbol named nil and no symbol at all.
    ;; thing-at-point returns a string OR nil, which means we don't get this problem.
    (let ((symbol (thing-at-point 'symbol)))
      (substring-no-properties (if (equal nil symbol) "" symbol)))))

(defun tide-nav (arg)
  "Search and navigate to named types."
  (interactive "P")
  (let ((completion-ignore-case t)
        (last-completions nil)
        (default (when arg (tide-get-symbol-at-point))))
    (-when-let (completion
                (completing-read-default
                 "Search: "
                 (completion-table-dynamic
                  (lambda (prefix)
                    (let ((response (tide-command:navto prefix)))
                      (tide-on-response-success response nil
                        (-when-let (navto-items (plist-get response :body))
                          (setq navto-items
                                (-filter
                                 (lambda (navto-item) (member (plist-get navto-item :kind) '("class" "interface" "type" "enum")))
                                 navto-items))
                          (setq last-completions navto-items)
                          (-map (lambda (navto-item) (plist-get navto-item :name))
                                navto-items)))))
                  t) nil t default))
      (let ((navto-item (-find (lambda (navto-item) (string-equal completion (plist-get navto-item :name))) last-completions)))
        (tide-jump-to-filespan navto-item)))))

(defun tide-command:navto (type)
  (tide-send-command-sync "navto" `(:file ,buffer-file-name :searchValue ,type :maxResultCount 100)))

;;; Eldoc

(defun tide-annotate-display-part (display-part &optional highlight)
  (let ((text (plist-get display-part :text))
        (face (pcase (plist-get display-part :kind)
                ("aliasName" 'font-lock-type-face)
                ("className" 'font-lock-type-face)
                ("enumName" 'font-lock-type-face)
                ("fieldName" nil)
                ("interfaceName" 'font-lock-type-face)
                ("keyword" 'font-lock-keyword-face)
                ("lineBreak" nil)
                ("numericLiteral" nil)
                ("stringLiteral" 'font-lock-string-face)
                ("localName" 'font-lock-variable-name-face)
                ("methodName" nil)
                ("moduleName" nil)
                ("operator" nil)
                ("parameterName" (and highlight 'eldoc-highlight-function-argument))
                ("propertyName" nil)
                ("punctuation" nil)
                ("space" nil)
                ("text" nil)
                ("typeParameterName" 'font-lock-variable-name-face)
                ("enumMemberName" 'font-lock-constant-face)
                ("functionName" 'font-lock-function-name-face)
                ("regularExpressionLiteral" 'font-lock-string-face))))
    (if face
        (propertize text 'face face)
      text)))

(defun tide-annotate-display-parts (display-parts &optional highlight)
  (tide-join (-map (lambda (part) (tide-annotate-display-part part highlight)) display-parts)))

(defun tide-annotate-signature-parameter (parameter highlight)
  (tide-join
   (-map
    (lambda (part) (tide-annotate-display-part part highlight))
    (plist-get parameter :displayParts))))

(defun tide-annotate-signature (signature selected-arg-index)
  (let ((separator (tide-join (-map #'tide-annotate-display-part (plist-get signature :separatorDisplayParts)))))
    (tide-join
     (-concat
      (-map #'tide-annotate-display-part (plist-get signature :prefixDisplayParts))
      (list
       (mapconcat
        #'identity
        (-map-indexed
         (lambda (i parameter)
           (tide-annotate-signature-parameter parameter (eq i selected-arg-index)))
         (plist-get signature :parameters))
        separator))
      (-map #'tide-annotate-display-part (plist-get signature :suffixDisplayParts))))))

(defun tide-annotate-signatures (body)
  (let ((selected-index (plist-get body :selectedItemIndex))
        (selected-arg-index (plist-get body :argumentIndex)))
    (tide-annotate-signature
     (nth selected-index (plist-get body :items))
     selected-arg-index)))

(defun tide-command:signatureHelp (cb)
  (tide-send-command
   "signatureHelp"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))
   (tide-on-response-success-callback response t
     (funcall cb (tide-annotate-signatures (plist-get response :body))))))

(defun tide-method-call-p ()
  (or (looking-at "[(,]") (and (not (looking-at "\\sw")) (looking-back "[(,]\n?\\s-*"))))

(defun tide-doc-text (quickinfo-or-completion-detail)
  (or (plist-get quickinfo-or-completion-detail :displayString) ;; old
      (tide-annotate-display-parts
       (plist-get quickinfo-or-completion-detail :displayParts))))

(defun tide-doc-documentation (quickinfo-or-completion-detail)
  (let ((documentation (plist-get quickinfo-or-completion-detail :documentation)))
    (if (stringp documentation) ;; old
        documentation
      (tide-annotate-display-parts documentation))))

(defun tide-format-jsdoc (name text)
  (setq text (s-trim (or text "")))
  (concat (propertize (concat "@" name) 'face 'font-lock-keyword-face)
          (if (s-contains? "\n" text) "\n" " ")
          text
          "\n"))

(defun tide-doc-jsdoc (quickinfo-or-completion-detail)
  (tide-join
   (-map
    (lambda (tag)
      (tide-format-jsdoc (plist-get tag :name) (plist-get tag :text)))
    (plist-get quickinfo-or-completion-detail :tags))))

(defun tide-construct-documentation (quickinfo-or-completion-detail)
  (when quickinfo-or-completion-detail
    (let* ((display-string (tide-doc-text quickinfo-or-completion-detail))
           (documentation (tide-doc-documentation quickinfo-or-completion-detail))
           (jsdoc (tide-doc-jsdoc quickinfo-or-completion-detail)))
      (when (or (or (not (s-blank? documentation))
                    (not (s-blank? jsdoc)))
                tide-always-show-documentation)
        (tide-doc-buffer
         (tide-join
          (-concat (list display-string "\n\n")
                   (if (not (s-blank? documentation)) (list documentation "\n\n") '())
                   (list jsdoc))))))))

(defun tide-command:quickinfo-old (cb)
  (tide-send-command "quickinfo" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset)) cb))

(defun tide-command:quickinfo-full (cb)
  (tide-send-command "quickinfo-full" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset)) cb))

(defun tide-command:quickinfo (cb)
  (tide-fallback-if-not-supported "quickinfo-full" tide-command:quickinfo-full tide-command:quickinfo-old cb))


(defun tide-eldoc-function ()
  (when (not (member last-command '(next-error previous-error)))
    (if (tide-method-call-p)
        (tide-command:signatureHelp #'tide-eldoc-maybe-show)
      (when (looking-at "\\s_\\|\\sw")
        (tide-command:quickinfo
         (tide-on-response-success-callback response t
           (tide-eldoc-maybe-show (tide-doc-text (plist-get response :body))))))))
  nil)


;;; Copied from eldoc.el
(defun tide-eldoc-maybe-show (text)
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
         (eldoc-message text))))


(defun tide-documentation-at-point ()
  "Show documentation of the symbol at point."
  (interactive)
  (tide-command:quickinfo
   (tide-on-response-success-callback response nil
     (-if-let (buffer (tide-construct-documentation (plist-get response :body)))
         (display-buffer buffer t)
       (message "No documentation available.")))))

;;; Buffer Sync

(defun tide-remove-tmp-file ()
  (when tide-buffer-tmp-file
    (delete-file tide-buffer-tmp-file)
    (setq tide-buffer-tmp-file nil)))

(defun tide-command:reloadfile ()
  (tide-send-command "reload" `(:file ,buffer-file-name :tmpfile ,buffer-file-name)))

(defun tide-handle-change (_beg _end _len)
  (setq tide-buffer-dirty t))

(defun tide-sync-buffer-contents ()
  ;; The real file that backs a buffer could be changed in various
  ;; ways, one common example is the rename operation. Ensure that we
  ;; send the open command for the new file before using it as an
  ;; argument for any other command.
  (unless (string-equal tide-active-buffer-file-name buffer-file-name)
    (tide-configure-buffer))
  (when tide-buffer-dirty
    (setq tide-buffer-dirty nil)
    (when (not tide-buffer-tmp-file)
      (setq tide-buffer-tmp-file (make-temp-file "tide")))
    (write-region (point-min) (point-max) tide-buffer-tmp-file nil 'no-message)
    (tide-send-command "reload" `(:file ,buffer-file-name :tmpfile ,tide-buffer-tmp-file))))

;;; Code-fixes

(defun tide-apply-code-edits (file-code-edits)
  (save-excursion
    (dolist (file-code-edit file-code-edits)
      (with-current-buffer (find-file-noselect (plist-get file-code-edit :fileName))
        (tide-format-regions (tide-apply-edits (plist-get file-code-edit :textChanges)))
        (basic-save-buffer)
        (run-hooks 'tide-post-code-edit-hook)))))

(defun tide-get-flycheck-errors-ids-at-point ()
  (-map #'flycheck-error-id (flycheck-overlay-errors-at (point))))

(defun tide-command:getCodeFixes ()
  (tide-send-command-sync "getCodeFixes" `(:file ,(buffer-file-name) :startLine ,(tide-line-number-at-pos) :startOffset ,(tide-current-offset) :endLine ,(tide-line-number-at-pos) :endOffset ,(+ 1 (tide-current-offset)) :errorCodes ,(tide-get-flycheck-errors-ids-at-point))))

(defun tide-get-fix-description (fix)
  (plist-get fix :description))

(defun tide-apply-codefix (fix)
  "Apply a single `FIX', which may apply to several files."
  (tide-apply-code-edits (plist-get fix :changes)))


(defun tide-fix ()
  "Apply code fix for the error at point."
  (interactive)
  (unless (tide-get-flycheck-errors-ids-at-point)
    (error "No errors available at current point."))
  (let ((response (tide-command:getCodeFixes)))
    (tide-on-response-success response nil
      (let ((fixes (plist-get response :body)))
        (cond ((= 0 (length fixes)) (message "No code-fixes available."))
              ((= 1 (length fixes)) (tide-apply-codefix (car fixes)))
              (t (tide-apply-codefix
                  (tide-select-item-from-list "Select fix: " fixes #'tide-get-fix-description (tide-can-use-popup-p 'code-fix)))))))))

;;; Refactor

(defun tide-location-or-range ()
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        `(:startLine ,(tide-line-number-at-pos start) :startOffset ,(tide-offset start)
          :endLine ,(tide-line-number-at-pos end) :endOffset ,(tide-offset end)))
    `(:line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-command:getEditsForRefactor (refactor action)
  (tide-send-command-sync
   "getEditsForRefactor"
   (append `(:refactor ,refactor :action ,action :file ,buffer-file-name)
           (tide-location-or-range))))

(defun tide-command:getApplicableRefactors ()
  (tide-send-command-sync
   "getApplicableRefactors"
   (append `(:file ,buffer-file-name) (tide-location-or-range))))

(defun tide-get-refactor-description (refactor)
  (plist-get refactor :description))

(defun tide-select-refactor (applicable-refactor-infos)
  (let ((available-refactors
         (-mapcat
          (lambda (applicable-refactor-info)
            (-map (lambda (refactor-action-info)
                    `(:action ,(plist-get refactor-action-info :name)
                              :refactor ,(plist-get applicable-refactor-info :name)
                              :inlineable ,(plist-get applicable-refactor-info :inlineable)
                              :description ,(plist-get refactor-action-info :description)))
                  (plist-get applicable-refactor-info :actions)))
          applicable-refactor-infos)))
    (tide-select-item-from-list "Select refactor: " available-refactors #'tide-get-refactor-description (tide-can-use-popup-p 'refactor))))

(defun tide-apply-refactor (selected)
  (let ((response (tide-command:getEditsForRefactor (plist-get selected :refactor) (plist-get selected :action))))
    (tide-on-response-success response nil
      (deactivate-mark)
      (tide-apply-code-edits (tide-plist-get response :body :edits))
      (-when-let (rename-location (tide-plist-get response :body :renameLocation))
        (with-current-buffer (find-file-noselect (tide-plist-get response :body :renameFilename))
          (tide-move-to-location rename-location)
          (when (tide-can-rename-symbol-p)
            (tide-rename-symbol)))))))

(defun tide-refactor ()
  "Refactor code at point or current region"
  (interactive)
  (let ((response (tide-command:getApplicableRefactors)))
    (cond ((tide-command-unknown-p response) (tide-tsserver-feature-not-supported "2.4"))
          ((and (tide-response-success-p response) (plist-get response :body)) (tide-apply-refactor
                                                                                (tide-select-refactor (plist-get response :body))))
          (t (message "No refactors available.")))))

;;; Auto completion

(defun tide-completion-annotation (name)
  (-if-let (meta (and tide-completion-detailed (tide-completion-meta name)))
      ;; Get everything before the first newline, if any, because company-mode
      ;; wants single-line annotations.
      (car (split-string meta "\n"))
    (pcase (plist-get (get-text-property 0 'completion name) :kind)
      ("keyword" " k")
      ("module" " M")
      ("class" " C")
      ("interface" " I")
      ("type" " T")
      ("enum" " E")
      ("var" " v")
      ("local var" " v")
      ("function" " ƒ")
      ("local function" " ƒ")
      ("method" " m")
      ("getter" " m")
      ("setter" " m")
      ("property" " p")
      ("constructor" " c")
      ("call" " m")
      ("index" " i")
      ("construct" " m")
      ("parameter" " p")
      ("type parameter" " T")
      ("primitive type" " T")
      ("label" " l")
      ("alias" " A")
      ("const" " c")
      ("let" " l")
      (_ nil))))

(defun tide-completion-rank (completion)
  "Get the sorting order of a COMPLETION candidate."
  (or
   (-elem-index
    (plist-get completion :kind)
    '("parameter"
      "local function"
      "local var"
      "let"
      "var"
      "const"
      "function"
      "class"
      "method"
      "getter"
      "setter"
      ))
   100))

(defun tide-compare-completions (completion-a completion-b)
  "Compare COMPLETION-A and COMPLETION-B based on their kind."
  (let ((modifier-a (plist-get completion-a :kindModifiers))
        (modifier-b (plist-get completion-b :kindModifiers)))
    (if (string-equal modifier-a modifier-b)
        (< (tide-completion-rank completion-a) (tide-completion-rank completion-b))
      ;; Rank declarations lower than variables
      (string-equal modifier-b "declare"))))

(defun tide-completion-prefix ()
  (company-grab-symbol-cons "\\." 1))

(defun tide-member-completion-p (prefix)
  (save-excursion
    (backward-char (length prefix))
    (and (> (point) (point-min))
         (equal (string (char-before (point))) "."))))

(defun tide-annotate-completions (completions prefix file-location)
  (-map
   (lambda (completion)
     (let ((name (plist-get completion :name)))
       (put-text-property 0 1 'file-location file-location name)
       (put-text-property 0 1 'completion completion name)
       name))
   (-sort
    'tide-compare-completions
    (-filter
     (lambda (completion)
       (string-prefix-p prefix (plist-get completion :name)))
     completions))))

(defun tide-command:completions (prefix cb)
  (let* ((file-location
          `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(- (tide-current-offset) (length prefix)))))
    (when (not (tide-member-completion-p prefix))
      (setq file-location (plist-put file-location :prefix prefix)))
    (tide-send-command
     "completions"
     file-location
     (lambda (response)
       (funcall
        cb
        (when (tide-response-success-p response)
          (tide-annotate-completions (plist-get response :body) prefix file-location)))))))

(defun tide-command:completionEntryDetails (name)
  (let ((arguments (-concat (get-text-property 0 'file-location name)
                            `(:entryNames (,name)))))
    (-when-let (response (tide-send-command-sync "completionEntryDetails" arguments))
      (when (tide-response-success-p response)
        response))))

(defun tide-completion-entry-details (name)
  (-if-let (detail-response (get-text-property 0 'completion-detail name))
      detail-response
    (let ((detail-response (tide-command:completionEntryDetails name)))
      (put-text-property 0 1 'completion-detail detail-response name)
      detail-response)))

(defun tide-completion-meta (name)
  (-when-let* ((response (tide-completion-entry-details name))
               (detail (car (plist-get response :body))))
    (tide-doc-text detail)))

(defun tide-completion-doc-buffer (name)
  (-when-let* ((response (tide-completion-entry-details name))
               (detail (car (plist-get response :body))))
    (tide-construct-documentation detail)))

;;;###autoload
(defun company-tide (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tide))
    (prefix (and
             (bound-and-true-p tide-mode)
             (-any-p #'derived-mode-p tide-supported-modes)
             (tide-current-server)
             (not (nth 4 (syntax-ppss)))
             (or (tide-completion-prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb)
                        (tide-command:completions arg cb))))
    (sorted t)
    (ignore-case tide-completion-ignore-case)
    (meta (tide-completion-meta arg))
    (annotation (tide-completion-annotation arg))
    (doc-buffer (tide-completion-doc-buffer arg))))

(eval-after-load 'company
  '(progn
     (cl-pushnew 'company-tide company-backends)))

;;; References

(defun tide-next-reference-function (n &optional reset)
  "Override for `next-error-function' for use in tide-reference-mode buffers."
  (interactive "p")

  (-when-let (buffer (get-buffer "*tide-references*"))
    (with-current-buffer buffer
      (when reset
        (goto-char (point-min)))
      (if (> n 0)
          (tide-find-next-reference (point) n)
        (tide-find-previous-reference (point) (- n)))
      (tide-goto-reference))))

(defun tide-find-next-reference (pos arg)
  "Move to next reference."
  (interactive "d\np")
  (setq arg (* 2 arg))
  (unless (get-text-property pos 'tide-reference)
    (setq arg (1- arg)))
  (dotimes (_i arg)
    (setq pos (next-single-property-change pos 'tide-reference))
    (unless pos
      (error "Moved past last reference")))
  (goto-char pos))

(defun tide-find-previous-reference (pos arg)
  "Move back to previous reference."
  (interactive "d\np")
  (dotimes (_i (* 2 arg))
    (setq pos (previous-single-property-change pos 'tide-reference))
    (unless pos
      (error "Moved back before first reference")))
  (goto-char pos))

(defun tide-goto-reference ()
  "Jump to reference location in the file."
  (interactive)
  (-when-let (reference (get-text-property (point) 'tide-reference))
    (tide-jump-to-filespan reference nil t)))

(defvar tide-references-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tide-find-next-reference)
    (define-key map (kbd "p") #'tide-find-previous-reference)
    (define-key map (kbd "C-m") #'tide-goto-reference)
    (define-key map [mouse-1] #'tide-goto-reference)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode tide-references-mode nil "tide-references"
  "Major mode for tide references list.

\\{tide-references-mode-map}"
  (use-local-map tide-references-mode-map)
  (setq buffer-read-only t)
  (setq next-error-function #'tide-next-reference-function))

(defun tide-command:references ()
  (tide-send-command-sync
   "references"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-annotate-line (reference line-text)
  (let ((start (1- (tide-plist-get reference :start :offset)))
        (end (1- (tide-plist-get reference :end :offset))))
    (put-text-property start end 'face 'tide-match line-text)
    (put-text-property start end 'mouse-face 'highlight line-text)
    (put-text-property start end 'help-echo "mouse-1: Visit the reference." line-text)
    (put-text-property start end 'tide-reference reference line-text)))

(defun tide-insert-references (references)
  "Create a buffer with the given REFERENCES.

Assumes references are grouped by file name and sorted by line
number."
  (let ((buffer (get-buffer-create "*tide-references*"))
        (inhibit-read-only t)
        (width tab-width)
        (project-root (tide-project-root))
        (last-file-name nil))
    (with-current-buffer buffer
      (erase-buffer)
      (tide-references-mode)
      (setq tab-width width)
      (while references
        (let* ((reference (car references))
               (full-file-name (plist-get reference :file))
               (file-name (file-relative-name full-file-name project-root))
               (line-number (tide-plist-get reference :start :line))
               (line-text (plist-get reference :lineText)))

          ;; file
          (when (not (equal last-file-name file-name))
            (setq last-file-name file-name)
            (insert (propertize file-name 'face 'tide-file))
            (insert "\n"))

          ;; line number
          (insert (propertize (format "%5d" line-number) 'face 'tide-line-number))
          (insert ":")

          ;; line text
          (tide-annotate-line reference line-text)
          (while (-when-let* ((next (cadr references))
                              (full-file-name0 (plist-get next :file))
                              (line-number0 (tide-plist-get next :start :line)))
                   (and (equal full-file-name0 full-file-name) (eq line-number0 line-number)))
            (tide-annotate-line (cadr references) line-text)
            (pop references))
          (insert line-text)

          (insert "\n"))
        (pop references))
      (goto-char (point-min))
      (current-buffer))))

(defun tide-is-identical-reference (original second)
  (and (equal (plist-get original :file) (plist-get second :file))
       (eq (tide-plist-get original :start :line) (tide-plist-get second :start :line))))
(defun tide-find-single-usage (references)
  (let ((definition nil)
        (usage nil)
        (multiple nil))
    (-each references
      #'(lambda (reference)
          (if (eq t (plist-get reference :isDefinition))
              (if (or (eq definition nil) (tide-is-identical-reference definition reference))
                  (setq definition reference)
                (setq multiple t))
            (if (or (eq usage nil) (tide-is-identical-reference usage reference))
                (setq usage reference)
              (setq multiple t)))))
    (if (and (not multiple) usage definition)
        usage
      nil)))

(defun tide-references ()
  "List all references to the symbol at point."
  (interactive)
  (let ((response (tide-command:references)))
    (tide-on-response-success response nil
      (let ((references (tide-plist-get response :body :refs)))
        (-if-let (usage (tide-find-single-usage references))
            (progn
              (message "This is the only usage.")
              (tide-jump-to-filespan usage nil t))
          (display-buffer (tide-insert-references references)))))))


;;; Imenu

(defun tide-build-flat-imenu-index (navtree &optional parent)
  (let* ((child-items (plist-get navtree :childItems))
         (text (plist-get navtree :text))
         (new-text (if parent (concat parent imenu-level-separator text) text))
         (node (cons (concat new-text " " (propertize (plist-get navtree :kind) 'face 'tide-imenu-type-face))
                     (tide-span-to-position (plist-get (car (plist-get navtree :spans)) :start)))))
    (if child-items
        (-concat (list node) (-flatten (-map (lambda (i) (tide-build-flat-imenu-index i new-text)) child-items)))
      (list node))))

(defun tide-build-imenu-index (navtree)
  (let* ((child-items (plist-get navtree :childItems))
         (text (plist-get navtree :text))
         (node (cons (concat text " " (propertize (plist-get navtree :kind) 'face 'tide-imenu-type-face))
                     (tide-span-to-position (plist-get (car (plist-get navtree :spans)) :start)))))
    (if child-items
        (cons text
              (-concat (list node)
                       (-map #'tide-build-imenu-index child-items)))
      node)))

(defun tide-command:navbar ()
  (tide-send-command-sync "navtree" `(:file ,buffer-file-name)))

(defun tide-imenu-index ()
  (let ((response (tide-command:navbar)))
    (tide-on-response-success response nil
      (let ((navtree (plist-get response :body)))
        (if tide-imenu-flatten
            (-flatten (-map #'tide-build-flat-imenu-index (plist-get navtree :childItems)))
          (list (tide-build-imenu-index navtree)))))))

;;; Rename

(defun tide-command:rename ()
  (tide-send-command-sync "rename" `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset))))

(defun tide-rename-symbol-at-location (location new-symbol)
  (let ((file (plist-get location :file)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (-each
            (-map (lambda (filespan)
                    (tide-move-to-location (plist-get filespan :start))
                    (cons (point-marker) filespan))
                  (plist-get location :locs))
          (lambda (pointer)
            (let* ((marker (car pointer))
                   (filespan (cdr pointer)))
              (goto-char marker)
              (delete-char (- (tide-plist-get filespan :end :offset) (tide-plist-get filespan :start :offset)))
              (insert new-symbol))))
        (basic-save-buffer)
        (length (plist-get location :locs))))))

(defun tide-read-new-symbol (old-symbol)
  (let ((new-symbol (read-from-minibuffer (format "Rename %s to: " old-symbol) old-symbol)))
    (if (string-match-p "\\`[ \t\n\r]*\\'" new-symbol)
        (error "Invalid name")
      new-symbol)))

(defun tide-can-rename-symbol-p ()
  (let ((response (tide-command:rename)))
    (and
     (tide-response-success-p response)
     (eq (tide-plist-get response :body :info :canRename) t))))

(defun tide-rename-symbol ()
  "Rename symbol at point."
  (interactive)
  (let ((response (tide-command:rename)))
    (tide-on-response-success response nil
      (if (eq (tide-plist-get response :body :info :canRename) :json-false)
          (message "%s" (tide-plist-get response :body :info :localizedErrorMessage))
        (let* ((old-symbol (tide-plist-get response :body :info :displayName))
               (new-symbol (tide-read-new-symbol old-symbol))
               (locs (tide-plist-get response :body :locs))
               (count 0))
          (cl-flet ((current-file-p (loc)
                                    (file-equal-p (expand-file-name buffer-file-name)
                                                  (plist-get loc :file))))

            ;; Saving current file will trigger a compilation
            ;; check. So make sure all the other files are saved
            ;; before saving current file.

            (-each (nconc (-reject #'current-file-p locs)
                          (-select #'current-file-p locs))
              (lambda (loc)
                (cl-incf count (tide-rename-symbol-at-location loc new-symbol))))

            (message "Renamed %d occurrences." count)))))))

;;; Format

;;;###autoload
(defun tide-format-before-save ()
  "Before save hook to format the buffer before each save."
  (interactive)
  (when (bound-and-true-p tide-mode)
    (tide-format)))

;;;###autoload
(defun tide-format ()
  "Format the current region or buffer."
  (interactive)
  (if (use-region-p)
      (tide-format-region (region-beginning) (region-end))
    (tide-format-region (point-min) (point-max))))

(defun tide-normalize-lineshift (str)
  "Reformat `STR' to only contain line-shift formats expected by Emacs.

When inserting text in an Emacs-buffer Emacs only ever expects \n
for newlines, no matter what the actual encoding of the file
is.  Inserting anything else causes issues with formatting and
code-analysis."

  ;; convert DOS CR+LF to LF
  (setq str (replace-regexp-in-string "\r\n" "\n" str))
  ;; convert Mac CR to LF
  (setq str (subst-char-in-string ?\r ?\n str))
  str)

(defun tide-insert (str)
  "Insert `STR' into the buffer, but normalize the line-enings."

  (insert (tide-normalize-lineshift str)))

(defun tide-apply-edit (edit)
  (goto-char (tide-location-to-point (plist-get edit :start)))
  (delete-region (point) (tide-location-to-point (plist-get edit :end)))
  (let ((start (point-marker)))
    (tide-insert (plist-get edit :newText))
    (cons start (point-marker))))

(defun tide-apply-edits (edits)
  (save-excursion
    (-map (lambda (edit) (tide-apply-edit edit))
          (nreverse edits))))

(defun tide-format-region (start end)
  (let ((response (tide-send-command-sync
                   "format"
                   `(:file ,buffer-file-name
                           :line ,(tide-line-number-at-pos start)
                           :offset ,(tide-offset start)
                           :endLine ,(tide-line-number-at-pos end)
                           :endOffset ,(tide-offset end)))))
    (tide-on-response-success response nil
      (tide-apply-edits (plist-get response :body)))))

(defun tide-format-regions (ranges)
  (let ((positions (->>
                    ranges
                    (-mapcat (lambda (range) (list (marker-position (car range)) (marker-position (cdr range)))))
                    (-sort '<))))
    (tide-format-region (-min positions) (-max positions))))

;;; Mode

(defvar tide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'tide-jump-to-definition)
    (define-key map (kbd "M-,") #'tide-jump-back)
    map))

(defun tide-configure-buffer ()
  (setq tide-active-buffer-file-name buffer-file-name)
  (tide-command:openfile)
  (tide-command:configure))

(defun tide-cleanup-buffer ()
  (ignore-errors
    (tide-command:closefile))
  (ignore-errors
    (tide-remove-tmp-file)))

;;;###autoload
(defun tide-setup ()
  "Setup `tide-mode' in current buffer."
  (interactive)

  ;; skip buffers where buffer-file-name is not defined, such as org-mode code
  ;; blocks as they are fontified for html export
  (if (stringp buffer-file-name)
      (progn
        (tide-start-server-if-required)
        (tide-mode 1)
        (set (make-local-variable 'eldoc-documentation-function)
             'tide-eldoc-function)
        (set (make-local-variable 'imenu-auto-rescan) t)
        (set (make-local-variable 'imenu-create-index-function)
             'tide-imenu-index)

        (tide-configure-buffer))
    (display-warning 'tide "A file backed buffer is required to start the tsserver.")))

;;;###autoload
(define-minor-mode tide-mode
  "Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}"
  :lighter " tide"
  :keymap tide-mode-map
  (if tide-mode
      (progn
        (add-hook 'after-save-hook 'tide-sync-buffer-contents nil t)
        (add-hook 'after-save-hook 'tide-auto-compile-file nil t)
        (add-hook 'after-change-functions 'tide-handle-change nil t)
        (add-hook 'kill-buffer-hook 'tide-cleanup-buffer nil t)
        (add-hook 'hack-local-variables-hook 'tide-configure-buffer nil t)
        (when (commandp 'typescript-insert-and-indent)
          (eldoc-add-command 'typescript-insert-and-indent)))
    (remove-hook 'after-save-hook 'tide-sync-buffer-contents)
    (remove-hook 'after-save-hook 'tide-auto-compile-file)
    (remove-hook 'after-change-functions 'tide-handle-change)
    (remove-hook 'kill-buffer-hook 'tide-cleanup-buffer)
    (remove-hook 'hack-local-variables-hook 'tide-configure-buffer)
    (tide-cleanup-buffer)))


;;; Error highlighting

(defun tide-command:geterr (cb)
  (let* ((result '())
         (resolved nil)
         (err nil))
    (cl-flet
        ((resolve ()
                  (when (not resolved)
                    (if err
                        (progn
                          (setq resolved t)
                          (funcall cb err))
                      (when (and (plist-member result :syntaxDiag)
                                 (plist-member result :semanticDiag))
                        (setq resolved t)
                        (funcall cb `(:body (,result) :success t)))))))
      (tide-send-command
       "syntacticDiagnosticsSync"
       `(:file ,buffer-file-name)
       (lambda (response)
         (if (tide-response-success-p response)
             (setq result (plist-put result :syntaxDiag (plist-get response :body)))
           (setq err response))
         (resolve)))
      (tide-send-command
       "semanticDiagnosticsSync"
       `(:file ,buffer-file-name)
       (lambda (response)
         (if (tide-response-success-p response)
             (setq result (plist-put result :semanticDiag (plist-get response :body)))
           (setq err response))
         (resolve))))))

(defun tide-parse-error (response checker)
  (-map
   (lambda (diagnostic)
     (let* ((start (plist-get diagnostic :start))
            (line (plist-get start :line))
            (column (tide-column line (plist-get start :offset))))
       (flycheck-error-new-at line column 'error (plist-get diagnostic :text)
                              :checker checker
                              :id (plist-get diagnostic :code))))
   (let ((diagnostic (car (tide-plist-get response :body))))
     (-concat (plist-get diagnostic :syntaxDiag)
              (plist-get diagnostic :semanticDiag)))))

(defun tide-flycheck-send-response (callback checker response)
  (condition-case err
      (funcall callback 'finished (tide-parse-error response checker))
    (error (funcall callback 'errored (error-message-string err)))))

(defun tide-flycheck-start (checker callback)
  (tide-command:geterr
   (lambda (response)
     (when (tide-command-unknown-p response)
       (tide-tsserver-version-not-supported))
     (if (tide-response-success-p response)
         (tide-flycheck-send-response callback checker response)
       (funcall callback 'errored (plist-get response :message))))))

(defun tide-flycheck-verify (_checker)
  (list
   (flycheck-verification-result-new
    :label "Typescript server"
    :message (if (tide-current-server) "running" "not running")
    :face (if (tide-current-server) 'success '(bold error)))
   (flycheck-verification-result-new
    :label "Tide mode"
    :message (if (bound-and-true-p tide-mode) "enabled" "disabled")
    :face (if (bound-and-true-p tide-mode) 'success '(bold warning)))))

(defun tide-flycheck-predicate ()
  (and (bound-and-true-p tide-mode) (tide-current-server) (not (file-equal-p (tide-project-root) tide-tsserver-directory))))

(flycheck-define-generic-checker 'typescript-tide
  "A TypeScript syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(typescript-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'typescript-tide)
(flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)

(flycheck-define-generic-checker 'javascript-tide
  "A Javascript syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(js-mode js2-mode js3-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'javascript-tide t)

(flycheck-define-generic-checker 'jsx-tide
  "A JSX syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(web-mode js2-jsx-mode rjsx-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'jsx-tide t)

(flycheck-define-generic-checker 'tsx-tide
  "A TSX syntax checker using tsserver."
  :start #'tide-flycheck-start
  :verify #'tide-flycheck-verify
  :modes '(web-mode)
  :predicate #'tide-flycheck-predicate)

(add-to-list 'flycheck-checkers 'tsx-tide)
(flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)

;;; Project errors

(defun tide-command:geterrForProject ()
  (tide-send-command
   "geterrForProject"
   `(:file ,buffer-file-name :delay 0)))

(defun tide-project-errors-buffer-name ()
  (format "*%s-errors*" (tide-project-name)))

(defun tide-display-erros (file-names)
  (with-current-buffer (get-buffer-create (tide-project-errors-buffer-name))
    (tide-project-errors-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (display-buffer (current-buffer) t)
    (let* ((project-files (-filter (lambda (file-name)
                                     (not (string-match-p "node_modules/typescript/" file-name)))
                                   file-names))
           (syntax-remaining-files (cl-copy-list project-files))
           (semantic-remaining-files (cl-copy-list project-files))
           (syntax-errors 0)
           (semantic-errors 0)
           (last-file-name nil))
      (tide-set-event-listener
       (lambda (response)
         (save-excursion
           (goto-char (point-max))
           (let ((inhibit-read-only t)
                 (file-name (tide-plist-get response :body :file))
                 (diagnostics (tide-plist-get response :body :diagnostics)))
             (pcase (plist-get response :event)
               ("syntaxDiag"
                (progn
                  (setq syntax-remaining-files (remove file-name syntax-remaining-files))
                  (cl-incf syntax-errors (length diagnostics))))
               ("semanticDiag"
                (progn
                  (setq semantic-remaining-files (remove file-name semantic-remaining-files))
                  (cl-incf semantic-errors (length diagnostics)))))

             (when diagnostics
               (-each diagnostics
                 (lambda (diagnostic)
                   (let ((line-number (tide-plist-get diagnostic :start :line)))
                     (when (not (equal last-file-name file-name))
                       (setq last-file-name file-name)
                       (insert (propertize (file-relative-name file-name (tide-project-root)) 'face 'tide-file))
                       (insert "\n"))

                     (insert (propertize (format "%5d" line-number) 'face 'tide-line-number 'tide-error (plist-put diagnostic :file file-name)))
                     (insert ": ")
                     (insert (plist-get diagnostic :text))
                     (insert "\n")))))
             (when (and (null syntax-remaining-files) (null semantic-remaining-files))
               (insert (format "\n%d syntax error(s), %d semantic error(s)\n" syntax-errors semantic-errors))
               (goto-char (point-min))
               (tide-clear-event-listener))))))))
  (tide-command:geterrForProject))

(defun tide-next-error-function (n &optional reset)
  "Override for `next-error-function' for use in tide-project-errors-mode buffers."
  (interactive "p")

  (-when-let (buffer (get-buffer (tide-project-errors-buffer-name)))
    (with-current-buffer buffer
      (when reset
        (goto-char (point-min)))
      (if (> n 0)
          (tide-find-next-error (point) n)
        (tide-find-previous-error (point) (- n)))
      (tide-goto-error))))

(defun tide-find-next-error (pos arg)
  "Move to next error."
  (interactive "d\np")
  (setq arg (* 2 arg))
  (unless (get-text-property pos 'tide-error)
    (setq arg (1- arg)))
  (dotimes (_i arg)
    (setq pos (next-single-property-change pos 'tide-error))
    (unless pos
      (error "Moved past last error")))
  (goto-char pos))

(defun tide-find-previous-error (pos arg)
  "Move back to previous error."
  (interactive "d\np")
  (dotimes (_i (* 2 arg))
    (setq pos (previous-single-property-change pos 'tide-error))
    (unless pos
      (error "Moved back before first error")))
  (goto-char pos))

(defun tide-goto-error ()
  "Jump to error location in the file."
  (interactive)
  (-when-let (error (get-text-property (point) 'tide-error))
    (tide-jump-to-filespan error nil t)))

(defvar tide-project-errors-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tide-find-next-error)
    (define-key map (kbd "p") #'tide-find-previous-error)
    (define-key map (kbd "C-m") #'tide-goto-error)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode tide-project-errors-mode nil "tide-project-errors"
  "Major mode for tide project-errors list.

\\{tide-project-errors-mode-map}"
  (use-local-map tide-project-errors-mode-map)
  (setq buffer-read-only t)
  (setq next-error-function #'tide-next-error-function))

;;;###autoload
(defun tide-project-errors ()
  (interactive)
  (tide-command:projectInfo
   (lambda (response)
     (tide-on-response-success response nil
       (tide-display-erros (tide-plist-get response :body :fileNames))))
   t))

;;; Identifier highlighting

(defun tide-command:documentHighlights (cb)
  (tide-send-command
   "documentHighlights"
   `(:file ,buffer-file-name :line ,(tide-line-number-at-pos) :offset ,(tide-current-offset) :filesToSearch (,buffer-file-name))
   cb))

(defface tide-hl-identifier-face
  '((t (:inherit highlight)))
  "Face used for highlighting identifiers in `tide-hl-identifier'."
  :group 'tide)

(defcustom tide-hl-identifier-idle-time 0.50
  "How long to wait after user input before highlighting the current identifier."
  :type 'float
  :group 'tide)

(tide-def-permanent-buffer-local tide--hl-last-token 0)

(defun tide--hl-new-token ()
  "Invalidate all existing tokens to get document highlights and
create a new token"
  (cl-incf tide--hl-last-token))

(defvar tide--current-hl-identifier-idle-time
  0
  "The current delay for hl-identifier-mode.")

(defvar tide--hl-identifier-timer
  nil
  "The global timer used for highlighting identifiers.")

;;;###autoload
(defun tide-unhighlight-identifiers ()
  "Remove highlights from previously highlighted identifier."
  (tide--hl-new-token)
  (remove-overlays nil nil 'tide-overlay 'sameid))

;;;###autoload
(defun tide-hl-identifier ()
  "Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier."
  (interactive)
  (tide-unhighlight-identifiers)
  (tide--hl-identifier))

(defun tide--hl-identifier ()
  "Highlight all instances of the identifier under point."
  (let ((token (tide--hl-new-token)))
    (tide-command:documentHighlights
     (lambda (response)
       (when (and
              (equal token tide--hl-last-token)
              (tide-response-success-p response))
         (tide--hl-highlight response))))))

(defun tide--hl-highlight (response)
  "Highlight all instances of the identifier under point."
  (let ((references (plist-get (car (plist-get response :body)) :highlightSpans)))
    (-each references
      (lambda (reference)
        (let* ((kind (plist-get reference :kind))
               (id-start (plist-get reference :start))
               (id-end (plist-get reference :end)))
          (when (member kind '("reference" "writtenReference"))
            (let ((x (make-overlay (tide-location-to-point id-start) (tide-location-to-point id-end))))
              (overlay-put x 'tide-overlay 'sameid)
              (overlay-put x 'face 'tide-hl-identifier-face))))))))

(defun tide--hl-identifiers-function ()
  "Function run after an idle timeout, highlighting the
identifier at point, if necessary."
  (when tide-hl-identifier-mode
    (unless (tide--on-overlay-p 'sameid)
      (tide-hl-identifier))
    (unless (eq tide--current-hl-identifier-idle-time tide-hl-identifier-idle-time)
      (tide--hl-set-timer))))

(defun tide--hl-set-timer ()
  (if tide--hl-identifier-timer
      (cancel-timer tide--hl-identifier-timer))
  (setq tide--current-hl-identifier-idle-time tide-hl-identifier-idle-time)
  (setq tide--hl-identifier-timer (run-with-idle-timer
                                   tide-hl-identifier-idle-time
                                   t
                                   #'tide--hl-identifiers-function)))

;;;###autoload
(define-minor-mode tide-hl-identifier-mode
  "Highlight instances of the identifier at point after a short
timeout."
  :group 'tide
  (if tide-hl-identifier-mode
      (progn
        (tide--hl-set-timer)
        ;; Unhighlight if point moves off identifier
        (add-hook 'post-command-hook #'tide--hl-identifiers-post-command-hook nil t)
        ;; Unhighlight any time the buffer changes
        (add-hook 'before-change-functions #'tide--hl-identifiers-before-change-function nil t))
    (remove-hook 'post-command-hook #'tide--hl-identifiers-post-command-hook t)
    (remove-hook 'before-change-functions #'tide--hl-identifiers-before-change-function t)
    (tide-unhighlight-identifiers)))

(defun tide--on-overlay-p (id)
  "Return whether point is on a tide overlay of type ID."
  (cl-find-if (lambda (el) (eq (overlay-get el 'tide-overlay) id)) (overlays-at (point))))

(defun tide--hl-identifiers-post-command-hook ()
  (if (and tide-hl-identifier-mode
           (not (tide--on-overlay-p 'sameid)))
      (tide-unhighlight-identifiers)))

(defun tide--hl-identifiers-before-change-function (_beg _end)
  (tide-unhighlight-identifiers))


;;; Compile On Save

(defun tide-command:compileOnSaveEmitFile ()
  (tide-send-command "compileOnSaveEmitFile" `(:file ,buffer-file-name)))

(defun tide-compile-file ()
  "Compiles the current file"
  (interactive)
  (tide-command:compileOnSaveEmitFile))

(defun tide-auto-compile-file ()
  "Compiles the current file if compileOnSave is set"
  (interactive)
  (tide-project-config
   (lambda (config)
     (when (and config (eq (plist-get config :compileOnSave) t))
       (tide-command:compileOnSaveEmitFile)))))

(defun tide-project-config (cb)
  (let ((config (gethash (tide-project-name) tide-project-configs :not-loaded)))
    (if (eq config :not-loaded)
        (tide-command:projectInfo
         (lambda (response)
           (tide-on-response-success response nil
             (let* ((config-file-name (tide-plist-get response :body :configFileName))
                    (config (and config-file-name (file-exists-p config-file-name) (tide-safe-json-read-file config-file-name))))
               (puthash (tide-project-name) config tide-project-configs)
               (funcall cb config)))))
      (funcall cb config))))

;;; Utility commands

(defun tide-restart-server ()
  "Restarts tsserver."
  (interactive)
  (-when-let (server (tide-current-server))
    (delete-process server))
  (tide-start-server)
  (tide-each-buffer (tide-project-name) #'tide-configure-buffer))

(provide 'tide)

;;; tide.el ends here
