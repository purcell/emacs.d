;;; byte-code-cache.el --- Compile files as they're used

;; $Id: byte-code-cache.el 17 2007-11-16 12:13:42Z quotemstr $
;; This file is in the public domain.

;;; Commentary:

;; Description:
;; Automatically cache byte-compiled versions of .el files as they're
;; loaded, automatically recompiling them as necessary.

;; Install:
;; Load this file as early as possible in ~/.emacs (after setting
;; variables, and after custom, if you used custom to set them.)
;; Anything loaded after it will be automatically byte-compiled.

;; This file cannot byte-compile itself, so you should do that
;; separately.

;; Author: Daniel Colascione <dan.colascione@gmail.com>

;;; Change Log:
;; Fri Nov 16 00:33:28 EST 2007 - Made more robust against
;; pathological recursive invocations.

;;; Code:

(require 'advice) ; for documentation magic

(defgroup byte-code-cache nil
  "Instead of separately byte-compiling everything, this package
intercepts LOADs and byte-compiles files on the fly."
  :group 'internal)

(defcustom bcc-cache-directory "~/.emacs.d/byte-cache"
  "The directory in which we store cached byte-compiled files"
  :type 'directory
  :group 'byte-code-cache)

(defcustom bcc-enabled t
  "Whether to use the byte-code cache when loading"
  :type 'boolean
  :group 'byte-code-cache)

(defcustom bcc-blacklist '("/\\.recentf$" "/history$")
  "List of regular expressions matching files that should
not be cached. Files that are modified every time Emacs
is run are good candidates for this list."
  :type '(repeat regexp)
  :group 'byte-code-cache)

(unless (boundp 'load-source-file-function)
  (error "byte-code-cache requires LOAD-SOURCE-FILE-FUNCTION"))

(defvar bcc-old-load-source-file-function
  load-source-file-function
  "Saved LOAD-SOURCE-FILE-FUNCTION")

(defvar bcc-regenerate-toplevel t
  "t unless we're inside BCC-REGENERATE-CACHE")

(defvar bcc-loaded-fake-cache-entry nil
  "Internal. Fake cache entries set this to t to indicate that
BCC-LOAD-SOURCE-FILE should load the original file.")

(defvar bcc-loaded nil
  "List of files loaded with bcc-load-source-file. List of conses.
car is origname, cdr is cachename.")

(defconst bcc-compiled-doc-string 4
  "From lisp.h")

(defmacro bcc-assert (expr)
  "Like ASSERT, but doesn't depend on CL"
  `(or ,expr (signal 'bcc-assert-failed (list ',expr))))

(defun bcc-delete-file-noerror (filename)
  "Delete file FILENAME. No error if it doesn't exist."
  (condition-case nil
      (delete-file filename)
    (error nil)))

(defun bcc-unconditionally-kill-buffer (buffer)
  "Kill buffer without asking the user or running any hooks"

  (when (buffer-modified-p buffer)
    (with-current-buffer buffer
      (restore-buffer-modified-p nil)))

  (let (kill-buffer-hook kill-buffer-query-functions)
    (kill-buffer buffer)))

;; Life is ugly without CL. We need to avoid it in order to not
;; trigger recursive-load loops. In fact, we need to avoid anything
;; autoloaded, and anything not in the Emacs core.
(defun bcc-alist-member-delete-all (alist &rest keys)
  "Delete from ALIST all elements whose car is `equal' to any element in KEYS.
Return the modified alist. Elements of ALIST that are not conses
are ignored."
  (while (and (consp (car alist))
              (member (car (car alist)) keys))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (member (car (car tail-cdr)) keys))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun bcc-cache-file-name (file-name)
  "Transform an absolute file-name into its cache directory entry.
The resulting name is always an absolute path to a file ending in
.elc"

  ;; Assumes unix here
  (concat
   (file-name-as-directory (expand-file-name bcc-cache-directory))
   (subst-char-in-string
    ?/ ?!
    (file-name-sans-extension
     (file-relative-name file-name "/")))
   ".elc"))

(defun bcc-in-blacklist (string blist)
  "Return non-NIL iff STRING matches a regexp in BLIST.
Does not save match data."
  (cond
   ((null blist) nil)
   ((string-match (car blist) string))
   (t (bcc-in-blacklist string (cdr blist)))))

(defun bcc-byte-compile-to (input output)
  "Like byte-compile-file, but puts the result in OUTPUT. Returns
the result of BYTE-COMPILE-FILE."

  ;; Require byte-compile here instead of letting the call to
  ;; byte-compile-file autoload it. We need byte-compile-dest-file to
  ;; be defined; if we define it ourselves and then let byte-compile
  ;; load, byte-compile will notice that byte-compile-dest-file is
  ;; alreay defined and not define its version.
  (require 'bytecomp)
  (let ((saved-dest-func (symbol-function #'byte-compile-dest-file)))

    (unwind-protect
        (progn
          (fset #'byte-compile-dest-file
                #'(lambda (src)
                    (if (equal src input)
                        output
                      (funcall saved-dest-func src))))

          (byte-compile-file input))

      (fset #'byte-compile-dest-file saved-dest-func))))

(defun bcc-make-fake-cache-entry (cachename origname)
  "Creates a compiled lisp file called CACHENAME that simply
loads ORIGNAME."

  ;; This function is not on the fast path.

  (let ((byte-compile-verbose nil)
        (font-lock-verbose nil)
        (byte-compile-warnings '())
        (temp-file
         (make-temp-file (expand-file-name
                          "fake-cache-"
                          (or small-temporary-file-directory
                              temporary-file-directory))
                         nil
                         ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (prin1 `(setq bcc-loaded-fake-cache-entry t)
                   (current-buffer)))

          (bcc-byte-compile-to temp-file cachename)
          (bcc-assert (file-readable-p cachename)))

      (delete-file temp-file))))

(defun bcc-regenerate-cache (input cachename nomessage)
  "Regenerate the byte-code cache for INPUT,
putting the result in OUTPUT. If INPUT cannot be compiled,
generate a fake cache entry instead. NOMESSAGE means the same
thing it does for LOAD."

  ;; This function is not on the fast path.

  (unless nomessage
    (message "Regenerating cache for %s" fullname))

  (let ((byte-compile-verbose nil)
        (font-lock-verbose nil)
        (byte-compile-warnings '())
        (kill-buffer-query-functions '())

        ;; byte-comp (for some reason) sets the mode in its input
        ;; buffer to emacs-lisp-mode. That mode's hook might load code
        ;; that needs to be compiled using this very function.
        (emacs-lisp-mode-hook '())

        ;; Make sure we don't enter byte-compile-file recursively
        (bcc-regenerate-toplevel nil))

    (let ((buf (find-buffer-visiting cachename)))
      (when buf
        (bcc-unconditionally-kill-buffer buf)))

    (bcc-delete-file-noerror cachename)
    (bcc-byte-compile-to input cachename)

    (unless (file-readable-p cachename)
      (unless nomessage
        (message "Making fake cache entry for %S" cachename))

      (bcc-make-fake-cache-entry cachename input)))

  (unless nomessage
    (message "Regenerating cache for %s...done" fullname)))

(defun bcc-load-cached-file (cachename origname noerror nomessage)
  "Load compiled file CACHENAME, but pretend we're loading
ORIGNAME. NOERROR and NOMESSAGE mean what they do for LOAD."

  ;; We need this function (and can't use LOAD) because some elisp
  ;; files depend on LOAD-FILE-NAME being in the same directory as the
  ;; uncompiled file. LOAD unconditionally sets it to cachename. Also,
  ;; we can do away with the load-history manipulation if we just load
  ;; files here directly.

  ;; Patterned on LOAD-WITH-CODE-CONVERSION. Compiled elisp files
  ;; always use the interal Emacs encoding, according to lread.c.

  ;; This function is on the fast path, and it needs to be re-entrant.
  (if (null (file-readable-p cachename))
      (and (null noerror)
	   (signal 'file-error (list "Cannot open load file" cachename)))

    (let* ((default-major-mode 'fundamental-mode)
           (default-enable-multibyte-characters nil)
           (buffer (get-buffer-create (generate-new-buffer-name " *load*")))
           (load-in-progress t)

           ;; BYTE-COMPILER-WARNINGS is sometimes unbound even though
           ;; (featurep 'bytecomp) is true. This happens when we're
           ;; loading custom files, since BYTE-COMPILER-WARNINGS is a
           ;; customization variable. Advice notices that bytecomp is
           ;; loaded and tries to compile advised functions, which
           ;; fails because we're in some strange customize-induced
           ;; twilight zone.
           (ad-default-compilation-action
            (if (and (featurep 'bytecomp)
                     (not (boundp 'byte-compiler-warnings)))
                'never
              ad-default-compilation-action)))

      (unless nomessage
        (message "Loading %S as %S..." cachename origname))

      (unwind-protect
          (let ((load-file-name origname)
                (inhibit-file-name-operation nil))

            (with-current-buffer buffer
              (let ((coding-system-for-read 'no-conversion)
                    deactivate-mark
                    buffer-undo-list)
                (insert-file-contents cachename)
                (set-buffer-multibyte nil)))

            (setq bcc-loaded (cons (cons origname cachename) bcc-loaded))
            (eval-buffer buffer nil origname nil t))

        (bcc-unconditionally-kill-buffer buffer))

      (do-after-load-evaluation origname)

      (unless nomessage
        (message "Loading %S as %S...done" cachename origname))

      t)))

(defun bcc-load-source-file (fullname file noerror nomessage)
  "Load the given file. If it's a plain elisp file, compile it
and stuff the compiled file in the cache directory. Then load
it."
  ;; This function is on the fast path. It also needs to be
  ;; re-entrant.

  (let (cachename
        hist-ent loaded-from-bcc-cache
        bcc-loaded-fake-cache-entry)

    (when (and bcc-enabled
               (not (save-match-data
                      (bcc-in-blacklist fullname bcc-blacklist))))

      (setq cachename (file-truename (bcc-cache-file-name fullname)))
      (make-directory (file-name-directory cachename) t)

      (when (and bcc-regenerate-toplevel
                 (file-newer-than-file-p fullname cachename))

        (bcc-regenerate-cache fullname cachename nomessage))

      (when (file-readable-p cachename)
        (bcc-load-cached-file cachename fullname noerror nomessage)

        (unless bcc-loaded-fake-cache-entry
          (setq loaded-from-bcc-cache t))))

    (unless loaded-from-bcc-cache
      (funcall bcc-old-load-source-file-function
               fullname file noerror nomessage))))

(defadvice documentation-property (before bcc-documentation-property-fix activate)
  "Work around Emacs bug"

  (let ((docobj (get (ad-get-arg 0) (ad-get-arg 1)))
        loadinfo)

    (when (and (numberp (cdr-safe docobj))
               (setq loadinfo (assoc (car docobj) bcc-loaded)))
      (setcar docobj (cdr loadinfo)))))

(defadvice documentation (before bcc-documentation-fix activate preactivate)
  "Work around Emacs bug"

  (let* ((fun (ad-get-arg 0))
         docobj loadinfo funcar prop)

    (if (and (symbolp fun)
             (setq prop (get fun 'function-documentation)))

        ;; Called for side-effect
        (documentation-property fun 'function-documentation (ad-get-arg 1))

      (setq fun (indirect-function fun t))
      (setq funcar (car-safe fun))

      (when (eq funcar 'macro)
        (setq fun (indirect-function (cdr fun)))
        (setq funcar (car-safe fun)))

      (cond ((memq funcar '(lambda autoload))
             (setq docobj (car (cdr (cdr fun)))))

            ((and (byte-code-function-p fun)
                  (> (length fun) bcc-compiled-doc-string))
             (setq docobj (aref fun bcc-compiled-doc-string))))

      (when (and (consp docobj)
                 (setq loadinfo (assoc (car docobj) bcc-loaded)))

        (setcar docobj (cdr loadinfo))))))

(setq load-source-file-function #'bcc-load-source-file)

(provide 'byte-code-cache)

;;; byte-code-cache.el ends here.
