;;; scratch.el --- Mode-specific scratch buffers

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 1.2
;; Package-Version: 20170614.1401
;; Keywords: convenience, tools, files

;; Copyright (c) 1999-2017 Ian Eure <ian.eure@gmail.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:
;;
;;   (autoload 'scratch "scratch" nil t)
;;

;;; History:

;; 1999 Ian Eure
;;   Initial version.

;; 2010-08-16 Ian Eure
;;   Broke out into its own file.

;; 2012-08-30 Ian Eure
;;   Dump current region into new scratch buffer

;; 2017-05-23 Ian Eure
;;   Set up *sql* buffers so they know about the inferior process they
;;   were created from.
;;   Add mappings for additional inferior modes.
;;   Fix checkdoc & package-lint issues.

;;; Code:

(defgroup scratch nil
  "Scratch buffers."
  :prefix "scratch-"
  :group 'editing)

(defcustom scratch-mode-alist
  '((erc-mode . fundamental-mode)
    (sql-interactive-mode . sql-mode)
    (shell-mode . sh-mode)
    (inferior-python-mode . python-mode)
    (cider-repl-mode . clojure-mode)
    (inferior-tcl-mode . tcl-mode)
    (inferior-octave-mode . octave-mode))
  "Alist of mappings from major modes to major modes for SCRATCH.

Some interactive modes don't lend themselves well to scratch
buffers; this alist is used to change the mode used by SCRATCH
for those buffers."

  :type '(alist :key-type symbol :value-type symbol))

(defvar scratch-history nil
  "History of scratch buffers.")

(defvar scratch-major-mode-list nil
  "List of major modes SCRATCH may use.  See `scratch-list-modes'.")

(defvar scratch-parent nil
  "The parent of this scratch buffer.")
(make-variable-buffer-local 'scratch-parent)

;;;###autoload
(defvar scratch-buffer nil
  "Non-nil if the current buffer is a scratch buffer.")
(make-variable-buffer-local 'scratch-buffer)

(defun scratch-list-modes ()
  "List known major modes."
  (or scratch-major-mode-list
      (let ((modes))
        (mapatoms
         (lambda (sym)
           (let ((name (symbol-name sym)))
             (when (and (functionp sym)
                        (not (member sym minor-mode-list))
                        (string-match "-mode$" name))
               (push (substring name 0 -5) modes)))))
        modes)))

(defun scratch-link-sql ()
  "Link a scratch buffer to a SQLi buffer.

   This sets the scratch buffer up so SQL-SEND-BUFFER etc work as
   expected."
  (let ((product (with-current-buffer scratch-parent sql-product)))
    (setq sql-product product
          sql-buffer parent)))

(defun scratch-link-buffers ()
  "Link a parent and child buffer.

   When a scratch buffer is created from a mode for an inferior
   process, and has features which rely on knowing the inferior
   process, link them."
  (cond
   ((eq (with-current-buffer scratch-parent major-mode) 'sql-interactive-mode)
    (scratch-link-sql))
   (t nil)))

;;;###autoload
(defun scratch (&optional arg)
  "Get a scratch buffer for the current mode.

   When prefix ARG is set, prompt for scratch buffer mode."
  (interactive "p")
  (let* ((tmp) (name)
         (parent (current-buffer))
         (mode (cond (current-prefix-arg
                      (intern (concat (setq name (completing-read
                                                  "Mode: " (scratch-list-modes)
                                                  nil t nil scratch-history))
                                      "-mode")))
                     ((setq tmp (assoc major-mode scratch-mode-alist))
                      (cdr tmp))
                     (t major-mode)))
         (name
          (format "*%s*"
                  (or name (replace-regexp-in-string "-mode$" ""
                                                     (symbol-name mode)))))
         (buf (get-buffer name)))
    (cond ((bufferp buf) (pop-to-buffer buf)) ; Existing scratch buffer
          (t                                  ; New scratch buffer
           (let ((contents (when (region-active-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
             (setq buf (get-buffer-create name))
             (pop-to-buffer buf)
             (let ((scratch-buffer t))
               (funcall mode))
             (when contents (save-excursion (insert contents)))
             (setq scratch-buffer t)
             (unless current-prefix-arg
               (setq scratch-parent parent)
               (scratch-link-buffers)))))))

(provide 'scratch)
;;; scratch.el ends here
