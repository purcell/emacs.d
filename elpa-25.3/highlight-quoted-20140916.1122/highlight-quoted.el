;;; highlight-quoted.el --- Highlight Lisp quotes and quoted symbols  -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/highlight-quoted
;; Package-Version: 20140916.1122
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Minor mode proving highlight of Lisp quotes and quoted symbols.

;;; Code:

(defgroup highlight-quoted nil
  "Highlight Lisp quotes."
  :prefix "highlight-quoted-"
  :group 'faces)

(defface highlight-quoted-quote
  '((t :inherit font-lock-keyword-face))
  "Face to highlight Lisp quotes."
  :group 'highlight-quoted)

(defface highlight-quoted-symbol
  '((t :inherit font-lock-constant-face))
  "Face to highlight quoted Lisp symbols."
  :group 'highlight-quoted)

(defcustom highlight-quoted-highlight-symbols t
  "Non-nil iff quoted symbols should be highlighted.

When the value of this variable is changed, `highlight-quoted-mode' must be
re-enabled."
  :type 'boolean
  :group 'highlight-quoted)

(defconst highlight-quoted--quote-only-keywords
  `((,(rx (or "`" "'" "#'")) . 'highlight-quoted-quote)))

(defconst highlight-quoted--full-keywords
  `((,(rx (and (group (or "`" "'" "#'"))
               (? (* whitespace)
                  (group (+ (or (syntax word)
                                (syntax symbol)))))))
     (1 'highlight-quoted-quote)
     (2 'highlight-quoted-symbol nil t))))

(defvar highlight-quoted--buffer-keywords nil)

(defun highlight-quoted--turn-on ()
  "Set up `highlight-quoted-mode'."
  (let ((keywords (if highlight-quoted-highlight-symbols
                      highlight-quoted--full-keywords
                    highlight-quoted--quote-only-keywords)))
    (set (make-local-variable 'highlight-quoted--buffer-keywords) keywords)
    (font-lock-add-keywords nil keywords 'append)))

(defun highlight-quoted--turn-off ()
  "Tear down `highlight-quoted-mode'."
  (when highlight-quoted--buffer-keywords
    (font-lock-remove-keywords nil highlight-quoted--buffer-keywords)
    (kill-local-variable 'highlight-quoted--buffer-keywords)))

;;;###autoload
(define-minor-mode highlight-quoted-mode
  "Highlight Lisp quotes and quoted symbols.

Toggle Highlight-Quoted mode on or off.
With a prefix argument ARG, enable Highlight-Quoted mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'.
\\{highlight-quoted-mode-map}"
  :init-value nil
  :lighter ""
  :keymap nil
  (highlight-quoted--turn-off)
  (when highlight-quoted-mode
    (highlight-quoted--turn-on))
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'highlight-quoted)
;;; highlight-quoted.el ends here
