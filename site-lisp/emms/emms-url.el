;;; emms-url.el --- Make URL and EMMS work together well

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These routines sanify input to URL and parse data returned by URL.

;;; Code:

(require 'url)
(require 'emms-compat)

(defvar emms-url-specials-entire
  '((?\  . "%20")
    (?\n . "%0D%0A"))
  "*An alist of characters which must be represented specially in URLs.
The transformation is the key of the pair.

This is used by `emms-url-quote-entire'.")

(defun emms-url-quote-entire (url)
  "Escape specials conservatively in an entire URL.

The specials to escape are specified by the `emms-url-specials-entire'
variable.

If you want to escape parts of URLs thoroughly, then use
`emms-url-quote' instead."
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (let ((repl (assoc ch emms-url-specials-entire)))
              (if (null repl)
                  (char-to-string ch)
                (cdr repl))))
          (append url nil))))

(defun emms-url-quote (s &optional safe)
  "Replace special characters in S using the `%xx' escape.
This is useful for escaping parts of URLs, but not entire URLs.

Characters in [a-zA-Z_.-/] and SAFE(default is \"\") will never be
quoted.
e.g.,
    (emms-url-quote \"abc def\") => \"abc%20def\"."
  (if (not (stringp s))
      ""
    (or safe (setq safe ""))
    (save-match-data
      (let ((re (if (string-match "]" safe)
                    ;; `]' should be placed at the beginning inside []
                    (format "[]a-zA-Z_.-/%s]"
                            (emms-replace-regexp-in-string "]" "" safe))
                  (format "[a-zA-Z_.-/%s]" safe))))
        (mapconcat
         (lambda (c)
           (let ((s1 (char-to-string c)))
             (if (string-match re s1)
                 s1
               (format "%%%02x" c))))
         (string-to-list (encode-coding-string s 'utf-8))
         "")))))

(defun emms-url-quote-plus (s &optional safe)
  "Run (emms-url-quote s \" \"), then replace ` ' with `+'."
  (emms-replace-regexp-in-string
   " " "+" (emms-url-quote s (concat safe " "))))

(defun emms-http-content-coding ()
  (save-match-data
    (and (boundp 'url-http-content-type)
         (stringp url-http-content-type)
         (string-match ";\\s-*charset=\\([^;[:space:]]+\\)"
                       url-http-content-type)
         (intern-soft (downcase (match-string 1 url-http-content-type))))))

(defun emms-http-decode-buffer (&optional buffer)
  "Recode the buffer with `url-retrieve's contents. Else the
buffer would contain multibyte chars like \\123\\456."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((default (or (car default-process-coding-system) 'utf-8))
           (coding  (or (emms-http-content-coding) default)))
      (when coding
        ;; (pop-to-buffer (current-buffer))
        ;; (message "content-type: %s" url-http-content-type)
        ;; (message "coding: %S [default: %S]" coding default)
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) coding)))))

(provide 'emms-url)
;;; emms-url.el ends here
