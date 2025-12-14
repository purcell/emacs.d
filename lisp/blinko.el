;;; blinko.el --- Simple API client for posting to Blinko  -*- lexical-binding: t; -*-

;; Author: Marcus Lannister (ChatGPT-assisted)
;; Version: 1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, notes, blinko
;; URL: https://github.com/marcuslannister/blinko.el

;;; Commentary:

;; This package provides simple helper commands to send text content
;; to a Blinko server using its REST API.
;;
;; Features:
;; - Post arbitrary content string
;; - Post selected region
;; - Post entire buffer
;; - Proper UTF-8 support for Chinese and other multibyte characters
;;
;; Configure:
;;   M-x customize-group RET blinko RET
;;
;; Main commands:
;;   blinko-post-content
;;   blinko-post-region
;;   blinko-post-buffer

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'auth-source)

;;; Customizations

(defgroup blinko nil
  "Send text and notes to Blinko."
  :group 'tools
  :prefix "blinko-")

(defcustom blinko-api-endpoint ""
  "Full Blinko API endpoint for sending posts.
If empty, it will be built from host/port found in auth-source and `blinko-api-path'."
  :type 'string
  :group 'blinko)

(defcustom blinko-api-path "/api/v1/note/upsert"
  "Path component appended to host/port when building the endpoint."
  :type 'string
  :group 'blinko)

(defcustom blinko-api-token ""
  "Fallback API token for authenticating with Blinko.
Prefer storing credentials in `~/.authinfo' and let `auth-source' load them."
  :type 'string
  :group 'blinko)

(defcustom blinko-authinfo-user "apikey"
  "Login/user for locating Blinko credentials in `auth-source'."
  :type 'string
  :group 'blinko)

(defcustom blinko-note-type 0
  "Note type flag sent to Blinko API, e.g. 0 for plain text."
  :type 'integer
  :group 'blinko)

;;; Core API Function

(defun blinko--auth-source-entry ()
  "Return first auth-source entry for Blinko or nil."
  (let* ((parsed (unless (string-empty-p blinko-api-endpoint)
                   (url-generic-parse-url blinko-api-endpoint)))
         (host (and parsed (url-host parsed)))
         (port (and parsed (url-portspec parsed))))
    (let ((entry (or (car (auth-source-search
                           :host host :port port :user blinko-authinfo-user :max 1))
                     (car (auth-source-search
                           :user blinko-authinfo-user :max 1)))))
      (when (and entry (or (not (plist-get entry :host))
                           (not (plist-get entry :port))))
        (setq entry (or (car (auth-source-search
                              :user blinko-authinfo-user
                              :require '(:host :port)
                              :max 1))
                        entry)))
      entry)))

(defun blinko--auth-source-token (entry)
  "Return token from auth-source ENTRY or nil."
  (when entry
    (let ((secret (plist-get entry :secret)))
      (if (functionp secret) (funcall secret) secret))))

(defun blinko--resolve-endpoint (entry)
  "Resolve endpoint from custom settings or auth-source ENTRY."
  (if (not (string-empty-p blinko-api-endpoint))
      blinko-api-endpoint
    (let* ((host (and entry (plist-get entry :host)))
           (port (and entry (or (plist-get entry :port)
                                (plist-get entry :service))))
           (port (cond
                  ((numberp port) (number-to-string port))
                  ((stringp port) port)
                  (t port)))
           (path (or blinko-api-path "")))
      (unless host
        (user-error "Blinko: no host found; set `blinko-api-endpoint' or add host in authinfo"))
      (unless port
        (user-error "Blinko: no port found; set `blinko-api-endpoint' or add port in authinfo"))
      (format "https://%s:%s%s" host port path))))

(defun blinko--make-unibyte-string (str)
  "Convert STR to unibyte string safely."
  (if (multibyte-string-p str)
      (string-to-unibyte str)
    str))

(defun blinko--encode-json-payload (content)
  "Encode CONTENT as JSON with proper UTF-8 handling.
Returns a unibyte string suitable for HTTP transmission."
  (let* ((json-encoding-pretty-print nil)
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-false :json-false)
         ;; Create the payload
         (payload (json-encode `(("content" . ,content)
                                 ("type" . ,blinko-note-type)))))
    ;; Encode to UTF-8 bytes and return as unibyte string
    (encode-coding-string payload 'utf-8 t)))

(defun blinko--request (content)
  "Internal helper to send CONTENT to Blinko using built-in URL libs."
  (let* ((entry (blinko--auth-source-entry))
         (token (or (blinko--auth-source-token entry)
                    (unless (string-empty-p blinko-api-token) blinko-api-token)
                    (user-error "Blinko: no token configured; set authinfo entry or `blinko-api-token'")))
         (endpoint (blinko--resolve-endpoint entry))
         (url-request-method "POST")
         ;; ALL strings must be unibyte for url.el
         (url-request-extra-headers
          (list (cons (blinko--make-unibyte-string "Content-Type")
                      (blinko--make-unibyte-string "application/json; charset=utf-8"))
                (cons (blinko--make-unibyte-string "Authorization")
                      (blinko--make-unibyte-string (concat "Bearer " token)))))
         ;; Payload must be unibyte
         (url-request-data (blinko--encode-json-payload content))
         (response nil))
    ;; Retrieve with proper coding
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'binary))
      (setq response (url-retrieve-synchronously endpoint t t 5)))
    (unless response
      (user-error "Blinko: no HTTP response"))
    (unwind-protect
        (with-current-buffer response
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (unless (re-search-forward "^HTTP/1\\.[01] \\([0-9]+\\)" nil t)
            (error "Blinko: invalid HTTP response"))
          (let ((status (string-to-number (match-string 1))))
            (re-search-forward "\r?\n\r?\n" nil t)
            (let ((body (decode-coding-string
                         (buffer-substring-no-properties (point) (point-max))
                         'utf-8 t)))
              (if (< status 300)
                  (message "Blinko: posted (HTTP %d)" status)
                (error "Blinko: HTTP %d %s" status body)))))
      (when (buffer-live-p response)
        (kill-buffer response)))))

;;; Public Commands

(defun blinko-post-content (content)
  "Send CONTENT to Blinko."
  (interactive "sContent: ")
  (blinko--request content))

(defun blinko-post-region (start end)
  "Post selected region from START to END to Blinko."
  (interactive "r")
  (if (use-region-p)
      (let ((content (buffer-substring-no-properties start end)))
        (blinko--request content))
    (message "No region selected.")))

(defun blinko-post-buffer ()
  "Post the entire current buffer to Blinko."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (blinko--request content)))

(defun blinko-post-content-debug (content)
  "Send CONTENT and dump raw HTTP exchange into *blinko-debug*."
  (interactive "sContent: ")
  (let* ((url-debug t)
         (entry (blinko--auth-source-entry))
         (token (or (blinko--auth-source-token entry)
                    (unless (string-empty-p blinko-api-token) blinko-api-token)
                    (user-error "Blinko: no token configured; set authinfo entry or `blinko-api-token'")))
         (endpoint (blinko--resolve-endpoint entry))
         (url-request-method "POST")
         (url-request-extra-headers
          (list (cons (blinko--make-unibyte-string "Content-Type")
                      (blinko--make-unibyte-string "application/json; charset=utf-8"))
                (cons (blinko--make-unibyte-string "Authorization")
                      (blinko--make-unibyte-string (concat "Bearer " token)))))
         (url-request-data (blinko--encode-json-payload content))
         (resp nil))
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'binary))
      (setq resp (url-retrieve-synchronously endpoint t t 10)))
    (unless resp (user-error "Blinko: no HTTP response"))
    (with-current-buffer (get-buffer-create "*blinko-debug*")
      (erase-buffer)
      (set-buffer-multibyte t)
      (insert (with-current-buffer resp (buffer-string)))
      ;; Avoid leaking token
      (save-excursion
        (goto-char (point-min))
        (while (search-forward token nil t)
          (replace-match "***TOKEN***" t t)))
      (display-buffer (current-buffer)))
    (when (buffer-live-p resp) (kill-buffer resp))))

;;; Keymap

(defvar blinko-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'blinko-post-content)
    (define-key map (kbd "r") #'blinko-post-region)
    (define-key map (kbd "b") #'blinko-post-buffer)
    map)
  "Keymap for Blinko commands.")

(define-prefix-command 'blinko-prefix)
(global-set-key (kbd "C-c b") blinko-keymap)

;;; Provide

(provide 'blinko)

;;; blinko.el ends here
