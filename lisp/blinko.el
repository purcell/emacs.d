;;; blinko.el --- Simple API client for posting to Blinko  -*- lexical-binding: t; -*-

;; Author: Marcus Lannister (ChatGPT-assisted)
;; Version: 0.1
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

;;; Customizations

(defgroup blinko nil
  "Send text and notes to Blinko."
  :group 'tools
  :prefix "blinko-")

(defcustom blinko-api-endpoint "https://xxx.yyy"
  "Blinko API endpoint for sending posts."
  :type 'string
  :group 'blinko)

(defcustom blinko-api-token "xxxyyy"
  "API token for authenticating with Blinko."
  :type 'string
  :group 'blinko)

(defcustom blinko-note-type 0
  "Note type flag sent to Blinko API, e.g. 0 for plain text."
  :type 'integer
  :group 'blinko)

;;; Core API Function

(defun blinko--request (content)
  "Internal helper to send CONTENT to Blinko using built-in URL libs."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
          ("Authorization" . ,(concat "Bearer " blinko-api-token))))
         (url-request-data
          (encode-coding-string
           (json-encode `(("content" . ,content)
                          ("type" . ,blinko-note-type)))
           'utf-8))
         (response (url-retrieve-synchronously blinko-api-endpoint t t 5)))
    (unless response
      (user-error "Blinko: no HTTP response"))
    (unwind-protect
        (with-current-buffer response
          (goto-char (point-min))
          (unless (re-search-forward "^HTTP/1\\.[01] \\([0-9]+\\)" nil t)
            (error "Blinko: invalid HTTP response"))
          (let ((status (string-to-number (match-string 1))))
            (re-search-forward "\r?\n\r?\n" nil t)
            (let ((body (string-trim (buffer-substring-no-properties (point) (point-max)))))
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
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " blinko-api-token))))
         (url-request-data (encode-coding-string
                            (json-encode `(("content" . ,content)
                                           ("type" . ,blinko-note-type)))
                            'utf-8))
         (resp (url-retrieve-synchronously blinko-api-endpoint t t 10)))
    (unless resp (user-error "Blinko: no HTTP response"))
    (with-current-buffer (get-buffer-create "*blinko-debug*")
      (erase-buffer)
      (insert (with-current-buffer resp (buffer-string)))
      ;; Avoid leaking token
      (save-excursion
        (goto-char (point-min))
        (while (search-forward blinko-api-token nil t)
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
