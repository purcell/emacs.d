;;; sos.el --- Emacs-SOS, StackOverflow Search for Emacs

;; Copyright (C) 2014 Rudolf Olah <omouse@gmail.com>

;; Author: Rudolf Olah
;; URL: https://github.com/omouse/emacs-sos
;; Version: 0.1
;; Created: 2012-02-15
;; By: Rudolf Olah
;; keywords: tools, search, questions

;; Emacs-SOS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Emacs-SOS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-SOS. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl)
(require 'json)
(require 'url)
(require 'url-http)
(require 'org)

(provide 'sos)

(defun sos-decode-html-entities ()
  "Decodes HTML entities in a buffer."
  (loop for entity in '(("&quot;" . "\"")
                        ("&apos;" . "'")
                        ("&#39;" . "'")
                        ("&hellip" . "...")
                        ("&amp;" . "&")
                        ("&gt;" . ">")
                        ("&lt;" . "<")
                        ("&#194;" . "Â")
                        ("&#178;" . "²"))
        do (goto-char (point-min))
        (while (search-forward (car entity) nil t)
          (replace-match (cdr entity) nil t))))

(defun sos-uncompress-callback (&optional status)
  "Callback for url-retrieve that decompresses gzipped content in
the HTTP response. Code taken from
http://stackoverflow.com/a/4124056/9903

Modified for use with url-retrieve-synchronously by making the
`status' argument optional.

Returns the buffer of the uncompressed gzipped content."
  (let ((filename (make-temp-file "download" nil ".gz")))
    (search-forward "\n\n") ; Skip response headers.
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (find-file filename))
    (current-buffer)))

(defun sos-get-response-body (buffer)
  "Extract HTTP response body from HTTP response, parse it as JSON, and return the JSON object. `buffer' may be a buffer or the name of an existing buffer.

Modified based on fogbugz-mode, renamed from
`fogbugz-get-response-body':
https://github.com/omouse/fogbugz-mode"
  (set-buffer buffer)
  (switch-to-buffer buffer)
  (let* ((uncompressed-buffer (sos-uncompress-callback))
         (json-response (json-read)))
    (kill-buffer uncompressed-buffer)
    json-response))

(defun sos-search (query)
  "Searches StackOverflow for the given `query'. Displays excerpts from the search results.

API Reference: http://api.stackexchange.com/docs/excerpt-search"
  (let* ((api-url (concat "http://api.stackexchange.com/2.2/search/excerpts"
                          "?order=desc"
                          "&sort=activity"
                          "&q=" (url-hexify-string query)
                          "&site=stackoverflow"))
         (response-buffer (url-retrieve-synchronously api-url))
         (json-response (sos-get-response-body response-buffer)))
    ;; set up the buffer
    (switch-to-buffer (concat "*sos - " query "*"))
    (erase-buffer)
    (org-mode)
    (visual-line-mode t)
    (insert "#+TITLE: StackOverflow Search: " query "\n")

    ;; display the search results
    (loop for item across (cdr (assoc 'items json-response))
          do (insert (format "* %s: %s [[http://stackoverflow.com/q/%d][link]] :@%s:\n"
                             (upcase (subseq (cdr (assoc 'item_type item)) 0 1))
                             (cdr (assoc 'title item))
                             (cdr (assoc 'question_id item))
                             (reduce (lambda (x y) (format "%s:@%s" x y))
                                     (cdr (assoc 'tags item))))
                     (cdr (assoc 'excerpt item))
                     "\n\n"))

    (sos-decode-html-entities)
    ;; strip out HTML tags
    (goto-char (point-min))
    (while (search-forward "<span class=\"highlight\">" nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (search-forward "</span>" nil t)
      (replace-match "" nil t))

    (goto-char (point-min))
    (org-global-cycle 1)))

(defun sos (query)
  (interactive "sSearch StackOverflow: ")
  (sos-search query))
