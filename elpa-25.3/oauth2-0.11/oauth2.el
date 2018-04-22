;;; oauth2.el --- OAuth 2.0 Authorization Protocol

;; Copyright (C) 2011-2016 Free Software Foundation, Inc

;; Author: Julien Danjou <julien@danjou.info>
;; Version: 0.11
;; Keywords: comm

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of the OAuth 2.0 draft.
;;
;; The main entry point is `oauth2-auth-and-store' which will return a token
;; structure.  This token structure can be then used with
;; `oauth2-url-retrieve-synchronously' or `oauth2-url-retrieve' to retrieve
;; any data that need OAuth authentication to be accessed.
;;
;; If the token needs to be refreshed, the code handles it automatically and
;; store the new value of the access token.

;;; Code:

(eval-when-compile (require 'cl))
(require 'plstore)
(require 'json)
(require 'url-http)

(defun oauth2-request-authorization (auth-url client-id &optional scope state redirect-uri)
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url (concat auth-url
                      (if (string-match-p "\?" auth-url) "&" "?")
                      "client_id=" (url-hexify-string client-id)
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                      (if scope (concat "&scope=" (url-hexify-string scope)) "")
                      (if state (concat "&state=" (url-hexify-string state)) "")))
  (read-string "Enter the code your browser displayed: "))

(defun oauth2-request-access-parse ()
  "Parse the result of an OAuth request."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (json-read)))

(defun oauth2-make-access-request (url data)
  "Make an access request to URL using DATA in POST."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((data (oauth2-request-access-parse)))
        (kill-buffer (current-buffer))
        data))))

(defstruct oauth2-token
  plstore
  plstore-id
  client-id
  client-secret
  access-token
  refresh-token
  token-url
  access-response)

(defun oauth2-request-access (token-url client-id client-secret code &optional redirect-uri)
  "Request OAuth access at TOKEN-URL.
The CODE should be obtained with `oauth2-request-authorization'.
Return an `oauth2-token' structure."
  (when code
    (let ((result
           (oauth2-make-access-request
            token-url
            (concat
             "client_id=" client-id
             "&client_secret=" client-secret
             "&code=" code
             "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
             "&grant_type=authorization_code"))))
      (make-oauth2-token :client-id client-id
                         :client-secret client-secret
                         :access-token (cdr (assoc 'access_token result))
                         :refresh-token (cdr (assoc 'refresh_token result))
                         :token-url token-url
                         :access-response result))))

;;;###autoload
(defun oauth2-refresh-access (token)
  "Refresh OAuth access TOKEN.
TOKEN should be obtained with `oauth2-request-access'."
  (setf (oauth2-token-access-token token)
        (cdr (assoc 'access_token
                    (oauth2-make-access-request
                     (oauth2-token-token-url token)
                     (concat "client_id=" (oauth2-token-client-id token)
                             "&client_secret=" (oauth2-token-client-secret token)
                             "&refresh_token=" (oauth2-token-refresh-token token)
                             "&grant_type=refresh_token")))))
  ;; If the token has a plstore, update it
  (let ((plstore (oauth2-token-plstore token)))
    (when plstore
      (plstore-put plstore (oauth2-token-plstore-id token)
                   nil `(:access-token
                         ,(oauth2-token-access-token token)
                         :refresh-token
                         ,(oauth2-token-refresh-token token)
                         :access-response
                         ,(oauth2-token-access-response token)
                         ))
      (plstore-save plstore)))
  token)

;;;###autoload
(defun oauth2-auth (auth-url token-url client-id client-secret &optional scope state redirect-uri)
  "Authenticate application via OAuth2."
  (oauth2-request-access
   token-url
   client-id
   client-secret
   (oauth2-request-authorization
    auth-url client-id scope state redirect-uri)
   redirect-uri))

(defcustom oauth2-token-file (concat user-emacs-directory "oauth2.plstore")
  "File path where store OAuth tokens."
  :group 'oauth2
  :type 'file)

(defun oauth2-compute-id (auth-url token-url resource-url)
  "Compute an unique id based on URLs.
This allows to store the token in an unique way."
  (secure-hash 'md5 (concat auth-url token-url resource-url)))

;;;###autoload
(defun oauth2-auth-and-store (auth-url token-url resource-url client-id client-secret &optional redirect-uri)
  "Request access to a resource and store it using `plstore'."
  ;; We store a MD5 sum of all URL
  (let* ((plstore (plstore-open oauth2-token-file))
         (id (oauth2-compute-id auth-url token-url resource-url))
         (plist (cdr (plstore-get plstore id))))
    ;; Check if we found something matching this access
    (if plist
        ;; We did, return the token object
        (make-oauth2-token :plstore plstore
                           :plstore-id id
                           :client-id client-id
                           :client-secret client-secret
                           :access-token (plist-get plist :access-token)
                           :refresh-token (plist-get plist :refresh-token)
                           :token-url token-url
                           :access-response (plist-get plist :access-response))
      (let ((token (oauth2-auth auth-url token-url
                                client-id client-secret resource-url nil redirect-uri)))
        ;; Set the plstore
        (setf (oauth2-token-plstore token) plstore)
        (setf (oauth2-token-plstore-id token) id)
        (plstore-put plstore id nil `(:access-token
                                      ,(oauth2-token-access-token token)
                                      :refresh-token
                                      ,(oauth2-token-refresh-token token)
                                      :access-response
                                      ,(oauth2-token-access-response token)))
        (plstore-save plstore)
        token))))

(defun oauth2-url-append-access-token (token url)
  "Append access token to URL."
  (concat url
          (if (string-match-p "\?" url) "&" "?")
          "access_token=" (oauth2-token-access-token token)))

(defvar oauth--url-advice nil)
(defvar oauth--token-data)

(defun oauth2-authz-bearer-header (token)
  "Return 'Authoriztions: Bearer' header with TOKEN."
  (cons "Authorization" (format "Bearer %s" token)))

(defun oauth2-extra-headers (extra-headers)
  "Return EXTRA-HEADERS with 'Authorization: Bearer' added."
  (cons (oauth2-authz-bearer-header (oauth2-token-access-token (car oauth--token-data)))
        extra-headers))


;; FIXME: We should change URL so that this can be done without an advice.
(defadvice url-http-handle-authentication (around oauth-hack activate)
  (if (not oauth--url-advice)
      ad-do-it
    (let ((url-request-method url-http-method)
          (url-request-data url-http-data)
          (url-request-extra-headers
           (oauth2-extra-headers url-http-extra-headers))))
    (oauth2-refresh-access (car oauth--token-data))
    (url-retrieve-internal (cdr oauth--token-data)
               url-callback-function
               url-callback-arguments)
    ;; This is to make `url' think it's done.
    (when (boundp 'success) (setq success t)) ;For URL library in Emacs<24.4.
    (setq ad-return-value t)))                ;For URL library in Emacs≥24.4.

;;;###autoload
(defun oauth2-url-retrieve-synchronously (token url &optional request-method request-data request-extra-headers)
  "Retrieve an URL synchronously using TOKEN to access it.
TOKEN can be obtained with `oauth2-auth'."
  (let* ((oauth--token-data (cons token url)))
    (let ((oauth--url-advice t)         ;Activate our advice.
          (url-request-method request-method)
          (url-request-data request-data)
          (url-request-extra-headers
           (oauth2-extra-headers request-extra-headers)))
      (url-retrieve-synchronously url))))

;;;###autoload
(defun oauth2-url-retrieve (token url callback &optional
                                  cbargs
                                  request-method request-data request-extra-headers)
  "Retrieve an URL asynchronously using TOKEN to access it.
TOKEN can be obtained with `oauth2-auth'.  CALLBACK gets called with CBARGS
when finished.  See `url-retrieve'."
  ;; TODO add support for SILENT and INHIBIT-COOKIES.  How to handle this in `url-http-handle-authentication'.
  (let* ((oauth--token-data (cons token url)))
    (let ((oauth--url-advice t)         ;Activate our advice.
          (url-request-method request-method)
          (url-request-data request-data)
          (url-request-extra-headers
           (oauth2-extra-headers request-extra-headers)))
      (url-retrieve url callback cbargs))))

;;;; ChangeLog:

;; 2016-07-09  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: send authentication token via Authorization header
;; 
;; 2014-01-28  Rüdiger Sonderfeld  <ruediger@c-plusplus.de>
;; 
;; 	oauth2.el: Add support for async retrieve.
;; 
;; 	* packages/oauth2/oauth2.el (oauth--tokens-need-renew): Remove.
;; 	 (oauth--token-data): New variable.
;; 	 (url-http-handle-authentication): Call `url-retrieve-internal'
;; 	 directly instead of depending on `oauth--tokens-need-renew'.
;; 	 (oauth2-url-retrieve-synchronously): Call `url-retrieve' once.
;; 	 (oauth2-url-retrieve): New function.
;; 
;; 	Signed-off-by: Rüdiger Sonderfeld <ruediger@c-plusplus.de> 
;; 	Signed-off-by: Julien Danjou <julien@danjou.info>
;; 
;; 2013-07-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* oauth2.el: Only require CL at compile time and avoid flet.
;; 	(success): Don't defvar.
;; 	(oauth--url-advice, oauth--tokens-need-renew): New dynbind variables.
;; 	(url-http-handle-authentication): Add advice.
;; 	(oauth2-url-retrieve-synchronously): Use the advice instead of flet.
;; 
;; 2013-06-29  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: release 0.9, require url-http
;; 
;; 	This is needed so that the `flet' calls doesn't restore the overriden 
;; 	function to an unbound one.
;; 
;; 	Signed-off-by: Julien Danjou <julien@danjou.info>
;; 
;; 2012-08-01  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: upgrade to 0.8, add missing require on cl
;; 
;; 2012-07-03  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: store access-reponse, bump versino to 0.7
;; 
;; 2012-06-25  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: add redirect-uri parameter, update to 0.6
;; 
;; 2012-05-29  Julien Danjou  <julien@danjou.info>
;; 
;; 	* packages/oauth2/oauth2.el: Revert fix URL double escaping, update to
;; 	0.5
;; 
;; 2012-05-04  Julien Danjou  <julien@danjou.info>
;; 
;; 	* packages/oauth2/oauth2.el: Don't use aget, update to 0.4
;; 
;; 2012-04-19  Julien Danjou  <julien@danjou.info>
;; 
;; 	* packages/oauth2/oauth2.el: Fix URL double escaping, update to 0.3
;; 
;; 2011-12-20  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: update version 0.2
;; 
;; 	* oauth2: update version to 0.2
;; 
;; 2011-12-20  Julien Danjou  <julien@danjou.info>
;; 
;; 	oauth2: allow to use any HTTP request type
;; 
;; 	* oauth2: allow to use any HTTP request type
;; 
;; 2011-10-08  Julien Danjou  <julien@danjou.info>
;; 
;; 	* oauth2.el: Require json.
;; 	 Fix compilation warning with success variable from url.el.
;; 
;; 2011-09-26  Julien Danjou  <julien@danjou.info>
;; 
;; 	* packages/oauth2/oauth2.el (oauth2-request-authorization): Add missing
;; 	 calls to url-hexify-string.
;; 
;; 2011-09-26  Julien Danjou  <julien@danjou.info>
;; 
;; 	* packages/oauth2/oauth2.el: Reformat to avoid long lines.
;; 
;; 2011-09-23  Julien Danjou  <julien@danjou.info>
;; 
;; 	New package oauth2
;; 


(provide 'oauth2)

;;; oauth2.el ends here
