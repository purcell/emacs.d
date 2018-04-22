;;; xml-rpc.el --- An elisp implementation of clientside XML-RPC

;; Copyright (C) 2002-2010 Mark A. Hershberger
;; Copyright (C) 2001 CodeFactory AB.
;; Copyright (C) 2001 Daniel Lundin.
;; Copyright (C) 2006 Shun-ichi Goto
;;   Modified for non-ASCII character handling.

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Original Author: Daniel Lundin <daniel@codefactory.se>
;; Version: 1.6.12
;; Package-Version: 20160430.1458
;; Created: May 13 2001
;; Keywords: xml rpc network
;; URL: http://github.com/hexmode/xml-rpc-el
;; Last Modified: <2016-04-30 17:56:41 mah>

(defconst xml-rpc-version "1.6.12"
  "Current version of xml-rpc.el")

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an XML-RPC client implementation in elisp, capable of both
;; synchronous and asynchronous method calls (using the url package's async
;; retrieval functionality).
;; XML-RPC is remote procedure calls over HTTP using XML to describe the
;; function call and return values.

;; xml-rpc.el represents XML-RPC datatypes as lisp values, automatically
;; converting to and from the XML datastructures as needed, both for method
;; parameters and return values, making using XML-RPC methods fairly
;; transparent to the lisp code.

;;; Installation:

;; If you use ELPA (http://tromey.com/elpa), you can install via the
;; M-x package-list-packages interface. This is preferrable as you
;; will have access to updates automatically.

;; Otherwise, just make sure this file in your load-path (usually
;; ~/.emacs.d is included) and put (require 'xml-rpc) in your
;; ~/.emacs or ~/.emacs.d/init.el file.

;;; Requirements

;; xml-rpc.el uses the url package for http handling and xml.el for
;; XML parsing. url is a part of the W3 browser package.  The url
;; package that is part of Emacs 22+ works great.
;;
;; xml.el is a part of GNU Emacs 21, but can also be downloaded from
;; here: <URL:ftp://ftp.codefactory.se/pub/people/daniel/elisp/xml.el>

;;; Bug reports

;; Please use M-x xml-rpc-submit-bug-report to report bugs.

;;; XML-RPC datatypes are represented as follows

;;          int:  42
;; float/double:  42.0
;;       string:  "foo"
;;       base64:  (list :base64 (base64-encode-string "hello" t)) '(:base64 "aGVsbG8=")
;;        array:  '(1 2 3 4)   '(1 2 3 (4.1 4.2))  [ ]  '(:array (("not" "a") ("struct" "!")))
;;       struct:  '(("name" . "daniel") ("height" . 6.1))
;;     dateTime:  '(:datetime (1234 124))


;;; Examples

;; Here follows some examples demonstrating the use of xml-rpc.el

;; Normal synchronous operation
;; ----------------------------

;; (xml-rpc-method-call "http://localhost:80/RPC" 'foo-method foo bar zoo)

;; Asynchronous example (cb-foo will be called when the methods returns)
;; ---------------------------------------------------------------------

;; (defun cb-foo (foo)
;;   (print (format "%s" foo)))

;; (xml-rpc-method-call-async 'cb-foo "http://localhost:80/RPC"
;;                            'foo-method foo bar zoo)


;; Some real world working examples for fun and play
;; -------------------------------------------------

;; Check the temperature (celsius) outside jonas@codefactory.se's apartment

;; (xml-rpc-method-call
;;      "http://flint.bengburken.net:80/xmlrpc/onewire_temp.php"
;;      'onewire.getTemp)


;; Fetch the latest NetBSD news the past 5 days from O'reillynet

;; (xml-rpc-method-call "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
;;                   'meerkat.getItems
;;                   '(("channel" . 1024)
;;                     ("search" . "/NetBSD/")
;;                     ("time_period" . "5DAY")
;;                     ("ids" . 0)
;;                     ("descriptions" . 200)
;;                     ("categories" . 0)
;;                     ("channels" . 0)
;;                     ("dates" . 0)
;;                     ("num_items" . 5)))


;;; History:

;; 1.6.12  - Add tests (thanks mdorman!), fix struct detection

;; 1.6.11  - Add a way (xml-rpc-request-headers) for clients to add extra headers.

;; 1.6.10.1 - removed extra HTTP header "Connection: close" and re-enabled keep-alive
;;            to work with long-lived connections when large data is transmitted (LTC)

;; 1.6.10  - Improve detection of structs with a patch from Jos'h Fuller.

;; 1.6.9   - Add support for the i8 type (64 bit integers)
;;         - Quote lambda with #' instead of ' to silence byte compiler

;; 1.6.8.3 - [linda] Support for explicitly passing 'base64 data types.

;; 1.6.8.2 - [linda] Fixed bug that empty values were translated into a boolean (nil)
;;           instead of an empty string "" when turning XML into an Emacs list.

;; 1.6.8.1 - [linda] Fixed bugs to be able to use empty lists and lists of lists
;;           of strings as XML parameters.
;;           (Bugs reported to web site with patches in Dec-2010.)

;; 1.6.8   - Add a report-xml-rpc-bug function
;;           Eliminate unused xml-rpc-get-temp-buffer-name
;;           Improve compatibility with Xemacs

;; 1.6.7   - Skipped version

;; 1.6.6   - Use the correct dateTime elements.  Fix bug in parsing null int.

;; 1.6.5.1 - Fix compile time warnings.

;; 1.6.5   - Made handling of dateTime elements more robust.

;; 1.6.4.1 - Updated to work with both Emacs22 and Emacs23.

;; 1.6.2.2 - Modified to allow non-ASCII string again.
;;           It can handle non-ASCII page name and comment
;;           on Emacs 21 also.

;; 1.6.2.1 - Modified to allow non-ASCII string.
;;           If xml-rpc-allow-unicode-string is non-nil,
;;           make 'value' object instead of 'base64' object.
;;           This is good for WikiRPC.

;; 1.6.2   - Fix whitespace issues to work better with new xml.el
;;           Fix bug in string handling.
;;           Add support for gzip-encoding when needed.

;; 1.6.1   - base64 support added.
;;           url-insert-entities-in-string done on string types now.

;; 1.6     - Fixed dependencies (remove w3, add cl).
;;           Move string-to-boolean and boolean-to-string into xml-rpc
;;           namespace.
;;           Fix bug in xml-rpc-xml-to-response where non-existent var was.
;;           More tweaking of "Connection: close" header.
;;           Fix bug in xml-rpc-request-process-buffer so that this works with
;;           different mixes of the url.el code.

;; 1.5.1   - Added Andrew J Cosgriff's patch to make the
;;           xml-rpc-clean-string function work in XEmacs.

;; 1.5     - Added headers to the outgoing url-retreive-synchronously
;;           so that it would close connections immediately on completion.

;; 1.4     - Added conditional debugging code.  Added version tag.

;; 1.2     - Better error handling.  The documentation didn't match
;;           the code.  That was changed so that an error was
;;           signaled.  Also, better handling of various and
;;           different combinations of xml.el and url.el.

;; 1.1     - Added support for boolean types.  If the type of a
;;           returned value is not specified, string is assumed

;; 1.0     - First version


;;; Code:

(require 'xml)
(require 'url-http)
(require 'timezone)
(eval-when-compile
  (require 'cl))

(defconst xml-rpc-maintainer-address "mah@everybody.org"
  "The address where bug reports should be sent.")

(defcustom xml-rpc-load-hook nil
  "*Hook run after loading xml-rpc."
  :type 'hook :group 'xml-rpc)

(defcustom xml-rpc-use-coding-system
  (if (coding-system-p 'utf-8) 'utf-8 'iso-8859-1)
  "The coding system to use."
  :type 'symbol :group 'xml-rpc)

(defcustom xml-rpc-allow-unicode-string (coding-system-p 'utf-8)
  "If non-nil, non-ASCII data is composed as 'value' instead of 'base64'.
And this option overrides `xml-rpc-base64-encode-unicode' and
`xml-rpc-base64-decode-unicode' if set as non-nil."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-base64-encode-unicode (coding-system-p 'utf-8)
  "If non-nil, then strings with non-ascii characters will be turned
into Base64."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-base64-decode-unicode (coding-system-p 'utf-8)
  "If non-nil, then base64 strings will be decoded using the
utf-8 coding system."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-debug 0
  "Set this to 1 or greater to avoid killing temporary buffers.
Set it higher to get some info in the *Messages* buffer"
  :type 'integer :group 'xml-rpc)

(defvar xml-rpc-fault-string nil
  "Contains the fault string if a fault is returned")

(defvar xml-rpc-fault-code nil
  "Contains the fault code if a fault is returned")

(defvar xml-rpc-request-extra-headers nil
  "A list of extra headers to send with the next request.
Should be an assoc list of headers/contents.  See `url-request-extra-headers'")

(defsubst xml-rpc-valuep (value)
  "Return t if VALUE is any sort of xml-rpc structure.

Return nil otherwise."
  (or (xml-rpc-value-intp value)
      (xml-rpc-value-doublep value)
      (xml-rpc-value-stringp value)
      (xml-rpc-value-structp value)
      (xml-rpc-value-arrayp value)
      (xml-rpc-value-vectorp value)
      (xml-rpc-value-booleanp value)
      (xml-rpc-value-datetimep value)
      (xml-rpc-value-base64p value)))

;;
;; Value type handling functions
;;

(defsubst xml-rpc-value-intp (value)
  "Return t if VALUE is an integer."
  (integerp value))

(defsubst xml-rpc-value-doublep (value)
  "Return t if VALUE is a double precision number."
  (floatp value))

(defsubst xml-rpc-value-stringp (value)
  "Return t if VALUE is a string."
  (stringp value))

;; An XML-RPC struct is a list where every car is cons or a list of
;; length 1 or 2 and has a string for car.
(defsubst xml-rpc-value-structp (value)
  "Return t if VALUE is an XML-RPC struct."
  (and (listp value)
       (let ((vals value)
             (result t)
             curval)
         (while (and vals result)
           (setq result (and
                         (setq curval (car-safe vals))
                         (consp curval)
                         (stringp (car-safe curval))
                         (xml-rpc-valuep (cdr curval))))
           (setq vals (cdr-safe vals)))
         result)))

;; A somewhat lazy predicate for arrays
(defsubst xml-rpc-value-arrayp (value)
  "Return t if VALUE is an XML-RPC array - specified by keyword :array or
a list that is not datetime, base64 or struct."
  (and (listp value)
       (or
	(eq (car value) :array)
	(and
	 (not (xml-rpc-value-datetimep value))
	 (not (xml-rpc-value-base64p value))
	 (not (xml-rpc-value-structp value))))))

(defsubst xml-rpc-value-vectorp (value)
  "Return t if VALUE is a vector - used to pass in empty lists"
  (vectorp value))

(defun xml-rpc-submit-bug-report ()
 "Submit a bug report on xml-rpc."
 (interactive)
 (require 'reporter)
 (let ((xml-rpc-tz-pd-defined-in
        (if (fboundp 'find-lisp-object-file-name)
            (find-lisp-object-file-name
             'timezone-parse-date (symbol-function 'timezone-parse-date))
          (symbol-file 'timezone-parse-date)))
       (date-parses-as (timezone-parse-date "20091130T00:52:53")))
   (reporter-submit-bug-report
    xml-rpc-maintainer-address
    (concat "xml-rpc.el " xml-rpc-version)
    (list 'xml-rpc-tz-pd-defined-in
          'date-parses-as
          'xml-rpc-load-hook
          'xml-rpc-use-coding-system
          'xml-rpc-allow-unicode-string
          'xml-rpc-base64-encode-unicode
          'xml-rpc-base64-decode-unicode))))

(defun xml-rpc-value-booleanp (value)
  "Return t if VALUE is a boolean."
  (or (eq value nil)
      (eq value t)))

(defun xml-rpc-value-datetimep (value)
  "Return t if VALUE is a datetime.  For Emacs XML-RPC
implementation, you must put time keyword :datetime before the
time, or it will be confused for a list."
  (and (listp value)
       (eq (car value) :datetime)))

(defun xml-rpc-value-base64p (value)
  "Return t if VALUE is a base64 byte array.  For Emacs XML-RPC
implementation, you must put keyword :base64 before the
sequence, or it will be confused for a list."
  (and (listp value)
       (eq (car value) :base64)))

(defun xml-rpc-string-to-boolean (value)
  "Return t if VALUE is a boolean"
  (or (string-equal value "true") (string-equal value "1")))

(defun xml-rpc-caddar-safe (list)
  "Assume that LIST is '((value nil REST)) and return REST.  If REST is nil, then return \"\""
  (let ((rest (car-safe (cdr-safe (cdr-safe (car-safe list))))))
    (if rest
	rest
      "")))

(defun xml-rpc-xml-list-to-value (xml-list)
  "Convert an XML-RPC structure in an xml.el style XML-LIST to an elisp list, \
interpreting and simplifying it while retaining its structure."
  (let (valtype valvalue)
    (cond
     ((and (xml-rpc-caddar-safe xml-list)
           (listp (xml-rpc-caddar-safe xml-list)))
      (setq valtype (car (caddar xml-list))
            valvalue (caddr (caddar xml-list)))
      (cond
       ;; Base64
       ((eq valtype 'base64)
        (list :base64 (base64-decode-string valvalue))) ; for some reason, Emacs wraps this in a second encoding
       ;; Boolean
       ((eq valtype 'boolean)
        (xml-rpc-string-to-boolean valvalue))
       ;; String
       ((eq valtype 'string)
        valvalue)
       ;; Integer
       ((or (eq valtype 'int) (eq valtype 'i4) (eq valtype 'i8))
        (string-to-number (or valvalue "0")))
       ;; Double/float
       ((eq valtype 'double)
        (string-to-number (or valvalue "0.0")))
       ;; Struct
       ((eq valtype 'struct)
        (mapcar (lambda (member)
                  (let ((membername (cadr (cdaddr member)))
                        (membervalue (xml-rpc-xml-list-to-value
                                      (cdddr member))))
                    (cons membername membervalue)))
                (cddr (caddar xml-list))))
       ;; Fault
       ((eq valtype 'fault)
        (let* ((struct (xml-rpc-xml-list-to-value (list valvalue)))
               (fault-string (cdr (assoc "faultString" struct)))
               (fault-code (cdr (assoc "faultCode" struct))))
          (list 'fault fault-code fault-string)))
       ;; DateTime
       ((or (eq valtype 'dateTime.iso8601)
            (eq valtype 'dateTime))
        (list :datetime (date-to-time valvalue)))
       ;; Array
       ((eq valtype 'array)
        (mapcar (lambda (arrval)
                  (xml-rpc-xml-list-to-value (list arrval)))
                (cddr valvalue)))))
     (t
      (xml-rpc-caddar-safe xml-list)))))

(defun xml-rpc-boolean-to-string (value)
  "Convert a boolean value to a string"
  (if value
      "1"
    "0"))

(defun xml-rpc-datetime-to-string (value)
  "Convert a date time to a valid XML-RPC date"
  (format-time-string "%Y%m%dT%H:%M:%S" (cadr value)))

(defun xml-rpc-value-to-xml-list (value)
  "Return XML representation of VALUE properly formatted for use with the  \
functions in xml.el."
  (cond
   ;; boolean
   ((xml-rpc-value-booleanp value)
    `((value nil (boolean nil ,(xml-rpc-boolean-to-string value)))))
   ;; Date
   ((xml-rpc-value-datetimep value)
    `((value nil (dateTime.iso8601 nil ,(xml-rpc-datetime-to-string value)))))
   ;; base64 (explicit)
   ((xml-rpc-value-base64p value)
    `((value nil (base64 nil ,(base64-encode-string (cadr value)))))) ; strip keyword; for some reason, Emacs decodes this twice
   ;; array as vector (for empty lists)
   ((xml-rpc-value-vectorp value)
    (let ((result nil)
          (xmlval nil))
      (dotimes (i (length value))
	(setq xmlval (xml-rpc-value-to-xml-list (elt value i))
	      result (if result (append result xmlval) xmlval)))
      `((value nil (array nil ,(append '(data nil) result))))))
   ;; array as list
   ((xml-rpc-value-arrayp value)
    (setq value (if (eq (car value) :array) (cadr value) value)) ; strip keyword if any
    (let ((result nil)
          (xmlval nil))
      (while (setq xmlval (xml-rpc-value-to-xml-list (car value))
                   result (if result (append result xmlval)
                            xmlval)
                   value (cdr value)))
      `((value nil (array nil ,(append '(data nil) result))))))
   ;; struct
   ((xml-rpc-value-structp value)
    (let ((result nil)
          (xmlval nil))
      (while (setq xmlval `((member nil (name nil ,(caar value))
                                    ,(car (xml-rpc-value-to-xml-list
                                           (cdar value)))))
                   result (append result xmlval)
                   value (cdr value)))
      `((value nil ,(append '(struct nil) result)))))
   ;; Value is a scalar
   ((xml-rpc-value-intp value)
    `((value nil (int nil ,(int-to-string value)))))
   ((xml-rpc-value-stringp value)
    (let ((charset-list (find-charset-string value)))
      (if (or xml-rpc-allow-unicode-string
              (and (eq 1 (length charset-list))
                   (eq 'ascii (car charset-list)))
              (not xml-rpc-base64-encode-unicode))
          `((value nil (string nil ,value)))
        `((value nil (string nil ,(if xml-rpc-base64-encode-unicode
                                      (base64-encode-string
                                       (encode-coding-string
                                        value xml-rpc-use-coding-system))
                                    (base64-encode-string value))))))))
   ((xml-rpc-value-doublep value)
    `((value nil (double nil ,(number-to-string value)))))
   (t
    `((value nil (string nil ,(base64-encode-string value)))))))

(defun xml-rpc-xml-to-string (xml)
  "Return a string representation of the XML tree as valid XML markup."
  (let ((tree (xml-node-children xml))
        (result (concat "<" (symbol-name (xml-node-name xml)) ">")))
    (while tree
      (cond
       ((listp (car tree))
        (setq result (concat result (xml-rpc-xml-to-string (car tree)))))
       ((stringp (car tree))
        (setq result (concat result (car tree))))
       (t
        (error "Invalid XML tree")))
      (setq tree (cdr tree)))
    (setq result (concat result "</" (symbol-name (xml-node-name xml)) ">"))
    result))

;;
;; Response handling
;;

(defsubst xml-rpc-response-errorp (response)
  "An 'xml-rpc-method-call'  result value is always a list, where the first \
element in RESPONSE is either nil or if an error occured, a cons pair \
according to (errnum .  \"Error string\"),"
  (eq 'fault (car-safe (caddar response))))

(defsubst xml-rpc-response-error-code (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 1 (xml-rpc-xml-list-to-value response))))

(defsubst xml-rpc-response-error-string (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 2 (xml-rpc-xml-list-to-value response))))

(defun xml-rpc-xml-to-response (xml)
  "Convert an XML list to a method response list.  An error is
signaled if there is a fault or if the response does not appear
to be an XML-RPC response (i.e. no methodResponse).  Otherwise,
the parsed XML response is returned."
  ;; Check if we have a methodResponse
  (cond
   ((not (eq (car-safe (car-safe xml)) 'methodResponse))
    (error "No methodResponse found"))

   ;; Did we get a fault response
   ((xml-rpc-response-errorp xml)
    (let ((resp (xml-rpc-xml-list-to-value xml)))
      (setq xml-rpc-fault-string (nth 2 resp))
      (setq xml-rpc-fault-code   (nth 1 resp))
      (error "XML-RPC fault `%s'" xml-rpc-fault-string)))

   ;; Interpret the XML list and produce a more useful data structure
   (t
    (let ((valpart (cdr (cdaddr (caddar xml)))))
      (xml-rpc-xml-list-to-value valpart)))))

;;
;; Method handling
;;

(defun xml-rpc-request (server-url xml &optional async-callback-function)
  "Perform http post request to SERVER-URL using XML.

If ASYNC-CALLBACK-FUNCTION is non-nil, the request will be performed
asynchronously and ASYNC-CALLBACK-FUNCTION should be a callback function to
be called when the reuest is finished.  ASYNC-CALLBACK-FUNCTION is called with
a single argument being an xml.el style XML list.

It returns an XML list containing the method response from the XML-RPC server,
or nil if called with ASYNC-CALLBACK-FUNCTION."
  (declare (special url-current-callback-data
                    url-current-callback-func
                    url-http-response-status))
  (unwind-protect
      (save-excursion
        (let ((url-request-method "POST")
              (url-package-name "xml-rpc.el")
              (url-package-version xml-rpc-version)
              (url-request-data (concat "<?xml version=\"1.0\""
                                        " encoding=\"UTF-8\"?>\n"
                                        (with-temp-buffer
                                          (xml-print xml)
                                          (when xml-rpc-allow-unicode-string
                                            (encode-coding-region
                                             (point-min) (point-max) 'utf-8))
                                          (buffer-string))
                                        "\n"))
              (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
              (url-request-coding-system xml-rpc-use-coding-system)
              (url-http-attempt-keepalives t)
              (url-request-extra-headers (append
                                          (list
                                           (cons "Connection" "close")
                                           (cons "Content-Type"
                                                 "text/xml; charset=utf-8"))
                                          xml-rpc-request-extra-headers)))
          (when (> xml-rpc-debug 1)
            (print url-request-data (create-file-buffer "request-data")))

          (cond ((boundp 'url-be-asynchronous) ; Sniff for w3 lib capability
                 (if async-callback-function
                     (setq url-be-asynchronous t
                           url-current-callback-data (list
                                                      async-callback-function
                                                      (current-buffer))
                           url-current-callback-func
                           'xml-rpc-request-callback-handler)
                   (setq url-be-asynchronous nil))
                 (url-retrieve server-url t)

                 (when (not url-be-asynchronous)
                   (let ((result (xml-rpc-request-process-buffer
                                  (current-buffer))))
                     (when (> xml-rpc-debug 1)
                       (print result (create-file-buffer "result-data")))
                     result)))
                (t                      ; Post emacs20 w3-el
                 (if async-callback-function
                     (let ((cbargs (list async-callback-function)))
                       (url-retrieve server-url
                                     'xml-new-rpc-request-callback-handler cbargs))
                   (let ((buffer (url-retrieve-synchronously server-url)))
                     (with-current-buffer buffer
                       (when (not (numberp url-http-response-status))
                         ;; this error may occur when keep-alive bug
                         ;; of url-http.el is not cleared.
                         (error "Why? url-http-response-status is %s"
                                url-http-response-status))
                       (when (> url-http-response-status 299)
                         (error "Error during request: %s"
                                url-http-response-status)))
                     (xml-rpc-request-process-buffer buffer)))))))))


(defun xml-rpc-clean-string (s)
  (if (string-match "\\`[ \t\n\r]*\\'" s)
      ;;"^[ \t\n]*$" s)
      nil
    s))

(defun xml-rpc-clean (l)
  (cond
   ((listp l)
    (let (elem
          (result nil))
      (while l
        ;; iterate
        (setq elem (car l)
              l (cdr l))
        ;; test the head
        (cond
         ;; a string, so clean it.
         ((stringp elem)
          (let ((tmp (xml-rpc-clean-string elem)))
            (when (and tmp xml-rpc-allow-unicode-string)
              (setq tmp (decode-coding-string tmp xml-rpc-use-coding-system)))
            (if tmp
                (setq result (append result (list tmp)))
              result)))
         ;; a list, so recurse.
         ((listp elem)
          (setq result (append result (list (xml-rpc-clean elem)))))

         ;; everthing else, as is.
         (t
          (setq result (append result (list elem))))))
      result))

   ((stringp l)                   ; will returning nil be acceptable ?
    nil)

   (t l)))

(defun xml-rpc-request-process-buffer (xml-buffer)
  "Process buffer XML-BUFFER."
  (unwind-protect
      (with-current-buffer xml-buffer
        (when (fboundp 'url-uncompress)
          (let ((url-working-buffer xml-buffer))
            (url-uncompress)))
        (goto-char (point-min))
        (search-forward-regexp "<\\?xml" nil t)
        (move-to-column 0)
        ;; Gather the results
        (let* ((status (if (boundp 'url-http-response-status)
                           ;; Old URL lib doesn't save the result.
                           url-http-response-status 200))
               (result (cond
                        ;; A probable XML response
                        ((looking-at "<\\?xml ")
                         (xml-rpc-clean (xml-parse-region (point-min)
                                                          (point-max))))

                        ;; No HTTP status returned
                        ((not status)
                         (let ((errstart
                                (search-forward "\n---- Error was: ----\n")))
                           (and errstart
                                (buffer-substring errstart (point-max)))))

                        ;; Maybe they just gave us an the XML w/o PI?
                        ((search-forward "<methodResponse>" nil t)
                         (xml-rpc-clean (xml-parse-region (match-beginning 0)
                                                          (point-max))))

                        ;; Valid HTTP status
                        (t
                         (int-to-string status)))))
          (when (< xml-rpc-debug 3)
            (kill-buffer (current-buffer)))
          result))))


(defun xml-rpc-request-callback-handler (callback-fun xml-buffer)
  "Marshall a callback function request to CALLBACK-FUN with the results \
handled from XML-BUFFER."
  (let ((xml-response (xml-rpc-request-process-buffer xml-buffer)))
    (when (< xml-rpc-debug 1)
      (kill-buffer xml-buffer))
    (funcall callback-fun (xml-rpc-xml-to-response xml-response))))


(defun xml-new-rpc-request-callback-handler (status callback-fun)
  "Handle a new style `url-retrieve' callback passing `STATUS' and `CALLBACK-FUN'."
  (let ((xml-buffer (current-buffer)))
    (xml-rpc-request-callback-handler callback-fun xml-buffer)))


(defun xml-rpc-method-call-async (async-callback-func server-url method
                                                      &rest params)
  "Call an XML-RPC method asynchronously at SERVER-URL named METHOD with \
PARAMS as parameters. When the method returns, ASYNC-CALLBACK-FUNC will be \
called with the result as parameter."
  (let* ((m-name (if (stringp method)
                     method
                   (symbol-name method)))
         (m-params (mapcar #'(lambda (p)
                               `(param nil ,(car (xml-rpc-value-to-xml-list
                                                  p))))
                           (if async-callback-func
                               params
                             (car-safe params))))
         (m-func-call `((methodCall nil (methodName nil ,m-name)
                                    ,(append '(params nil) m-params)))))
    (when (> xml-rpc-debug 1)
      (print m-func-call (create-file-buffer "func-call")))
    (xml-rpc-request server-url m-func-call async-callback-func)))

(defun xml-rpc-method-call (server-url method &rest params)
  "Call an XML-RPC method at SERVER-URL named METHOD with PARAMS as \
parameters."
  (let ((response
         (xml-rpc-method-call-async nil server-url method params)))
    (cond ((stringp response)
           (list (cons nil (concat "URL/HTTP Error: " response))))
          (t
           (xml-rpc-xml-to-response response)))))

(unless (fboundp 'xml-escape-string)
  (defun xml-debug-print (xml &optional indent-string)
    "Outputs the XML in the current buffer.
XML can be a tree or a list of nodes.
The first line is indented with the optional INDENT-STRING."
    (setq indent-string (or indent-string ""))
    (dolist (node xml)
      (xml-debug-print-internal node indent-string)))

  (defalias 'xml-print 'xml-debug-print)

  (when (not (boundp 'xml-entity-alist))
    (defvar xml-entity-alist
      '(("lt" . "<")
        ("gt" . ">")
        ("apos" . "'")
        ("quot" . "\"")
        ("amp" . "&"))))

  (defun xml-escape-string (string)
    "Return the string with entity substitutions made from
xml-entity-alist."
    (mapconcat (lambda (byte)
                 (let ((char (char-to-string byte)))
                   (if (rassoc char xml-entity-alist)
                       (concat "&" (car (rassoc char xml-entity-alist)) ";")
                     char)))
               ;; This differs from the non-unicode branch.  Just
               ;; grabbing the string works here.
               string ""))

  (defun xml-debug-print-internal (xml indent-string)
    "Outputs the XML tree in the current buffer.
The first line is indented with INDENT-STRING."
    (let ((tree xml)
          attlist)
      (insert indent-string ?< (symbol-name (xml-node-name tree)))

      ;;  output the attribute list
      (setq attlist (xml-node-attributes tree))
      (while attlist
        (insert ?\  (symbol-name (caar attlist)) "=\""
                (xml-escape-string (cdar attlist)) ?\")
        (setq attlist (cdr attlist)))

      (setq tree (xml-node-children tree))

      (if (null tree)
          (insert ?/ ?>)
        (insert ?>)

        ;;  output the children
        (dolist (node tree)
          (cond
           ((listp node)
            (insert ?\n)
            (xml-debug-print-internal node (concat indent-string "  ")))
           ((stringp node)
            (insert (xml-escape-string node)))
           (t
            (error "Invalid XML tree"))))

        (when (not (and (null (cdr tree))
                        (stringp (car tree))))
          (insert ?\n indent-string))
        (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>)))))

(let ((tdate (timezone-parse-date "20090101T010101Z")))
  (when (not (string-equal (aref tdate 0) "2009"))
    (defun timezone-parse-date (date)
      "Parse DATE and return a vector [YEAR MONTH DAY TIME TIMEZONE].
Two-digit dates are `windowed'.  Those <69 have 2000 added; otherwise 1900
is added.  Three-digit dates have 1900 added.
TIMEZONE is nil for DATEs without a zone field.

Understands the following styles:
 (1) 14 Apr 89 03:20[:12] [GMT]
 (2) Fri, 17 Mar 89 4:01[:33] [GMT]
 (3) Mon Jan 16 16:12[:37] [GMT] 1989
 (4) 6 May 1992 1641-JST (Wednesday)
 (5) 22-AUG-1993 10:59:12.82
 (6) Thu, 11 Apr 16:17:12 91 [MET]
 (7) Mon, 6  Jul 16:47:20 T 1992 [MET]
 (8) 1996-06-24 21:13:12 [GMT]
 (9) 1996-06-24 21:13-ZONE
 (10) 19960624T211312"
      ;; Get rid of any text properties.
      (and (stringp date)
           (or (text-properties-at 0 date)
               (next-property-change 0 date))
           (setq date (copy-sequence date))
           (set-text-properties 0 (length date) nil date))
      (let ((date (or date ""))
            (year nil)
            (month nil)
            (day nil)
            (time nil)
            (zone nil))                 ;This may be nil.
        (cond ((string-match
                "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
               ;; Styles: (1) and (2) with timezone and buggy timezone
               ;; This is most common in mail and news,
               ;; so it is worth trying first.
               (setq year 3 month 2 day 1 time 4 zone 5))
              ((string-match
                "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\'" date)
               ;; Styles: (1) and (2) without timezone
               (setq year 3 month 2 day 1 time 4 zone nil))
              ((string-match
                "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\'" date)
               ;; Styles: (6) and (7) without timezone
               (setq year 6 month 3 day 2 time 4 zone nil))
              ((string-match
                "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
               ;; Styles: (6) and (7) with timezone and buggy timezone
               (setq year 6 month 3 day 2 time 4 zone 7))
              ((string-match
                "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([0-9]+\\)" date)
               ;; Styles: (3) without timezone
               (setq year 4 month 1 day 2 time 3 zone nil))
              ((string-match
                "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)[ \t]+\\([0-9]+\\)" date)
               ;; Styles: (3) with timezone
               (setq year 5 month 1 day 2 time 3 zone 4))
              ((string-match
                "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
               ;; Styles: (4) with timezone
               (setq year 3 month 2 day 1 time 4 zone 5))
              ((string-match
                "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)?[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
               ;; Styles: (5) with timezone.
               (setq year 3 month 2 day 1 time 4 zone 6))
              ((string-match
                "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)?" date)
               ;; Styles: (5) without timezone.
               (setq year 3 month 2 day 1 time 4 zone nil))
              ((string-match
                "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
               ;; Styles: (8) with timezone.
               (setq year 1 month 2 day 3 time 4 zone 5))
              ((string-match
                "\\([0-9]\\{4\\}\\)-?\\([0-9]\\{0,2\\}\\)-?\\([0-9]\\{0,2\\}\\)[T \t]+\\([0-9]\\{0,2\\}:?[0-9]\\{0,2\\}:?[0-9]\\{0,2\\}\\)[ \t]*\\([-+a-zA-Z]+[0-9:]*\\)" date)
               ;; Styles: (8) with timezone with a colon in it.
               (setq year 1 month 2 day 3 time 4 zone 5))
              ((string-match
                "\\([0-9]\\{4\\}\\)-?\\([0-9]\\{0,2\\}\\)-?\\([0-9]\\{0,2\\}\\)[T \t]+\\([0-9]+:?[0-9]+:?[0-9]+\\)" date)
               ;; Styles: (8) without timezone.
               (setq year 1 month 2 day 3 time 4 zone nil)))

        (when year
          (setq year (match-string year date))
          ;; Guess ambiguous years.  Assume years < 69 don't predate the
          ;; Unix Epoch, so are 2000+.  Three-digit years are assumed to
          ;; be relative to 1900.
          (when (< (length year) 4)
            (let ((y (string-to-number year)))
              (when (< y 69)
                (setq y (+ y 100)))
              (setq year (int-to-string (+ 1900 y)))))
          (setq month
                (if (or (= (aref date (+ (match-beginning month) 2)) ?-)
                        (let ((n (string-to-number
                                  (char-to-string
                                   (aref date (+ (match-beginning month) 2))))))
                          (= (aref (number-to-string n) 0)
                             (aref date (+ (match-beginning month) 2)))))
                    ;; Handle numeric months, spanning exactly two digits.
                    (substring date
                               (match-beginning month)
                               (+ (match-beginning month) 2))
                  (let* ((string (substring date
                                            (match-beginning month)
                                            (+ (match-beginning month) 3)))
                         (monthnum
                          (cdr (assoc (upcase string) timezone-months-assoc))))
                    (when monthnum
                      (int-to-string monthnum)))))
          (setq day (match-string day date))
          (setq time (match-string time date)))
        (when zone (setq zone (match-string zone date)))
        ;; Return a vector.
        (if (and year month)
            (vector year month day time zone)
          (vector "0" "0" "0" "0" nil))))))

(provide 'xml-rpc)

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; xml-rpc.el ends here
