This is an XML-RPC client implementation in elisp, capable of both
synchronous and asynchronous method calls (using the url package's async
retrieval functionality).
XML-RPC is remote procedure calls over HTTP using XML to describe the
function call and return values.

xml-rpc.el represents XML-RPC datatypes as lisp values, automatically
converting to and from the XML datastructures as needed, both for method
parameters and return values, making using XML-RPC methods fairly
transparent to the lisp code.

Installation:

If you use ELPA (http://tromey.com/elpa), you can install via the
M-x package-list-packages interface. This is preferrable as you
will have access to updates automatically.

Otherwise, just make sure this file in your load-path (usually
~/.emacs.d is included) and put (require 'xml-rpc) in your
~/.emacs or ~/.emacs.d/init.el file.

Requirements

xml-rpc.el uses the url package for http handling and xml.el for
XML parsing. url is a part of the W3 browser package.  The url
package that is part of Emacs 22+ works great.

xml.el is a part of GNU Emacs 21, but can also be downloaded from
here: <URL:ftp://ftp.codefactory.se/pub/people/daniel/elisp/xml.el>

Bug reports

Please use M-x xml-rpc-submit-bug-report to report bugs.

XML-RPC datatypes are represented as follows

         int:  42
float/double:  42.0
      string:  "foo"
      base64:  (list :base64 (base64-encode-string "hello" t)) '(:base64 "aGVsbG8=")
       array:  '(1 2 3 4)   '(1 2 3 (4.1 4.2))  [ ]  '(:array (("not" "a") ("struct" "!")))
      struct:  '(("name" . "daniel") ("height" . 6.1))
    dateTime:  '(:datetime (1234 124))


Examples

Here follows some examples demonstrating the use of xml-rpc.el

Normal synchronous operation
----------------------------

(xml-rpc-method-call "http://localhost:80/RPC" 'foo-method foo bar zoo)

Asynchronous example (cb-foo will be called when the methods returns)
---------------------------------------------------------------------

(defun cb-foo (foo)
  (print (format "%s" foo)))

(xml-rpc-method-call-async 'cb-foo "http://localhost:80/RPC"
                           'foo-method foo bar zoo)


Some real world working examples for fun and play
-------------------------------------------------

Check the temperature (celsius) outside jonas@codefactory.se's apartment

(xml-rpc-method-call
     "http://flint.bengburken.net:80/xmlrpc/onewire_temp.php"
     'onewire.getTemp)


Fetch the latest NetBSD news the past 5 days from O'reillynet

(xml-rpc-method-call "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
                  'meerkat.getItems
                  '(("channel" . 1024)
                    ("search" . "/NetBSD/")
                    ("time_period" . "5DAY")
                    ("ids" . 0)
                    ("descriptions" . 200)
                    ("categories" . 0)
                    ("channels" . 0)
                    ("dates" . 0)
                    ("num_items" . 5)))
