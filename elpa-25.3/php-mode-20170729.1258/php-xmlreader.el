;; XML Reader
;; More see file:///usr/share/doc/php-doc/html/book.xmlreader.html
;; http://php.net/manual/en/class.xmlreader.php

(define-skeleton php-xmlreader
  "Insert a new xmlreader object"
  ""
  > "$" (skeleton-read "Var? ") " = new XMLReader();" \n
)

(define-skeleton php-xmlreader-open
  "Insert a new xmlreader object"
  ""
  > (skeleton-read "Var? ") "->open(" (skeleton-read "File? ") ")" \n
)
