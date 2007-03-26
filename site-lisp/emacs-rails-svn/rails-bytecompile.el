(require 'rails)

(mapcar
 #'byte-compile-file
 (directory-files "./" t "\\.el$"))