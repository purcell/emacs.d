;;; semantic-gcc.el --- gcc querying special code for the C parser

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-gcc.el,v 1.8 2009/02/12 02:07:19 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; GCC stores things in special places.  These functions will query
;; GCC, and set up the preprocessor and include paths.

;;; Code:

(defun semantic-gcc-query (&optional gcc-cmd)
  "Query gcc.  Return a list of configurations.
GCC-CMD is an optional command to execute instead of \"gcc\""
  ;; $ gcc -v
  ;;
  (let ((buff (get-buffer-create " *gcc-query*")))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (condition-case nil
	  (call-process (or gcc-cmd "gcc") nil buff nil "-v")
	(error ;; Some bogus directory for the first time perhaps?
	 (let ((default-directory (expand-file-name "~/")))
	   (condition-case nil
	       (call-process (or gcc-cmd "gcc") nil buff nil "-v")
	     (error ;; gcc doesn't exist???
	      nil)))))
      (prog1
	  (buffer-string)
	(kill-buffer buff)
	)))
  )

(defun semantic-gcc-fields (str)
  "Convert GCC output STR into an alist of fields."
  (let ((fields nil)
	(lines (split-string str "\n"))
	)
    (dolist (L lines)
      ;; For any line, what do we do with it?
      (cond ((string-match "Configured with\\(:\\)" L)
	     (let* ((parts (substring L (match-end 1)))
		    (opts (cedet-split-string parts " " t))
		    )
	       (dolist (O (cdr opts))
		 (let* ((data (split-string O "="))
			(sym (intern (car data)))
			(val (car (cdr data))))
		   (push (cons sym val) fields)
		   ))
	       ))
	    ((string-match "gcc version" L)
	     (let ((parts (split-string L " ")))
	       (push (cons 'version (nth 2 parts)) fields)))
	    ((string-match "Target: " L)
	     (let ((parts (split-string L " ")))
	       (push (cons 'target (nth 1 parts)) fields)))
	    ))
    fields))

(defvar semantic-gcc-setup-data nil
  "The GCC setup data.
This is setup by `semantic-gcc-setup'.
This is an alist, and should include keys of:
  'version - The version of gcc
  '--host  - The host symbol.  (Used in include directories)
  '--prefix - Where GCC was installed.
It should also include other symbols GCC was compiled with.")

;;;###autoload
(defun semantic-gcc-setup (&optional gcc-cmd)
  "Setup Semantic C parsing based on GCC output.
Optional argument GCC-CMD is an optional command to use instead of \"gcc\"."
  (interactive)
  (let* ((fields (or semantic-gcc-setup-data
		     (semantic-gcc-fields (semantic-gcc-query))))
	 (ver (cdr (assoc 'version fields)))
	 (host (or (cdr (assoc 'target fields))
		   (cdr (assoc '--host fields))))
	 (prefix (cdr (assoc '--prefix fields)))
	 (include-root (concat prefix "/include"))
	 (include-cpp (concat prefix
			      (or (cdr (assoc '--with-gxx-include-dir fields))
				  (concat "/include/c++/" ver))))
	 (include-cpp-sys (concat include-cpp "/" host))
	 (cppconfig (concat include-cpp-sys "/bits/c++config.h"))
	 )
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    ;; Now setup include paths
    (semantic-add-system-include "/usr/include" 'c-mode)
    (semantic-add-system-include "/usr/include" 'c++-mode)
    (semantic-add-system-include include-root 'c-mode)
    (semantic-add-system-include include-root 'c++-mode)
    (semantic-add-system-include include-cpp 'c-mode)
    (semantic-add-system-include include-cpp 'c++-mode)
    (semantic-add-system-include include-cpp-sys 'c-mode)
    (semantic-add-system-include include-cpp-sys 'c++-mode)
    ;; Setup the core macro header
    (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
	(add-to-list 'semantic-lex-c-preprocessor-symbol-file cppconfig)
      (setq semantic-lex-c-preprocessor-symbol-file (list cppconfig)))
    (when (featurep 'semantic-c)
      (semantic-c-reset-preprocessor-symbol-map))
    nil))

;;; TESTING
;;
;; Example output of "gcc -v"
(defvar semantic-gcc-test-strings
  '(;; My old box:
    "Reading specs from /usr/lib/gcc-lib/i386-redhat-linux/3.2.2/specs
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --host=i386-redhat-linux
Thread model: posix
gcc version 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"
    ;; Alex Ott:
    "Using built-in specs.
Target: i486-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.3.1-9ubuntu1' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-targets=all --enable-checking=release --build=i486-linux-gnu --host=i486-linux-gnu --target=i486-linux-gnu
Thread model: posix
gcc version 4.3.1 (Ubuntu 4.3.1-9ubuntu1)"
    ;; My debian box:
    "Using built-in specs.
Target: x86_64-unknown-linux-gnu
Configured with: ../../../sources/gcc/configure --prefix=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3 --with-gmp=/usr/local/gcc/gmp --with-mpfr=/usr/local/gcc/mpfr --enable-languages=c,c++,fortran --with-as=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/as --with-ld=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/ld --disable-multilib
Thread model: posix
gcc version 4.2.3"
    ;; My mac:
    "Using built-in specs.
Target: i686-apple-darwin8
Configured with: /private/var/tmp/gcc/gcc-5341.obj~1/src/configure --disable-checking -enable-werror --prefix=/usr --mandir=/share/man --enable-languages=c,objc,c++,obj-c++ --program-transform-name=/^[cg][^.-]*$/s/$/-4.0/ --with-gxx-include-dir=/include/c++/4.0.0 --with-slibdir=/usr/lib --build=powerpc-apple-darwin8 --with-arch=pentium-m --with-tune=prescott --program-prefix= --host=i686-apple-darwin8 --target=i686-apple-darwin8
Thread model: posix
gcc version 4.0.1 (Apple Computer, Inc. build 5341)"
    )
  "A bunch of sample gcc -v outputs from different machines.")

(defvar semantic-gcc-test-strings-fail
  '(;; A really old solaris box I found
    "Reading specs from /usr/local/gcc-2.95.2/lib/gcc-lib/sparc-sun-solaris2.6/2.95.2/specs
gcc version 2.95.2 19991024 (release)"
    )
  "A bunch of sample gcc -v outputs that fail to provide the info we want.")

(defun semantic-gcc-test-output-parser ()
  "Test the output parser against some collected strings."
  (interactive)
  (let ((fail nil))
    (dolist (S semantic-gcc-test-strings)
      (let* ((fields (semantic-gcc-fields S))
	     (v (cdr (assoc 'version fields)))
	     (h (or (cdr (assoc 'target fields))
		    (cdr (assoc '--host fields))))
	     (p (cdr (assoc '--prefix fields)))
	     )
	(when (not (and v h p))
	  (let ((strs (split-string S "\n")))
	    (message "Test failed on %S\nV H P:\n%S %S %S" (car strs) v h p))
	  (setq fail t))
	))
    (dolist (S semantic-gcc-test-strings-fail)
      (let* ((fields (semantic-gcc-fields S))
	     (v (cdr (assoc 'version fields)))
	     (h (or (cdr (assoc '--host fields))
		    (cdr (assoc 'target fields))))
	     (p (cdr (assoc '--prefix fields)))
	     )
	(when (and v h p)
	  (message "Negative test failed on %S" S)
	  (setq fail t))
	))
    (if (not fail) (message "Tests passed."))
    ))

(provide 'semantic-gcc)
;;; semantic-gcc.el ends here
