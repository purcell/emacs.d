;;; semantic-utest.el --- Tests for semantic's parsing system.

;;; Copyright (C) 2003, 2004, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-utest.el,v 1.10.2.1 2009/03/01 02:29:14 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Semantic's parsing and partial parsing system is pretty complex.
;; These unit tests attempt to emulate semantic's partial reparsing
;; and full reparsing system, and anything else I may feel the urge
;; to write a test for.

(require 'cedet-utests)
(require 'semantic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for C tests

(defvar semantic-utest-C-buffer-contents
  "/* Test file for C language for Unit Tests */

#include <stdio.h>
#include \"sutest.h\"

struct mystruct1 {
  int slot11;
  char slot12;
  float slot13;
};

int var1;

float funp1(char arg11, char arg12);

char fun2(int arg_21, int arg_22) /*1*/
{
  struct mystruct1 *ms1 = malloc(sizeof(struct mystruct1));

  char sv = calc_sv(var1);

  if (var1 == 0) {
     sv = 1;
  } else if (arg_21 == 0) {
     sv = 2;
  } else if (arg_22 == 0) {
     sv = 3;
  } else {
     sv = 4;
  }

  printf(\"SV = %d\\n\", sv);

  /* Memory Leak */
  ms1.slot1 = sv;

  return 'A' + sv;
}
"
  "Contents of a C buffer initialized by this unit test.
Be sure to change `semantic-utest-C-name-contents' when you
change this variable.")

(defvar semantic-utest-C-h-buffer-contents
  "/* Test file for C language header file for Unit Tests */

int calc_sv(int);

"
  "Contents of a C header file buffer initialized by this unit test.")


(defvar semantic-utest-C-filename "/tmp/sutest.c"
  "File to open and erase during this test for C.")

(defvar semantic-utest-C-filename-h 
  (concat (file-name-sans-extension semantic-utest-C-filename)
	  ".h")
  "Header file filename for C")


(defvar semantic-utest-C-name-contents
  '(("stdio.h" include
     (:system-flag t)
     nil (overlay 48 66 "sutest.c"))
    ("sutest.h" include nil nil (overlay 67 86 "sutest.c"))
    ("mystruct1" type
     (:members
      (("slot11" variable
	(:type "int")
	(reparse-symbol classsubparts)
	(overlay 109 120 "sutest.c"))
       ("slot12" variable
	(:type "char")
	(reparse-symbol classsubparts)
	(overlay 123 135 "sutest.c"))
       ("slot13" variable
	(:type "float")
	(reparse-symbol classsubparts)
	(overlay 138 151 "sutest.c")))
      :type "struct")
     nil (overlay 88 154 "sutest.c"))
    ("var1" variable
     (:type "int")
     nil (overlay 156 165 "sutest.c"))
    ("funp1" function
     (:prototype-flag t :arguments
		      (("arg11" variable
			(:type "char")
			(reparse-symbol arg-sub-list)
			(overlay 179 190 "sutest.c"))
		       ("arg12" variable
			(:type "char")
			(reparse-symbol arg-sub-list)
			(overlay 191 202 "sutest.c")))
		      :type "float")
     nil (overlay 167 203 "sutest.c"))
    ("fun2" function
     (:arguments
      (("arg_21" variable
	(:type "int")
	(reparse-symbol arg-sub-list)
	(overlay 215 226 "sutest.c"))
       ("arg_22" variable
	(:type "int")
	(reparse-symbol arg-sub-list)
	(overlay 227 238 "sutest.c")))
      :type "char")
     nil (overlay 205 566 "sutest.c")))
  "List of expected tag names for C.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Python tests

(defvar semantic-utest-Python-buffer-contents
"
def fun1(a,b,c):
  return a

def fun2(a,b,c): #1
  return b

"


)
;  "pyhon test case. notice that python is indentation sensitive


(defvar semantic-utest-Python-name-contents
  '(("fun1" function
     (:arguments
      (("a" variable nil
        (reparse-symbol function_parameters)
	(overlay 10 11 "tst.py"))
       ("b" variable nil
        (reparse-symbol function_parameters)
        (overlay 12 13 "tst.py"))
       ("c" variable nil
        (reparse-symbol function_parameters)
        (overlay 14 15 "tst.py"))))
     nil (overlay 1 31 "tst.py"))
    ("fun2" function
     (:arguments
      (("a" variable nil
        (reparse-symbol function_parameters)
        (overlay 41 42 "tst.py"))
       ("b" variable nil
        (reparse-symbol function_parameters)
        (overlay 43 44 "tst.py"))
       ("c" variable nil
        (reparse-symbol function_parameters)
        (overlay 45 46 "tst.py"))))
     nil (overlay 32 65 "tst.py")))
  
  "List of expected tag names for Python.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Java tests

(defvar semantic-utest-Java-buffer-contents
"
class JavaTest{
  void fun1(int a,int b){
    return a;
  }

  void fun2(int a,int b){ //1
    return b;
  }

}
"
)

(defvar semantic-utest-Java-name-contents
  '(("JavaTest" type
     (:members
      (("fun1" function
        (:arguments
         (("a" variable
           (:type "int")
           (reparse-symbol formal_parameters)
           (overlay 30 35 "JavaTest.java"))
	  ("b" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 36 41 "JavaTest.java")))
         :type "void")
        (reparse-symbol class_member_declaration)
        (overlay 20 61 "JavaTest.java"))
       ("fun2" function
	(:arguments
	 (("a" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 75 80 "JavaTest.java"))
	  ("b" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 81 86 "JavaTest.java")))
	 :type "void")
	(reparse-symbol class_member_declaration)
	(overlay 65 110 "JavaTest.java")))
      :type "class")
     nil (overlay 2 113 "JavaTest.java")))
  "List of expected tag names for Java."
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Javascript tests

(defvar semantic-utest-Javascript-buffer-contents
"
function fun1(a, b){
    return a;
  }

function fun2(a,b){ //1
    return b;
  }
"
)


(defvar semantic-utest-Javascript-name-contents
  '(("fun1" function
     (:arguments
      (("a" variable nil
	(reparse-symbol FormalParameterList)
	(overlay 15 16 "tst.js"))
       ("b" variable nil
	(reparse-symbol FormalParameterList)
	(overlay 18 19 "tst.js"))))
     nil (overlay 1 39 "tst.js"))
    ("fun2" function
     (:arguments
      (("a" variable nil
	(reparse-symbol FormalParameterList)
	(overlay 55 56 "tst.js"))
       ("b" variable nil
	(reparse-symbol FormalParameterList)
	(overlay 57 58 "tst.js"))))
     nil (overlay 41 82 "tst.js")))

  "List of expected tag names for Javascript.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Makefile tests

(defvar semantic-utest-Makefile-buffer-contents
"
t1:
\techo t1

t2:t1 #1
\techo t2


"
)


(defvar semantic-utest-Makefile-name-contents
  '(("t1" function nil nil (overlay 1 9 "Makefile"))
    ("t2" function
     (:arguments
      ("t1"))
     nil (overlay 18 28 "Makefile")))
  "List of expected tag names for Makefile.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Scheme tests

(defvar semantic-utest-Scheme-buffer-contents
  "
 (define fun1 2)

 (define fun2 3  ;1
              )
")

(defvar semantic-utest-Scheme-name-contents
  '(("fun1" variable
     (:default-value ("2"))
     nil (overlay 3 18 "tst.scm"))
    ("fun2" variable
     (:default-value ("3"))
     nil (overlay 21 55 "tst.scm")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Html tests

(defvar semantic-utest-Html-buffer-contents
  "
<html>
  <body>
    <h1>hello</h1>
  </body><!--1-->
</html>
"
  )

(defvar semantic-utest-Html-name-contents
  '(("hello" section
     (:members
      (("hello" section nil nil (overlay 21 24 "tst.html"))))
     nil (overlay 10 15 "tst.html")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for PHP tests

(defvar semantic-utest-PHP-buffer-contents
  "<?php

function fun1(){
   return \"fun1\";
}

function fun2($arg1){
   $output = \"argument to fun2: \" . $arg1;
   return $output;
}

class aClass {
   public function fun1($a, $b){
      return $a;
   }

   public function fun2($a, $b){
      return $b;
   }
}
?> "
  )

(defvar semantic-utest-PHP-name-contents
  '(("fun1" function nil
     nil (overlay 9 45 "phptest.php"))
    ("fun2" function
     (:arguments (("$arg1" variable nil (reparse-symbol formal_parameters) (overlay 61 66 "phptest.php"))))
     nil
     (overlay 47 132 "phptest.php"))
    ("aClass" type
     (:members (("fun1" function
		 (:typemodifiers ("public") :arguments
				 (("$a" variable nil (reparse-symbol formal_parameters) (overlay 174 176 "phptest.php"))
				  ("$b" variable nil (reparse-symbol formal_parameters) (overlay 178 180 "phptest.php"))))

		 nil
		 (overlay 153 204 "phptest.php"))

		("fun2" function
		 (:typemodifiers ("public") :arguments
				 (("$a" variable nil (reparse-symbol formal_parameters) (overlay 230 232 "phptest.php"))
				  ("$b" variable nil (reparse-symbol formal_parameters) (overlay 234 236 "phptest.php"))
				  ))
		 nil
		 (overlay 209 260 "phptest.php"))) :type "class")
     nil
     (overlay 135 262 "phptest.php"))
    )
  "Expected results from the PHP Unit test"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data for Csharp C# tests

(defvar semantic-utest-Csharp-buffer-contents
"
class someClass {
  int fun1(int a, int b) {
    return a; }
  int fun2(int a, int b) {
    return b; }
}
")

(defvar semantic-utest-Csharp-name-contents
  '(("someClass" type
     (:members
      (("fun1" function
	(:arguments
	 (("a" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 30 35 "tst.cs"))
	  ("b" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 37 42 "tst.cs")))
	 :type "int")
	(reparse-symbol class_member_declaration)
	(overlay 21 61 "tst.cs"))
       ("fun2" function
	(:arguments
	 (("a" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 73 78 "tst.cs"))
	  ("b" variable
	   (:type "int")
	   (reparse-symbol formal_parameters)
	   (overlay 80 85 "tst.cs")))
	 :type "int")
	(reparse-symbol class_member_declaration)
	(overlay 64 104 "tst.cs")))
      :type "class")
     nil (overlay 1 106 "tst.cs")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun semantic-utest-makebuffer (filename contents)
  "Create a buffer for FILENAME for use in a unit test.
Pre-fill the buffer with CONTENTS."
  (let ((buff (semantic-find-file-noselect filename)))
    (set-buffer buff)
    (setq buffer-offer-save nil)
    (font-lock-mode -1) ;; Font lock has issues in Emacs 23
    (erase-buffer)
    (insert contents)
    ;(semantic-fetch-tags) ;JAVE could this go here?
    (set-buffer-modified-p nil)
    buff
    )
  )

(defun semantic-utest-C ()
  "Run semantic's C unit test."
  (interactive)
  (save-excursion
    (let ((buff  (semantic-utest-makebuffer semantic-utest-C-filename   semantic-utest-C-buffer-contents))
	  (buff2 (semantic-utest-makebuffer semantic-utest-C-filename-h semantic-utest-C-h-buffer-contents))
	  )
      (semantic-fetch-tags)
      (set-buffer buff)
      
      ;; Turn off a range of modes
      (semantic-idle-scheduler-mode -1)

      ;; Turn on some modes
      (semantic-highlight-edits-mode 1)

      ;; Update tags, and show it.
      (semantic-fetch-tags)

      (switch-to-buffer buff)
      (sit-for 0)
      
      ;; Run the tests.
      ;;(message "First parsing test.")
      (semantic-utest-verify-names semantic-utest-C-name-contents)

      ;;(message "Invalid tag test.")
      (semantic-utest-last-invalid semantic-utest-C-name-contents '("fun2") "/\\*1\\*/" "/* Deleted this line */")
      (semantic-utest-verify-names semantic-utest-C-name-contents)

      (set-buffer-modified-p nil)
      ;; Clean up
      ;; (kill-buffer buff)
      ;; (kill-buffer buff2)
      ))
  (message "All C tests passed.")
  )




(defun semantic-utest-generic (testname filename contents name-contents names-removed killme insertme)
  "Generic unit test according to template.
Should work for languages withouth .h files, python javascript java.
TESTNAME is the name of the test.
FILENAME is the name of the file to create.
CONTENTS is the contents of the file to test.
NAME-CONTENTS is the list of names that should be in the contents.
NAMES-REMOVED is the list of names that gets removed in the removal step.
KILLME is the name of items to be killed.
INSERTME is the text to be inserted after the deletion."
  (save-excursion
    (let ((buff  (semantic-utest-makebuffer filename  contents))
	  )
      ;; Turn off a range of modes
      (semantic-idle-scheduler-mode -1)

      ;; Turn on some modes
      (semantic-highlight-edits-mode 1)

      ;; Update tags, and show it.
      (semantic-fetch-tags)
      (switch-to-buffer buff)
      (sit-for 0)
      
      ;; Run the tests.
      ;;(message "First parsing test %s." testname)
      (semantic-utest-verify-names name-contents)

      ;;(message "Invalid tag test %s." testname)
      (semantic-utest-last-invalid name-contents names-removed killme insertme)
      (semantic-utest-verify-names name-contents)

      (set-buffer-modified-p nil)
      ;; Clean up
      ;; (kill-buffer buff)
      ))
  (message "All %s tests passed." testname)
  )

(defun semantic-utest-Python()
  (interactive)
  (if (fboundp 'python-mode)
      (semantic-utest-generic "Python" "/tmp/pytest.py" semantic-utest-Python-buffer-contents  semantic-utest-Python-name-contents   '("fun2") "#1" "#deleted line")
    (message "Skilling Python test: NO major mode."))
  )


(defun semantic-utest-Javascript()
  (interactive)
  (if (fboundp 'javascript-mode)
      (semantic-utest-generic "Javascript" "/tmp/javascripttest.js" semantic-utest-Javascript-buffer-contents  semantic-utest-Javascript-name-contents   '("fun2") "//1" "//deleted line")
    (message "Skipping JavaScript test: NO major mode."))
  )

(defun semantic-utest-Java()
  (interactive)
  ;; If JDE is installed, it might mess things up depending on the version
  ;; that was installed.
  (let ((auto-mode-alist  '(("\\.java\\'" . java-mode))))
    (semantic-utest-generic "Java" "/tmp/JavaTest.java" semantic-utest-Java-buffer-contents  semantic-utest-Java-name-contents   '("fun2") "//1" "//deleted line")
    ))

(defun semantic-utest-Makefile()
  (interactive)
  (semantic-utest-generic "Makefile" "/tmp/Makefile" semantic-utest-Makefile-buffer-contents  semantic-utest-Makefile-name-contents   '("fun2") "#1" "#deleted line")
  )

(defun semantic-utest-Scheme()
  (interactive)
  (semantic-utest-generic "Scheme" "/tmp/tst.scm" semantic-utest-Scheme-buffer-contents  semantic-utest-Scheme-name-contents   '("fun2") ";1" ";deleted line")
  )


(defun semantic-utest-Html()
  (interactive)
  ;; Disable html-helper auto-fill-in mode.
  (let ((html-helper-build-new-buffer nil))
    (semantic-utest-generic "HTML" "/tmp/tst.html" semantic-utest-Html-buffer-contents  semantic-utest-Html-name-contents   '("fun2") "<!--1-->" "<!--deleted line-->")
    ))

(defun semantic-utest-PHP()
  (interactive)
  (if (fboundp 'php-mode)
      (semantic-utest-generic "PHP" "/tmp/phptest.php" semantic-utest-PHP-buffer-contents semantic-utest-PHP-name-contents '("fun1") "fun2" "%^@")
    (message "Skipping PHP Test.  No php-mode loaded."))
  )

;look at http://mfgames.com/linux/csharp-mode
(defun semantic-utest-Csharp() ;; hmm i dont even know how to edit a scharp file. need a csharp mode implementation i suppose
  (interactive)
  (if (fboundp 'csharp-mode)
      (semantic-utest-generic "C#" "/tmp/csharptest.cs" semantic-utest-Csharp-buffer-contents  semantic-utest-Csharp-name-contents   '("fun2") "//1" "//deleted line")
    (message "Skipping C# test.  No csharp-mode loaded."))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stubs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stuff for Erlang
;;-module(hello).
;-export([hello_world/0]).
;
;hello_world()->
;    io:format("Hello World ~n"). 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun semantic-utest-Erlang()
;  (interactive)
;  (semantic-utest-generic "Erlang" "/tmp/tst.erl" semantic-utest-Erlang-buffer-contents  semantic-utest-Erlang-name-contents   '("fun2") "//1" "//deleted line")
;  )
;
;;texi is also supported
;(defun semantic-utest-Texi()
;  (interactive)
;  (semantic-utest-generic "texi" "/tmp/tst.texi" semantic-utest-Texi-buffer-contents  semantic-utest-Texi-name-contents   '("fun2") "//1" "//deleted line")
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun semantic-utest-main()
  (interactive)
  "call all utests"
  (cedet-utest-log-start "multi-lang parsing")
  (cedet-utest-log " * C tests...")
  (semantic-utest-C)
  (cedet-utest-log " * Python tests...")
  (semantic-utest-Python)
  (cedet-utest-log " * Java tests...")
  (semantic-utest-Java) 
  (cedet-utest-log " * Javascript tests...")
  (semantic-utest-Javascript)
  (cedet-utest-log " * Makefile tests...")
  (semantic-utest-Makefile)
  (cedet-utest-log " * Scheme tests...")
  (semantic-utest-Scheme)
  (cedet-utest-log " * Html tests...")
  (semantic-utest-Html)
  (cedet-utest-log " * PHP tests...")
  (semantic-utest-PHP)
  (cedet-utest-log " * Csharp tests...")
  (semantic-utest-Csharp)

  (cedet-utest-log-shutdown "multi-lang parsing")
  )

;;; Buffer contents validation
;;
(defun semantic-utest-match-attributes (attr1 attr2 skipnames)
  "Compare attribute lists ATTR1 and ATTR2.
Argument SKIPNAMES is a list of names that may be child nodes to skip."
  (let ((res t))
    (while (and res attr1 attr2)

      ;; Compare
      (setq res
	    (cond ((and (listp (car attr1))
			(semantic-tag-p (car (car attr1))))
		   ;; Compare the list of tags...
		   (semantic-utest-taglists-equivalent-p
		    (car attr2) (car attr1) skipnames)
		   )
		  (t
		   (equal (car attr1) (car attr2)))))
      
      (if (not res)
	  (error "TAG INTERNAL DIFF: %S %S"
		 (car attr1) (car attr2)))

      (setq attr1 (cdr attr1)
	    attr2 (cdr attr2)))
    res))

(defun semantic-utest-equivalent-tag-p (tag1 tag2 skipnames)
  "Determine if TAG1 and TAG2 are the same.
SKIPNAMES includes lists of possible child nodes that should be missing."
  (and (equal (semantic-tag-name tag1) (semantic-tag-name tag2))
       (semantic-tag-of-class-p tag1 (semantic-tag-class tag2))
       (semantic-utest-match-attributes
	(semantic-tag-attributes tag1) (semantic-tag-attributes tag2)
	skipnames)
       ))

(defun semantic-utest-taglists-equivalent-p (table names skipnames)
  "Compare TABLE and NAMES, where skipnames allow list1 to be different.
SKIPNAMES is a list of names that should be skipped in the NAMES list."
  (let ((SN skipnames))
    (while SN
      (setq names (remove (car SN) names))
      (setq SN (cdr SN))))
  (while (and names table)
    (if (not (semantic-utest-equivalent-tag-p (car names)
					      (car table)
					      skipnames))
	(error "Expected %s, found %s"
	       (semantic-format-tag-prototype (car names))
	       (semantic-format-tag-prototype (car table))))
    (setq names (cdr names)
	  table (cdr table)))
  (when names (error "Items forgotten: %S"
		     (mapcar 'semantic-tag-name names)
		     ))
  (when table (error "Items extra: %S" 
		     (mapcar 'semantic-tag-name table)))
  t)

(defun semantic-utest-verify-names (name-contents &optional skipnames)
  "Verify the names of the test buffer from NAME-CONTENTS.
Argument SKIPNAMES is a list of names that should be skipped
when analyzing the file.

JAVE this thing would need to be recursive to handle java and csharp"
  (let ((names name-contents)
	(table (semantic-fetch-tags))
	)
    (semantic-utest-taglists-equivalent-p table names skipnames)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;
; JAVE redefine a  new validation function
; is not quite as good as the old one yet
(defun semantic-utest-verify-names-jave (name-contents &optional skipnames)
  "JAVE version of `semantic-utest-verify-names'.
NAME-CONTENTS is a sample of the tags buffer to test against.
SKIPNAMES is a list of names to remove from NAME-CONTENTS"
 (assert (semantic-utest-verify-names-2 name-contents (semantic-fetch-tags))
	 nil "failed test")
)

(defun semantic-utest-verify-names-2 (l1 l2)
  (cond   ( (and (consp l1) (equal (car l1) 'overlay))
            (overlayp l2))
          ((not (consp l1))
           (equal l1 l2))
          ((consp l1)
           (and (semantic-utest-verify-names-2 (car l1) (car l2)) (semantic-utest-verify-names-2 (cdr l1) (cdr l2))))
          (t (error "internal error"))))





;;; Kill indicator line
;;
(defvar semantic-utest-last-kill-text nil
  "The text from the last kill.")

(defvar semantic-utest-last-kill-pos nil
  "The position of the last kill.")

(defun semantic-utest-kill-indicator ( killme insertme)
  "Kill the line with KILLME on it and insert INSERTME in its place."
  (goto-char (point-min))
;  (re-search-forward (concat "/\\*" indicator "\\*/")); JAVE this isnt generic enough for different lagnuages
  (re-search-forward killme)
  (beginning-of-line)
  (setq semantic-utest-last-kill-pos (point))
  (setq semantic-utest-last-kill-text
	(buffer-substring (point) (point-at-eol)))
  (delete-region (point) (point-at-eol))
  (insert insertme)
  (sit-for 0)
)

(defun semantic-utest-unkill-indicator ()
  "Unkill the last indicator."
  (goto-char semantic-utest-last-kill-pos)
  (delete-region (point) (point-at-eol))
  (insert semantic-utest-last-kill-text)
  (sit-for 0)
  )

;;;  EDITING TESTS
;;

(defun semantic-utest-last-invalid (name-contents names-removed killme insertme)
  "Make the last fcn invalid."
  (semantic-utest-kill-indicator killme insertme)
;  (semantic-utest-verify-names name-contents names-removed); verify its gone ;new validator doesnt handle skipnames yet
  (semantic-utest-unkill-indicator);put back killed stuff
  )




;"#<overlay from \\([0-9]+\\) to \\([0-9]+\\) in \\([^>]*\\)>"
;#<overlay from \([0-9]+\) to \([0-9]+\) in \([^>]*\)>
;(overlay \1 \2 "\3")


;; JAVE
;; these are some unit tests for cedet that I got from Eric and modified a bit for:
;;   python
;;   javascript
;;   java
;; I tried to generalize the structure of the tests a bit to make it easier to add languages

;; Mail from Eric:
;; Many items in the checklist look like:

;;       M-x global-semantic-highlight-edits-mode RET
;;       - Edit a file.  See the highlight of newly inserted text.
;;       - Customize `semantic-edits-verbose-flag' to be non-nil.
;;       - Wait for the idle scheduler, it should clean up the edits.
;;         - observe messages from incremental parser.  Do they relate
;; 	  to the edits?
;;       - M-x bovinate RET - verify your changes are reflected.

;; It's all about watching the behavior.  Timers go off, things get
;; cleaned up, you type in new changes, etc.  An example I tried to
;; do is below, but covers only 1 language, and not very well at that.
;; I seem to remember seeing a unit test framework going by one of the
;; lists.  I'm not sure if that would help.

;; Another that might be automatable:

;;       M-x semantic-analyze-current-context RET
;;        - Do this in different contexts in your language
;;          files.   Verify that reasonable results are returned
;;          such as identification of assignments, function arguments, etc.

;; Anyway, those are some ideas.  Any effort you put it will be helpful!

;; Thanks
;; Eric

;; -----------



