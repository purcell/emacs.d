;;; semantic-regtest.el --- Perform regression tests for grammars

;;; Copyright (C) 2003 Klaus Berndl

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: syntax test
;; X-RCS: $Id: semantic-regtest.el,v 1.6 2005/09/30 20:21:03 zappo Exp $

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
;; This library implements regression-tests for testing grammars and parsers
;; of semantic.
;;
;; This library offers:
;;
;; 1. Commands to run regression tests for grammar/parser tests. See the
;;    commands
;;    - `semantic-regtest-run-test'
;;    - `semantic-regtest-create-output'
;;    - `semantic-regtest-cmp-results'
;;    for a first description what this library can do with this respect.
;;
;;    Because for each of these three commands a function *--internal exists
;;    (which is meant to be used from within elisp) it should not be hard to
;;    run these functions from within a Makefile to run all regression-tests
;;    in batch-mode - e.g. before releasing a new release.
;;
;; 2. A new major-mode `semantic-regtest-mode' which is added to the
;;    `auto-mode-alist' for files ending with "*.res' (e.g. the command
;;    `semantic-regtest-run-test' creates autom. a result-file with such an
;;    extension). This new major-mode makes a lot of stuff in the result-file
;;    clickable - for details and keybindings see `semantic-regtest-mode'.
;;
;;
;; Currently this code is tested with GNU Emacs 21.X and the current CVS
;; cedet-suite

;;; TODO:
;;
;;  - testing with XEmacs
;;  - defining some constants, e.g. for the separtor-string " |###| " and some
;;    other currently hard coded stuff.
;;  - maybe using another parent-major-mode instead of `view-mode'?
;;  - testing when driven by a Makefile
;;  - testing with other code than c++, e.g. java, elisp....

;;; Code

(require 'semantic)

(defgroup semantic-regtest nil
  "Settings for semantic grammar/parser regression-tests."
  :group 'semantic
  :prefix "semantic-regtest-")

(defcustom semantic-regtest-functions
  '(semantic-regtest-prin1)
  "*Functions used for the grammar/parser regression-test.
Every element must be a function which gets one tag-argument and must return
a string which is the printed information about this tag. The function must
take into accout the value of `semantic-regtest-print-tag-boundaries'.

If nil then always `semantic-format-tag-prin1' is used; then of course the
value of `semantic-regtest-print-tag-boundaries' is automatically considered."
  :group 'semantic-regtest
  :type '(repeat (function :tag "Regression-test function")))

(defcustom semantic-regtest-print-tag-boundaries nil
  "*The generic regression-tag-format contains tag-boundaries.

The default-value is nil because normally it is not senseful to include
tag-boundaries into the printed generic tag-format because it prevents the
parsing check being independent from changing whitespace or comments in the
testfiles - which would not changing the tag-data itself but the
data-locations. But if this option is not nil then for each tag the
tag-boundaries are included in the output - if the tag is not positionless."
  :group 'semantic-regtest
  :type 'boolean)  

(defcustom semantic-regtest-highlight-tag t
  "*Highlight tag in the source-file.
This highlights the tag jumped to by `semantic-regtest-open-source-file' or
`semantic-regtest-mouse-open-source-file'."
  :group 'semantic-regtest
  :type 'boolean)

(defcustom semantic-regtest-find-file-function 'find-file-other-window
  "*Displayfunction for the files of `semantic-regtest-mode'.
This function is used to display a file in a window if one of the commands of
`semantic-regtest-mode' is used. The function gets one argument - a filename -
and has to display this file in a window.

Default is `find-file-other-window'."
  :group 'semantic-regtest
  :type 'function)

(defface semantic-regtest-test-button-face
  '((((class color) (background dark))
     (:forground "blue" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t)))
  "*Face used to show clickable buttons for the test files.
This can be the source-file and the test output file."
  :group 'semantic-regtest)

(defface semantic-regtest-reference-button-face
  '((((class color) (background dark))
     (:forground "ForestGreen" :bold t))
    (((class color) (background light))
     (:foreground "ForestGreen" :bold t)))
  "*Face used to show clickable buttons for the reference file."
  :group 'semantic-regtest)

;;;###autoload
(defun semantic-regtest-run-test ()
  (interactive)
  "Run a regression-test for a semantic-supported source-file.
The user will be asked for the file-name of that file for which the test
should be performed. If the current buffer is a semantic-supported buffer then
its file-name will be offered as default. For more details see the function
`semantic-regtest-run-test--internal'."
  (let* ((source-file (if (semantic-active-p) (buffer-file-name)))
         (file (read-file-name "Source-file: " nil source-file nil
                               (and source-file
                                    (file-name-nondirectory source-file)))))
    (if (semantic-regtest-run-test--internal file)
        (message "Regressiontest fails - see the generated result-file for the diff!")
      (message "Regressiontest succeeds - no differences to the reference-file!"))))

(defun semantic-regtest-run-test--internal (test-source-file)
  "Run a regression test for TEST-SOURCE-FILE.
If the regression-tests fails - i.e. if there are differences to the
reference-file - then the generated result-file will be displayed in another
window with active `semantic-regtest-mode'.

`semantic-regtest-run-test' is a regression test function which uses all the
utility functions of this library to run a regression test for a source-file.
The function assumes the following dir- and file-structure:
- all files reside in the same subdir
- Name of the reference output-file: TEST-SOURCE-FILE.ro
  \(Must already be generated with `semantic-regtest-create-output'!)
- Name of the test output-file: TEST-SOURCE-FILE.to
  \(Will be generated with `semantic-regtest-create-output')
- Name of the result file of the test: TEST-SOURCE-FILE.res \(Will be
  generated with `semantic-regtest-cmp-results' by comparing
  TEST-SOURCE-FILE.to with TEST-SOURCE-FILE.ro.

Example for test.cpp:
- Reference output-file: test.cpp.ro
- Test output-file: test.cpp.to
- Result file of the regression-test: test.cpp.res

Return nil if the are no differences in the test-outputs, i.e. if the test
succeeds. If the test fails \(i.e. there are differences between the
test-outputs) then the name of the generated result-file is returned.

The format of the file TEST-SOURCE-FILE.res is described at the command
`semantic-regtest-cmp-results'. Also how to interpret and use the file
TEST-SOURCE-FILE.res."  
  (let* ((test-file (expand-file-name test-source-file))
         (ref-output-file (concat test-file ".ro"))
         (test-output-file (concat test-file ".to"))
         (result-file (concat test-file ".res")))
    ;; opening the test source-file
    (save-excursion
      (set-buffer (find-file-noselect test-file))
      ;; generating the output of the grammar/parser test
      (semantic-regtest-create-output--internal test-output-file))
    ;; comparing with the reference output and writing a result-file.
    (when (semantic-regtest-cmp-results--internal test-file test-output-file
                                                  ref-output-file result-file)
        ;; now opening the result file in `semantic-regtest-mode'
      (find-file-other-window result-file)
      result-file)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: These pure utility-function should
;; be placed elsewhere!
(defun semantic-regtest-excessive-trim (str)
  "Return a string where all double-and-more whitespaces in STR are replaced
with a single space-character."
  (let ((s str))
    (save-match-data
      (while (string-match "[ \t][ \t]+" s)
        (setq s (concat (substring s 0 (match-beginning 0))
                        " "
                        (substring s (match-end 0))))))
    s))

(defun semantic-regtest-normalize-whitespace (text)
  "Replace all newlines with one single space and run the function
`semantic-regtest-excessive-trim' onto the result."
  (semantic-regtest-excessive-trim (subst-char-in-string ?\n 32 text)))


;;;###autoload
(defun semantic-regtest-create-output ()
  "Creates the test-output for the current buffer.
The user will be asked for the file-name of the created test-output-file \(see
`semantic-regtest-create-output--internal')."
  (interactive)
  (let ((file (if (file-exists-p (concat (buffer-file-name) ".ro"))
                  (concat (buffer-file-name) ".to")
                (concat (buffer-file-name) ".ro"))))
    (setq file (read-file-name "Test-output: " nil file nil
                               (file-name-nondirectory file)))
    (semantic-regtest-create-output--internal file)))
  

(defun semantic-regtest-create-output--internal (test-output-file)
  "Runs the functions in `semantic-regtest-functions' on every tag in current
buffer and writes the output to TEST-OUTPUT-FILE. This gives a regression-able
test of a grammar/parser because this function can run on a testfile F before
grammar-changes and after grammar-changes and after that the two output-files
can be compared with a tool like diff \(but recommended is to use
`semantic-regtest-cmp-results').

IMPORTANT: ALL information about a tag is written in ONE line. This is for
better comparsion with line-oriented tools like diff. The format of a line is:

  <tag-name> |###| <tag-type> |###| <full tag-text> |###|
     <output of print-function-1> |###| <output of print-function-2> |###|
     ... |###|

whereas <tag-name>, <full tag-text> and <output of print-function-X> are
normalized concerning whitespace \(`semantic-regtest-normalize-whitespace'),
<output of print-function-1> is \"<print-function-1>: <print-text>\" whereas
<print-function-X> is part of `semantic-regtest-functions'.

Return the number of tags."
  (goto-char (point-min))
  (let ((buf (get-buffer-create "*Semantic regression test*"))
        (test-functions (or semantic-regtest-functions
                            '(semantic-format-tag-prin1)))
        (tag-counter 0)
        tag tag-extend tag-text output-str)

    (unless (semantic-active-p)
      (error "Sorry, regression-test are only possible for semantic supported sources!"))
    
    ;; clean the output buffer
    (save-excursion
      (set-buffer buf)
      (erase-buffer))
    
    ;; reparse the whole source-buffer so we have fresh-parsed tags
    (semantic-fetch-tags)

    ;; print out the tag informations of all tags. IMPORTANT: ALL
    ;; information about a tag is written in ONE line. This is for better
    ;; comparsion with line-oriented tools like diff.
    ;; The format of a line is:
    ;; <tag-name> |###| <full tag-text> |###| <output of print-function-1>
    ;;    |###| <output of print-function-2> |###| ... |###|
    ;; whereas <output of print-function-1> is "<print-function-1>: <print-text>"
    ;; (all in one single line without linebreaks!)
    
    (while (setq tag (semantic-find-tag-by-overlay-next))
      (setq tag-counter (1+ tag-counter))
      (if (not (semantic-tag-with-position-p tag))
          (setq tag-text "This is a positionless tag")
        (setq tag-extend (semantic-tag-bounds tag))
        (setq tag-text (buffer-substring-no-properties (nth 0 tag-extend)
                                                       (nth 1 tag-extend))))
      (setq output-str (format "%s |###| %s |###| %s |###|"
                               ;; we have to normalize also the whitespace of
                               ;; a tag-name because because there is nowhere
                               ;; forbidden that a tag-name can contain spaces
                               ;; or newlines (e.g. the python-parser produces
                               ;; such tag-names)
                               (semantic-regtest-normalize-whitespace
                                (semantic-tag-name tag))
                               (symbol-name (semantic-tag-class tag))
                               ;; to make testresults whitespace-independend
                               ;; we remove all newlines and then we trim all
                               ;; spaces to exactly one space
                               (semantic-regtest-normalize-whitespace tag-text)))

      (dolist (fnc test-functions)
        (setq output-str
              (concat output-str (format " %s: %s |###|"
                                         (symbol-name fnc)
                                         ;; we normalize the whitespace of the
                                         ;; returned string because there can
                                         ;; be tags with a tagname which
                                         ;; contains spaces or newlines (e.g.
                                         ;; with python)
                                         (semantic-regtest-normalize-whitespace
                                          (funcall fnc tag))))))
        (save-excursion
        (set-buffer buf)
        (goto-char (point-max))
        (insert output-str)
        (insert "\n"))
      (goto-char (semantic-tag-start tag)))

    ;; write the generated tag-informations into TEST-OUTPUT-FILE
    (save-excursion
      (set-buffer buf)
      ;; maybe removing the overlay-positions
      (goto-char (point-min))
      (if semantic-regtest-print-tag-boundaries
          (while (re-search-forward
                  "#<overlay from \\([0-9]+\\) to \\([0-9]+\\) in [^>]+>"
                  nil t)
            (replace-match "[\\1 \\2]"))
        (while (re-search-forward "#<overlay from [0-9]+ to [0-9]+ in [^>]+>"
                                  nil t)
          (replace-match "[Location info filtered out]")))
      (write-region (point-min) (point-max) test-output-file))

    ;; clean up
    (kill-buffer buf)
    (goto-char (point-min))

    ;; return number of printed tags
    tag-counter))

(defun semantic-regtest-convert-difference (buffer start end)
  "Parse the diff-difference located in BUFFER between START and END. Cause of
the facts that each line in the output of `semantic-regtest-create-output'
represents exactly one tag and \[START, END] always define a
set of complete lines of BUFFER \(and therefore a set of tag-outputs) the
text between START and END can be splitted in lines and each of these lines is
splitted by the separator \" |###| \".

Result is either nil \(if START = END) or a list of sublists whereas each
sublist represents one line resp. tag between START and END and consist
therefore of the following elements:
0. tag-number of tag in the test-file (= line-number in the test-file)
1. name of the tag
2. type of the tag \(function, variable, type, include etc...)
3. the complete tag text
4. the tag-string of the first tag-print-function. This string looks like
   \"<print-function>: <print-output>\", e.g. \"semantic-format-tag-prin1:
   \(\\\"c++-test.hh\\\" include nil nil nil \[Location info filtered out])\"
   \(all output of a tag is in one line - no linebreaks!)
5. the tag-string of the second tag-print-function
6. ...
If a list then every sublist contains at least 5 elements \(0. to 4.)."
  (and (not (= start end))
       (save-excursion
         (set-buffer buffer)
         (let ((line-list (split-string (buffer-substring-no-properties start
                                                                        end)
                                        "\n"))
               (line-counter (1+ (count-lines (point-min) start)))
               result)
           (dolist (line line-list)
             (setq result
                   (cons
                    (append (list line-counter)
                            (split-string line " |###| ?"))
                    result))
             (setq line-counter (1+ line-counter)))
           (nreverse result)))))

;; The following two function are examples how to print the data of one
;; diff-difference (can contain data for more than 1 line (resp. tag)!).
(defun semantic-regtest-1-diffdata2str (diff-data file &optional prefix)
  "Convert the data of DIFF-DATA into a suitable string-representation where
each element of DIFF-DATA is separated by a newline within this string. PREFIX
is the prefix for each line if a string."
  (let ((output-str nil))
    (dolist (elem diff-data output-str)
      (setq output-str
            (concat output-str
                    (format "%s%s (tag-type: %s, [%d. tag of %s file])\n"
                            (or prefix
                                "")
                            (nth 1 elem) (nth 2 elem) (nth 0 elem) file))))))

(defun semantic-regtest-2-diffdata2str (a-diff-data b-diff-data
                                                    &optional prefix)
  "Convert the data of A-DIFF-DATA into a suitable string-representation by
comparing each elem of A-DIFF-DATA with the related elem of B-DIFF-DATA where
each element of A-DIFF-DATA is printed by two lines whereas the first line
contains the tag-name of the A-DIFF-DATA-elem and the tag-numbers and the
second line contains the kind of difference between the two elements \(
different tag-name, tag-type, tag-text and/or tag-output). PREFIX is
the prefix for the first line of such a two-line-block - the second line gets
a prefix with same length as PREFIX but filled with spaces.

If the length of A-DIFF-DATA and B-DIFF-DATA is unequal then an error is
reported."
  (if (not (= (length a-diff-data) (length b-diff-data)))
      (error "Can not compare diff-lists with unequal length!")
    (let ((b-diff-data-copy b-diff-data)
          str)
      (dolist (elem a-diff-data str)
        (setq str
              (concat str
                      (format "%s%s (type: %s, [%d. tag of test file], [%d. tag of reference file])\n"
                              (or prefix
                                  "")
                              (nth 1 elem)
                              (nth 2 elem)
                              (nth 0 elem)
                              (nth 0 (car b-diff-data-copy)))
                      (format "%s%s%s%s%s\n"
                              (make-string (length prefix) 32)
                              (if (not (string= (nth 1 elem)
                                                (nth 1 (car b-diff-data-copy))))
                                  "Different tag-name, "
                                "")
                              (if (not (string= (nth 2 elem)
                                                (nth 2 (car b-diff-data-copy))))
                                  "Different tag-type, "
                                "")
                              (if (not (string= (nth 3 elem)
                                                (nth 3 (car b-diff-data-copy))))
                                  "Different tag-text, "
                                "")
                              (if (not (string= (nth 4 elem)
                                                (nth 4 (car b-diff-data-copy))))
                                  "Different tag-output"
                                ""))))
        (setq b-diff-data-copy (cdr b-diff-data-copy))))))

;; this is the only function where ediff-stuff is used!
(defun semantic-regtest-ediff (file-a file-b)
  "Run ediff noninteractively to compare FILE-A and FILE-B. The result
is is list with contains for every difference between FILE-A and FILE-B a
vector: \[a-start a-end b-start b-end nil nil nil nil nil nil nil]

What is the \"semantic\" of such a difference-result-vector:

If \(a-start = a-end) Then lines \(= tags) between b-start and b-end of
                          FILE-B are missed in FILE-A
ElseIf \(b-start = b-end) Then lines \(= tags between a-start and a-end are
                              new in FILE-A (missed in the FILE-B) 
Else lines \(= tags between a-start and a-end are parsed differently.

If there are no differences between FILE-A and FILE-B then nil is returned."      
  (require 'ediff)
  ;; we must set ediff-buffer-A, ediff-buffer-B and ediff-buffer-C because
  ;; these buffers are needed by ediff to work
  (let ((ediff-buffer-A (find-file-noselect (expand-file-name file-a)))
        (ediff-buffer-B (find-file-noselect (expand-file-name file-b)))
        (ediff-buffer-C nil))

    (if (string-match "c" ediff-diff-options)
        (error "Option `-c' is not allowed in `ediff-diff-options'"))

    ;; use some ediff stuff to produce correct differences between test-file
    ;; and ref-file
    (or (and ediff-diff-buffer (buffer-live-p ediff-diff-buffer))
        (setq ediff-diff-buffer
              (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))
    (ediff-make-diff2-buffer ediff-diff-buffer file-a file-b)
    (ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
    (cdr (ediff-extract-diffs ediff-diff-buffer nil nil))))


;;;###autoload
(defun semantic-regtest-cmp-results (&optional use-full-path-name)
  "Compare two test-outputs and create a suitable formatted result-file.

The user will be asked for four file-names:

   SOURCE-FILE: The underlying source-file for which the test-outputs have
   been created. If current buffer is a semantic-supported buffer then the
   file-name of the current buffer is offered as default.

   TEST-FILE: The regression-testoutput for SOURCE-FILE. It must be an already
   existing file which has been created by `semantic-regtest-create-output' or
   the function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.to exists already in current directory then this file is
   offered as default.

   REF-FILE: The reference testoutput for SOURCE-FILE. TEST-FILE will be
   compared against this file. It must be an already existing file which has
   been created by the command `semantic-regtest-create-output' or the
   function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.ro exists already in current directory then this file is
   offered as default.

   RESULT-FILE: That file will contain the comparisson-result generated by
   `semantic-regtest-cmp-results--internal'. Per default the filename
   SOURCE-FILE.res is offered.

This command calls `semantic-regtest-cmp-results--internal' with that four
file-names. See this function for details about the optional argument
`use-full-path-name' and a description of the format of RESULT-FILE."
  (interactive "P")
  (let* ((source-file (if (semantic-active-p) (buffer-file-name)))
         (test-file (and source-file
                         (file-exists-p (concat source-file ".to"))
                         (concat source-file ".to")))
         (ref-file (and source-file
                        (file-exists-p (concat source-file ".ro"))
                        (concat source-file ".ro")))
         (result-file (and source-file (concat source-file ".res"))))
    (setq source-file (read-file-name "Source-file: " nil source-file nil
                                      (and source-file
                                           (file-name-nondirectory source-file))))
    (setq test-file (read-file-name "Test-output: " nil test-file nil
                                    (and test-file
                                         (file-name-nondirectory test-file))))
    (setq ref-file (read-file-name "Reference-output: " nil ref-file nil
                                   (and ref-file
                                        (file-name-nondirectory ref-file))))
    (setq result-file (read-file-name "Test-result: " nil result-file nil
                                      (and result-file
                                           (file-name-nondirectory result-file))))
    (semantic-regtest-cmp-results--internal source-file test-file ref-file
                                            result-file use-full-path-name)))
    

(defun semantic-regtest-cmp-results--internal (source-file
                                               test-file
                                               ref-file
                                               result-file
                                               &optional use-full-path-name)
  "Compare TEST-FILE and REF-FILE and write the results to RESULT-FILE.

SOURCE-FILE is only used to write the file-name into RESULT-FILE.

Return nil if there are no differences between TEST-FILE and REF-FILE
otherwise return not nil.

Format of RESULT-FILE is:

------------------------------------------------------------------------
Semantic grammar/parser regression-test

Source file: SOURCE-FILE
Test output file: TEST-FILE
Reference file: REF-FILE

<Here are listed all tag-parsing differences: This can be missed tags
\(i.e. tag which are only in REF-FILE), new tags \(tag which are only in
TEST-FILE) and differently parsed tags. Each type can occur multiple times
and the sequence follows the original sequence of the differences detected by
the ediff-comparison>
------------------------------------------------------------------------

If USE-FULL-PATH-NAME is nil then these three filesnames are without
path-informations because normally all four files \(SOURCE-FILE TEST-FILE
REF-FILE and RESULT-FILE) should reside in the same directory so the path-info
is not needed to open these files from within `semantic-regtest-mode'. If
USE-FULL-PATH-NAME is not nil \(called with a prefix arg) filenames include
full path-info.

How to interpret and use the created RESULT-FILE:
  
For all differences reported in RESULT-FILE the number N of the each missed,
new or differently parsed tag is printed out. With this number you can
- use `semantic-regtest-goto-tag' to jump to the N-th tag in the
  source-file for which TEST-FILE is generated to check the tag in the
  source-code
- use `goto-line' to go to the N-th line in either TEST-FILE or REF-FILE to
  check the output of `semantic-regtest-create-output' for this tag.
- Open the file in `semantic-regtest-mode' and use the offered buttons and
  keybindings."
  (let ((diff-result (semantic-regtest-ediff test-file ref-file))
        (test-buffer (find-file-noselect (expand-file-name test-file)))
        (ref-buffer (find-file-noselect (expand-file-name ref-file)))
        a-start a-end a-diff-data b-start b-end b-diff-data output-str)
    
    (with-temp-file (expand-file-name result-file)
      (erase-buffer)
      (insert "Semantic grammar/parser regression-test\n\n")
      (insert (format "Source file: [%s]\n"
                      (if use-full-path-name
                          source-file
                        (file-name-nondirectory source-file))))
      (insert (format "Test output file: [%s]\n"
                      (if use-full-path-name
                          test-file
                        (file-name-nondirectory test-file))))
      (insert (format "Reference file: [%s]\n"
                      (if use-full-path-name
                          ref-file
                        (file-name-nondirectory ref-file))))
      (insert "\n\n")
      
      (if (null diff-result)
          (insert "No differences!\n")
        ;; evaluating the ediff-result
        (dolist (diff-elem diff-result)
          (setq a-start (aref diff-elem 0)
                a-end (aref diff-elem 1)                
                a-diff-data (semantic-regtest-convert-difference
                             test-buffer a-start a-end)
                
                b-start (aref diff-elem 2)
                b-end (aref diff-elem 3)
                b-diff-data (semantic-regtest-convert-difference
                             ref-buffer b-start b-end))
          
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: The following is just a
          ;; first example how the output of the test-result could look. Maybe
          ;; it would be useful to print out more data about differences - but
          ;; this is not a problem, because we have all data we need in the
          ;; a-diff-data resp. b-diff-data.
          
          (cond ((null a-diff-data) ;; tags are missed
                 (setq output-str
                       (concat "These tags are only in the reference file:\n"
                               (semantic-regtest-1-diffdata2str b-diff-data
                                                                "reference"
                                                                "- "))))
                ((null b-diff-data) ;; tags are new
                 (setq output-str
                       (concat "These tags are only in the test file:\n"
                               (semantic-regtest-1-diffdata2str a-diff-data
                                                                "test"
                                                                "+ "))))
                (t ;; tag are parsed differently
                 ;; if a-diff-data and b-diff-data contain the same number of
                 ;; elements then we can compare the tags of a-diff-data and
                 ;; b-diff-data on a pair-basis. Otherwise we simply list the
                 ;; tags of a-diff-data and then the tags of b-diff-data.
                 (if (= (length a-diff-data) (length b-diff-data))
                     (setq output-str
                           (concat "These tags are parsed differently:\n"
                                   (semantic-regtest-2-diffdata2str a-diff-data
                                                                    b-diff-data
                                                                    "* ")))
                   (setq output-str
                         (concat "These tag of a the test- and the reference-file are parsed differently:\n"
                                 (semantic-regtest-1-diffdata2str a-diff-data
                                                                  "test"
                                                                  "-t- ")
                                 (semantic-regtest-1-diffdata2str b-diff-data
                                                                  "reference"
                                                                  "-r- "))))))
          
          (insert output-str)
          (insert "\n\n"))))

    ;; clean up
    (kill-buffer test-buffer)
    (kill-buffer ref-buffer)
    diff-result))

(defun semantic-regtest-goto-tag (tag-number)
  "Jump to the tag with number TAG-NUMBER in current buffer.
Counting starts always at the beginning of current buffer.

This function can be used for fast and easy jumping to the differences
reported by `semantic-regtest-cmp-results'."
  (interactive "nNumber of tag to jump: ")
  (goto-char (point-min))
  (let ((tag-counter 0)
        tag)
    (while (and (< tag-counter tag-number)
                (setq tag (semantic-find-tag-by-overlay-next)))
      (setq tag-counter (1+ tag-counter))
      (goto-char (semantic-tag-start tag)))))


;; ------ code for the new major-mode semantic-regtest-mode -----------------

(defun semantic-regtest-mouse-open-source-file (e)
  "See `semantic-regtest-open-source-file'"
  (interactive "e")
  (mouse-set-point e)
  (semantic-regtest-goto-file 'source))

(defun semantic-regtest-mouse-open-output-file (e)
  "See `semantic-regtest-open-output-file'"
  (interactive "e")
  (mouse-set-point e)
  (semantic-regtest-goto-file 'output))
      
(defun semantic-regtest-open-source-file ()
  "Open the source-file of this button in another window. If the button is a
tag-number then jump also to this tag."
  (interactive)
  (semantic-regtest-goto-file 'source))

(defun semantic-regtest-open-output-file ()
  "Open the output-file of this button in another window. If the button is a
tag-number then jump also to this line in the output-file."
  (interactive)
  (semantic-regtest-goto-file 'output))
      
    
(defun semantic-regtest-goto-file (type)
  "Action function for all clickable buttons in `semantic-regtest-mode'.
TYPE can be one of the symbols `output' or `source'. In case of the former one
it tries to open the right output-file in the other-window and tries to jump
to the right line. In case of the latter one it opens the source-file in the
other window and tries to jump to the right tag."
  (let ((file (if (equal type 'output)
                  (or (get-text-property (point)
                                         'semantic-regtest-mode-test-file)
                      (get-text-property (point)
                                         'semantic-regtest-mode-ref-file))
                (get-text-property (point)
                                   'semantic-regtest-mode-source-file)))
        (tag-number (ignore-errors
                        (string-to-number
                         (get-text-property
                          (point)
                          'semantic-regtest-mode-tag-number)))))
    (when file
      (message "Opening file: %s" (file-name-nondirectory file))
      (funcall semantic-regtest-find-file-function file)
      (when tag-number
        (if (equal type 'output)
            (goto-line tag-number)
          (semantic-regtest-goto-tag tag-number)
          (if semantic-regtest-highlight-tag
              (semantic-momentary-highlight-tag
               (semantic-current-tag))))))))


(defun semantic-regtest-mode-init ()
  "Initializes `semantic-regtest-mode'. This means making all tag-numbers
and the source-file, the test output file and the reference file clickable."
  (let ((buffer-read-only nil)
        regtest-mode-source-file
        regtest-mode-test-file
        regtest-mode-ref-file)
    (goto-char (point-min))

    ;; make the 3 files clickable

    (if (re-search-forward "^Source file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-source-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 help-echo
                                 ,(format "Mouse-2 opens the file %s"
                                          regtest-mode-source-file)
                                 face
                                 semantic-regtest-test-button-face
                                 semantic-regtest-mode-source-file
                                 ,regtest-mode-source-file)))
      (error "No source file found in the regtest result!"))
    (goto-char (point-min))
    (if (re-search-forward "^Test output file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-test-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 help-echo
                                 ,(format "Mouse-1 opens the file %s"
                                          regtest-mode-test-file)
                                 face
                                 semantic-regtest-test-button-face
                                 semantic-regtest-mode-test-file
                                 ,regtest-mode-test-file)))
      (error "No test ouput file found in the regtest result!"))
    (goto-char (point-min))
    (if (re-search-forward "^Reference file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-ref-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 help-echo
                                 ,(format "Mouse-1 opens the file %s"
                                          regtest-mode-ref-file)
                                 face
                                 semantic-regtest-reference-button-face
                                 semantic-regtest-mode-ref-file
                                 ,regtest-mode-ref-file)))
      (error "No reference-file file found in the regtest result!"))

    ;; now make all tag-numbers clickable
    
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]+\\)\\. tag of test file" nil t)
      (add-text-properties (1- (match-beginning 0))
                           (1+ (match-end 0))
                           `(mouse-face
                             highlight
                             help-echo
                             ,(format "Mouse-1 jumps to line %s in %s, mouse-2 jumps to this tag in %s"
                                      (match-string 1) regtest-mode-test-file
                                      regtest-mode-source-file)
                             face
                             semantic-regtest-test-button-face
                             semantic-regtest-mode-tag-number
                             ,(match-string 1)
                             semantic-regtest-mode-source-file
                             ,regtest-mode-source-file
                             semantic-regtest-mode-test-file
                             ,regtest-mode-test-file))
      )
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]+\\)\\. tag of reference file" nil t)
      (add-text-properties (1- (match-beginning 0))
                           (1+ (match-end 0))
                           `(mouse-face
                             highlight
                             help-echo
                             ,(format "Mouse-1 jumps to line %s in %s"
                                      (match-string 1) regtest-mode-ref-file)
                             face
                             semantic-regtest-reference-button-face
                             semantic-regtest-mode-tag-number
                             ,(match-string 1)
                             semantic-regtest-mode-ref-file
                             ,regtest-mode-ref-file))
      )
    (set-buffer-modified-p nil)
    (goto-char (point-min))))
  

(define-derived-mode semantic-regtest-mode
  view-mode "se-re-te"
  "Major mode for viewing result files of semantic regression tests. The main
purpose of this mode is to make all tag-numbers and the source-file, the
test output file and the reference file clickable.
\\{semantic-regtest-mode-map}"
  (semantic-regtest-mode-init))

;; mouse-bindings
(define-key semantic-regtest-mode-map
  (if (featurep 'xemacs) '(button1) [mouse-1])
  'semantic-regtest-mouse-open-output-file)

(define-key semantic-regtest-mode-map
  (if (featurep 'xemacs) '(button2) [mouse-2])
  'semantic-regtest-mouse-open-source-file)

;; keyboard bindings:
(define-key semantic-regtest-mode-map
  (kbd "O")
  'semantic-regtest-open-output-file)

(define-key semantic-regtest-mode-map
  (kbd "S")
  'semantic-regtest-open-source-file)


;; adding reference- and regtest-output- and result-files to the
;; auto-mode-alist. We open the *.to and *.ro-files in text-mode to avoid
;; parsing this files by semantic.
(setq auto-mode-alist (append '(("\\.res\\'" . semantic-regtest-mode))
                              auto-mode-alist))
(setq auto-mode-alist (append '(("\\.to\\'" . text-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ro\\'" . text-mode)) auto-mode-alist))


;;; Generic format

(defun semantic-regtest-convert-tag-table (table)
  "Convert the tag table TABLE to a generic format."
  (mapcar #'semantic-regtest-convert-tag table))

(defun semantic-regtest--convert-tag (tag)
  "Default tag-conversion of TAG into a generic format.
Recurses over children when they are found. If the value of the option
`semantic-regtest-print-tag-boundaries' is not nil then the tag-boundaries are
added at the beginning of the generic tag-format."
    (let* ((name (semantic-tag-name tag))
           (class (semantic-tag-class tag))
           (bounds (if (and semantic-regtest-print-tag-boundaries
                            (semantic-tag-with-position-p tag))
                       (semantic-tag-bounds tag)))
           (attr (semantic-tag-attributes tag))
           (generic nil))
      (while attr
        (let ((sym (car attr))
              (val (car (cdr attr))))
          (cond ((semantic-tag-p val)
                 ;; This attribute is a tag (ie, a type perhaps?)
                 (setq val (semantic-regtest-convert-tag val)))
                ((and (listp val) (semantic-tag-p (car val)))
                 ;; List of more tags in this property.  Children/members
                 (setq val (semantic-regtest-convert-tag-table val)))
                (t nil))
          (setq generic (cons (list sym val) generic))
          (setq attr (cdr (cdr attr)))))
      ;; At this point, generic is an ALIST, not a PROPERTY LIST.
      ;; We need to sort it so that order changes do not effect the
      ;; test.
      (setq generic (sort generic (lambda (a b)
                                    (string< (symbol-name (car a))
                                             (symbol-name (car b))))))
      (append (delq nil (list bounds name class))
              (apply 'append generic))
      ))

(define-overload semantic-regtest-convert-tag (tag)
  "Convert TAG into a generic format.
Recurses over children when they are found."
  (semantic-regtest--convert-tag tag))

(defun semantic-regtest-prin1 (tag)
  "Dump TAG to a string and return this string."
  (prin1-to-string (semantic-regtest-convert-tag tag)))


(provide 'semantic-regtest)

;;; semantic-regtest.el ends here
