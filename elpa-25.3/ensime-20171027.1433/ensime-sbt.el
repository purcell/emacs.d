;;; ensime-sbt.el --- SBT support for ENSIME -*- lexical-binding: t -*-

;; Copyright (C) 2015 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  SBT should be an optional system dependency of ENSIME (we try to
;;  be build tool agnostic, even if we have a favourite) and ideally
;;  all sbt-related content should be contained to this file.
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'sbt-mode)
(require 'ensime-config)
(require 'ensime-inf)

(defgroup ensime-sbt nil
  "Support for sbt build REPL."
  :group 'ensime
  :prefix "ensime-sbt-")

(defcustom ensime-sbt-perform-on-save nil
  "Which (if any) sbt action to perform when a file is saved."
  :type '(choice (const nil) string)
  :group 'ensime-sbt)

(defun ensime-sbt ()
  "Switch to the sbt shell (create if necessary) if or if already there, back.
   If already there but the process is dead, restart the process. "
  (interactive)
  (ensime-with-conn-interactive
   conn
   (with-current-buffer (sbt-start)
     (setq ensime-buffer-connection conn)
     (add-hook 'ensime-source-buffer-saved-hook 'ensime-sbt-maybe-auto-compile)
     (add-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter))))

(defun ensime-sbt-maybe-auto-compile ()
  (when (and
         (ensime-connected-p)
         ensime-sbt-perform-on-save
         (get-buffer (sbt:buffer-name)))
    (sbt:command ensime-sbt-perform-on-save)))

(defalias 'ensime-sbt-switch 'ensime-sbt)

(defun ensime-sbt-send-eol ()
  (interactive)
  (let ((sbt:clear-buffer-before-command nil))
    (sbt:command "")))

(defun ensime-sbt-do-compile ()
  "Compile all sources including tests." 
  (interactive)
  (sbt:command "test:compile"))

(defun ensime-sbt-do-compile-only (arg)
  "Compile this buffer using `sbt-ensime's `ensimeCompileOnly', ARG will add `-Xprint:typer'."
  (interactive "P")
  (save-buffer)
  (let ((command
         (if arg "ensimeCompileOnly -Xprint:typer" "ensimeCompileOnly")))
    (ensime-sbt-run-command-in-subproject command (buffer-file-name-with-indirect))))

(defun ensime-sbt-do-scalariform-only ()
  "Format the current file using Scalariform."
  (interactive)
  (save-buffer)
  (ensime-sbt-run-command-in-subproject "ensimeScalariformOnly" (buffer-file-name-with-indirect)))

(defun ensime-sbt-run-command-in-subproject (command file-name)
  "Run a sbt COMMAND in the module containing FILE-NAME, if specified."
  (let ((subproject (ensime-subproject-for-config)))
    (if subproject
        (if (ensime-is-test-file file-name)
            (sbt:command (concat subproject "/" "test:" command " " file-name))
          (sbt:command (concat subproject "/" command " " file-name)))
      (if (ensime-is-test-file file-name)
          (sbt:command (concat "test:"command " " file-name))
        (sbt:command (concat command " " file-name))))))

(defun ensime-sbt-do-run ()
  "Execute the sbt `run' command for the project."
  (interactive)
  (sbt:command "run"))

(defun ensime-sbt-do-clean ()
  "Execute the sbt `clean' command for the project."
  (interactive)
  (sbt:command "clean"))

(defun ensime-sbt-do-ensime-config ()
  "Execute the sbt `ensimeConfig' command for the project."
  (interactive)
  (sbt:command "ensimeConfig"))

(defun ensime-sbt-do-package ()
  "Build a jar file of the project."
  (interactive)
  (sbt:command "package"))

;; shameless copypasta from magit-utils.el
;; added one little thing to eval the dynamic submodule binding
(defmacro ensime-sbt-read-char-prompt (prompt verbose &rest clauses)
  "Prompts and reads for a character defined in CLAUSES.

CLAUSES can either be an associative list of list clauses
(char prompt string), or it can be something that when evaluated
returns an associative list of the same structure.  Prompting for
modules is done dynamically, and uses the latter, while other
prompting requires no evaluation and is done up front."
  (let* ((first-clause (car clauses))
	 (clauses (if (and (sequencep first-clause)
			   (= 3 (length first-clause)))
		      clauses
		    (eval clauses))))
    (declare (indent 2)
	     (debug (form form &rest (characterp form body))))
    `(pcase (read-char-choice
	     (concat ,prompt
		     ,(concat (mapconcat 'cadr clauses ", ")
			      (and verbose ", or [C-g] to abort") " "))
	     ',(mapcar 'car clauses))
       ,@(--map `(,(car it) ,@(cddr it)) clauses))))

(defun ensime-sbt-module-prompt (module abbrev li ri)
  "Makes the module prompt string.

SUBPROJECT is the module name, i.e. `core' ABBREV is the
abbreviation character for the prompt, i.e. `?o' LI AND RI are
the indices to the left and right of the ABBREV character in the
string.

Results in the same module name, but with brackets enclosing the
abbreviation character."
  (let ((left (s-left li module))
        (prompt-abbrev (char-to-string abbrev))
        (right (-> (- (length module) ri) (s-right module))))
    (s-concat left "[" prompt-abbrev "]" right)))

(defun ensime-sbt-prompt-char-indices (module li ri module-prompt-clauses)
  "Finds the char abbreviation and indices of this MODULE.

MODULE is the module name, i.e. `core'.
MODULE-PROMPT-CLAUSES is a
potentially empty associative list that is of the form
  (char module-prompt module).

Looks at the character defined at left and right indices (`LI'
and `RI'), makes sure that character isn't already used as a key
in the MODULE-PROMPT-CLAUSES If no key is defined for that
character use it, otherwise move on to the next character and try
again."
  (let ((abbrev (-> (substring module li ri) string-to-char)))
    (if (eq nil (assoc abbrev module-prompt-clauses))
        (list abbrev li ri)
      (ensime-sbt-prompt-char-indices module (+ 1 li) (+ 1 ri) module-prompt-clauses))))

(defun ensime-sbt-prompt-for-test ()
  "Prompt sequence when `*-test-dwim' can't figure out what to do."
  (let ((module
         (completing-read "Module to test: "
                          (->> (-> (ensime-connection) ensime-config (plist-get :subprojects))
                               (-map (lambda (sp) (plist-get sp :name))))))
        (source-set
         (ensime-sbt-read-char-prompt "Test suite: " t
                                      (?t "[t]est" "")
                                      (?i "[i]t" "it:")
                                      (?f "[f]un" "fun:")))
        (task
         (ensime-sbt-read-char-prompt "Test command: " t
                                      (?t "[t]est" "test")
                                      (?o "test-[o]nly" "testOnly")
                                      (?q "test-[q]uick" "testQuick"))))
    (concat module "/" source-set task)))

(defun ensime-sbt-test-dwim (command)
  (let* ((file-name (or (buffer-file-name-with-indirect) default-directory))
         (source-set (cond
                      ((string-match-p "src/test" file-name) "")
                      ((string-match-p "/test" file-name) "") ;; for Play's default dir layout
                      ((string-match-p "src/it" file-name) "it:")
                      ((string-match-p "src/fun" file-name) "fun:"))))
    (if source-set
        (-> (ensime-subproject-for-config)
            (concat "/" source-set command)
            sbt:command)
      (-> (ensime-sbt-prompt-for-test) sbt:command)
      (message "Run `%s' from a test file to avoid the prompts"
               (key-description
                (where-is-internal this-command overriding-local-map t))))))

(defun ensime-sbt-do-test ()
  "Run all the tests."
  (interactive)
  (sbt-command "test"))

(defun ensime-sbt-do-test-dwim ()
  "Execute the sbt `test' command for the project and suite that
corresponds to the current source test file.

If not run from a test source file, then prompt for the project
module, test suite and test command."
  (interactive)
  (ensime-sbt-test-dwim "test"))

(defun ensime-sbt-do-test-quick-dwim ()
  "Execute the sbt `testQuick' command for the project and suite
that corresponds to the current source test file.

If not run from a test source file, then prompt for the project
module, test suite and test command."
  (interactive)
  (ensime-sbt-test-dwim "testQuick"))

(defun ensime-top-level-class-closest-to-point ()
  "Return the name of first class, trait or object enclosing the point,
or (if the point isn't inside a class definition) the class that follows
the point. Return nil if no class can be found."
  ;; TODO use an RPC call instead of this cheesy search
  (cl-labels
      ((inside-string? () (nth 3 (syntax-ppss)))
       (pos-of-top-level-class (&optional last-try)
         (save-excursion
           (save-restriction
             (widen)
             (while (inside-string?)
               (goto-char (1- (point))))
             (let ((top-level-sexp (point)))
               ;; Try to go up a sexp until we get an error
               (condition-case nil
                   (while t
                     (setq top-level-sexp (point))
                     (backward-up-list))
                 (error nil))
               (goto-char top-level-sexp)

               (re-search-backward "}\\|\\<object\\>\\|\\<class\\>\\|\\<trait\\>" nil t)
               (let ((class-re
                      (concat "\\<\\(object\\|class\\|trait\\)[ \t\n]+\\("
                              scala-syntax:id-re
                              "\\)")))
                 (if (re-search-forward class-re nil t)
                     (match-beginning 2)
                   (unless last-try
                     (pos-of-top-level-class t)))))))))
    (let ((pos (pos-of-top-level-class)))
      (when pos
        (save-excursion
          (goto-char pos)
          (replace-regexp-in-string
           "\\$$" ""
           (plist-get (ensime-rpc-get-type-at-point) :full-name)))))))

(defun ensime-sbt-do-test-only-dwim ()
  "Execute the sbt `testOnly' command for the project and suite
that corresponds to the current source test file.

If not run from a test source file, then prompt for the project
module, test suite and test command."
  (interactive)
  (let* ((impl-class
          (or (ensime-top-level-class-closest-to-point)
              (return (message "Could not find top-level class"))))
         (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
         (command (concat "test-only" " " cleaned-class)))
    (ensime-sbt-test-dwim command)))


(provide 'ensime-sbt)

;;; ensime-sbt.el ends here
