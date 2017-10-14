;;; yari.el --- Yet Another RI interface for Emacs

;; Copyright (C) 2010-2013  Aleksei Gusev, Jose Pablo Barrantes, Perry Smith

;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: 24 Apr 2010
;; Version: 0.8
;; Package-Version: 20151127.2339
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
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

;; yari.el provides an Emacs frontend to Ruby's `ri' documentation
;; tool. It offers lookup and completion.
;;
;; This version will load all completion targets the first time it's
;; invoked. This can be a significant startup time, but it will not
;; have to look up anything after that point.
;;
;; This library tries to be compatible with any version of `rdoc' gem.
;; Self-testing covers all versions from 1.0.1 to 2.5.8 (current).
;;
;; The main function you should use as interface to ri is M-x yari
;; (yari-helm is a variant using Helm input framework). I recommend to
;; bind it on some key local when you are ruby-mode. Here is the example:
;;
;; (defun ri-bind-key ()
;;   (local-set-key [f1] 'yari))
;;
;;  or
;;
;; (defun ri-bind-key ()
;;   (local-set-key [f1] 'yari-helm))
;;
;; (add-hook 'ruby-mode-hook 'ri-bind-key)
;;
;; You can use C-u M-x yari to reload all completion targets.

;;; Code:

(eval-when-compile (require 'cl))

(require 'thingatpt)
(require 'ansi-color)

(defgroup yari nil
  "Yet Another Ri Interface."
  :group 'programming)

(defcustom yari-mode-hook nil
  "Hooks to run when invoking yari-mode."
  :group 'yari
  :type 'hook)

(defcustom yari-ri-program-name "ri"
  "This constant defines how yari.el will find ri, e.g. `ri1.9'.")

(defcustom yari-ruby-program-name "ruby"
  "This constant defines how yari.el will find ruby, e.g. `ruby1.9'.")

(defvar yari-anything-source-ri-pages
  '((name . "RI documentation")
    (candidates . (lambda () (yari-ruby-obarray)))
    (action  ("Show with Yari" . yari))
    (candidate-number-limit . 300)
    (requires-pattern . 2)
    "Source for completing RI documentation."))

;;;###autoload
(defun yari-anything (&optional rehash)
  (interactive (list current-prefix-arg))
  (when current-prefix-arg (yari-ruby-obarray rehash))
  (anything 'yari-anything-source-ri-pages (yari-symbol-at-point)))

(defvar yari-helm-ri-pages
  `((name . "RI documentation")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'local
               (yari-ruby-obarray nil t))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (action . yari))
  "Source for completing RI documentation.")

;;;###autoload
(defun yari-helm ()
  (interactive)
  (helm :sources '(yari-helm-ri-pages)
        :buffer "*yari-helm*"
        :prompt "yari: "
        :input (yari-symbol-at-point)))

;;;###autoload
(defun yari (&optional ri-topic rehash)
  "Look up Ruby documentation."
  (interactive (list nil current-prefix-arg))
  (let ((completing-read-func (if (null ido-mode)
                                  'completing-read
				'ido-completing-read)))
    (setq ri-topic (or ri-topic
                       (funcall completing-read-func
				"yari: "
				(yari-ruby-obarray rehash)
				nil
				t
				(yari-symbol-at-point)))))
  (let ((yari-buffer-name (format "*yari %s*" ri-topic)))
    (unless (get-buffer yari-buffer-name)
      (let ((yari-buffer (get-buffer-create yari-buffer-name))
            (ri-content (yari-ri-lookup ri-topic)))
        (with-current-buffer yari-buffer
          (erase-buffer)
          (insert ri-content)
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-min))
          (yari-mode))))
    (display-buffer yari-buffer-name)))

(defun yari-symbol-at-point ()
  ;; TODO: make this smart about class/module at point
  (let ((yari-symbol (symbol-at-point)))
    (if yari-symbol
        (symbol-name yari-symbol)
      "")))

(defvar yari-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q")    'quit-window)
    (define-key map (kbd "SPC")  'scroll-up)
    (define-key map (kbd "\C-?") 'scroll-down)
    map))

(defun yari-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (use-local-map yari-mode-map)
  (setq mode-name "yari")
  (setq major-mode 'yari-mode)
  (yari-find-buttons)
  (goto-char (point-min))
  (setq buffer-read-only t)
  (run-hooks 'yari-mode-hook))

(defmacro when-ert-loaded (&rest body)
  `(dont-compile
     (when (featurep 'ert)
       ,@body)))

(when-ert-loaded
 (defmacro yari-with-ruby-obarray-cache-mock (cache-mock &rest body)
   (declare (indent 1))
   `(unwind-protect
	(let* ((,cache-mock '("NotExistClassInRuby" "NotExistClassInRuby#mmmmm"))
               (yari-ruby-obarray-cache ,cache-mock))
          ,@body))))


(defun yari-ri-lookup (name)
  "Return content from ri for NAME."
  (assert (member name (yari-ruby-obarray)) nil
          (format "%s is unknown symbol to RI." name))
  (shell-command-to-string
   (format (concat yari-ri-program-name " -T -f ansi %s")
           (shell-quote-argument name))))

(when-ert-loaded
 (ert-deftest yari-test-ri-lookup-should-generate-error ()
   (should-error
    (yari-ri-lookup "AbSoLuTttelyImposibleThisexists#bbb?")))

 (ert-deftest yari-test-ri-lookup-should-have-content ()
   (should (string-match "RDoc" (yari-ri-lookup "RDoc"))))

 (ert-deftest yari-test-ri-lookup ()
   (should (yari-ri-lookup "RDoc"))))


(defvar yari-ruby-obarray-cache nil
  "Variable to store all possible completions of RI pages.")

(defun yari-ruby-obarray (&optional rehash do-not-split)
  "Build collection of classes and methods for completions."
  (let ((output (yari-ruby-methods-from-ri rehash)))
    (if do-not-split
        output
      (split-string output))))

(when-ert-loaded
 (ert-deftest yari-test-ruby-obarray-should-rehash ()
   (yari-with-ruby-obarray-cache-mock
    cache-mock
    (yari-ruby-obarray t)
    (should-not (equal yari-ruby-obarray-cache cache-mock))))



 (ert-deftest yari-test-ruby-obarray-should-set-cache ()
   (let ((yari-ruby-obarray-cache))
     (yari-ruby-obarray)
     (should yari-ruby-obarray-cache)))

 (ert-deftest yari-test-ruby-obarray-for-class-first-level ()
   (should (member "RDoc" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-class-deep-level ()
   (should (member "RDoc::TopLevel" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-class-method ()
   (should (member "RDoc::TopLevel::new" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-for-object-method ()
   (should (member "RDoc::TopLevel#full_name" (yari-ruby-obarray)))))

(defun yari-ruby-methods-from-ri (rehash)
  "Return string with all ruby methods known to ri command."
  (if (or rehash (null yari-ruby-obarray-cache))
    (setq yari-ruby-obarray-cache
          (yari-eval-ruby-code
            (cond
             ((yari-ri-version-at-least "2.5")
               "require 'rdoc/ri/driver';       \
                driver  = RDoc::RI::Driver.new(RDoc::RI::Driver.process_args([])); \
                puts driver.list_known_classes; \
                puts driver.list_methods_matching('.')")
              ((yari-ri-version-at-least "2.2.0")
               "require 'rdoc/ri/reader'; \
                require 'rdoc/ri/cache';  \
                require 'rdoc/ri/paths';  \
                all_paths = RDoc::RI::Paths.path(true,true,true,true); \
                cache  = RDoc::RI::Cache.new(all_paths); \
                reader = RDoc::RI::Reader.new(cache);    \
                puts reader.all_names")
              ((yari-ri-version-at-least "2.0.0")
               "require 'rdoc/ri/driver';            \
                driver  = RDoc::RI::Driver.new;      \
                puts driver.class_cache.keys;        \
                methods = driver.select_methods(//); \
                puts methods.map{|m| m['full_name']}")
              ((yari-ri-version-at-least "1.0.0")
               "require 'rdoc/ri/ri_reader'; \
                require 'rdoc/ri/ri_cache';  \
                require 'rdoc/ri/ri_paths'; \
                all_paths = RI::Paths.path(true,true,true,true); \
                cache = RI::RiCache.new(all_paths); \
                reader = RI::RiReader.new(cache);    \
                puts reader.all_names;")
              (t
               (error "Unknown Ri version.")))))
     yari-ruby-obarray-cache))

(when-ert-loaded
 (ert-deftest yari-test-ruby-obarray-should-use-cache ()
   (yari-with-ruby-obarray-cache-mock
       cache-mock
     (yari-ruby-methods-from-ri nil)
     (should (equal yari-ruby-obarray-cache cache-mock)))))

(defun yari-eval-ruby-code (ruby-code)
  "Return stdout from ruby -rrubyges -eRUBY-CODE."
  (shell-command-to-string (format "%s -rrubygems -e\"%s\"" yari-ruby-program-name ruby-code)))

(when-ert-loaded
 (ert-deftest yari-test-ruby-obarray-filter-standard-warning ()
   (should-not (member ". not found, maybe you meant:"
                           (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-filter-updating-class-cache ()
   (should-not (let ((case-fold-search nil)
                         (bad-thing-found-p))
                     (mapc '(lambda (line)
                              (when (string-match "Updating class cache" line)
				(setq bad-thing-found-p t)))
                           (yari-ruby-obarray))
                     bad-thing-found-p)))

 (ert-deftest yari-test-ruby-obarray-filter-empty-string ()
   (should-not (member "" (yari-ruby-obarray))))

 (ert-deftest yari-test-ruby-obarray-filter-standard-ruler ()
   (should-not (member "----------------------------------------------"
                           (yari-ruby-obarray)))))

(defun yari-ri-version-at-least (minimum)
  "Detect if RI version at least MINIMUM."
  (let ((ri-version (yari-get-ri-version)))
    (or (string< minimum ri-version) (string= minimum ri-version))))

(defun yari-get-ri-version (&optional version)
  "Return list of version parts or RI."
  (let* ((raw-version-output
          (or version (shell-command-to-string
                       (concat yari-ri-program-name " --version"))))
         (raw-version (cadr (split-string raw-version-output))))
    (string-match "v?\\(.*\\)" raw-version)
    (match-string 1 raw-version)))

(when-ert-loaded
 (ert-deftest yari-test-get-ri-version-for-1.0.0 ()
   (should (equal "1.0.1" (yari-get-ri-version "ri v1.0.1 - 20041108"))))
 (ert-deftest yari-test-get-ri-version-for-2.5.6 ()
   (should (equal "2.5.6" (yari-get-ri-version "ri 2.5.6")))))

;;
;; Buttons for method/class names in yari buffer.
;;
(define-button-type 'yari-method
  'help-echo "mouse-2, RET: Display yari help on this method"
  'follow-link t
  'action (lambda (button)
	    (yari (button-get button 'yari-method))))

(defcustom yari-emacs-method-face
  'underline
  "*Face for method name in yari output, or nil for none."
  :group 'yari
  :type 'face)

(defvar yari-debug t)			;set to t when debugging

(defun yari-find-buttons ( )
  (goto-char (point-min))
  ;; The types of pages I know of so far is an instance method or a
  ;; class method.  In those cases, we find the class in the first
  ;; line and make a button for it.  The other searches are going to
  ;; fail.
  ;;
  ;; The other type of page I know of is a Module or a Class (which I
  ;; treat the same so far).  In this case, I want to make a button
  ;; for the subclass so we can easily walk up the tree.  I also need
  ;; to save off the original class or module.
  ;;
  ;; For Class and Module pages, we continue to scan down the page
  ;; looking for Includes, Class methods: and Instance Methods making
  ;; buttons for each of the entries under each of those sections.
  ;;
  ;; For the class and instance methods, the class or module that the
  ;; page is displaying has to be prepended to the method name with
  ;; either a "::" or a "#" in between.
  ;;
  ;; Ruby 1.9 formats the page different.  The first line is all -'s.
  ;; The second line has what use to be at the end of the first line
  (let* ((bol (progn
		(if (looking-at "^-+$")
		    (forward-line 1))
		(point)))
	 (eol (progn (forward-line 1) (point)))
	 (includes-start (re-search-forward "^Includes:" nil t))
	 (constant-start  (re-search-forward "^Constants:" nil t))
	 (class-start  (re-search-forward "^Class methods:" nil t))
	 (instance-start (re-search-forward "^Instance methods:" nil t))
	 (page-end (point-max))
	 (class nil)
	 (parent-class nil)
	 (base-class nil)
	 (method nil)
	 search-end)
    (goto-char bol)
    (if (re-search-forward
	 " ?\\(\\(Module\\|Class\\): \\)?\\(\\(\\(\\([^#: ]+\\)\\(::\\|#\\)\\)*\\)\\([^: \t\r\n]+\\)\\)\\( < \\(\\S +\\)\\)?\\s *$"
	 eol t)
	(let ((match-string0 (match-string 0))
	      (match-string1 (match-string 1))
	      (match-string2 (match-string 2))
	      (match-string3 (match-string 3))
	      (match-string4 (match-string 4))
	      (match-string5 (match-string 5))
	      (match-string6 (match-string 6))
	      (match-string7 (match-string 7))
	      (match-string8 (match-string 8))
	      (match-string9 (match-string 9))
	      (match-string10 (match-string 10))
	      include-pat
	      constant-pat
	      class-pat
	      instance-pat)
	  (if t				;t for debugging
	      (progn
		;; "Class: " or "Module: " if present.  In Ruby 1.9.2
		;; using RDoc 2.5.8, it is not present
		(and yari-debug (message (format "match  1: '%s'" match-string1)))
		;; "Class" or "Module" if present
		(and yari-debug (message (format "match  2: '%s'" match-string2)))
		;; entire class, module, or method string
		(and yari-debug (message (format "match  3: '%s'" match-string3)))
		;; #3 with final segment removed but the # or :: still
		;; attached
		(and yari-debug (message (format "match  4: '%s'" match-string4)))
		;; The piece of the A::B::C:: string.  This is not
		;; useful that I can see.
		(and yari-debug (message (format "match  5: '%s'" match-string5)))
		;; #4 but with the :: or # removed
		(and yari-debug (message (format "match  6: '%s'" match-string6)))
		;; The final :: or #.  If it is # then we know we have
		;; an instance method.  If it is :: we can have a
		;; Module or Class or a class method.  A class method
		;; will be noticed by starting with something other
		;; than an upper case letter.
		;; (This still needs to be implemented)
		(and yari-debug (message (format "match  7: '%s'" match-string7)))
		;; The method name if a method was looked up.  If a
		;; class or module was looked up, this is just the
		;; final segment of what was looked up.
		(and yari-debug (message (format "match  8: '%s'" match-string8)))
		;; "< base class" if present
		(and yari-debug (message (format "match  9: '%s'" match-string9)))
		;; "base class" if present
		(and yari-debug (message (format "match 10: '%s'" match-string10)))))
	  ;; We have a module or a class if match-string 7 is
	  ;; null... i.e. we have just "IO" for example.  We know we
	  ;; have a method if match-string 7 is "#".  If match-string
	  ;; 7 is "::" we have to check to see if match-string 8
	  ;; starts with an upper case letter.  Sigh....
	  (save-match-data
	    (if (or (null match-string7)
		    (and (string= match-string7 "::")
			 (string-match "^[A-Z]" match-string8)))
		(progn
		  (and yari-debug (message "have module or class"))
		  (setq class match-string3))
	      (and yari-debug (message "have a method"))
	      (setq method match-string8)))
	  ;; If this page has Class:, we use one type of pattern,
	  ;; otherwise, we use a differnt pattern
	  (if match-string2
	      (setq include-pat "\\s +\\([^, \n\r\t]+\\)\\(,\\|\\s *$\\)"
		    constant-pat nil
		    class-pat "\\s +\\([^, \n\r\t]+\\)\\(,\\|\\s *$\\)"
		    instance-pat "\\s +\\([^, \n\r\t]+\\)\\(,\\|\\s *$\\)")
	    (setq include-pat "^\\s +\\([^-,. \n\r\t]+\\)\\s *\n"
		  constant-pat "^\\s +\\([^-:,. \n\r\t]+\\):?\\s *\n"
		  class-pat "^\\s +\\([^-,. \n\r\t]+\\)\\s *\n"
		  instance-pat "^\\s +\\([^-,. \n\r\t]+\\)\\s *\n"))

	  ;; Icky but we need to trim off the last :: or # of the entire class name.
	  (if (< (match-beginning 4) (match-end 4))
	      (setq parent-class (buffer-substring (match-beginning 4)
						   (match-beginning 7))))
	  (setq base-class match-string10)
	  (and yari-debug (message (format "base-class %s" base-class)))
	  (and yari-debug (message (format "parent-class %s" parent-class)))
	  ;; Make a button for the parent class if any
	  (if (< (match-beginning 4) (match-end 4))
	      (make-button (match-beginning 4)
			   (match-beginning 7)
			   'type 'yari-method
			   'face yari-emacs-method-face
			   'yari-method parent-class))
	  ;; Make a button for the base class if any
	  (if base-class
	      (make-button (match-beginning 10)
			   (match-end 10)
			   'type 'yari-method
			   'face yari-emacs-method-face
			   'yari-method base-class))
	  ;; If these match, then it must be a Module or a Class.  So
	  ;; use the class as the containing class or module
	  ;; name.
	  (if includes-start
	      (progn
		(goto-char (or constant-start class-start instance-start page-end))
		(forward-line 0)
		(setq search-end (point))
		(goto-char includes-start)
		(while (re-search-forward include-pat search-end t)
		  (make-button (match-beginning 1)
			       (match-end 1)
			       'type 'yari-method
			       'face yari-emacs-method-face
			       'yari-method (match-string 1)))))
	  (if (and constant-start constant-pat)
	      (progn
		(goto-char (or class-start instance-start page-end))
		(forward-line 0)
		(setq search-end (point))
		(goto-char constant-start)
		(while (re-search-forward constant-pat search-end t)
		  (make-button (match-beginning 1)
			       (match-end 1)
			       'type 'yari-method
			       'face yari-emacs-method-face
			       'yari-method  (concat class "::" (match-string 1))))))
	  (if class-start
	      (progn
		(goto-char (or instance-start page-end))
		(forward-line 0)
		(setq search-end (point))
		(goto-char class-start)
		(while (re-search-forward class-pat search-end t)
		  (make-button (match-beginning 1)
			       (match-end 1)
			       'type 'yari-method
			       'face yari-emacs-method-face
			       'yari-method  (concat class "::" (match-string 1))))))
	  (if instance-start
	      (progn
		(goto-char instance-start)
		(while (re-search-forward instance-pat nil t)
		  (make-button (match-beginning 1)
			       (match-end 1)
			       'type 'yari-method
			       'face yari-emacs-method-face
			       'yari-method (concat class "#" (match-string 1)))))))
      (and yari-debug (message "total miss")))))

(provide 'yari)
;;; yari.el ends here
