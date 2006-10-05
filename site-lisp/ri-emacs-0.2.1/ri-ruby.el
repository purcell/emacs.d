;;;; ri-ruby.el emacs wrapper around ri
;;
;; Author: Kristof Bastiaensen <kristof@vleeuwen.org>
;;
;;
;;    Copyright (C) 2004,2006 Kristof Bastiaensen
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;----------------------------------------------------------------------
;;
;;
;;  Installing:
;;  ===========
;;
;;  add the following to your init.el, replacing the filenames with
;;  their correct locations:
;;
;;  (setq ri-ruby-script "/home/kristof/.xemacs/ri-emacs.rb")
;;  (autoload 'ri "/home/kristof/.xemacs/ri-ruby.el" nil t)
;;
;;  You may want to bind the ri command to a key.
;;  For example to bind it to F1 in ruby-mode:
;;  Method/class completion is also available.
;;
;;   (add-hook 'ruby-mode-hook (lambda ()
;;                               (local-set-key 'f1 'ri)
;;                               (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                               (local-set-key 'f4 'ri-ruby-show-args)
;;                               ))
;;
;;
;;  Usage:
;;  ======
;;  M-x ri
;;
;;  M-Tab for completion
;;  
;;  Bugs:
;;  ====
;;
;;  * The first time you give the ri command on xemacs, it may give
;;    strange behaviour in XEmacs.  This is probably due to a
;;    bug in the way XEmacs handles processes under linux.
;;
;;  * It is reported that the ruby-script doesn't work with XEmacs under
;;    MS-Windows.  This is probably a bug in processes in XEmacs.  
;;
;;  Contributors:
;;  =============
;;
;;  rubikitch (http://www.rubyist.net/~rubikitch/):
;;    fixed highlighting under Emacs

(require 'ansi-color)

(defvar ri-ruby-program "ruby"
  "The ruby program name.")

(defvar ri-ruby-script "/home/kristof/.xemacs/ri-emacs.rb"
  "the ruby script to communicate with")

(defvar ri-ruby-process nil
  "The current ri process where emacs is interacting with")

(defvar ri-ruby-history nil
  "The history for ri")

(defvar ri-ruby-process-buffer nil)

(defun ri-ruby-get-process ()
  (cond ((or (null ri-ruby-process)
	     (not (equal (process-status ri-ruby-process) 'run)))
	 (setq ri-ruby-process
	       (start-process "ri-ruby-process"
			      nil
			      ri-ruby-program ri-ruby-script))
	 (process-kill-without-query ri-ruby-process) ;kill when ending emacs
	 (ri-ruby-process-check-ready)))
  ri-ruby-process)

(defun ri-ruby-process-filter-expr (proc str)
  (let ((ansi-color-context nil))
    (save-excursion
      (set-buffer ri-ruby-process-buffer)
      (goto-char (point-max))
      (insert-string (ansi-color-filter-apply str)))))

(defun ri-ruby-process-filter-lines (proc str)
  (save-excursion
    (set-buffer ri-ruby-process-buffer)
    (goto-char (point-max))
    (insert-string (ansi-color-apply str))))

(defvar ri-startup-timeout 60)
(defun ri-ruby-process-check-ready ()
  (let ((ri-ruby-process-buffer (generate-new-buffer  " ri-ruby-output")))
    (unwind-protect
	(save-excursion
	  (set-buffer ri-ruby-process-buffer)
	  (set-process-filter ri-ruby-process 'ri-ruby-process-filter-expr)
	  (ri-ruby-check-process ri-ruby-process-buffer)
	  (accept-process-output ri-ruby-process ri-startup-timeout)
	  (goto-char (point-min))
	  (cond ((not (looking-at "READY.*\n"))
		 (delete-process ri-ruby-process)
		 (error 'io-error "couldn't start ruby script"))))
      (set-process-filter ri-ruby-process t)
      (kill-buffer ri-ruby-process-buffer))))

(defun ri-ruby-check-process (buffer)
  (or (equal (process-status ri-ruby-process) 'run)
      (let ((output (with-current-buffer buffer
                      (buffer-substring (point-min)
                                        (point-max)))))
	(error 'io-error "Process is not running.\n" output))))

(defun ri-ruby-process-get-expr (cmd param)
  (ri-ruby-get-process)
    (let ((ri-ruby-process-buffer (generate-new-buffer  " ri-ruby-output"))
	  (command (concat cmd " " param "\n")))
      (unwind-protect
	  (save-excursion
	    (set-buffer ri-ruby-process-buffer)
	    (set-process-filter ri-ruby-process 'ri-ruby-process-filter-expr)
	    (process-send-string ri-ruby-process command)
	    (ri-ruby-check-process ri-ruby-process-buffer)
	    (while (progn (goto-char (point-min))
			  (not (looking-at ".*\n"))) ;we didn't read a whole line
	      (ri-ruby-check-process ri-ruby-process-buffer)
	      (accept-process-output ri-ruby-process))
	    (goto-char (point-min))
	    (read (buffer-substring (point)
				    (point-at-eol))))
	(set-process-filter ri-ruby-process t)
	(kill-buffer ri-ruby-process-buffer))))

(defun ri-ruby-process-get-lines (cmd param)
  (ri-ruby-get-process)
  (if (equal param "") nil
    (let ((ri-ruby-process-buffer (generate-new-buffer " ri-ruby-output"))
	  (command (concat cmd " " param "\n")))
      (unwind-protect
	  (save-excursion
	    (set-buffer ri-ruby-process-buffer)
	    (set-process-filter ri-ruby-process 'ri-ruby-process-filter-lines)
	    (process-send-string ri-ruby-process command)
	    (ri-ruby-check-process ri-ruby-process-buffer)
	    (while (progn (goto-char (point-max))
			  (goto-char (point-at-bol 0))
			  (not (looking-at "RI_EMACS_END_OF_INFO$")))
	      (ri-ruby-check-process ri-ruby-process-buffer)
	      (accept-process-output ri-ruby-process))
	    (if (bobp) nil
	      (backward-char)
	      (buffer-substring (point-min) (point))))
	(set-process-filter ri-ruby-process t)
	(kill-buffer ri-ruby-process-buffer)))))

(defun ri-ruby-complete-method (str pred type)
  (let* ((cmd (cdr (assoc type '((nil . "TRY_COMPLETION")
				 (t   . "COMPLETE_ALL")
				 (lambda . "LAMBDA")))))
	 (result (ri-ruby-process-get-expr cmd str)))
    (if (and pred (listp result))
	(setq result (mapcar pred result)))
    result))
					       
(defun ri-ruby-read-keyw ()
  (let* ((curr (current-word))
	 (match (ri-ruby-process-get-expr "LAMBDA" curr))
	 (default (if match curr nil))
	 (prompt (concat "method- or classname"
			 (if default (concat " (default " default ")") "")
			 ": "))
	 (keyw (completing-read prompt 'ri-ruby-complete-method
				nil t "" 'ri-ruby-history default))
	 (classes (ri-ruby-process-get-expr "CLASS_LIST" keyw))
	 (class (cond ((null classes) nil)
		      ((null (cdr classes)) (caar classes))
		      (t (completing-read (concat prompt keyw
						  " classname: ")
					  classes nil t)))))
    (list keyw class)))

(defun ri-ruby-method-with-class (meth classes)
  (concat meth " [" (mapconcat 'car classes ", ") "]"))

(defun ri-ruby-complete-symbol ()
  "Completion on ruby-mode."
  (interactive)
  (let* ((curr (current-word))
         (keyw curr)
 	 (classes (ri-ruby-process-get-expr "CLASS_LIST_WITH_FLAG" keyw))
         (completion (try-completion curr 'ri-ruby-complete-method nil)))
    (cond ((eq completion t)
           (message "%s" (ri-ruby-method-with-class curr classes))
           )
	  ((null completion)
	   (message "Can't find completion for \"%s\"" curr)
	   (ding))
	  ((not (string= curr completion))
	   (delete-region (save-excursion (search-backward curr) (point))
                          (point))
	   (insert completion)
           (setq classes (ri-ruby-process-get-expr "CLASS_LIST_WITH_FLAG" completion))
           (message "%s" (ri-ruby-method-with-class completion classes))
           )
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
 	     (display-completion-list
              (all-completions curr 'ri-ruby-complete-method)
             ))
           (message "%s" (ri-ruby-method-with-class completion classes))))))

(defun test-ri-ruby-complete-symbol ()
  "Test of ri-ruby-complete-symbol."
  (interactive)
  (pop-to-buffer "*ruby completion test*")
  (ruby-mode)
  (erase-buffer)
  (goto-char (point-min))
  (insert "prin
object_id
intern
printf
# (kill-process \"ri-ruby-process\")
"))

(defun ri-ruby-show-args ()
  (interactive)
  (let* ((method (current-word))
         (info (ri-ruby-process-get-lines "DISPLAY_ARGS" method)))
    (when info
      (message "%s" info)))
  )

(defun ri (keyw &optional class)
  "Execute `ri'."
  (interactive (ri-ruby-read-keyw))
  (let* ((method (if class (concat class "#" keyw) keyw))
	(info (ri-ruby-process-get-lines "DISPLAY_INFO" method)))
    (cond (info (ri-ruby-show-info method info))
	  ((null class))
	  (t (setq method (concat class "::" keyw))
	     (setq info (ri-ruby-process-get-lines "DISPLAY_INFO" method))
	     (if info (ri-ruby-show-info method info))))))

(cond ((fboundp 'with-displaying-help-buffer) ; for XEmacs
       (defun ri-ruby-show-info (method info) 
	 (with-displaying-help-buffer
	  (lambda () (princ info))
	  (format "ri `%s'" method))))
      (t                                ; for Emacs
       (defun ri-ruby-show-info (method info)
         (let ((b (get-buffer-create (format "ri `%s'" method))))
           (display-buffer b)
           (with-current-buffer b
             (buffer-disable-undo)
             (erase-buffer)
             (insert info)
             (goto-char 1)))
         info)))
