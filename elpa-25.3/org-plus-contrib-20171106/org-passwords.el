;;; org-passwords.el --- org derived mode for managing passwords

;; Author: Jorge A. Alfaro-Murillo <jorge.alfaro-murillo@yale.edu>
;; Created: December 26, 2012
;; Keywords: passwords, password

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for managing your passwords with
;; Org-mode. It is part of org/contrib (see http://orgmode.org/). If
;; you want to contribute with development, or have a problem, do it
;; here: https://bitbucket.org/alfaromurillo/org-passwords.el

;; A basic setup needs to indicate a passwords file, and a dictionary
;; for the random words:

;;   (require 'org-passwords)
;;   (setq org-passwords-file "~/documents/passwords.gpg")
;;   (setq org-passwords-random-words-dictionary "/etc/dictionaries-common/words")

;; Basic usage:

;;   `M-x org-passwords' opens the passwords file in
;;   `org-passwords-mode'.

;;   `M-x org-passwords-generate-password' generates a random string
;;   of numbers, lowercase letters and uppercase letters.

;;   `C-u M-x org-passwords-generate-password' generates a random
;;   string of numbers, lowercase letters, uppercase letters and
;;   symbols.

;;   `M-x org-passwords-random-words' concatenates random words from
;;   the dictionary defined by `org-passwords-random-words-dictionary'
;;   into a string, each word separated by the string defined in
;;   `org-passwords-random-words-separator'.

;;   `C-u M-x org-passwords-random-words' does the same as above, and
;;   also makes substitutions according to
;;   `org-passwords-random-words-substitutions'.

;; It is also useful to set up keybindings for the functions
;; `org-passwords-copy-username', `org-passwords-copy-password' and
;; `org-passwords-open-url' in the `org-passwords-mode', to easily
;; make the passwords and usernames available to the facility for
;; pasting text of the window system (clipboard on X and MS-Windows,
;; pasteboard on Nextstep/Mac OS, etc.), without inserting them in the
;; kill-ring. You can set for example:

;;   (eval-after-load "org-passwords"
;;     '(progn
;;        (define-key org-passwords-mode-map
;; 	 (kbd "C-c u")
;; 	 'org-passwords-copy-username)
;;        (define-key org-passwords-mode-map
;; 	 (kbd "C-c p")
;; 	 'org-passwords-copy-password)
;; 	 (kbd "C-c o")
;; 	 'org-passwords-open-url)))

;; Finally, to enter new passwords, you can use `org-capture' and a
;; minimal template like:

;;   ("p" "password" entry (file "~/documents/passwords.gpg")
;;    "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p")

;; When asked for the password you can then call either
;; `org-passwords-generate-password' or `org-passwords-random-words'.
;; Be sure to enable recursive minibuffers to call those functions
;; from the minibuffer:

;;   (setq enable-recursive-minibuffers t)

;;; Code:

(require 'org)

;;;###autoload
(define-derived-mode org-passwords-mode org-mode
  "org-passwords-mode"
  "Mode for storing passwords"
  nil)

(defgroup org-passwords nil
  "Options for password management."
  :group 'org)

(defcustom org-passwords-password-property "PASSWORD"
  "Name of the property for password entry."
  :type 'string
  :group 'org-passwords)

(defcustom org-passwords-username-property "USERNAME"
  "Name of the property for user name entry."
  :type 'string
  :group 'org-passwords)

(defcustom org-passwords-url-property "URL"
  "Name of the property for URL entry."
  :type 'string
  :group 'org-passwords)

(defcustom org-passwords-file nil
  "Default file name for the file that contains the passwords."
  :type 'file
  :group 'org-passwords)

(defcustom org-passwords-time-opened "1 min"
  "Time that the password file will remain open. It has to be a
string, a number followed by units."
  :type 'str
  :group 'org-passwords)

(defcustom org-passwords-default-password-size "20"
  "Default number of characters to use in
org-passwords-generate-password. It has to be a string."
  :type 'str
  :group 'org-passwords)

(defcustom org-passwords-random-words-dictionary nil
  "Default file name for the file that contains a dictionary of
words for `org-passwords-random-words'. Each non-empty line in
the file is considered a word."
  :type 'file
  :group 'org-passwords)

(defcustom org-passwords-default-random-words-number "5"
  "Default number of words to use in org-passwords-random-words.
It has to be a string."
  :type 'str
  :group 'org-passwords)

(defvar org-passwords-random-words-separator "-"
  "A string to separate words in `org-passwords-random-words'.")

(defvar org-passwords-random-words-substitutions
  '(("a" . "@")
    ("e" . "3")
    ("o" . "0"))
"A list of substitutions to be made with
`org-passwords-random-words' if it is called with
`universal-argument'. Each element is pair of
strings (SUBSTITUTE-THIS . BY-THIS).")

(defun org-passwords-copy-password ()
  "Makes the password available to other programs. Puts the
password of the entry at the location of the cursor in the
facility for pasting text of the window system (clipboard on X
and MS-Windows, pasteboard on Nextstep/Mac OS, etc.), without
putting it in the kill ring."
  (interactive)
  (funcall interprogram-cut-function
           (org-entry-get (point)
			  org-passwords-password-property)))

(defun org-passwords-copy-username ()
  "Makes the password available to other programs. Puts the
username of the entry at the location of the cursor in the
facility for pasting text of the window system (clipboard on X
and MS-Windows, pasteboard on Nextstep/Mac OS, etc.), without
putting it in the kill ring."
  (interactive)
  (funcall interprogram-cut-function
           (org-entry-get (point)
			  org-passwords-username-property
			  t)))

(defun org-passwords-open-url ()
  "Browse the URL associated with the entry at the location of
the cursor."
  (interactive)
  (browse-url (org-entry-get (point)
			    org-passwords-url-property
			    t)))

;;;###autoload
(defun org-passwords (&optional arg)
  "Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.
"
  (interactive "P")
  (if org-passwords-file
      (progn
	(add-to-list 'auto-mode-alist
		     (cons
		      (regexp-quote
		       (expand-file-name org-passwords-file))
		      'org-passwords-mode))
	(if (equal arg '(4))
	    (find-file-read-only org-passwords-file)
	  (if (equal arg '(16))
	      (find-file org-passwords-file)
	    (progn
	      (find-file-read-only org-passwords-file)
	      (org-passwords-set-up-kill-password-buffer)))))
    (minibuffer-message "No default password file defined. Set the variable `org-password-file'.")))

(defun org-passwords-set-up-kill-password-buffer ()
  (run-at-time org-passwords-time-opened
	       nil
	       '(lambda ()
		  (if (get-file-buffer org-passwords-file)
		      (kill-buffer
		       (get-file-buffer org-passwords-file))))))

;;; Password generator

;; Set random number seed from current time and pid. Otherwise
;; `random' gives the same results every time emacs restarts.
(random t)

(defun org-passwords-generate-password (arg)
  "Ask a number of characters and insert a password of that size.
Password has a random string of numbers, lowercase letters, and
uppercase letters.  Argument ARG include symbols."
  (interactive "P")
  (let ((number-of-chars
	 (read-from-minibuffer
	  (concat "Number of characters (default "
		  org-passwords-default-password-size
		  "): ")
	  nil
	  nil
	  t
	  nil
	  org-passwords-default-password-size)))
    (if arg
	(insert (org-passwords-generate-password-with-symbols "" number-of-chars))
	(insert (org-passwords-generate-password-without-symbols "" number-of-chars)))))

(defun org-passwords-generate-password-with-symbols (previous-string nums-of-chars)
  "Return a string consisting of PREVIOUS-STRING and
NUMS-OF-CHARS random characters."
  (if (eq nums-of-chars 0) previous-string
    (org-passwords-generate-password-with-symbols
     (concat previous-string
	     (char-to-string
	      ;; symbols, letters, numbers are from 33 to 126
	      (+ (random (- 127 33)) 33)))
     (1- nums-of-chars))))

(defun org-passwords-generate-password-without-symbols (previous-string nums-of-chars)
  "Return string consisting of PREVIOUS-STRING and NUMS-OF-CHARS
random numbers, lowercase letters, and numbers."
  (if (eq nums-of-chars 0)
      previous-string
      ; There are 10 numbers, 26 lowercase letters and 26 uppercase
      ; letters. 10 + 26 + 26 = 62. The number characters go from 48
      ; to 57, the uppercase letters from 65 to 90, and the lowercase
      ; from 97 to 122. The following makes each equally likely.
      (let ((temp-value (random 62)))
 	(cond ((< temp-value 10)
	       ; If temp-value<10, then add a number
	       (org-passwords-generate-password-without-symbols
		(concat previous-string
			(char-to-string (+ 48 temp-value)))
		(1- nums-of-chars)))
	      ((and (> temp-value 9) (< temp-value 36))
	       ; If 9<temp-value<36, then add an uppercase letter
	       (org-passwords-generate-password-without-symbols
		(concat previous-string
			(char-to-string (+ 65 (- temp-value 10))))
		(1- nums-of-chars)))
	      ((> temp-value 35)
               ; If temp-value>35, then add a lowecase letter
	       (org-passwords-generate-password-without-symbols
		(concat previous-string
			(char-to-string (+ 97 (- temp-value 36))))
		(1- nums-of-chars)))))))

;;; Random words

(defun org-passwords-random-words (arg)
  "Ask for a number of words and inserts a sequence of that many
random words from the list in the file
`org-passwords-random-words-dictionary' separated by
`org-passwords-random-words-separator'. ARG make substitutions in
the words as defined by
`org-passwords-random-words-substitutions'."
  (interactive "P")
  (if org-passwords-random-words-dictionary
      (let ((number-of-words
	     (read-from-minibuffer
	      (concat "Number of words (default "
		      org-passwords-default-random-words-number
		      "): ")
	      nil
	      nil
	      t
	      nil
	      org-passwords-default-random-words-number))
	    (list-of-words
	     (with-temp-buffer
	       (insert-file-contents
		org-passwords-random-words-dictionary)
	       (split-string (buffer-string) "\n" t))))
	(insert
	 (org-passwords-substitute
	  (org-passwords-random-words-attach-number-of-words
	   (nth (random (length list-of-words))
		list-of-words)
	   (1- number-of-words)
	   list-of-words
	   org-passwords-random-words-separator)
	  (if arg
	      org-passwords-random-words-substitutions
	    nil))))
    (minibuffer-message
     "No default dictionary file defined. Set the variable `org-passwords-random-words-dictionary'.")))

(defun org-passwords-random-words-attach-number-of-words
  (previous-string number-of-words list-of-words separator)
  "Returns a string consisting of PREVIOUS-STRING followed by a
succession of NUMBER-OF-WORDS random words from the list LIST-OF-WORDS
separated SEPARATOR."
  (if (eq number-of-words 0)
      previous-string
      (org-passwords-random-words-attach-number-of-words
       (concat previous-string
	       separator
	       (nth (random (length list-of-words)) list-of-words))
       (1- number-of-words)
       list-of-words
       separator)))

(defun org-passwords-substitute (string-to-change list-of-substitutions)
  "Substitutes each appearence in STRING-TO-CHANGE of the `car' of
each element of LIST-OF-SUBSTITUTIONS by the `cdr' of that
element. For example:
 (org-passwords-substitute \"ab\" \'((\"a\" . \"b\") (\"b\" . \"c\")))
       => \"bc\"
Substitutions are made in order of the list, so for example:
 (org-passwords-substitute \"ab\" \'((\"ab\" . \"c\") (\"b\" . \"d\")))
       => \"c\""
  (if list-of-substitutions
      (concat (org-passwords-concat-this-with-string
	       (cdar list-of-substitutions)
	       (mapcar (lambda (x)
			 (org-passwords-substitute
			  x
			  (cdr list-of-substitutions)))
		       (split-string string-to-change
				     (caar list-of-substitutions)))))
    string-to-change))

(defun org-passwords-concat-this-with-string (this list-of-strings)
  "Put the string THIS in between every string in LIST-OF-STRINGS. For example:
 (org-passwords-concat-this-with-string \"Here\" \'(\"First\" \"Second\" \"Third\"))
      => \"FirstHereSencondHereThird\""
  (if (cdr list-of-strings)
      (concat (car list-of-strings)
	      this
	      (org-passwords-concat-this-with-string
	       this
	       (cdr list-of-strings)))
    (car list-of-strings)))

(provide 'org-passwords)

;;; org-passwords.el ends here
