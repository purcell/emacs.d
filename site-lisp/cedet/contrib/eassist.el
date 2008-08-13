;;; eassist.el --- EmacsAssist, C/C++/Java/Python/ELisp method/function navigator.

;; Copyright (C) 2006, 2007 Anton V. Belyaev
;; Author: Anton V. Belyaev <anton.belyaev at the gmail.com>

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Version: 0.9
;; CEDET CVS Version: $Id: eassist.el,v 1.5 2008/02/23 20:35:54 kpoxman Exp $

;; Compatibility: Emacs 22 or 23, CEDET 1.0pre4

;;; Commentary:

;; Contains some useful functions features for C/C++ developers similar to
;; those from VisualAssist.  Remember that convenient M-o, M-g and M-m?

;; 1) Method navigation.
;;    When eassist-list-methods called when c/c++ body file buffer is active
;;    a new buffer is shown, containing list of methods and functions in the
;;    format: return type, class, method name.  You can select the method
;;    moving to its line and press ENTER to jump to the method.  You also can
;;    type a string in the buffer and method list will be reduced to those
;;    which contain the string as a substring.  Nice highlight is implemented.
;;    This function is recommended to be bound to M-m in c-mode.

;; 2) Header <-> Body file switch.
;;    You can easily switch between body (c, cpp, cc...) and its corresponding
;;    header file (h, hpp...) using eassist-switch-h-cpp.  The counterpart file
;;    is first searched in opened buffers and if there is no match the file is
;;    searched in the same directory.  You can adjust body to header correspondence
;;    customizing eassist-header-switches variable.
;;    This function is recommended to be bound to M-o in c-mode.

;; EmacsAssist uses Semantic (http://cedet.sourceforge.net/semantic.shtml)
;; EmacsAssist is a part of CEDET project (current CVS version of CEDET contains
;; EmacsAssist)
;; EmacsAssist works with current (22) and development (23) versions of Emacs and
;; does not work with version 21.
;; EmacsAssist works with CEDET 1.0pre4 and subsequent CVS versions of CEDET.

;; EmacsAssist has a page at Emacs Wiki, where you can always find the latest
;; version: http://www.emacswiki.org/cgi-bin/wiki/EAssist

;; Usage:

;; 1) Install CEDET package for Emacs (if you don't have CEDET already).
;; 2) Add convenient keymaps for fast EmacsAssist calls in c-mode and (or) python-mode
;;    and for lisp:
;;
;;    (defun my-c-mode-common-hook ()
;;      (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
;;      (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
;;    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;
;;    (defun my-python-mode-hook ()
;;      (define-key python-mode-map (kbd "M-m") 'eassist-list-methods))
;;    (add-hook 'python-mode-hook 'my-python-mode-hook)
;;
;;    (define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)
;;
;; 3) Open any C++ file with class definition, press M-m.  Try to type
;;    any method name.
;; 4) Open any .cpp file.  Press M-o.  If there is .h or .hpp file in the
;;    same folder, it will be opened.

;;; Changelog:

;; 27 mar 2006 -- v0.1 Initial version created.
;; 29 mar 2006 -- v0.2 Code is more readable now.
;;                     Thanks to Thien-Thi Nguyen for code review!
;; 17 apr 2006 -- v0.3 Added Java and Python support. Coloring based on faces.
;;                     Multiple string matching.
;; 12 sep 2006 -- v0.4 Fixed ELisp code handling. Reduced string matching to function name only.
;; 23 feb 2007 -- v0.5 Added (require 'semantic) to fix possible issues.
;;                     Thanks to Damien Deville for the patch.
;; 13 mar 2007 -- v0.6 Added documentation to functions.
;;                     Thanks to Eric Ludlam for CHECKDOC tool suggestion.
;; 23 jun 2007 -- v0.7 EAssist is now a part of CEDET project.
;;                     Added autoload cookies for some vars and funs.
;; 29 aug 2007 -- v0.8 "M-o" function now tries first to use already opened buffers
;;                     and if there are no counterparts, tries to search them in the
;;                     current directory.
;;                     Thanks to Alekseenko Dimitry for great feature suggestion.
;; 23 feb 2008 -- v0.9 "M-m" buffer comes up with current function highlighted.
;;                     Thanks to Christoph Conrad for great suggestions and patches.

;;; Code:

(require 'semantic)

;; ================================== My STRING utils ========================
(defun eassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun eassist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))
;; ================================== My STRING utils end ====================

;; ================================== CPP-H switch ===========================
;;;###autoload
(defvar eassist-header-switches '(("h" . ("cpp" "cc" "c"))
			     ("hpp" . ("cpp" "cc"))
			     ("cpp" . ("h" "hpp"))
			     ("c" . ("h"))
			     ("C" . ("H"))
			     ("H" . ("C" "CPP" "CC"))
			     ("cc" . ("h" "hpp")))
"This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun eassist-switch-h-cpp ()
  "Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (eassist-string-without-last (buffer-name) (length ext)))
         (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
                 when (bufferp (get-buffer b)) return (switch-to-buffer b))
           (loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c)))
        (message "There is no corresponding pair (header or body) file.")))
     (t
      (message "It is not a header or body file! See eassist-header-switches variable.")))))
;; ================================== CPP-H switch end =========================

;; ================================== Method navigator =========================
(defvar eassist-current-tag nil
  "Current Semantic tag in source buffer.")
(defvar eassist-buffer nil
  "Buffer used to selecting tags in EAssist.")
(defvar eassist-names-column nil
  "Column used when selecting tags in EAssist.")
(defvar eassist-methods nil
  "Collection of methods used when searching for current selection.")
(defvar eassist-actual-methods nil
  "Collection of actual methods used when searching for current selection.")
(defvar eassist-search-string nil
  "The current search string during a search.")
(defvar eassist-overlays nil
  "List of active overlays.")

(defun eassist-function-tags ()
  "Return all function tags from the current buffer using Semantic API.
The function first gets all toplevel function tags from the current buffer.
Then it searches for all toplevel type tags and gets all function tags that
are children to toplevel type tags.  Secondlevel function (member) tags are
annotated (without side effect) with :parent attribute to have the same
structure as toplevel function tags."
  (nconc
   ;; for C++/C
   (semantic-find-tags-by-class 'function (semantic-something-to-tag-table eassist-buffer))
   ;; for Java and Python: getting classes and then methods for each class.
   ;; Adding parent property for each method, beacause semantic does not provide parents for
   ;; methods which are inside body of the class. This is true for Java class methods,
   ;; for C++ header definitions and for Python class methods.
   (mapcan
    (lambda (type)
      (mapcar
       (lambda (tag) (semantic-tag-put-attribute-no-side-effect tag :parent (semantic-tag-name type)))
       (semantic-find-tags-by-class 'function (semantic-tag-type-members type))))
    (semantic-find-tags-by-class 'type (semantic-something-to-tag-table eassist-buffer)))))

(defun eassist-car-if-list (thing)
  "Return car of THING if it is a list or THING itself, if not."
  (cond ((listp thing) (car thing))
	(t thing)))

(defun eassist-function-string-triplet (f)
  "Return a list of three strings, representing type, parent and name of tag F."
  (list
   (eassist-car-if-list (semantic-tag-type f))
   (semantic-tag-function-parent f)
   (semantic-tag-name f)))

(defun eassist-format-triplets (f)
  "Return a list of formatted (whitespaces, faces, delimeters) methods/function.
F - list of triplets of tag type, parent and name."
  (let ((return-width (reduce 'max (mapcar 'length (mapcar 'car f)) :initial-value 0))
        (class-width (reduce 'max (mapcar 'length (mapcar 'cadr f)) :initial-value 0))
        (name-width (reduce 'max (mapcar 'length (mapcar 'caddr f)) :initial-value 0)))
    (setq eassist-names-column (+ return-width class-width 4))
    (mapcar
     (lambda (tri)
       (let ((retrn (car tri))
             (class (cadr tri))
             (name (caddr tri)))
         (setq retrn (if retrn (propertize retrn 'face 'font-lock-type-face) ""))
         (if class
             (setq class (propertize class 'face 'font-lock-type-face)))
         (setq name (propertize name 'face 'font-lock-function-name-face))
         (cond
          (class (format (format "%%%ds  %%%ds :: %%s\n" return-width class-width) retrn class name))
          (t     (format (format "%%%ds  %%%ds    %%s\n" return-width class-width) retrn ""    name)))))
     f)))

;;;###autoload
(defun eassist-list-methods ()
  "Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey."
  (interactive)
  (setq eassist-buffer (current-buffer))
  (setq eassist-current-tag (semantic-current-tag))
  (switch-to-buffer (get-buffer-create (concat (buffer-name (current-buffer)) " method list")) t)
  (eassist-mode))

(defun eassist-jump-to-method ()
  "Jump to a method/function, corresponding the current line in method buffer.
When called standing on a line of method/function list, it closes the list
buffer and sets the point to a method/function, corresponding the line."
  (interactive)
  (let ((method-record (nth (1- (line-number-at-pos)) eassist-actual-methods)))
    (cond
     (method-record
      (kill-buffer (current-buffer))
      (switch-to-buffer eassist-buffer t)
      (goto-char (eassist-method-position method-record))
      (recenter))
     (t (message "The line does not contain method description!")))))

(defun eassist-matches-all (string substrings)
  "Return non-nil if STRING contain each of SUBSTRINGS as a substring."
  (reduce (lambda (prev part) (and prev (string-match part string))) substrings :initial-value t))

(defun eassist-search-string-updated ()
  "Update method/function list according to search string."
  (message eassist-search-string)
  (setq eassist-actual-methods
	(remove-if-not
	 (lambda (elt) (eassist-matches-all (eassist-method-name elt) (split-string eassist-search-string)))
	 eassist-methods))
  (erase-buffer)
  (dolist (i eassist-overlays)
    (delete-overlay i))
  (setq eassist-overlays nil)
  (loop for i in (mapcar 'eassist-method-full-line eassist-actual-methods)
        with pos = 1
        with strings = (split-string eassist-search-string)
        do
        (insert i)
        (dolist (j strings)
          (let ((p (string-match j i eassist-names-column)))
             (when p
               (push (make-overlay (+ pos p) (+ pos p (length j))) eassist-overlays)
               (overlay-put (car eassist-overlays) 'face '(background-color . "yellow")))))
        (setq pos (+ pos (length i))))
  (goto-line (/ (count-lines (point-min) (point-max)) 2)))

(defun eassist-key-pressed (key)
  "Called when KEY is pressed."
  (setq eassist-search-string (concat eassist-search-string (char-to-string key)))
  (eassist-search-string-updated))

(defun eassist-backspace-pressed ()
  "Called when Backspace is pressed."
  (interactive)
  (setq eassist-search-string (eassist-string-without-last eassist-search-string 1))
  (eassist-search-string-updated))

(defun eassist-make-key-function (key)
  "Return a function for KEY."
   `(lambda () (interactive) (eassist-key-pressed ,key)))

(defun eassist-key-itself (map key)
  "Maps in the MAP KEY to its function."
  (define-key map (char-to-string key) (eassist-make-key-function key)))

(defun eassist-escape ()
  "Kill method list buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer eassist-buffer))

(defvar eassist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (do ((k (string-to-char "a") (+ 1 k))) ((> k (string-to-char "z")))
      (define-key
    	map
    	(read-kbd-macro (char-to-string k))
    	(eassist-make-key-function k)))
    (do ((k (string-to-char "A") (+ 1 k))) ((> k (string-to-char "Z")))
      (define-key
    	map
    	(read-kbd-macro (char-to-string k))
    	(eassist-make-key-function k)))
    (do ((k (string-to-char "0") (+ 1 k))) ((> k (string-to-char "9")))
      (define-key
    	map
    	(read-kbd-macro (char-to-string k))
    	(eassist-make-key-function k)))
    (dolist (k (string-to-list "=><&!"))
      (define-key
    	map
    	(read-kbd-macro (char-to-string k))
    	(eassist-make-key-function k)))

    (eassist-key-itself map (string-to-char " "))
    (eassist-key-itself map (string-to-char "_"))

    (define-key map (kbd "<RET>") 'eassist-jump-to-method)
    (define-key map (kbd "<backspace>") 'eassist-backspace-pressed)
    (define-key map (kbd "<ESC>") 'eassist-escape)
    map)
  "Keymap for `eassist-mode'.")

(defstruct eassist-method
  (full-line)
  (name)
  (position)
  (tag))

(defun eassist-mode-init ()
  "Initialize method/function list mode."
  (make-local-variable 'eassist-search-string)   ;; current method search string
  (make-local-variable 'eassist-methods)         ;; list of eassist-method structures
  (make-local-variable 'eassist-actual-methods)  ;; subset of eassist-methods that contain eassist-search string in the name string
  (make-local-variable 'eassist-names-column)    ;; this is the column where method name fields starts
  (make-local-variable 'eassist-overlays)        ;; overlays used to highligh search string matches in method names
  (setq eassist-overlays nil)
  (setq eassist-search-string "")

  (setq eassist-methods
        (let* ((method-tags (eassist-function-tags))
               (method-triplets (mapcar 'eassist-function-string-triplet method-tags)))
          (mapcar* '(lambda (full-line name position tag)
                      (make-eassist-method :full-line full-line :name name :position position :tag tag))
                   (eassist-format-triplets method-triplets)
                   (mapcar 'caddr method-triplets)
                   (mapcar 'semantic-tag-start method-tags)
                   method-tags)))
  (eassist-search-string-updated)

  ;; Set current line corresponding to the current function/method if any
  (let ((line (position-if 
               (lambda (item) (eq eassist-current-tag (eassist-method-tag item)))
               eassist-methods)))
    (when line
      (goto-line (1+ line))))

  ;;(setq b1 (current-buffer))
  ;;(setq ov1 (make-overlay 1 30 b1))
  ;;(overlay-put ov1 'face '(background-color . "yellow"))
  (hl-line-mode))

(define-derived-mode eassist-mode nil "Eassist methods"
  "EmacsAssist method selection mode.
   \\{eassist-mode-map}
   Turning on Text mode runs the normal hook `eassist-mode-hook'."
  (eassist-mode-init))

;; ================================== Method navigator end ======================

(provide 'eassist)

;;; eassist.el ends here
