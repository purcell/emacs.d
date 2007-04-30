;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sym-lock.el - Extension of Font-Lock mode for symbol fontification.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Copyright © 1997-2004 Albert Cohen, all rights reserved.
;;         Copying is covered by the GNU General Public License.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 History
;; 
;; first prototype by wg <wg@cs.tu-berlin.de> 5-96
;; tweaked by Steve Dunham <dunham@gdl.msu.edu> 5-96
;; rewritten and enhanced by Albert Cohen <Albert.Cohen@inria.fr> 3-97
;;   new symbol-face format and ergonomy improvement 2-98
;;   major step towards portability and customization 5-98
;;   removed bug with multiple appends in hook by 3-99
;;   removed use-fonts check (due to incomatibilities) 9-00
;;   new after-change/pre-idle policy 6-01
;;   disable M$ Windows (symbol font problem) and fixed movement bug
;;     (incompatibile atomic-extents and paren) 8-01
;;   check for the availability of a symbol font 1-02
;;   patch size inference and use the upper bound (of <= sizes) 1-02
;;   support for custom replacement faces (e.g., for the lambda symbol) 10-04

;; look at the symbol font? check out: xfd -fn '-adobe-symbol-*--14-*'

(require 'cl)
(require 'font-lock)
(require 'atomic-extents)

(defvar sym-lock-sym-count 0
  "Counter for internal symbols.")

(defvar sym-lock-ext-start nil "Temporary for atomicization.")
(make-variable-buffer-local 'sym-lock-ext-start)
(defvar sym-lock-ext-end nil "Temporary for atomicization.")
(make-variable-buffer-local 'sym-lock-ext-end)

(defvar sym-lock-font-size nil
  "Default size for Sym-Lock symbol font.")
(make-variable-buffer-local 'sym-lock-font-size)
(put 'sym-lock-font-size 'permanent-local t)

(defvar sym-lock-keywords nil
  "Similar to `font-lock-keywords'.")
(make-variable-buffer-local 'sym-lock-keywords)
(put 'sym-lock-keywords 'permanent-local t)

(defvar sym-lock-enabled nil
  "Sym-Lock switch.")
(make-variable-buffer-local 'sym-lock-enabled)
(put 'sym-lock-enabled 'permanent-local t)

(defvar sym-lock-color (face-foreground 'default)
  "*Sym-Lock default color in `font-lock-use-colors' mode.")
(make-variable-buffer-local 'sym-lock-color)
(put 'sym-lock-color 'permanent-local t)

(defvar sym-lock-mouse-face-enabled t
  "Mouse face switch.")
(make-variable-buffer-local 'sym-lock-mouse-face-enabled)
(put 'sym-lock-mouse-face-enabled 'permanent-local t)

(defun sym-lock-gen-symbol (&optional prefix)
  "Generate a new internal symbol."
  ;; where is the standard function to do this ?
  (setq sym-lock-sym-count (+ sym-lock-sym-count 1))
  (intern (concat "sym-lock-gen-" (or prefix "") 
		  (int-to-string sym-lock-sym-count))))

(defun sym-lock-make-symbols-atomic (&optional begin end)
  "Function to make symbol faces atomic."
  (if sym-lock-enabled
      (map-extents
       (lambda (extent maparg)
	 (let ((face (extent-face extent)) (ext))
	   (if (and face (setq ext (face-property face 'sym-lock-remap)))
	       (progn
		 (if (numberp ext)
		     (set-extent-endpoints
		      extent (- (extent-start-position extent) ext)
		      (extent-end-position extent)))
		 (if ext
		     (progn
		       (if sym-lock-mouse-face-enabled
			   (set-extent-property extent 'mouse-face
						'default))
		       (set-extent-property extent 'atomic t)
		       (set-extent-property extent 'start-open t))))))
	 nil)
       (current-buffer)
       (if begin (save-excursion (goto-char begin) (beginning-of-line) (point))
	 (point-min))
       (if end (save-excursion (goto-char end) (end-of-line) (point))
	 (point-max))
       nil nil)))

(defun sym-lock-compute-font-size ()
  "Computes the size of the \"better\" symbol font."
  (let ((num (face-height 'default))
	(size) (minsize)
	(lf (list-fonts "-adobe-symbol-medium-r-normal--*")))
    (setq minsize 8)
    (while lf
      (string-match "-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-\\([^-]*\\)-.*"
		    (car lf))
      (setq size (string-to-number (substring (car lf)
					      (match-beginning 1)
					      (match-end 1))))
      (if (and (<= size num) (> size minsize))
	  (setq minsize size))
      (setq lf (cdr lf)))
    minsize))

(defun sym-lock-enable ()
  "Enable Sym-Lock on this buffer."
  (interactive)
  (if (not (and (fboundp 'console-type)
		(or (eq (console-type) 'x)
		    (eq (console-type) 'gtk))
		(sym-lock-look-for-symbol-font)))
      (setq sym-lock-enabled nil)
    ;; X-Window with symbol font
    (if (not sym-lock-keywords)
	(error "No Sym-Lock keywords defined!")
      (setq sym-lock-enabled t)
      (if font-lock-mode
	  (progn
	    (setq font-lock-keywords nil) ; Font-Lock explicit-defaults bug!
	    (font-lock-set-defaults t)
	    (font-lock-fontify-buffer)))
      (message "Sym-Lock enabled."))))

(defun sym-lock-disable ()
  "Disable Sym-Lock on this buffer."
  (interactive)
  (if (not sym-lock-keywords)
      (error "No Sym-Lock keywords defined!")
    (setq sym-lock-enabled nil)
    (if font-lock-mode
	(progn
	  (setq font-lock-keywords nil) ; Font-Lock explicit-defaults bug!
	  (font-lock-set-defaults t)
	  (font-lock-fontify-buffer)))
    (message "Sym-Lock disabled.")))

(defvar sym-lock-font-name
  (concat "-adobe-symbol-medium-r-normal--"
	  (if sym-lock-font-size sym-lock-font-size
	    (number-to-string (sym-lock-compute-font-size)))
	  "-*-*-*-p-*-adobe-fontspecific")
  "Name of the font used by Sym-Lock.")
(make-variable-buffer-local 'sym-lock-font-name)
(put 'sym-lock-font-name 'permanent-local t)

;;(make-face 'sym-lock-adobe-symbol-face "Face for Sym-Lock symbols")
;;(set-face-property 'sym-lock-adobe-symbol-face 'font sym-lock-font-name)

(defun sym-lock-look-for-symbol-font ()
  "Returns whether there is a symbol font registred in the font server,
   and sets sym-lock-enabled to false if not."
  (if (list-fonts sym-lock-font-name)
      t
    (setq sym-lock-enabled nil)
    nil))

(defun sym-lock-set-foreground ()
  "Set foreground color of Sym-Lock faces."
  (if (and (boundp 'sym-lock-defaults) sym-lock-defaults)
      (let ((l (car sym-lock-defaults))
	    (color (face-foreground 'default) sym-lock-color))
	(if (and (consp l) (eq (car l) 'quote)) (setq l (eval l)))
	(if (symbolp l) (setq l (eval l)))
	(dolist (c l)
	  (setq c (nth 2 c))
	  (if (consp c) (setq c (eval c)))
	  (if (string-match "-adobe-symbol-medium-r-normal-"
			    (font-name (face-font c)))
	      (set-face-foreground c color))))))

(defun sym-lock-remap-face (pat pos obj atomic face)
  "Make a temporary face which remaps the POS char of PAT to the
given OBJ under the symbol face and all other characters to
the empty string. OBJ may either be a string or a character."
  (let* ((name (sym-lock-gen-symbol "face"))
	 (table (make-display-table))
	 (tface (make-face name "sym-lock-remap-face" t)))
    (fillarray table "")
    (aset table (string-to-char (substring pat (1- pos) pos))
	  (if (stringp obj) obj (make-string 1 obj)))
    (if face
	(set-face-parent tface face)
      (set-face-foreground tface sym-lock-color)
      (set-face-property tface 'font sym-lock-font-name))
    (set-face-property tface 'display-table table)
    (set-face-property tface 'sym-lock-remap atomic) ; mark it
    tface
    ;; return face value and not face name
    ;; the temporary face would be otherwise GCed
    ))

(defvar sym-lock-clear-face
  (let* ((name (sym-lock-gen-symbol "face"))
	 (table (make-display-table))
	 (tface (make-face name "sym-lock-remap-face" t)))
    (fillarray table "")
    (set-face-property tface 'display-table table)
    (set-face-property tface 'sym-lock-remap 1) ; mark it
    tface 
    ;; return face value and not face name
    ;; the temporary face would be otherwise GCed
    ))

(defun sym-lock (fl)
  "Create font-lock table entries from a list of (PAT NUM POS OBJ) where
PAT (at NUM) is substituted by OBJ under the symbol face. The face's extent
will become atomic."
  (if (not (and (fboundp 'console-type)
		(or (eq (console-type) 'x)
		    (eq (console-type) 'gtk))))
      (setq sym-lock-enabled nil)
    ;; X-Window
    (if (sym-lock-look-for-symbol-font)
	(progn
	  (message "Computing Sym-Lock faces...")
	  (setq sym-lock-keywords (sym-lock-rec fl))
	  (setq sym-lock-enabled t)
	  (message "Computing Sym-Lock faces... done.")))
    ;; ugly hack to make atomic keywords traversable when
    ;; paren-highlighting is also using post-command-hook...
    ;; it moves atomic-extents post-command-hook to the front
    (remove-hook 'post-command-hook 'atomic-extent-post-hook)
    (add-hook 'post-command-hook 'atomic-extent-post-hook)))

(defun sym-lock-rec (fl)
  (let ((f (car fl)))
    (if f (let* ((pat (car f))
		 (pos (caddr f))
		 (c (substring pat (1- pos) pos)))
	    (if (or (string-match c (substring pat pos (length pat)))
		    (string-match c (substring pat 0 (1- pos))))
		(cons (apply 'sym-lock-atom f)
		      (cons (apply 'sym-lock-face f)
			    (sym-lock-rec (cdr fl))))
	      (cons (apply 'sym-lock-atom-face f)
		    (sym-lock-rec (cdr fl))))))))

(defun sym-lock-atom-face (pat num pos obj face &optional override)
  "Define an entry for the font-lock table which substitutes PAT (at NUM) by
OBJ under the symbol face. The face extent WILL become atomic."
  (list pat num (sym-lock-remap-face pat pos obj t face) override))

(defun sym-lock-face (pat num pos obj face &optional override)
  "Define an entry for the font-lock table which substitutes PAT (at NUM) by
OBJ under symbol face. The face extent will NOT become
atomic."
  (list (concat "\\(" (substring pat 0 pos) "\\)"
		(substring pat pos (length pat)))
	(1+ num) (sym-lock-remap-face pat pos obj nil face) override))

(defun sym-lock-atom (pat num pos obj face &optional override)
  "Define an entry for the font lock table which substitutes PAT (at NUM) by
a void face. To build the atom, the face extent will be reshaped from
\"begin_point\"-1 to \"end_point\"."
  (list (concat (substring pat 0 pos) "\\("
		(substring pat pos (length pat)) "\\)")
	(1+ num) sym-lock-clear-face override))

(defun sym-lock-after-change-function (beg end old-len)
  (when sym-lock-enabled
    (setq sym-lock-ext-start (if sym-lock-ext-start
				 (min beg sym-lock-ext-start) beg))
    (setq sym-lock-ext-end (if sym-lock-ext-end
			       (max end sym-lock-ext-end) end))))

(defun sym-lock-pre-idle-hook-last ()
  (if sym-lock-enabled
      (condition-case nil
	  (when (and sym-lock-enabled sym-lock-ext-start)
	    (sym-lock-make-symbols-atomic sym-lock-ext-start sym-lock-ext-end)
	    (setq sym-lock-ext-start nil)
	    (setq sym-lock-ext-end nil))
	(error (warn "Error caught in `sym-lock-pre-idle-hook-last'")))))

(add-hook 'font-lock-after-fontify-buffer-hook
	  'sym-lock-make-symbols-atomic)

(defun sym-lock-mouse-face-enable ()
  "Enable special face for symbols under mouse."
  (interactive)
  (setq sym-lock-mouse-face-enabled t)
  (if sym-lock-enabled
      (font-lock-fontify-buffer)))

(defun sym-lock-mouse-face-disable ()
  "Disable special face for symbols under mouse."
  (interactive)
  (setq sym-lock-mouse-face-enabled nil)
  (if sym-lock-enabled
      (font-lock-fontify-buffer)))

(defun sym-lock-font-lock-hook ()
  "Function called by `font-lock-mode' for initialization purposes."
  (add-hook 'after-change-functions 'sym-lock-after-change-function)
  (add-hook 'pre-idle-hook 'sym-lock-pre-idle-hook-last t)
  (if (and (featurep 'sym-lock) sym-lock-enabled
	   font-lock-defaults (boundp 'sym-lock-keywords))
      (progn
	(sym-lock-patch-keywords)
	(sym-lock-set-foreground))))

(defun font-lock-set-defaults (&optional explicit-defaults)
  (when
      (and
       (featurep 'font-lock)
       (if font-lock-auto-fontify
           (not (memq major-mode font-lock-mode-disable-list))
         (memq major-mode font-lock-mode-enable-list))
       (font-lock-set-defaults-1 explicit-defaults)
       (sym-lock-patch-keywords))
    (turn-on-font-lock)))

(defun sym-lock-patch-keywords ()
  (if (and font-lock-keywords sym-lock-enabled
	   (boundp 'sym-lock-keywords)
	   (listp (car font-lock-keywords))
	   (listp (cdar font-lock-keywords))
	   (listp (cddar font-lock-keywords))
	   (or (listp (caddar font-lock-keywords))
	       (not (string-match
		     "sym-lock"
		     (symbol-name
		      (face-name (cadr (cdar font-lock-keywords))))))))
      (setq font-lock-keywords (append sym-lock-keywords
				       font-lock-keywords))) t)

(add-menu-button '("Options" "Syntax Highlighting")
		 ["Sym-Lock"
		  (if sym-lock-enabled (sym-lock-disable) (sym-lock-enable))
		  :style toggle :selected sym-lock-enabled
		  :active sym-lock-keywords] "Automatic")

(add-hook 'font-lock-mode-hook 'sym-lock-font-lock-hook)

(provide 'sym-lock)
