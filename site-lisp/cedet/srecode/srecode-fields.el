;;; srecode-fields.el --- Handling type-in fields in a buffer.
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-fields.el,v 1.5.2.1 2009/03/01 02:55:58 zappo Exp $
;;
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
;; Idea courtesy of yasnippets.
;;
;; If someone prefers not to type unknown dictionary entries into
;; mini-buffer prompts, it could instead use in-buffer fields.
;;
;; A template-region specifies an area in which the fields exist.  If
;; the cursor exits the region, all fields are cleared.
;;
;; Each field is independent, but some are linked together by name.
;; Typing in one will cause the matching ones to change in step.
;;
;; Each field has 2 overlays.  The second overlay allows control in
;; the character just after the field, but does not highlight it.

;; Keep this library independent of SRecode proper.
(require 'eieio)

;;; Overlay Compatibility
;;
(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defalias 'srecode-make-overlay
	  (lambda (beg end &optional buffer &rest rest)
	    (let ((ol (make-extent beg end buffer)))
	      (when rest
		(set-extent-property ol 'start-open (car rest))
		(setq rest (cdr rest)))
	      (when rest
		(set-extent-property ol 'end-open (car rest)))
	      ol)))
	(defalias 'srecode-overlay-put    'set-extent-property)
	(defalias 'srecode-overlay-get    'extent-property)
	(defalias 'srecode-overlay-move   'set-extent-endpoints)
	(defalias 'srecode-overlay-delete 'delete-extent)
	(defalias 'srecode-overlay-p      'extentp)
	(defalias 'srecode-overlay-start  'extent-start-position)
	(defalias 'srecode-overlay-end    'extent-end-position)
	(defalias 'srecode-overlays-at
	  (lambda (pos) 
	    (condition-case nil
		(extent-list nil pos pos)
	      (error nil))
	    ))
	)
    (defalias 'srecode-make-overlay   'make-overlay)
    (defalias 'srecode-overlay-put    'overlay-put)
    (defalias 'srecode-overlay-get    'overlay-get)
    (defalias 'srecode-overlay-move   'move-overlay)
    (defalias 'srecode-overlay-delete 'delete-overlay)
    (defalias 'srecode-overlays-at    'overlays-at)
    (defalias 'srecode-overlay-p      'overlayp)
    (defalias 'srecode-overlay-start  'overlay-start)
    (defalias 'srecode-overlay-end    'overlay-end)
    ))

;;; Code:
(defvar srecode-field-archive nil
  "While inserting a set of fields, collect in this variable.
Once an insertion set is done, these fields will be activated.")

(defface srecode-field-face
  '((((class color) (background dark))
     (:underline "green"))
    (((class color) (background light))
     (:underline "green4")))
  "*Face used to specify editable fields from a template."
  :group 'semantic-faces)

;;; BASECLASS
;;
;; Fields and the template region share some basic overlay features.

(defclass srecode-overlaid ()
  ((overlay :documentation
	    "Overlay representing this field.
The overlay will crossreference this object.")
   )
  "An object that gets automatically bound to an overlay.
Has virtual :start and :end initializers.")

(defmethod initialize-instance ((olaid srecode-overlaid) &optional args)
  "Initialize OLAID, being sure it archived."
  ;; Extract :start and :end from the olaid list.
  (let ((newargs nil)
	(smarker (make-marker))
	start end
	)

    (while args
      (cond ((eq (car args) :start)
	     (setq args (cdr args))
	     (setq start (car args))
	     (setq args (cdr args))
	     )
	    ((eq (car args) :end)
	     (setq args (cdr args))
	     (setq end (car args))
	     (setq args (cdr args))
	     )
	    (t
	     (push (car args) newargs)
	     (setq args (cdr args))
	     (push (car args) newargs)
	     (setq args (cdr args)))
	    ))

    (move-marker smarker start (current-buffer))
    (oset olaid overlay (cons smarker (- end start)))
    
    (call-next-method olaid (nreverse newargs))

    ))

(defmethod srecode-overlaid-activate ((olaid srecode-overlaid))
  "Activate the overlaid area."
  (let* ((ola (oref olaid overlay))
	 (start (car ola))
	 (end (+ start (cdr ola)))
	 (ol (srecode-make-overlay start end (current-buffer) nil t)))

    (srecode-overlay-put ol 'srecode olaid)
    
    (oset olaid overlay ol)

    ))

(defmethod srecode-delete ((olaid srecode-overlaid))
  "Delete the overlay from OLAID."
  (srecode-overlay-delete (oref olaid overlay))
  (slot-makeunbound olaid 'overlay)
  )

(defmethod srecode-empty-region-p ((olaid srecode-overlaid))
  "Return non-nil if the region covered by OLAID is of length 0."
  (= 0 (srecode-region-size olaid)))

(defmethod srecode-region-size ((olaid srecode-overlaid))
  "Return the length of region covered by OLAID."
  (let ((start (srecode-overlay-start (oref olaid overlay)))
	(end (srecode-overlay-end (oref olaid overlay))))
    (- end start)))

(defmethod srecode-point-in-region-p ((olaid srecode-overlaid))
  "Return non-nil if point is in the region of OLAID."
  (let ((start (srecode-overlay-start (oref olaid overlay)))
	(end (srecode-overlay-end (oref olaid overlay))))
    (and (>= (point) start) (<= (point) end))))

(defun srecode-overlaid-at-point (class)
  "Return a list of overlaid fields of type CLASS at point."
  (let ((ol (srecode-overlays-at (point)))
	(ret nil))
    (while ol
      (let ((tmp (srecode-overlay-get (car ol) 'srecode)))
	(when (and tmp (object-of-class-p tmp class))
	  (setq ret (cons tmp ret))))
      (setq ol (cdr ol)))
    (car (nreverse ret))))

(defmethod srecode-overlaid-text ((olaid srecode-overlaid) &optional set-to)
  "Return the text under OLAID.
If SET-TO is a string, then replace the text of OLAID wit SET-TO."
  (let* ((ol (oref olaid overlay))
	 (start (srecode-overlay-start ol)))
    (if (not (stringp set-to))
	;; Just return it.
	(buffer-substring-no-properties start (srecode-overlay-end ol))
      ;; Replace it.
      (save-excursion
	(delete-region start (srecode-overlay-end ol))
	(goto-char start)
	(insert set-to)
	(srecode-overlay-move ol start (+ start (length set-to))))
      nil)))

;;; INSERTED REGION
;;
;; Managing point-exit, and flushing fields.

(defclass srecode-template-inserted-region (srecode-overlaid)
  ((fields :documentation
	   "A list of field overlays in this region.")
   (active-region :allocation :class
		  :documentation
		  "The template region currently being handled.")
   )
  "Manage a buffer region in which fields exist.")

(defmethod initialize-instance ((ir srecode-template-inserted-region)
				&rest args)
  "Initialize IR, capturing the active fields, and creating the overlay."
  ;; Fill in the fields
  (oset ir fields srecode-field-archive)
  (setq srecode-field-archive nil)

  ;; Initailize myself first.
  (call-next-method)
  )

(defmethod srecode-overlaid-activate ((ir srecode-template-inserted-region))
  "Activate the template area for IR."
  ;; Activate all our fields

  (dolist (F (oref ir fields))
    (srecode-overlaid-activate F))

  ;; Activate our overlay.
  (call-next-method)

  ;; Position the cursor at the first field
  (let ((first (car (oref ir fields))))
    (goto-char (srecode-overlay-start (oref first overlay))))

  ;; Set ourselves up as 'active'
  (oset ir active-region ir)

  ;; Setup the post command hook.
  (add-hook 'post-command-hook 'srecode-field-post-command t t)
  )

(defmethod srecode-delete ((ir srecode-template-inserted-region))
  "Call into our base, but also clear out the fields."
  ;; Clear us out of the baseclass.
  (oset ir active-region nil)
  ;; Clear our fields.
  (mapc 'srecode-delete (oref ir fields))
  ;; Call to our base
  (call-next-method)
  ;; Clear our hook.
  (remove-hook 'post-command-hook 'srecode-field-post-command t)
  )

(defsubst srecode-active-template-region ()
  "Return the active region for template fields."
  (oref srecode-template-inserted-region active-region))

(defun srecode-field-post-command ()
  "Srecode field handler in the post command hook."
  (let ((ar (srecode-active-template-region))
	)
    (if (not ar)
	;; Find a bug and fix it.
	(remove-hook 'post-command-hook 'srecode-field-post-command t)
      (if (srecode-point-in-region-p ar)
	  nil ;; Keep going
	;; We moved out of the temlate.  Cancel the edits.
	(srecode-delete ar)))
    ))

;;; FIELDS
;;
;;;###autoload
(defclass srecode-field (srecode-overlaid)
  ((tail :documentation 
	 "Overlay used on character just after this field.
Used to provide useful keybindings there.")
   (name :initarg :name
	 :documentation
	 "The name of this field.
Usually initialized from the dictionary entry name that
the users needs to edit.")
   (prompt :initarg :prompt
	   :documentation
	   "A prompt string to use if this were in the minibuffer.
Display when the cursor enters this field.")
   (read-fcn :initarg :read-fcn
	     :documentation
	     "A function that would be used to read a string.
Try to use this to provide useful completion when available.")
   )
  "Representation of one field.")

(defvar srecode-field-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" 'srecode-field-next)
    (define-key km "\M-\C-i" 'srecode-field-prev)
    (define-key km "\C-e" 'srecode-field-end)
    (define-key km "\C-a" 'srecode-field-start)
    (define-key km "\M-m" 'srecode-field-start)
    km)
  "Keymap applied to field overlays.")

(defmethod initialize-instance ((field srecode-field) &optional args)
  "Initialize FIELD, being sure it archived."
  (add-to-list 'srecode-field-archive field t)
  (call-next-method)
  )

(defmethod srecode-overlaid-activate ((field srecode-field))
  "Activate the FIELD area."
  (call-next-method)

  (let* ((ol (oref field overlay))
	 (end nil)
	 (tail nil))
    (srecode-overlay-put ol 'face 'srecode-field-face)
    (srecode-overlay-put ol 'keymap srecode-field-keymap)
    (srecode-overlay-put ol 'modification-hooks '(srecode-field-mod-hook))
    (srecode-overlay-put ol 'insert-behind-hooks '(srecode-field-behind-hook))
    (srecode-overlay-put ol 'insert-in-front-hooks '(srecode-field-mod-hook))

    (setq end (srecode-overlay-end ol))
    (setq tail (srecode-make-overlay end (+ end 1) (current-buffer)))

    (srecode-overlay-put tail 'srecode field)
    (srecode-overlay-put tail 'keymap srecode-field-keymap)
    (srecode-overlay-put tail 'face 'srecode-field-face)
    (oset field tail tail)
    )
  )

(defmethod srecode-delete ((olaid srecode-field))
  "Delete our secondary overlay."
  ;; Remove our spare overlay
  (srecode-overlay-delete (oref olaid tail))
  (slot-makeunbound olaid 'tail)
  ;; Do our baseclass work.
  (call-next-method)
  )

(defvar srecode-field-replication-max-size 100
  "Maximum size of a field before cancelling replication.")

(defun srecode-field-mod-hook (ol after start end &optional pre-len)
  "Modification hook for the field overlay.
OL is the overlay.
AFTER is non-nil if it is called after the change.
START and END are the bounds of the change.
PRE-LEN is used in the after mode for the length of the changed text."
  (when (and after (not undo-in-progress))
    (let* ((field (srecode-overlay-get ol 'srecode))
	   (inhibit-point-motion-hooks t)
	   (inhibit-modification-hooks t)
	   )
      ;; Sometimes a field is deleted, but we might still get a stray
      ;; event.  Lets just ignore those events.
      (when (slot-boundp field 'overlay)
	;; First, fixup the two overlays, in case they got confused.
	(let ((main (oref field overlay))
	      (tail (oref field tail)))
	  (srecode-overlay-move main
				(srecode-overlay-start main)
				(1- (srecode-overlay-end tail)))
	  (srecode-overlay-move tail
				(1- (srecode-overlay-end tail))
				(srecode-overlay-end tail)))
	;; Now capture text from the main overlay, and propagate it.
	(let* ((new-text (srecode-overlaid-text field))
	       (region (srecode-active-template-region))
	       (allfields (when region (oref region fields)))
	       (name (oref field name)))
	  (dolist (F allfields)
	    (when (and (not (eq F field))
		       (string= name (oref F name)))
	      (if (> (length new-text) srecode-field-replication-max-size)
		  (message "Field size too large for replication.")
		;; If we find other fields with the same name, then keep
		;; then all together.  Disable change hooks to make sure
		;; we don't get a recursive edit.
		(srecode-overlaid-text F new-text)
		))))
	))))

(defun srecode-field-behind-hook (ol after start end &optional pre-len)
  "Modification hook for the field overlay.
OL is the overlay.
AFTER is non-nil if it is called after the change.
START and END are the bounds of the change.
PRE-LEN is used in the after mode for the length of the changed text."
  (when after
    (let* ((field (srecode-overlay-get ol 'srecode))
	   )
      (srecode-overlay-move ol (srecode-overlay-start ol) end)
      (srecode-field-mod-hook ol after start end pre-len))
    ))

(defmethod srecode-field-goto ((field srecode-field))
  "Goto the FIELD."
  (goto-char (srecode-overlay-start (oref field overlay))))

(defun srecode-field-next ()
  "Move to the next field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field))
	 (tr (srecode-overlaid-at-point 'srecode-template-inserted-region))
	 )
    (when (not f) (error "Not in a field"))
    (when (not tr) (error "Not in a template region"))

    (let ((fields (oref tr fields)))
      (while fields
	;; Loop over fields till we match.  Then move to the next one.
	(when (eq f (car fields))
	  (if (cdr fields)
	      (srecode-field-goto (car (cdr fields)))
	    (srecode-field-goto (car (oref tr fields))))
	  (setq fields nil)
	  )
	(setq fields (cdr fields))))
    ))

(defun srecode-field-prev ()
  "Move to the prev field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field))
	 (tr (srecode-overlaid-at-point 'srecode-template-inserted-region))
	 )
    (when (not f) (error "Not in a field"))
    (when (not tr) (error "Not in a template region"))

    (let ((fields (reverse (oref tr fields))))
      (while fields
	;; Loop over fields till we match.  Then move to the next one.
	(when (eq f (car fields))
	  (if (cdr fields)
	      (srecode-field-goto (car (cdr fields)))
	    (srecode-field-goto (car (oref tr fields))))
	  (setq fields nil)
	  )
	(setq fields (cdr fields))))
    ))

(defun srecode-field-end ()
  "Move to the end of this field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field)))
    (goto-char (srecode-overlay-end (oref f overlay)))))

(defun srecode-field-start ()
  "Move to the end of this field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field)))
    (goto-char (srecode-overlay-start (oref f overlay)))))

;;; COMPOUND VALUE
;;
(defclass srecode-field-value (srecode-dictionary-compound-value)
  ((firstinserter :initarg :firstinserter
		  :documentation
		  "The inserter object for the first occurance of this field.")
   (defaultvalue :initarg :defaultvalue
     :documentation
     "The default value for this inserter.")
   )
  "When inserting values with editable field mode, a dictionary value.
Compound values allow a field to be stored in the dictionary for when
it is referenced a second time.  This compound value can then be
inserted with a new editable field.")

(defmethod srecode-compound-toString((cp srecode-field-value)
				     function
				     dictionary)
  "Convert this field into an insertable string."
  ;; If we are not in a buffer, then this is not supported.
  (when (not (bufferp standard-output))
    (error "FIELDS invoked while inserting template to non-buffer."))

  (if function
      (error "@todo: Cannot mix field insertion with functions.")

    ;; Otherwise, apply the function to the tag itself.
    ;; We know we are in a buffer, so we can perform the insertion.
    (let* ((dv (oref cp defaultvalue))
	   (sti (oref cp firstinserter))
	   (start (point))
	   (name (oref sti :object-name)))

      (if (or (not dv) (string= dv ""))
	  (insert name)
	(insert dv))

      (srecode-field name :name name
		     :start start
		     :end (point)
		     :prompt (oref sti prompt)
		     :read-fcn (oref sti read-fcn)
		     )
      ))
  ;; Returning nil is a signal that we have done the insertion ourselves.
  nil)

;;; TESTS
;;
;; Test out field modification w/out using srecode templates.
;;
(defvar srecode-field-utest-text
  "This is a test buffer.

It is filled with some text."
  "Text for tests.")

;;;###autoload
(defun srecode-field-utest ()
  "Test the srecode field manager."
  (interactive)
  (if (featurep 'xemacs)
      (message "There is no XEmacs support for SRecode Fields.")
    (srecode-field-utest-impl)))

(defun srecode-field-utest-impl ()
  "Implementation of the SRecode field utest."
  (save-excursion
    (find-file "/tmp/srecode-field-test.txt")

    (erase-buffer)
    (goto-char (point-min))
    (insert srecode-field-utest-text)
    (set-buffer-modified-p nil)

    ;; Test basic field generation.
    (let ((srecode-field-archive nil)
	  (f nil))
      
      (end-of-line)
      (forward-word -1)

      (setq f (srecode-field "Test"
			     :name "TEST"
			     :start 6
			     :end 8))

      (when (or (not (slot-boundp f 'overlay)) (not (oref f overlay)))
	(error "Field test: Overlay info not created for field"))

      (when (srecode-overlay-p (oref f overlay))
	(error "Overlay created during creation"))

      (srecode-overlaid-activate f)

      (when (not (srecode-overlay-p (oref f overlay)))
	(error "Overlay not created during activation"))

      (when (not (= (length srecode-field-archive) 1))
	(error "Field test: Incorrect number of elements in the field archive"))
      (when (not (eq f (car srecode-field-archive)))
	(error "Field test: Field did not auto-add itself to the field archive"))

      (when (not (srecode-overlay-get (oref f overlay) 'keymap))
	(error "Field test: Overlay keymap not set"))

      (when (not (string= "is" (srecode-overlaid-text f)))
	(error "Field test: Expected field text 'is', not %s"
	       (srecode-overlaid-text f)))

      ;; Test deletion.
      (srecode-delete f)

      (when (slot-boundp f 'overlay)
	(error "Field test: Overlay not deleted after object delete"))
      )

    ;; Test basic region construction.
    (let* ((srecode-field-archive nil)
	   (reg nil)
	   (fields
	    (list
	     (srecode-field "Test1" :name "TEST-1" :start 5 :end 10)
	     (srecode-field "Test2" :name "TEST-2" :start 15 :end 20)
	     (srecode-field "Test3" :name "TEST-3" :start 25 :end 30)

	     (srecode-field "Test4" :name "TEST-4" :start 35 :end 35))
	    ))
		     
      (when (not (= (length srecode-field-archive) 4))
	(error "Region Test: Found %d fields.  Expected 4"
	       (length srecode-field-archive)))

      (setq reg (srecode-template-inserted-region "REG"
						  :start 4
						  :end 40))

      (srecode-overlaid-activate reg)

      ;; Make sure it was cleared.
      (when srecode-field-archive
	(error "Region Test: Did not clear field archive"))

      ;; Auto-positioning.
      (when (not (eq (point) 5))
	(error "Region Test: Did not reposition on first field"))

      ;; Active region
      (when (not (eq (srecode-active-template-region) reg))
	(error "Region Test: Active region not set"))

      ;; Various sizes
      (mapc (lambda (T)
	      (if (string= (object-name-string T) "Test4")
		  (progn
		    (when (not (srecode-empty-region-p T))
		      (error "Field %s is not empty"
			     (object-name T)))
		    )
		(when (not (= (srecode-region-size T) 5))
		  (error "Calculated size of %s was not 5"
			 (object-name T)))))
	    fields)

      ;; Make sure things stay up after a 'command'.
      (srecode-field-post-command)
      (when (not (eq (srecode-active-template-region) reg))
	(error "Region Test: Active region did not stay up"))

      ;; Test field movement.
      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 0 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 0 fields))))

      (srecode-field-next)

      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 1 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 1 fields))))

      (srecode-field-prev)

      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 0 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 0 fields))))

      ;; Move cursor out of the region and have everything cleaned up.
      (goto-char 42)
      (srecode-field-post-command)
      (when (srecode-active-template-region)
	(error "Region Test: Active region did not clear on move out"))

      (mapc (lambda (T)
	      (when (slot-boundp T 'overlay)
		(error "Overlay did not clear off of of field %s"
		       (object-name T))))
	    fields)

      ;; End of LET
      )

    ;; Test variable linkage.
    (let* ((srecode-field-archive nil)
	   (f1 (srecode-field "Test1" :name "TEST" :start 6 :end 8))
	   (f2 (srecode-field "Test2" :name "TEST" :start 28 :end 30))
	   (f3 (srecode-field "Test3" :name "NOTTEST" :start 35 :end 40))
	   (reg (srecode-template-inserted-region "REG" :start 4 :end 40))
	   )
      (srecode-overlaid-activate reg)
      
      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: Init strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: Init string on dissimilar fields is now the same"))

      (goto-char 7)
      (insert "a")

      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: mid-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: mid-insert string on dissimilar fields is now the same"))

      (goto-char 9)
      (insert "t")

      (when (not (string= (srecode-overlaid-text f1) "iast"))
	(error "Linkage Test: tail-insert failed to captured added char"))
      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: tail-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: tail-insert string on dissimilar fields is now the same"))

      (goto-char 6)
      (insert "b")

      (when (not (string= (srecode-overlaid-text f1) "biast"))
	(error "Linkage Test: tail-insert failed to captured added char"))
      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: tail-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: tail-insert string on dissimilar fields is now the same"))

      ;; Cleanup
      (srecode-delete reg)
      )

    (set-buffer-modified-p nil)

    (message "   All field tests passed.")
    ))

(provide 'srecode-fields)
;;; srecode-fields.el ends here
