;;;; Source-paths

;;; CMUCL/SBCL use a data structure called "source-path" to locate
;;; subforms.  The compiler assigns a source-path to each form in a
;;; compilation unit.  Compiler notes usually contain the source-path
;;; of the error location.
;;;
;;; Compiled code objects don't contain source paths, only the
;;; "toplevel-form-number" and the (sub-) "form-number".  To get from
;;; the form-number to the source-path we need the entire toplevel-form
;;; (i.e. we have to read the source code).  CMUCL has already some
;;; utilities to do this translation, but we use some extended
;;; versions, because we need more exact position info.  Apparently
;;; Hemlock is happy with the position of the toplevel-form; we also
;;; need the position of subforms.
;;;
;;; We use a special readtable to get the positions of the subforms.
;;; The readtable stores the start and end position for each subform in
;;; hashtable for later retrieval.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.

;;; Taken from swank-cmucl.lisp, by Helmut Eller

(in-package :swank-backend)

;; Some test to ensure the required conformance
(let ((rt (copy-readtable nil)))
  (assert (or (not (get-macro-character #\space rt))
	      (nth-value 1 (get-macro-character #\space rt))))
  (assert (not (get-macro-character #\\ rt))))

(defun make-sharpdot-reader (orig-sharpdot-reader)
  #'(lambda (s c n)
      ;; We want things like M-. to work regardless of any #.-fu in
      ;; the source file that is to be visited. (For instance, when a
      ;; file contains #. forms referencing constants that do not
      ;; currently exist in the image.)
      (ignore-errors (funcall orig-sharpdot-reader s c n))))

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (declare (type function fn))
  (lambda (stream char)
    (let ((start (1- (file-position stream)))
	  (values (multiple-value-list (funcall fn stream char)))
	  (end (file-position stream)))
      #+(or)
      (format t "[~D \"~{~A~^, ~}\" ~D ~D ~S]~%" 
	      start values end (char-code char) char)
      (unless (null values)
	(push (cons start end) (gethash (car values) source-map)))
      (values-list values))))

(defun make-source-recording-readtable (readtable source-map) 
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (flet ((install-special-sharpdot-reader (*readtable*)
	   (let ((old-reader (ignore-errors
			       (get-dispatch-macro-character #\# #\.))))
	     (when old-reader
	       (set-dispatch-macro-character #\# #\.
		 (make-sharpdot-reader old-reader))))))
    (let* ((tab (copy-readtable readtable))
	   (*readtable* tab))
      (dotimes (code 128)
	(let ((char (code-char code)))
	  (multiple-value-bind (fn term) (get-macro-character char tab)
	    (when fn
	      (set-macro-character char (make-source-recorder fn source-map) 
				   term tab)))))
      (install-special-sharpdot-reader tab)
      tab)))

(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         (*readtable* (make-source-recording-readtable *readtable* source-map))
	 (start (file-position stream))
	 (form (ignore-errors (read stream)))
	 (end (file-position stream)))
    ;; ensure that at least FORM is in the source-map
    (unless (gethash form source-map)
      (push (cons start end) (gethash form source-map)))
    (values form source-map)))

(defun skip-toplevel-forms (n stream)
  (let ((*read-suppress* t))
    (dotimes (i n)
      (read stream))))

(defun read-source-form (n stream)
  "Read the Nth toplevel form number with source location recording.
Return the form and the source-map."
  (skip-toplevel-forms n stream)
  (let ((*read-suppress* nil))
    (read-and-record-source-map stream)))
  
(defun source-path-stream-position (path stream)
  "Search the source-path PATH in STREAM and return its position."
  (check-source-path path)
  (destructuring-bind (tlf-number . path) path
    (multiple-value-bind (form source-map) (read-source-form tlf-number stream)
      (source-path-source-position (cons 0 path) form source-map))))

(defun check-source-path (path)
  (unless (and (consp path)
               (every #'integerp path))
    (error "The source-path ~S is not valid." path)))

(defun source-path-string-position (path string)
  (with-input-from-string (s string)
    (source-path-stream-position path s)))

(defun source-path-file-position (path filename)
  ;; We go this long way round, and don't directly operate on the file
  ;; stream because FILE-POSITION (used above) is not totally savy even
  ;; on file character streams; on SBCL, FILE-POSITION returns the binary
  ;; offset, and not the character offset---screwing up on Unicode.
  (let ((toplevel-number (first path))
	(buffer))
    (with-open-file (file filename)
      (skip-toplevel-forms (1+ toplevel-number) file)
      (let ((endpos (file-position file)))
	(setq buffer (make-array (list endpos) :element-type 'character
				 :initial-element #\Space))
	(assert (file-position file 0))
	(read-sequence buffer file :end endpos)))
    (source-path-string-position path buffer)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of the deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for n in path
		     for f = form then (nth n f)
		     collect f)))
    ;; select the first subform present in source-map
    (loop for form in (reverse forms)
	  for positions = (gethash form source-map)
	  until (and positions (null (cdr positions)))
	  finally (destructuring-bind ((start . end)) positions
		    (return (values start end))))))

