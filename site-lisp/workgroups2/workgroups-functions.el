;;; workgroups-functions --- Functions you might find useful
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'workgroups-variables)
(require 'workgroups-utils-basic)
(require 'workgroups-structs)

;;; session ops

(defun wg-current-session (&optional noerror)
  "Return `wg-current-session', setting it first if necessary."
  (or wg-current-session
      (unless noerror
        (error "No session is defined"))))

(defmacro wg-buf-list ()
  "setf'able `wg-current-session' buf-list slot accessor."
  `(wg-session-buf-list (wg-current-session)))

(defmacro wg-workgroup-list ()
  "setf'able `wg-current-session' modified slot accessor."
  `(wg-session-workgroup-list (wg-current-session)))

(defun wg-workgroup-list-or-error (&optional noerror)
  "Return the value of `wg-current-session's :workgroup-list slot."
  (or (wg-workgroup-list)
      (unless noerror
        (error "No workgroups are defined"))))

(defun wg-modified-p ()
  "Return t when the current session or any of its workgroups are modified."
  (or (wg-session-modified (wg-current-session))
      (cl-some 'wg-workgroup-modified (wg-workgroup-list))))

(defun wg-mark-everything-unmodified ()
  "Mark the session and all workgroups as unmodified."
  (setf (wg-session-modified (wg-current-session)) nil)
  (dolist (workgroup (wg-workgroup-list))
    (setf (wg-workgroup-modified workgroup) nil)))

(defun wg-workgroup-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-workgroup-name (wg-workgroup-list-or-error noerror)))



;;; session parameters

(defun wg-session-parameter (session parameter &optional default)
  "Return SESSION's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
SESSION nil defaults to the current session."
  (wg-aget (wg-session-parameters (or session (wg-current-session)))
           parameter default))

(defun wg-set-session-parameter (session parameter value)
  "Set SESSION's value of PARAMETER to VALUE.
SESSION nil means use the current session.
Return value."
  (let ((session (or session (wg-current-session))))
    (wg-set-parameter (wg-session-parameters session) parameter value)
    (setf (wg-session-modified session) t)
    value))

(defun wg-remove-session-parameter (session parameter)
  "Remove parameter PARAMETER from SESSION's parameters."
  (let ((session (or session (wg-current-session))))
    (wg-asetf (wg-session-parameters session) (wg-aremove it parameter))
    (setf (wg-session-modified session) t)))

(defun wg-session-local-value (variable &optional session)
  "Return the value of VARIABLE in SESSION.
SESSION nil defaults to the current session.  If VARIABLE does
not have a session-local binding in SESSION, the value is
resolved by Emacs."
  (let* ((undefined (cl-gensym))
         (value (wg-session-parameter session variable undefined)))
    (if (not (eq value undefined)) value
      (symbol-value variable))))



;;; buffer object utils

(defun wg-buffer-uid (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer-local value of `wg-buffer-uid'."
  (buffer-local-value 'wg-buffer-uid (wg-get-buffer buffer-or-name)))

(defun wg-bufobj-uid (bufobj)
  "Return BUFOBJ's uid."
  (cl-etypecase bufobj
    (buffer (wg-buffer-uid bufobj))
    (wg-buf (wg-buf-uid bufobj))
    (string (wg-bufobj-uid (wg-get-buffer bufobj)))))

(defun wg-bufobj-name (bufobj)
  "Return BUFOBJ's buffer name."
  (cl-etypecase bufobj
    (buffer (buffer-name bufobj))
    (wg-buf (wg-buf-name bufobj))
    (string (wg-buffer-name bufobj))))

(defun wg-bufobj-file-name (bufobj)
  "Return BUFOBJ's filename."
  (cl-etypecase bufobj
    (buffer (buffer-file-name bufobj))
    (wg-buf (wg-buf-file-name bufobj))
    (string (wg-bufobj-file-name (wg-get-buffer bufobj)))))

(defun wg-buf-major-mode (buf)
  "Return BUF's `major-mode'.
It's stored in BUF's local-vars list, since it's a local variable."
  (wg-aget (wg-buf-local-vars buf) 'major-mode))

(defun wg-bufobj-major-mode (bufobj)
  "Return BUFOBJ's major-mode."
  (cl-etypecase bufobj
    (buffer (wg-buffer-major-mode bufobj))
    (wg-buf (wg-buf-major-mode bufobj))
    (string (wg-buffer-major-mode bufobj))))

;; `wg-equal-bufobjs' and `wg-find-bufobj' may need to be made a lot smarter
(defun wg-equal-bufobjs (bufobj1 bufobj2)
  "Return t if BUFOBJ1 is \"equal\" to BUFOBJ2."
  (let ((fname1 (wg-bufobj-file-name bufobj1))
        (fname2 (wg-bufobj-file-name bufobj2)))
    (cond ((and fname1 fname2) (string= fname1 fname2))
          ((or fname1 fname2) nil)
          ((string= (wg-bufobj-name bufobj1) (wg-bufobj-name bufobj2)) t))))

(defun wg-find-bufobj (bufobj bufobj-list)
  "Find BUFOBJ in BUFOBJ-LIST, testing with `wg-equal-bufobjs'."
  (cl-find bufobj bufobj-list :test 'wg-equal-bufobjs))

(defun wg-find-bufobj-by-uid (uid bufobj-list)
  "Find the bufobj in BUFOBJ-LIST with uid UID."
  (cl-find uid bufobj-list :test 'string= :key 'wg-bufobj-uid))

(defun wg-find-buf-in-buf-list (buf buf-list)
  "Find BUF in BUF-LIST.
This is only here for completeness."
  (cl-find buf buf-list))

(defun wg-find-buffer-in-buffer-list (buffer-or-name buffer-list)
  "Find BUFFER-OR-NAME in BUFFER-LIST."
  (cl-find (wg-get-buffer buffer-or-name) buffer-list :key 'wg-get-buffer))

(defun wg-find-buffer-in-buf-list (buffer-or-name buf-list)
  "Find BUFFER-OR-NAME in BUF-LIST."
  (wg-aif (wg-buffer-uid buffer-or-name)
      (wg-find-bufobj-by-uid it buf-list)
    (wg-find-bufobj buffer-or-name buf-list)))

(defun wg-find-buf-in-buffer-list (buf buffer-list)
  "Find BUF in BUFFER-LIST."
  (or (wg-find-bufobj-by-uid (wg-buf-uid buf) buffer-list)
      (wg-find-bufobj buf buffer-list)))

(defun wg-find-buf-by-uid (uid)
  "Find a buf in `wg-buf-list' by UID."
  (wg-find-bufobj-by-uid uid (wg-buf-list)))

(defun wg-set-buffer-uid-or-error (uid &optional buffer)
  "Set BUFFER's buffer local value of `wg-buffer-uid' to UID.
If BUFFER already has a buffer local value of `wg-buffer-uid',
and it's not equal to UID, error."
  (if wg-buffer-uid
      ;;(if (string= wg-buffer-uid uid) uid
      ;;  (error "uids don't match %S and %S" uid wg-buffer-uid))
    (setq wg-buffer-uid uid)))




;;; wconfig construction

(defun wg-buffer-special-data (buffer)
  "Return BUFFER's auxiliary serialization, or nil."
  (cl-some (lambda (fn) (funcall fn buffer)) wg-special-buffer-serdes-functions))

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-serialize-buffer-local-variables ()
  "Return an alist of buffer-local variable symbols and their values.
See `wg-buffer-local-variables-alist' for details."
  (wg-docar (entry wg-buffer-local-variables-alist)
    (wg-dbind (var ser des) entry
      (when (local-variable-p var)
        (cons var (if ser (funcall ser) (symbol-value var)))))))

(defun wg-buffer-to-buf (buffer)
  "Return the serialization (a wg-buf) of Emacs buffer BUFFER."
  (with-current-buffer buffer
    (wg-make-buf
     :name           (buffer-name)
     :file-name      (buffer-file-name)
     :point          (point)
     :mark           (mark)
     :local-vars     (wg-serialize-buffer-local-variables)
     :special-data   (wg-buffer-special-data buffer))))

(defun wg-add-buffer-to-buf-list (buffer)
  "Make a buf from BUFFER, and add it to `wg-buf-list' if necessary.
If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it.  Return BUFFER's uid
in either case."
  (with-current-buffer buffer
    (setq wg-buffer-uid
          (wg-aif (wg-find-buffer-in-buf-list buffer (wg-buf-list))
              (wg-buf-uid it)
            (let ((buf (wg-buffer-to-buf buffer)))
              (push buf (wg-buf-list))
              (wg-buf-uid buf))))))

(defun wg-buffer-uid-or-add (buffer)
  "If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it.  Return BUFFER's uid
in either case."
  (or (wg-buffer-uid buffer) (wg-add-buffer-to-buf-list buffer)))

(defun wg-bufobj-uid-or-add (bufobj)
  "If BUFOBJ is a wg-buf, return its uid.
If BUFOBJ is a buffer or a buffer name, see `wg-buffer-uid-or-add'."
  (cl-etypecase bufobj
    (wg-buf (wg-buf-uid bufobj)) ;; possibly also add to `wg-buf-list'
    (buffer (wg-buffer-uid-or-add bufobj))
    (string (wg-bufobj-uid-or-add (wg-get-buffer bufobj)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notes on buffer and window properties:
;;
;; fringes, margins and scroll-bars are properly properties of buffers, but
;; their settings can be forced ephemerally in a window with the set-window-foo
;; functions.
;;
;; window-point is a property of a buffer/window pair, but won't set properly
;; unless the buffer is current -- i.e. (set-window-buffer some-window
;; some-buffer) (set-window-point some-window 0)) won't set some-buffer's point
;; in some-window unless some-buffer is also current.
;;
;; window-start and window-hscroll are properties of buffer/window pairs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun wg-window-to-win (window)
  "Return the serialization (a wg-win) of Emacs window WINDOW."
  (let ((selected (eq window (selected-window))))
    (with-selected-window window
      (wg-make-win
       :edges              (window-edges window)
       :point              (wg-window-point window)
       :start              (window-start window)
       :hscroll            (window-hscroll window)
       :selected           selected
       :minibuffer-scroll  (eq window minibuffer-scroll-window)
       :dedicated          (window-dedicated-p window)
       :buf-uid            (wg-buffer-uid-or-add (window-buffer window))))))

(defun wg-window-tree-to-wtree (window-tree)
  "Return the serialization (a wg-wtree) of Emacs window tree WINDOW-TREE."
  (wg-barf-on-active-minibuffer)
  (cl-labels
      ((inner (w) (if (windowp w) (wg-window-to-win w)
                    (wg-dbind (dir edges . wins) w
                      (wg-make-wtree
                       :dir    dir
                       :edges  edges
                       :wlist  (mapcar #'inner wins))))))
    (let ((w (car window-tree)))
      (when (and (windowp w) (window-minibuffer-p w))
        (error "Workgroups can't operate on minibuffer-only frames."))
      (inner w))))

(defun wg-frame-to-wconfig (&optional frame)
  "Return the serialization (a wg-wconfig) of Emacs frame FRAME.
FRAME nil defaults to `selected-frame'."
  (let* ((frame (or frame (selected-frame)))
         (fullscrn (frame-parameter frame 'fullscreen)))
    (wg-make-wconfig
     :left                  (frame-parameter frame 'left)
     :top                   (frame-parameter frame 'top)
     :width                 (frame-parameter frame 'width)
     :height                (frame-parameter frame 'height)
     :parameters            `((fullscreen . ,fullscrn))
     :vertical-scroll-bars  (frame-parameter frame 'vertical-scroll-bars)
     :scroll-bar-width      (frame-parameter frame 'scroll-bar-width)
     :wtree                 (wg-window-tree-to-wtree (window-tree frame))
     )))

(defun wg-current-wconfig ()
  "Return the current wconfig.
If `wg-current-wconfig' is non-nil, return it.  Otherwise return
`wg-frame-to-wconfig'."
  (or (frame-parameter nil 'wg-current-wconfig)
      (wg-frame-to-wconfig)))

(defmacro wg-with-current-wconfig (frame wconfig &rest body)
  "Eval BODY with WCONFIG current in FRAME.
FRAME nil defaults to `selected-frame'."
  (declare (indent 2))
  (wg-with-gensyms (frame-sym old-value)
    `(let* ((,frame-sym (or ,frame (selected-frame)))
            (,old-value (frame-parameter ,frame-sym 'wg-current-wconfig)))
       (unwind-protect
           (progn
             (set-frame-parameter ,frame-sym 'wg-current-wconfig ,wconfig)
             ,@body)
         (when (frame-live-p ,frame-sym)
           (set-frame-parameter ,frame-sym 'wg-current-wconfig ,old-value))))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-frame-to-wconfig)))


;;; win/wtree/wconfig utils

(defun wg-w-edges (w)
  "Return W's edge list."
  (cl-etypecase w
    (wg-win (wg-win-edges w))
    (wg-wtree (wg-wtree-edges w))))

(defun wg-copy-w (w)
  "Return a copy of W.  W should be a wg-win or a wg-wtree."
  (cl-etypecase w
    (wg-win (wg-copy-win w))
    (wg-wtree (wg-copy-wtree w))))

(defun wg-set-edges (w edges)
  "Set W's edge list, and return W."
  (cl-etypecase w
    (wg-win (setf (wg-win-edges w) edges))
    (wg-wtree (setf (wg-wtree-edges w) edges)))
  w)

(defun wg-min-size (dir)
  "Return the minimum window size in split direction DIR."
  (if dir wg-window-min-height wg-window-min-width))

(defun wg-actual-min-size (dir)
  "Return the actual minimum window size in split direction DIR."
  (if dir wg-actual-min-height wg-actual-min-width))

(defmacro wg-with-edges (w spec &rest body)
  "Bind W's edge list to SPEC and eval BODY."
  (declare (indent 2))
  `(wg-dbind ,spec (wg-w-edges ,w) ,@body))

(defmacro wg-with-bounds (wtree dir spec &rest body)
  "Bind SPEC to W's bounds in DIR, and eval BODY.
\"bounds\" are a direction-independent way of dealing with edge lists."
  (declare (indent 3))
  (wg-with-gensyms (dir-sym l1 t1 r1 b1)
    (wg-dbind (ls1 hs1 lb1 hb1) spec
      `(wg-with-edges ,wtree (,l1 ,t1 ,r1 ,b1)
         (cond (,dir (let ((,ls1 ,l1) (,hs1 ,r1) (,lb1 ,t1) (,hb1 ,b1))
                       ,@body))
               (t    (let ((,ls1 ,t1) (,hs1 ,b1) (,lb1 ,l1) (,hb1 ,r1))
                       ,@body)))))))

(defun wg-set-bounds (w dir ls hs lb hb)
  "Set W's edges in DIR with bounds LS HS LB and HB."
  (wg-set-edges w (if dir (list ls lb hs hb) (list lb ls hb hs))))

(defun wg-step-edges (edges1 edges2 hstep vstep)
  "Return W1's edges stepped once toward W2's by HSTEP and VSTEP."
  (wg-dbind (l1 t1 r1 b1) edges1
    (wg-dbind (l2 t2 r2 b2) edges2
      (let ((left (wg-step-to l1 l2 hstep))
            (top  (wg-step-to t1 t2 vstep)))
        (list left top
              (+ left (wg-step-to (- r1 l1) (- r2 l2) hstep))
              (+ top  (wg-step-to (- b1 t1) (- b2 t2) vstep)))))))

(defun wg-w-edge-operation (w edges op)
  "Return a copy of W with its edges mapped against EDGES through OP."
  (wg-set-edges w (cl-mapcar op (wg-w-edges w) edges)))

(defun wg-first-win (w)
  "Return the first actual window in W."
  (if (wg-win-p w) w
    (wg-first-win (car (wg-wtree-wlist w)))))

(defun wg-last-win (w)
  "Return the last actual window in W."
  (if (wg-win-p w) w
    (wg-last-win (wg-last1 (wg-wtree-wlist w)))))

(defun wg-minify-win (w)
  "Set W's edges to the smallest allowable."
  (let* ((edges (wg-w-edges w))
         (left (car edges))
         (top (cadr edges)))
    (wg-set-edges w (list left top
                          (+ left wg-actual-min-width)
                          (+ top  wg-actual-min-height)))))

(defun wg-minified-copy-of-last-win (w)
  "Minify a copy of the last actual window in W."
  (wg-minify-win (wg-copy-win (wg-last-win w))))

(defun wg-w-size (w &optional height)
  "Return the width or height of W, calculated from its edge list."
  (wg-with-edges w (l1 t1 r1 b1)
    (if height (- b1 t1) (- r1 l1))))

(defun wg-adjust-w-size (w width-fn height-fn &optional new-left new-top)
  "Adjust W's width and height with WIDTH-FN and HEIGHT-FN."
  (wg-with-edges w (left top right bottom)
    (let ((left (or new-left left)) (top (or new-top top)))
      (wg-set-edges (wg-copy-w w)
                    (list left
                          top
                          (+ left (funcall width-fn  (- right  left)))
                          (+ top  (funcall height-fn (- bottom top))))))))

(defun wg-scale-w-size (w width-scale height-scale)
  "Scale W's size by WIDTH-SCALE and HEIGHT-SCALE."
  (cl-labels
      ((wscale (width)  (truncate (* width  width-scale)))
       (hscale (height) (truncate (* height height-scale))))
    (wg-adjust-w-size w #'wscale #'hscale)))

(defun wg-equal-wtrees (w1 w2)
  "Return t when W1 and W2 have equal structure."
  (cond ((and (wg-win-p w1) (wg-win-p w2))
         (equal (wg-w-edges w1) (wg-w-edges w2)))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-wtree-dir w1) (wg-wtree-dir w2))
              (equal (wg-wtree-edges w1) (wg-wtree-edges w2))
              (cl-every #'wg-equal-wtrees
                     (wg-wtree-wlist w1)
                     (wg-wtree-wlist w2))))))

(defun wg-normalize-wtree (wtree)
  "Clean up and return a new wtree from WTREE.
Recalculate the edge lists of all subwins, and remove subwins
outside of WTREE's bounds.  If there's only one element in the
new wlist, return it instead of a new wtree."
  (if (wg-win-p wtree) wtree
    (wg-with-slots wtree ((dir wg-wtree-dir)
                          (wlist wg-wtree-wlist))
      (wg-with-bounds wtree dir (ls1 hs1 lb1 hb1)
        (let* ((min-size (wg-min-size dir))
               (max (- hb1 1 min-size))
               (lastw (wg-last1 wlist)))
          (cl-labels
              ((mapwl
                (wl)
                (wg-dbind (sw . rest) wl
                  (cons (wg-normalize-wtree
                         (wg-set-bounds
                          sw dir ls1 hs1 lb1
                          (setq lb1 (if (eq sw lastw) hb1
                                      (let ((hb2 (+ lb1 (wg-w-size sw dir))))
                                        (if (>= hb2 max) hb1 hb2))))))
                        (when (< lb1 max) (mapwl rest))))))
            (let ((new (mapwl wlist)))
              (if (not (cdr new)) (car new)
                (setf (wg-wtree-wlist wtree) new)
                wtree))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-w-size wtree wscale hscale)))
    (if (wg-win-p wtree) scaled
      (wg-asetf (wg-wtree-wlist scaled)
                (wg-docar (sw it) (wg-scale-wtree sw wscale hscale)))
      scaled)))

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wconfig-wtree wconfig)
    (/ (float new-width)  (wg-wconfig-width wconfig))
    (/ (float new-height) (wg-wconfig-height wconfig)))))
;; (wg-wconfig-width (wg-current-wconfig))

(defun wg-resize-frame-scale-wtree (wconfig)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (selected-frame)))
    (wg-with-slots wconfig ((wcwidth wg-wconfig-width)
                            (wcheight wg-wconfig-height))
      (when window-system (set-frame-size frame wcwidth wcheight))
      (let ((fwidth  (frame-parameter frame 'width))
            (fheight (frame-parameter frame 'height)))
        (if (and (= wcwidth fwidth) (= wcheight fheight))
            (wg-wconfig-wtree wconfig)
          (wg-scale-wconfigs-wtree wconfig fwidth fheight))))))

(defun wg-reverse-wlist (w &optional dir)
  "Reverse W's wlist and those of all its sub-wtrees in direction DIR.
If DIR is nil, reverse WTREE horizontally.
If DIR is 'both, reverse WTREE both horizontally and vertically.
Otherwise, reverse WTREE vertically."
  (cl-labels
      ((inner (w) (if (wg-win-p w) w
                    (wg-with-slots w ((d1 wg-wtree-dir))
                      (wg-make-wtree
                       :dir d1
                       :edges (wg-wtree-edges w)
                       :wlist (let ((wl2 (mapcar #'inner (wg-wtree-wlist w))))
                                (if (or (eq dir 'both) (eq dir d1))
                                    (nreverse wl2)
                                  wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (cl-labels
      ((inner (w) (if (wg-win-p w) w
                    (wg-with-slots w ((wlist wg-wtree-wlist))
                      (wg-make-wtree
                       :dir (wg-wtree-dir w)
                       :edges (wg-wtree-edges w)
                       :wlist (wg-aif (cl-find t wlist :key 'wg-win-selected)
                                  (wg-cyclic-offset-elt it wlist offset)
                                (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))

(defun wg-reverse-wconfig (wconfig &optional dir)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-reverse-wlist it dir))
  wconfig)

(defun wg-wconfig-move-window (wconfig offset)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-wtree-move-window it offset))
  wconfig)

(defun wg-flatten-wtree (wtree &optional key)
  "Return a new list by flattening WTREE.
KEY non returns returns a list of WTREE's wins.
KEY non-nil returns a list of the results of calling KEY on each win."
  (cl-labels
      ((inner (w) (if (wg-win-p w) (list (if key (funcall key w) w))
                    (cl-mapcan #'inner (wg-wtree-wlist w)))))
    (inner wtree)))

(defun wg-win-list (wtree)
  "Construct and return a list of all wg-wins in WTREE."
  (wg-flatten-wtree wtree))


(require 'workgroups-specialbufs)
(require 'workgroups-restore)


;;; workgroup utils

(defun wg-flag-workgroup-modified (workgroup)
  "Set WORKGROUP's and the current session's modified flags."
  (when wg-flag-modified
    (setf (wg-workgroup-modified workgroup) t)
    (setf (wg-session-modified (wg-current-session)) t)))

(defun wg-find-workgroup-by (slotkey value &optional noerror)
  "Return the workgroup on which ACCESSOR returns VALUE or error."
  (let ((accessor (cl-ecase slotkey
                    (:name 'wg-workgroup-name)
                    (:uid  'wg-workgroup-uid))))
    (or (cl-find value (wg-workgroup-list-or-error noerror) :test 'equal :key accessor)
        (unless noerror
          (error "No are no workgroups with a %S of %S"
                 accessor value)))))

(defun wg-first-workgroup ()
  "Return a first workgroup."
  (interactive)
  (car (wg-workgroup-list-or-error)))

(defun wg-current-workgroup (&optional noerror frame)
  "Return the current workgroup in FRAME, or error unless NOERROR."
  (or wg-current-workgroup
      (wg-aif (frame-parameter frame 'wg-current-workgroup-uid)
          (wg-find-workgroup-by :uid it noerror)
        (unless noerror (error "No current workgroup in this frame")))))

(defun wg-previous-workgroup (&optional noerror frame)
  "Return the previous workgroup in FRAME, or error unless NOERROR."
  (wg-aif (frame-parameter frame 'wg-previous-workgroup-uid)
      (wg-find-workgroup-by :uid it noerror)
    (unless noerror (error "No previous workgroup in this frame"))))

(defun wg-set-current-workgroup (workgroup &optional frame)
  "Set the current workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-current-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-set-previous-workgroup (workgroup &optional frame)
  "Set the previous workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-previous-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-current-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup t frame)
    (eq workgroup it)))

(defun wg-previous-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup t frame)
    (eq workgroup it)))

(defmacro wg-with-current-workgroup (workgroup &rest body)
  "Execute forms in BODY with WORKGROUP temporarily current.
WORKGROUP should be any workgroup identifier accepted by
`wg-get-workgroup'.  The value returned is the last form
in BODY."
  (declare (indent 1))
  `(let ((wg-current-workgroup (wg-get-workgroup ,workgroup)))
     ,@body))

(defun wg-get-workgroup (obj &optional noerror)
  "Return a workgroup from OBJ.
If OBJ is a workgroup, return it.
If OBJ is a string, return the workgroup named OBJ, or error unless NOERROR.
If OBJ is nil, return the current workgroup, or error unless NOERROR."
  (cond ((wg-workgroup-p obj) obj)
        ((stringp obj) (wg-find-workgroup-by :name obj noerror))
        ((null obj) (wg-current-workgroup noerror))
        (t (error "Can't get workgroup from type:: %S" (type-of obj)))))



;;; workgroup parameters
;;
;; Quick test:
;; (wg-workgroup-parameters (wg-current-workgroup))
;; (wg-set-workgroup-parameter (wg-current-workgroup) 'test1 t)
;; (wg-workgroup-parameter (wg-current-workgroup) 'test1)
(defun wg-workgroup-parameter (workgroup parameter &optional default)
  "Return WORKGROUP's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
WORKGROUP should be accepted by `wg-get-workgroup'."
  (wg-aget (wg-workgroup-parameters (wg-get-workgroup workgroup))
           parameter default))

(defun wg-set-workgroup-parameter (workgroup parameter value)
  "Set WORKGROUP's value of PARAMETER to VALUE.
WORKGROUP should be a value accepted by `wg-get-workgroup'.
Return VALUE."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-set-parameter (wg-workgroup-parameters workgroup) parameter value)
    (wg-flag-workgroup-modified workgroup)
    value))

(defun wg-remove-workgroup-parameter (workgroup parameter)
  "Remove PARAMETER from WORKGROUP's parameters."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-flag-workgroup-modified workgroup)
    (wg-asetf (wg-workgroup-parameters workgroup) (wg-aremove it parameter))))

(defun wg-workgroup-local-value (variable &optional workgroup)
  "Return the value of VARIABLE in WORKGROUP.
WORKGROUP nil defaults to the current workgroup.  If there is no
current workgroup, or if VARIABLE does not have a workgroup-local
binding in WORKGROUP, resolve VARIABLE with `wg-session-local-value'."
  (let ((workgroup (wg-get-workgroup workgroup t)))
    (if (not workgroup) (wg-session-local-value variable)
      (let* ((undefined (cl-gensym))
             (value (wg-workgroup-parameter workgroup variable undefined)))
        (if (not (eq value undefined)) value
          (wg-session-local-value variable))))))

(defalias 'wg-local-value 'wg-workgroup-local-value)


;;; workgroup associated buffers

(defun wg-workgroup-associated-buf-uids (workgroup)
  "Return a new list containing all of WORKGROUP's associated buf uids."
  (append (wg-workgroup-strong-buf-uids workgroup)
          (wg-workgroup-weak-buf-uids workgroup)))

(defun wg-workgroup-associated-bufs (workgroup)
  "Return a new list containing all of WORKGROUP's associated bufs."
  (delete nil (mapcar 'wg-find-buf-by-uid
                      (wg-workgroup-associated-buf-uids workgroup))))

(defun wg-workgroup-associated-buffers (workgroup &optional initial names)
  "Return a list of WORKGROUP's live associated buffers."
  (let ((assoc-bufs (wg-workgroup-associated-bufs workgroup)))
    (cl-remove-if-not
     (lambda (buffer) (wg-find-buffer-in-buf-list buffer assoc-bufs))
     (or initial (buffer-list)))))

(defun wg-workgroup-bufobj-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (let ((uid (wg-bufobj-uid-or-add bufobj)))
    (or (and (member uid (wg-workgroup-strong-buf-uids workgroup)) 'strong)
        (and (member uid (wg-workgroup-weak-buf-uids workgroup)) 'weak))))

(defun wg-workgroup-strongly-associate-bufobj (workgroup bufobj)
  "Strongly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-weakly-associate-bufobj (workgroup bufobj)
  "Weakly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-dissociate-bufobj (workgroup bufobj)
  "Dissociate BUFOBJ from WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (rem1p (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                              :test 'string=))
         (rem2p (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                              :test 'string=)))
    (wg-awhen (or rem1p rem2p)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-associate-bufobj (workgroup bufobj &optional weak)
  "Associate BUFOBJ with WORKGROUP.
WEAK non-nil means weakly associate it.  Otherwise strongly associate it."
  (if weak (wg-workgroup-weakly-associate-bufobj workgroup bufobj)
    (wg-workgroup-strongly-associate-bufobj workgroup bufobj)))

(defun wg-workgroup-cycle-bufobj-association-type (workgroup bufobj)
  "Cycle the BUFOBJ's association type in WORKGROUP.
If it's strongly associated with the workgroup, weakly associate it.
If it's weakly associated with the workgroup, dissociate it.
If it's unassociated with the workgroup, mark it as strongly associated."
  (cl-case (wg-workgroup-bufobj-association-type workgroup bufobj)
    (strong (wg-workgroup-weakly-associate-bufobj workgroup bufobj) 'weak)
    (weak (wg-workgroup-dissociate-bufobj workgroup bufobj) nil)
    (otherwise (wg-workgroup-strongly-associate-bufobj workgroup bufobj) 'strong)))

(defun wg-workgroup-dissociate-weakly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all weakly associated buffers."
  (when (wg-workgroup-weak-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-weak-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-strongly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all strongly associated buffers."
  (when (wg-workgroup-strong-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-strong-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-all-buffers (workgroup)
  "Dissociate from WORKGROUP all its associated buffers."
  (wg-workgroup-dissociate-weakly-associated-buffers workgroup)
  (wg-workgroup-dissociate-strongly-associated-buffers workgroup))

(defun wg-auto-dissociate-buffer-hook ()
  "`kill-buffer-hook' that automatically dissociates buffers from workgroups."
  (when wg-dissociate-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-bufobj it (current-buffer)))))



;;; filtered buffer-list construction

(defun wg-get-buffer-list-filter-id-flexibly (blf-id)
  "Return a buffer-list-filter-id one way or another."
  (or blf-id wg-current-buffer-list-filter-id 'all))

(defun wg-get-buffer-list-filter-val (id key &optional noerror)
  "Return ID's KEY's value in `wg-buffer-list-filter-definitions'.
Lots of possible errors here because
`wg-buffer-list-filter-definitions' can be modified by the user."
  (let ((slot-num (cl-case key (symbol 0) (indicator 1) (constructor 2))))
    (if (not slot-num)
        (unless noerror
          (error "`%S' is not a valid buffer-list-filter definition slot" key))
      (let* ((id (wg-get-buffer-list-filter-id-flexibly id))
             (entry (assq id (wg-local-value
                              'wg-buffer-list-filter-definitions))))
        (if (not entry)
            (unless noerror
              (error "`%S' is an undefined buffer-list-filter" id))
          (or (nth slot-num entry)
              (unless noerror
                (error "Slot `%S' is undefined in `%S's definition"
                       key id))))))))

(defun wg-filtered-buffer-list (&optional names workgroup bfl-id initial)
  "Return a filtered buffer-list from NAMES, WORKGROUP, BLF-ID and INITIAL.
NAMES non-nil means return a list of buffer-names instead of buffer objects.
WORKGROUP non-nil should be any workgroup identifier accepted by
`wg-get-workgroup'.
BLF-ID non-nil should be the identifier of a defined buffer-list-filter.
It defaults to `wg-get-buffer-list-filter-val'.
INITIAL non-nil should be an initial buffer-list to filter.  It defaults to
`wg-interesting-buffers'."
  (let ((buffer-list (funcall (wg-get-buffer-list-filter-val
                               (wg-get-buffer-list-filter-id-flexibly bfl-id)
                               'constructor)
                              (wg-get-workgroup workgroup)
                              (or initial (wg-interesting-buffers)))))
    (if names (mapcar 'wg-buffer-name buffer-list) buffer-list)))


;; buffer-list filters

(defun wg-buffer-list-filter-all (workgroup initial)
  "Return all buffers in INITIAL."
  initial)

(defun wg-buffer-list-filter-associated (workgroup initial)
  "Return only those buffers associated with WORKGROUP."
  (wg-workgroup-associated-buffers workgroup initial))

(defun wg-buffer-list-filter-unassociated (workgroup initial)
  "Return only those buffer unassociated with WORKGROUP."
  (let ((buffers (wg-workgroup-associated-buffers workgroup initial)))
    (cl-remove-if (lambda (buffer) (member buffer buffers)) initial)))


;; buffer-list filtration utils

(defun wg-filter-buffer-list-by-regexp (regexp buffer-list)
  "Return only those buffers in BUFFER-LIST with names matching REGEXP."
  (cl-remove-if-not (lambda (bname) (string-match regexp bname))
                 buffer-list :key 'buffer-name))

(defun wg-filter-buffer-list-by-root-dir (root-dir buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files undo ROOT-DIR."
  (cl-remove-if-not (lambda (f) (when f (wg-file-under-root-path-p root-dir f)))
                 buffer-list :key 'buffer-file-name))

(defun wg-filter-buffer-list-by-major-mode (major-mode buffer-list)
  "Return only those buffers in BUFFER-LIST in major-mode MAJOR-MODE."
  (cl-remove-if-not (lambda (mm) (eq mm major-mode))
                 buffer-list :key 'wg-buffer-major-mode))


;; Example custom buffer-list-filters

(defun wg-buffer-list-filter-irc (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST with names starting in \"#\"."
  (wg-filter-buffer-list-by-regexp "^#" buffer-list))

(defun wg-buffer-list-filter-home-dir (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files under ~/."
  (wg-filter-buffer-list-by-root-dir "~/" buffer-list))

(defun wg-buffer-list-filter-elisp (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `emacs-lisp-mode'."
  (wg-filter-buffer-list-by-major-mode 'emacs-lisp-mode buffer-list))

(defun wg-buffer-list-filter-home-dir->elisp (workgroup buffer-list)
  "Example of chaining buffer-list filters.
This returns all buffers under \"~/\" that are also in `emacs-lisp-mode'."
  (wg-buffer-list-filter-elisp
   nil (wg-buffer-list-filter-home-dir nil buffer-list)))


;; buffer-list-filter context

(defun wg-buffer-list-filter-order (command)
  "Return WORKGROUP's buffer-list-filter order for COMMAND, or a default."
  (let ((bfo (wg-local-value 'wg-buffer-list-filter-order-alist)))
    (or (wg-aget bfo command) (wg-aget bfo 'default))))

(defmacro wg-prior-mapping (mode command)
  "Return whatever COMMAND would call if MODE wasn't on."
  `(or (let (,mode) (command-remapping ,command)) ,command))

(defun wg-filter-buffer-list-p ()
  "Return the current workgroup when buffer-list-filters are on."
  (and workgroups-mode wg-buffer-list-filtration-on (wg-current-workgroup t)))

(defmacro wg-with-buffer-list-filters (command &rest body)
  "Create buffer-list filter context for COMMAND, and eval BODY.
Binds `wg-current-buffer-list-filter-id' in BODY."
  (declare (indent 1))
  (wg-with-gensyms (order status)
    `(let* ((wg-previous-minibuffer-contents nil)
            (,order (wg-buffer-list-filter-order ,command)))
       (catch 'wg-result
         (while 'your-mom
           (let* ((wg-current-buffer-list-filter-id (car ,order))
                  (,status (catch 'wg-action (list 'done (progn ,@body)))))
             (cl-case (car ,status)
               (done (throw 'wg-result (cadr ,status)))
               (next (setq ,order (wg-rotate-list ,order 1))
                     (setq wg-previous-minibuffer-contents (cadr ,status)))
               (prev (setq ,order (wg-rotate-list ,order -1))
                     (setq wg-previous-minibuffer-contents
                           (cadr ,status))))))))))


;;; workgroup working-wconfig and wconfig undo/redo

(defun wg-workgroup-state-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-state-table)
      (let ((wtree (make-hash-table :test 'equal)))
        (set-frame-parameter frame 'wg-workgroup-state-table wtree)
        wtree)))

(defun wg-get-workgroup-state (workgroup &optional frame)
  "Return FRAME's WORKGROUP's state table."
  (let ((uid (wg-workgroup-uid workgroup))
        (state-table (wg-workgroup-state-table frame)))
    (or (gethash uid state-table)
        (let ((wgs (wg-make-workgroup-state
                    :undo-pointer 0
                    :undo-list
                    (list (or (wg-workgroup-selected-frame-wconfig workgroup)
                              (wg-workgroup-base-wconfig workgroup))))))
          (puthash uid wgs state-table)
          wgs))))

(defmacro wg-with-undo (workgroup spec &rest body)
  "Bind WORKGROUP's undo state to SPEC and eval BODY."
  (declare (indent 2))
  (wg-dbind (state undo-pointer undo-list) spec
    `(let* ((,state (wg-get-workgroup-state ,workgroup))
            (,undo-pointer (wg-workgroup-state-undo-pointer ,state))
            (,undo-list (wg-workgroup-state-undo-list ,state)))
       ,@body)))

(defun wg-flag-just-exited-minibuffer ()
  "Added to `minibuffer-exit-hook'."
  (setq wg-just-exited-minibuffer t))

(defun wg-flag-window-configuration-changed ()
  "Set `wg-window-configuration-changed' to t unless the
minibuffer was just exited.  Added to
`window-configuration-change-hook'."
  (if wg-just-exited-minibuffer
      (setq wg-just-exited-minibuffer nil)
    (setq wg-window-configuration-changed t)))

(defun wg-unflag-undoify-window-configuration-change ()
  "Set `wg-undoify-window-configuration-change' to nil, exempting
from undoification any window-configuration changes caused by the
current command."
  (setq wg-undoify-window-configuration-change nil))

(defun wg-set-workgroup-working-wconfig (workgroup wconfig)
  "Set the working-wconfig of WORKGROUP to WCONFIG."
  (wg-flag-workgroup-modified workgroup)
  (setf (wg-workgroup-selected-frame-wconfig workgroup) wconfig)
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) wconfig)))

(defun wg-add-wconfig-to-undo-list (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's undo list, truncating its future if necessary."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((undo-list (cons nil (nthcdr undo-pointer undo-list))))
      (wg-awhen (nthcdr wg-wconfig-undo-list-max undo-list) (setcdr it nil))
      (setf (wg-workgroup-state-undo-list state) undo-list))
    (setf (wg-workgroup-state-undo-pointer state) 0))
  (wg-set-workgroup-working-wconfig workgroup wconfig))

(defun wg-workgroup-working-wconfig (workgroup &optional noupdate)
  "Return WORKGROUP's working-wconfig.
If WORKGROUP is the current workgroup in `selected-frame', and
NOUPDATE is nil, set its working wconfig in `selected-frame' to
`wg-current-wconfig' and return the updated wconfig.  Otherwise
return WORKGROUP's current undo state."
  (if (and (not noupdate) (wg-current-workgroup-p workgroup))
      (wg-set-workgroup-working-wconfig workgroup (wg-current-wconfig))
    (wg-with-undo workgroup (state undo-pointer undo-list)
      (nth undo-pointer undo-list))))

(defun wg-update-current-workgroup-working-wconfig ()
  "Update `selected-frame's current workgroup's working-wconfig
with `wg-current-wconfig'."
  (wg-awhen (wg-current-workgroup t)
    (wg-set-workgroup-working-wconfig it (wg-current-wconfig))))

(defun wg-restore-wconfig-undoably (wconfig &optional noundo)
  "Restore WCONFIG in `selected-frame', saving undo information."
  (when noundo (wg-unflag-undoify-window-configuration-change))
  (wg-update-current-workgroup-working-wconfig)
  (wg-restore-wconfig wconfig))

(defun wg-workgroup-offset-position-in-undo-list (workgroup increment)
  "Increment WORKGROUP's undo-pointer by INCREMENT.
Also restore the wconfig at the incremented undo-pointer if
WORKGROUP is current."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((new-pointer (+ undo-pointer increment)))
      (when (wg-within new-pointer 0 (length undo-list))
        (when (wg-current-workgroup-p workgroup)
          (wg-restore-wconfig-undoably (nth new-pointer undo-list) t))
        (setf (wg-workgroup-state-undo-pointer state) new-pointer)))))

(defun wg-undoify-window-configuration-change ()
  "Conditionally `wg-add-wconfig-to-undo-list'.
Added to `post-command-hook'."
  (when (and
         ;; When the window config has changed,
         wg-window-configuration-changed
         ;; and undoification is still on for the current command
         wg-undoify-window-configuration-change
         ;; and the change didn't occur while the minibuffer is active,
         (wg-minibuffer-inactive-p))
    ;; and there's a current workgroup,
    (wg-when-let ((workgroup (wg-current-workgroup t)))
      ;; add the current wconfig to that workgroup's undo list:
      (wg-add-wconfig-to-undo-list workgroup (wg-current-wconfig))))
  ;; Reset all flags no matter what:
  (setq wg-window-configuration-changed nil
        wg-undoify-window-configuration-change t
        wg-already-updated-working-wconfig nil))

(defun wg-update-working-wconfig-hook ()
  "Used in before advice on all functions that trigger
`window-configuration-change-hook' to save up to date undo info
before the change."
  (when (and (not wg-already-updated-working-wconfig)
             (wg-minibuffer-inactive-p))
    (wg-update-current-workgroup-working-wconfig)
    (setq wg-already-updated-working-wconfig t)))



;;; base wconfig updating

(defun wg-update-all-base-wconfigs ()
  "Update every workgroup's base wconfig with
`wg-workgroup-update-base-wconfig'."
  (dolist (workgroup (wg-workgroup-list))
    (wg-awhen (wg-workgroup-selected-frame-wconfig workgroup)
      (setf (wg-workgroup-base-wconfig workgroup) it
            (wg-workgroup-selected-frame-wconfig workgroup) nil))))

(defun wg-update-working-wconfig-on-delete-frame (frame)
  "Update FRAME's current workgroup's working-wconfig before
FRAME is deleted, so we don't lose its state."
  (with-selected-frame frame
    (wg-update-current-workgroup-working-wconfig)))



;;; workgroup saved wconfigs

(defun wg-workgroup-saved-wconfig-names (workgroup)
  "Return a new list of the names of all WORKGROUP's saved wconfigs."
  (mapcar 'wg-wconfig-name (wg-workgroup-saved-wconfigs workgroup)))

(defun wg-workgroup-get-saved-wconfig (workgroup wconfig-or-name)
  "Return the wconfig in WORKGROUP's saved wconfigs named WCONFIG-OR-NAME.
WCONFIG-OR-NAME must be either a string or a wconfig.  If
WCONFIG-OR-NAME is a string and there is no saved wconfig with
that name, return nil.  If WCONFIG-OR-NAME is a wconfig, and it
is a member of WORKGROUP's saved wconfigs, return is as given.
Otherwise return nil."
  (let ((wconfigs (wg-workgroup-saved-wconfigs workgroup)))
    (cl-etypecase wconfig-or-name
      (wg-wconfig (car (memq wconfig-or-name wconfigs)))
      (string (cl-find wconfig-or-name wconfigs
                    :key 'wg-wconfig-name
                    :test 'string=)))))

(defun wg-workgroup-save-wconfig (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's saved wconfigs.  WCONFIG must have
a name.  If there's already a wconfig with the same name in
WORKGROUP's saved wconfigs, replace it."
  (let ((name (wg-wconfig-name wconfig)))
    (unless name (error "Attempt to save a nameless wconfig"))
    (setf (wg-workgroup-modified workgroup) t)
    (wg-asetf (wg-workgroup-saved-wconfigs workgroup)
              (cons wconfig (cl-remove name it
                                       :key 'wg-wconfig-name
                                       :test 'string=)))))

(defun wg-workgroup-kill-saved-wconfig (workgroup wconfig-or-name)
  "Delete WCONFIG-OR-NAME from WORKGROUP's saved wconfigs.
WCONFIG-OR-NAME is resolved with `wg-workgroup-get-saved-wconfig'."
  (wg-when-let ((wconfig (wg-workgroup-get-saved-wconfig
                          workgroup wconfig-or-name)))
    (wg-asetf (wg-workgroup-saved-wconfigs workgroup) (remq wconfig it)
              (wg-workgroup-modified workgroup) t)))




;;; garbage collection

;; update buf list

(defun wg-update-buffer-in-buf-list (&optional buffer)
  "Update BUFFER's corresponding buf in `wg-buf-list'.
BUFFER nil defaults to `current-buffer'."
  (let ((buffer (or buffer (current-buffer))))
    (wg-when-let ((uid (wg-buffer-uid buffer))
                  (old-buf (wg-find-buf-by-uid uid))
                  (new-buf (wg-buffer-to-buf buffer)))
      (setf (wg-buf-uid new-buf) (wg-buf-uid old-buf))
      (wg-asetf (wg-buf-list) (cons new-buf (remove old-buf it))))))

(defun wg-update-buf-list (&optional buffer-list)
  "Update all bufs in `wg-buf-list' corresponding to buffers in BUFFER-LIST."
  (mapc 'wg-update-buffer-in-buf-list (or buffer-list (buffer-list))))


;; gc buf uids

(defun wg-workgroup-gc-buf-uids (workgroup)
  "Remove buf uids from WORKGROUP that have no referent in `wg-buf-list'."
  (wg-asetf (wg-workgroup-strong-buf-uids workgroup)
            (cl-remove-if-not 'wg-find-buf-by-uid it)
            (wg-workgroup-weak-buf-uids workgroup)
            (cl-remove-if-not 'wg-find-buf-by-uid it)))

(defun wg-gc-buf-uids ()
  "Remove from all workgroups those buf uids that have no referent in `wg-buf-list'."
  (mapc 'wg-workgroup-gc-buf-uids (wg-workgroup-list)))


;; gc bufs

(defun wg-wtree-buf-uids (wtree)
  "Return a new list of the buf uids of all wins in WTREE."
  (if (not wtree)
      (error "WTREE is nil in `wg-wtree-buf-uids'!"))
  (wg-flatten-wtree wtree 'wg-win-buf-uid))

(defun wg-wtree-unique-buf-uids (wtree)
  "Return a list of the unique buf uids of all wins in WTREE."
  (cl-remove-duplicates (wg-wtree-buf-uids wtree) :test 'string=))

(defun wg-wconfig-buf-uids (wconfig)
  "Return WCONFIG's wtree's `wg-wtree-buf-uids'."
  (if (not (wg-wconfig-wtree wconfig))
      (error "WTREE is nil in `wg-wconfig-buf-uids'!"))
  (wg-wtree-unique-buf-uids (wg-wconfig-wtree wconfig)))

(defun wg-workgroup-base-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's working wconfig."
  (wg-wconfig-buf-uids (wg-workgroup-base-wconfig workgroup)))

(defun wg-workgroup-saved-wconfigs-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's base wconfig."
  (cl-reduce 'wg-string-list-union
          (wg-workgroup-saved-wconfigs workgroup)
          :key 'wg-wconfig-buf-uids))

(defun wg-workgroup-all-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's wconfigs."
  (cl-union (wg-workgroup-base-wconfig-buf-uids workgroup)
         (wg-workgroup-saved-wconfigs-buf-uids workgroup)
         :test 'string=))

(defun wg-workgroup-all-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP."
  (cl-reduce 'wg-string-list-union
          (list (wg-workgroup-base-wconfig-buf-uids workgroup)
                (wg-workgroup-saved-wconfigs-buf-uids workgroup)
                (if wg-restore-associated-buffers
                    (wg-workgroup-associated-buf-uids workgroup))
                )))

(defun wg-session-all-buf-uids (&optional session)
  "Return a new list of all unique buf uids in SESSION.
SESSION nil defaults to `wg-current-session'."
  (cl-reduce 'wg-string-list-union
          (wg-session-workgroup-list (or session (wg-current-session)))
          :key 'wg-workgroup-all-buf-uids))

(defun wg-buffer-list-all-uids (&optional buffer-list)
  "Return a list of the uids of all buffers in BUFFER-LIST in
which `wg-buffer-uid' is locally bound.
BUFFER-LIST nil defaults to `buffer-list'."
  (delq nil (mapcar 'wg-buffer-uid (or buffer-list (buffer-list)))))

(defun wg-all-buf-uids (&optional session buffer-list)
  "Return the union of `wg-session-all-buf-uids' and `wg-buffer-list-all-uids'."
  (cl-union (wg-session-all-buf-uids session)
         (wg-buffer-list-all-uids buffer-list)
         :test 'string=))

(defun wg-gc-bufs ()
  "gc bufs from `wg-buf-list' that are no longer needed."
  (let ((all-buf-uids (wg-all-buf-uids)))
    (wg-asetf (wg-buf-list)
              (cl-remove-if-not (lambda (uid) (member uid all-buf-uids)) it
                                :key 'wg-buf-uid))))


;; FIXME: Duplicate buf names probably shouldn't be allowed.  An unrelated error
;; causes two *scratch* buffers to be present, triggering the "uids don't match"
;; error.  Write something to remove bufs with duplicate names.


(defun wg-perform-session-maintenance ()
  "Perform various maintenance operations on the current Workgroups session."
  (wg-update-current-workgroup-working-wconfig)
  (wg-update-all-base-wconfigs)
  (wg-gc-bufs)
  (wg-gc-buf-uids)
  (wg-update-buf-list))


;; session consistency testing

(defun wg-session-uids-consistent-p ()
  "Return t if there are no duplicate bufs or buf uids in the wrong places,
nil otherwise."
  (and (cl-every (lambda (wg)
                (not (wg-dups-p (wg-workgroup-associated-buf-uids wg)
                                :test 'string=)))
              (wg-workgroup-list))
       (not (wg-dups-p (wg-buf-list) :key 'wg-buf-uid :test 'string=))
       (not (wg-dups-p (wg-workgroup-list) :key 'wg-workgroup-uid :test 'string=))))



;;; workgroup restoration

(defun wg-restore-workgroup-associated-buffers-internal (workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (save-window-excursion
    (delete nil (mapcar 'wg-restore-buffer
                        (wg-workgroup-associated-bufs workgroup)))))

(defun wg-restore-workgroup (workgroup)
  "Restore WORKGROUP in `selected-frame'."
  (when wg-restore-associated-buffers
    (wg-restore-workgroup-associated-buffers-internal workgroup))
  (let (wg-flag-modified)
    (wg-restore-wconfig-undoably
     (wg-workgroup-working-wconfig workgroup)
     t)))



;;; workgroup-list ops

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-workgroup-list'.
Also delete all references to it by `wg-workgroup-state-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (dolist (frame (frame-list))
    (remhash (wg-workgroup-uid workgroup) (wg-workgroup-state-table frame))
    (when (wg-current-workgroup-p workgroup frame)
      (wg-set-current-workgroup nil frame))
    (when (wg-previous-workgroup-p workgroup frame)
      (wg-set-previous-workgroup nil frame)))
  (setf (wg-workgroup-list) (remove workgroup (wg-workgroup-list-or-error)))
  (setf (wg-session-modified (wg-current-session)) t)
  workgroup)

(defun wg-add-workgroup (workgroup &optional index)
  "Add WORKGROUP to `wg-workgroup-list' at INDEX or the end.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-find-workgroup-by :name (wg-workgroup-name workgroup) t)
    (unless index (setq index (cl-position it (wg-workgroup-list-or-error))))
    (wg-delete-workgroup it))
  (wg-asetf (wg-workgroup-list)
            (wg-insert-before workgroup it (or index (length it))))
  (setf (wg-session-modified (wg-current-session)) t)
  workgroup)

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-workgroup-list'.
Ask to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup))
        (uid (wg-workgroup-uid workgroup)))
    (when (wg-find-workgroup-by :uid uid t)
      (error "A workgroup with uid %S already exists" uid))
    (when (wg-find-workgroup-by :name name t)
      (unless (or wg-no-confirm-on-destructive-operation
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-make-and-add-workgroup (name &optional blank)
  "Create a workgroup named NAME and add it with `wg-check-and-add-workgroup'."
  (wg-check-and-add-workgroup
   (wg-make-workgroup
    :name name
    :base-wconfig (if blank (wg-make-blank-wconfig)
                    (wg-current-wconfig)))))

(defun wg-get-workgroup-create (workgroup)
  "Return the workgroup specified by WORKGROUP, creating a new one if needed.
If `wg-get-workgroup' on WORKGROUP returns a workgroup, return it.
Otherwise, if WORKGROUP is a string, create a new workgroup with
that name and return it. Otherwise error."
  (or (wg-get-workgroup workgroup t)
      (if (stringp workgroup)
          (when (or (not wg-confirm-on-get-workgroup-create)
                    (y-or-n-p (format "%S doesn't exist.  Create it? "
                                      workgroup)))
            (wg-make-and-add-workgroup workgroup))
        ;; Call this again for its error message
        (wg-get-workgroup workgroup))))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-workgroup-list' by N."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (unless (member workgroup workgroup-list)
      (error "Workgroup isn't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-cyclic-offset-elt workgroup workgroup-list n)
          (wg-session-modified (wg-current-session)) t)))

(defun wg-swap-workgroups-in-workgroup-list (workgroup1 workgroup2)
  "Swap the positions of WORKGROUP1 and WORKGROUP2 in `wg-workgroup-list'."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (when (eq workgroup1 workgroup2)
      (error "Can't swap a workgroup with itself"))
    (unless (and (memq workgroup1 workgroup-list)
                 (memq workgroup2 workgroup-list))
      (error "Both workgroups aren't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-util-swap workgroup1 workgroup2 workgroup-list)
          (wg-session-modified (wg-current-session)) t)))

(defun wg-cyclic-nth-from-workgroup (workgroup &optional n)
  "Return the workgroup N places from WORKGROUP in `wg-workgroup-list'."
  (wg-cyclic-nth-from-elt workgroup (wg-workgroup-list-or-error) (or n 1)))



;;; buffer association

(defun wg-associate-buffers (workgroup window-or-emacs-window-tree)
  "Associate the buffers visible in window elements of
WINDOW-OR-EMACS-WINDOW-TREE with the given WORKGROUP.
WINDOW-OR-EMACS-WINDOW-TREE must be either a window or a tree of
the form produced by `(car (window-tree))'."
  (wg-aif (windowp window-or-emacs-window-tree)
      (with-current-buffer (window-buffer window-or-emacs-window-tree)
        (setq wg-buffer-workgroup workgroup))
    (dolist (w (cddr window-or-emacs-window-tree))
      (when w (wg-associate-buffers workgroup w)))))

(defun wg-associate-frame-buffers ()
  "Associate the buffers visible in the current frame with the
current workgroup (unless it is currently being deactivated)."
  (wg-awhen (wg-current-workgroup :noerror)
    (unless (member it wg-deactivation-list)
      (wg-associate-buffers it (car (window-tree))))))

(defun wg-associate-all-frame-buffers ()
  "Associate all visible buffers with the current
workgroup (unless it is currently being deactivated)."
  (mapcar 'wg-associate-frame-buffers (frame-list)))

(defun wg-buffer-predicate (buffer)
  "Return t iff the given BUFFER should be considered a candidate
for display by `other-buffer' in the current workgroup."
  (or (not wg-associate-buffers)
      (wg-awhen (wg-current-workgroup :noerror)
        (with-current-buffer buffer
          (eq wg-buffer-workgroup it)))))

(defun wg-after-make-frame (frame)
  (set-frame-parameter frame 'buffer-predicate
                       'wg-buffer-predicate))

;;; mode-line

(defun wg-mode-line-buffer-association-indicator (workgroup)
  "Return a string indicating `current-buffer's association-type in WORKGROUP."
  (cl-case (wg-workgroup-bufobj-association-type workgroup (current-buffer))
    (strong wg-mode-line-decor-strongly-associated)
    (weak wg-mode-line-decor-weakly-associated)
    (otherwise wg-mode-line-decor-unassociated)))

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((wg (wg-current-workgroup t))
        (wg-use-faces wg-mode-line-use-faces))
    (cond (wg (wg-fontify " "
                ;;(consp (cons :div wg-mode-line-decor-left-brace))
                ;;(keywordp (car (cons :div wg-mode-line-decor-left-brace)))
                ;;(:div wg-mode-line-decor-left-brace)
                (:brace wg-mode-line-decor-left-brace)
                (:mode (wg-workgroup-name wg))
                (if (not wg-mode-line-only-name)
                    (concat
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (wg-mode-line-buffer-association-indicator wg)
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (if (window-dedicated-p)
                         wg-mode-line-decor-window-dedicated
                       wg-mode-line-decor-window-undedicated)
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (if (wg-session-modified (wg-current-session))
                         wg-mode-line-decor-session-modified
                       wg-mode-line-decor-session-unmodified)
                     (if (wg-workgroup-modified wg)
                         wg-mode-line-decor-workgroup-modified
                       wg-mode-line-decor-workgroup-unmodified)))
                (:brace wg-mode-line-decor-right-brace)))
          (t (if wg-display-nowg
                 (progn
                   (wg-fontify " "
                     (:brace wg-mode-line-decor-left-brace)
                     (:mode wg-nowg-string)
                     (:brace wg-mode-line-decor-right-brace)))
               "")))))

(defun wg-add-mode-line-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (or (assq 'wg-mode-line-display-on mode-line-format)
              wg-mode-line-disable)
    (let ((format '(wg-mode-line-display-on (:eval (wg-mode-line-string))))
          (pos (or (cl-position 'mode-line-position mode-line-format) 10)))
      (set-default 'mode-line-format
                   (wg-insert-after format mode-line-format pos))
      (force-mode-line-update))))

(defun wg-remove-mode-line-display ()
  "Remove Workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-display-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))



;;; messaging

(defun wg-message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also save the msg to `wg-last-message'."
  (setq wg-last-message (apply #'message format-string args)))

(defmacro wg-fontified-message (&rest format)
  "`wg-fontify' FORMAT and call `wg-message' on it."
  (declare (indent defun))
  `(wg-message (wg-fontify ,@format)))



;;; fancy displays

;; FIXME: add `wg-display-max-lines' to chop long display strings at max-line
;; and element-name boundaries

(defun wg-element-display (elt elt-string &optional current-elt-p previous-elt-p)
  "Return display string for ELT."
  (cond ((and current-elt-p (funcall current-elt-p elt))
         (wg-fontify (:cur (concat wg-list-display-decor-current-left
                                   elt-string
                                   wg-list-display-decor-current-right))))
        ((and previous-elt-p (funcall previous-elt-p elt))
         (wg-fontify (:prev (concat wg-list-display-decor-previous-left
                                    elt-string
                                    wg-list-display-decor-previous-right))))
        (t (wg-fontify (:other elt-string)))))

(defun wg-workgroup-display (workgroup index)
  "Return display string for WORKGROUP at INDEX."
  (if (not workgroup) wg-nowg-string
    (wg-element-display
     workgroup
     (format "%d: %s" index (wg-workgroup-name workgroup))
     'wg-current-workgroup-p
     'wg-previous-workgroup-p)))

(defun wg-buffer-display (buffer index)
  "Return display string for BUFFER. INDEX is ignored."
  (if (not buffer) "No buffers"
    (wg-element-display
     (wg-get-buffer buffer)
     (format "%s" (wg-buffer-name buffer))
     'wg-current-buffer-p)))


;; (defun wg-display-internal (elt-fn list)
;;   "Return display string built by calling ELT-FN on each element of LIST."
;;   (let ((div (wg-add-face :div wg-list-display-decor-divider))
;;         (i -1))
;;     (wg-fontify
;;       (:brace wg-list-display-decor-left-brace)
;;       (if (not list) (funcall elt-fn nil nil)
;;         (wg-doconcat (elt list div) (funcall elt-fn elt (cl-incf i))))
;;       (:brace wg-list-display-decor-right-brace))))

(defcustom wg-display-max-lines 1
  "FIXME: docstring this"
  :type 'integer
  :group 'workgroups)



(defun wg-display-internal (elt-fn list)
  "Return display string built by calling ELT-FN on each element of LIST."
  (let ((div (wg-add-face :div wg-list-display-decor-divider))
        (wwidth (window-width (minibuffer-window)))
        (i -1)
        (str))
    (setq str
          (wg-fontify
            (:brace wg-list-display-decor-left-brace)
            (if (not list) (funcall elt-fn nil nil)
              (wg-doconcat (elt list div) (funcall elt-fn elt (cl-incf i))))
            (:brace wg-list-display-decor-right-brace)))
    ;; (subseq str 0 wwidth)
    ))

(defun wg-workgroup-list-display (&optional workgroup-list)
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-workgroup-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (wg-display-internal 'wg-workgroup-display
                       (or workgroup-list (wg-workgroup-list))))

;; TODO: Possibly add scroll animation for the buffer list display during
;; `wg-next-buffer' and `wg-previous-buffer'
(defun wg-buffer-list-display (buffer-list)
  "Return the buffer-list display string."
  (wg-display-internal
   'wg-buffer-display
   (if wg-center-rotate-buffer-list-display
       (wg-center-rotate-list buffer-list) buffer-list)))

(defun wg-buffer-list-filter-display (&optional workgroup blf-id)
  "Return a buffer-list-filter display string from WORKGROUP and BLF-ID."
  (wg-fontify
    "("
    (wg-workgroup-name (wg-get-workgroup workgroup))
    ":"
    (wg-get-buffer-list-filter-val blf-id 'indicator)
    ")"))

(defun wg-buffer-list-filter-prompt (prompt &optional workgroup blf-id)
  "Return a prompt string from PROMPT indicating WORKGROUP and BLF-ID."
  (wg-fontify
    prompt " "
    (wg-buffer-list-filter-display workgroup blf-id)
    ": "))

(defun wg-buffer-command-display (&optional buffer-list)
  "Return the buffer command display string."
  (concat
   (wg-buffer-list-filter-display) ": "
   (wg-buffer-list-display (or buffer-list (wg-filtered-buffer-list)))))

(defun wg-timeline-display (position length)
  "Return a timeline visualization string from POSITION and LENGTH."
  (wg-fontify
    ;;(cons :div "-<{")
    "-<{"
    (wg-make-string (- length position) "-" "=")
    "O"
    (wg-make-string (1+ position) "-" "=")
    "}>-"))

(defun wg-undo-timeline-display (workgroup)
  "Return WORKGROUP's undo timeline string."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (wg-timeline-display undo-pointer (length undo-list))))



(require 'workgroups-ido)


;;; minibuffer reading

(defun wg-read-buffer (prompt &optional default require-match)
  "Workgroups' version of `read-buffer'."
  (if (not (wg-filter-buffer-list-p))
      (funcall (wg-read-buffer-function) prompt default require-match)
    (wg-with-buffer-list-filters 'read-buffer
      (funcall (wg-read-buffer-function)
               (wg-buffer-list-filter-prompt
                (wg-aif (string-match ": *$" prompt)
                    (substring prompt 0 it) prompt))
               default require-match))))

;; TODO: Add minibuffer commands for killing, cloning, etc.
(defun wg-read-workgroup-name (&optional require-match)
  "Read a workgroup with `wg-completing-read'."
  (wg-completing-read
   "Workgroup: " (wg-workgroup-names) nil require-match nil nil
   (wg-awhen (wg-current-workgroup t) (wg-workgroup-name it))))

(defun wg-new-default-workgroup-name ()
  "Return a new, unique, default workgroup name."
  (let ((names (wg-workgroup-names t)) (index -1) result)
    (while (not result)
      (let ((new-name (format "wg%s" (cl-incf index))))
        (unless (member new-name names)
          (setq result new-name))))
    result))

(defun wg-unique-workgroup-name-p (new-name)
  "Return t if NEW-NAME is unique in `wg-workgroup-list', nil otherwise."
  (cl-every (lambda (existing-name) (not (equal new-name existing-name)))
         (wg-workgroup-names t)))

(defun wg-read-new-workgroup-name (&optional prompt)
  "Read a non-empty name string from the minibuffer."
  (let ((default (wg-new-default-workgroup-name)))
    (wg-read-object
     (or prompt (format "Name (default: %S): " default))
     (lambda (new) (and (stringp new)
                        (not (equal new ""))
                        (wg-unique-workgroup-name-p new)))
     "Please enter a unique, non-empty name"
     nil nil nil nil default)))

(defun wg-read-workgroup-index ()
  "Prompt for the index of a workgroup."
  (let ((max (1- (length (wg-workgroup-list-or-error)))))
    (wg-read-object
     (format "%s\n\nEnter [0-%d]: " (wg-workgroup-list-display) max)
     (lambda (obj) (and (integerp obj) (wg-within obj 0 max t)))
     (format "Please enter an integer [%d-%d]" 0 max)
     nil nil t)))

(defun wg-read-saved-wconfig-name (workgroup &optional prompt require-match)
  "Read the name of a saved wconfig, completing on the names of
WORKGROUP's saved wconfigs."
  (wg-completing-read
   (or prompt "Saved wconfig name: ")
   (wg-workgroup-saved-wconfig-names workgroup)
   nil require-match))

(defun wg-read-saved-wconfig (workgroup)
  "Read the name of and return one of WORKGROUP's saved wconfigs."
  (wg-workgroup-get-saved-wconfig
   workgroup (wg-read-saved-wconfig-name workgroup nil t)))



;;; session resetting

(defun wg-reset-frame (frame)
  "Reset Workgroups' frame-parameters in FRAME to nil."
  (set-frame-parameter frame 'wg-workgroup-state-table nil)
  (set-frame-parameter frame 'wg-current-workgroup-uid nil)
  (set-frame-parameter frame 'wg-previous-workgroup-uid nil))

(defun wg-reset-buffer (buffer)
  "Return BUFFER.
Currently only sets BUFFER's `wg-buffer-uid' to nil."
  (with-current-buffer buffer
    (setq wg-buffer-uid nil)))

(defun wg-reset-internal (&optional session)
  "Reset Workgroups, setting `wg-current-session' to SESSION.
Resets all frame parameters, buffer-local vars, current
Workgroups session object, etc.  SESSION nil defaults to a new,
blank session object."
  (mapc 'wg-reset-frame (frame-list))
  (mapc 'wg-reset-buffer (buffer-list))
  (setq wg-wconfig-kill-ring nil)
  (setq wg-current-session (or session (wg-make-session))))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(provide 'workgroups-functions)
;;; workgroups-functions.el ends here
