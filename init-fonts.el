(defun steve-set-default-font (name size)
  (interactive
   (let ((current-name (face-attribute 'default :family))
         (current-height (/ (face-attribute 'default :height) 10)))
     (list (ido-completing-read "font-name: " (mapcar (lambda (n) (list n n)) (steve-font-family-name-list)) nil t current-name)
           (read-number "size: " current-height))))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size)))

(defun steve-font-family-name-list ()
  (if *is-cocoa-emacs*
      (mapcar (lambda (n) (symbol-name n)) (x-font-family-list))
    (mapcar #'car (x-font-family-list))))

(defmacro preserving-maximization (&rest body)
  (let ((maximized-frames (gensym)))
    `(let ((,maximized-frames (loop for f in (frame-list)
                                    when (maximized-p f)
                                    collect f)))
       (prog1 (progn ,@body)
         (dolist (frame ,maximized-frames)
           (select-frame frame)
           (maximize-frame))))))

(defun increment-default-font-height (delta)
  (preserving-maximization
   (let ((new-height (+ (face-attribute 'default :height) delta)))
     (set-face-attribute 'default nil :height new-height)
     (message "default font size is now %d" (/ new-height 10)))))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(global-set-key "\M-=" 'increase-default-font-height)
(global-set-key "\M--" 'decrease-default-font-height)

(defmacro preserving-default-font-size (&rest body)
  (let ((old-size (gensym)))
    `(preserving-maximization
      (let ((,old-size (face-attribute 'default :height)))
        (prog1 (progn ,@body)
          (set-face-attribute 'default nil :height ,old-size))))))
