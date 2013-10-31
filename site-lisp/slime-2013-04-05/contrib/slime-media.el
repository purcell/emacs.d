(define-slime-contrib slime-media
  "Display things other than text in SLIME buffers"
  (:authors "Christophe Rhodes <csr21@cantab.net>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-media)
  (:on-load
   (add-hook 'slime-event-hooks 'slime-dispatch-media-event)))

(defun slime-dispatch-media-event (event)
  (destructure-case event
    ((:write-image image string)
     (let ((image (find-image image)))
       (slime-media-insert-image image string))
     t)
    ((:popup-buffer bufname string mode)
     (slime-with-popup-buffer (bufname :mode mode :connection t :package t)
       (princ string)
       (goto-char (point-min)))
     t)
    (t nil)))

(defun slime-media-insert-image (image string &optional bol)
  (with-current-buffer (slime-output-buffer)
    (let ((marker (slime-output-target-marker :repl-result)))
      (goto-char marker)
      (slime-propertize-region `(face slime-repl-result-face
                                      rear-nonsticky (face))
        (insert-image image string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point)))
    (slime-repl-show-maximum-output)))

(provide 'slime-media)
