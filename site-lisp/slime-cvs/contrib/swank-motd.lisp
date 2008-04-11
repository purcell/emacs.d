(in-package :swank)

(defun parse-changelog (changelog-pathname)
  (with-open-file (stream changelog-pathname :direction :input)
    (labels ((entry-line-p (line)
               (and (<= 10 (length line))
                    (digit-char-p (aref line 0))
                    (digit-char-p (aref line 1))
                    (digit-char-p (aref line 2))
                    (digit-char-p (aref line 3))
                    (char= #\- (aref line 4))
                    (digit-char-p (aref line 5))
                    (digit-char-p (aref line 6))
                    (char= #\- (aref line 7))
                    (digit-char-p (aref line 8))
                    (digit-char-p (aref line 9))))
             (read-next-entry ()
               ;; don't use with-output-to-string to avoid sbcl
               ;; compiler warnings
               (with-output-to-string (entry-text)
                 (loop
                    for changelog-line = (read-line stream nil stream nil)
                    when (eq changelog-line stream)
                      do (return-from read-next-entry
                           (values (get-output-stream-string entry-text) nil))
                    when (entry-line-p changelog-line)
                      do (return-from read-next-entry
                           (values (get-output-stream-string entry-text) changelog-line))
                    do (write-line changelog-line entry-text)))))
      (let ((this-author-line (nth-value 1 (read-next-entry)))
            (entries '()))
        (loop
           (multiple-value-bind (text next-author-line)
               (read-next-entry)
             (with-output-to-string (text+author)
               (write-line this-author-line text+author)
               (write-string text text+author)
               (push (list (encode-universal-time 0 0 0
                                                  (parse-integer this-author-line :start 8 :end 10)
                                                  (parse-integer this-author-line :start 5 :end 7)
                                                  (parse-integer this-author-line :start 0 :end 4))
                           (get-output-stream-string text+author))
                     entries))
             (if (null next-author-line)
                 (return-from parse-changelog entries)
                 (setf this-author-line next-author-line))))))))

(defun read-motd (motd-pathname)
  (handler-case
      (let ((entries (mapcar #'second
                             (remove-if (lambda (date/entry-text)
                                          (< (first date/entry-text) (- (get-universal-time) (* 60 60 24 7))))
                                        (parse-changelog motd-pathname)))))
        
        (when entries
          (with-output-to-string (motd-for-emacs)
            (format motd-for-emacs ";; MOTD read from ~S.~%" motd-pathname)
            (dolist (entry entries)
              (with-input-from-string (stream entry)
                (loop
                  for line = (read-line stream nil stream nil)
                  until (eq line stream)
                  do (write-string ";; " motd-for-emacs)
                  do (write-line line motd-for-emacs)))))))
    (error (c)
      (format nil ";; ERROR ~S OPENING MOTD ~S.~%" c motd-pathname))))
