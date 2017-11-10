;;; ensime-comint-utils.el
;;
;;;; License
;;
;;     Copyright (C) 2012 Grégoire Neuville
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'comint)

(defvar ensime-comint-filter-buffer " *ensime-comint-filter-buffer*"
  "Name of the buffer used by `ensime-comint-cplt-output-filter'
to put output from process into for further processing.")

(defvar ensime-comint-completion-invalid-values "\C-h"
  "Regexp matching values to be discarded from
the output received after a call to `ensime-comint-complete'.")

(defun ensime-comint-sanitise(str)
  (replace-regexp-in-string
   (concat ensime-comint-completion-invalid-values "\\|" ansi-color-drop-regexp)
   "" str))

(defun ensime-comint-shape-candidate (candidate cand-max-length nbr-cols candidates)
  (let* ((cand-length (length candidate))
         (cand-index (+ 1 (position candidate candidates :test 'string=)))
         (nbr-spaces (- cand-max-length cand-length))
         (new-cand (concat candidate (make-string nbr-spaces ? ))))
    (if (= 0 (% cand-index nbr-cols))
        (concat candidate "\n")
      (concat new-cand " "))))

(defun ensime-comint-shape-candidates (candidates)
  (let* ((wwidth (window-width))
         (cand-max-length
          (+ 1 (apply 'max (mapcar 'length candidates))))
         (nbr-cols (/ wwidth cand-max-length)))
    (mapcar #'(lambda (cand)
               (ensime-comint-shape-candidate
                cand cand-max-length nbr-cols candidates))
            candidates)))

;; TODO : this function is messy and too much based on sbt's behaviour
;; => it should go in ensime-sbt.el
(defun ensime-comint-treat-output (proc cand-regexp err-regexp output)
  (with-current-buffer (process-buffer proc)
    (let* ((output-list
            (split-string (ensime-comint-sanitise output) cand-regexp t))
           (input (car output-list))
           (invalid-input (cl-find err-regexp output-list :test 'string-match))
           (rev-output-list (reverse output-list))
           (prompt-completion
            (cl-find comint-prompt-regexp rev-output-list :test 'string-match))
           ;; this is very ugly
           (trailing-space (if (and (string= (replace-regexp-in-string "\s" "" input)
                                             (replace-regexp-in-string
                                               (concat "\s\\|" comint-prompt-regexp) ""
                                               (if prompt-completion
                                                   prompt-completion
                                                 " ")))
                                    (<= (length output-list) 3))
                               " "
                             (if (string-match "\s$" input)
                                 " "
                               ""))))
      (if invalid-input
          (concat invalid-input "\n" prompt-completion)
        (if (> (length output-list) 1)
            (cond
             ((<= (length output-list) 3) ;; only one candidate
              (concat "\n" prompt-completion trailing-space))
             ((> (length output-list) 3) ;; several candidates
              (concat (mapconcat 'identity
                                 (ensime-comint-shape-candidates
                                  (reverse (set-difference
                                            (cdr output-list)
                                            (list prompt-completion trailing-space)))) "")
                      "\n" prompt-completion trailing-space)))
          "")))))

(defun ensime-comint-cplt-output-filter (proc output)
  (with-current-buffer (get-buffer-create ensime-comint-filter-buffer)
    (insert output))
  "")

(defun ensime-comint-complete (proc input cand-regexp err-regexp)
  "Get the completion candidates from sbt/repl process"
  (let* ((old-proc-filter (process-filter proc)))
    (set-process-filter proc 'ensime-comint-cplt-output-filter)
    (comint-proc-query proc (concat input (kbd "TAB")))
    (comint-proc-query proc (kbd "C-a"))
    (comint-proc-query proc (kbd "C-k"))
    (sit-for 0.2) ;; make sure all output has been  received
    (let ((custom-output (with-current-buffer
                             (get-buffer-create ensime-comint-filter-buffer)
                           (ensime-comint-treat-output proc cand-regexp
                                                       err-regexp (buffer-string)))))
      (if (string-to-list custom-output)
          (progn
            (comint-kill-input)
            (comint-output-filter proc (concat "\n" custom-output))
            ;; the below is a bit silly but I didn't find any other way to prevent
            ;; the completed word from becoming read-only
            (let ((new-input (buffer-substring
                              (comint-line-beginning-position)
                              (point)))
                  (inhibit-read-only t))
              (kill-region (comint-line-beginning-position) (point))
              (remove-list-of-text-properties 0 (length new-input)
                                              '(read-only) new-input)
              (insert new-input)))
        (message "No completion candidates")))
    (kill-buffer ensime-comint-filter-buffer)
    (set-process-filter proc old-proc-filter)))

(provide 'ensime-comint-utils)

;; Local Variables:
;; End:

