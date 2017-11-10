;;; ensime-undo.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
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

(defvar ensime-undo-info-buffer-name "*ENSIME-Undo*")

(defvar ensime-undo-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda()(interactive)
				(funcall continue-undo)
				(ensime-popup-buffer-quit-function)
				))
    (define-key map (kbd "q") (lambda()(interactive)
				(funcall cancel-undo)
				(ensime-popup-buffer-quit-function)
				))
    map)
  "Key bindings for the undo confirmation popup.")


(defun ensime-undo-peek ()
  "Get rid of an intermediate variable."
  (interactive)
  (let ((undo (ensime-rpc-peek-undo)))
    (destructuring-bind (&key id summary changes) undo

      (let ((cont `(lambda ()
		     (ensime-undo-exec ,id)))
	    (cancel `(lambda ())))

	(ensime-with-popup-buffer
	 (ensime-undo-info-buffer-name t t)
           ;; Override ensime-popup-buffer-mode's normal keymap
           ;; because of "q"
           (add-to-list
            'minor-mode-overriding-map-alist
            (cons 'ensime-popup-buffer-mode ensime-undo-info-map))
	 (set (make-local-variable 'cancel-undo) cancel)
	 (set (make-local-variable 'continue-undo) cont)
	 (ensime-undo-populate-confirmation-buffer
	  summary changes)
	 (goto-char (point-min)))))))

(defun ensime-undo-exec (id)
  (let* ((result (ensime-rpc-exec-undo id))
	 (touched (plist-get result :touched-files)))
    (ensime-revert-visited-files touched t)))


(defun ensime-undo-populate-confirmation-buffer (summary changes)

  (ensime-insert-with-face "Proposed undo of "
			   'font-lock-constant-face)
  (ensime-insert-with-face (concat "\"" summary "\"")
			   'font-lock-variable-name-face)
  (ensime-insert-with-face " would cause the following changes."
			   'font-lock-constant-face)
  (ensime-insert-with-face
   " (c to confirm, q to cancel)"
   'font-lock-constant-face)

  (insert "\n\n\n")

  (if (null changes)
      (insert "Nothing to be done.")
    (ensime-insert-change-list changes))

  )



(provide 'ensime-undo)

;; Local Variables:
;; End:

