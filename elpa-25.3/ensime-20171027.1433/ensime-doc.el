;;; ensime-doc.el
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

(defun ensime-make-doc-url (type &optional member)
  "Given a type and an optional member object, yields an http url for
 browsing the documentation for those objects."
  (ensime--normalise-url
   (ensime-rpc-doc-uri-for-symbol (ensime-type-full-name type)
                                  (when member (ensime-member-name member)))))

(defun ensime-show-doc-for-symbol-at-point ()
  "Browse to documentation for the symbol at current point."
  (interactive)
  (let* ((url (ensime-rpc-doc-uri-at-point (buffer-file-name-with-indirect) (point))))
    (if url (browse-url (ensime--normalise-url url))
      (message "No documentation found."))))

(provide 'ensime-doc)
;; Local Variables:
;; End:
