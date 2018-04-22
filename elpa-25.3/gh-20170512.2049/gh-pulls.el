;;; gh-pulls.el --- pull requests module for gh.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-comments)
(require 'gh-common)

(require 'gh-repos)

;;;###autoload
(defclass gh-pulls-cache (gh-cache)
  ((invalidation-chain :allocation :class
                       :initform '(("^/repos/.*/.*/pulls$" . "\0")
                                   ("^/repos/.*/.*/pulls/.*$" . "\0")))))

;;;###autoload
(defclass gh-pulls-api (gh-api-v3 gh-comments-api-mixin)
  ((cache-cls :allocation :class :initform gh-pulls-cache)

   (req-cls :allocation :class :initform gh-pulls-request)
   (comment-cls :allocation :class :initform gh-pulls-comment))
  "Git pull requests API")

;;;###autoload
(gh-defclass gh-pulls-comment (gh-comment)
  ((path :initarg :path)
   (diff-hunk :initarg :diff-hunk)
   (position :initarg :position)
   (original-position :initarg :original-position)
   (commit-id :initarg :commit-id)
   (original-commit-id :initarg :original-commit-id)
   (in-reply-to :initarg :in-reply-to :initform nil)))

(defmethod gh-pulls-comment-req-to-create ((req gh-pulls-comment))
  (let ((in-reply-to (oref req in-reply-to))
	(to-update `(("body" . ,(oref req body)))))
    (if in-reply-to
	(nconc to-update `(("in_reply_to" . ,in-reply-to)))
      (nconc to-update `(("commit_id" . ,(oref req commit-id))
			 ("path" . ,(oref req path))
			 ("position" . ,(oref req position)))))
    to-update))

;;;###autoload
(gh-defclass gh-pulls-request-stub (gh-ref-object)
  ((diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)
   (issue-url :initarg :issue-url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (closed-at :initarg :closed-at)
   (merged-at :initarg :merged-at)
   (head :initarg :head :initform nil :marshal-type gh-repos-ref)
   (base :initarg :base :initform nil :marshal-type gh-repos-ref)))

;;;###autoload
(gh-defclass gh-pulls-request (gh-pulls-request-stub)
  ((merged :initarg :merged)
   (mergeable :initarg :mergeable)
   (merged-by :initarg :merged-by)
   (comments :initarg :comments)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (commits :initarg :commits)
   (additions :initarg :additions)
   (deletions :initarg :deletions)
   (changed-files :initarg :changed-files))
  "Git pull requests API")

(defmethod gh-pulls-req-to-new ((req gh-pulls-request))
  (let ((head (oref req :head))
        (base (oref req :base)))
    `(("title" . ,(oref req :title))
      ("body" . ,(oref req :body))
      ("head" . ,(or (oref head :ref) (oref head :sha)))
      ("base" . ,(or (oref base :ref) (oref base :sha))))))

(defmethod gh-pulls-req-to-update ((req gh-pulls-request-stub))
  `(("title" . ,(oref req :title))
    ("body" . ,(oref req :body))
    ("state" . ,(oref req :state))))

(defmethod gh-pulls-list ((api gh-pulls-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/pulls" user repo)))

(defmethod gh-pulls-get ((api gh-pulls-api) user repo id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/pulls/%s" user repo id)))

(defmethod gh-pulls-new ((api gh-pulls-api) user repo req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "POST"
   (format "/repos/%s/%s/pulls" user repo)
   (gh-pulls-req-to-new req)))

(defmethod gh-pulls-update ((api gh-pulls-api) user repo id req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "PATCH"
   (format "/repos/%s/%s/pulls/%s" user repo id)
   (gh-pulls-req-to-update req)))

;;; Comments

(defmethod gh-pulls-comments-list ((api gh-pulls-api) user repo pull-id)
  (gh-comments-list api (format "/repos/%s/%s/pulls/%s" user repo pull-id)))

(defmethod gh-pulls-comments-get ((api gh-pulls-api) user repo comment-id)
  (gh-comments-get api (format "/repos/%s/%s/pulls" user repo) comment-id))

(defmethod gh-pulls-comments-update ((api gh-pulls-api)
                                      user repo comment-id comment)
  (gh-comments-update api (format "/repos/%s/%s/pulls" user repo)
                      comment-id (gh-comment-req-to-update comment)))

(defmethod gh-pulls-comments-new ((api gh-pulls-api)
                                   user repo pull-id comment)
  (gh-comments-new api (format "/repos/%s/%s/pulls/%s" user repo pull-id)
                   (gh-pulls-comment-req-to-create comment)))

(defmethod gh-pulls-comments-delete ((api gh-pulls-api) user repo comment-id)
  (gh-comments-delete api (format "/repos/%s/%s/pulls" user repo) comment-id))

(provide 'gh-pulls)
;;; gh-pulls.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
