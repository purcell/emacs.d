;;; gh-issues.el --- issues api for github

;; Copyright (C) 2014-2015  Yann Hodique
;; Copyright (C) 2014 Travis Thieman
;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic usage:

;; (setf api (gh-issues-api "api" :sync nil :cache nil :num-retries 1))
;; (setf issues (gh-issues-issue-list api "user" "repo"))
;; (last (oref issues data)) ; get one issue
;; (setq mi (make-instance 'gh-issues-issue :body "issue body" :title "issue title"))
;; (gh-issues-issue-new api "user" "repo" mi)
;; (setf comments (gh-issues-comments-list api "user" "repo" "issue id"))
;; (setq my-comment (make-instance 'gh-issues-comment :body "This is great!"))
;; (gh-issues-comments-new api "user" "repo" "issue id" my-comment)

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
(defclass gh-issues-api (gh-api-v3 gh-comments-api-mixin)
  ((issue-cls :allocation :class :initform gh-issues-issue)
   (milestone-cls :allocation :class :initform gh-issues-milestone)
   (label-cls :allocation :class :initform gh-issues-label)
   (comment-cls :allocation :class :initform gh-issues-comment))
  "Github Issues api")

;;;###autoload
(gh-defclass gh-issues-issue (gh-ref-object)
  ((number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (labels :initarg :labels :initform nil :marshal-type (list gh-issues-label))
   (assignees :initarg :assignees :initform nil :marshal-type (list gh-user))
   (assignee :initarg :assignee :initform nil :marshal-type gh-user)
   (milestone :initarg :milestone :initform nil :marshal-type gh-issues-milestone)
   (comments :initarg :comments :initform 0)
   (pull-request :initarg :pull-request :marshal-type gh-issues-pull-request)
   (closed-at :initarg :closed-at)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at))
  "issues request")

;;;###autoload
(gh-defclass gh-issues-pull-request (gh-object)
  ((html-url :initarg :html-url)
   (diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)))

;;;###autoload
(gh-defclass gh-issues-label (gh-ref-object)
  ((name :initarg :name)
   (color :initarg :color)))

(defmethod gh-issues-label-req-to-update ((label gh-issues-label))
  `(("name" . ,(oref label :name))
    ("color" . ,(oref label :color))))

;;;###autoload
(gh-defclass gh-issues-milestone (gh-ref-object)
  ((number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (description :initarg :description)
   (creator :initarg :creator :initform nil :marshal-type gh-user)
   (open-issues :initarg :open-issues )
   (closed-issues :initarg :closed-issues)
   (created-at :initarg :created-at)
   (due-on :initarg :due-on))
  "github milestone")

;;;###autoload
(gh-defclass gh-issues-comment (gh-comment)
  ())

(defmethod gh-issues-issue-list ((api gh-issues-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api issue-cls)) "GET"
   (format "/repos/%s/%s/issues" user repo)))

(defmethod gh-issues-milestone-list ((api gh-issues-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api milestone-cls)) "GET"
   (format "/repos/%s/%s/milestones" user repo)))

(defmethod gh-issues-milestone-get ((api gh-issues-api) user repo id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api milestone-cls)) "GET"
   (format "/repos/%s/%s/milestones/%s" user repo id)))

(defmethod gh-issues-milestone-new ((api gh-issues-api) user repo milestone)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api milestone-cls)) "POST"
   (format "/repos/%s/%s/milestones" user repo)
   (gh-issues-milestone-req-to-update milestone)))

(defmethod gh-issues-milestone-update ((api gh-issues-api) user repo
                                       id milestone)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api milestone-cls)) "PATCH"
   (format "/repos/%s/%s/milestones/%s" user repo id)
   (gh-issues-milestone-req-to-update milestone)))

(defmethod gh-issues-milestone-req-to-update ((milestone gh-issues-milestone))
  (let ((state (oref milestone :state))
        (description (oref milestone :description))
        (due-on (oref milestone :due-on))
        (to-update `(("title" . ,(oref milestone :title)))))
    (when state (nconc to-update `(("state" . ,state))))
    (when description (nconc to-update `(("description" . ,description))))
    (when due-on (nconc to-update `(("due_on" . ,due-on))))
    to-update))

(defmethod gh-issues-issue-get ((api gh-issues-api) user repo id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api issue-cls)) "GET"
   (format "/repos/%s/%s/issues/%s" user repo id)))

(defmethod gh-issues-issue-req-to-update ((req gh-issues-issue))
  (let ((assignee (oref req :assignee))
        ;; (labels (oref req labels))
        (milestone (oref req :milestone))
        (to-update `(("title" . ,(oref req :title))
                     ("state" . ,(oref req :state))
                     ("body" . ,(oref req :body)))))

    ;; (when labels (nconc to-update `(("labels" . ,(oref req labels) ))))
    (when milestone
      (nconc to-update `(("milestone" . ,(oref milestone :number)))))
    (when assignee
      (nconc to-update `(("assignee" . ,(oref assignee :login)))))
    to-update))

(defmethod gh-issues-issue-update ((api gh-issues-api) user repo id req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api issue-cls)) "PATCH"
   (format "/repos/%s/%s/issues/%s" user repo id)
   (gh-issues-issue-req-to-update req)))

(defmethod gh-issues-issue-new ((api gh-issues-api) user repo issue)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api issue-cls)) "POST"
   (format "/repos/%s/%s/issues" user repo)
   (gh-issues-issue-req-to-update issue)))

;;; Labels

(defmethod gh-issues-label-get ((api gh-issues-api) user repo name)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api label-cls)) "GET"
   (format "/repos/%s/%s/labels/%s" user repo name)))

(defmethod gh-issues-label-list ((api gh-issues-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api label-cls)) "GET"
   (format "/repos/%s/%s/labels" user repo )))

(defmethod gh-issues-label-new ((api gh-issues-api) user repo req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api label-cls)) "POST"
   (format "/repos/%s/%s/labels" user repo)
   (gh-issues-label-req-to-update req)))

(defmethod gh-issues-label-update ((api gh-issues-api) user repo req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api label-cls)) "POST"
   (format "/repos/%s/%s/labels/%s" user repo (oref req :name))
   (gh-issues-label-req-to-update req)))

(defmethod gh-issues-label-delete ((api gh-issues-api) user repo name)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api label-cls)) "DELETE"
   (format "/repos/%s/%s/labels/%s" user repo name)))


(defmethod gh-issues-labels-in-issue ((api gh-issues-api) user repo
                                      issue-or-issue-id)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
   (gh-api-authenticated-request
    api (gh-object-list-reader (oref api label-cls)) "GET"
    (format "/repos/%s/%s/issues/%s/labels" user repo issue-id))))

(defmethod gh-issues-labels-add-to-issue ((api gh-issues-api) user repo
                                          issue-or-issue-id labels)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "PUT"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id)
     (mapcar #'gh-issues--label-name labels))))

(defmethod gh-issues-labels-remove-all-from-issue ((api gh-issues-api) user repo
                                                   issue-or-issue-id )
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (lambda (x) x) "DELETE"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id))))

(defmethod gh-issues-labels-in-milestone ((api gh-issues-api) user repo
                                          milestone-or-milestone-id)
  (let ((milestone-id (gh-issues--milestone-id milestone-or-milestone-id)))
   (gh-api-authenticated-request
    api (gh-object-list-reader (oref api label-cls)) "GET"
    (format "/repos/%s/%s/milestones/%s/labels" user repo milestone-id))))

;;; Comments

(defmethod gh-issues-comments-list ((api gh-issues-api) user repo issue-id)
  (gh-comments-list api (format "/repos/%s/%s/issues/%s" user repo issue-id)))

(defmethod gh-issues-comments-get ((api gh-issues-api) user repo comment-id)
  (gh-comments-get api (format "/repos/%s/%s/issues" user repo) comment-id))

(defmethod gh-issues-comments-update ((api gh-issues-api)
                                      user repo comment-id comment)
  (gh-comments-update api (format "/repos/%s/%s/issues" user repo)
                      comment-id (gh-comment-req-to-update comment)))

(defmethod gh-issues-comments-new ((api gh-issues-api)
                                   user repo issue-id comment)
  (gh-comments-new api (format "/repos/%s/%s/issues/%s" user repo issue-id)
                   (gh-comment-req-to-update comment)))

(defmethod gh-issues-comments-delete ((api gh-issues-api) user repo comment-id)
  (gh-comments-delete api (format "/repos/%s/%s/issues" user repo) comment-id))

;;; helpers

(defun gh-issues--issue-id (issue-or-issue-id)
  (if (eieio-object-p issue-or-issue-id)
      (oref issue-or-issue-id :id)
    issue-or-issue-id))

(defun gh-issues--milestone-id (milestone-or-milestone-id)
  (if (eieio-object-p milestone-or-milestone-id)
      (oref milestone-or-milestone-id :id)
    milestone-or-milestone-id))

(defun gh-issues--label-name (label-or-label-name)
  (if (eieio-object-p label-or-label-name)
      (oref label-or-label-name :name)
    label-or-label-name))


(provide 'gh-issues)
;;; gh-issues.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
