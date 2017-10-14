;;; gh-pull-comments.el --- pull request comments api for github

;; Copyright (C) 2014 Toni Reina

;; Author: Toni Reina <areina0@gmail.com>
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

;; TODOS:
;;   * Support listing all comments in a repository

;; Basic usage:

;; (setf api (gh-pull-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (gh-pull-comments-list api "user" "repo" "pull request id"))
;; (setq my-comment (make-instance 'gh-pull-comments-comment
;; 				:body "This is great!"
;; 				:path "README.md"
;; 				:position 2
;; 				:commit-id "commit sha"))
;; (gh-pull-comments-new api "user" "repo" "pull request id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(require 'gh-pulls)

(let ((ver "1.0.0"))
  (define-obsolete-function-alias
      'gh-pull-comments-api 'gh-pulls-api ver)
  (define-obsolete-function-alias
      'gh-pull-comments-comment 'gh-pulls-comment ver)

  (define-obsolete-function-alias
      'gh-pull-comments-req-to-update 'gh-comment-req-to-update ver)
  (define-obsolete-function-alias
      'gh-pull-comments-req-to-create 'gh-pulls-comment-req-to-create)

  (define-obsolete-function-alias
      'gh-pull-comments-list 'gh-pulls-comments-list ver)
  (define-obsolete-function-alias
      'gh-pull-comments-get 'gh-pulls-comments-get ver)
  (define-obsolete-function-alias
      'gh-pull-comments-update 'gh-pulls-comments-update ver)
  (define-obsolete-function-alias
      'gh-pull-comments-new 'gh-pulls-comments-new ver)
  (define-obsolete-function-alias
      'gh-pull-comments-delete 'gh-pulls-comments-delete ver))

(provide 'gh-pull-comments)
;;; gh-pull-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
