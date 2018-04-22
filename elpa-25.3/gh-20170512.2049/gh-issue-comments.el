;;; gh-issue-comments.el --- issue comments api for github

;; Copyright (C) 2014 Travis Thieman

;; Author: Travis Thieman <travis.thieman@gmail.com>
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

;; (setf api (gh-issue-comments-api "api" :sync nil :cache nil :num-retries 1))
;; (setf comments (gh-issue-comments-list api "user" "repo" "issue id"))
;; (setq my-comment (make-instance 'gh-issue-comments-comment :body "This is great!"))
;; (gh-issue-comments-new api "user" "repo" "issue id" my-comment)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(require 'gh-issues)

(let ((ver "1.0.0"))
  (define-obsolete-function-alias
      'gh-issue-comments-api 'gh-issues-api ver)
  (define-obsolete-function-alias
      'gh-issue-comments-comment 'gh-issues-comment ver)

  (define-obsolete-function-alias
      'gh-issue-comments-req-to-update 'gh-comment-req-to-update ver)

  (define-obsolete-function-alias
      'gh-issue-comments-list 'gh-issues-comments-list ver)
  (define-obsolete-function-alias
      'gh-issue-comments-get 'gh-issues-comments-get ver)
  (define-obsolete-function-alias
      'gh-issue-comments-update 'gh-issues-comments-update ver)
  (define-obsolete-function-alias
      'gh-issue-comments-new 'gh-issues-comments-new ver)
  (define-obsolete-function-alias
      'gh-issue-comments-delete 'gh-issues-comments-delete ver))

(provide 'gh-issue-comments)
;;; gh-issue-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
