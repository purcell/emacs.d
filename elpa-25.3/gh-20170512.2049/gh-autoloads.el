;;; gh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gh-api" "gh-api.el" (23009 21886 0 0))
;;; Generated autoloads from gh-api.el

(require 'eieio)

(eieio-defclass-autoload 'gh-api 'nil "gh-api" "Github API")

(eieio-defclass-autoload 'gh-api-v3 '(gh-api) "gh-api" "Github API v3")

(eieio-defclass-autoload 'gh-api-request '(gh-url-request) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-response '(gh-url-response) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-paged-request '(gh-api-request) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-paged-response '(gh-api-response) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-callback '(gh-url-callback) "gh-api" nil)

;;;***

;;;### (autoloads nil "gh-auth" "gh-auth.el" (23009 21887 0 0))
;;; Generated autoloads from gh-auth.el

(require 'eieio)

(eieio-defclass-autoload 'gh-authenticator 'nil "gh-auth" "Abstract authenticator")

(eieio-defclass-autoload 'gh-auth-2fa-callback '(gh-url-callback) "gh-auth" "2-factor callback")

(eieio-defclass-autoload 'gh-password-authenticator '(gh-authenticator) "gh-auth" "Password-based authenticator")

(eieio-defclass-autoload 'gh-oauth-authenticator '(gh-authenticator) "gh-auth" "Oauth-based authenticator")

;;;***

;;;### (autoloads nil "gh-cache" "gh-cache.el" (23009 21887 0 0))
;;; Generated autoloads from gh-cache.el

(require 'eieio)

(eieio-defclass-autoload 'gh-cache '(pcache-repository) "gh-cache" nil)

(eieio-defclass-autoload 'gh-cache-entry '(pcache-entry) "gh-cache" nil)

;;;***

;;;### (autoloads nil "gh-comments" "gh-comments.el" (23009 21886
;;;;;;  0 0))
;;; Generated autoloads from gh-comments.el

(require 'eieio)

(eieio-defclass-autoload 'gh-comments-api-mixin 'nil "gh-comments" :abstract)

;;;***

;;;### (autoloads nil "gh-common" "gh-common.el" (23009 21887 0 0))
;;; Generated autoloads from gh-common.el

(require 'eieio)

(autoload 'gh-marshal-default-spec "gh-common" "\


\(fn SLOT)" nil nil)

(autoload 'gh-defclass "gh-common" "\


\(fn NAME SUPERCLASS SLOTS &rest OPTIONS-AND-DOC)" nil t)

(gh-defclass gh-object nil nil)

(gh-defclass gh-ref-object (gh-object) ((id :initarg :id) (url :initarg :url) (html-url :initarg :html-url)))

(gh-defclass gh-user (gh-ref-object) ((login :initarg :login) (gravatar-url :initarg :gravatar-url)) "Github user object")

(gh-defclass gh-comment (gh-ref-object) ((body :initarg :body) (user :initarg :user :initform nil :marshal-type gh-user) (created-at :initarg :created_at) (updated-at :initarg :updated_at)) "Github comment object")

;;;***

;;;### (autoloads nil "gh-gist" "gh-gist.el" (23009 21886 0 0))
;;; Generated autoloads from gh-gist.el

(require 'eieio)

(eieio-defclass-autoload 'gh-gist-api '(gh-api-v3) "gh-gist" "Gist API")

(gh-defclass gh-gist-gist-stub (gh-object) ((files :initarg :files :type list :initform nil :marshal-type (list gh-gist-gist-file)) (public :initarg :public :marshal-type bool) (description :initarg :description)) "Class for user-created gist objects")

(gh-defclass gh-gist-history-change (gh-object) ((total :initarg :total) (additions :initarg :additions) (deletions :initarg :deletions)))

(gh-defclass gh-gist-history-entry (gh-object) ((user :initarg :user :initform nil :marshal-type gh-user) (version :initarg :version) (committed :initarg :committed :marshal ((alist . committed_at))) (change :initarg :change :marshal ((alist . change_status)) :marshal-type gh-gist-history-change) (url :initarg :url)))

(gh-defclass gh-gist-fork-entry (gh-ref-object) ((user :initarg :user :initform nil :marshal-type gh-user) (created :initarg :created :marshal ((alist . created_at))) (updated :initarg :updated :marshal ((alist . updated_at)))))

(gh-defclass gh-gist-gist (gh-ref-object gh-gist-gist-stub) ((date :initarg :date :marshal ((alist . created_at))) (update :initarg :update :marshal ((alist . updated_at))) (push-url :initarg :push-url :marshal ((alist . git_push_url))) (pull-url :initarg :pull-url :marshal ((alist . git_pull_url))) (comments :initarg :comments) (user :initarg :user :initform nil :marshal-type gh-user :marshal ((alist . owner))) (history :initarg :history :initform nil :type list :marshal-type (list gh-gist-history-entry)) (forks :initarg :forks :initform nil :type list :marshal-type (list gh-gist-fork-entry))) "Gist object")

(gh-defclass gh-gist-gist-file (gh-object) ((filename :initarg :filename) (size :initarg :size) (url :initarg :url :marshal ((alist . raw_url))) (content :initarg :content)))

;;;***

;;;### (autoloads nil "gh-issue-comments" "gh-issue-comments.el"
;;;;;;  (23009 21887 0 0))
;;; Generated autoloads from gh-issue-comments.el

(require 'eieio)

;;;***

;;;### (autoloads nil "gh-issues" "gh-issues.el" (23009 21887 0 0))
;;; Generated autoloads from gh-issues.el

(require 'eieio)

(eieio-defclass-autoload 'gh-issues-api '(gh-api-v3 gh-comments-api-mixin) "gh-issues" "Github Issues api")

(gh-defclass gh-issues-issue (gh-ref-object) ((number :initarg :number) (state :initarg :state) (title :initarg :title) (body :initarg :body) (user :initarg :user :initform nil :marshal-type gh-user) (labels :initarg :labels :initform nil :marshal-type (list gh-issues-label)) (assignees :initarg :assignees :initform nil :marshal-type (list gh-user)) (assignee :initarg :assignee :initform nil :marshal-type gh-user) (milestone :initarg :milestone :initform nil :marshal-type gh-issues-milestone) (comments :initarg :comments :initform 0) (pull-request :initarg :pull-request :marshal-type gh-issues-pull-request) (closed-at :initarg :closed-at) (created-at :initarg :created-at) (updated-at :initarg :updated-at)) "issues request")

(gh-defclass gh-issues-pull-request (gh-object) ((html-url :initarg :html-url) (diff-url :initarg :diff-url) (patch-url :initarg :patch-url)))

(gh-defclass gh-issues-label (gh-ref-object) ((name :initarg :name) (color :initarg :color)))

(gh-defclass gh-issues-milestone (gh-ref-object) ((number :initarg :number) (state :initarg :state) (title :initarg :title) (description :initarg :description) (creator :initarg :creator :initform nil :marshal-type gh-user) (open-issues :initarg :open-issues) (closed-issues :initarg :closed-issues) (created-at :initarg :created-at) (due-on :initarg :due-on)) "github milestone")

(gh-defclass gh-issues-comment (gh-comment) nil)

;;;***

;;;### (autoloads nil "gh-oauth" "gh-oauth.el" (23009 21886 0 0))
;;; Generated autoloads from gh-oauth.el

(require 'eieio)

(eieio-defclass-autoload 'gh-oauth-api '(gh-api-v3) "gh-oauth" "OAuth API")

(eieio-defclass-autoload 'gh-oauth-password-authenticator '(gh-password-authenticator) "gh-oauth" nil)

(gh-defclass gh-oauth-authorization (gh-ref-object) ((scopes :initarg :scopes) (token :initarg :token) (app :initarg :app :initform nil :marshal-type gh-oauth-app) (updated-at :initarg :updated-at) (created-at :initarg :created-at)))

(gh-defclass gh-oauth-app (gh-object) ((url :initarg :url) (name :initarg :name)))

;;;***

;;;### (autoloads nil "gh-orgs" "gh-orgs.el" (23009 21887 0 0))
;;; Generated autoloads from gh-orgs.el

(require 'eieio)

(eieio-defclass-autoload 'gh-orgs-api '(gh-api-v3) "gh-orgs" "Orgs API")

(gh-defclass gh-orgs-org-stub (gh-ref-object) ((login :initarg :login) (avatar-url :initarg :avatar-url) (description :initarg :description)))

(gh-defclass gh-orgs-plan (gh-object) ((name :initarg :name) (space :initarg :space) (private-repos :initarg :private-repos)))

(gh-defclass gh-orgs-org (gh-orgs-org-stub) ((name :initarg :name) (company :initarg :company) (blog :initarg :blog) (location :initarg :location) (email :initarg :email) (public-repos :initarg :public-repos) (public-gists :initarg :public-gists) (followers :initarg :followers) (following :initarg :following) (created-at :initarg :created-at) (type :initarg :type) (total-private-repos :initarg :total-private-repos) (owned-private-repos :initarg :owned-private-repos) (private-gists :initarg :private-gists) (disk-usage :initarg :disk-usage) (collaborators :initarg :collaborators) (billing-email :initarg :billing-email) (plan :initarg :plan :initform nil :marshal-type gh-orgs-plan)) "Class for GitHub organizations")

;;;***

;;;### (autoloads nil "gh-pull-comments" "gh-pull-comments.el" (23009
;;;;;;  21887 0 0))
;;; Generated autoloads from gh-pull-comments.el

(require 'eieio)

;;;***

;;;### (autoloads nil "gh-pulls" "gh-pulls.el" (23009 21887 0 0))
;;; Generated autoloads from gh-pulls.el

(require 'eieio)

(eieio-defclass-autoload 'gh-pulls-cache '(gh-cache) "gh-pulls" nil)

(eieio-defclass-autoload 'gh-pulls-api '(gh-api-v3 gh-comments-api-mixin) "gh-pulls" "Git pull requests API")

(gh-defclass gh-pulls-comment (gh-comment) ((path :initarg :path) (diff-hunk :initarg :diff-hunk) (position :initarg :position) (original-position :initarg :original-position) (commit-id :initarg :commit-id) (original-commit-id :initarg :original-commit-id) (in-reply-to :initarg :in-reply-to :initform nil)))

(gh-defclass gh-pulls-request-stub (gh-ref-object) ((diff-url :initarg :diff-url) (patch-url :initarg :patch-url) (issue-url :initarg :issue-url) (number :initarg :number) (state :initarg :state) (title :initarg :title) (body :initarg :body) (created-at :initarg :created-at) (updated-at :initarg :updated-at) (closed-at :initarg :closed-at) (merged-at :initarg :merged-at) (head :initarg :head :initform nil :marshal-type gh-repos-ref) (base :initarg :base :initform nil :marshal-type gh-repos-ref)))

(gh-defclass gh-pulls-request (gh-pulls-request-stub) ((merged :initarg :merged) (mergeable :initarg :mergeable) (merged-by :initarg :merged-by) (comments :initarg :comments) (user :initarg :user :initform nil :marshal-type gh-user) (commits :initarg :commits) (additions :initarg :additions) (deletions :initarg :deletions) (changed-files :initarg :changed-files)) "Git pull requests API")

;;;***

;;;### (autoloads nil "gh-repos" "gh-repos.el" (23009 21887 0 0))
;;; Generated autoloads from gh-repos.el

(require 'eieio)

(eieio-defclass-autoload 'gh-repos-api '(gh-api-v3) "gh-repos" "Repos API")

(gh-defclass gh-repos-repo-stub (gh-object) ((name :initarg :name) (description :initarg :description) (homepage :initarg :homepage) (private :initarg :private)) "Class for user-created repository objects")

(gh-defclass gh-repos-repo (gh-ref-object gh-repos-repo-stub) ((clone-url :initarg :clone-url) (git-url :initarg :git-url) (ssh-url :initarg :ssh-url) (svn-url :initarg :svn-url) (mirror-url :initarg :mirror-url) (owner :initarg :owner :initform nil :marshal-type gh-user) (full-name :initarg :full-name) (language :initarg :language) (fork :initarg :fork) (forks :initarg :forks) (forks-count :initarg :forks-count) (watchers :initarg :watchers) (watchers-count :initarg :watchers-count) (stargazers-count :initarg :stargazers-count) (size :initarg :size) (master-branch :initarg :master-branch) (open-issues :initarg :open-issues) (pushed-at :initarg :pushed-at) (created-at :initarg :created-at) (updated-at :initarg :updated-at) (organisation :initarg :organisation :initform nil :marshal-type gh-user) (parent :initarg :parent :marshal-type gh-repos-repo) (source :initarg :source :marshal-type gh-repos-repo) (has-issues :initarg :has-issues) (has-wiki :initarg :has-wiki) (has-downloads :initarg :has-downloads)) "Class for GitHub repositories")

(gh-defclass gh-repos-ref (gh-object) ((label :initarg :label) (ref :initarg :ref :initform nil) (sha :initarg :sha :initform nil) (user :initarg :user :initform nil :marshal-type gh-user) (repo :initarg :repo :initform nil :marshal-type gh-repos-repo)))

;;;***

;;;### (autoloads nil "gh-search" "gh-search.el" (23009 21886 0 0))
;;; Generated autoloads from gh-search.el

(eieio-defclass-autoload 'gh-search-api '(gh-api-v3) "gh-search" nil)

;;;***

;;;### (autoloads nil "gh-url" "gh-url.el" (23009 21886 0 0))
;;; Generated autoloads from gh-url.el

(require 'eieio)

(eieio-defclass-autoload 'gh-url-request 'nil "gh-url" nil)

(eieio-defclass-autoload 'gh-url-response 'nil "gh-url" nil)

(eieio-defclass-autoload 'gh-url-callback 'nil "gh-url" nil)

;;;***

;;;### (autoloads nil "gh-users" "gh-users.el" (23009 21887 0 0))
;;; Generated autoloads from gh-users.el

(require 'eieio)

(eieio-defclass-autoload 'gh-users-api '(gh-api-v3) "gh-users" "Users API")

(gh-defclass gh-users-user (gh-user) ((gravatar-id :initarg :gravatar-id) (html-url :initarg :html-url) (followers-url :initarg :followers-url) (following-url :initarg :following-url) (gists-url :initarg :gists-url) (starred-url :initarg :starred-url) (subscriptions-url :initarg :subscriptions-url) (organizations-url :initarg :organizations-url) (repos-url :initarg :repos-url) (events-url :initarg :events-url) (received-events-url :initarg :received-events-url) (type :initarg :type) (site-admin :initarg :site-admin) (name :initarg :name) (company :initarg :company) (blog :initarg :blog) (location :initarg :location) (email :initarg :email) (hireable :initarg :hireable) (bio :initarg :bio) (public-repos :initarg :public-repos) (public-gists :initarg :public-gists) (followers :initarg :followers) (following :initarg :following) (created-at :initarg :created-at) (update-at :initarg :update-at)))

;;;***

;;;### (autoloads nil nil ("gh-pkg.el" "gh-profile.el" "gh.el") (23009
;;;;;;  21886 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gh-autoloads.el ends here
