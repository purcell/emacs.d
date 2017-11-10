;;; ensime-config.el
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

(require 'dash)

(defvar ensime-config-file-name ".ensime"
  "The default file name for ensime project configurations.")

(defun ensime-config-for-buffer ()
  "Resolve the config for the current buffer via the ENSIME connection."
  (let ((connection (ensime-connection)))
    (ensime-config connection)))

(defun ensime-process-for-config (&optional config)
  "Obtain the ENSIME Server process for the given CONFIG (auto discovered if NIL)."
  ;; this is a bit of a hack, we should always have ready access to this
  (let ((config (or config (ensime-config-for-buffer))))
   (-first (lambda (p) (eq config (process-get p :ensime-config)))
           ensime-server-processes)))

(defun ensime-subproject-for-config (&optional config)
  "Obtain the subproject for the current buffer for the given CONFIG (auto discovered if NIL)."
  ;; this is a good candidate for caching
  (let ((config (or config (ensime-config-for-buffer))))
   (let* ((case-insensitive-fs t) ;; https://github.com/ensime/ensime-emacs/issues/532
          (canonical (convert-standard-filename (buffer-file-name-with-indirect)))
          (subprojects (plist-get config :subprojects))
          (matches-subproject-dir? (lambda (dir) (s-starts-with-p dir canonical case-insensitive-fs)))
          (find-subproject (lambda (sp)
                             (-any matches-subproject-dir? (plist-get sp :source-roots)))))
     (-> (-find find-subproject subprojects) (plist-get :name)))))


;; DEPRECATED: these getters should be replaced with a function that takes the key
(defun ensime--get-cache-dir (config)
  (let ((cache-dir (plist-get config :cache-dir)))
    (unless cache-dir
      (error "Cache dir in ensime configuration file appears to be unset"))
    cache-dir))

(defun ensime--get-root-dir (config)
  (let ((root-dir (plist-get config :root-dir)))
    (unless root-dir
      (error "Root dir in ensime configuration file appears to be unset"))
    root-dir))

(defun ensime--get-name (config)
  (let ((name (plist-get config :name)))
    (unless name
      (error "Name in ensime configuration file appears to be unset"))
    name))

(defun ensime--get-java-home (config)
  (let ((value (plist-get config :java-home)))
    (unless value
      (error "java-home in ensime configuration file appears to be unset"))
    value))

(defun ensime-config-source-roots (conf)
  "Returns a list of all directories mentioned in :source-roots directives."
  (let ((subs (plist-get conf :subprojects)))
    (-mapcat (lambda (sub) (plist-get sub :source-roots)) subs)))

(defun ensime-source-jars-dir (config)
  "Directory containing extracted dependency sources for the given CONFIG."
  (let ((cache-dir (ensime--get-cache-dir config)))
    (concat cache-dir "/dep-src/source-jars/")))

(defvar ensime--cache-source-root-set nil)
(defun ensime--source-root-set (conf no-ref-sources)
  "Returns a hash set containing all source directories (expanded with
 file-truename) of the give config."
  (or
   (cdr (assoc (list conf no-ref-sources) ensime--cache-source-root-set))

   (let ((result (make-hash-table :test 'equal)))
     (dolist (f (ensime-config-source-roots conf))
       (when (file-directory-p f)
	 (puthash (file-name-as-directory (file-truename f)) t result)))
     (unless no-ref-sources
       (-when-let (f (ensime-source-jars-dir conf))
         (puthash (file-name-as-directory (file-truename f)) t result)))

     (setq ensime--cache-source-root-set
	   (cons (cons (list conf no-ref-sources) result)
		 ensime--cache-source-root-set))

     result)))

(defun ensime-config-includes-source-file
    (conf file &optional no-ref-sources)
  "`t' if FILE is contained in `:source-roots' or the extracted dependencies.
NO-REF-SOURCES allows skipping the extracted dependencies."
  (when file
    (let ((dir-set (ensime--source-root-set conf no-ref-sources)))
      (let ((d (file-name-directory (expand-file-name file))))
        (catch 'return
          (while d
            (let ((prev d))
              (when (gethash (file-truename d) dir-set)
                (throw 'return t))
              (setq d (file-name-directory (directory-file-name d)))
              (when (equal d prev)
                (throw 'return nil)))))))))

(defun ensime-default-config-file (&optional dir)
  (expand-file-name ensime-config-file-name dir))

(defun ensime-config-find-file (file-name)
  "Search up the directory tree starting at file-name
   for a suitable config file to load, return it's path. Return nil if
   no such file found."
  (let* ((dir (file-name-directory file-name))
	 (possible-path (ensime-default-config-file dir)))
    (when (and dir (file-directory-p dir))
      (if (file-exists-p possible-path)
         possible-path
	(if (not (equal dir (directory-file-name dir)))
	    (ensime-config-find-file (directory-file-name dir)))))))

(defun ensime-config-find (&optional force-dir)
  "Query the user for the path to a config file, then load it."
  (let* ((hint (or force-dir buffer-file-name default-directory))
	 (guess (when hint (ensime-config-find-file hint)))
	 (file (if ensime-prefer-noninteractive
                   guess
		 (read-file-name
		  "ENSIME Project file: "
		  (if guess (file-name-directory guess))
		  guess
		  nil
		  (if guess (file-name-nondirectory guess) "")))))
    (if (and file
             (file-exists-p file)
             (not (file-directory-p file)))
        file
      (warn (concat
              "Could not find an ENSIME project file. "
              "See http://ensime.org/build_tools"))
      nil)))

(defun ensime-config-load (file-name)
  "Load, parse, and return FILE-NAME as a Lisp object."
  (condition-case problem
      (with-temp-buffer
        (insert-file-contents file-name)
        (read (current-buffer)))
    (error (error "Error reading configuration file, %s: %s" file-name problem))))

(defun ensime-source-roots-from-config ()
  "Return all source directories from all subprojects"
  (-flatten
   (mapcar
    (lambda (m) (plist-get m :source-roots))
    (plist-get (ensime-config (ensime-connection)) :subprojects))))


;; Confing auto-gen -- sbt only

(defun ensime-refresh-config ()
  "Try to refresh the ENSIME config file based on the project definition. Currently
only sbt projects are supported."
  (interactive)
  (ensime--maybe-refresh-config
   t
   '(lambda () (message "ENSIME config updated."))
   '(lambda (reason) (message "ENSIME config not updated: %s" reason))))

(defun ensime--config-and-generator (project-root)
  "Returns a cons cell consisting of the config file
corresponding to the current buffer, followed by the sbt task
needed to regenerate that config file. (Doesn't understand nested
project directories, because neither does ensime-sbt.)"
  (if (equal (expand-file-name "project/" project-root) default-directory)
      (cons (ensime-default-config-file) "ensimeConfigProject")
    (cons (ensime-default-config-file project-root) "ensimeConfig")))

(defun ensime--maybe-refresh-config (force after-refresh-fn no-refresh-fn)
  (let ((no-refresh-reason "couldn't detect project type"))
    (-when-let (project-root (sbt:find-root))
      (let* ((c-and-g (ensime--config-and-generator project-root))
             (config-file (car c-and-g))
             (generator-task (cdr c-and-g)))
        (if (or force
                (ensime--config-sbt-needs-refresh-p config-file))
            (progn
              (setq no-refresh-reason nil)
              (ensime--refresh-config-sbt project-root generator-task after-refresh-fn))
          (setq no-refresh-reason "config up to date"))))

    (when no-refresh-reason
      (funcall no-refresh-fn no-refresh-reason))))

(defun ensime--refresh-config-sbt (project-root task on-success-fn)
  (with-current-buffer (get-buffer-create "*ensime-gen-config*")
    (erase-buffer)
      (let ((default-directory (file-name-as-directory project-root)))
        (if (executable-find ensime-sbt-command)
            (let ((process (start-process "*ensime-gen-config*" (current-buffer)
                                          ensime-sbt-command task)))
              (display-buffer (current-buffer) nil)
        (set-process-sentinel process
                              `(lambda (process event)
                                 (ensime--refresh-config-sentinel process
                                                                  event
                                                                  ',on-success-fn)))
              (message "Updating ENSIME config..."))
          (error "sbt command not found")))))

(defun ensime--refresh-config-sentinel (process event on-success-fn)
  (cond
   ((equal event "finished\n")
    (-when-let (win (get-buffer-window (process-buffer process)))
      (delete-window win))
    (funcall on-success-fn))
   (t
    (message "Process %s exited: %s" process event))))

(defun ensime--config-sbt-needs-refresh-p (config-file)
  (let* ((project-root (file-name-directory config-file))
         (sbt-project (ensime--join-paths project-root "project"))
         (sbt-files (append (directory-files project-root t ".*\\.sbt")
                            (if (file-exists-p sbt-project)
                                (directory-files sbt-project t ".*\\.scala")
                              nil))))
    (if sbt-files
        (ensime--dependencies-newer-than-target-p config-file sbt-files)
      nil)))


(provide 'ensime-config)

;; Local Variables:
;; End:
;;
;;; ensime-config.el ends here
