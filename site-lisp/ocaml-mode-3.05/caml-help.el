;; caml-info.el --- contextual completion and help to caml-mode

;; Didier Remy, November 2001.

;; This provides two functions completion and help
;; look for caml-complete and caml-help

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This is a preliminary version.
;;
;;  Possible improvements?
;;   - dump some databaes: Info, Lib, ...
;;   - accept a search path for local libraries instead of current dir
;;     (then distinguish between different modules lying in different
;;     directories) 
;;   - improve the construction for info files.
;;
;;  Abstract over 
;;   - the viewing method and the database, so that the documentation for
;;     and identifier could be search in 
;;       * info / html / man / mli's sources
;;       * viewed in emacs or using an external previewer.
;;
;;  Take all identifiers (labels, Constructors, exceptions, etc.)
;;       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Loading or building databases.
;; 

;; variables to be customized

(defvar ocaml-lib-path 'lazy
  "Path for ocaml lib sources (mli files)

'lazy means ask ocaml to find it for your at first use.")
(defun ocaml-lib-path ()
  "Computes if necessary and returns the path for ocaml libs"
  (if (listp 'ocaml-lib-path) nil
    (setq ocaml-lib-path
          (split-string
           (shell-command-to-string
            (or
             (and (boundp 'inferior-caml-program)
                      (string-match "\\([^ ]*/ocaml\\)\\( \\|$\\)"
                       inferior-caml-program)
                      (let ((file
                             (concat (match-string 1 inferior-caml-program)
                                     "c")))
                        (and (file-executable-p file)
                             (concat file " -where"))))
             "ocamlc -where"))))
    ocaml-lib-path))

      

;; General purpose auxiliary functions

(defun ocaml-capitalize (s)
  (concat (capitalize (substring s 0 1)) (substring s 1)))

(defun ocaml-uncapitalize (s)
  (concat (downcase (substring s 0 1)) (substring s 1)))

(defun iter (f l) (while (consp l) (apply f (list (car l))) (setq l (cdr l))))

(defun ocaml-find-files (path filter &optional depth split)
  (let* ((path-string
          (if (stringp path)
              (if (file-directory-p path) path nil)
            (mapconcat '(lambda (d) (if (file-directory-p d) d))
                       path " "))) 
         (command
          (and path-string
               (concat "find " path-string
                       " '(' " filter " ')' "
                       (if depth (concat " -maxdepth " (int-to-string depth)))
                       (if split nil " -printf '%\p '") 
                       )))
          (files
           (and command (shell-command-to-string command))))
         (if (and split (stringp files)) (split-string files "\n") files) 
         ))

;; Specialized auxiliary functions


;; Global table of modules contents of modules loaded lazily.

(defvar ocaml-module-alist 'lazy
  "A-list of modules with how and where to find help information. 
  'delay means non computed yet")

(defun ocaml-add-mli-modules (modules tag &optional path)
  (let ((files
         (ocaml-find-files (or path (ocaml-lib-path))
                           "-type f -name '*.mli'" 1 t)))
    (while (consp files)
      (if (string-match "\\([^/]*\\).mli" (car files))
          (let* ((module (ocaml-capitalize (match-string 1 (car files))))
                 (dir (file-name-directory (car files)))
                 (dirp (member dir (ocaml-lib-path))))
            (if (and (consp dirp) (string-equal dir (car dirp)))
                (setq dir (car dirp)))
            (if (assoc module modules) nil
              (setq modules
                    (cons (cons module (cons (cons tag dir) 'lazy)) modules))
              )))
      (setq files (cdr files)))
    modules))

(defun ocaml-module-alist ()
  "Call by need value of valriable ocaml-module-alist"
  (if (listp ocaml-module-alist)
      nil
    ;; build list of mli files
    (setq ocaml-module-alist (ocaml-add-mli-modules nil 'lib))
    ;; dumping information ? TODO
    )
  ocaml-module-alist)

(defun ocaml-get-or-make-module (module &optional tag)
  (let ((info (assoc module (ocaml-module-alist))))
    (if info nil
      (setq info (cons module (cons (cons 'local default-directory) 'lazy)))
      (setq ocaml-module-alist (cons info ocaml-module-alist))
      )
    info))

;; Symbols of module are lazily computed

(defun ocaml-module-filename (module)
  (let ((module (ocaml-uncapitalize module)) (name))
    (if (file-exists-p (setq name (concat module ".mli"))) nil
      (let ((tmp (ocaml-lib-path)))
        (while (consp tmp)
          (setq name (concat (car tmp) "/" module ".mli"))
          (if (file-exists-p name) (setq tmp nil)
            (setq name nil)))))
    name))

(defun ocaml-module-symbols (module-info)
  (let* ((module (car module-info))
         (tail (and module-info (cdr module-info)))
         (tag (caar tail))
         (dir (cdar tail))
         (file)
         (alist))
    (if (listp (cdr tail))
        (cdr tail)
      (if (equal tag 'info)
          (setq dir (car ocaml-lib-path)) ; XXX to be fixed
        )
      (setq file (concat dir (ocaml-uncapitalize module) ".mli"))
      (message file)
      (save-window-excursion
        (set-buffer (get-buffer-create "*caml-help*"))
        (if (and file (file-exists-p file))
            (progn
              (message "Scanning module %s" file)
              (insert-file-contents file))
          (message "Module %s not found" module))
        (while (re-search-forward
                "^\\([ \t]*val\\|let\\|external\\) \\([^ (:=]*\\)" (point-max) 'move)
          (setq alist (cons (match-string 2) alist)))
        (erase-buffer)
        )
      (setcdr tail alist)
      alist)
      ))

;; Local list of visible modules. 

(defvar ocaml-visible-modules 'lazy
  "A-list of open modules, local to every file.")
(make-variable-buffer-local 'ocaml-visible-modules)
(defun ocaml-visible-modules ()
  (if (listp ocaml-visible-modules) nil
    (progn
      (setq ocaml-visible-modules
            (list (ocaml-get-or-make-module "Pervasives")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^ *open  *\\([A-Z][a-zA-Z'_0-9]*\\)"
                                  (point-max) t)
          (let ((module (match-string 1)))
            (if (member module ocaml-visible-modules) nil
              (setq ocaml-visible-modules
                    (cons (ocaml-get-or-make-module module)
                          ocaml-visible-modules)))))
        )))
  ocaml-visible-modules)

;; Look for identifiers around point

(defun ocaml-qualified-identifier (&optional show)
  "Search for a qualified identifier (Path. entry) around point. 

Entry may be nil.
Currently, the path may only be nil or a single Module. 
For paths is of the form Module.Path', it returns Module 
and always nil for entry. 

If defined Module and Entry are represented by a region in the buffer, 
and are nil otherwise. 

For debugging purposes, it returns the string Module.entry if called 
with an optional non-nil argument. 
"
  (save-excursion
    (let ((module) (entry))
      (if (looking-at "[ \n]") (skip-chars-backward " ")) 
      (if (re-search-backward
           "[^A-Za-z0-9_.']\\([A-Za-z0-9_']*[.]\\)*[A-Za-z0-9_']*\\="
           (- (point) 100) t)
          (progn
            (forward-char 1)
            (if (looking-at "\\<\\([A-Za-z_][A-Za-z0-9_']*\\)[.]")
                (progn
                  (setq module (cons (match-beginning 1) (match-end 1)))
                  (goto-char (match-end 0))))
            (if (looking-at "\\<\\([a-z_][A-Za-z0-9_']*\\)\\>")
                (setq entry (cons (match-beginning 1) (match-end 1))))))
      (if show
          (concat
           (and module (buffer-substring (car module) (cdr module)))
           "."
           (and entry (buffer-substring (car entry) (cdr entry))))
      (cons module entry))
    )))

;; completion around point

(defun ocaml-completion (pattern module)
  (let ((list
         (or
          (and module
               (list 
                (or (assoc module (ocaml-module-alist))
                    (error "Unknown module %s" module))))
          (ocaml-visible-modules))))
    (message "Completion from %s" (mapconcat 'car list " "))
    (if (null pattern)
        (apply 'append (mapcar 'ocaml-module-symbols list))
      (let ((pat (concat "^" (regexp-quote pattern))) (res))
        (iter
         '(lambda (l)
            (iter '(lambda (x)
                     (if (string-match pat (car l))
                         (if (member x res) nil (setq res (cons x res)))))
                  (ocaml-module-symbols l)))
         list)
        res)
      )))

(defun caml-complete (arg)
  "Does completion for qualified identifiers. 

It attemps to recognize an qualified identifier Module . entry 
around point using function `ocaml-qualified-identifier'.

If Module is defined, it does completion for identifier in Module.

If Module is undefined, it does completion in visible modules. 
Then, if completion fails, it does completion among  all modules 
where identifier is defined."
  (interactive "p")
  (let* ((module-entry (ocaml-qualified-identifier))
         (module) 
         (entry (cdr module-entry))
         (beg) (end) (pattern))
    (if (car module-entry)
        (progn
          (setq module
                (buffer-substring (caar module-entry) (cdar module-entry)))
          (or (assoc module (ocaml-module-alist))
              (and (setq module
                         (completing-read "Module: " (ocaml-module-alist)
                                          nil nil module))
                   (save-excursion
                     (goto-char (caar module-entry))
                     (delete-region (caar module-entry) (cdar module-entry))
                     (insert module) t)
                   (setq module-entry (ocaml-qualified-identifier))
                   (car module-entry)
                   (progn (setq entry (cdr module-entry)) t))
              (error "Unknown module %s" module))))
    (if (consp (cdr module-entry))
        (progn         
          (setq beg (cadr module-entry))
          (setq end (cddr module-entry)))
      (if (and module
           (save-excursion
            (goto-char (cdar module-entry))
            (looking-at " *[.]")))
          (progn
            (setq beg (match-end 0))
            (setq end beg))))
    (if (not (and beg end))
        (error "Did not find anything to complete around point")

      (setq pattern (buffer-substring beg end))
      (let* ((table 'ocaml-completion)
             (all-completions (ocaml-completion pattern module))
             (completion
              (try-completion pattern (mapcar 'list all-completions))))
        (cond ((eq completion t))

              ((null completion)
               (let*
                   ((modules (ocaml-find-module pattern))
                    (hist)
                    (module
                     (cond
                      ((null modules)
                       nil)
                      ((equal (length modules) 1)
                       (caar modules))
                      (t
                       (setq hist (mapcar 'car modules))
                       (completing-read "Module: " modules nil t
                                        "" (cons 'hist 0)))
                      )))
                 (if (null module)
                     (error "Can't find completion for \"%s\"" pattern)
                   (message "Completion found in module %s" module)
                   (if (and (consp module-entry) (consp (cdr module-entry)))
                       (delete-region (caar module-entry) end)
                     (delete-region beg end))
                   (insert module "." pattern))))
                     
              ((not (string-equal pattern completion))
               (delete-region beg end)
               (insert completion))

              (t
                (with-output-to-temp-buffer "*Completions*"
                  (display-completion-list all-completions))
                ))
               ))))


;; Info files (only in ocamldoc style)


(defvar ocaml-info-basename "ocaml"
  "Basename of ocaml info files describing library modules.
Suffix .info will be added to info files. 
Additional suffix .gz may be added if info files are compressed.
")
;; 

(defun ocaml-hevea-info-add-entries (entries dir name)
  (let*
      ((filter
        (concat "-type f -regex '.*/" name
                "\\(.info\\|\\)\\(-[0-9]*\\|\\)\\([.]gz\\|\\)'"
                ))
       (section-regexp
        "\\* \\(Section [1-9][0-9--]*\\)::[ \t][ \t]*Module *\\([A-Z][A-Za-z_0-9]*\\)")
       (files (ocaml-find-files dir filter))
       (command))
    ;; scanning info files
    (if (or (null files)
            (not (stringp files))
            (string-match files "^ *$"))
        (message "No info file found: %s." (mapconcat 'identity files " "))
      (message "Scanning info files %s." files)
      (save-window-excursion
        (set-buffer (get-buffer-create "*caml-help*"))
        (setq command
              (concat "zcat -f " files
                      " | grep -e '" section-regexp "'"))
        (message "Scanning files with: %s" command)
        (or (shell-command command (current-buffer))
            (error "Error while scanning"))
        (goto-char (point-min))
        (while (re-search-forward section-regexp (point-max) t)
          (let* ((module (match-string 2))
                 (section (match-string 1)))
            ;; (message "%s %s" module section)
            (if (assoc module entries) nil
              (setq entries
                    (cons (cons module (concat "(" name ")" section))
                          entries))
              )))
        (let ((buf (get-buffer "*caml-help*")))
          (if buf (kill-buffer buf)))))
    entries))

(defun ocaml-hevea-info ()
  "The default way to create an info data base from the value 
of `Info-default-directory-list' and the base name `ocaml-info-name'
of files with basename `ocaml-info-basename' to look for. 

This uses info files produced by HeVeA.
"
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (ocaml-hevea-info-add-entries
                      collect d ocaml-info-basename))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

(defun ocaml-ocamldoc-info-add-entries (entries dir name)
  (let*
      ((module-regexp "^Node: \\([A-Z][A-Za-z_0-9]*\\)[^ ]")
       (command
        (concat
         "find " dir " -type f -regex '.*/" name
         "\\(.info\\|\\)\\([.]gz\\|\\)' -print0"
         " | xargs -0 zcat -f | grep '" module-regexp "'")))
    (message "Scanning info files in %s" dir)
    (save-window-excursion
      (set-buffer (get-buffer-create "*caml-help*"))
      (or (shell-command command (current-buffer)) (error "HERE"))
      (goto-char (point-min))
      (while (re-search-forward module-regexp (point-max) t)
        (if (equal (char-after (match-end 1)) 127)
            (let* ((module (match-string 1)))
              (if (assoc module entries) nil
                (setq entries
                      (cons (cons module (concat "(" name ")" module))
                            entries))
                ))))
      ; (kill-buffer (current-buffer))
      )
    entries))

(defun ocaml-ocamldoc-info ()
  "The default way to create an info data base from the value 
of `Info-default-directory-list' and the base name `ocaml-info-name' 
of files with basename `ocaml-info-basename' to look for. 

This uses info files produced by ocamldoc."
  (require 'info)
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (ocaml-ocamldoc-info-add-entries collect d
                                                      ocaml-info-prefix))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

;; Continuing

(defvar ocaml-info-alist nil
  "A-list binding module names to info entries: 

  nil means do not use info.

  A function to build the list lazily (at the first call). The result of
the function call will be assign permanently to this variable for future
uses. We provide two default functions `ocaml-hevea-info' and
`ocaml-ocamldoc-info'. 

  Otherwise, this value should be an alist binding module names to info
entries of the form to \"(entry)section\" be taken by the `info'
command. An entry may be an info module or a complete file name."
)

(defun ocaml-info-alist ()
  "Call by need value of variable ocaml-info-alist"
  (cond
   ((listp ocaml-info-alist))
   ((functionp ocaml-info-alist)
    (setq ocaml-info-alist (apply ocaml-info-alist nil)))
   (t
    (error "wrong type for ocaml-info-alist")))
  ocaml-info-alist)

;; help around point

(defun ocaml-find-module (symbol &optional module-list)
  (let ((list (or module-list (ocaml-module-alist)))
        (collect))
    (while (consp list)
      (if (member symbol (ocaml-module-symbols (car list)))
          (setq collect (cons (car list) collect)))
      (setq list (cdr list)))
    collect
    ))

(defun ocaml-buffer-substring (region)
  (and region (buffer-substring-no-properties (car region) (cdr region))))

;; Help function. 

(defun ocaml-goto-help (&optional module entry)
  "Searches info manual for MODULE and ENTRY in MODULE.
If unspecified, MODULE and ENTRY are inferred from the position in the
current buffer using `ocaml-qualified-identifier'."
  (interactive)
  (let ((info-section (assoc module (ocaml-info-alist))))
    (if info-section (info (cdr info-section))
      (ocaml-visible-modules)
      (let* ((module-info
              (or (assoc module (ocaml-module-alist))
                  (and (file-exists-p
                        (concat (ocaml-uncapitalize module) ".mli"))
                       (ocaml-get-or-make-module module))))                  
             (location (cdr (cadr module-info))))
        (cond
         (location
          (view-file (concat location (ocaml-uncapitalize module) ".mli"))
          (bury-buffer (current-buffer)))
         (info-section (error "Aborted"))
         (t (error "No help for module %s" module))))
      ))
  (if (stringp entry)
      (let ((here (point)))
        (goto-char (point-min))
        (or (re-search-forward
             (concat "\\(val\\|exception\\|external\\|[|{;]\\) +"
                     (regexp-quote entry))
             (point-max) t)
            (search-forward entry (point-max) t)
            (progn
              (message "Help for entry %s not found in module %s"
                       entry module)
              (goto-char here)))))
  )

(defun caml-help (arg)
  "Find help for qualified identifiers. 

It attemps to recognize an qualified identifier of the form Module . entry 
around point using function `ocaml-qualified-identifier'.

If Module is undefined it finds it from indentifier and visible modules, 
or asks the user interactively. 

It then opens the info documentation for Module if available or 
to the Module.mli file otherwises, and searches for entry. 

With prefix arg 0, it recomputes visible modules and their content. 
With prefix arg 4, it prompt for Module instead of its contectual value. 
"
  (interactive "p")
  (let ((module) (entry))
    (cond
     ((= arg 4)
      (or (and
           (setq module
                (completing-read "Module: " ocaml-module-alist nil t))
           (not (string-equal module "")))
          (error "Quit")))
     (t
      (if (= arg 0) (setq ocaml-visible-modules 'lazy))
      (let ((module-entry (ocaml-qualified-identifier)))
        (setq entry (ocaml-buffer-substring (cdr module-entry)))
        (setq module
              (or (ocaml-buffer-substring (car module-entry))
                  (let ((modules
                         (or (ocaml-find-module entry (ocaml-visible-modules))
                             (ocaml-find-module entry)))
                         (hist))
                    (cond
                     ((null modules)
                      (error "No module found for entry %s" entry))
                     ((equal (length modules) 1)
                      (caar modules))
                     (t
                      (setq hist (mapcar 'car modules))
                      (completing-read "Module: " modules nil t
                                       "" (cons 'hist 0)))
                     ))))
        )))
     (message "Help for %s%s%s" module (if entry "." "") (or entry ""))
     (ocaml-goto-help module entry)
     ))


;; bindings

(if (and (boundp 'caml-mode-map) (keymapp caml-mode-map))
    (progn 
      (define-key caml-mode-map [?\C-c?\C-h] 'caml-help)
      (define-key caml-mode-map [?\C-c?\t] 'caml-complete)
      ))

(provide 'caml-help)
