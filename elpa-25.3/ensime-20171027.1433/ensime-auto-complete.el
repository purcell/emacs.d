;;; ensime-auto-complete.el
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
(require 'ensime-completion-util)
(require 'ensime-model)
(require 'ensime-util)

;; we don't explicitly depend on auto-complete, this file should
;; really be released as a separate package.
;; (require 'auto-complete)
(autoload 'ac-define-source "auto-complete")
(autoload 'ac-set-trigger-key "auto-complete")
(autoload 'auto-complete-mode "auto-complete")

(defcustom ensime-ac-enable-argument-placeholders t
  "If non-nil, insert placeholder arguments in the buffer on completion."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-ac-override-settings t
    "If non-nil, override auto-complete settings."
    :type 'boolean
    :group 'ensime-ui)

(defcustom ensime-ac-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defvar ensime-ac-max-results 30
  "Maximum number of completions to request in one call to server.")

(defun ensime-ac-completion-candidates (&optional prefix)
  "Return candidate list of propertized strings."
  (let* ((completions
	  (plist-get (ensime-get-completions
		      ensime-ac-max-results
		      ensime-ac-case-sensitive) :candidates)))
    (mapcar (lambda (m) (propertize m 'summary (ensime-ac-get-doc m)))
	    completions)))

(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (plist-get (get-text-property 0 'type-info item) :full-name))

(defun ensime-ac-completion-prefix ()
  "Starting at current point. Find the point of completion."
  (let ((prefix (ensime-completion-prefix-at-point)))
     (- (point) (length prefix))))

(defun ensime-ac-complete-action (&optional candidate-in)
  "Defines action to perform when user selects a completion candidate.
If the candidate is a callable symbol, add the meta-info about the
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
     (type-info (get-text-property 0 'type-info candidate))
     (is-callable (plist-get type-info :arrow-type))
	 (to-insert (get-text-property 0 'to-insert candidate))
	 (name-start-point (- (point) (length name))))

    ;; If an alternate to-insert string is available, delete the
    ;; candidate inserted into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If this member is callable, lookup call completion
    ;; information to show parameter hints.
    (when is-callable
      (let* ((call-info (get-text-property 0 'type-info candidate))
	     (param-sections (ensime-type-param-sections call-info)))
	(when (and call-info param-sections)

	  ;; Insert space or parens depending on the nature of the
	  ;; call
	  (save-excursion
	    (let* ((is-operator
		    (and (= 1 (length param-sections))
			 (= 1 (length (plist-get
				       (car param-sections) :params)))
			 (null (string-match "[A-z]" name)))))
	      (if ensime-ac-enable-argument-placeholders
		  (let ((args (ensime-ac-call-info-argument-list
			       call-info is-operator)))
		    (cond
		     (is-operator (insert (concat " " args)))
		     (t (insert args))))
		(cond
		 (is-operator (insert " "))
		 (t (insert "()"))))))

	  (if (car param-sections)
	      (progn
		;; Save param info as a text properties of the member name..
		(add-text-properties name-start-point
				     (+ name-start-point (length name))
				     (list 'call-info call-info
					   ))

		;; Setup hook function to show param help later..
		(add-hook 'post-command-hook
			  'ensime-ac-update-param-help nil t)
		;; This command should trigger help hook..
		(forward-char))

	    ;; Otherwise, skip to the end
	    (forward-char 2))

	  )))))

(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that
   we are currently completing."
  (save-excursion
    (catch 'return
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment-p (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((call-info (get-text-property (point) 'call-info)))
	      (if (and (or (> balance 0)) call-info)
		  (throw 'return (list
				  :name-end-point (point)
				  :call-info call-info))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))
	       (call-info (plist-get info :call-info))
	       (signature (ensime-ac-call-info-signature call-info)))
	  (message signature))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))

(defun ensime-ac-call-info-argument-list (call-info &optional is-operator)
  "Return a pretty string representation of argument list."
  (let ((param-sections (plist-get call-info :param-sections)))
    (mapconcat
     (lambda (sect)
       (let* ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit))
	      (result
	       (concat (if is-operator "" "(")
		       (mapconcat
			(lambda (nm-and-tp)
			  (format
			   "%s:%s"
			   (propertize (car nm-and-tp)
				       'face font-lock-variable-name-face)
			   (propertize (ensime-type-name-with-args
					(cadr nm-and-tp))
				       'face font-lock-type-face)
			   ))
			params ", ") (if is-operator "" ")"))))
	 (if is-implicit
	     (propertize result 'face font-lock-comment-face)
	   result)
	 ))
     param-sections "=>" )))


(defun ensime-ac-call-info-signature (call-info)
  "Return a pretty string representation of a call-info object."
  (let ((param-sections (plist-get call-info :param-sections))
	(result-type (plist-get call-info :result-type)))
    (concat
     (ensime-ac-call-info-argument-list call-info)
     " => "
     (propertize
      (ensime-type-name-with-args result-type)
      'face font-lock-type-face)
     )))

(ac-define-source ensime-completions
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-completion-candidates ac-prefix))
    (prefix . ensime-ac-completion-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    ))

;;;###autoload
(defun ensime-ac-enable ()
  (when ensime-ac-override-settings
    (make-local-variable 'ac-sources)
	(setq ac-sources '(ac-source-ensime-completions))

	(make-local-variable 'ac-use-comphist)
	(setq ac-use-comphist nil)

	(make-local-variable 'ac-auto-show-menu)
	(setq ac-auto-show-menu 0.5)

	(make-local-variable 'ac-candidates-cache)
	(setq ac-candidates-cache nil)

	(make-local-variable 'ac-auto-start)
	(setq ac-auto-start nil)

	(make-local-variable 'ac-expand-on-auto-complete)
	(setq ac-expand-on-auto-complete t)

	(make-local-variable 'ac-use-fuzzy)
	(setq ac-use-fuzzy nil)

	(make-local-variable 'ac-dwim)
	(setq ac-dwim nil)

	(make-local-variable 'ac-use-quick-help)
	(setq ac-use-quick-help t)

	(make-local-variable 'ac-delete-dups)
	(setq ac-delete-dups nil)

	(make-local-variable 'ac-ignore-case)
	(setq ac-ignore-case t)

	(make-local-variable 'ac-trigger-key)
	(ac-set-trigger-key "TAB")

	(auto-complete-mode 1)
  ))

(defun ensime-ac-disable ()
  (auto-complete-mode 0))

(defun ensime-completion-at-point-function ()
  "Standard Emacs 24+ completion function, handles completion-at-point requests.
 See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html"
  (when (ensime-connected-p)
    (let* ((prefix (ensime-completion-prefix-at-point))
           (start (- (point) (length prefix)))
           (end (point))
           (props '(:annotation-function
                    (lambda (m)
                      (let* ((type-info (get-text-property 0 'type-info m))
                             (is-callable (plist-get type-info :arrow-type)))
                        (when is-callable
                          (plist-get (get-text-property 0 'type-info m) :full-name))))
                    :exit-function
                    (lambda (m status)
                      (when (eq status 'finished)
                        (ensime-ac-complete-action m)))))
           (completion-func
            (lambda (prefix pred action)
              (cond
               ((eq action 'metadata)
                '(metadata . ((display-sort-function . identity))))
               (t
                (complete-with-action
                 action (plist-get (ensime-get-completions 1000000 nil)
                                   :candidates) prefix pred))))))
      `(,start ,end ,completion-func . ,props))))

(provide 'ensime-auto-complete)

;; Local Variables:
;; End:

