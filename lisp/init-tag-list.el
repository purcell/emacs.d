;;; Package -- Simple tag list
;;; Commentary:
;;; The code is copied from Frank Pan
;;; @see: http://frankpzh.wordpress.com/2011/06/14/light-weight-taglist-in-emacs/
;;; Code:

(defvar taglist-mode-hook nil)

(defvar taglist-keywords
  (list (list "^\t\\([^ ]*\\) \\(L[0-9]+\\) *\\(.*\\)$" 1 font-lock-keyword-face)
	(list "^\t\\([^ ]*\\) \\(L[0-9]+\\) *\\(.*\\)$" 2 font-lock-comment-delimiter-face)
	(list "^\t\\([^ ]*\\) \\(L[0-9]+\\) *\\(.*\\)$" 3 font-lock-function-name-face)))

(defvar taglist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'taglist-jump)
    (define-key map (kbd "q") 'taglist-quit)
    map))

(defvar taglist-window nil)
(defvar taglist-current 0)

(defun base-buffer-file-name (buffer)
  "Return FILE from base BUFFER.
The cloned buffer need to get FILE from 'buffer-base-buffer' function."
  (if (buffer-base-buffer buffer)
      (buffer-file-name (buffer-base-buffer buffer))
    (buffer-file-name buffer)))

(defun taglist nil
  (interactive)
  (require 'speedbar)
  (require 'imenu)

  ;; Clear cache
  (setq imenu--index-alist nil)

  (let ((source-buffer (current-buffer))
	(current-line (line-number-at-pos)))

    ;; Create a buffer
    (if (get-buffer "*etags tmp*")
	(kill-buffer "*etags tmp*"))
    (if (get-buffer "*etags list*")
	(kill-buffer "*etags list*"))
    (set-buffer (get-buffer-create "*etags list*"))

    ;; Call speedbar tags
    (setq taglist-current 0)
    (taglist-fill-tags
     source-buffer
     (cddr (speedbar-fetch-dynamic-tags
	    (base-buffer-file-name source-buffer)))
     ""
     current-line)

    (goto-char (point-min))
    (forward-line (1- taglist-current))

    (setq taglist-window (split-window-vertically))
    (set-window-buffer taglist-window "*etags list*")
    (select-window taglist-window)
    (taglist-mode)))

(defun taglist-fill-tags (source-buffer tags prefix current)
  (while tags
    (if (integer-or-marker-p (cdar tags))
	(let ((tag-line
	       (with-current-buffer source-buffer
		 (line-number-at-pos (cdar tags)))))
	  (insert (format "\t%s L%-5d%s%s\n"
			  (buffer-name source-buffer)
			  tag-line
			  prefix
			  (caar tags)))
	  (when (>= current tag-line)
	    (setq taglist-current
		  (1+ taglist-current))))
      (let* ((dir-string (caar tags))
	     (marker (get-text-property 0 'org-imenu-marker dir-string))
	     (tag-line 0))
	(if marker
	  (setq tag-line
		(with-current-buffer source-buffer
		  (line-number-at-pos marker))))
	(insert (format "\t%s L%-5d%s%s\n"
			(buffer-name source-buffer)
			tag-line
			prefix
			(caar tags)))
	(when (>= current tag-line)
	  (setq taglist-current
		(1+ taglist-current)))
	(taglist-fill-tags source-buffer
			   (cdar tags)
			   (concat "+-" prefix)
			   current)))
    (setq tags (cdr tags))))

(defun taglist-kill nil
  (if (and taglist-window
	   (window-live-p taglist-window)
	   (not (one-window-p)))
      (delete-window taglist-window))
  (setq taglist-window nil)
  (kill-buffer "*etags list*"))

(defun taglist-jump nil
  (interactive)
  (let ((line (buffer-substring
	       (line-beginning-position)
	       (line-end-position))))
    (string-match "^\t\\([^ ]*\\) L\\([0-9]+\\)[^0-9]" line)
    (taglist-kill)
    (switch-to-buffer (match-string 1 line))
    (goto-char (point-min))
    (forward-line (1- (string-to-number (match-string 2 line))))))

(defun taglist-quit nil
  (interactive)
  (taglist-kill))

(defun taglist-mode nil
  (interactive)
  (kill-all-local-variables)
  (use-local-map taglist-map)
  (setq major-mode 'taglist-mode)
  (setq mode-name "Tag-List")
  (setq font-lock-defaults
	(list 'taglist-keywords))
  (run-mode-hooks 'taglist-mode-hook))

(provide 'init-tag-list)
;;; init-tag-list ends here
