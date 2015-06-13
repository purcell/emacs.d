(require 'org)


(setq org-latex-to-pdf-process
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "iconv -f utf-8 -t gbk %b.out > %b.out.bak"
    "mv %b.out.bak %b.out"
    "gbk2uni %b.out"
    "pdflatex -interaction nonstopmode -output-directory %o %f"
    "rm %b.out.bak %b.tex"))

(setq org-export-latex-packages-alist '(
    (""   "CJK"   t)
    (""     "indentfirst"  nil)
    ("pdftex"     "graphicx"  t)
    (""     "fancyhdr" nil)
    ("CJKbookmarks=true"     "hyperref"  nil)
"%% Define a museincludegraphics command, which is
%%   able to handle escaped special characters in image filenames.
\\def\\museincludegraphics{%
  \\begingroup
  \\catcode`\\\|=0
  \\catcode`\\\\=12
  \\catcode`\\\#=12
  \\includegraphics[width=0.75\\textwidth]
}"))

(add-to-list 'org-export-latex-classes
                  '("cjk-article"
                    "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defun org-export-latex-make-header (title opt-plist)
  "Make the LaTeX header and return it as a string.
TITLE is the current title from the buffer or region.
OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents))
        (author (org-export-apply-macros-in-string
                 (plist-get opt-plist :author)))
        (email (replace-regexp-in-string
                "_" "\\\\_"
                (org-export-apply-macros-in-string
                 (plist-get opt-plist :email)))))
    (concat
     (if (plist-get opt-plist :time-stamp-file)
         (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; insert LaTeX custom header and packages from the list
     (org-splice-latex-header
      (org-export-apply-macros-in-string org-export-latex-header)
      org-export-latex-default-packages-alist
      org-export-latex-packages-alist nil
      (org-export-apply-macros-in-string
       (plist-get opt-plist :latex-header-extra)))
     ;; append another special variable
     (org-export-apply-macros-in-string org-export-latex-append-header)
     ;; define alert if not yet defined
     "\n\\providecommand{\\alert}[1]{\\textbf{#1}}"

     ;; changed by wuyao721@163.com
     ;; beginning of the document
     "\n\\begin{document}\n\n"
     "\n\\begin{CJK*}{UTF8}{gbsn}\n\n"
     ;; insert the title
     (format
      "\n\n\\title{%s}\n"
      (org-export-latex-fontify-headline title))
     ;; insert author info
     (if (plist-get opt-plist :author-info)
         (format "\\author{%s%s}\n"
                 (org-export-latex-fontify-headline (or author user-full-name))
                 (if (and (plist-get opt-plist :email-info) email
                          (string-match "\\S-" email))
                     (format "\\thanks{%s}" email)
                   ""))
       (format "%%\\author{%s}\n"
               (org-export-latex-fontify-headline (or author user-full-name))))
     ;; insert the date
     (format "\\date{%s}\n"
             (format-time-string
              (or (plist-get opt-plist :date)
                  org-export-latex-date-format)))
     ;; insert the title command
     (when (string-match "\\S-" title)
       (if (string-match "%s" org-export-latex-title-command)
           (format org-export-latex-title-command title)
         org-export-latex-title-command))
     "\n\n"
     ;; table of contents
     (when (and org-export-with-toc
                (plist-get opt-plist :section-numbers))
       (funcall org-export-latex-format-toc-function
                (cond ((numberp toc)
                       (min toc (plist-get opt-plist :headline-levels)))
                      (toc  (plist-get opt-plist :headline-levels))))))))

(defun org-export-as-latex (arg &optional hidden ext-plist
                                to-buffer body-only pub-dir)
  "Export current buffer to a LaTeX file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-latex-low-levels'.  The default is to
convert them as description lists.
HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting LaTeX as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of \\begin{document}...\\end{document},
without even the \\begin{document} and \\end{document} commands.
when PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (when (and (not body-only) arg (listp arg)) (setq body-only t))
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
             (not buffer-file-name))
    (if (buffer-base-buffer)
        (org-set-local 'buffer-file-name
                       (with-current-buffer (buffer-base-buffer)
                         buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to LaTeX...")
  (org-unmodified
   (let ((inhibit-read-only t))
     (remove-text-properties (point-min) (point-max)
                             '(:org-license-to-kill nil))))
  (org-update-radio-target-regexp)
  (org-export-latex-set-initial-vars ext-plist arg)
  (setq org-export-opt-plist org-export-latex-options-plist
        org-export-footnotes-data (org-footnote-all-labels 'with-defs)
        org-export-footnotes-seen nil
        org-export-latex-footmark-seen nil)
  (org-install-letbind)
  (run-hooks 'org-export-latex-after-initial-vars-hook)
  (let* ((wcf (current-window-configuration))
         (opt-plist
          (org-export-process-option-filters org-export-latex-options-plist))
         (region-p (org-region-active-p))
         (rbeg (and region-p (region-beginning)))
         (rend (and region-p (region-end)))
         (subtree-p
          (if (plist-get opt-plist :ignore-subtree-p)
              nil
            (when region-p
              (save-excursion
                (goto-char rbeg)
                (and (org-at-heading-p)
                     (>= (org-end-of-subtree t t) rend))))))
         (opt-plist (setq org-export-opt-plist
                          (if subtree-p
                              (org-export-add-subtree-options opt-plist rbeg)
                            opt-plist)))
         ;; Make sure the variable contains the updated values.
         (org-export-latex-options-plist (setq org-export-opt-plist opt-plist))
         ;; The following two are dynamically scoped into other
         ;; routines below.
         (org-current-export-dir
          (or pub-dir (org-export-directory :html opt-plist)))
         (org-current-export-file buffer-file-name)
         (title (or (and subtree-p (org-export-get-title-from-subtree))
                    (plist-get opt-plist :title)
                    (and (not
                          (plist-get opt-plist :skip-before-1st-heading))
                         (org-export-grab-title-from-buffer))
                    (and buffer-file-name
                         (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name)))
                    "No Title"))
         (filename
          (and (not to-buffer)
               (concat
                (file-name-as-directory
                 (or pub-dir
                     (org-export-directory :LaTeX ext-plist)))
                (file-name-sans-extension
                 (or (and subtree-p
                          (org-entry-get rbeg "EXPORT_FILE_NAME" t))
                     (file-name-nondirectory ;sans-extension
                      (or buffer-file-name
                          (error "Don't know which export file to use")))))
                ".tex")))
         (filename
          (and filename
               (if (equal (file-truename filename)
                          (file-truename (or buffer-file-name "dummy.org")))
                   (concat filename ".tex")
                 filename)))
         (buffer (if to-buffer
                     (cond
                      ((eq to-buffer 'string) (get-buffer-create
                                               "*Org LaTeX Export*"))
                      (t (get-buffer-create to-buffer)))
                   (find-file-noselect filename)))
         (odd org-odd-levels-only)
         (header (org-export-latex-make-header title opt-plist))
         (skip (cond (subtree-p nil)
                     (region-p nil)
                     (t (plist-get opt-plist :skip-before-1st-heading))))
         (text (plist-get opt-plist :text))
         (org-export-preprocess-hook
          (cons
           `(lambda () (org-set-local 'org-complex-heading-regexp
                                      ,org-export-latex-complex-heading-re))
           org-export-preprocess-hook))
         (first-lines (if skip "" (org-export-latex-first-lines
                                   opt-plist
                                   (if subtree-p
                                       (save-excursion
                                         (goto-char rbeg)
                                         (point-at-bol 2))
                                     rbeg)
                                   (if region-p rend))))
         (coding-system (and (boundp 'buffer-file-coding-system)
                             buffer-file-coding-system))
         (coding-system-for-write (or org-export-latex-coding-system
                                      coding-system))
         (save-buffer-coding-system (or org-export-latex-coding-system
                                        coding-system))
         (region (buffer-substring
                  (if region-p (region-beginning) (point-min))
                  (if region-p (region-end) (point-max))))
         (text
          (and text (string-match "\\S-" text)
               (org-export-preprocess-string
                text
                :emph-multiline t
                :for-backend 'latex
                :comments nil
                :tags (plist-get opt-plist :tags)
                :priority (plist-get opt-plist :priority)
                :footnotes (plist-get opt-plist :footnotes)
                :drawers (plist-get opt-plist :drawers)
                :timestamps (plist-get opt-plist :timestamps)
                :todo-keywords (plist-get opt-plist :todo-keywords)
                :tasks (plist-get opt-plist :tasks)
                :add-text nil
                :skip-before-1st-heading skip
                :select-tags nil
                :exclude-tags nil
                :LaTeX-fragments nil)))
         (string-for-export
          (org-export-preprocess-string
           region
           :emph-multiline t
           :for-backend 'latex
           :comments nil
           :tags (plist-get opt-plist :tags)
           :priority (plist-get opt-plist :priority)
           :footnotes (plist-get opt-plist :footnotes)
           :drawers (plist-get opt-plist :drawers)
           :timestamps (plist-get opt-plist :timestamps)
           :todo-keywords (plist-get opt-plist :todo-keywords)
           :tasks (plist-get opt-plist :tasks)
           :add-text (if (eq to-buffer 'string) nil text)
           :skip-before-1st-heading skip
           :select-tags (plist-get opt-plist :select-tags)
           :exclude-tags (plist-get opt-plist :exclude-tags)
           :LaTeX-fragments nil)))

    (set-buffer buffer)
    (erase-buffer)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the header and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert header))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-latex-content
               text '(lists tables fixed-width keywords))
               "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (string-match "^\\*" first-lines))
      (insert first-lines))

    ;; export the content of headlines
    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
         (let* ((asters (length (match-string 1)))
                (level (if odd (- asters 2) (- asters 1))))
           (setq org-export-latex-add-level
                 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
           (org-export-latex-parse-global level odd)))))

    ;; changed by wuyao721@163.com
    ;; finalization
    ;;(unless body-only (insert "\n\\end{document}"))
    (unless body-only (insert "\n\\end{CJK*}")(insert "\n\\end{document}"))

    ;; Attach description terms to the \item macro
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\\\item\\([ \t]+\\)\\[" nil t)
      (delete-region (match-beginning 1) (match-end 1)))

    ;; Relocate the table of contents
    (goto-char (point-min))
    (when (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
      (goto-char (point-min))
      (while (re-search-forward "\\\\tableofcontents\\>[ \t]*\n?" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (and (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
           (replace-match "\\tableofcontents" t t)))

    ;; Cleanup forced line ends in items where they are not needed
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*\\\\item\\>.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*\n\\\\begin"
            nil t)
      (delete-region (match-beginning 1) (match-end 1)))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*\\\\item\\>.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*"
            nil t)
      (if (looking-at "[\n \t]+")
          (replace-match "\n")))

    (run-hooks 'org-export-latex-final-hook)
    (if to-buffer
        (unless (eq major-mode 'latex-mode) (latex-mode))
      (save-buffer))
    (org-export-latex-fix-inputenc)
    (run-hooks 'org-export-latex-after-save-hook)
    (goto-char (point-min))
    (or (org-export-push-to-kill-ring "LaTeX")
        (message "Exporting to LaTeX...done"))
    (prog1
        (if (eq to-buffer 'string)
            (prog1 (buffer-substring (point-min) (point-max))
              (kill-buffer (current-buffer)))
          (current-buffer))
      (set-window-configuration wcf))))

(provide 'org-latex-hack)
