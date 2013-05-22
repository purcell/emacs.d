(require 'org-export-generic)

(defun test-preproc ()
  (interactive)
  (let ((string
         (let ((region
                (buffer-substring
                 (if (org-region-active-p) (region-beginning) (point-min))
                 (if (org-region-active-p) (region-end) (point-max))))
               (opt-plist (org-combine-plists (org-default-export-plist)
                                              (org-infile-export-plist)))
               (export-plist '("tikiwiki" :file-suffix ".txt" :key-binding 85 :header-prefix "" :header-suffix "" :title-format "-= %s =-\n" :date-export nil :toc-export nil :body-header-section-numbers nil :body-section-prefix "\n" :body-section-header-prefix
                               ("! " "!! " "!!! " "!!!! " "!!!!! " "!!!!!! " "!!!!!!! ")
                               :body-section-header-suffix
                               (" \n" " \n" " \n" " \n" " \n" " \n")
                               :body-line-export-preformated t :body-line-format "%s " :body-line-wrap nil :body-line-fixed-format " %s\n" :body-list-format "* %s\n" :body-number-list-format "# %s\n" :blockquote-start "\n^\n" :blockquote-end "^\n\n" :body-newline-paragraph "\n" :bold-format "__%s__" :italic-format "''%s''" :underline-format "===%s===" :strikethrough-format "--%s--" :code-format "-+%s+-" :verbatim-format "~pp~%s~/pp~")))
    (org-export-preprocess-string
		  region
		  :for-ascii t
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :drawers (plist-get export-plist :drawers-export)
		  :tags (plist-get export-plist :tags-export)
		  :priority (plist-get export-plist :priority-export)
		  :footnotes (plist-get export-plist :footnotes-export)
		  :timestamps (plist-get export-plist :timestamps-export)
		  :todo-keywords (plist-get export-plist :todo-keywords-export)
		  :verbatim-multiline t
		  :select-tags (plist-get export-plist :select-tags-export)
		  :exclude-tags (plist-get export-plist :exclude-tags-export)
                  :emph-multiline t
		  :archived-trees
		  (plist-get export-plist :archived-trees-export)
		  :add-text (plist-get opt-plist :text)))))
    (save-excursion
      (org-pop-to-buffer-same-window "*preproc-temp*")
      (point-max)
      (insert string))))
