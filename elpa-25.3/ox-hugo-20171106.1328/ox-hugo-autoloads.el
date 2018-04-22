;;; ox-hugo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ox-blackfriday" "ox-blackfriday.el" (23041
;;;;;;  39992 941643 783000))
;;; Generated autoloads from ox-blackfriday.el

(autoload 'org-blackfriday-export-as-markdown "ox-blackfriday" "\
Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org BLACKFRIDAY Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-blackfriday-convert-region-to-md "ox-blackfriday" "\
Convert text in the current region to Blackfriday Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it.

\(fn)" t nil)

(autoload 'org-blackfriday-export-to-markdown "ox-blackfriday" "\
Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-blackfriday-publish-to-blackfriday "ox-blackfriday" "\
Publish an Org file to Blackfriday Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

;;;***

;;;### (autoloads nil "ox-hugo" "ox-hugo.el" (23041 39992 961645
;;;;;;  24000))
;;; Generated autoloads from ox-hugo.el

(autoload 'org-hugo-slug "ox-hugo" "\
Return a slug string for STR.
STR is in Markdown format, most likely a Markdown heading.  The
returned slug string has the following specification:

- Should contain only lower case alphabet, number and hyphen
  characters.
- Remove *any* HTML tag like \"<code>..</code>\", \"<span
  class=..>..</span>\", etc from STR if present.
- URLs if present in STR should be removed.
- Replace \".\" in STR with \"and\", and \"&\" with \"and\".
- Parentheses should be replaced with double-hyphens ( \"foo (bar)
  baz\" becomes \"foo--bar--baz\").
- One or more consecutive spaces should be replaced with a single
  hyphen.
- Maximum number of consecutive hyphens allowed is two.
- No hyphens should be present at the leading or trailing end of the
  returned string .

\(fn STR)" nil nil)

(autoload 'org-hugo-export-as-md "ox-hugo" "\
Export current buffer to a Hugo-compatible Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Hugo Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-hugo-export-to-md "ox-hugo" "\
Export current buffer to a Hugo-compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-hugo-publish-to-md "ox-hugo" "\
Publish an Org file to Hugo-compatible Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(autoload 'org-hugo-export-subtree-to-md "ox-hugo" "\
Publish the current subtree to a Hugo post.
The next parent subtree having the \"EXPORT_FILE_NAME\" property
is exported if the current subtree doesn't have that property.

If ALL-SUBTREES is non-nil, publish all subtrees in the current
file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.  If ALL-SUBTREES is non-nil, return
nil.

\(fn &optional ALL-SUBTREES ASYNC VISIBLE-ONLY)" t nil)

(autoload 'org-hugo-export-subtree-to-md-after-save "ox-hugo" "\
Fn for `after-save-hook' to run `org-hugo-export-subtree-to-md'.
Executes `org-hugo-export-subtree-to-md', but only when in a
valid Hugo post subtree.

The export is also skipped if `org-hugo-allow-export-after-save'
is nil.

\(fn)" nil nil)

(autoload 'org-hugo-debug-info "ox-hugo" "\
Get Emacs, Org and Hugo version and ox-hugo customization info.
The information is converted to Markdown format and copied to the
kill ring.  The same information is displayed in the Messages
buffer and returned as a string in Org format.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ox-hugo-pkg.el") (23041 39992 957644
;;;;;;  777000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ox-hugo-autoloads.el ends here
