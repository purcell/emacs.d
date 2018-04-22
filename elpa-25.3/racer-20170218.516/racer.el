;;; racer.el --- code completion, goto-definition and docs browsing for Rust via racer  -*- lexical-binding: t -*-

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/racer-rust/emacs-racer
;; Package-Version: 20170218.516
;; Version: 1.3
;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0") (dash "2.13.0") (s "1.10.0") (f "0.18.2"))
;; Keywords: abbrev, convenience, matching, rust, tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
;; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Please see the readme for full documentation:
;; https://github.com/racer-rust/emacs-racer

;;; Quickstart:

;; You will need to configure Emacs to find racer:
;;
;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
;; (setq racer-cmd "<path-to-racer>/target/release/racer")
;;
;; To activate racer in Rust buffers, run:
;;
;; (add-hook 'rust-mode-hook #'racer-mode)
;;
;; You can also use racer to find definition at point via
;; `racer-find-definition', bound to `M-.' by default.
;;
;; Finally, you can also use Racer to show the signature of the
;; current function in the minibuffer:
;;
;; (add-hook 'racer-mode-hook #'eldoc-mode)

;;; Code:

(require 'dash)
(require 'etags)
(require 'rust-mode)
(require 's)
(require 'f)
(require 'thingatpt)
(require 'button)
(require 'help-mode)

(defgroup racer nil
  "Code completion, goto-definition and docs browsing for Rust via racer."
  :link '(url-link "https://github.com/racer-rust/emacs-racer/")
  :group 'rust-mode)

(defcustom racer-cmd
  (or (executable-find "racer")
      (f-expand "~/.cargo/bin/racer")
      "/usr/local/bin/racer")
  "Path to the racer binary."
  :type 'file
  :group 'racer)

(defcustom racer-rust-src-path
  (or
   (getenv "RUST_SRC_PATH")
   (when (executable-find "rustc")
     (let* ((sysroot (s-trim-right
                      (shell-command-to-string
                       (format "%s --print sysroot" (executable-find "rustc")))))
            (src-path (f-join sysroot "lib/rustlib/src/rust/src")))
       (when (file-exists-p src-path)
         src-path)
       src-path))
   "/usr/local/src/rust/src")

  "Path to the rust source tree.
If nil, we will query $RUST_SRC_PATH at runtime.
If $RUST_SRC_PATH is not set, look for rust source in rustup's install directory."
  :type 'file
  :group 'racer)

(defcustom racer-cargo-home
  (or
   (getenv "CARGO_HOME")
   "~/.cargo")
  "Path to your current cargo home. Usually `~/.cargo'.
If nil, we will query $CARGO_HOME at runtime."
  :type 'file
  :group 'racer)

(defun racer--cargo-project-root ()
  "Find the root of the current Cargo project."
  (let ((root (locate-dominating-file (or (buffer-file-name (buffer-base-buffer)) default-directory)
                                      "Cargo.toml")))
    (and root (file-truename root))))

(defun racer--header (text)
  "Helper function for adding text properties to TEXT."
  (propertize text 'face 'racer-help-heading-face))

(defvar racer--prev-state nil)

(defun racer-debug ()
  "Open a buffer describing the last racer command run.
Helps users find configuration issues, or file bugs on
racer or racer.el."
  (interactive)
  (unless racer--prev-state
    (user-error "Must run a racer command before debugging"))
  (let ((buf (get-buffer-create "*racer-debug*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (setq buffer-read-only t)
      (let* ((process-environment
              (plist-get racer--prev-state :process-environment))
             (rust-src-path-used
              (--first (s-prefix-p "RUST_SRC_PATH=" it) process-environment))
             (cargo-home-used
              (--first (s-prefix-p "CARGO_HOME=" it) process-environment))
             (stdout (plist-get racer--prev-state :stdout))
             (stderr (plist-get racer--prev-state :stderr)))
        (insert
         ;; Summarise the actual command that we run.
         (racer--header "The last racer command was:\n\n")
         (format "$ cd %s\n"
                 (plist-get racer--prev-state :default-directory))
         (format "$ export %s\n" cargo-home-used)
         (format "$ export %s\n" rust-src-path-used)
         (format "$ %s %s\n\n"
                 (plist-get racer--prev-state :program)
                 (s-join " " (plist-get racer--prev-state :args)))

         ;; Describe the exit code and outputs.
         (racer--header
          (format "This command terminated with exit code %s.\n\n"
                  (plist-get racer--prev-state :exit-code)))
         (if (s-blank? stdout)
             (racer--header "No output on stdout.\n\n")
           (format "%s\n\n%s\n\n"
                   (racer--header "stdout:")
                   (s-trim-right stdout)))
         (if (s-blank? stderr)
             (racer--header "No output on stderr.\n\n")
           (format "%s\n\n%s\n\n"
                   (racer--header "stderr:")
                   (s-trim-right stderr)))

         ;; Give copy-paste instructions for reproducing any errors
         ;; the user has seen.
         (racer--header
          (s-word-wrap 60 "The temporary file will have been deleted. You should be able to reproduce the same output from racer with the following command:\n\n"))
         (format "$ %s %s %s %s\n\n" cargo-home-used rust-src-path-used
                 (plist-get racer--prev-state :program)
                 (s-join " "
                         (-drop-last 1 (plist-get racer--prev-state :args))))

         ;; Tell the user what to do next if they have problems.
         (racer--header "Please report bugs ")
         (racer--url-button "on GitHub" "https://github.com/racer-rust/emacs-racer/issues/new")
         (racer--header "."))))
    (switch-to-buffer buf)
    (goto-char (point-min))))

(defun racer--call (command &rest args)
  "Call racer command COMMAND with args ARGS.
Return stdout if COMMAND exits normally, otherwise show an
error."
  (let ((rust-src-path (or racer-rust-src-path (getenv "RUST_SRC_PATH")))
        (cargo-home (or racer-cargo-home (getenv "CARGO_HOME"))))
    (when (null rust-src-path)
      (user-error "You need to set `racer-rust-src-path' or `RUST_SRC_PATH'"))
    (unless (file-exists-p rust-src-path)
      (user-error "No such directory: %s. Please set `racer-rust-src-path' or `RUST_SRC_PATH'"
                  rust-src-path))
    (let ((default-directory (or (racer--cargo-project-root) default-directory))
          (process-environment (append (list
                                        (format "RUST_SRC_PATH=%s" (expand-file-name rust-src-path))
                                        (format "CARGO_HOME=%s" (expand-file-name cargo-home)))
                                       process-environment)))
      (-let [(exit-code stdout _stderr)
             (racer--shell-command racer-cmd (cons command args))]
        ;; Use `equal' instead of `zero' as exit-code can be a string
        ;; "Aborted" if racer crashes.
        (unless (equal 0 exit-code)
          (user-error "%s exited with %s. `M-x racer-debug' for more info"
                      racer-cmd exit-code))
        stdout))))

(defmacro racer--with-temporary-file (path-sym &rest body)
  "Create a temporary file, and bind its path to PATH-SYM.
Evaluate BODY, then delete the temporary file."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,path-sym (make-temp-file "racer")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,path-sym))))

(defun racer--slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun racer--shell-command (program args)
  "Execute PROGRAM with ARGS.
Return a list (exit-code stdout stderr)."
  (racer--with-temporary-file tmp-file-for-stderr
    (let (exit-code stdout stderr)
      ;; Create a temporary buffer for `call-process` to write stdout
      ;; into.
      (with-temp-buffer
        (setq exit-code
              (apply #'call-process program nil
                     (list (current-buffer) tmp-file-for-stderr)
                     nil args))
        (setq stdout (buffer-string)))
      (setq stderr (racer--slurp tmp-file-for-stderr))
      (setq racer--prev-state
            (list
             :program program
             :args args
             :exit-code exit-code
             :stdout stdout
             :stderr stderr
             :default-directory default-directory
             :process-environment process-environment))
      (list exit-code stdout stderr))))

(defun racer--call-at-point (command)
  "Call racer command COMMAND at point of current buffer.
Return a list of all the lines returned by the command."
  (racer--with-temporary-file tmp-file
    (write-region nil nil tmp-file nil 'silent)
    (s-lines
     (s-trim-right
      (racer--call command
                   (number-to-string (line-number-at-pos))
                   (number-to-string (racer--current-column))
                   (buffer-file-name (buffer-base-buffer))
                   tmp-file)))))

(defun racer--read-rust-string (string)
  "Convert STRING, a rust string literal, to an elisp string."
  (when string
    (->> string
         ;; Remove outer double quotes.
         (s-chop-prefix "\"")
         (s-chop-suffix "\"")
         ;; Replace escaped characters.
         (s-replace "\\n" "\n")
         (s-replace "\\\"" "\"")
         (s-replace "\\'" "'")
         (s-replace "\\;" ";"))))

(defun racer--split-parts (raw-output)
  "Given RAW-OUTPUT from racer, split on semicolons and doublequotes.
Unescape strings as necessary."
  (let ((parts nil)
        (current "")
        (i 0))
    (while (< i (length raw-output))
      (let ((char (elt raw-output i))
            (prev-char (and (> i 0) (elt raw-output (1- i)))))
        (cond
         ;; A semicolon that wasn't escaped, start a new part.
         ((and (equal char ?\;) (not (equal prev-char ?\\)))
          (push current parts)
          (setq current ""))
         (t
          (setq current (concat current (string char))))))
      (setq i (1+ i)))
    (push current parts)
    (mapcar #'racer--read-rust-string (nreverse parts))))

(defun racer--split-snippet-match (line)
  "Given LINE, a string \"MATCH ...\" from complete-with-snippet,
split it into its constituent parts."
  (let* ((match-parts (racer--split-parts line))
         (docstring (nth 7 match-parts)))
    (when (and match-parts (equal (length match-parts) 8))
      (list :name (s-chop-prefix "MATCH " (nth 0 match-parts))
            :line (string-to-number (nth 2 match-parts))
            :column (string-to-number (nth 3 match-parts))
            :path (nth 4 match-parts)
            ;; Struct or Function:
            :kind (nth 5 match-parts)
            :signature (nth 6 match-parts)
            :docstring (if (> (length docstring) 0) docstring nil)))))

(defun racer--describe-at-point (name)
  "Get a description of the symbol at point matching NAME.
If there are multiple possibilities with this NAME, prompt
the user to choose."
  (let* ((output-lines (save-excursion
                         ;; Move to the end of the current symbol, to
                         ;; increase racer accuracy.
                         (skip-syntax-forward "w_")
                         (racer--call-at-point "complete-with-snippet")))
         (all-matches (--map (when (s-starts-with-p "MATCH " it)
                               (racer--split-snippet-match it))
                             output-lines))
         (relevant-matches (--filter (equal (plist-get it :name) name)
                                     all-matches)))
    (if (> (length relevant-matches) 1)
        ;; We might have multiple matches with the same name but
        ;; different types. E.g. Vec::from.
        (let ((signature
               (completing-read "Multiple matches: "
                                (--map (plist-get it :signature) relevant-matches))))
          (--first (equal (plist-get it :signature) signature) relevant-matches))
      (-first-item relevant-matches))))

(defun racer--help-buf (contents)
  "Create a *Racer Help* buffer with CONTENTS."
  (let ((buf (get-buffer-create "*Racer Help*"))
        ;; If the buffer already existed, we need to be able to
        ;; override `buffer-read-only'.
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert contents)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (racer-help-mode))
    buf))

(defface racer-help-heading-face
  '((t :weight bold))
  "Face for markdown headings in *Racer Help* buffers.")

(defun racer--url-p (target)
  "Return t if TARGET looks like a fully qualified URL."
  (not (null
        (string-match-p (rx bol "http" (? "s") "://") target))))

(defun racer--propertize-links (markdown)
  "Propertize links in MARKDOWN."
  (replace-regexp-in-string
   ;; Text of the form [foo](http://example.com)
   (rx "[" (group (+? (not (any "]")))) "](" (group (+? anything)) ")")
   ;; For every match:
   (lambda (whole-match)
     ;; Extract link and target.
     (let ((link-text (match-string 1 whole-match))
           (link-target (match-string 2 whole-match)))
       ;; If it's a web URL, use a clickable link.
       (if (racer--url-p link-target)
           (racer--url-button link-text link-target)
         ;; Otherwise, just discard the target.
         link-text)))
   markdown))

(defun racer--propertize-all-inline-code (markdown)
  "Given a single line MARKDOWN, replace all instances of `foo` or
\[`foo`\] with a propertized string."
  (let ((highlight-group
         (lambda (whole-match)
           (racer--syntax-highlight (match-string 1 whole-match)))))
    (->> markdown
         (replace-regexp-in-string
          (rx "[`" (group (+? anything)) "`]")
          highlight-group)
         (replace-regexp-in-string
          (rx "`" (group (+? anything)) "`")
          highlight-group))))

(defun racer--indent-block (str)
  "Indent every line in STR."
  (s-join "\n" (--map (concat "    " it) (s-lines str))))

(defun racer--trim-newlines (str)
  "Remove newlines from the start and end of STR."
  (->> str
       (s-chop-prefix "\n")
       (s-chop-suffix "\n")))

(defun racer--remove-footnote-links (str)
  "Remove footnote links from markdown STR."
  (->> (s-lines str)
       (--remove (string-match-p (rx bol "[`" (+? anything) "`]: ") it))
       (s-join "\n")
       ;; Collapse consecutive blank lines caused by removing footnotes.
       (s-replace "\n\n\n" "\n\n")))

(defun racer--docstring-sections (docstring)
  "Split DOCSTRING into text, code and heading sections."
  (let* ((sections nil)
         (current-section-lines nil)
         (section-type :text)
         ;; Helper function.
         (finish-current-section
          (lambda ()
            (when current-section-lines
              (let ((current-section
                     (s-join "\n" (nreverse current-section-lines))))
                (unless (s-blank? current-section)
                  (push (list section-type current-section) sections))
                (setq current-section-lines nil))))))
    (dolist (line (s-lines docstring))
      (cond
       ;; If this is a closing ```
       ((and (s-starts-with-p "```" line) (eq section-type :code))
        (push line current-section-lines)
        (funcall finish-current-section)
        (setq section-type :text))
       ;; If this is an opening ```
       ((s-starts-with-p "```" line)
        (funcall finish-current-section)
        (push line current-section-lines)
        (setq section-type :code))
       ;; Headings
       ((and (not (eq section-type :code)) (s-starts-with-p "# " line))
        (funcall finish-current-section)
        (push (list :heading line) sections))
       ;; Normal text.
       (t
        (push line current-section-lines))))
    (funcall finish-current-section)
    (nreverse sections)))

(defun racer--clean-code-section (section)
  "Given a SECTION, a markdown code block, remove
fenced code delimiters and code annotations."
  (->> (s-lines section)
       (-drop 1)
       (-drop-last 1)
       ;; Ignore annotations like # #[allow(dead_code)]
       (--remove (s-starts-with-p "# " it))
       (s-join "\n")))

(defun racer--propertize-docstring (docstring)
  "Replace markdown syntax in DOCSTRING with text properties."
  (let* ((sections (racer--docstring-sections docstring))
         (propertized-sections
          (--map (-let [(section-type section) it]
                   ;; Remove trailing newlines, so we can ensure we
                   ;; have consistent blank lines between sections.
                   (racer--trim-newlines
                    (pcase section-type
                      (:text
                       (racer--propertize-all-inline-code
                        (racer--propertize-links
                         (racer--remove-footnote-links
                          section))))
                      (:code
                       (racer--indent-block
                        (racer--syntax-highlight
                         (racer--clean-code-section section))))
                      (:heading
                       (racer--header
                        (s-chop-prefix "# " section))))))
                 sections)))
    (s-join "\n\n" propertized-sections)))

(defun racer--find-file (path line column)
  "Open PATH and move point to LINE and COLUMN."
  (find-file path)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))

(defun racer--button-go-to-src (button)
  (racer--find-file
   (button-get button 'path)
   (button-get button 'line)
   (button-get button 'column)))

(define-button-type 'racer-src-button
  'action 'racer--button-go-to-src
  'follow-link t
  'help-echo "Go to definition")

(defun racer--url-button (text url)
  "Return a button that opens a browser at URL."
  (with-temp-buffer
    (insert-text-button
     text
     :type 'help-url
     'help-args (list url))
    (buffer-string)))

(defun racer--src-button (path line column)
  "Return a button that navigates to PATH at LINE number and
COLUMN number."
  ;; Convert "/foo/bar/baz/foo.rs" to "baz/foo.rs"
  (let* ((filename (f-filename path))
         (parent-dir (f-filename (f-parent path)))
         (short-path (f-join parent-dir filename)))
    (with-temp-buffer
      (insert-text-button
       short-path
       :type 'racer-src-button
       'path path
       'line line
       'column column)
      (buffer-string))))

(defun racer--kind-description (raw-kind)
  "Human friendly description of a rust kind.
For example, 'EnumKind' -> 'an enum kind'."
  (let* ((parts (s-split-words raw-kind))
         (description (s-join " " (--map (downcase it) parts)))
         (a (if (string-match-p (rx bos (or "a" "e" "i" "o" "u")) description)
                "an" "a")))
    (format "%s %s" a description)))

(defun racer--describe (name)
  "Return a *Racer Help* buffer for the function or type at point.
If there are multiple candidates at point, use NAME to find the
correct value."
  (let ((description (racer--describe-at-point name)))
    (when description
      (let* ((name (plist-get description :name))
             (raw-docstring (plist-get description :docstring))
             (docstring (if raw-docstring
                            (racer--propertize-docstring raw-docstring)
                          "Not documented."))
             (kind (plist-get description :kind)))
        (racer--help-buf
         (format
          "%s is %s defined in %s.\n\n%s%s"
          name
          (racer--kind-description kind)
          (racer--src-button
           (plist-get description :path)
           (plist-get description :line)
           (plist-get description :column))
          (if (equal kind "Module")
              ;; No point showing the 'signature' of modules, which is
              ;; just their full path.
              ""
            (format "    %s\n\n" (racer--syntax-highlight (plist-get description :signature))))
          docstring))))))

(defun racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point."
  (interactive)
  (let ((buf (racer--describe (thing-at-point 'symbol))))
    (if buf
        (temp-buffer-window-show buf)
      (user-error "No function or type found at point"))))

(defvar racer-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map)
  "Keymap for racer help mode.")

(define-derived-mode racer-help-mode fundamental-mode
  "Racer-Help"
  "Major mode for *Racer Help* buffers.

Commands:
\\{racer-help-mode-map}")

(defcustom racer-complete-in-comments
  nil
  "If non-nil, query racer for completions inside comments too."
  :type 'boolean
  :group 'racer)

(defun racer-complete-at-point ()
  "Complete the symbol at point."
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (when (and
           (not in-string)
           (or (not in-comment) racer-complete-in-comments))
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (or (car bounds) (point)))
             (end (or (cdr bounds) (point))))
        (list beg end
              (completion-table-dynamic #'racer-complete)
              :annotation-function #'racer-complete--annotation
              :company-prefix-length (racer-complete--prefix-p beg end)
              :company-docsig #'racer-complete--docsig
              :company-doc-buffer #'racer--describe
              :company-location #'racer-complete--location)))))

(defun racer--file-and-parent (path)
  "Convert /foo/bar/baz/q.txt to baz/q.txt."
  (let ((file (f-filename path))
        (parent (f-filename (f-parent path))))
    (f-join parent file)))

(defun racer-complete (&optional _ignore)
  "Completion candidates at point."
  (->> (racer--call-at-point "complete")
       (--filter (s-starts-with? "MATCH" it))
       (--map (-let [(name line col file matchtype ctx)
                     (s-split-up-to "," (s-chop-prefix "MATCH " it) 5)]
                (put-text-property 0 1 'line (string-to-number line) name)
                (put-text-property 0 1 'col (string-to-number col) name)
                (put-text-property 0 1 'file file name)
                (put-text-property 0 1 'matchtype matchtype name)
                (put-text-property 0 1 'ctx ctx name)
                name))))

(defun racer--trim-up-to (needle s)
  "Return content after the occurrence of NEEDLE in S."
  (-if-let (idx (s-index-of needle s))
      (substring s (+ idx (length needle)))
    s))

(defun racer-complete--prefix-p (beg _end)
  "Return t if a completion should be triggered for a prefix between BEG and END."
  (save-excursion
    (goto-char beg)
    ;; If we're at the beginning of the buffer, we can't look back 2
    ;; characters.
    (ignore-errors
      (looking-back "\\.\\|::" 2))))

(defun racer-complete--annotation (arg)
  "Return an annotation for completion candidate ARG."
  (let* ((ctx (get-text-property 0 'ctx arg))
         (type (get-text-property 0 'matchtype arg))
         (pretty-ctx
          (pcase type
            ("Module"
             (if (string= arg ctx)
                 ""
               (concat " " (racer--file-and-parent ctx))))
	    ("StructField"
	     (concat " " ctx))
            (_
             (->> ctx
                  (racer--trim-up-to arg)
                  (s-chop-suffixes '(" {" "," ";")))))))
    (format "%s : %s" pretty-ctx type)))

(defun racer-complete--docsig (arg)
  "Return a signature for completion candidate ARG."
  (racer--syntax-highlight (format "%s" (get-text-property 0 'ctx arg))))

(defun racer-complete--location (arg)
  "Return location of completion candidate ARG."
  (cons (get-text-property 0 'file arg)
        (get-text-property 0 'line arg)))

(defun racer--current-column ()
  "Get the current column based on underlying character representation."
  (length (buffer-substring-no-properties
           (line-beginning-position) (point))))

;;;###autoload
(defun racer-find-definition ()
  "Run the racer find-definition command and process the results."
  (interactive)
  (-if-let (match (--first (s-starts-with? "MATCH" it)
                           (racer--call-at-point "find-definition")))
      (-let [(_name line col file _matchtype _ctx)
             (s-split-up-to "," (s-chop-prefix "MATCH " match) 5)]
        (if (fboundp 'xref-push-marker-stack)
            (xref-push-marker-stack)
          (with-no-warnings
            (ring-insert find-tag-marker-ring (point-marker))))
        (racer--find-file file (string-to-number line) (string-to-number col)))
    (error "No definition found")))

(defun racer--syntax-highlight (str)
  "Apply font-lock properties to a string STR of Rust code."
  (let (result)
    ;; Load all of STR in a rust-mode buffer, and use its
    ;; highlighting.
    (with-temp-buffer
      (insert str)
      (delay-mode-hooks (rust-mode))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))
      (setq result (buffer-string)))
    (when (and
           ;; If we haven't applied any properties yet,
           (null (text-properties-at 0 result))
           ;; and if it's a standalone symbol, then assume it's a
           ;; variable.
           (string-match-p (rx bos (+ (any lower "_")) eos) str))
      (setq result (propertize str 'face 'font-lock-variable-name-face)))
    result))

(defun racer--goto-func-name ()
  "If point is inside a function call, move to the function name.

foo(bar, |baz); -> foo|(bar, baz);"
  (let ((last-paren-pos (nth 1 (syntax-ppss)))
        (start-pos (point)))
    (when last-paren-pos
      ;; Move to just before the last paren.
      (goto-char last-paren-pos)
      ;; If we're inside a round paren, we're inside a function call.
      (unless (looking-at "(")
        ;; Otherwise, return to our start position, as point may have been on a
        ;; function already:
        ;; foo|(bar, baz);
        (goto-char start-pos)))))

(defun racer--relative (path &optional directory)
  "Return PATH relative to DIRECTORY (`default-directory' by default).
If PATH is not in DIRECTORY, just abbreviate it."
  (unless directory
    (setq directory default-directory))
  (if (s-starts-with? directory path)
      (concat "./" (f-relative path directory))
    (f-abbrev path)))

(defun racer-eldoc ()
  "Show eldoc for context at point."
  (save-excursion
    (racer--goto-func-name)
    ;; If there's a variable at point:
    (-when-let* ((rust-sym (symbol-at-point))
                 (comp-possibilities (racer-complete))
                 (matching-possibility
                  (--find (string= it (symbol-name rust-sym)) comp-possibilities))
                 (prototype (get-text-property 0 'ctx matching-possibility))
                 (matchtype (get-text-property 0 'matchtype matching-possibility)))
      (if (equal matchtype "Module")
          (racer--relative prototype)
        ;; Syntax highlight function signatures.
        (racer--syntax-highlight prototype)))))

(defvar racer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'racer-find-definition)
    (define-key map (kbd "M-,") #'pop-tag-mark)
    map))

;;;###autoload
(define-minor-mode racer-mode
  "Minor mode for racer."
  :lighter " racer"
  :keymap racer-mode-map
  (setq-local eldoc-documentation-function #'racer-eldoc)
  (set (make-local-variable 'completion-at-point-functions) nil)
  (add-hook 'completion-at-point-functions #'racer-complete-at-point))

(define-obsolete-function-alias 'racer-turn-on-eldoc 'eldoc-mode)
(define-obsolete-function-alias 'racer-activate 'racer-mode)

(provide 'racer)
;;; racer.el ends here
