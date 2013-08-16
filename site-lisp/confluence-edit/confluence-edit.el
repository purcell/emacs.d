;;; confluence-edit.el --- Emacs mode for editing confluence content buffers

;; Copyright (C) 2008-2011 Kyle Burton, James Ahlborn

;; Author: James Ahlborn <james@boomi.com>
;; Keywords: confluence, wiki
;; Version: 1.7-beta
;; Package-Requires: 
;; EmacsWiki: ConfluenceMode

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Generally, confluence-edit-mode will not be used directly.  However, there
;; may be times when a buffer is being edited using confluence wiki syntax but
;; without interacting directly with a live confluence instance.  In this
;; case, it may be desirable to use this mode directly.
;;
;; INSTALLATION
;;
;; This mode can be used standalone (without confuence.el and its
;; dependencies) by simply adding confluence-edit.el to your load path.  In
;; general, you can follow the instructions in confluence.el for configuring
;; this mode and setting up longlines mode, just substitute
;; "confluence-edit-mode" everywhere you see "confluence-mode".
;; 

;;; Code:

(require 'font-lock)

;;
;; Various utility code
;;

;; these are never set directly, only defined here to make the compiler happy
(defvar confluence-completing-read nil)
(defvar cfln-read-current-completions nil)
(defvar cfln-read-current-other-completions nil)
(defvar cfln-read-last-comp-str nil)
(defvar cfln-read-completion-buffer nil)

(defmacro with-quiet-rpc (&rest body)
  "Execute the forms in BODY with `url-show-status' set to nil."
  `(let ((url-show-status nil))
     ,@body))
  
(defun cfln-get-page-anchors ()
  "Gets the anchors in the current page."
  (let ((anchors nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "{anchor:\\([^{}\n]+\\)}" nil t)
        (push (cons (match-string 1) t) anchors))
      ;; headings are also implicit anchors
      (goto-char (point-min))
      (while (re-search-forward "^h[1-9][.]\\s-+\\(.+?\\)\\s-*$" nil t)
        (push (cons (match-string 1) t) anchors)))
    anchors))

(defun cfln-read-string (prompt-prefix prompt hist-alist-var hist-key 
                       comp-func-or-table &optional
                       require-match init-val def-val)
  "Prompt for a string using the given prompt info and history alist."
  ;; we actually use the history var as an alist of history vars so we can
  ;; have different histories in different contexts (e.g. separate space
  ;; histories for each url and separate page histories for each space)
  (let ((hist-list (cfln-get-struct-value (symbol-value hist-alist-var) 
                                        hist-key))
        (result-string nil))
    (setq result-string
          (cfln-read-string-simple (concat (or prompt-prefix "") prompt)
                                 'hist-list comp-func-or-table
                                 require-match init-val def-val))
    ;; put the new history list back into the alist
    (cfln-set-struct-value hist-alist-var hist-key hist-list)
    result-string))

(defun cfln-read-string-simple (prompt hist-list-var comp-func-or-table
                              &optional require-match init-val def-val)
  "Prompt for a string using the given prompt info and history list."
  (let ((cfln-read-current-completions nil)
        (cfln-read-current-other-completions nil)
        (cfln-read-last-comp-str nil)
        (cfln-read-completion-buffer 
         (or (and (boundp 'cfln-read-completion-buffer)
                  cfln-read-completion-buffer)
             (current-buffer)))
        (confluence-completing-read t))
    (with-quiet-rpc
     ;; prefer ido-completing-read if available
     (if (and (fboundp 'ido-completing-read)
              (listp comp-func-or-table))
         (ido-completing-read prompt (mapcar 'car comp-func-or-table) nil require-match init-val hist-list-var def-val)
       (completing-read prompt comp-func-or-table
                        nil require-match init-val hist-list-var def-val t)))))

(defun cfln-read-char (prompt allowed-chars-regex &optional def-char)
  "Prompt for a character using the given PROMPT and ALLOWED-CHARS-REGEX.
If DEF-CHAR is given it will be returned if user hits the <enter> key."
  (let ((the-char nil))
    (while (not the-char)
      (setq the-char (char-to-string (read-char-exclusive prompt)))
      (if (not (string-match allowed-chars-regex the-char))
          (if (and def-char (string-equal (char-to-string ?\r) the-char))
              (setq the-char def-char)
            (setq the-char nil))))
    the-char))
  
(defun cfln-complete (comp-str pred comp-flag comp-table)
  "Executes completion for the given args and COMP-TABLE."
  (cond
   ((not comp-flag)
    (or (try-completion comp-str comp-table pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str comp-table pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str comp-table) t))))


(defun cfln-result-to-completion-list (result-list key)
  "Translates the rpc result list into a list suitable for completion."
  (mapcar
   (lambda (el)
      (cons (cfln-get-struct-value el key) t))
   result-list))

(defun cfln-get-struct-value (struct key &optional default-value)
  "Gets a STRUCT value for the given KEY from the given struct, returning the
given DEFAULT-VALUE if not found."
  (or (and struct
           (cdr (assoc key struct)))
      default-value))

(defun cfln-set-struct-value-copy (struct key value)
  "Copies the given STRUCT, sets the given KEY to the given VALUE and returns
the new STRUCT."
  (let ((temp-struct (copy-alist struct)))
    (cfln-set-struct-value 'temp-struct key value)
    temp-struct))

(defun cfln-set-struct-value (struct-var key value)
  "Sets (or adds) the given KEY to the given VALUE in the struct named by the
given STRUCT-VAR."
  (let ((cur-assoc (assoc key (symbol-value struct-var))))
    (if cur-assoc
        (setcdr cur-assoc value)
      (add-to-list struct-var (cons key value) t))))

(defun cfln-string-notempty (str)
  "Returns t if the given string is not empty."
  (> (length str) 0))

(defun cfln-string-empty (str)
  "Returns t if the given string is empty."
  (= (length str) 0))



;;
;; Edit mode specific code
;;

(defgroup confluence-faces nil
  "Faces used when editing confluence wiki pages."
  :group 'faces)

(defvar confluence-get-attachment-names-function nil)

(defvar confluence-code-face 'confluence-code-face)

(defface confluence-code-face
  '((((class color) (background dark))
     (:foreground "dim gray" :bold t))
    (((class color) (background light))
     (:foreground "dim gray"))
    (t (:bold t)))
  "Font Lock Mode face used for code in confluence pages."
  :group 'confluence-faces)

(defvar confluence-panel-face 'confluence-panel-face)

(defface confluence-panel-face
  '((((class color) (background dark))
     (:background "LightGray"))
    (((class color) (background light))
     (:background "LightGray"))
    (t nil))
  "Font Lock Mode face used for panel in confluence pages."
  :group 'confluence-faces)

(defvar confluence-embedded-link-face '(font-lock-constant-face underline))

(defconst confluence-font-lock-keywords-1
  (list
  
   '("{\\([^{}:\n]+:?\\)[^{}\n]*}"
     (1 'font-lock-constant-face))
  
   '("{[^{}\n]+[:|]title=\\([^}|\n]+\\)[^{}\n]*}"
     (1 'bold append))
  
   '("{warning\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){warning}"
     (1 'font-lock-warning-face prepend))
   '("{note\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){note}"
     (1 'font-lock-minor-warning-face prepend))
   '("{info\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){info}"
     (1 'font-lock-doc-face prepend))
   '("{tip\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){tip}"
     (1 'font-lock-comment-face prepend))
  
   ;; bold
   '("[^[:word:]\\*#\n][*]\\([^*\n]+\\)[*]\\W"
     (1 'bold))
   
   ;; code
   '("{{\\([^}\n]+\\)}}"
     (1 'confluence-code-face t))
   
   ;; italics/emphasised
   '("[^[:word:]\\]_\\([^_\n]+\\)_\\W"
     (1 'italic prepend))
   '("[^[:word:]\\][?]\\{2\\}\\([^?\n]+\\)[?]\\{2\\}\\W"
     (1 'italic prepend))

   ;; underline
   '("[^[:word:]\\][+]\\([^+\n]+\\)[+]\\W"
     (1 'underline prepend))

   ;; strike-through
   '("[^[:word:]\\][-]\\([^-\n]+\\)[-]\\W"
     (1 '(:strike-through t) prepend))

   ;; headings
   '("^h1[.] \\(.*?\\)\\s-*$"
     (1 '(bold underline) prepend))
   '("^h2[.] \\(.*?\\)\\s-*$"
     (1 '(bold italic underline) prepend))
   '("^h3[.] \\(.*?\\)\\s-*$"
     (1 '(italic underline) prepend))
   '("^h[4-9][.] \\(.*?\\)\\s-*$"
     (1 'underline prepend))

   ;; bullet points
   '("^\\([*#]+\\)\\s-"
     (1 'font-lock-constant-face))
   
   ;; links
   '("\\(\\[\\)\\([^]|\n]*\\)[|]\\([^]\n]+\\)\\(\\]\\)"
     (1 'font-lock-constant-face)
     (2 'font-lock-string-face)
     (3 'underline)
     (4 'font-lock-constant-face))
   '("\\(\\[\\)\\([^]|\n]+\\)\\(\\]\\)"
     (1 'font-lock-constant-face)
     (2 '(font-lock-string-face underline))
     (3 'font-lock-constant-face))
   '("{anchor:\\([^{}\n]+\\)}"
     (1 'font-lock-string-face))

   ;; images, embedded content
   '("\\([!]\\)\\([^|\n]+\\)[|]\\(?:[^!\n]*\\)\\([!]\\)"
     (1 'font-lock-constant-face)
     (2 confluence-embedded-link-face)
     (3 'font-lock-constant-face))
   '("\\([!]\\)\\([^!|\n]+\\)\\([!]\\)"
     (1 'font-lock-constant-face)
     (2 confluence-embedded-link-face)
     (3 'font-lock-constant-face))
   
   ;; tables
   '("[|]\\{2\\}\\([^|\n]+\\)"
     (1 'bold))
   '("\\([|]\\{1,2\\}\\)"
     (1 'font-lock-constant-face))
   )
  
  "Basic level highlighting for confluence mode.")

(defconst confluence-font-lock-keywords-2
  (append confluence-font-lock-keywords-1
          (list
  
           ;; code/preformatted blocks
           '("{noformat\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){noformat}"
             (1 'confluence-code-face t))
           '("{code\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){code}"
             (1 'confluence-code-face t))

           ;; panels
           '("{panel\\(?:[:][^}\n]*\\)?}\\(?:\\s-*[\r]?[\n]\\)?\\(\\(.\\|[\n]\\)*?\\){panel}"
             (1 'confluence-panel-face append))
           ))
  "Gaudy level highlighting for confluence mode.")

(defvar confluence-font-lock-keywords confluence-font-lock-keywords-1
  "Default expressions to highlight in Confluence modes.")


(defun confluence-newline-and-indent ()
  "Inserts a newline and indents using the previous indentation.
Supports lists, tables, and headers."
  (interactive)
  (let ((indentation nil)
        (limit nil))
    ;; find the beginning of the previous line, skipping "soft" newlines if
    ;; "hard" newlines are being used (like in longlines mode)
    (save-excursion
      (while (and (search-backward "\n" nil 'silent)
                  use-hard-newlines
                  (not (get-text-property (match-beginning 0) 'hard))))
      (setq limit (point)))
    ;; find the indentation of the previous line
    (save-excursion
      (if (re-search-backward "^\\(?:\\(?:\\(?:[*#]+\\|h[0-9][.]\\)[ \t]+\\)\\|[|]+\\)" limit t)
          (setq indentation (match-string 0))))
    (newline)
    (if indentation
        (insert indentation))))

(defun confluence-list-indent-dwim (&optional arg)
  "Increases the list indentationn on the current line by 1 bullet.  With ARG decreases by 1 bullet."
  (interactive "P")
  (let ((indent-arg (if arg -1 1)))
    (if (and mark-active transient-mark-mode)
        (let ((beg (min (point) (mark)))
              (end (max (point) (mark)))
              (tmp-point nil))
          (save-excursion
            (goto-char end)
            (if (bolp)
                (forward-line -1))
            (setq tmp-point (line-beginning-position))
            (confluence-modify-list-indent indent-arg)
            (while (and (forward-line -1)
                        (not (equal (line-beginning-position) tmp-point))
                        (>= (line-end-position) beg))
              (setq tmp-point (line-beginning-position))
              (confluence-modify-list-indent indent-arg))
          ))
    (confluence-modify-list-indent indent-arg))))

(defun confluence-modify-list-indent (depth)
  "Updates the list indentation on the current line, adding DEPTH bullets if DEPTH is positive or removing DEPTH
bullets if DEPTH is negative (does nothing if DEPTH is 0)."
  (interactive "nList Depth Change: ")
  (save-excursion
    (beginning-of-line)
    (cond
     ((> depth 0)
      (let ((indent-str (concat (make-string depth ?*) " ")))
        (if (re-search-forward "\\=\\([*#]+\\)" (line-end-position) t)
            (setq indent-str (make-string depth (elt (substring (match-string 1) -1) 0))))
        (insert-before-markers indent-str)))
     ((< depth 0)
      (let ((tmp-point (point))
            (indent-str ""))
        (if (re-search-forward "\\=\\([*#]+\\)" (line-end-position) t)
            (progn 
              (setq indent-str (match-string 1))
              (setq indent-str
                    (if (< (abs depth) (length indent-str))
                        (substring indent-str 0 depth)
                      ""))))
        (delete-region tmp-point (point))
        (insert-before-markers indent-str))))))

(defsubst cfln-region-is-active ()
  "Return t when the region is active."
  ;; The determination of region activeness is different in both Emacs and
  ;; XEmacs.
  (cond
   ;; Emacs
   ((boundp 'mark-active) mark-active)
   ;; XEmacs
   ((and (fboundp 'region-active-p)
         (boundp 'zmacs-regions)
         zmacs-regions)
    (region-active-p))
   ;; fallback; shouldn't get here
   (t (mark t))))

(defsubst cfln-hard-newline ()
  "Return newline string, including hard property if hard newlines are being
used."
  (if use-hard-newlines
      (propertize "\n" 'hard 't)
    "\n"))

(defun cfln-format-block-tag (tag-text tag-point)
  "Formats a block tag with appropriate newlines based on the insertion
point."
  (concat
   (if (equal (char-before tag-point) ?\n)
       ""
     (cfln-hard-newline))
   tag-text
   (if (equal (char-after tag-point) ?\n)
       ""
     (cfln-hard-newline))))

(defun cfln-wrap-text (pre-wrap-str &optional post-wrap-str are-block-tags)
  "Wraps the current region (if active) or current word with PRE-WRAP-STR and
POST-WRAP-STR.  If POST-WRAP-STR is nil, PRE-WRAP-STR is reused.  If
ARE-BLOCK-TAGS is not nil, the wrap strings will be formatted using
`cfln-format-block-tag' before insertion."
  (save-excursion
    (let ((beg nil)
          (end nil)
          (wrap-str nil)
          (end-marker (make-marker)))
      (if (cfln-region-is-active)
          (progn
            (setq beg (region-beginning))
            (setq end (region-end))
            (deactivate-mark))
        (progn
          (backward-word 1)
          (setq beg (point))
          (forward-word 1)
          (setq end (point))))
      (if are-block-tags
          (setq pre-wrap-str (cfln-format-block-tag pre-wrap-str beg)
                post-wrap-str (cfln-format-block-tag (or post-wrap-str 
                                                       pre-wrap-str) end)))
      (set-marker end-marker end)
      (goto-char beg)
      (insert-before-markers pre-wrap-str)
      (goto-char end-marker)
      (insert-before-markers (or post-wrap-str pre-wrap-str))
      (set-marker end-marker nil))))


(defun confluence-boldify-text ()
  "Wraps the current region/word with *bold* marks."
  (interactive)
  (cfln-wrap-text "*"))

(defun confluence-italicize-text ()
  "Wraps the current region/word with _italics_ marks."
  (interactive)
  (cfln-wrap-text "_"))

(defun confluence-strike-text ()
  "Wraps the current region/word with -strikethrough- marks."
  (interactive)
  (cfln-wrap-text "-"))

(defun confluence-underline-text ()
  "Wraps the current region/word with +underline+ marks."
  (interactive)
  (cfln-wrap-text "+"))

(defun confluence-superscript-text ()
  "Wraps the current region/word with ^superscript^ marks."
  (interactive)
  (cfln-wrap-text "^"))

(defun confluence-subscript-text ()
  "Wraps the current region/word with ~subscript~ marks."
  (interactive)
  (cfln-wrap-text "~"))

(defun confluence-cite-text ()
  "Wraps the current region/word with ??citation?? marks."
  (interactive)
  (cfln-wrap-text "??"))

(defun confluence-linkify-text (&optional link-url)
  "Wraps the current region/word as a [link]."
  (interactive "MURL: ")
  (cfln-wrap-text "[" (concat (if (cfln-string-notempty link-url)
                                (concat "|" link-url)
                              "") "]")))

(defun confluence-codify-text (&optional arg)
  "Wraps the current region/word as {{monospace}} if single-line, otherwise
as a {code}code block{code}."
  (interactive "P")
  (let ((pre-str "{{")
        (post-str "}}")
        (are-block-tags nil))
    (if (or arg
            (and (cfln-region-is-active)
                 (save-excursion
                   (let ((beg (region-beginning))
                         (end (region-end))
                         (found-newline nil))
                     (goto-char beg)
                     ;; search for a non-soft newline in the current region
                     (while (and (search-forward "\n" end 'silent)
                                 (setq found-newline t)
                                 use-hard-newlines
                                 (not (get-text-property (match-beginning 0)
                                                         'hard))
                                 (setq found-newline 'soft)))
                     (eq found-newline t)))))
        (setq pre-str "{code:}"
              post-str "{code}"
              are-block-tags t))
    (cfln-wrap-text pre-str post-str are-block-tags)))

(defun confluence-linkify-anchor-text (&optional anchor-name)
  "Wraps the current region/word as an anchor [link|#ANCHOR-NAME]."
  (interactive)
  (if (not anchor-name)
      (let ((cur-anchors (cfln-get-page-anchors)))
        (setq anchor-name (cfln-read-string-simple "Confluence Anchor Name: " 
                                                 nil cur-anchors))))
  (cfln-wrap-text "[" (concat "|#" (or anchor-name "") "]")))

(defun confluence-linkify-attachment-text (&optional file-name)
  "Wraps the current region/word as an attachment [link|#FILE-NAME]."
  (interactive)
  (if (not file-name)
      (let ((cur-attachments 
             (if confluence-get-attachment-names-function
                 (funcall confluence-get-attachment-names-function)
               nil)))
        (setq file-name (cfln-read-string-simple "Confluence attachment file name: " 
                                                 'confluence-attachment-history cur-attachments))))
  (cfln-wrap-text "[" (concat "|^" (or file-name "") "]")))

(defun confluence-embed-text ()
  "Wraps the current region/word as an embedded content !link!."
  (interactive)
  (cfln-wrap-text "!"))

(defun confluence-insert-anchor (anchor-name)
  "Inserts an {anchor}."
  (interactive "MNew AnchorName: ")
  (insert "{anchor:" anchor-name "}"))

(defun confluence-insert-horizontal-rule ()
  "Inserts horizontal rule."
  (interactive)
  (insert (cfln-format-block-tag 
           (concat (cfln-hard-newline) "----" (cfln-hard-newline)) 
           (point))))

(defvar confluence-max-block-search 200
  "Maximum amount of characters back to search when highlighting blocks.")

(defun confluence-backward-paragraph-or-block ()
  "Moves backward one format block or paragraph (if not within or near a
format block).  note, this is kind of a guessing game because there is
 (often) no difference between a format block start and end tag."
  (interactive)
  (let* ((orig-pos (point))
         (cur-pos orig-pos)
         (search-start-pos (point-min)))
    (if (> (- cur-pos search-start-pos) confluence-max-block-search)
        (setq search-start-pos (- cur-pos confluence-max-block-search)))
    (if (re-search-backward "^{\\([^{}\n]+\\)}" search-start-pos t)
        (unless (cfln-beginning-of-block-p (match-string 1))
          (let ((first-match-pos (point))) 
            (goto-char cur-pos)
            (backward-paragraph)
            (setq cur-pos (point))
            (if (and (< cur-pos first-match-pos)
                     (> cur-pos search-start-pos)
                     (re-search-backward "^{\\([^{}\n]+\\)}"
                                         search-start-pos t))
                (unless (cfln-beginning-of-block-p (match-string 1))
                  (goto-char cur-pos))))))
    (if (= orig-pos (point))
        (backward-paragraph))))

(defun confluence-forward-paragraph-or-block ()
  "Moves forward one format block or paragraph (if not within or near a
format block).  note, this is kind of a guessing game because there is
 (often) no difference between a format block start and end tag."
  (interactive)
  (let* ((orig-pos (point))
         (cur-pos orig-pos)
         (search-end-pos (point-max)))
    (if (> (- search-end-pos cur-pos) confluence-max-block-search)
        (setq search-end-pos (+ cur-pos confluence-max-block-search)))
    (if (re-search-forward "^{\\([^{}\n]+\\)}" search-end-pos t)
        (unless (cfln-end-of-block-p (match-string 1))
          (let ((first-match-pos (point))) 
            (goto-char cur-pos)
            (forward-paragraph)
            (setq cur-pos (point))
            (if (and (> cur-pos first-match-pos)
                     (< cur-pos search-end-pos)
                     (re-search-forward "^{\\([^{}\n]+\\)}" 
                                        search-end-pos t))
                (unless (cfln-end-of-block-p (match-string 1))
                  (goto-char cur-pos))))))
    (if (= orig-pos (point))
        (forward-paragraph)
      (unless (bolp)
        (forward-line)))))

(defun cfln-beginning-of-block-p (block-str)
  "Returns non-nil if the current position and BLOCK-STR represent the
beginning of a format block, nil otherwise."
  (or (string-match ":" block-str)
      (bobp)
      (not (get-text-property (- (point) 1) 'font-lock-multiline))))

(defun cfln-end-of-block-p (block-str)
  "Returns non-nil if the current position and BLOCK-STR represent the end of
a format block, nil otherwise."
  (and (not (string-match ":" block-str))
       (or (eobp)
           (not (get-text-property (+ (point) 1) 'font-lock-multiline)))))


(defvar confluence-format-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'confluence-italicize-text)
    (define-key map "c" 'confluence-codify-text)
    (define-key map "b" 'confluence-boldify-text)
    (define-key map "l" 'confluence-linkify-text)
    (define-key map "u" 'confluence-underline-text)
    (define-key map "a" 'confluence-linkify-anchor-text)
    (define-key map "t" 'confluence-linkify-attachment-text)
    (define-key map "A" 'confluence-insert-anchor)
    (define-key map "e" 'confluence-embed-text)
    (define-key map "h" 'confluence-insert-horizontal-rule)
    (define-key map "s" 'confluence-superscript-text)
    (define-key map "S" 'confluence-subscript-text)
    (define-key map "C" 'confluence-cite-text)
    (define-key map "x" 'confluence-strike-text)
    map)
  "Keybinding prefix map which can be bound for common formatting functions in
confluence mode.")


(define-derived-mode confluence-edit-mode text-mode "ConfluenceEdit"
  "Set major mode for editing Confluence Wiki page buffers."
  (turn-off-auto-fill)
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (set-syntax-table (make-syntax-table (syntax-table)))
  (modify-syntax-entry ?\\ "\\")
  (setq font-lock-defaults
        '((confluence-font-lock-keywords confluence-font-lock-keywords-1
                                         confluence-font-lock-keywords-2)
          nil nil nil confluence-backward-paragraph-or-block
          (font-lock-multiline . t)))
)


(provide 'confluence-edit)
;;; confluence-edit.el ends here
