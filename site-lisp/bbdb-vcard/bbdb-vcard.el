;;; bbdb-vcard.el --- vCard import/export for BBDB

;; Copyright (c) 2010 Bert Burgemeister

;; Author: Bert Burgemeister <trebbu@googlemail.com>
;; Keywords: data calendar mail news
;; URL: http://github.com/trebb/bbdb-vcard
;; Version: 0.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; The exporter functionality is based on code from
;; bbdb-vcard-export.el by Jim Hourihan and Alex Schroeder.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;
;; Purpose
;; =======
;;
;; Import and export of vCards as defined in RFC 2425 and RFC 2426
;; to/from The Insidious Big Brother Database (BBDB).
;;
;;
;; Usage
;; =====
;;
;; vCard Import
;; ------------
;;
;; On a file, a buffer or a region containing one or more vCards, use
;; `bbdb-vcard-import-file', `bbdb-vcard-import-buffer', or
;; `bbdb-vcard-import-region' respectively to import them into BBDB.
;;
;; Preferred input format is vCard version 3.0.  Version 2.1 vCards
;; are converted to version 3.0 on import.
;;
;;
;; vCard Export
;; ------------
;;
;; In buffer *BBDB*, press v to export the record under point.  Press
;; * v to export all records in buffer into one vCard file.  Press *
;; C-u v to export them into one file each.
;;
;; To put one or all vCard(s) into the kill ring, press V or * V
;; respectively.
;;
;; Exported vCards are always version 3.0.  They can be re-imported
;; without data loss with one exception: North American phone numbers
;; lose their structure and are stored as flat strings.
;;
;;
;; There are a few customization variables grouped under `bbdb-vcard'.
;;
;;
;; Installation
;; ============
;;
;; Put this file and file vcard.el into your `load-path' and add the
;; following line to your Emacs initialization file:
;;
;;   (require 'bbdb-vcard)
;;
;;
;; Implementation
;; ==============
;;
;; vCard Import
;; ------------
;;
;; For conversion of v2.1 vCards into v3.0 on import, Noah Friedman's
;; vcard.el is needed.
;;
;; An existing BBDB record is extended by new information from a vCard
;;
;;   (a) if name and company and an email address match
;;   (b) or if name and company match
;;   (c) or if name and an email address match
;;   (d) or if name and birthday match
;;   (e) or if name and a phone number match.
;;
;; Otherwise, a fresh BBDB record is created.
;;
;; When `bbdb-vcard-try-merge' is set to nil, there is always a fresh
;; record created.
;;
;; In cases (c), (d), and (e), if the vCard has ORG defined, this ORG
;; would overwrite an existing Company in BBDB.
;;
;; Phone numbers are always imported as strings.
;;
;; For vCard types that have more or less direct counterparts in BBDB,
;; labels and parameters are translated and structured values
;; (lastname; firstname; additional names; prefixes etc.) are
;; converted appropriately with the risk of some (hopefully
;; unessential) information loss.  For labels of the vCard types ADR
;; and TEL, parameter translation is defined in
;; `bbdb-vcard-import-translation-table'.
;;
;; If there is a REV element, it is stored in BBDB's creation-date in
;; newly created BBDB records, or discarded for existing ones.  Time
;; and time zone information from REV are stored there as well if
;; there are any, but are ignored by BBDB (v2.36).
;;
;; VCard type prefixes (A.ADR:..., B.ADR:... etc.) are stripped off
;; and discarded from the following types: N, FN, NICKNAME, ORG (first
;; occurrence), ADR, TEL, EMAIL, URL, BDAY (first occurrence), NOTE.
;;
;; VCard types that are prefixed `X-BBDB-' are stored in BBDB without
;; the prefix.
;;
;; VCard type X-BBDB-ANNIVERSARY may contain (previously exported)
;; newline-separated non-birthday anniversaries that are meant to be
;; read by org-mode.
;;
;; All remaining vCard types that don't match the regexp in
;; `bbdb-vcard-skip-on-import' and that have a non-empty value are
;; stored unaltered in the BBDB Notes alist where, for instance,
;; `TZ;VALUE=text:-05:00' is stored as `(tz\;value=text . "-05:00")'.
;; From the BBDB data fields AKA, Phones, Addresses, Net Addresses,
;; and Notes, duplicates are removed, respectively.
;;
;; VCards found inside other vCards (as values of type AGENT) are
;; imported as well.
;;
;;
;; Handling of the individual types defined in RFC2426 during import
;; (assuming default label translation and no vCard type exclusion):
;; "
;; |----------------------+----------------------------------------|
;; | VCARD TYPE;          | STORAGE IN BBDB                        |
;; | PARAMETERS           |                                        |
;; |----------------------+----------------------------------------|
;; | VERSION              | -                                      |
;; |----------------------+----------------------------------------|
;; | N                    | First occurrence:                      |
;; |                      | Firstname                              |
;; |                      | Lastname                               |
;; |                      |                                        |
;; |                      | Rest:                                  |
;; |                      | AKAs (append)                          |
;; |----------------------+----------------------------------------|
;; | FN                   | AKAs (append)                          |
;; | NICKNAME             | AKAs (append)                          |
;; |----------------------+----------------------------------------|
;; | ORG                  | First occurrence:                      |
;; |                      | Company                                |
;; |                      |                                        |
;; |                      | Rest:                                  |
;; |                      | Notes<org                              |
;; |                      | (repeatedly)                           |
;; |----------------------+----------------------------------------|
;; | ADR;TYPE=x,HOME,y    | Addresses<Home                         |
;; | ADR;TYPE=x;TYPE=HOME | Addresses<Home                         |
;; | ADR;TYPE=x,WORK,y    | Addresses<Office                       |
;; | ADR;TYPE=x;TYPE=WORK | Addresses<Office                       |
;; | ADR;TYPE=x,y,z       | Addresses<x,y,z                        |
;; | ADR;TYPE=x;TYPE=y    | Addresses<x,y                          |
;; | ADR                  | Addresses<Office                       |
;; |----------------------+----------------------------------------|
;; | TEL;TYPE=x,HOME,y    | Phones<Home (append)                   |
;; | TEL;TYPE=x;TYPE=HOME | Phones<Home (append)                   |
;; | TEL;TYPE=x,WORK,y    | Phones<Office (append)                 |
;; | TEL;TYPE=x;TYPE=WORK | Phones<Office (append)                 |
;; | TEL;TYPE=x,CELL,y    | Phones<Mobile (append)                 |
;; | TEL;TYPE=x;TYPE=CELL | Phones<Mobile (append)                 |
;; | TEL;TYPE=x,y,z       | Phones<x,y,z (append)                  |
;; | TEL;TYPE=x;TYPE=y    | Phones<x,y (append)                    |
;; | TEL                  | Phones<Office (append)                 |
;; |----------------------+----------------------------------------|
;; | EMAIL;TYPE=x,y,z     | Net-Addresses (append)                 |
;; | URL                  | Notes<www                              |
;; |----------------------+----------------------------------------|
;; | BDAY                 | Notes<anniversary (append as birthday) |
;; | X-BBDB-ANNIVERSARY   | Notes<anniversary (append)             |
;; |----------------------+----------------------------------------|
;; | NOTE                 | Notes<notes (append)                   |
;; | REV                  | Notes<creation-date                    |
;; | CATEGORIES           | Notes<mail-alias (append)              |
;; | SORT-STRING          | Notes<sort-string                      |
;; | KEY                  | Notes<key                              |
;; | GEO                  | Notes<geo                              |
;; | TZ                   | Notes<tz                               |
;; | PHOTO                | Notes<photo                            |
;; | LABEL                | Notes<label                            |
;; | LOGO                 | Notes<logo                             |
;; | SOUND                | Notes<sound                            |
;; | TITLE                | Notes<title                            |
;; | ROLE                 | Notes<role                             |
;; | AGENT                | Notes<agent                            |
;; | MAILER               | Notes<mailer                           |
;; | UID                  | Notes<uid                              |
;; | PRODID               | Notes<prodid                           |
;; | CLASS                | Notes<class                            |
;; | X-foo                | Notes<x-foo                            |
;; | X-BBDB-bar           | Notes<bar                              |
;; |----------------------+----------------------------------------|
;; | anyJunK;a=x;b=y      | Notes<anyjunk;a=x;b=y                  |
;; |----------------------+----------------------------------------|
;; "
;;
;; vCard Export
;; ------------
;;
;; VCard types N (only fields lastname, firstname) and FN both come
;; from BBDB's Name.
;;
;; Members of BBDB field AKA are stored comma-separated under the
;; vCard type NICKNAME.
;;
;; Labels of Addresses and Phones are translated as defined in
;; `bbdb-vcard-export-translation-table' into type parameters of
;; vCard types ADR and TEL, respectively.
;;
;; In vCard type ADR, fields postbox and extended address are always
;; empty.  Newlines which subdivide BBDB Address fields are converted
;; into commas subdividing vCard ADR fields.
;;
;; The value of 'anniversary in Notes is supposed to be subdivided by
;; newlines.  The birthday part (either just a date or a date followed
;; by \"birthday\") is stored under vCard type BDAY. The rest is
;; stored newline-separated in the non-standard vCard type
;; X-BBDB-ANNIVERSARY.
;;
;; Field names listed in `bbdb-vcard-x-bbdb-candidates' are in the
;; exported vCard prepended by `X-BBDB-'.
;;
;; The creation-date of the BBDB record is stored as vCard type REV.
;;
;; Remaining members of BBDB Notes are exported to the vCard without
;; change.
;;


;;; History:
;;


;;; Code:

(require 'cl)
(require 'bbdb)
(require 'vcard)
(require 'bbdb-com)

(defconst bbdb-vcard-version "0.3"
  "Version of the vCard importer/exporter.
The major part increases on user-visible changes.")



;;;; User Variables

(defgroup bbdb-vcard nil
  "Customizations for vCards"
  :group 'bbdb)

(defcustom bbdb-vcard-skip-on-import "X-GSM-"
  "Regexp describing vCard elements that are to be discarded during import.
Example: `X-GSM-\\|X-MS-'."
  :group 'bbdb-vcard
  :type 'regexp)

(defcustom bbdb-vcard-skip-valueless t
  "Skip vCard element types with an empty value.
Nil means insert empty types into BBDB."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-import-translation-table
  '(("CELL\\|CAR" . "Mobile")
    ("WORK" . "Office")
    ("HOME" . "Home")  ; translates e.g. "dom,home,postal,parcel" to "Home"
    ("^$" . "Office")) ; acts as a default for parameterless ADR or TEL
  "Label translation on vCard import.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (VCARD-LABEL-REGEXP . BBDB-LABEL).  One entry
should map a default BBDB label to the empty string (`\"^$\"') which
corresponds to unlabelled vCard elements."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-try-merge t
  "Try to merge vCards into existing BBDB records.
Nil means create a fresh bbdb record each time a vCard is read."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-type-canonicalizer 'upcase
  "Function to apply to vCard type names on export.
Most reasonable choices are `upcase' and `downcase'."
  :group 'bbdb-vcard
  :type 'function)

(defcustom bbdb-vcard-x-bbdb-candidates
  '(attribution
    finger-host
    gnus-score
    mark-char
    mail-name
    face
    tex-name
    aka)                                ; not sure what this is for
  "List of translatable BBDB user field names.
On export to a vCard, they are transformed into vCard-compliant
extended types by prepending `X-BBDB-'.  On (re-)import, this prefix
is removed again."
  :group 'bbdb-vcard
  :type '(repeat symbol))

(defcustom bbdb-vcard-export-translation-table
  '(("Mobile" . "CELL")
    ("Office" . "WORK"))
  "Label translation on vCard export.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (BBDB-LABEL-REGEXP . VCARD-LABEL)."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-export-coding-system
  'utf-8-dos        ; dos line endings mandatory according to RFC 2426
  "Coding system to use when writing vCard files."
  :group 'bbdb-vcard
  :type 'symbol)

(defcustom bbdb-vcard-default-dir "~/exported-vcards/"
  "Default storage directory for exported vCards.
Nil means current directory."
  :group 'bbdb-vcard
  :type '(choice directory (const :tag "Current directory" nil)))



;;;; User Functions

;;;###autoload
(defun bbdb-vcard-import-region (begin end)
  "Import the vCards between BEGIN and END into BBDB.
Existing BBDB records may be altered."
  (interactive "r")
  (bbdb-vcard-iterate-vcards 'bbdb-vcard-import-vcard
                             (buffer-substring-no-properties begin end)))

;;;###autoload
(defun bbdb-vcard-import-buffer (vcard-buffer)
  "Import vCards from VCARD-BUFFER into BBDB.
Existing BBDB records may be altered."
  (interactive (list (current-buffer)))
  (set-buffer vcard-buffer)
  (bbdb-vcard-import-region (point-min) (point-max)))

;;;###autoload
(defun bbdb-vcard-import-file (vcard-file)
  "Import vCards from VCARD-FILE into BBDB.
If VCARD-FILE is a wildcard, import each matching file.  Existing BBDB
records may be altered."
  (interactive "FvCard file (or wildcard): ")
  (dolist (vcard-file (file-expand-wildcards vcard-file))
    (with-temp-buffer
      (insert-file-contents vcard-file)
      (bbdb-vcard-import-region (point-min) (point-max)))))

;;;###autoload
(defun bbdb-vcard-export
  (filename-or-directory all-records-p one-file-per-record-p)
  "From Buffer *BBDB*, write one or more record(s) as vCard(s) to file(s).
\\<bbdb-mode-map>\
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-vcard-export]\"\
is used instead of simply \"\\[bbdb-vcard-export]\", then export all \
records currently
in the *BBDB* buffer.  If used with prefix argument, store records
in individual files."
  (interactive
   (let ((default-filename              ; argument filename-or-directory
           (bbdb-vcard-make-file-name (bbdb-current-record nil)))
         (all-records-p (bbdb-do-all-records-p)))
     (list
      (if all-records-p
          (if current-prefix-arg
              (read-directory-name "Write vCard files to directory: "
                                   bbdb-vcard-default-dir nil 42)
            (read-file-name
             "Write vCards to file: "
             bbdb-vcard-default-dir
             nil nil
             (format-time-string "%Y-%m-%dT%H:%M.vcf" (current-time))))
        (read-file-name "Write current record to vCard file: "
                        bbdb-vcard-default-dir nil nil default-filename))
      all-records-p           ; argument all-records-p
      current-prefix-arg)))   ; argument one-file-per-record-p
  (if all-records-p
      (let ((records (progn (set-buffer bbdb-buffer-name)
                            (mapcar 'car bbdb-records)))
            used-up-basenames)          ; keep them unique
        (if one-file-per-record-p
            (progn
              (dolist (record records)
                (with-temp-buffer
                  (let ((basename
                         (bbdb-vcard-make-file-name record
                                                    used-up-basenames)))
                    (insert (bbdb-vcard-from record))
                    (bbdb-vcard-write-buffer
                     (concat filename-or-directory basename))
                    (push basename used-up-basenames))))
              (message "Wrote %d vCards to %s"
                       (length used-up-basenames) filename-or-directory))
          (with-temp-buffer     ; all visible BBDB records in one file
            (dolist (record records)
              (insert (bbdb-vcard-from record)))
            (bbdb-vcard-write-buffer filename-or-directory))))
    (let ((vcard (bbdb-vcard-from (bbdb-current-record nil)))) ; current record
      (with-temp-buffer
        (insert vcard)
        (bbdb-vcard-write-buffer filename-or-directory)))))

;;;###autoload
(defun bbdb-vcard-export-to-kill-ring (all-records-p)
  "From Buffer *BBDB*, copy one or more record(s) as vCard(s) to the kill ring.
\\<bbdb-mode-map>\
If \"\\[bbdb-apply-next-command-to-all-records]\
\\[bbdb-vcard-export-to-kill-ring]\"\
is used instead of simply \"\\[bbdb-vcard-export-to-kill-ring]\", \
then export all records currently in
the *BBDB* buffer."
  (interactive (let ((all-records-p (bbdb-do-all-records-p)))
                 (list all-records-p)))
  (if all-records-p
      (let ((records (progn (set-buffer bbdb-buffer-name)
                            (mapcar 'car bbdb-records))))
        (kill-new "")
        (dolist (record records)
          (kill-append (bbdb-vcard-from record) nil))
        (message "Saved %d records as vCards" (length records)))
    (kill-new (bbdb-vcard-from (bbdb-current-record nil)))
    (message "Saved record as vCard")))

;;;###autoload (define-key bbdb-mode-map [(v)] 'bbdb-vcard-export)
(define-key bbdb-mode-map [(v)] 'bbdb-vcard-export)
;;;###autoload (define-key bbdb-mode-map [(V)] 'bbdb-vcard-export-to-kill-ring)
(define-key bbdb-mode-map [(V)] 'bbdb-vcard-export-to-kill-ring)



(defun bbdb-vcard-iterate-vcards (vcard-processor vcards)
  "Apply VCARD-PROCESSOR successively to each vCard in string VCARDS.
When VCARDS is nil, return nil.  Otherwise, return t."
  (with-temp-buffer
    (insert vcards)
    (goto-char (point-min))
    ;; Change CRLF into CR if necessary, dealing with inconsistent line
    ;; endings.
    (while (re-search-forward "\r\n" nil t)
      (replace-match "\n" nil nil nil 1))
    (setf (buffer-string) (bbdb-vcard-unfold-lines (buffer-string)))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([[:alnum:]-]*\\.\\)?*BEGIN:VCARD[\n[:print:][:cntrl:]]*?\\(^\\([[:alnum:]-]*\\.\\)?END:VCARD\\)"
            nil t)
      (let ((vcard (match-string 0)))
        (if (string= "3.0" (bbdb-vcard-version-of vcard))
            (funcall vcard-processor vcard)
          (funcall vcard-processor      ; probably a v2.1 vCard
                   (bbdb-vcard-unfold-lines
                    (bbdb-vcard-convert-to-3.0 vcard))))))))

(defun bbdb-vcard-version-of (vcard)
  "Return version number string of VCARD."
  (with-temp-buffer
    (insert vcard)
    (car (bbdb-vcard-values-of-type "version" "value"))))

(defun bbdb-vcard-import-vcard (vcard)
  "Store VCARD (version 3.0) in BBDB.
Extend existing BBDB records where possible."
  (with-temp-buffer
    (insert vcard)
    (let* ((raw-name (car (bbdb-vcard-values-of-type "N" "value" t t)))
           ;; Name suitable for storing in BBDB:
           (name (bbdb-vcard-unescape-strings
                  (bbdb-vcard-unvcardize-name raw-name)))
           ;; Name to search for in BBDB now:
           (name-to-search-for
            (when raw-name (if (stringp raw-name)
                               raw-name
                             (concat (nth 1 raw-name) ;given name
                                     " .*"
                                     (nth 0 raw-name))))) ; family name
           ;; Additional names from prefixed types like A.N, B.N, etc.:
           (other-names
            (mapcar
             (lambda (n)
               (bbdb-join (bbdb-vcard-unvcardize-name (cdr (assoc "value" n)))
                          " "))
             (bbdb-vcard-elements-of-type "N" nil t)))
           (vcard-formatted-names (bbdb-vcard-unescape-strings
                                   (bbdb-vcard-values-of-type "FN" "value")))
           (vcard-nicknames
            (bbdb-vcard-unescape-strings
             (bbdb-vcard-split-structured-text
              (car (bbdb-vcard-values-of-type "NICKNAME" "value"))
              "," t)))
           ;; Company suitable for storing in BBDB:
           (vcard-org
            (bbdb-vcard-unescape-strings
             (bbdb-vcard-unvcardize-org
              (car (bbdb-vcard-values-of-type "ORG" "value" t t)))))
           ;; Company to search for in BBDB now:
           (org-to-search-for vcard-org) ; sorry
           ;; Email suitable for storing in BBDB:
           (vcard-email (bbdb-vcard-values-of-type "EMAIL" "value"))
           ;; Email to search for in BBDB now:
           (email-to-search-for
            (when vcard-email
              (concat "\\(" (bbdb-join vcard-email "\\)\\|\\(") "\\)")))
           ;; Phone numbers suitable for storing in BBDB:
           (vcard-tels
            (mapcar (lambda (tel)
                      (vector (bbdb-vcard-translate
                               (or (cdr (assoc "type" tel)) ""))
                              (cdr (assoc "value" tel))))
                    (bbdb-vcard-elements-of-type "TEL")))
           ;; Phone numbers to search for in BBDB now:
           (tel-to-search-for
            (when vcard-tels
              (concat "\\("
                      (mapconcat (lambda (x) (elt x 1))
                                 vcard-tels "\\)\\|\\(")
                      "\\)")))
           ;; Addresses
           (vcard-adrs
            (mapcar 'bbdb-vcard-unvcardize-adr
                    (bbdb-vcard-elements-of-type "ADR" nil t)))
           (vcard-url (car (bbdb-vcard-values-of-type "URL" "value" t)))
           (vcard-notes (bbdb-vcard-values-of-type "NOTE" "value"))
           (raw-bday (bbdb-vcard-unvcardize-date-time
                      (car (bbdb-vcard-values-of-type "BDAY" "value" t))))
           ;; Birthday suitable for storing in BBDB (usable by org-mode):
           (vcard-bday (when raw-bday (concat raw-bday " birthday")))
           ;; Birthday to search for in BBDB now:
           (bday-to-search-for vcard-bday)
           ;; Non-birthday anniversaries, probably exported by ourselves:
           (vcard-x-bbdb-anniversaries
            (bbdb-vcard-split-structured-text
             (car (bbdb-vcard-values-of-type "X-BBDB-ANNIVERSARY" "value"))
             "\\\\n" t))
           (vcard-rev (bbdb-vcard-unvcardize-date-time
                       (car (bbdb-vcard-values-of-type "REV" "value"))))
           (vcard-categories (bbdb-vcard-values-of-type "CATEGORIES" "value"))
           ;; The BBDB record to change:
           (record-freshness-info "BBDB record changed:") ; default user info
           (bbdb-record
            (or
             ;; Try to find an existing one ...
             ;; (a) try company and net and name:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for
                        org-to-search-for email-to-search-for)))
             ;; (b) try company and name:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records) name-to-search-for org-to-search-for)))
             ;; (c) try net and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil email-to-search-for)))
             ;; (d) try birthday and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil nil bday-to-search-for)))
             ;; (e) try phone and name; we may change company here:
             (car (and bbdb-vcard-try-merge
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for nil nil nil tel-to-search-for)))
             ;; No existing record found; make a fresh one:
             (let ((fresh-record (make-vector bbdb-record-length nil)))
               (bbdb-record-set-cache fresh-record
                                      (make-vector bbdb-cache-length nil))
               (if vcard-rev            ; For fresh records,
                   (bbdb-record-putprop ; set creation-date from vcard-rev
                    fresh-record 'creation-date vcard-rev)
                 (run-hook-with-args 'bbdb-create-hook fresh-record))
               (setq record-freshness-info "BBDB record added:") ; user info
               fresh-record)))
           (bbdb-akas (bbdb-record-aka bbdb-record))
           (bbdb-addresses (bbdb-record-address bbdb-record))
           (bbdb-phones (bbdb-record-phone bbdb-record))
           (bbdb-nets (bbdb-record-mail bbdb-record))
           (bbdb-raw-notes (bbdb-record-Notes bbdb-record))
           notes
           other-vcard-type)
      (bbdb-vcard-elements-of-type "BEGIN")   ; get rid of delimiter
      (bbdb-vcard-elements-of-type "END")     ; get rid of delimiter
      (bbdb-vcard-elements-of-type "VERSION") ; get rid of this too
      (when name ; which should be the case as N is mandatory in vCard
        (bbdb-record-set-firstname bbdb-record (car name))
        (bbdb-record-set-lastname bbdb-record (cadr name)))
      (bbdb-record-set-aka
       bbdb-record
       (remove (concat (bbdb-record-firstname bbdb-record)
                       " " (bbdb-record-lastname bbdb-record))
               (reduce (lambda (x y) (union x y :test 'string=))
                       (list vcard-nicknames
                             other-names
                             vcard-formatted-names
                             bbdb-akas))))
;      (when vcard-org (bbdb-record-set-organization bbdb-record vcard-org))
      (bbdb-record-set-mail
       bbdb-record (union vcard-email bbdb-nets :test 'string=))
      (bbdb-record-set-address
       bbdb-record (union vcard-adrs bbdb-addresses :test 'equal))
      (bbdb-record-set-phone bbdb-record
                              (union vcard-tels bbdb-phones :test 'equal))
      ;; prepare bbdb's notes:
      (when vcard-url (push (cons 'www vcard-url) bbdb-raw-notes))
      (when vcard-notes
        ;; Put vCard NOTEs under key 'notes (append if necessary).
        (unless (assq 'notes bbdb-raw-notes)
          (push (cons 'notes "") bbdb-raw-notes))
        (setf (cdr (assq 'notes bbdb-raw-notes))
              (bbdb-vcard-merge-strings
               (cdr (assq 'notes bbdb-raw-notes))
               (bbdb-vcard-unescape-strings vcard-notes)
               ";\n")))
      (when (or vcard-bday vcard-x-bbdb-anniversaries)
        ;; Put vCard BDAY and vCard X-BBDB-ANNIVERSARY's under key
        ;; 'anniversary (append if necessary) where org-mode can find
        ;; it.  Org-mode doesn't currently (v6.35) bother with time
        ;; and time zone, though.
        (when vcard-bday (push vcard-bday vcard-x-bbdb-anniversaries))
        (unless (assq 'anniversary bbdb-raw-notes)
          (push (cons 'anniversary "") bbdb-raw-notes))
        (setf (cdr (assq 'anniversary bbdb-raw-notes))
              (bbdb-vcard-merge-strings
               (cdr (assq 'anniversary bbdb-raw-notes))
               (bbdb-vcard-unescape-strings vcard-x-bbdb-anniversaries)
               "\n")))
      (when vcard-categories
        ;; Put vCard CATEGORIES under key 'mail-alias (append if necessary).
        (unless (assq 'mail-alias bbdb-raw-notes)
          (push (cons 'mail-alias "") bbdb-raw-notes))
        (setf (cdr (assq 'mail-alias bbdb-raw-notes))
              (bbdb-vcard-merge-strings
               (cdr (assq 'mail-alias bbdb-raw-notes))
               vcard-categories
               ",")))
      (while (setq other-vcard-type (bbdb-vcard-other-element))
        (when (string-match "^\\([[:alnum:]-]*\\.\\)?AGENT"
                            (symbol-name (car other-vcard-type)))
          ;; Notice other vCards inside the current one.
          (bbdb-vcard-iterate-vcards
           'bbdb-vcard-import-vcard    ; needed for inner v2.1 vCards:
           (replace-regexp-in-string "\\\\" "" (cdr other-vcard-type))))
        (unless (or (and bbdb-vcard-skip-on-import
                         (string-match bbdb-vcard-skip-on-import
                                       (symbol-name (car other-vcard-type))))
                    (and bbdb-vcard-skip-valueless
                         (zerop (length (cdr other-vcard-type)))))
          (push (bbdb-vcard-remove-x-bbdb other-vcard-type) bbdb-raw-notes)))
      (bbdb-record-set-Notes
       bbdb-record
       (remove-duplicates bbdb-raw-notes :test 'equal :from-end t))
      (bbdb-change-record bbdb-record t t)
      ;; Tell the user what we've done.
      (message "%s %s %s -- %s"
               record-freshness-info
               (bbdb-record-firstname bbdb-record)
               (bbdb-record-lastname bbdb-record)
               (replace-regexp-in-string
                "\n" "; " (or (bbdb-record-organization bbdb-record) "-"))))))

(defun bbdb-vcard-from (record)
  "Return BBDB RECORD as a vCard."
  (with-temp-buffer
    (let* ((name (bbdb-record-name record))
           (first-name (bbdb-record-firstname record))
           (last-name (bbdb-record-lastname record))
           (aka (bbdb-record-aka record))
           (company (bbdb-record-company record))
           (net (bbdb-record-mail record))
           (phones (bbdb-record-phone record))
           (addresses (bbdb-record-address record))
           (www (bbdb-get-field record 'www))
           (notes
            (bbdb-vcard-split-structured-text (bbdb-record-notes record)
                                              ";\n" t))
           (raw-anniversaries (bbdb-vcard-split-structured-text
                               (bbdb-get-field record 'anniversary) "\n" t))
           (birthday-regexp
            "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9][t:0-9]*[-+z:0-9]*\\)\\([[:blank:]]+birthday\\)?\\'")
           (birthday
            (car (bbdb-vcard-split-structured-text
                  (find-if (lambda (x) (string-match birthday-regexp x))
                           raw-anniversaries)
                  " " t)))
           (other-anniversaries
            (remove-if (lambda (x) (string-match birthday-regexp x))
                       raw-anniversaries :count 1))
           (creation-date (bbdb-get-field record 'creation-date))
           (mail-aliases (bbdb-record-getprop record
                                              bbdb-define-all-aliases-field))
           (raw-notes (copy-alist (bbdb-record-raw-notes record))))
      (bbdb-vcard-insert-vcard-element "BEGIN" "VCARD")
      (bbdb-vcard-insert-vcard-element "VERSION" "3.0")
      (bbdb-vcard-insert-vcard-element "FN" (bbdb-vcard-escape-strings name))
      (bbdb-vcard-insert-vcard-element
       "N" (bbdb-vcard-escape-strings last-name)
       ";" (bbdb-vcard-escape-strings first-name)
       ";;;") ; Additional Names, Honorific Prefixes, Honorific Suffixes
      (bbdb-vcard-insert-vcard-element
       "NICKNAME" (bbdb-join (bbdb-vcard-escape-strings aka) ","))
      (bbdb-vcard-insert-vcard-element
       "ORG" (bbdb-vcard-escape-strings company))
      (dolist (mail net)
        (bbdb-vcard-insert-vcard-element
         "EMAIL;TYPE=INTERNET" (bbdb-vcard-escape-strings mail)))
      (dolist (phone phones)
        (bbdb-vcard-insert-vcard-element
         (concat
          "TEL;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-phone-location phone) t)))
         (bbdb-vcard-escape-strings (bbdb-phone-string phone))))
      (dolist (address addresses)
        (bbdb-vcard-insert-vcard-element
         (concat
          "ADR;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-address-location address) t)))
         ";;"                           ; no Postbox, no Extended
         (bbdb-join (bbdb-vcard-escape-strings (bbdb-address-streets address))
                    ",")
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-city address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-state address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-zip address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-country address)))))
      (bbdb-vcard-insert-vcard-element "URL" www)
      (dolist (note notes)
        (bbdb-vcard-insert-vcard-element
         "NOTE" (bbdb-vcard-escape-strings note)))
      (bbdb-vcard-insert-vcard-element "BDAY" birthday)
      (bbdb-vcard-insert-vcard-element  ; non-birthday anniversaries
       "X-BBDB-ANNIVERSARY" (bbdb-join other-anniversaries "\\n"))
      (bbdb-vcard-insert-vcard-element "REV" creation-date)
      (bbdb-vcard-insert-vcard-element
       "CATEGORIES"
       (bbdb-join (bbdb-vcard-escape-strings
                   (bbdb-vcard-split-structured-text mail-aliases "," t)) ","))
      ;; prune raw-notes...
      (dolist (key '(www notes anniversary mail-alias creation-date timestamp))
        (setq raw-notes (assq-delete-all key raw-notes)))
      ;; ... and output what's left
      (dolist (raw-note raw-notes)
        (bbdb-vcard-insert-vcard-element
         (symbol-name (bbdb-vcard-prepend-x-bbdb-maybe (car raw-note)))
         (bbdb-vcard-escape-strings (cdr raw-note))))
      (bbdb-vcard-insert-vcard-element "END" "VCARD")
      (bbdb-vcard-insert-vcard-element nil)) ; newline
    (buffer-string)))



(defun bbdb-vcard-convert-to-3.0 (vcard)
  "Convert VCARD from v2.1 to v3.0.
Return a version 3.0 vCard as a string.  Don't bother about the vCard
v3.0 mandatory elements N and FN."
  ;; Prevent customization of vcard.el's from being changed behind our back:
  (let ((vcard-standard-filters '(vcard-filter-html)))
    (with-temp-buffer
      (bbdb-vcard-insert-vcard-element "BEGIN" "VCARD")
      (bbdb-vcard-insert-vcard-element "VERSION" "3.0")
      (dolist (element (remove*
                        "VERSION" (vcard-parse-string vcard)
                        :key (lambda (x) (upcase (caar x))) :test 'string=))
        (bbdb-vcard-insert-vcard-element
         (concat (caar element)
                 (mapconcat 'bbdb-vcard-parameter-pair (cdar element) ""))
         (bbdb-join (bbdb-vcard-escape-strings (cdr element)) ";")))
      (bbdb-vcard-insert-vcard-element "END" "VCARD")
      (bbdb-vcard-insert-vcard-element nil)
      (buffer-string))))

(defun bbdb-vcard-parameter-pair (input)
  "Return \"parameter=value\" made from INPUT.
INPUT is its representation in vcard.el.  Return empty string if INPUT
is nil."
  (cond ((consp input) (concat ";" (car input) "=" (cdr input)))
        ((stringp input) (concat ";TYPE=" input))
        ((null input) "")))



(defun bbdb-vcard-values-of-type
  (type parameter &optional one-is-enough-p split-value-at-semi-colon-p)
  "Return in a list the values of PARAMETER of vCard element of TYPE.
The VCard element is read and deleted from current buffer which is
supposed to contain a single vCard.  If ONE-IS-ENOUGH-P is non-nil,
read and delete only the first element of TYPE.  If PARAMETER is
\"value\" and SPLIT-VALUE-AT-SEMI-COLON-P is non-nil, split the value
at semi-colons into a list."
  (mapcar (lambda (x) (cdr (assoc parameter x)))
          (bbdb-vcard-elements-of-type
           type one-is-enough-p split-value-at-semi-colon-p)))

(defun bbdb-vcard-elements-of-type
  (type &optional one-is-enough-p split-value-at-semi-colon-p)
  "From current buffer read and delete the vCard elements of TYPE.
The current buffer is supposed to contain a single vCard.  If
ONE-IS-ENOUGH-P is non-nil, read and delete only the first element of
TYPE.  Return a list of alists, one per element.  Each alist has a
cell with key \"value\" containing the element's value, and may have
other elements of the form \(parameter-name . parameter-value).  If
SPLIT-VALUE-AT-SEMI-COLON-P is non-nil, split the value at key
\"value\" at semi-colons into a list."
  (goto-char (point-min))
  (let (values parameters read-enough)
    (while
        (and
         (not read-enough)
         (re-search-forward
          (concat
           "^\\([[:alnum:]-]*\\.\\)?\\(" type "\\)\\(;.*\\)?:\\(.*\\)$")
          nil t))
      (goto-char (match-end 2))
      (setq parameters nil)
      (push (cons "value" (if split-value-at-semi-colon-p
                              (bbdb-vcard-split-structured-text
                               (match-string 4) ";")
                            (match-string 4)))
            parameters)
      (while (re-search-forward "\\([^;:=]+\\)=\\([^;:]+\\)"
                                (line-end-position) t)
        (let* ((parameter-key (downcase (match-string 1)))
               (parameter-value (downcase (match-string 2)))
               (parameter-sibling (assoc parameter-key parameters)))
          (if parameter-sibling         ; i.e., pair with equal key
              ;; collect vCard parameter list `;a=x;a=y;a=z'
              ;; into vCard value list `;a=x,y,z'; becoming ("a" . "x,y,z")
              (setf (cdr parameter-sibling)
                    (concat (cdr parameter-sibling) "," parameter-value))
            ;; vCard parameter pair `;key=value;' with new key
            (push (cons parameter-key parameter-value) parameters))))
      (push parameters values)
      (delete-region (line-end-position 0) (line-end-position))
      (when one-is-enough-p (setq read-enough t)))
    (nreverse values)))

(defun bbdb-vcard-other-element ()
  "From current buffer read and delete the topmost vCard element.
Buffer is supposed to contain a single vCard.  Return (TYPE . VALUE)."
  (goto-char (point-min))
  (when (re-search-forward "^\\([[:graph:]]*?\\):\\(.*\\)$" nil t)
    (let ((type (match-string 1))
          (value (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (cons (intern (downcase type)) (bbdb-vcard-unescape-strings value)))))

(defun bbdb-vcard-insert-vcard-element (type &rest values)
  "Insert a vCard element comprising TYPE, `:', VALUES into current buffer.
Take care of TYPE canonicalization, line folding, and closing newline.
Do nothing if TYPE is non-nil and VALUES are empty.  Insert just a
newline if TYPE is nil."
  (if type
      (let ((value (bbdb-join values "")))
        (unless (zerop (length value))
          (insert (bbdb-vcard-fold-line
                   (concat (bbdb-vcard-canonicalize-vcard-type type)
                           ":" value)))))
    (insert (bbdb-vcard-fold-line ""))))



(defun bbdb-vcard-unfold-lines (vcards)
  "Return folded vCard lines from VCARDS unfolded."
  (replace-regexp-in-string  "\n\\( \\|\t\\)" "" vcards))

(defun bbdb-vcard-fold-line (long-line)
  "Insert after every 75th position in LONG-LINE a newline and a space."
  (with-temp-buffer (insert long-line)
                    (goto-char (point-min))
                    (while (< (goto-char (+ (point) 75))
                              (point-max))
                      (insert "\n "))
                    (insert "\n")
                    (buffer-string)))

(defun bbdb-vcard-unescape-strings (escaped-strings)
  "Unescape escaped `;', `,', `\\', and newlines in ESCAPED-STRINGS.
ESCAPED-STRINGS may be a string or a sequence of strings."
  (flet ((unescape (x) (replace-regexp-in-string
                        "\\([\\\\]\\)\\([,;\\]\\)" ""
                        (replace-regexp-in-string "\\\\n" "\n" x)
                        nil nil 1)))
    (bbdb-vcard-process-strings 'unescape escaped-strings)))

(defun bbdb-vcard-escape-strings (unescaped-strings )
  "Escape `;', `,', `\\', and newlines in UNESCAPED-STRINGS.
UNESCAPED-STRINGS may be a string or a sequence of strings."
  (flet ((escape (x) (replace-regexp-in-string ; from 2.1 conversion:
                      "\r" "" (replace-regexp-in-string
                               "\n" "\\\\n" (replace-regexp-in-string
                                             "\\(\\)[,;\\]" "\\\\" (or x "")
                                             nil nil 1)))))
    (bbdb-vcard-process-strings 'escape unescaped-strings)))

(defun bbdb-vcard-process-strings (string-processor strings)
  "Apply STRING-PROCESSOR to STRINGS.
STRINGS may be a string or a sequence of strings."
  (if (stringp strings)
      (funcall string-processor strings)
    (mapcar string-processor strings)))



(defun bbdb-vcard-remove-x-bbdb (vcard-element)
  "Remove the `X-BBDB-' prefix from the type part of VCARD-ELEMENT if any."
  (cons (intern (replace-regexp-in-string
                 "^X-BBDB-" "" (symbol-name (car vcard-element))))
        (cdr vcard-element)))

(defun bbdb-vcard-prepend-x-bbdb-maybe (bbdb-fieldname)
  "If BBDB-FIELDNAME is in `bbdb-vcard-x-bbdb-candidates', prepend `X-BBDB'."
  (if (member bbdb-fieldname bbdb-vcard-x-bbdb-candidates)
      (intern (concat "x-bbdb-" (symbol-name bbdb-fieldname)))
    bbdb-fieldname))                  ; lowercase more consistent here

(defun bbdb-vcard-unvcardize-name (vcard-name)
  "Convert VCARD-NAME (type N) into (FIRSTNAME LASTNAME)."
  (if (stringp vcard-name)              ; unstructured N
      (bbdb-divide-name vcard-name)
    (let ((vcard-name
           (mapcar (lambda (x)
                     (bbdb-join (bbdb-vcard-split-structured-text x "," t)
                                " "))
                   vcard-name))) ; flatten comma-separated substructure
      (list (concat (nth 3 vcard-name)  ; honorific prefixes
                    (unless (zerop (length (nth 3 vcard-name))) " ")
                    (nth 1 vcard-name)  ; given name
                    (unless (zerop (length (nth 2 vcard-name))) " ")
                    (nth 2 vcard-name)) ; additional names
            (concat (nth 0 vcard-name)  ; family name
                    (unless (zerop (length (nth 4 vcard-name))) " ")
                    (nth 4 vcard-name)))))) ; honorific suffixes

(defun bbdb-vcard-unvcardize-org (vcard-org)
  "Convert VCARD-ORG (type ORG), which may be a list, into a string."
  (if (or (null vcard-org)
          (stringp vcard-org)) ; unstructured, probably non-standard ORG
      vcard-org                ; Company, unit 1, unit 2...
    (bbdb-join vcard-org "\n")))

(defun bbdb-vcard-unvcardize-adr (vcard-adr)
  "Convert VCARD-ADR into BBDB format.
Turn a vCard element of type ADR into (TYPE STREETS CITY STATE ZIP
COUNTRY)."
  (let ((adr-type (or (cdr (assoc "type" vcard-adr)) ""))
        (streets         ; all comma-separated sub-elements of
         (remove         ; Postbox, Extended, Streets go into one list
          "" (reduce 'append
                     (mapcar (lambda (x)
                               (bbdb-vcard-split-structured-text x "," t))
                             (subseq (cdr (assoc "value" vcard-adr))
                                     0 3)))))
        (non-streets          ; turn comma-separated substructure into
         (mapcar              ; newline-separated text
          (lambda (x) (bbdb-join
                       (bbdb-vcard-split-structured-text x "," t)
                       "\n"))
          (subseq (cdr (assoc "value" vcard-adr))
                  3 nil))))
    (vector (bbdb-vcard-translate adr-type)
            streets
            (or (elt non-streets 0) "")    ; City
            (or (elt non-streets 1) "")    ; State
            (or (elt non-streets 2) "")    ; Zip
            (or (elt non-streets 3) "")))) ; Country

(defun bbdb-vcard-unvcardize-date-time (date-time)
  "If necessary, make DATE-TIME usable for storage in BBDB.
Convert yyyymmdd, yyyymmddThhmmss, or yyymmddThhmmssZhhmm into
yyyy-mm-dd, yyyy-mm-ddThh:mm:ss, or yyy-mm-ddThh:mm:ssZhh:mm
respectively.  Discard fractions of a second.  Return anything else
unchanged."
  (if (and (stringp date-time)
           (string-match
            "\\([0-9]\\{4\\}\\)-?\\([0-2][0-9]\\)-?\\([0-3][0-9]\\)\\(?:t\\([0-5][0-9]\\):?\\([0-5][0-9]\\):?\\([0-5][0-9]\\)\\(?:[,.0-9]*\\(\\([+-][0-5][0-9]\\):?\\([0-5][0-9]\\)?\\|z\\)\\)?\\)?"
            date-time))
      (concat
       (match-string 1 date-time) "-"
       (match-string 2 date-time) "-" (match-string 3 date-time)
       (when (match-string 6 date-time) ; seconds part of time
         (concat
          "T" (match-string 4 date-time) ":"
          (match-string 5 date-time) ":" (match-string 6 date-time)
          (when (match-string 7 date-time) ; time zone
            (if (match-string 9 date-time) ; time zone minute
                  (concat (match-string 8 date-time) ; time zone hour
                          ":" (match-string 9 date-time)) ; time zone minute
              "Z")))))
    date-time))

(defun bbdb-vcard-vcardize-address-element (address-element)
  "Replace escaped newlines in ADDRESS-ELEMENT by commas."
  (replace-regexp-in-string "\\\\n" "," address-element))

(defun bbdb-vcard-translate (label &optional exportp)
  "Translate LABEL from vCard to BBDB or, if EXPORTP is non-nil, vice versa.
Translations are defined in `bbdb-vcard-import-translation-table' and
`bbdb-vcard-export-translation-table' respectively."
  (when label
    (capitalize
     (or (assoc-default label
                        (if exportp
                            bbdb-vcard-export-translation-table
                          bbdb-vcard-import-translation-table) 'string-match)
         label))))

(defun bbdb-vcard-merge-strings (old-string new-strings separator)
  "Merge strings successively from list NEW-STRINGS into OLD-STRING.
If an element of NEW-STRINGS is already in OLD-STRING, leave
OLD-STRING unchanged.  Otherwise append SEPARATOR and NEW-STRING."
  (with-temp-buffer
    (insert old-string)
    (dolist (new-string new-strings)
      (unless (prog1 (search-backward new-string nil t)
                (goto-char (point-max)))
        (unless (zerop (buffer-size)) (insert separator))
        (insert new-string)))
    (buffer-string)))

(defun bbdb-vcard-split-structured-text
  (text separator &optional return-always-list-p)
  "Split TEXT at unescaped occurrences of SEPARATOR; return parts in a list.
Return text unchanged if there aren't any separators and RETURN-ALWAYS-LIST-P
is nil."
  (when (stringp text)
    (let ((string-elements
           (split-string
            (replace-regexp-in-string
             (concat "\\\\\r" separator) (concat "\\\\" separator)
             (replace-regexp-in-string separator (concat "\r" separator) text))
            (concat "\r" separator))))
      (if (and (null return-always-list-p)
               (= 1 (length string-elements)))
          (car string-elements)
        string-elements))))

(defun bbdb-vcard-canonicalize-vcard-type (&rest strings)
  "Concatenate STRINGS and apply `bbdb-vcard-type-canonicalizer' to them."
  (funcall bbdb-vcard-type-canonicalizer (bbdb-join strings "")))

(defun bbdb-vcard-write-buffer (vcard-file-name)
  "Write current buffer to VCARD-FILE-NAME.
Create directories where necessary."
  (make-directory (file-name-directory vcard-file-name) t)
  (let ((buffer-file-coding-system bbdb-vcard-export-coding-system))
    (write-region nil nil vcard-file-name nil nil nil t)))

(defun bbdb-vcard-make-file-name (bbdb-record &optional used-up-basenames)
  "Come up with a vCard filename given a BBDB-RECORD.
Make it unique against the list USED-UP-BASENAMES."
  (let ((name (bbdb-record-name bbdb-record))
        (aka (car (bbdb-record-aka bbdb-record)))
        (unique-number 0)
        filename)
    (while (member
            (setq filename
                  (concat
                   (replace-regexp-in-string
                    "[[:blank:]]+" "_"
                    (or (unless (zerop (length name)) name)
                        (unless (zerop (length aka)) aka)
                        "bbdb-record"))
                   (unless (zerop unique-number)
                     (concat "-" (number-to-string unique-number)))
                   ".vcf"))
            used-up-basenames)
      (incf unique-number))
    filename))

(defmacro bbdb-vcard-search-intersection
  (records &optional name company net notes phone)
  "Search RECORDS for records that match each non-nil argument."
  (let*
      ((phone-search
        (if phone `(when ,phone (bbdb-search ,records nil nil nil nil ,phone))
          records))
       (notes-search
        (if notes `(when ,notes (bbdb-search ,phone-search nil nil nil ,notes))
          phone-search))
       (net-search
        (if net `(when ,net (bbdb-search ,notes-search nil nil ,net))
          notes-search))
       (company-search
        (if company `(when ,company (bbdb-search ,net-search nil ,company))
          net-search))
       (name-search
        (if name `(when ,name (bbdb-search ,company-search ,name))
          company-search)))
    name-search))

(defun bbdb-join (list separator)
  "Join a LIST to a string where the list elements are separated by SEPARATOR.
The inverse function of `bbdb-split'."
  (when list
    (mapconcat 'identity list separator)))

(provide 'bbdb-vcard)

;;; bbdb-vcard.el ends here

; LocalWords:  vcard firstname
