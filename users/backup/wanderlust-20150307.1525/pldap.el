;;; pldap.el --- A portable LDAP support for Emacs.

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Original was ldap.el:
;; Author: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>

;; pldap.el:
;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Maintainer: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: emulating, LDAP, comm
;; Created: 15 June 2000

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;;; Code:
;;

(eval-when-compile (require 'cl))

(defmacro ldap-static-if (cond then &rest else)
  "`if' expression but COND is evaluated at compile-time."
  (if (eval cond)
      then
    `(progn ,@else)))

(ldap-static-if (and (not (featurep 'pldap))
		     (fboundp 'ldap-open))
    ;; You have built-in ldap feature (XEmacs).
    (require 'ldap)

;; You don't have built-in ldap feature.
;; Use external program.

;;; For LDIF encoding.
;; SAFE-CHAR                = %x01-09 / %x0B-0C / %x0E-7F
(defconst ldap-ldif-safe-char-regexp
  "[\000-\011\013\014\016-\177]"
  "A Regexp for safe-char.")
;; SAFE-INIT-CHAR           = %x01-09 / %x0B-0C / %x0E-1F /
;;                            %x21-39 / %x3B / %x3D-7F
(defconst ldap-ldif-safe-init-char-regexp
  "[\001-\011\013\014\016-\037\038-\071\073\075-\177]"
  "A Regexp for safe-init-char.")
;; SAFE-STRING              = [SAFE-INIT-CHAR *SAFE-CHAR]
(defconst ldap-ldif-safe-string-regexp
  (concat ldap-ldif-safe-init-char-regexp ldap-ldif-safe-char-regexp "*")
  "A Regexp for safe-string.")

(defconst ldap-ldif-field-name-regexp "[a-zA-Z][a-zA-Z0-9-;]*"
  "A Regexp for field name.")

(defconst ldap-ldif-field-head-regexp
  (concat "^" ldap-ldif-field-name-regexp ":")
  "A Regexp for field head.")

(defconst ldap-ldif-next-field-head-regexp
  (concat "\n" ldap-ldif-field-name-regexp ":")
  "A Regexp for next field head.")

(defun ldap/ldif-safe-string-p (string)
  "Return t if STRING is a safe-string for LDIF."
  ;; Need better implentation.
  (string-match ldap-ldif-safe-string-regexp string))

(defgroup ldap nil
  "Lightweight Directory Access Protocol"
  :group 'comm)

(defvar ldap-search-program "ldapsearch"
  "LDAP search program.")

(defvar ldap-add-program "ldapadd"
  "LDAP add program.")

(defvar ldap-delete-program "ldapdelete"
  "LDAP delete program.")

(defvar ldap-modify-program "ldapmodify"
  "LDAP modify program.")

(defcustom ldap-search-program-arguments '("-LL" "-x")
  "*A list of additional arguments to pass to `ldapsearch'.
It is recommended to use the `-T' switch with Nescape's
implementation to avoid line wrapping.
`-L' is needed to get LDIF outout.
\(`-LL' is needed to get rid of comments from OpenLDAP's ldapsearch.\)
`-x' is needed to use simple authentication.
The `-B' switch should be used to enable the retrieval of
binary values."
  :type '(repeat :tag "`ldapsearch' Arguments"
		 (string :tag "Argument"))
  :group 'ldap)

(defcustom ldap-default-host nil
  "*Default LDAP server hostname."
  :type '(choice (string :tag "Host name")
		 (const :tag "Use library default" nil))
  :group 'ldap)

(defcustom ldap-default-port nil
  "*Default TCP port for LDAP connections.
Initialized from the LDAP library at build time.  Default value is 389."
  :type '(choice (const :tag "Use library default" nil)
		 (integer :tag "Port number"))
  :group 'ldap)

(defcustom ldap-default-base nil
  "*Default base for LDAP searches.
This is a string using the syntax of RFC 1779.
For instance, \"o=ACME, c=US\" limits the search to the
Acme organization in the United States."
  :type '(choice (const :tag "Use library default" nil)
		 (string :tag "Search base"))
  :group 'ldap)

(defcustom ldap-host-parameters-alist nil
  "*Alist of host-specific options for LDAP transactions.
The format of each list element is:
\(HOST PROP1 VAL1 PROP2 VAL2 ...)
HOST is the hostname of an LDAP server (with an optional TCP port number
appended to it  using a colon as a separator).
PROPn and VALn are property/value pairs describing parameters for the server.
Valid properties include:
  `binddn' is the distinguished name of the user to bind as
    (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `auth' is the authentication method to use.
    Possible values are: `simple', `krbv41' and `krbv42'.
  `base' is the base for the search as described in RFC 1779.
  `scope' is one of the three symbols `subtree', `base' or `onelevel'.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return."
  :type '(repeat :menu-tag "Host parameters"
		 :tag "Host parameters"
		 (list :menu-tag "Host parameters"
		       :tag "Host parameters"
		       :value nil
		       (string :tag "Host name")
		       (checklist :inline t
				  :greedy t
				  (list
				   :tag "Search Base"
				   :inline t
				   (const :tag "Search Base" base)
				   string)
				  (list
				   :tag "Binding DN"
				   :inline t
				   (const :tag "Binding DN" binddn)
				   string)
				  (list
				   :tag "Password"
				   :inline t
				   (const :tag "Password" passwd)
				   string)
				  (list
				   :tag "Authentication Method"
				   :inline t
				   (const :tag "Authentication Method" auth)
				   (choice
				    (const :menu-tag "None" :tag "None" nil)
				    (const :menu-tag "Simple" :tag "Simple" simple)
				    (const :menu-tag "Kerberos 4.1" :tag "Kerberos 4.1" krbv41)
				    (const :menu-tag "Kerberos 4.2" :tag "Kerberos 4.2" krbv42)))
				  (list
				   :tag "Search Scope"
				   :inline t
				   (const :tag "Search Scope" scope)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Subtree" :tag "Subtree" subtree)
				    (const :menu-tag "Base" :tag "Base" base)
				    (const :menu-tag "One Level" :tag "One Level" onelevel)))
				  (list
				   :tag "Dereferencing"
				   :inline t
				   (const :tag "Dereferencing" deref)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Never" :tag "Never" never)
				    (const :menu-tag "Always" :tag "Always" always)
				    (const :menu-tag "When searching" :tag "When searching" search)
				    (const :menu-tag "When locating base" :tag "When locating base" find)))
				  (list
				   :tag "Time Limit"
				   :inline t
				   (const :tag "Time Limit" timelimit)
				   (integer :tag "(in seconds)"))
				  (list
				   :tag "Size Limit"
				   :inline t
				   (const :tag "Size Limit" sizelimit)
				   (integer :tag "(number of records)")))))
:group 'ldap)

(defcustom ldap-verbose nil
  "*If non-nil, LDAP operations echo progress messages."
  :type 'boolean
  :group 'ldap)

(defcustom ldap-ignore-attribute-codings nil
  "*If non-nil, do not perform any encoding/decoding on LDAP attribute values."
  :type 'boolean
  :group 'ldap)

(defcustom ldap-default-attribute-encoder nil
  "*Encoder function to use for attributes whose syntax is unknown."
  :type 'symbol
  :group 'ldap)

(defcustom ldap-default-attribute-decoder nil
  "*Decoder function to use for attributes whose syntax is unknown."
  :type 'symbol
  :group 'ldap)

(defcustom ldap-coding-system nil
  "*Coding system of LDAP string values.
LDAP v3 specifies the coding system of strings to be UTF-8.
Mule support is needed for this."
  :type 'symbol
  :group 'ldap)

(defvar ldap-attribute-syntax-encoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-encode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-encode-country-string		; 11 Country String                  Y
   ldap-encode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-encode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   number-to-string			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-encode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-encode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to encode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")

(defvar ldap-attribute-syntax-decoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-decode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-decode-string			; 11 Country String                  Y
   ldap-decode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-decode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   string-to-number			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-decode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-decode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to decode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")

(defvar ldap-attribute-syntaxes-alist
  '((createtimestamp . 24)
    (modifytimestamp . 24)
    (creatorsname . 12)
    (modifiersname . 12)
    (subschemasubentry . 12)
    (attributetypes . 3)
    (objectclasses . 37)
    (matchingrules . 30)
    (matchingruleuse . 31)
    (namingcontexts . 12)
    (altserver . 26)
    (supportedextension . 38)
    (supportedcontrol . 38)
    (supportedsaslmechanisms . 15)
    (supportedldapversion . 27)
    (ldapsyntaxes . 16)
    (ditstructurerules . 17)
    (nameforms . 35)
    (ditcontentrules . 16)
    (objectclass . 38)
    (aliasedobjectname . 12)
    (cn . 15)
    (sn . 15)
    (serialnumber . 44)
    (c . 15)
    (l . 15)
    (st . 15)
    (street . 15)
    (o . 15)
    (ou . 15)
    (title . 15)
    (description . 15)
    (searchguide . 25)
    (businesscategory . 15)
    (postaladdress . 41)
    (postalcode . 15)
    (postofficebox . 15)
    (physicaldeliveryofficename . 15)
    (telephonenumber . 50)
    (telexnumber . 52)
    (telexterminalidentifier . 51)
    (facsimiletelephonenumber . 22)
    (x121address . 36)
    (internationalisdnnumber . 36)
    (registeredaddress . 41)
    (destinationindicator . 44)
    (preferreddeliverymethod . 14)
    (presentationaddress . 43)
    (supportedapplicationcontext . 38)
    (member . 12)
    (owner . 12)
    (roleoccupant . 12)
    (seealso . 12)
    (userpassword . 40)
    (usercertificate . 8)
    (cacertificate . 8)
    (authorityrevocationlist . 9)
    (certificaterevocationlist . 9)
    (crosscertificatepair . 10)
    (name . 15)
    (givenname . 15)
    (initials . 15)
    (generationqualifier . 15)
    (x500uniqueidentifier . 6)
    (dnqualifier . 44)
    (enhancedsearchguide . 21)
    (protocolinformation . 42)
    (distinguishedname . 12)
    (uniquemember . 34)
    (houseidentifier . 15)
    (supportedalgorithms . 49)
    (deltarevocationlist . 9)
    (dmdname . 15))
  "A map of LDAP attribute names to their type object id minor number.
This table is built from RFC2252 Section 5 and RFC2256 Section 5")

;;; LDAP primitive functions.
;;
;; LDAP object is
;; (__ldap-object HOSTNAME PLIST)

(defun ldapp (object)
  "Return t if OBJECT is a LDAP connection."
  (and (listp object)
       (eq (car object) '__ldap-object)))

(defun ldap-open (host &optional plist)
  "Open a LDAP connection to HOST.
PLIST is a plist containing additional parameters for the connection.
Valid keys in that list are:
  `port' the TCP port to use for the connection if different from
`ldap-default-port'.
  `auth' is the authentication method to use, possible values depend on
the LDAP library: `simple', `krbv41' and `krbv42'.
  `binddn' is the distinguished name of the user to bind as
 (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return."
  (list '__ldap-object host plist))

(defun ldap-host (ldap)
  "Return the server host of the connection LDAP, as a string."
  (nth 1 ldap))

(defun ldap-close (ldap)
  "Close an LDAP connection."
  t)

(defun ldap-delete (ldap dn)
  "Delete an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to delete."
  (let* ((plist (or (nth 2 ldap)
		    (cdr (assoc (ldap-host ldap)
				ldap-host-parameters-alist))))
	 (port   (plist-get plist 'port))
	 (binddn (plist-get plist 'binddn))
	 (passwd (plist-get plist 'passwd))
	 arglist ret)
    (setq arglist (list (format "-h%s" (ldap-host ldap))))
    (if (and port (not (equal 389 port)))
	(setq arglist (nconc arglist (list (format "-p%d" port)))))
    (if (and binddn
	     (not (equal "" binddn)))
	(setq arglist (nconc arglist (list (format "-D%s" binddn)))))
    (if (and passwd
	     (not (equal "" passwd)))
	(setq arglist (nconc arglist (list (format "-w%s" passwd)))))
    (with-temp-buffer
      (setq ret (apply 'call-process
		       ldap-delete-program
		       nil (current-buffer) t
		       (append arglist
			       (list dn))))
      (cond ((integerp ret)
	     (or (zerop ret)
		 (error "%s" (car (split-string (buffer-string) "\n")))))
	    ((and (setq ret (buffer-string)); Nemacs
		  (string-match "ldap_delete:" ret))
	     (error "%s" (car (split-string ret "\n"))))))))

(defmacro ldap/ldif-insert-field (attr value)
  `(if (not (ldap/ldif-safe-string-p ,value))
       (insert ,attr ":: " (base64-encode-string ,value) "\n")
     (insert ,attr ": " ,value "\n")))

(defun ldap-modify (ldap dn mods)
  "Add an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to modify.
MODS is a list of modifications to apply.
A modification is a list of the form (MOD-OP ATTR VALUE1 VALUE2 ...)
MOD-OP and ATTR are mandatory, VALUEs are optional depending on MOD-OP.
MOD-OP is the type of modification, one of the symbols `add', `delete'
or `replace'.  ATTR is the LDAP attribute type to modify."
  (let* ((plist (or (nth 2 ldap)
		    (cdr (assoc (ldap-host ldap)
				ldap-host-parameters-alist))))
	 (port   (plist-get plist 'port))
	 (binddn (plist-get plist 'binddn))
	 (passwd (plist-get plist 'passwd))
	 arglist ret)
    (setq arglist (list (format "-h%s" (ldap-host ldap))))
    (if (and port (not (equal 389 port)))
	(setq arglist (nconc arglist (list (format "-p%d" port)))))
    (if (and binddn
	     (not (equal "" binddn)))
	(setq arglist (nconc arglist (list (format "-D%s" binddn)))))
    (if (and passwd
	     (not (equal "" passwd)))
	(setq arglist (nconc arglist (list (format "-w%s" passwd)))))
    (with-temp-buffer
      (ldap/ldif-insert-field "dn" dn)
      (insert "changetype: modify\n")
      (while mods
	(cond
	 ((eq (nth 0 (car mods)) 'add)
	  (insert "add: " (nth 1 (car mods)) "\n")
	  (ldap/ldif-insert-field (nth 1 (car mods)) (nth 2 (car mods)))
	  (insert "-\n"))
	 ((eq (nth 0 (car mods)) 'delete)
	  (insert "delete: " (nth 1 (car mods)) "\n-\n"))
	 ((eq (nth 0 (car mods)) 'replace)
	  (insert "replace: " (nth 1 (car mods)) "\n")
	  (ldap/ldif-insert-field (nth 1 (car mods)) (nth 2 (car mods)))
	  (insert "-\n")))
	(setq mods (cdr mods)))
      (setq ret (apply 'call-process-region
		       (point-min) (point-max)
		       ldap-modify-program
		       t t nil
		       arglist))
      (cond ((integerp ret)
	     (or (zerop ret)
		 (error "%s" (car (split-string (buffer-string) "\n")))))
	    ((and (setq ret (buffer-string)); Nemacs
		  (string-match "ldap_modify:" ret))
	     (error "%s" (car (split-string ret "\n"))))))))

(defun ldap-add (ldap dn entry)
  "Add an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to add.
ENTRY is an entry specification, i.e., a list of cons cells
containing attribute/value string pairs."
  (let* ((plist (or (nth 2 ldap)
		    (cdr (assoc (ldap-host ldap)
				ldap-host-parameters-alist))))
	 (port   (plist-get plist 'port))
	 (binddn (plist-get plist 'binddn))
	 (passwd (plist-get plist 'passwd))
	 arglist ret)
    (setq arglist (list (format "-h%s" (ldap-host ldap))))
    (if (and port (not (equal 389 port)))
	(setq arglist (nconc arglist (list (format "-p%d" port)))))
    (if (and binddn
	     (not (equal "" binddn)))
	(setq arglist (nconc arglist (list (format "-D%s" binddn)))))
    (if (and passwd
	     (not (equal "" passwd)))
	(setq arglist (nconc arglist (list (format "-w%s" passwd)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (ldap/ldif-insert-field "dn" dn)
      (while entry
	(ldap/ldif-insert-field (car (car entry)) (cdr (car entry)))
	(setq entry (cdr entry)))
      (setq ret (apply 'call-process-region
		       (point-min) (point-max)
		       ldap-add-program
		       t t nil
		       arglist))
      (cond ((integerp ret)
	     (or (zerop ret)
		 (error "%s" (car (split-string (buffer-string) "\n")))))
	    ((and (setq ret (buffer-string)) ; Nemacs
		  (string-match "ldap_add:" ret))
	     (error "%s" (car (split-string ret "\n"))))))))

(defun ldap-search-basic (ldap filter base scope
			       &optional attrs attrsonly withdn verbose)
  "Perform a search on a LDAP server.  (Use external program `ldapsearch')
FILTER is a filter string for the search as described in RFC 1558.
BASE is the distinguished name at which to start the search.
SCOPE is one of the symbols `base', `onelevel' or `subtree' indicating
the scope of the search.
ATTRS is a list of strings indicating which attributes to retrieve
 for each matching entry.  If nil return all available attributes.
If ATTRSONLY is non-nil then only the attributes are retrieved, not
the associated values.
If WITHDN is non-nil each entry in the result will be prepended with
its distinguished name DN.
If VERBOSE is non-nil progress messages will be echoed.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs optionally preceded by the DN of the
entry according to the value of WITHDN."
  (let* ((plist (or (nth 2 ldap)
		    (cdr (assoc (ldap-host ldap)
				ldap-host-parameters-alist))))
	 (port   (plist-get plist 'port))
	 (base (or base (plist-get plist 'base) ldap-default-base))
	 (scope (or scope (plist-get plist 'scope)))
	 (binddn (plist-get plist 'binddn))
	 (passwd (plist-get plist 'passwd))
	 (deref (plist-get plist 'deref))
	 (timelimit (plist-get plist 'timelimit))
	 (sizelimit (plist-get plist 'sizelimit))
	 start value attrs-result
	 (i 0)
	 result arglist ret)
    (setq arglist (list (format "-h%s" (ldap-host ldap))))
    (if (and port (not (equal 389 port)))
	(setq arglist (nconc arglist (list (format "-p%d" port)))))
    (if (and base
	     (not (equal "" base)))
	(setq arglist (nconc arglist (list (format "-b%s" base)))))
    (if (and scope
	     (not (equal "" scope)))
	(setq
	 arglist
	 (nconc
	  arglist
	  (list (format "-s%s"
			(cond ((eq scope 'onelevel) "one")
			      ((eq scope 'base) "base")
			      ((eq scope 'subtree) "sub")
			      ((null scope) "sub")
			      (t (error "Invalid scope: %s" scope))))))))
    (if (and binddn
	     (not (equal "" binddn)))
	(setq arglist (nconc arglist (list (format "-D%s" binddn)))))
    (if (and passwd
	     (not (equal "" passwd)))
	(setq arglist (nconc arglist (list (format "-w%s" passwd)))))
    (if (and deref
	     (not (equal "" deref)))
	(setq arglist (nconc arglist (list (format "-a%s" deref)))))
    (if (and timelimit
	     (not (equal "" timelimit)))
	(setq arglist (nconc arglist (list (format "-l%s" timelimit)))))
    (if (and sizelimit
	     (not (equal "" sizelimit)))
	(setq arglist (nconc arglist (list (format "-z%s" sizelimit)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq ret (apply 'call-process
		       ldap-search-program
		       nil (current-buffer) t
		       (append arglist
			       ldap-search-program-arguments
			       (list filter)
			       attrs)))
      (if (and (integerp ret)
	       (not (zerop ret))
	       ;; When openldap's `ldapsearch' exceeds response size limit,
	       ;; it's exit status becomes `4'.
               (/= ret 4)
	       ;; When openldap's `ldapsearch' uses referral,
	       ;; it's exit status becomes `32'.
	       (/= ret 32))
	  (error "LDAP error: \"No such object\""))
      (goto-char (point-min))
      (setq start (point))
      (while (and (not (eobp))
		  (re-search-forward "^$" nil t)) ; empty line is a delimiter.
	(if verbose
	    (message "Parsing ldap results...%d" (setq i (+ i 1))))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (if attrs
		(setq attrs-result (delq
				    nil
				    (mapcar
				     (lambda (attr)
				       ;; dn is not an attribute.
				       (unless (string= attr "dn")
					 (if (setq value
						   (ldap/field-body attr))
					     (if attrsonly
						 (list attr)
					       (nconc (list attr) value)))))
				     attrs)))
	      (setq attrs-result (ldap/collect-field "dn"))
	      (if attrsonly
		  (setq attrs-result (mapcar (lambda (x) (list (car x)))
					     attrs-result))))
	    (setq result
		  (cons
		   (if withdn
		       (if attrs-result
			   (nconc (ldap/field-body "dn") attrs-result)
			 (ldap/field-body "dn"))
		     attrs-result)
		   result))))
	(if (not (eobp)) (forward-char))
	(setq start (point)))
      (if verbose
	  (message "Parsing ldap results...done"))
      (delq nil (nreverse result)))))

(defun ldap/field-end ()
  "Move to end of field and return this point."
  (if (re-search-forward ldap-ldif-next-field-head-regexp nil t)
      (goto-char (match-beginning 0))
    (if (re-search-forward "^$" nil t)
	(goto-char (1- (match-beginning 0)))
      (end-of-line)))
  (point))

(defun ldap/field-body (name)
  "Return field body list of NAME."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (field-body nil)
	  body)
      ;; search for the line which have name with options.
      (while (re-search-forward (concat "^" name
					"\\(;[a-zA-Z0-9-]+\\)?:[ \t]*") nil t)
	;; Base64
	(if (string-match "^:[ \t]*" (setq body
					   (buffer-substring-no-properties
					    (match-end 0)
					    (ldap/field-end))))
	    (setq body (base64-decode-string (substring body (match-end 0)))))
	(setq field-body (nconc field-body (list body))))
      field-body)))

(defun ldap/collect-field (without)
  "Collect fields without WITHOUT."
  (goto-char (point-min))
  (let ((regexp (concat "\\(" ldap-ldif-field-head-regexp "\\)[ \t]*"))
	dest name name-option body entry)
    (while (re-search-forward regexp nil t)
      ;; name with options.
      (setq name-option (split-string (downcase (buffer-substring-no-properties
						 (match-beginning 1)
						 (1- (match-end 1))))
				      ";"))
      ;; XXX options are discarded.
      (setq name (car name-option))
      (setq body (buffer-substring-no-properties
		  (match-end 0) (ldap/field-end)))
      (if (string-match "^:[ \t]*" body)
	  (setq body (base64-decode-string (substring body (match-end 0)))))
      (unless (string= name without)
	(if (setq entry (assoc name dest))
	    (nconc entry (list body))
	  (setq dest (cons (list name body) dest)))))
    (nreverse dest)))

;;; Coding/decoding functions
;;
(defun ldap-encode-boolean (bool)
  "Encode BOOL to LDAP type."
  (if bool
      "TRUE"
    "FALSE"))

(defun ldap-decode-boolean (str)
  "Decode STR to elisp type."
  (cond
   ((string-equal str "TRUE")
    t)
   ((string-equal str "FALSE")
    nil)
   (t
    (error "Wrong LDAP boolean string: %s" str))))

(defun ldap-encode-country-string (str)
  "Encode STR to LDAP country string."
  ;; We should do something useful here...
  (if (not (= 2 (length str)))
      (error "Invalid country string: %s" str)))

(defun ldap-decode-string (str)
  "Decode LDAP STR."
  (if (and (fboundp 'decode-coding-string)
	   ldap-coding-system)
      (decode-coding-string str ldap-coding-system)
    str))

(defun ldap-encode-string (str)
  "Encode LDAP STR."
  (if (and (fboundp 'encode-coding-string)
	   ldap-coding-system)
      (encode-coding-string str ldap-coding-system)
    str))

(defun ldap-decode-address (str)
  "Decode LDAP address STR."
  (mapconcat 'ldap-decode-string
	     (split-string str "\\$")
	     "\n"))

(defun ldap-encode-address (str)
  "Encode address STR to LDAP type."
  (mapconcat 'ldap-encode-string
	     (split-string str "\n")
	     "$"))

;;; LDAP protocol functions
;;
(defun ldap-get-host-parameter (host parameter)
  "Get HOST's PARAMETER in `ldap-host-parameters-alist'."
  (plist-get (cdr (assoc host ldap-host-parameters-alist))
	     parameter))

(defun ldap-encode-attribute (attr)
  "Encode the attribute/value pair ATTR according to LDAP rules.
The attribute name is looked up in `ldap-attribute-syntaxes-alist'
and the corresponding decoder is then retrieved from
`ldap-attribute-syntax-encoders' and applied on the value(s)."
  (let* ((name (car attr))
	 (values (cdr attr))
	 (syntax-id (cdr (assq (intern (downcase name))
			       ldap-attribute-syntaxes-alist)))
	 encoder)
    (if syntax-id
	(setq encoder (aref ldap-attribute-syntax-encoders
			    (1- syntax-id)))
      (setq encoder ldap-default-attribute-encoder))
    (if encoder
	(cons name (mapcar encoder values))
      attr)))

(defun ldap-decode-attribute (attr)
  "Decode the attribute/value pair ATTR according to LDAP rules.
The attribute name is looked up in `ldap-attribute-syntaxes-alist'
and the corresponding decoder is then retrieved from
`ldap-attribute-syntax-decoders' and applied on the value(s)."
  (if (consp attr)
      (let* ((name (car attr))
	     (values (cdr attr))
	     (syntax-id (cdr (assq (intern (downcase name))
				   ldap-attribute-syntaxes-alist)))
	     decoder)
	(if syntax-id
	    (setq decoder (aref ldap-attribute-syntax-decoders
				(1- syntax-id)))
	  (setq decoder ldap-default-attribute-decoder))
	(if decoder
	    (cons name (mapcar decoder values))
	  attr))
    attr))

(defun ldap-search (arg1 &rest args)
  "Perform an LDAP search.if ARG1 is LDAP object, invoke `ldap-search-basic'.
Otherwise, invoke `ldap-search-entries'.  ARGS are passed to each function."
      (apply (if (ldapp arg1)
		 'ldap-search-basic
	       'ldap-search-entries) arg1 args))

(make-obsolete 'ldap-search
	       "Use `ldap-search-entries' instead or
`ldap-search-basic' for the low-level search API.")

(defun ldap-search-entries (filter &optional host attributes attrsonly withdn)
  "Perform an LDAP search.
FILTER is the search filter in RFC1558 syntax, i.e., something that
looks like \"(cn=John Smith)\".
HOST is the LDAP host on which to perform the search.
ATTRIBUTES is a list of attributes to retrieve; nil means retrieve all.
If ATTRSONLY is non nil, the attributes will be retrieved without
the associated values.
If WITHDN is non-nil each entry in the result will be prepennded with
its distinguished name DN.
Additional search parameters can be specified through
`ldap-host-parameters-alist' which see.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs optionally preceded by the DN of the
entry according to the value of WITHDN."
  (interactive "sFilter:")
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	result)
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Searching with LDAP on %s..." host))
    (setq result (ldap-search ldap (ldap-encode-string filter)
			      (plist-get host-plist 'base)
			      (plist-get host-plist 'scope)
			      attributes attrsonly withdn
			      ldap-verbose))
    (ldap-close ldap)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (if ldap-ignore-attribute-codings
	  result
	(mapcar
	 (lambda (record)
	   (mapcar 'ldap-decode-attribute record))
	 result)))))

(defun ldap-add-entries (entries &optional host binddn passwd)
  "Add entries to an LDAP directory.
ENTRIES is a list of entry specifications of
the form (DN (ATTR . VALUE) (ATTR . VALUE) ...) where
DN is the distinguished name of an entry to add, the following
are cons cells containing attribute/value string pairs.
HOST is the LDAP host, defaulting to `ldap-default-host'
BINDDN is the DN to bind as to the server
PASSWD is the corresponding password"
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	(i 1))
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Adding LDAP entries..."))
    (mapc (lambda (thisentry)
	    (setcdr thisentry
		    (mapcar
		     (lambda (add-spec)
		       (setq add-spec (ldap-encode-attribute
				       (list (car add-spec)
					     (cdr add-spec))))
		       (cons (nth 0 add-spec)
			     (nth 1 add-spec)))
		     (cdr thisentry)))
	    (setq thisentry (ldap-encode-attribute thisentry))
	    (ldap-add ldap (car thisentry) (cdr thisentry))
	    (if ldap-verbose
		(message "%d added" i))
	    (setq i (1+ i)))
	  entries)
    (ldap-close ldap)))

(defun ldap-modify-entries (entry-mods &optional host binddn passwd)
  "Modify entries of an LDAP directory.
ENTRY-MODS is a list of entry modifications of the form
  \(DN MOD-SPEC1 MOD-SPEC2 ...\) where DN is the distinguished name of
the entry to modify, the following are modification specifications.
A modification specification is itself a list of the form
\(MOD-OP ATTR VALUE1 VALUE2 ...\) MOD-OP and ATTR are mandatory,
VALUEs are optional depending on MOD-OP.
MOD-OP is the type of modification, one of the symbols `add', `delete'
or `replace'.  ATTR is the LDAP attribute type to modify.
HOST is the LDAP host, defaulting to `ldap-default-host'
BINDDN is the DN to bind as to the server
PASSWD is the corresponding password"
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	(i 1))
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Modifying LDAP entries..."))
    (mapc
     (lambda (thisentry)
       (setcdr thisentry
	       (mapcar
		(lambda (mod-spec)
		  (if (or (eq (car mod-spec) 'add)
			  (eq (car mod-spec) 'replace))
		      (append (list (nth 0 mod-spec))
			      (ldap-encode-attribute
			       (cdr mod-spec)))))
		(cdr thisentry)))
       (ldap-modify ldap (car thisentry) (cdr thisentry))
       (if ldap-verbose
	   (message "%d modified" i))
       (setq i (1+ i)))
     entry-mods)
    (ldap-close ldap)))

(defun ldap-delete-entries (dn &optional host binddn passwd)
  "Delete an entry from an LDAP directory.
DN is the distinguished name of an entry to delete or
a list of those.
HOST is the LDAP host, defaulting to `ldap-default-host'
BINDDN is the DN to bind as to the server
PASSWD is the corresponding password."
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap)
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if (consp dn)
	(let ((i 1))
	  (if ldap-verbose
	      (message "Deleting LDAP entries..."))
	  (mapc
	   (lambda (thisdn)
	     (ldap-delete ldap thisdn)
	     (if ldap-verbose
		 (message "%d deleted" i))
	     (setq i (1+ i)))
	   dn))
      (if ldap-verbose
	  (message "Deleting LDAP entry..."))
      (ldap-delete ldap dn))
    (ldap-close ldap)))
;; end of ldap-static-if
)

(provide 'pldap)

;;; pldap.el ends here
