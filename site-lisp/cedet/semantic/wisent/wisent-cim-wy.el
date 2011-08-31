;;; wisent-cim-wy.el --- Generated parser support file

;; Copyright (C) 2002 David Ponce

;; Author: Mona <whisky@ubuntu.ubuntu-domain>
;; Created: 2011-05-20 16:20:50-0400
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-cim.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-cim-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst wisent-cim-wy--token-table
  (semantic-lex-make-type-table
   '(("tval"
      (HTEXTKONST))
     ("rval"
      (HREALKONST))
     ("ival"
      (HCHARACTERKONST)
      (HINTEGERKONST)
      (HBOOLEANKONST))
     ("ident"
      (HIDENTIFIER))
     ("<no-type>"
      (HDOTDOTDOT)
      (HEXP)
      (HINTDIV)
      (HDIV)
      (HMUL)
      (HSUB)
      (HADD)
      (HNER)
      (HEQR)
      (HENDPAR)
      (HBEGPAR)
      (HSTATEMENTSEPARATOR)
      (HLABELSEPARATOR)
      (HPAREXPSEPARATOR)
      (HASSIGNREF)
      (HASSIGNVALUE)
      (HWHILE)
      (HWHEN)
      (HVIRTUAL)
      (HVAR)
      (HVALUE)
      (HUNTIL)
      (HTO)
      (HTHIS)
      (HTHEN)
      (HTEXT)
      (HSWITCH)
      (HSTEP)
      (HSHORT)
      (HREF)
      (HREAL)
      (HREACTIVATE)
      (HQUA)
      (HPROTECTED)
      (HPROCEDURE)
      (HPRIOR)
      (HOTHERWISE)
      (HNOTEXT)
      (HNONE)
      (HNEW)
      (HNE)
      (HNAME)
      (HLT)
      (HLONG)
      (HLE)
      (HLABEL)
      (HIS)
      (HINTEGER)
      (HINSPECT)
      (HINNER)
      (HIN)
      (HIF)
      (HHIDDEN)
      (HGT)
      (HGOTO)
      (HGO)
      (HGE)
      (HFOR)
      (HEXTERNAL)
      (HEQ)
      (HEND)
      (HELSE)
      (HDO)
      (HDELAY)
      (HCONC)
      (HCLASS)
      (HCHARACTER)
      (HBOOLEAN)
      (HBEGIN)
      (HBEFORE)
      (HAT)
      (HARRAY)
      (HAFTER)
      (HACTIVATE))
     ("token"
      (HFACTOROPERATOR)
      (UNEAR)
      (HTERMOPERATOR)
      (HOBJRELOPERATOR)
      (HREFRELOPERATOR)
      (HVALRELOPERATOR)
      (HASSIGN)))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-cim-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((HACTIVATE HAFTER HARRAY HAT HBEFORE HBEGIN HBOOLEAN HCHARACTER HCLASS HCONC HDELAY HDO HELSE HEND HEQ HEXTERNAL HFOR HGE HGO HGOTO HGT HHIDDEN HIF HIN HINNER HINSPECT HINTEGER HIS HLABEL HLE HLONG HLT HNAME HNE HNEW HNONE HNOTEXT HOTHERWISE HPRIOR HPROCEDURE HPROTECTED HQUA HREACTIVATE HREAL HREF HSHORT HSTEP HSWITCH HTEXT HTHEN HTHIS HTO HUNTIL HVALUE HVAR HVIRTUAL HWHEN HWHILE HASSIGNVALUE HASSIGNREF HPAREXPSEPARATOR HLABELSEPARATOR HSTATEMENTSEPARATOR HBEGPAR HENDPAR HEQR HNER HADD HSUB HMUL HDIV HINTDIV HEXP HDOTDOTDOT HIDENTIFIER HBOOLEANKONST HINTEGERKONST HCHARACTERKONST HREALKONST HTEXTKONST)
       ((right HASSIGN)
	(left HORELSE)
	(left HANDTHEN)
	(left HEQV)
	(left HIMP)
	(left HOR)
	(left HAND)
	(left HNOT)
	(left HVALRELOPERATOR HREFRELOPERATOR HOBJRELOPERATOR)
	(left HCONC)
	(left HTERMOPERATOR)
	(left UNEAR)
	(left HFACTOROPERATOR)
	(left HPRIMARYOPERATOR)
	(left HQUA)
	(left HDOT))
       (MAIN_MODULE
	(((progn)
	  MODULS)
	 (progn))
	((error HSTATEMENTSEPARATOR MBEE_DECLSTMS)))
       (EXT_DECLARATION
	((HEXTERNAL MBEE_TYPE HPROCEDURE
		    (progn)
		    EXT_LIST))
	((HEXTERNAL HIDENTIFIER HPROCEDURE
		    (progn)
		    HIDENTIFIER
		    (progn)
		    EXTERNAL_KIND_ITEM)
	 (progn))
	((HEXTERNAL HCLASS
		    (progn)
		    EXT_LIST)))
       (EXTERNAL_KIND_ITEM
	((EXT_IDENT HOBJRELOPERATOR
		    (progn)
		    MBEE_TYPE HPROCEDURE HIDENTIFIER
		    (progn)
		    HEADING EMPTY_BLOCK)
	 (progn)))
       (EMPTY_BLOCK
	(nil)
	((HBEGIN HEND)))
       (EXT_LIST
	((EXT_ITEM))
	((EXT_LIST HPAREXPSEPARATOR EXT_ITEM)))
       (EXT_ITEM
	((HIDENTIFIER EXT_IDENT)
	 (progn)))
       (EXT_IDENT
	(nil
	 (progn))
	((HVALRELOPERATOR
	  (progn)
	  HTEXTKONST)
	 (progn)))
       (NO_TYPE
	(nil
	 (progn)))
       (MBEE_TYPE
	((NO_TYPE))
	((TYPE)))
       (TYPE
	((HREF HBEGPAR HIDENTIFIER
	       (progn)
	       HENDPAR))
	((HTEXT)
	 (progn))
	((HBOOLEAN)
	 (progn))
	((HCHARACTER)
	 (progn))
	((HSHORT HINTEGER)
	 (progn))
	((HINTEGER)
	 (progn))
	((HREAL)
	 (progn))
	((HLONG HREAL)
	 (progn)))
       (MBEE_ELSE_PART
	(nil)
	((HELSE
	  (progn)
	  BLOCK)
	 (progn)))
       (FOR_LIST
	((FOR_LIST_ELEMENT)
	 (progn))
	((FOR_LIST_ELEMENT HPAREXPSEPARATOR FOR_LIST)
	 (progn)))
       (FOR_LIST_ELEMENT
	((EXPRESSION MBEE_F_L_EL_R_PT)))
       (MBEE_F_L_EL_R_PT
	(nil)
	((HWHILE EXPRESSION)
	 (progn))
	((HSTEP EXPRESSION HUNTIL EXPRESSION)
	 (progn)))
       (GOTO
	((HGO HTO))
	((HGOTO)))
       (CONN_STATE_R_PT
	((WHEN_CLAUSE_LIST))
	((HDO
	  (progn)
	  BLOCK)
	 (progn)))
       (WHEN_CLAUSE_LIST
	((HWHEN HIDENTIFIER HDO
		(progn)
		BLOCK)
	 (progn))
	((WHEN_CLAUSE_LIST HWHEN HIDENTIFIER HDO
			   (progn)
			   BLOCK)
	 (progn)))
       (MBEE_OTWI_CLAUS
	(nil)
	((HOTHERWISE
	  (progn)
	  BLOCK)
	 (progn)))
       (ACTIVATOR
	((HACTIVATE)
	 (progn))
	((HREACTIVATE)
	 (progn)))
       (SCHEDULE
	(nil
	 (progn))
	((ATDELAY EXPRESSION
		  (progn)
		  PRIOR))
	((BEFOREAFTER
	  (progn)
	  EXPRESSION)
	 (progn)))
       (ATDELAY
	((HAT)
	 (progn))
	((HDELAY)
	 (progn)))
       (BEFOREAFTER
	((HBEFORE)
	 (progn))
	((HAFTER)
	 (progn)))
       (PRIOR
	(nil
	 (progn))
	((HPRIOR)
	 (progn)))
       (MODULSTATEMENT
	((HWHILE EXPRESSION HDO
		 (progn)
		 BLOCK)
	 (progn))
	((HIF EXPRESSION HTHEN
	      (progn)
	      BLOCK
	      (progn)
	      MBEE_ELSE_PART)
	 (progn))
	((HFOR HIDENTIFIER HASSIGN
	       (progn)
	       FOR_LIST HDO
	       (progn)
	       BLOCK)
	 (progn))
	((GOTO EXPRESSION)
	 (progn))
	((HINSPECT EXPRESSION
		   (progn)
		   CONN_STATE_R_PT
		   (progn)
		   MBEE_OTWI_CLAUS)
	 (progn))
	((HINNER)
	 (progn))
	((HIDENTIFIER HLABELSEPARATOR
		      (progn)
		      DECLSTATEMENT)
	 (progn))
	((EXPRESSION_SIMP HBEGIN
			  (progn)
			  IMPORT_SPEC_MODULE
			  (progn)
			  MBEE_DECLSTMS HEND)
	 (progn))
	((EXPRESSION_SIMP HBEGIN error HSTATEMENTSEPARATOR MBEE_DECLSTMS HEND)
	 (progn))
	((EXPRESSION_SIMP HBEGIN error HEND)
	 (progn))
	((EXPRESSION_SIMP)
	 (progn))
	((ACTIVATOR EXPRESSION SCHEDULE)
	 (progn))
	((HBEGIN
	  (progn)
	  MBEE_DECLSTMS HEND)
	 (progn))
	((MBEE_TYPE HPROCEDURE HIDENTIFIER
		    (progn)
		    HEADING BLOCK)
	 (progn))
	((HIDENTIFIER HCLASS NO_TYPE
		      (progn)
		      IMPORT_SPEC_MODULE HIDENTIFIER
		      (progn)
		      HEADING BLOCK)
	 (progn))
	((HCLASS NO_TYPE HIDENTIFIER
		 (progn)
		 HEADING BLOCK)
	 (progn))
	((EXT_DECLARATION)
	 (progn))
	(nil
	 (progn)))
       (IMPORT_SPEC_MODULE
	(nil
	 (progn)))
       (DECLSTATEMENT
	((MODULSTATEMENT))
	((TYPE HIDENTIFIER MBEE_CONSTANT HPAREXPSEPARATOR
	       (progn)
	       IDENTIFIER_LISTC)
	 (progn))
	((TYPE HIDENTIFIER MBEE_CONSTANT)
	 (progn))
	((MBEE_TYPE HARRAY
		    (progn)
		    ARR_SEGMENT_LIST)
	 (progn))
	((HSWITCH HIDENTIFIER HASSIGN
		  (progn)
		  SWITCH_LIST)
	 (progn)))
       (BLOCK
	((DECLSTATEMENT)
	 (progn))
	((HBEGIN MBEE_DECLSTMS HEND))
	((HBEGIN error HSTATEMENTSEPARATOR MBEE_DECLSTMS HEND))
	((HBEGIN error HEND)))
       (MBEE_DECLSTMS
	((MBEE_DECLSTMSU)
	 (progn)))
       (MBEE_DECLSTMSU
	((DECLSTATEMENT)
	 (progn))
	((MBEE_DECLSTMSU HSTATEMENTSEPARATOR DECLSTATEMENT)
	 (progn)))
       (MODULS
	((MODULSTATEMENT)
	 (progn))
	((MODULS HSTATEMENTSEPARATOR MODULSTATEMENT)
	 (progn)))
       (ARR_SEGMENT_LIST
	((ARR_SEGMENT))
	((ARR_SEGMENT_LIST HPAREXPSEPARATOR ARR_SEGMENT)))
       (ARR_SEGMENT
	((ARRAY_SEGMENT HBEGPAR BAUND_PAIR_LIST HENDPAR)
	 (progn)))
       (ARRAY_SEGMENT
	((ARRAY_SEGMENT_EL)
	 (progn))
	((ARRAY_SEGMENT_EL HPAREXPSEPARATOR ARRAY_SEGMENT)
	 (progn)))
       (ARRAY_SEGMENT_EL
	((HIDENTIFIER)
	 (progn)))
       (BAUND_PAIR_LIST
	((BAUND_PAIR)
	 (progn))
	((BAUND_PAIR HPAREXPSEPARATOR BAUND_PAIR_LIST)
	 (progn)))
       (BAUND_PAIR
	((EXPRESSION HLABELSEPARATOR EXPRESSION)
	 (progn)))
       (SWITCH_LIST
	((EXPRESSION)
	 (progn))
	((EXPRESSION HPAREXPSEPARATOR SWITCH_LIST)
	 (progn)))
       (HEADING
	((MBEE_FMAL_PAR_P HSTATEMENTSEPARATOR
			  (progn)
			  MBEE_MODE_PART
			  (progn)
			  MBEE_SPEC_PART
			  (progn)
			  MBEE_PROT_PART
			  (progn)
			  MBEE_VIRT_PART)
	 (progn)))
       (MBEE_FMAL_PAR_P
	(nil)
	((FMAL_PAR_PART)))
       (FMAL_PAR_PART
	((HBEGPAR NO_TYPE MBEE_LISTV HENDPAR)))
       (MBEE_LISTV
	(nil)
	((LISTV)))
       (LISTV
	((HIDENTIFIER)
	 (progn))
	((FPP_CATEG HDOTDOTDOT)
	 (progn))
	((HIDENTIFIER
	  (progn)
	  HPAREXPSEPARATOR LISTV))
	((FPP_SPEC))
	((FPP_SPEC HPAREXPSEPARATOR LISTV)))
       (FPP_HEADING
	((HBEGPAR NO_TYPE FPP_MBEE_LISTV HENDPAR)))
       (FPP_MBEE_LISTV
	(nil)
	((FPP_LISTV)))
       (FPP_LISTV
	((FPP_CATEG HDOTDOTDOT)
	 (progn))
	((FPP_SPEC))
	((FPP_SPEC HPAREXPSEPARATOR LISTV)))
       (FPP_SPEC
	((FPP_CATEG SPECIFIER HIDENTIFIER)
	 (progn))
	((FPP_CATEG FPP_PROC_DECL_IN_SPEC)))
       (FPP_CATEG
	((HNAME HLABELSEPARATOR)
	 (progn))
	((HVALUE HLABELSEPARATOR)
	 (progn))
	((HVAR HLABELSEPARATOR)
	 (progn))
	(nil))
       (FPP_PROC_DECL_IN_SPEC
	((MBEE_TYPE HPROCEDURE HIDENTIFIER
		    (progn)
		    FPP_HEADING
		    (progn))
	 (progn)))
       (IDENTIFIER_LISTV
	((HIDENTIFIER)
	 (progn))
	((HDOTDOTDOT)
	 (progn))
	((HIDENTIFIER
	  (progn)
	  HPAREXPSEPARATOR IDENTIFIER_LISTV)))
       (MBEE_MODE_PART
	(nil)
	((MODE_PART)))
       (MODE_PART
	((NAME_PART))
	((VALUE_PART))
	((VAR_PART))
	((NAME_PART VALUE_PART))
	((VALUE_PART NAME_PART))
	((NAME_PART VAR_PART))
	((VAR_PART NAME_PART))
	((VALUE_PART VAR_PART))
	((VAR_PART VALUE_PART))
	((VAR_PART NAME_PART VALUE_PART))
	((NAME_PART VAR_PART VALUE_PART))
	((NAME_PART VALUE_PART VAR_PART))
	((VAR_PART VALUE_PART NAME_PART))
	((VALUE_PART VAR_PART NAME_PART))
	((VALUE_PART NAME_PART VAR_PART)))
       (NAME_PART
	((HNAME
	  (progn)
	  IDENTIFIER_LISTV HSTATEMENTSEPARATOR)))
       (VAR_PART
	((HVAR
	  (progn)
	  IDENTIFIER_LISTV HSTATEMENTSEPARATOR)))
       (VALUE_PART
	((HVALUE
	  (progn)
	  IDENTIFIER_LISTV HSTATEMENTSEPARATOR)))
       (MBEE_SPEC_PART
	(nil)
	((SPEC_PART)))
       (SPEC_PART
	((ONE_SPEC))
	((SPEC_PART ONE_SPEC)))
       (ONE_SPEC
	((SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR))
	((NO_TYPE HPROCEDURE HIDENTIFIER HOBJRELOPERATOR
		  (progn)
		  PROC_DECL_IN_SPEC HSTATEMENTSEPARATOR))
	((FPP_PROC_DECL_IN_SPEC HSTATEMENTSEPARATOR))
	((MBEE_TYPE HPROCEDURE HIDENTIFIER HSTATEMENTSEPARATOR)
	 (progn))
	((MBEE_TYPE HPROCEDURE HIDENTIFIER HPAREXPSEPARATOR IDENTIFIER_LIST HSTATEMENTSEPARATOR)
	 (progn)))
       (SPECIFIER
	((TYPE)
	 (progn))
	((MBEE_TYPE HARRAY)
	 (progn))
	((HLABEL)
	 (progn))
	((HSWITCH)
	 (progn)))
       (PROC_DECL_IN_SPEC
	((MBEE_TYPE HPROCEDURE HIDENTIFIER
		    (progn)
		    HEADING
		    (progn)
		    MBEE_BEGIN_END)
	 (progn)))
       (MBEE_BEGIN_END
	(nil)
	((HBEGIN HEND)))
       (MBEE_PROT_PART
	(nil)
	((PROTECTION_PART)))
       (PROTECTION_PART
	((PROT_SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR))
	((PROTECTION_PART PROT_SPECIFIER IDENTIFIER_LIST HSTATEMENTSEPARATOR)))
       (PROT_SPECIFIER
	((HHIDDEN)
	 (progn))
	((HPROTECTED)
	 (progn))
	((HHIDDEN HPROTECTED)
	 (progn))
	((HPROTECTED HHIDDEN)
	 (progn)))
       (MBEE_VIRT_PART
	(nil)
	((VIRTUAL_PART)))
       (VIRTUAL_PART
	((HVIRTUAL HLABELSEPARATOR MBEE_SPEC_PART)))
       (IDENTIFIER_LIST
	((HIDENTIFIER)
	 (progn))
	((IDENTIFIER_LIST HPAREXPSEPARATOR HIDENTIFIER)
	 (progn)))
       (IDENTIFIER_LISTC
	((HIDENTIFIER MBEE_CONSTANT)
	 (progn))
	((IDENTIFIER_LISTC HPAREXPSEPARATOR HIDENTIFIER MBEE_CONSTANT)
	 (progn)))
       (MBEE_CONSTANT
	(nil)
	((HVALRELOPERATOR
	  (progn)
	  EXPRESSION)
	 (progn)))
       (EXPRESSION
	((EXPRESSION_SIMP)
	 (progn))
	((HIF EXPRESSION HTHEN EXPRESSION HELSE EXPRESSION)
	 (progn)))
       (EXPRESSION_SIMP
	((EXPRESSION_SIMP HASSIGN EXPRESSION)
	 (progn))
	((EXPRESSION_SIMP HCONC EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HOR HELSE EXPRESSION_SIMP)
	 [HORELSE]
	 (progn))
	((EXPRESSION_SIMP HAND HTHEN EXPRESSION_SIMP)
	 [HANDTHEN]
	 (progn))
	((EXPRESSION_SIMP HEQV EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HIMP EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HOR EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HAND EXPRESSION_SIMP)
	 (progn))
	((HNOT EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HVALRELOPERATOR EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HREFRELOPERATOR EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HOBJRELOPERATOR EXPRESSION_SIMP)
	 (progn))
	((HTERMOPERATOR EXPRESSION_SIMP)
	 [UNEAR]
	 (progn))
	((EXPRESSION_SIMP HTERMOPERATOR EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HFACTOROPERATOR EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HPRIMARYOPERATOR EXPRESSION_SIMP)
	 (progn))
	((HBEGPAR EXPRESSION HENDPAR)
	 (progn))
	((HTEXTKONST)
	 (progn))
	((HCHARACTERKONST)
	 (progn))
	((HREALKONST)
	 (progn))
	((HINTEGERKONST)
	 (progn))
	((HBOOLEANKONST)
	 (progn))
	((HNONE)
	 (progn))
	((HIDENTIFIER
	  (progn)
	  MBEE_ARG_R_PT))
	((HTHIS HIDENTIFIER)
	 (progn))
	((HNEW HIDENTIFIER ARG_R_PT)
	 (progn))
	((EXPRESSION_SIMP HDOT EXPRESSION_SIMP)
	 (progn))
	((EXPRESSION_SIMP HQUA HIDENTIFIER)
	 (progn)))
       (ARG_R_PT
	(nil
	 (progn))
	((HBEGPAR ARGUMENT_LIST HENDPAR)))
       (MBEE_ARG_R_PT
	(nil
	 (progn))
	((HBEGPAR ARGUMENT_LIST HENDPAR)
	 (progn)))
       (ARGUMENT_LIST
	((EXPRESSION)
	 (progn))
	((EXPRESSION HPAREXPSEPARATOR ARGUMENT_LIST)
	 (progn))))
     '(MAIN_MODULE)))
  "Parser table.")

(defun wisent-cim-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-cim-wy--parse-table
	semantic-debug-parser-source "wisent-cim.wy"
	semantic-flex-keywords-obarray wisent-cim-wy--keyword-table
	semantic-lex-types-obarray wisent-cim-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;




(provide 'wisent-cim-wy)

;;; wisent-cim-wy.el ends here
