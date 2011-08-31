# This file defines some settings used for the ECB-Makefiles

# Copyright (C) 2000 - 2005 Klaus Berndl,
#                           Free Software Foundation, Inc.

# Author: Klaus Berndl <klaus.berndl@sdm.de>
# Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
# Keywords: browser, code, programming, tools
# Created: 2004

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# GNU Emacs; see the file COPYING.  If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# $Id: ecb-makedef.mk,v 1.9 2009/05/15 15:19:53 berndl Exp $

RM=rm -f
CP=cp
MV=mv -f
MKDIR=mkdir -p

EBATCH=$(EMACS) -batch -no-site-file

ecb_LISP_EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el \
            ecb-layout.el ecb-layout-defs.el ecb-navigate.el ecb.el \
            ecb-eshell.el ecb-cycle.el ecb-face.el ecb-compilation.el \
            ecb-upgrade.el ecb-create-layout.el silentcomp.el \
            ecb-speedbar.el ecb-examples.el ecb-tod.el ecb-autogen.el \
	    ecb-jde.el ecb-file-browser.el ecb-method-browser.el \
	    ecb-winman-support.el ecb-cedet-wrapper.el \
	    ecb-compatibility.el ecb-common-browser.el ecb-analyse.el \
	    ecb-symboldef.el ecb-advice-test.el

ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)

ecb_AUTOLOADS=ecb-autoloads.el

ecb_ETC=NEWS README RELEASE_NOTES ecb-makedef.mk Makefile make.bat

ecb_TEXI=ecb.texi

ecb_INFO=$(ecb_TEXI:.texi=.info)
ecb_HTML=$(ecb_TEXI:.texi=.html)
ecb_HTML_DIR=html-help
ecb_INFO_DIR=info-help

ecb_DVI=$(ecb_TEXI:.texi=.dvi)
ecb_PS=$(ecb_TEXI:.texi=.ps)
ecb_PDF=$(ecb_TEXI:.texi=.pdf)

ecb_IMAGE_DIR=ecb-images

ecb_DISTRIB_FILES=$(ecb_LISP_EL) $(ecb_AUTOLOADS) $(ecb_TEXI) $(ecb_ETC)

