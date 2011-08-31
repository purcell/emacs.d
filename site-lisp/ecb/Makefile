# This Makefile byte-compiles the ECB lisp files and generates online-help.

# Copyright (C) 2000 - 2005 Jesper Nordenberg,
#                           Klaus Berndl,
#                           Kevin A. Burton,
#                           Free Software Foundation, Inc.

# Author: Jesper Nordenberg <mayhem@home.se>
#         Klaus Berndl <klaus.berndl@sdm.de>
#         Kevin A. Burton <burton@openprivacy.org>
# Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
#             Kevin A. Burton <burton@openprivacy.org>
# Keywords: browser, code, programming, tools
# Created: 2001

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

# $Id: Makefile,v 1.114 2009/05/15 15:19:53 berndl Exp $


# ========================================================================
# User configurable section

# ------------------------------------------------------------------------
# Byte-compiling ECB:
# ------------------------------------------------------------------------

# Define here the correct path to your Emacs or XEmacs binary. Ensure you
# have set this variable to 'xemacs' if you want byte-compile with XEmacs!
EMACS=emacs

# In the following path-settings of this section use always FORWARD-SLASHES
# as directory-separator even with MS Windows systems.

# -------- Compiling ECB with the cedet-library 1.0 ----------------------

# cedet >= 1.0 (contains a.o. semantic >= 2.0, eieio >= 0.18 and speedbar
# >= 0.15). Set here the full path to the cedet-installation directory.

CEDET=~/.emacs.d/site-lisp/cedet

# You can set here more load-paths to arbitrary packages if you want. But
# this is really not necessary!
LOADPATH=

# Two ways to build ECB:
# - Call "make" to byte-compile the ECB. You can savely ignore the messages.
# - Or call
#
#      make EMACS="path/to/emacs"
#
#      or
#
#      make CEDET="path/to/cedet"
#
#   if you want to set either different load-paths or Emacs-binary and
#   you do not want edit the Makefile. Do not forget quoting the arguments
#   if they contain spaces!
#
# If there are any warning messages during byte-compilation (normally there
# are not any) you can savely ignore them!


# ------------------------------------------------------------------------
# Generating different online-help formats
# ------------------------------------------------------------------------

# If you want to generate all formats of online-help from the texi-source
# you must set here the FULL paths to the required tools. The Makefile
# tests if the tools are available on these locations, so if a tool x is
# not available let the related setting X empty! NOTE: For generating the
# PDF-format you will need an installed TeX and Ghostscript!
MAKEINFO=/usr/bin/makeinfo
TEXI2DVI=/C/Programme/texmf/miktex/bin/texi2dvi
# You need either the dvipdfm-tool
#DVIPDFM=/C/Programme/texmf/miktex/bin/dvipdfm
DVIPDFM=
# or the tools dvips and ps2pdf. If dvipdfm is available the Makefile uses
# this one!
DVIPS=/C/Programme/texmf/miktex/bin/dvips
PS2PDF=/C/home/bin/ps2pdf

# To generate the online-formats just call "make online-help" for info- and
# HTML-format and "make pdf" for PDF-format.

# ------------------------------------------------------------------------
# Installing the info online-help in the Top-directory of (X)Emacs-info
# ------------------------------------------------------------------------

# Set here the path of the info subdirectory of your (X)Emacs installation
# which contains the dir file.
EMACSINFOPATH=/C/Programme/emacs-22.3/info

# If you want to install the info-format of the online-help in the
# Top-directory of the info-directory of (X)Emacs (see above EMACSINFOPATH)
# then you must specify the full path of the tool install-info.
INSTALLINFO=/usr/bin/install-info

# To install the online-help just call "make install-help"

# end of user configurable section
# ========================================================================


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!! Do not change anything below !!!!!!!!!!!!!!!!!!!!!
# ========================================================================


# $Id: Makefile,v 1.114 2009/05/15 15:19:53 berndl Exp $

# For the ECB-maintainers: Change the version-number here and not
# elsewhere!
ecb_VERSION=2.40
#ecb_VERSION=2.32

include ecb-makedef.mk

ecb: $(ecb_LISP_EL)
	@echo "Byte-compiling ECB with LOADPATH=${LOADPATH} ..."
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script
	@if test ! -z "${CEDET}"; then\
	   echo "(load-file \"$(CEDET)/common/cedet.el\")" >> ecb-compile-script; \
	fi
	@if test ! -z "${LOADPATH}"; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> ecb-compile-script; \
	   done; \
	fi
	@echo "(require 'ecb)" >> ecb-compile-script
	@echo "(setq debug-on-error t)" >> ecb-compile-script
	$(EBATCH) -l ecb-compile-script --eval '(ecb-byte-compile t)'
	@$(RM) ecb-compile-script

all: ecb online-help

online-help: $(ecb_TEXI)
	@if test -x "$(MAKEINFO)"; then\
	   $(RM) -R $(ecb_INFO_DIR) $(ecb_HTML_DIR); \
	   $(MKDIR) $(ecb_INFO_DIR) $(ecb_HTML_DIR); \
	   echo Generating info-format...; \
	   $(MAKEINFO) --fill-column=78 $<; \
	   $(MV) *.info* $(ecb_INFO_DIR); \
	   echo Generating html-format...; \
	   $(MAKEINFO) --html --output=$(ecb_HTML_DIR) $<; \
	   for file in $(ecb_HTML_DIR)/*.html; do\
	      $(MV) $$file tmpfile; \
	      sed "s/index\\.html/$(ecb_HTML)/g" tmpfile > $$file; \
	      $(RM) tmpfile; \
	   done; \
	   $(MV) $(ecb_HTML_DIR)/index.html $(ecb_HTML_DIR)/$(ecb_HTML); \
	else \
	   echo No info- and html-format generating because the tool; \
	   echo - makeinfo in $(MAKEINFO); \
	   echo is not available!; \
	fi

pdf: $(ecb_TEXI)
	@if test -x "$(TEXI2DVI)" -a -x "$(DVIPDFM)"; then\
	   $(RM) $(ecb_DVI) $(ecb_PDF); \
	   echo Generating pdf-format with dvipdfm ...; \
	   $(TEXI2DVI) --clean $<; \
	   $(DVIPDFM) $(ecb_DVI); \
	   $(RM) $(ecb_DVI); \
	elif test -x "$(TEXI2DVI)" -a -x "$(DVIPS)" -a -x "$(PS2PDF)"; then\
	   $(RM) $(ecb_DVI) $(ecb_PS) $(ecb_PDF); \
	   echo Generating pdf-format with dvips and ps2pdf ...; \
	   $(TEXI2DVI) --quiet --clean $<; \
	   $(DVIPS) -Pcmz -q $(ecb_DVI) -o $(ecb_PS); \
	   $(PS2PDF) $(ecb_PS); \
	   $(RM) $(ecb_DVI) $(ecb_PS); \
	else \
	   echo No pdf-format generating because at least one of the tools; \
	   echo - texi2dvi in $(TEXI2DVI); \
	   echo - dvips in $(DVIPS); \
	   echo - ps2pdf in $(PS2PDF); \
	   echo is not available!; \
	fi


install-help: $(ecb_INFO_DIR)/$(ecb_INFO)
	@if test -x "$(INSTALLINFO)" -a -f "$(EMACSINFOPATH)/dir"; then\
	   echo Installing the Online-help in $(EMACSINFOPATH)...; \
	   $(CP) $(ecb_INFO_DIR)/*info* $(EMACSINFOPATH); \
	   $(INSTALLINFO) $< $(EMACSINFOPATH)/dir; \
	else \
	   echo Can not install the online-help because either; \
	   echo - the tool $(INSTALLINFO) or; \
	   echo - the file $(EMACSINFOPATH)/dir; \
	   echo is not available!; \
	fi


clean:
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script

# The targets below are only for maintaining the ECB-package.

$(ecb_INFO_DIR)/$(ecb_INFO): online-help

# updates RELEASE_NOTES, README, NEWS, ecb.texi and ecb.el to the
# version-number of $(ecb_VERSION).
prepversion:
	@$(MV) RELEASE_NOTES RELEASE_NOTES.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" RELEASE_NOTES.tmp > RELEASE_NOTES
	@$(RM) RELEASE_NOTES.tmp
	@$(MV) README README.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" README.tmp > README
	@$(RM) README.tmp
	@$(MV) NEWS NEWS.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" NEWS.tmp > NEWS
	@$(RM) NEWS.tmp
	@$(MV) ecb-upgrade.el ecb-upgrade.el.tmp
	@sed "s/^(defconst ecb-version.*/(defconst ecb-version \"$(ecb_VERSION)\"/" ecb-upgrade.el.tmp > ecb-upgrade.el
	@$(RM) ecb-upgrade.el.tmp
	@(echo "/@macro ecbver";		\
	  echo "+";				\
	  echo "c";				\
	  echo "$(ecb_VERSION)";		\
	  echo ".";				\
	  echo "w";				\
	  echo "q") | ed -s $(ecb_TEXI) 1> /dev/null


autoloads:
	@$(RM) $(ecb_AUTOLOADS) $(ecb_AUTOLOADS)c
	$(EBATCH) -l ecb-autogen -f ecb-update-autoloads


# builds the distribution file $(ecb_VERSION).tar.gz
distrib: $(ecb_INFO_DIR)/$(ecb_INFO) prepversion autoloads ecb
	@$(RM) ecb-$(ecb_VERSION).tar.gz
	@$(RM) -R ecb-$(ecb_VERSION)
	@$(MKDIR) ecb-$(ecb_VERSION)
	@$(CP) $(ecb_DISTRIB_FILES) ecb-$(ecb_VERSION)
	@$(CP) -r $(ecb_INFO_DIR) ecb-$(ecb_VERSION)
	@$(CP) -r $(ecb_HTML_DIR) ecb-$(ecb_VERSION)
	@$(CP) -r $(ecb_IMAGE_DIR) ecb-$(ecb_VERSION)
	@find ecb-$(ecb_VERSION)/$(ecb_IMAGE_DIR) -name CVS -print | xargs rm -Rf
	@find ecb-$(ecb_VERSION)/$(ecb_IMAGE_DIR) -name *~ -print | xargs $(RM)
	@find ecb-$(ecb_VERSION)/$(ecb_IMAGE_DIR) -name *.png -print | xargs $(RM)
	@tar -cvzf ecb-$(ecb_VERSION).tar.gz ecb-$(ecb_VERSION)
	@$(RM) -R ecb-$(ecb_VERSION)

# End of Makefile
