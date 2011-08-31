## Makefile --- Definition file for building CEDET
##
## Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009 by David Ponce
##
## Author: David Ponce <david@dponce.com>
## Maintainer: CEDET developers <http://sf.net/projects/cedet>
## Created: 12 Sep 2003
## X-RCS: $Id: Makefile,v 1.21 2009/02/24 03:17:19 zappo Exp $
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.

######## You can customize this part of the Makefile ########

## The directory where CEDET is installed
CEDET_HOME="$(CURDIR)"

## The CEDET's packages installed
CEDET_PACKAGES=\
common \
speedbar \
eieio \
semantic \
srecode \
ede \
cogre \
contrib \
tests

## Path to your Emacs
EMACS=emacs
EMACSFLAGS=-batch --no-site-file

## Your shell (On Windows/Cygwin I recommend to use bash)
#SHELL=bash

## Path to your find and rm commands
FIND=find
#RM = rm -f

## INSTALL PATHS
PREFIX=/usr/local

INFO_DIR=$(PREFIX)/share/info

INSTALL_INFO=ginstall-info

############### Internal part of the Makefile ###############
CEDET_VERSION=$(shell grep "defconst cedet-version" common/cedet.el | cut -d " " -f 3)

CEDET_FILES=Makefile INSTALL cedet-build.el cedet-update-version.el PRERELEASE_CHECKLIST USING_CEDET_FROM_CVS
DIST_ROOT=cedet-$(CEDET_VERSION)
DIST_DIR=$(CEDET_HOME)/$(DIST_ROOT)
DIST_FILE=$(DIST_DIR).tar.gz

__BUILD_AUTOLOADS=$(patsubst %,%-autoloads,$(CEDET_PACKAGES))
__CLEAN_AUTOLOADS=$(patsubst %,clean-%,$(__BUILD_AUTOLOADS))
__DOMAKE=$(MAKE) $(MFLAGS) EMACS="$(EMACS)" EMACSFLAGS="$(EMACSFLAGS)" SHELL="$(SHELL)"

## Build
##

all: clean-autoloads packages

bootstrap: clean-all packages

packages: $(CEDET_PACKAGES)

.PHONY: $(CEDET_PACKAGES)
$(CEDET_PACKAGES):
	cd $(CEDET_HOME)/$@ && $(__DOMAKE)

## Update
##

autoloads: $(__BUILD_AUTOLOADS)

.PHONY: $(__BUILD_AUTOLOADS)
$(__BUILD_AUTOLOADS):
	cd $(CEDET_HOME)/$(firstword $(subst -, ,$@)) && \
	$(__DOMAKE) autoloads

recompile: autoloads
	cd $(CEDET_HOME) && \
	"$(EMACS)" $(EMACSFLAGS) -l common/cedet.el \
	-f batch-byte-recompile-directory $(CEDET_PACKAGES)

## Cleanup
##

clean-autoloads: $(__CLEAN_AUTOLOADS)

.PHONY: $(__CLEAN_AUTOLOADS)
$(__CLEAN_AUTOLOADS):
	$(FIND) $(CEDET_HOME)/$(word 2,$(subst -, ,$@)) -type f \
	-name "*-loaddefs.el" \
	-print -exec $(RM) {} \;

.PHONY: clean-grammars
clean-grammars:
	$(FIND) $(CEDET_HOME) -type f -name "*-[bw]y.el" \
	! -name "semantic-grammar-wy.el" \
	-print -exec $(RM) {} \;

.PHONY: clean-info
clean-info:
	$(FIND) $(CEDET_HOME) -type f -name "*.info*" \
	-print -exec $(RM) {} \;

.PHONY: clean-elc
clean-elc:
	$(FIND) $(CEDET_HOME) -type f -name "*.elc" \
	-print -exec $(RM) {} \;

.PHONY: clean
clean:
	$(FIND) $(CEDET_HOME) -type f \( -name "*-script" -o -name "*~" \) \
	-print -exec $(RM) {} \;

clean-all: clean clean-elc clean-info clean-grammars clean-autoloads

### UNIT TEST Harness
## Run the master CEDET unit-test suite.
.PHONY: utest
utest:
	$(EMACS) $(EMACSFLAGS) -l "common/cedet.el" -f cedet-utest-batch

### Install info files
## Thanks Stefano Sabatini for the info install patch.
INFO_FILES=$(shell $(FIND) $(CEDET_HOME) -type f -name '*.info')

.PHONY: install-info
install-info:
	for file in $(INFO_FILES); do \
	    cp $$file $(INFO_DIR); \
	    $(INSTALL_INFO) $$file $(INFO_DIR)/dir ;\
	done 

## Uninstall info files 
INSTALLED_INFO_FILES=$(shell find . -name *.info | sed -e 's|.*/\(.*\.info$$\)|$(INFO_DIR)/\1|')

.PHONY: uninstall-info
uninstall-info:
	for file in $(INSTALLED_INFO_FILES); do \
	    $(INSTALL_INFO) --delete $$file $(INFO_DIR)/dir ;\
	    rm -f $$file;\
	done


## Build a distribution file.
dist: # $(CEDET_PACKAGES)
	rm -rf $(DIST_DIR)
	mkdir $(DIST_DIR)
	cp $(CEDET_FILES) $(DIST_DIR)
	for package in ${CEDET_PACKAGES}; do \
	   make -C $$package $(MFLAGS) DISTDIR=$(DIST_DIR)/$$package dist; \
	done;
	tar -cvzf $(DIST_FILE) $(DIST_ROOT)
	rm -rf $(DIST_DIR)

testvar:
	@echo "$(TESTVAR)=$($(TESTVAR))"

# Makefile ends here
