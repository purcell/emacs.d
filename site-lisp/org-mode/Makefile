# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#
# To install org-mode, edit the Makefile, type `make', then `make install'.
# To create the PDF and HTML documentation files, type `make doc'.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS=emacs

# Where local software is found
prefix=/usr/local

# Where local lisp files go
lispdir   = $(prefix)/share/emacs/site-lisp

# Where data files go
# $(datadir) contains auxiliary files for use with ODT exporter.
# See comments under DATAFILES.
datadir = $(prefix)/share/emacs/etc

# Where info files go
infodir = $(prefix)/share/info

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Using emacs in batch mode.

BATCH=$(EMACS) -batch -q -no-site-file -eval                             			\
  "(setq load-path (cons (expand-file-name \"./lisp/\") (cons \"$(lispdir)\" load-path)))" $(BATCH_EXTRA)

# Specify the byte-compiler for compiling org-mode files
ELC= $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf

# How to create directories
MKDIR = mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections
TEXI2HTMLNOPSLIT = makeinfo --html --no-split --number-sections

# How to copy the lisp files and elc files to their distination.
CP = cp -pr

# Name of the program to install info files
INSTALL_INFO=install-info

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPF      = 	org.el			\
		org-agenda.el		\
		org-ascii.el		\
	     	org-attach.el		\
	     	org-archive.el		\
		org-bbdb.el		\
		org-beamer.el		\
		org-bibtex.el		\
	     	org-capture.el		\
	     	org-clock.el		\
	     	org-colview.el		\
	     	org-colview-xemacs.el	\
	     	org-compat.el		\
	     	org-pcomplete.el	\
	     	org-crypt.el		\
	     	org-ctags.el		\
	     	org-datetree.el		\
	     	org-docview.el		\
	     	org-entities.el		\
		org-exp.el		\
		org-exp-blocks.el	\
		org-docbook.el		\
		org-faces.el		\
		org-feed.el		\
		org-footnote.el		\
		org-freemind.el		\
		org-gnus.el		\
		org-eshell.el		\
		org-habit.el		\
		org-html.el		\
		org-icalendar.el	\
		org-id.el		\
		org-indent.el		\
		org-info.el		\
		org-inlinetask.el	\
		org-jsinfo.el		\
		org-irc.el		\
		org-latex.el		\
		org-list.el		\
		org-lparse.el		\
		org-mac-message.el	\
	     	org-macs.el		\
		org-mew.el              \
		org-mhe.el		\
		org-mks.el		\
		org-mobile.el		\
		org-mouse.el		\
		org-odt.el		\
		org-publish.el		\
		org-plot.el		\
		org-protocol.el		\
		org-remember.el		\
		org-rmail.el		\
		org-special-blocks.el	\
		org-src.el		\
		org-table.el		\
		org-taskjuggler.el	\
		org-timer.el		\
		org-vm.el		\
		org-w3m.el              \
		org-wl.el		\
		org-xoxo.el		\
		ob.el			\
		ob-table.el		\
		ob-lob.el		\
		ob-ref.el		\
		ob-exp.el		\
		ob-tangle.el		\
		ob-comint.el		\
		ob-eval.el		\
		ob-keys.el		\
		ob-awk.el		\
		ob-C.el			\
		ob-calc.el		\
		ob-ditaa.el		\
		ob-haskell.el		\
		ob-perl.el		\
		ob-sh.el		\
		ob-R.el			\
		ob-dot.el		\
		ob-mscgen.el		\
		ob-latex.el		\
		ob-lisp.el		\
		ob-ledger.el		\
		ob-python.el		\
		ob-sql.el		\
		ob-asymptote.el		\
		ob-emacs-lisp.el	\
		ob-matlab.el		\
		ob-ruby.el		\
		ob-sqlite.el		\
		ob-clojure.el		\
		ob-ocaml.el		\
		ob-sass.el		\
		ob-css.el		\
		ob-gnuplot.el		\
		ob-octave.el		\
		ob-screen.el		\
		ob-plantuml.el		\
		ob-org.el		\
		ob-js.el		\
		ob-scheme.el		\
		ob-lilypond.el		\
		ob-java.el		\
		ob-shen.el		\
		ob-fortran.el		\
		ob-picolisp.el		\
		ob-maxima.el

LISPFILES0  = $(LISPF:%=lisp/%)
LISPFILES   = $(LISPFILES0) lisp/org-install.el
ELCFILES0   = $(LISPFILES0:.el=.elc)
ELCFILES    = $(LISPFILES:.el=.elc)
DOCFILES    = doc/org.texi doc/org.pdf doc/org doc/dir \
              doc/pdflayout.sty doc/.nosearch \
              doc/orgguide.texi doc/orgguide.pdf
CARDFILES   = doc/orgcard.tex doc/orgcard.pdf doc/orgcard_letter.pdf
TEXIFILES   = doc/org.texi
INFOFILES   = doc/org

# etc/styles contains OpenDocument style files.  These files *must* be
# installed for the ODT exporter to function.  These files are
# distirbuted with GNU ELPA as well as with stock Emacs >= 24.1.

# contrib/odt/etc/schema contains OpenDocument schema files.  It is
# *desirable* but *not* mandatory that these files be installed.
# These files are not distributed with stock Emacs.  This is because
# the terms under which OASIS distributes these files are not
# agreeable to FSF.

# BasicODConverter-x.y.z.oxt is a LibreOffice extension for converting
# OpenDocument files to numerous other formats.  It is similar to
# unoconv and is implemented in StarBasic.  It is *desirable* but
# *not* *mandatory* that the converter be installed.  It is
# distributed under the same license as GNU Emacs.  This file is *not*
# part of GNU Emacs.
DATAFILES   = etc/styles \
	      # contrib/odt/BasicODConverter/BasicODConverter*.oxt \
	      # contrib/odt/etc/schema \

# Package Manager (ELPA)
PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "nil"

PKG_FILES = $(LISPFILES0)		\
            doc/dir doc/org		\
            doc/pdflayout.sty		\
            doc/org.pdf			\
            doc/orgguide.pdf		\
            doc/orgcard.tex		\
            doc/orgcard.pdf		\
            doc/orgcard_letter.pdf	\
            etc/

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc

default: $(ELCFILES) $(ELCBFILES)

all:	$(ELCFILES) $(ELCBFILES) $(INFOFILES)

up2:	update
	sudo ${MAKE} install

update:
	git pull
	${MAKE} clean
	${MAKE} all

compile: $(ELCFILES0) $(ELCBFILES)

install: install-lisp install-data

doc: doc/org.html doc/org.pdf doc/orgcard.pdf doc/orgcard_letter.pdf doc/orgguide.pdf doc/orgcard.txt

p:
	${MAKE} pdf && open doc/org.pdf

g:
	${MAKE} pdf && open doc/orgguide.pdf

# Always force re-compilation of org-odt
lisp/org-odt.elc: org-odt-data-dir
org-odt-data-dir:

# Sleight of hand to "hard code" the value of $(datadir) in
# org-odt.el.  See variables `org-odt-styles-dir-list' and
# `org-odt-schema-dir-list'.
install-lisp: BATCH_EXTRA = -eval "(setq org-odt-data-dir (expand-file-name \"$(datadir)\"))"

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(DESTDIR)$(lispdir) ]; then \
		$(MKDIR) $(DESTDIR)$(lispdir); else true; fi ;
	$(CP) $(LISPFILES)  $(DESTDIR)$(lispdir)
	$(CP) $(ELCFILES)   $(DESTDIR)$(lispdir)

install-info: $(INFOFILES)
	if [ ! -d $(DESTDIR)$(infodir) ]; then \
		$(MKDIR) $(DESTDIR)$(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --infodir=$(DESTDIR)$(infodir) $(INFOFILES)

install-data: $(DATAFILES)
	if [ ! -d $(DESTDIR)$(datadir) ]; then \
		$(MKDIR) $(DESTDIR)$(datadir); else true; fi ;
	$(CP) $(DATAFILES) $(DESTDIR)$(datadir)

autoloads: lisp/org-install.el

lisp/org-install.el: $(LISPFILES0) Makefile
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "lisp/org-install.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPF))))' \
		--eval '(insert "\n(provide (quote org-install))\n")' \
		--eval '(save-buffer)'

doc/org: doc/org.texi
	(cd doc && $(MAKEINFO) --no-split org.texi -o org)

doc/org.pdf: doc/org.texi
	(cd doc && $(TEXI2PDF) org.texi)

doc/orgguide.pdf: doc/orgguide.texi
	(cd doc && $(TEXI2PDF) orgguide.texi)

doc/org.html: doc/org.texi
	(cd doc && $(TEXI2HTML) --no-split -o org.html org.texi)
	UTILITIES/manfull.pl doc/org.html

doc/orgcard.pdf: doc/orgcard.tex
	(cd doc && pdftex orgcard.tex)

doc/orgcard.txt: doc/orgcard.tex
	(cd doc && perl ../UTILITIES/orgcard2txt.pl orgcard.tex > orgcard.txt)

doc/orgcard_letter.tex: doc/orgcard.tex
	perl -pe 's/\\pdflayout=\(0l\)/\\pdflayout=(1l)/' \
                   doc/orgcard.tex > doc/orgcard_letter.tex

doc/orgcard_letter.pdf: doc/orgcard_letter.tex
	(cd doc && pdftex orgcard_letter.tex)

# Below here are special targets for maintenance only

html: doc/org.html

html_manual: doc/org.texi
	rm -rf doc/manual
	mkdir doc/manual
	$(TEXI2HTML) -o doc/manual doc/org.texi
	UTILITIES/mansplit.pl doc/manual/*.html

html_guide: doc/orgguide.texi
	rm -rf doc/guide
	mkdir doc/guide
	$(TEXI2HTML) -o doc/guide doc/orgguide.texi
	UTILITIES/guidesplit.pl doc/guide/*.html

info:	doc/org

pdf:	doc/org.pdf doc/orgguide.pdf

card:	doc/orgcard.pdf doc/orgcard_letter.pdf doc/orgcard.txt

testrelease:
	git checkout -b testrelease origin/maint
	git merge -s recursive -X theirs master
	UTILITIES/set-version.pl testing
	git commit -a -m "Release testing"
	make distfile TAG=testversion
	make cleanrel
	rm -rf org-testversion*
	git reset --hard
	git checkout master
	git branch -D testrelease

# The following target makes a full release for the stuff that is
# currently on master.  Do it like this:
#
#   make release TAG=7.01

release:
	git checkout maint
	git merge -s recursive -X theirs master
	UTILITIES/set-version.pl $(TAG)
	git commit -a -m "Major release $(TAG) from master"
	make relup TAG=$(TAG)
	make cleanrel
	make pushreleasetag TAG=$(TAG)
	git push -f origin maint
	git checkout master
	git merge -s ours maint
	UTILITIES/set-version.pl -a $(TAG)
	git commit -a -m "Bump to version $(TAG) as current release from master"
	git push

# The following target makes a release, but from the stuff that is on
# maint, not from the stuff that is on master.  The idea is that it pushes
# out a minor fix into a minor update, while development on master
# already went full steam ahead.  To make a micro-relesse, cherry-pick
# the necessary changes into maint, then run (with proper version number)
# This is just like release, but we skip  the step which merges master
# into maint.
#
#   make fixrelease TAG=7.01.02

fixrelease:
	git checkout maint
	UTILITIES/set-version.pl $(TAG)
	git commit -a -m "Bugfix release $(TAG) from maint"
	make relup TAG=$(TAG)
	make cleanrel
	make pushreleasetag TAG=$(TAG)
	git push -f origin maint
	git checkout master
	git merge -s ours maint
	UTILITIES/set-version.pl -o $(TAG)
	git commit -a -m "Bump to version $(TAG) as current release from maint"
	git push

# ~$ make relup only makes sense from orgmode.org server
# Don't call it from your computer!
relup:
	${MAKE} makerelease
	${MAKE} sync_release
	${MAKE} sync_manual

makerelease:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	${MAKE} distfile
	${MAKE} doc
	UTILITIES/gplmanual.pl
	${MAKE} html_manual
	${MAKE} html_guide
	rm -rf RELEASEDIR
	$(MKDIR) RELEASEDIR
	cp org-$(TAG).zip org-$(TAG).tar.gz RELEASEDIR
	cp doc/org.pdf doc/orgcard.pdf doc/org.texi doc/org.html RELEASEDIR
	cp doc/org_dual_license.texi RELEASEDIR
	cp doc/orgguide.pdf doc/orgcard.txt RELEASEDIR
	cp RELEASEDIR/org-$(TAG).zip    RELEASEDIR/org.zip
	cp RELEASEDIR/org-$(TAG).tar.gz RELEASEDIR/org.tar.gz

# ~$ make sync_release only makes sense from orgmode.org server
# Don't call it from your computer!
sync_release:
	rsync -avuz RELEASEDIR/ /var/www/orgmode.org/

# ~$ make sync_manual only makes sense from orgmode.org server
# Don't call it from your computer!
sync_manual:
	rsync -avuz --delete doc/manual/ /var/www/orgmode.org/manual/
	rsync -avuz --delete doc/guide/ /var/www/orgmode.org/guide/

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch doc/org.texi doc/orgcard.tex # force update
	${MAKE} cleancontrib
	${MAKE} info
	${MAKE} doc
	${MAKE} lisp/org-install.el
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	$(MKDIR) org-$(TAG)/doc
	$(MKDIR) org-$(TAG)/lisp
	cp -r $(LISPFILES) org-$(TAG)/lisp
	cp -r $(DOCFILES) $(CARDFILES) org-$(TAG)/doc
	cp -r $(DISTFILES_extra) org-$(TAG)/
	cp -r README_DIST org-$(TAG)/README
	zip -r org-$(TAG).zip org-$(TAG)
	tar zcvf org-$(TAG).tar.gz org-$(TAG)

pkg:
	@if [ "X$(PKG_TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch doc/org.texi doc/orgcard.tex # force update
	${MAKE} info
	${MAKE} doc
	rm -rf org-$(PKG_TAG) org-$(PKG_TAG).tar
	$(MKDIR) org-$(PKG_TAG)
	cp -r $(PKG_FILES) org-$(PKG_TAG)
	echo "(define-package \"org\" \"$(PKG_TAG)\" \"$(PKG_DOC)\" $(PKG_REQ))" > org-$(PKG_TAG)/org-pkg.el
	tar cf org-$(PKG_TAG).tar org-$(PKG_TAG) --remove-files

cleanall:
	${MAKE} clean
	rm -f lisp/org-install.el

clean:
	${MAKE} cleanelc
	${MAKE} cleandoc
	${MAKE} cleanrel
	rm -f *~ */*~ */*/*~

cleancontrib:
	find contrib -name \*~ -exec rm {} \;

cleanelc:
	rm -f $(ELCFILES)
cleandoc:
	-(cd doc && rm -f org.pdf org org.html orgcard.pdf orgguide.pdf)
	-(cd doc && rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs)
	-(cd doc && rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps)
	-(cd doc && rm -f orgcard_letter.tex orgcard_letter.pdf)
	-(cd doc && rm -rf manual)

cleanrel:
	rm -rf RELEASEDIR
	rm -rf org-7.*
	rm -rf org-7*zip org-7*tar.gz

.el.elc:
	$(ELC) $<


push:
	git push orgmode@orgmode.org:org-mode.git master

pushtag:
	git tag -m "Adding tag" -a $(TAG)
	git push orgmode@orgmode.org:org-mode.git $(TAG)

pushreleasetag:
	git tag -m "Adding release tag" -a release_$(TAG)
	git push orgmode@orgmode.org:org-mode.git release_$(TAG)

# Dependencies

lisp/org.elc:		lisp/org-macs.el lisp/org-compat.el lisp/org-faces.el
lisp/org-agenda.elc:	lisp/org.el
lisp/org-ascii.elc:	lisp/org-exp.el
lisp/org-attach.elc:	lisp/org.el lisp/org-id.el
lisp/org-archive.elc:	lisp/org.el
lisp/org-bbdb.elc:	lisp/org.el
lisp/org-beamer.elc:	lisp/org.el
lisp/org-bibtex.elc:	lisp/org.el
lisp/org-capture.elc:	lisp/org.el lisp/org-mks.el
lisp/org-clock.elc:	lisp/org.el
lisp/org-colview.elc:	lisp/org.el
lisp/org-colview-xemacs.elc:	lisp/org.el
lisp/org-compat.elc:	lisp/org-macs.el
lisp/org-crypt.elc:	lisp/org-crypt.el lisp/org.el
lisp/org-ctags.elc:	lisp/org.el
lisp/org-datetree.elc:	lisp/org.el
lisp/org-docview.elc:	lisp/org.el
lisp/org-entities.elc:
lisp/org-exp.elc:	lisp/org.el lisp/org-agenda.el
lisp/org-exp-blocks.elc: lisp/org.el
lisp/org-latex.elc:	lisp/org.el lisp/org-exp.el lisp/org-beamer.el
lisp/org-docbook.elc:	lisp/org.el lisp/org-exp.el
lisp/org-faces.elc:	lisp/org-macs.el lisp/org-compat.el
lisp/org-feed.elc:	lisp/org.el
lisp/org-footnotes.elc:	lisp/org-macs.el lisp/org-compat.el
lisp/org-freemind.elc:	lisp/org.el
lisp/org-gnus.elc:	lisp/org.el
lisp/org-html.elc:	lisp/org-exp.el
lisp/org-habit.elc:	lisp/org.el lisp/org-agenda.el
lisp/org-icalendar.elc:	lisp/org-exp.el
lisp/org-id.elc:	lisp/org.el
lisp/org-indent.elc:	lisp/org.el lisp/org-macs.el lisp/org-compat.el
lisp/org-info.elc:	lisp/org.el
lisp/org-inlinetask.elc:
lisp/org-irc.elc:	lisp/org.el
lisp/org-jsinfo.elc:	lisp/org.el lisp/org-exp.el
lisp/org-list.elc:	lisp/org-macs.el lisp/org-compat.el
lisp/org-lparse.elc:	lisp/org-exp.el
lisp/org-mac-message.elc:	lisp/org.el
lisp/org-macs.elc:
lisp/org-mew.elc:	lisp/org.el
lisp/org-mhe.elc:	lisp/org.el
lisp/org-mks.elc:
lisp/org-mobile.elc:	lisp/org.el
lisp/org-mouse.elc:	lisp/org.el
lisp/org-odt.elc:	lisp/org-lparse.el
lisp/org-plot.elc:	lisp/org.el lisp/org-exp.el lisp/org-table.el
lisp/org-publish.elc:
lisp/org-protocol.elc:	lisp/org.el
lisp/org-remember.elc:	lisp/org.el
lisp/org-rmail.elc:	lisp/org.el
lisp/org-special-blocks.elc:	lisp/org-compat.el
lisp/org-src.elc:	lisp/org-macs.el lisp/org-compat.el
lisp/org-table.elc:	lisp/org.el
lisp/org-taskjuggler.elc: lisp/org.el lisp/org-exp.el
lisp/org-timer.elc:	lisp/org.el
lisp/org-vm.elc:	lisp/org.el
lisp/org-w3m.elc:	lisp/org.el
lisp/org-wl.elc:	lisp/org.el
lisp/org-xoxo.elc:	lisp/org-exp.el

# Describe valid make targets for org-mode.
targets help:
	@echo "make - compile Org ELisp files"
	@echo "make clean - clean Elisp and documentation files"
	@echo "make all - compile Org ELisp files and documentation"
	@echo ""
	@echo "make doc - make all documentation"
	@echo "make info - make Info documentation"
	@echo "make html - make HTML documentation"
	@echo "make pdf - make pdf documentation"
	@echo "make card - make refcards documentation"
	@echo ""
	@echo "make install - install Org"
	@echo "make install-lisp - install Org ELisp files"
	@echo "make install-info - install Org Info file"
