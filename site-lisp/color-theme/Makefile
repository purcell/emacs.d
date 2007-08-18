# -*- Makefile -*-

# Borrowed from many emacs places

include Makefile.defs

SPECIAL = 
UNCOMPILED = 
AUTOLOADFILE = color-theme-autoloads
TESTING = 
THEMES_DIRECTORY = themes
THEMES_FILES := $(wildcard $(THEMES_DIRECTORY)/*.el)
ALLSOURCE := $(wildcard *.el) $(THEMES_FILES)
SOURCE	= $(filter-out $(SPECIAL) $(UNCOMPILED) $(TESTING),$(ALLSOURCE))
TARGET	= $(patsubst %.el,%.elc,$(SPECIAL) $(SOURCE))
MANUAL  = color-theme
MISC	= AUTHORS COPYING ChangeLog Makefile.defs Makefile $(AUTOLOADFILE).in
#AUTHORS CREDITS HISTORY NEWS README Makefile ChangeLog \
#ChangeLog.2005 ChangeLog.2004 ChangeLog.2003 ChangeLog.2002 \
#ChangeLog.2001 servers.pl color-theme-auto.in color-theme.texi

all: lisp #$(MANUAL).info

lisp: clean $(TARGET) 

autoloads: $(AUTOLOADFILE).elc

$(AUTOLOADFILE).el: $(AUTOLOADFILE).in #$(TARGET)
	cp $(AUTOLOADFILE).in $(AUTOLOADFILE).el
	rm -f $(AUTOLOADFILE).elc
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l $(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTOLOADFILE) \
		-f color-theme-generate-autoloads \
		$(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTOLOADFILE).el . \
		$(THEMES_DIRECTORY)

$(AUTOLOADFILE).elc: $(AUTOLOADFILE).el
	@echo "Byte compiling the autoload file "$<
	@$(EMACS) -batch -q -f batch-byte-compile $^
	@echo "*******************************************************************"
	@echo "Autoloads up to date. Put the following lines in your configuration"
	@echo "file (~/.emacs for a single user) :"
	@echo
	@echo ${patsubst %, "(add-to-list 'load-path \""%"\")   ", $(LISPDIRS)}
	@echo "(require 'color-theme-autoload \""$(AUTOLOADFILE)"\")"
	@echo

%.elc: %.el
	@$(EMACS) $(OPTIONCOMPILE) \
	--eval '(setq load-path (cons "." load-path))' \
	-f batch-byte-compile $<

%.info: %.texi
	@echo "No doc yet !"
#	makeinfo $<

%.html: %.texi
	@echo "No doc yet !"
#	makeinfo --html --no-split $<

doc: $(MANUAL).info $(MANUAL).html
	@echo "No doc yet !"

clean:
	-rm -f themes/*.elc
	-rm -f *~ *.elc $(AUTOLOADFILE).el

realclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html $(TARGET) $(SPECIAL)

install-info: $(MANUAL).info
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(MANUAL)
	$(INSTALLINFO) $(INFODIR)/$(MANUAL)

install-bin: lisp
	install -d $(ELISPDIR)
	install -d $(ELISPDIR)/themes
	install -m 0644 $(ALLSOURCE) $(TARGET) $(ELISPDIR)
	install -m 0644 $(THEMES_FILES) $(TARGET) $(ELISPDIR)/themes

install: install-bin install-info

## DO NOT TOUCH THIS !
## HELPERS FOR MAINTAINER(S)
distclean:
	-rm  $(MANUAL).info $(MANUAL).html $(TARGET)
	-rm -Rf ../$(DISTDIR)
	-rm -f debian/dirs debian/files
	-rm -rf $(DISTDIR) $(TARBALL)* $(ZIPFILE)* $(DEBNAME)*

dist: distclean Makefile
	$(MAKE) dist-prepare

# Idea taken from w3m-el
dist-prepare: CVS/Root CVS/Repository
	cvs -d $(CVSROOT) -w export -d $(DISTDIR) -r $(CVSBRANCH) $(CVSMODULE)
	-cvs diff |( cd $(DISTDIR) && patch -p0 )

$(TARBALL): tarball
$(DEBNAME): debian

tarball: dist
	find $(DISTDIR) -name .cvsignore | xargs rm -f
	find $(DISTDIR) -name debian | xargs rm -fr
	find $(DISTDIR) -type d | xargs chmod 755
	find $(DISTDIR) -type f | xargs chmod 644

	tar -cf `basename $(TARBALL) .gz` $(DISTDIR)
	gzip -9 `basename $(TARBALL) .gz`
	zip -r $(ZIPFILE) $(DISTDIR)
	gpg --detach $(TARBALL)
	gpg --detach $(ZIPFILE)

debian: dist
	(cd $(DISTDIR) && \
	  dpkg-buildpackage -v$(LASTUPLOAD) $(BUILDOPTS) \
	    -us -uc -rfakeroot && \
	  echo "Running lintian ..." && \
	  lintian -i ../$(DEBNAME)*.deb || : && \
	  echo "Done running lintian." && \
	  debsign)

	cp $(DEBNAME)* /var/spool/repo
	(cd /var/spool/repo && \
	dpkg-scanpackages . /dev/null | gzip -9 > Packages.gz && \
	dpkg-scansources . | gzip -9 > Sources.gz)

release: $(DEBNAME) $(TARBALL)
	rm -rf $(DISTDIR)
	$(MAKE) upload distclean

upload:
	(cd /var/spool/repo && echo open perso.nerim.net > upload.lftp ; \
	  echo cd /var/spool/repo >> upload.lftp ; \
	  echo mput * >> upload.lftp ; \
	  echo close >> upload.lftp ; \
	  lftp -f upload.lftp ; \
	  rm -f upload.lftp)
	(scp $(ZIPFILE)* $(TARBALL)* \
            zeDek@download.gna.org:/upload/color-theme)
