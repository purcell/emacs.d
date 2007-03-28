EXTRA=README NEWS VERSION TODO COPYING Makefile rng-auto.el \
  nxml-mode.xml nxml-mode.rnc nxml-mode.texi nxml-mode.info dir \
  test.valid.xml test.invalid.xml 

SRC=nxml-rap.el nxml-outln.el nxml-mode.el nxml-parse.el nxml-enc.el \
  nxml-uchnm.el nxml-glyph.el nxml-maint.el nxml-util.el nxml-ns.el \
  rng-dt.el rng-valid.el rng-pttrn.el rng-xsd.el rng-cmpct.el \
  rng-match.el rng-nxml.el rng-util.el rng-loc.el rng-uri.el \
  rng-parse.el rng-maint.el xsd-regexp.el xmltok.el

ELC=nxml-rap.elc nxml-outln.elc nxml-mode.elc nxml-parse.elc nxml-enc.elc \
  nxml-uchnm.elc nxml-glyph.elc nxml-maint.elc nxml-util.elc nxml-ns.elc \
  rng-dt.elc rng-valid.elc rng-pttrn.elc rng-xsd.elc rng-cmpct.elc \
  rng-match.elc rng-nxml.elc rng-util.elc rng-loc.elc rng-uri.elc \
  rng-parse.elc rng-maint.elc xsd-regexp.elc xmltok.elc

FILESTOCLEAN=stamp-byte-compile $(ELC) VERSION TAGS \
  nxml-mode.texi nxml-mode.info dir

EMACS=emacs
ETAGS=etags
MAKEINFO=makeinfo
INSTALL-INFO=install-info
PACKAGE=nxml-mode

stamp-byte-compile: $(SRC)
	-rm -f $(ELC)
	$(EMACS) -batch -l rng-auto.el -f rng-byte-compile-load
	touch $(@)

all: stamp-byte-compile nxml-mode.info

dir: nxml-mode.info
	rm -f $@
	$(INSTALL-INFO) $< $@

info: nxml-mode.info

nxml-mode.info: nxml-mode.texi
	$(MAKEINFO) $<

nxml-mode.texi: nxml-mode.xml
	$(EMACS) -batch -l rng-auto.el -f rng-format-manual

VERSION: stamp-byte-compile rng-auto.el
	$(EMACS) -batch -l rng-auto.el -f rng-write-version

TAGS: $(SRC)
	$(ETAGS) $(SRC)

dist: stamp-byte-compile $(EXTRA)
	@version=`cat VERSION`; \
	set -e; \
	echo Making $(PACKAGE)-$$version.tar.gz; \
	rm -fr $(PACKAGE)-$$version; \
	mkdir $(PACKAGE)-$$version; \
	cd $(PACKAGE)-$$version; \
	for f in $(EXTRA) $(SRC) $(ELC); do \
	  ln -s ../$$f .; \
	done; \
	mkdir schema; \
	cd schema; \
	for f in ../../schema/*.rnc ../../schema/*.xml; do \
	  ln -s $$f .; \
	done; \
	cd ..; \
	mkdir char-name; \
	mkdir char-name/unicode; \
	cd char-name/unicode; \
	for f in ../../../char-name/unicode/*-*.el; do \
	  ln -s $$f .; \
	done; \
	cd ../../..; \
	tar cfhz $(PACKAGE)-$$version.tar.gz $(PACKAGE)-$$version; \
	rm -fr $(PACKAGE)-$$version

clean:
	-rm -f $(FILESTOCLEAN)

.PHONY: all clean info
