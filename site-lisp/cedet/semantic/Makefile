# Automatically Generated Makefile by EDE.
# For use with: make
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
# 

top=
ede_FILES=Project.ede Makefile

EMACS=emacs
LOADPATH= ../common/  ./ ../eieio/  ../ede/\
    wisent/ bovine/  ../speedbar/
LOADDEFS=semantic-loaddefs.el
LOADDIRS=. bovine wisent
metagrammar_SEMANTIC_GRAMMAR=semantic-grammar.wy
EMACS=emacs
metagrammar_SEMANTIC_GRAMMAR_EL=semantic-grammar-wy.el
maintenance_LISP=semantic-ede-grammar.el
EMACS=emacs
scripts_MISC=semanticdb.sh
example_MISC=semantic-example.el semanticdb-skel.el
init_LISP=semantic-load.el
semantic_LISP=semantic.el semantic-util.el semantic-ctxt.el semantic-lex.el semantic-edit.el semantic-fw.el semantic-grammar.el semantic-tag.el semantic-find.el semantic-format.el semantic-complete.el semantic-sort.el semantic-decorate.el semantic-tag-file.el semantic-doc.el semantic-tag-ls.el semantic-ast.el semantic-lex-spp.el semantic-dep.el
Languages_LISP=semantic-texi.el semantic-html.el
tools_LISP=semantic-imenu.el semantic-sb.el semanticdb.el document.el document-vars.el semantic-chart.el semantic-util-modes.el semantic-analyze.el semantic-ia.el semantic-cb.el semantic-ia-sb.el semanticdb-file.el semanticdb-search.el semanticdb-mk.el semanticdb-el.el semanticdb-system.el semantic-debug.el semanticdb-find.el semantic-regtest.el semantic-idle.el semantic-decorate-mode.el semanticdb-ebrowse.el semantic-utest.el semantic-adebug.el semantic-adebug-eieio.el semantic-mru-bookmark.el
senator_LISP=senator.el
misc_AUX=INSTALL NEWS ChangeLog AUTHORS ONEWS renamelist.txt
VERSION=2.0pre4
DISTDIR=$(top)semantic-$(VERSION)



all: autoloads metagrammar scripts example init semantic Languages tools senator wisent Tests Documentation bovinator

.PHONY: autoloads
autoloads: 
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(require 'cedet-autogen)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f cedet-batch-update-autoloads $(LOADDEFS) $(LOADDIRS)

.PHONY: metagrammar
metagrammar: $(metagrammar_SEMANTIC_GRAMMAR)
	@echo "(add-to-list 'load-path nil)" > grammar-make-script
	@for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> grammar-make-script; \
	done;
	@echo "(require 'semantic-load)" >> grammar-make-script
	@echo "(require 'semantic-grammar)" >> grammar-make-script
	"$(EMACS)" -batch --no-site-file -l grammar-make-script -f semantic-grammar-batch-build-packages $^

.PHONY: maintenance
maintenance: $(maintenance_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

scripts: 
	@

example: 
	@

.PHONY: init
init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: semantic
semantic: $(semantic_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: Languages
Languages: $(Languages_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: tools
tools: $(tools_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: senator
senator: $(senator_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY:wisent
wisent:
	cd wisent; $(MAKE)

.PHONY:Tests
Tests:
	cd tests; $(MAKE)

.PHONY:Documentation
Documentation:
	cd doc; $(MAKE)

.PHONY:bovinator
bovinator:
	cd bovine; $(MAKE)

tags: 
	cd wisent/; make $(MFLAGS) $@
	cd tests/; make $(MFLAGS) $@
	cd doc/; make $(MFLAGS) $@
	cd bovine/; make $(MFLAGS) $@


clean:
	rm -f *.elc

.PHONY: dist

dist: autoloads $(metagrammar_SEMANTIC_GRAMMAR_EL)
	mkdir $(DISTDIR)
	cp semantic-loaddefs.el $(metagrammar_SEMANTIC_GRAMMAR) $(metagrammar_SEMANTIC_GRAMMAR_EL) $(maintenance_LISP) $(scripts_MISC) $(example_MISC) $(init_LISP) $(semantic_LISP) $(Languages_LISP) $(tools_LISP) $(senator_LISP) $(misc_AUX) $(ede_FILES) $(DISTDIR)
	cd wisent; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/wisent dist
	cd tests; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/tests dist
	cd doc; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/doc dist
	cd bovine; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/bovine dist

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you can use 'touch' to update the Makefile time stamp.
	@false



# End of Makefile
