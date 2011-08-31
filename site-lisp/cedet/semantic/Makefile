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
LOADPATH= ../common/  ctags/ wisent/ bovine/\
    symref/  ../speedbar/ ../ede/ ../eieio/\
    ./
LOADDEFS=semantic-loaddefs.el
LOADDIRS=. bovine wisent ctags symref
init_LISP=semantic-load.el
EMACS=emacs
EMACSFLAGS=-batch --no-site-file
semantic_LISP=semantic.el semantic-fw.el semantic-lex.el semantic-lex-spp.el semantic-edit.el semantic-util.el semantic-tag.el semantic-tag-ls.el semantic-find.el semantic-sort.el semantic-tag-file.el semantic-tag-write.el semantic-doc.el semantic-idle.el
utils_LISP=semantic-ast.el semantic-ctxt.el semantic-decorate.el semantic-dep.el semantic-format.el semantic-scope.el
Database_LISP=semanticdb.el semanticdb-mode.el semanticdb-debug.el semanticdb-ebrowse.el semanticdb-el.el semanticdb-file.el semanticdb-find.el semanticdb-mk.el semanticdb-ref.el semanticdb-search.el semanticdb-typecache.el semanticdb-javascript.el semanticdb-global.el
tools_LISP=semantic-adebug.el semantic-chart.el semantic-complete.el semantic-debug.el semantic-decorate-mode.el semantic-decorate-include.el semantic-elp.el semantic-grammar.el semantic-ia-sb.el semantic-ia.el semantic-imenu.el semantic-mru-bookmark.el semantic-sb.el semantic-util-modes.el senator.el
Analyzer_LISP=semantic-analyze.el semantic-analyze-complete.el semantic-analyze-fcn.el semantic-analyze-debug.el semantic-analyze-refs.el
Languages_LISP=semantic-texi.el semantic-html.el
maintenance_LISP=semantic-ede-grammar.el
metagrammar_SEMANTIC_GRAMMAR=semantic-grammar.wy
EMACS=emacs
metagrammar_SEMANTIC_GRAMMAR_EL=semantic-grammar-wy.el
tests_LISP=semantic-regtest.el semantic-ia-utest.el semantic-utest.el semantic-utest-c.el
example_MISC=semantic-example.el semanticdb-skel.el
scripts_MISC=semanticdb.sh
misc_AUX=INSTALL NEWS ChangeLog AUTHORS ONEWS renamelist.txt
VERSION=2.0pre6
DISTDIR=$(top)semantic-$(VERSION)



all: autoloads init semantic utils Database tools Analyzer Languages metagrammar tests example scripts wisent Tests Symref Documentation ctags bovinator

.PHONY: autoloads
autoloads: 
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(require 'cedet-autogen)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f cedet-batch-update-autoloads $(LOADDEFS) $(LOADDIRS)

.PHONY: init
init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: semantic
semantic: $(semantic_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: utils
utils: $(utils_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: Database
Database: $(Database_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: tools
tools: $(tools_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: Analyzer
Analyzer: $(Analyzer_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: Languages
Languages: $(Languages_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: maintenance
maintenance: $(maintenance_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: metagrammar
metagrammar: $(metagrammar_SEMANTIC_GRAMMAR)
	@echo "(add-to-list 'load-path nil)" > grammar-make-script
	@for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> grammar-make-script; \
	done;
	@echo "(require 'semantic-load)" >> grammar-make-script
	@echo "(require 'semantic-grammar)" >> grammar-make-script
	"$(EMACS)" -batch --no-site-file -l grammar-make-script -f semantic-grammar-batch-build-packages $^

.PHONY: tests
tests: $(tests_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

example: 
	@

scripts: 
	@

.PHONY:wisent
wisent:
	cd wisent; $(MAKE)

.PHONY:Tests
Tests:
	cd tests; $(MAKE)

.PHONY:Symref
Symref:
	cd symref; $(MAKE)

.PHONY:Documentation
Documentation:
	cd doc; $(MAKE)

.PHONY:ctags
ctags:
	cd ctags; $(MAKE)

.PHONY:bovinator
bovinator:
	cd bovine; $(MAKE)

tags: 
	cd wisent/; make $(MFLAGS) $@
	cd tests/; make $(MFLAGS) $@
	cd symref/; make $(MFLAGS) $@
	cd doc/; make $(MFLAGS) $@
	cd ctags/; make $(MFLAGS) $@
	cd bovine/; make $(MFLAGS) $@


clean:
	rm -f *.elc

.PHONY: dist

dist: autoloads $(metagrammar_SEMANTIC_GRAMMAR_EL)
	mkdir $(DISTDIR)
	cp semantic-loaddefs.el $(init_LISP) $(semantic_LISP) $(utils_LISP) $(Database_LISP) $(tools_LISP) $(Analyzer_LISP) $(Languages_LISP) $(maintenance_LISP) $(metagrammar_SEMANTIC_GRAMMAR) $(metagrammar_SEMANTIC_GRAMMAR_EL) $(tests_LISP) $(example_MISC) $(scripts_MISC) $(misc_AUX) $(ede_FILES) $(DISTDIR)
	cd wisent; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/wisent dist
	cd tests; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/tests dist
	cd symref; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/symref dist
	cd doc; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/doc dist
	cd ctags; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/ctags dist
	cd bovine; $(MAKE) $(MFLAGS) DISTDIR=$(DISTDIR)/bovine dist

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you can use 'touch' to update the Makefile time stamp.
	@false



# End of Makefile
