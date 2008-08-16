EMACS = emacs

ELFILES = \
	haskell-font-lock.el \
	haskell-mode.el \
	haskell-c.el \
	haskell-cabal.el \
	haskell-doc.el \
	haskell-decl-scan.el \
	haskell-indent.el \
	haskell-simple-indent.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
# AUTOLOADS = $(PACKAGE)-startup.el
AUTOLOADS = haskell-site-file.el

%.elc: %.el
	$(EMACS) --batch --eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

all: $(ELCFILES) $(AUTOLOADS)

info:
	# No Texinfo file, sorry.

######################################################################
###                    don't look below                            ###
######################################################################

PACKAGE=haskell-mode

$(AUTOLOADS): $(ELFILES)
	[ -f $@ ] || echo '' >$@
	$(EMACS) --batch --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

##

TAG = $(shell echo v$(VERSION) | tr '.' '_')
ftpdir=/u/monnier/html/elisp/
cvsmodule=$(shell cat CVS/Repository)
cvsroot=$(shell cat CVS/Root)

dist:
	cvs tag -F $(TAG) &&\
	cd $(TMP) &&\
	unset CVSREAD; cvs -d $(cvsroot) export -kv -r $(TAG) -d $(PACKAGE)-$(VERSION) $(cvsmodule) &&\
	cd $(PACKAGE)-$(VERSION) &&\
	make info $(AUTOLOADS) &&\
	rm -f gmon.out;\
	cd .. &&\
	tar zcf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION) &&\
	rm -rf $(PACKAGE)-$(VERSION)
	mv $(TMP)/$(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/
	ln -sf $(PACKAGE)-$(VERSION).tar.gz $(ftpdir)/$(PACKAGE).tar.gz

# arch-tag: 1ab314c8-3821-44fb-b533-dd58f5d75ba4
