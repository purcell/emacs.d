EMACS = emacs

ELFILES = \
	haskell-c.el \
	haskell-cabal.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell-font-lock.el \
	haskell-ghci.el \
	haskell-hugs.el \
	haskell-indent.el \
	haskell-indentation.el \
	haskell-mode.el \
	haskell-simple-indent.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
# AUTOLOADS = $(PACKAGE)-startup.el
AUTOLOADS = haskell-site-file.el

%.elc: %.el
	$(EMACS) --batch --eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

all: $(AUTOLOADS)

compile: $(ELCFILES)

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

VERSION = $(shell darcs show tags | head -n 1)
TAG = $(shell echo v$(VERSION) | sed 's/\./\\\./g')
TMP = $(shell echo $(PACKAGE)-$(VERSION))

dist:
	darcs get --lazy . $(TMP) &&\
	cd $(TMP) &&\
	rm -r _darcs &&\
	sed -i 's/\$$Name:  \$$/$(TAG)/g' * &&\
	make $(AUTOLOADS) &&\
	darcs changes > ChangeLog &&\
	rm Makefile &&\
	cd .. &&\
	tar czf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION) &&\
	rm -rf $(PACKAGE)-$(VERSION) &&\
	mv $(PACKAGE)-$(VERSION).tar.gz ../haskellmode-emacs-web/
