VERSION=0.0.3
EMACS = emacs
PREFIX = /usr/local
INSTALLLIBDIR = $(PREFIX)/share/emacs/site-lisp
FLAGS = -batch -L $(INSTALLLIBDIR) -q -f batch-byte-compile
SRC = yaml-mode.el
INSTALL = /usr/bin/install -c -m 444

all: bytecompile

bytecompile:
	$(EMACS) $(FLAGS) $(SRC)

install: bytecompile
	$(INSTALL) yaml-mode.elc $(INSTALLLIBDIR)
	$(INSTALL) yaml-mode.el $(INSTALLLIBDIR)

uninstall:
	rm $(INSTALLLIBDIR)/yaml-mode.elc
	rm $(INSTALLLIBDIR)/yaml-mode.el

tardist:
	mkdir yaml-mode-$(VERSION)
	cp yaml-mode.el Makefile README Changes yaml-mode-$(VERSION)
	tar zcvf yaml-mode-$(VERSION).tar.gz yaml-mode-$(VERSION)
	rm -fr yaml-mode-$(VERSION)

clean:
	rm -fr \#*\# *.elc *~ *.tar.gz

