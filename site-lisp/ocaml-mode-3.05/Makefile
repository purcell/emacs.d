# $Id: Makefile,v 1.9.2.1 2002/08/05 06:30:46 garrigue Exp $

# Files to install
FILES=	caml-font.el caml-hilit.el caml.el camldebug.el \
	inf-caml.el caml-compat.el caml-help.el

# Where to install. If empty, automatically determined.
#EMACSDIR=

# Name of Emacs executable
EMACS=emacs

# Where to install ocamltags script
SCRIPTDIR = /usr/local/bin

# Command for byte-compiling the files
COMPILECMD=(progn \
              (setq load-path (cons "." load-path)) \
              (byte-compile-file "caml.el") \
              (byte-compile-file "inf-caml.el") \
              (byte-compile-file "caml-help.el") \
              (byte-compile-file "camldebug.el"))

install:
	@if test "$(EMACSDIR)" = ""; then \
          set xxx `($(EMACS) --batch --eval "(mapcar 'print load-path)") \
                   2>/dev/null | \
                   sed -n -e '/\/site-lisp/s/"//gp'`; \
          if test "$$2" = ""; then \
            echo "Cannot determine Emacs site-lisp directory"; \
            exit 2; \
          fi; \
          $(MAKE) EMACSDIR="$$2" simple-install; \
        else \
          $(MAKE) simple-install; \
        fi

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(FILES) $(EMACSDIR)
	cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'

ocamltags:	ocamltags.in
	sed -e 's:@EMACS@:$(EMACS):' ocamltags.in >ocamltags
	chmod a+x ocamltags

install-ocamltags: ocamltags

	cp ocamltags $(SCRIPTDIR)/ocamltags

clean:
	rm -f ocamltags *~ #*#
