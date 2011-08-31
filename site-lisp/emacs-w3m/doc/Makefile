.SUFFIXES: .texi .info .pdf

INSTALL      = /bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = install-info
prefix       = /usr
datarootdir  = ${prefix}/share
infodir      = ${datarootdir}/info
srcdir       = .
subdir       = doc
top_srcdir   = ..
ADDITIONAL_LOAD_PATH = NONE

SHELL        = /bin/sh


MAKEINFO     = makeinfo
TEXI2DVI     = texi2dvi
PTEX         = false
JBIBTEX      = false
DVIPDFMX     = false
TEXI2PDF     = texi2pdf
EMACS        = /usr/bin/emacs
VANILLA_FLAG = -q -no-site-file --no-unibyte
FLAGS        = $(VANILLA_FLAG) -batch -l ./w3mhack.el $(ADDITIONAL_LOAD_PATH)

## This is used to set the environment variable XEMACSDEBUG for
## XEmacs 21.5 in order to suppress warnings for Lisp shadows
## when XEmacs 21.5 starts.
XEMACSDEBUG = 

EMACSINFO    = cd $(top_srcdir) && $(XEMACSDEBUG)$(EMACS) $(FLAGS) -f w3mhack-makeinfo

INFO_DEPS_EN = emacs-w3m.info
INFO_DEPS_JA = emacs-w3m-ja.info
INFO_DEPS    = $(INFO_DEPS_EN) $(INFO_DEPS_JA)

default: $(INFO_DEPS)

en: $(INFO_DEPS_EN)

ja: $(INFO_DEPS_JA)

version.texi: $(top_srcdir)/w3m.el
	@version=`cd $(top_srcdir)\
		&& $(XEMACSDEBUG)$(EMACS) $(FLAGS) -f w3mhack-version 2>/dev/null`;\
	echo "@set VERSION $$version"> version.texi

emacs-w3m.info: version.texi emacs-w3m.texi

emacs-w3m-ja.info: version.texi emacs-w3m-ja.texi

.texi.info:
	@texi=$<;\
	case "$$texi" in \
	  *-ja.texi) \
	    rm -f $@ ;\
	    $(EMACSINFO) $$texi ;;\
	  *) \
	    if test "$(MAKEINFO)" = "no"; then \
	      rm -f $@ ;\
	      $(EMACSINFO) $$texi ;\
	    else \
	      echo "Formatting $$texi using $(MAKEINFO)..." ;\
	      LC_ALL=C $(MAKEINFO) --no-split -o $@ $$texi ;\
	      echo "Formatting $$texi using $(MAKEINFO)...done" ;\
	    fi ;;\
	esac

install: $(INFO_DEPS)
	$(SHELL) $(top_srcdir)/mkinstalldirs "$(infodir)"
	@for file in $(INFO_DEPS); do\
	  if test -f $$file; then\
	    for old in `cd "$(infodir)" && echo $$file-[0-9]`; do\
	      if test -f "$(infodir)/$$old"; then\
	        echo "rm -f \"$(infodir)/$$old\"";\
	        rm -f "$(infodir)/$$old";\
	      fi;\
	    done;\
	    echo "$(INSTALL_DATA) $$file \"$(infodir)/$$file\"";\
	    $(INSTALL_DATA) $$file "$(infodir)/$$file";\
	  else : ; fi;\
	done
	@if $(SHELL) -c $(INSTALL_INFO)' --version| sed 1q\
		| fgrep -s -v -i debian'>/dev/null 2>&1; then\
	  for file in $(INFO_DEPS); do\
	    if test -f "$(infodir)/$$file"; then\
	      echo "$(INSTALL_INFO) --info-dir=\"$(infodir)\" \"$(infodir)/$$file\"";\
	      $(INSTALL_INFO) --info-dir="$(infodir)" "$(infodir)/$$file" || :;\
	    else : ; fi\
	  done;\
	else : ; fi

install-en: $(INFO_DEPS_EN)
	$(MAKE) INFO_DEPS='$(INFO_DEPS_EN)' $(XEMACSDEBUG)EMACS="$(EMACS)"\
		infodir="$(infodir)" install

install-ja: $(INFO_DEPS_JA)
	$(MAKE) INFO_DEPS='$(INFO_DEPS_JA)' $(XEMACSDEBUG)EMACS="$(EMACS)"\
		infodir="$(infodir)" install

dvi: dvi-en dvi-ja

dvi-en: emacs-w3m.dvi

dvi-ja: emacs-w3m-ja.dvi

emacs-w3m.dvi: version.texi emacs-w3m.texi

emacs-w3m-ja.dvi: version.texi emacs-w3m-ja.texi

.texi.dvi:
	@texi=$<;\
	case "$$texi" in \
	  *-ja.texi) \
	    env TEX=$(PTEX) BIBTEX=$(JBIBTEX) $(TEXI2DVI) -o $@ $$texi ;;\
	  *) \
	    $(TEXI2DVI) -o $@ $$texi ;;\
	esac

pdf: pdf-en pdf-ja

pdf-en: emacs-w3m.pdf

pdf-ja: emacs-w3m-ja.pdf

emacs-w3m.pdf: version.texi emacs-w3m.texi

emacs-w3m-ja.pdf: version.texi emacs-w3m-ja.texi

.texi.pdf:
	@texi=$<;\
	case "$$texi" in\
	  *-ja.texi)\
	     i=`basename $@ .pdf`;\
	    $(MAKE) $$i.dvi; $(DVIPDFMX) $$i;;\
	  *) \
	    $(TEXI2PDF) $$texi;;\
	esac

clean:
	-rm -rf *~ *.info version.texi *.dvi missfont.log *.pdf *.aux *.cp\
 *.cps *.fn *.fns *.ky *.kys *.log *.pg *.toc *.tp *.vr *.vrs

distclean: clean
	-rm -f Makefile
