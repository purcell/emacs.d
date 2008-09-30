
## set this variable to "xemacs" if you use XEmacs
EMACS = emacs

## You will probably not need to change anything below this line

BYTE = $(EMACS) -batch -q -no-site-file -l dot.el -f batch-byte-compile

.PHONY:	all default clean

default:
	$(MAKE) info-look.elc gnuplot.elc gnuplot-gui.elc

gnuplot.elc:	gnuplot.el
	$(BYTE) gnuplot.el

gnuplot-gui.elc:	gnuplot-gui.el
	$(BYTE) gnuplot-gui.el

## There are many possibilities for info-look:
##
##     EMACS version		use
##  -----------------------------------------
##   Emacs or XEmacs 19    info-look.20.2.el
##   Emacs 20.2 or less    info-look.20.2.el
##   Emacs 20.3            nothing
##   XEmacs 20+            info-look.20.3.el
##
## want to use my modified version even if 20.2 is installed because a
## bug is fixed
##
## the first 6 lines attempt to ascertain the version number of
## $(EMACS), then multiply by 100 to convert it to an integer for the
## sake of the integer comparisons in the following lines.  Is this a
## hassle, or what?!
##
MESSAGE = compiling info-look for $(EMACS) $$vnum
info-look.elc:	info-look.20.2.el info-look.20.3.el
	@if [ $(EMACS) = "emacs" ]; \
	  then vnum=`emacs  --version | grep 'Emacs [12]' | awk '{print $$3}'`; \
          else vnum=`xemacs --version | grep 'Emacs [12]' | awk '{print $$2}'`; \
        fi; \
        vn=`echo "$$vnum" | awk 'BEGIN{FS="."}{print $$1 "." $$2}'`; \
	version=`echo "$$vn" | awk '{print 100*$$1}'`; \
	if [ $(EMACS) = "emacs" -a $$version -ge 2030 ]; \
          then echo "no need to compile info-look for $(EMACS) $$vnum"; \
	  else echo "$(MESSAGE)"; \
        fi; \
	if   [ $(EMACS) = "emacs"  -a $$version -lt 2030 ]; \
	  then cp -v info-look.20.2.el info-look.el; \
	elif [ $(EMACS) = "xemacs" -a $$version -ge 2000 ]; \
	  then cp -v info-look.20.3.el info-look.el; \
	elif [ $(EMACS) = "xemacs" -a $$version -lt 2000 ]; \
	  then cp -v info-look.20.2.el info-look.el; \
        fi ;\
	if [ ! \( $(EMACS) = "emacs" -a $$version -ge 2030 \) ]; \
	  then $(BYTE) info-look.el; fi;

gpelcard.ps:	gpelcard.dvi
	dvips -o gpelcard.ps gpelcard.dvi

gpelcard.dvi:	gpelcard.tex
	 latex gpelcard.tex

all:
	$(MAKE) gnuplot.elc info-look.elc gnuplot-gui.elc gpelcard.ps

clean:
	rm -f *.elc info-look.el gpelcard.dvi gpelcard.log gpelcard.aux


##----------------------------------------------------------------------
## old stuff:

## from the `all' and `default' targets:
#  gnuplot-toolbar.elc kw-compl.elc

#kw-compl.elc:	kw-compl.el
#	$(BYTE) kw-compl.el

## only make this for XEmacs
#gnuplot-toolbar.elc:	gnuplot-toolbar.el
#	if [ $(EMACS) = "xemacs" ]; then $(BYTE) gnuplot-toolbar.el; fi
