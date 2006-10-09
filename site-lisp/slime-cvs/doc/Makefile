# Where to put the info file(s). NB: the GNU Coding Standards (GCS)
# and the Filesystem Hierarchy Standard (FHS) differ on where info
# files belong. The GCS says /usr/local/info; the FHS says
# /usr/local/share/info. Many distros obey the FHS, but people who
# installed their emacs from source probably have a GCS-ish file
# hierarchy.
infodir=/usr/local/info

# What command to use to install info file(s)
INSTALL_CMD=install -m 644

# Info files generated here.
infofiles=slime.info

all: slime.ps slime.info slime.pdf

install: install-info

uninstall: uninstall-info

slime.ps: slime.dvi
	dvips -o $@ $<

slime.dvi: slime.texi contributors.texi
	texi2dvi slime.texi

slime.pdf: slime.texi contributors.texi
	texi2pdf $<

slime.info: slime.texi contributors.texi
	makeinfo $<

# Create contributors.texi, a texinfo table listing all known
# contributors of code.
#
# Explicitly includes Eric Marsden (pre-ChangeLog hacker)
#
# The gist of this horror show is that the contributor list is piped
# into texinfo-tabulate.awk with one name per line, sorted
# alphabetically.
#
# Some special-case TeX-escaping of international characters.
contributors.texi: ../ChangeLog Makefile texinfo-tabulate.awk
	cat ../ChangeLog | \
	sed -ne '/^[0-9]/{s/^[^ ]* *//; s/ *<.*//; p;}' | \
	sort | \
	uniq -c | \
	sort -nr| \
	sed -e 's/^[^A-Z]*//' | \
	awk -f texinfo-tabulate.awk | \
	sed -e "s/\o341/@'a/g" | \
        sed -e "s/\o355/@'{@dotless{i}}/g" | \
	sed -e "s/\o351/@'e/g" | \
	sed -e "s/\o361/@~n/g" | \
	sed -e 's/\o370/@o{}/g' \
	> $@

#.INTERMEDIATE: contributors.texi

# Debian's install-info wants a --section argument.
section := $(shell grep INFO-DIR-SECTION $(infofiles) | sed 's/INFO-DIR-SECTION //')
install-info: slime.info
	mkdir -p $(infodir)
	$(INSTALL_CMD) $(infofiles) $(infodir)/$(infofiles)
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --section $(section) $(section) $(infodir)/$(infofiles)" && \
		install-info --infodir="$(infodir)" --section $(section) ${section} "$(infodir)/$(infofiles)" || :; fi

uninstall-info:
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --infodir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :; fi
	rm -f $(infodir)/$(infofiles)

clean:
	rm -f contributors.texi
	rm -f slime.aux
	rm -f slime.cp
	rm -f slime.dvi
	rm -f slime.fn
	rm -f slime.ky
	rm -f slime.log
	rm -f slime.pg
	rm -f slime.tmp
	rm -f slime.toc
	rm -f slime.tp
	rm -f slime.vr

really_clean: clean
	rm -f slime.info
	rm -f slime.pdf
	rm -f slime.ps
