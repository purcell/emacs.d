##----------------------------------------------------------------------
##  NEVER EDIT THIS FILE, PUT ANY ADAPTATIONS INTO local.mk
##-8<-------------------------------------------------------------------
##  CHECK AND ADAPT THE FOLLOWING DEFINITIONS
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS	= emacs

# Where local software is found
prefix	= /usr/share

# Where local lisp files go.
lispdir= $(prefix)/emacs/site-lisp/org

# Where local data files go.
datadir = $(prefix)/emacs/etc/org

# Where info files go.
infodir = $(prefix)/info

# Define if you only need info documentation, the default includes html and pdf
#ORG_MAKE_DOC = info # html pdf

# Define if you want to include some (or all) files from contrib/lisp
# just the filename please (no path prefix, no .el suffix), maybe with globbing
#ORG_ADD_CONTRIB = ox-* # e.g. the contributed exporter

# Where to create temporary files for the testsuite
# respect TMPDIR if it is already defined in the environment
TMPDIR ?= /tmp
testdir = $(TMPDIR)/tmp-orgtest

# Configuration for testing
# add options before standard load-path
BTEST_PRE   =
# add options after standard load path
BTEST_POST  =
              # -L <path-to>/ert      # needed for Emacs23, Emacs24 has ert built in
              # -L <path-to>/ess      # needed for running R tests
              # -L <path-to>/htmlize  # need at least version 1.34 for source code formatting
BTEST_OB_LANGUAGES = awk C fortran maxima lilypond octave python sh perl
              # R                     # requires ESS to be installed and configured
# extra packages to require for testing
BTEST_EXTRA =
              # ess-site  # load ESS for R tests
##->8-------------------------------------------------------------------
## YOU MAY NEED TO ADAPT THESE DEFINITIONS
##----------------------------------------------------------------------

# How to run tests
req-ob-lang = --eval '(require '"'"'ob-$(ob-lang))'
req-extra   = --eval '(require '"'"'$(req))'
BTEST	= $(BATCH) \
	  $(BTEST_PRE) \
	  --eval '(add-to-list '"'"'load-path "./lisp")' \
	  --eval '(add-to-list '"'"'load-path "./testing")' \
	  $(BTEST_POST) \
	  -l org-loaddefs.el \
	  -l testing/org-test.el \
	  $(foreach ob-lang,$(BTEST_OB_LANGUAGES),$(req-ob-lang)) \
	  $(foreach req,$(BTEST_EXTRA),$(req-extra)) \
	  --eval '(setq org-confirm-babel-evaluate nil)' \
	  -f org-test-run-batch-tests

# Using emacs in batch mode.
# BATCH = $(EMACS) -batch -vanilla # XEmacs
BATCH	= $(EMACS) -batch -Q \
	  --eval '(setq vc-handled-backends nil)'

# Emacs must be started in toplevel directory
BATCHO	= $(BATCH) \
	  --eval '(add-to-list '"'"'load-path "./lisp")'

# How to generate local.mk
MAKE_LOCAL_MK = $(BATCHO) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-local-mk)'

# Emacs must be started in lisp directory
BATCHL	= $(BATCH) \
	  --eval '(add-to-list '"'"'load-path ".")'

# How to generate org-loaddefs.el
MAKE_ORG_INSTALL = $(BATCHL) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-org-loaddefs)'

# How to generate org-version.el
MAKE_ORG_VERSION = $(BATCHL) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-org-version "$(ORGVERSION)" "$(GITVERSION)" "'$(datadir)'")'

# How to byte-compile the whole source directory
ELCDIR	= $(BATCHL) \
	  --eval '(batch-byte-recompile-directory 0)'

# How to byte-compile a single file
ELC	= $(BATCHL) \
	  --eval '(batch-byte-compile)'

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf --batch --clean

# How to make a pdf file from a tex file
PDFTEX = pdftex

# How to create directories with leading path components
# MKDIR	= mkdir -m 755 -p # try this if you have no install
MKDIR	= install -m 755 -d

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections

# How to find files
FIND	= find

# How to remove files
RM	= rm -f

# How to remove files recursively
RMR	= rm -fr

# How to copy the lisp files and elc files to their destination.
# CP	= cp -p	# try this if you have no install
CP	= install -m 644 -p

# How to obtain administrative privileges
# leave blank if you don't need this
# SUDO	=
SUDO	= sudo

# Name of the program to install info files
# INSTALL_INFO = ginstall-info # Debian: avoid harmless warning message
INSTALL_INFO = install-info

# target method for 'compile'
ORGCM	= dirall
# ORGCM	= dirall #   1x slowdown compared to default compilation method
# ORGCM	= single #   4x one Emacs process per compilation
# ORGCM	= source #   5x ditto, but remove compiled file immediately
# ORGCM	= slint1 #   3x possibly elicit more warnings
# ORGCM	= slint2 #   7x possibly elicit even more warnings
