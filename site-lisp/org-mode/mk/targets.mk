.EXPORT_ALL_VARIABLES:
.NOTPARALLEL: .PHONY
# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc

LISPDIRS      = lisp
OTHERDIRS     = doc etc
CLEANDIRS     = contrib testing mk
SUBDIRS       = $(OTHERDIRS) $(LISPDIRS)
INSTSUB       = $(SUBDIRS:%=install-%)
ORG_MAKE_DOC ?= info html pdf

ifneq ($(wildcard .git),)
  GITVERSION ?= $(shell git describe --abbrev=6 HEAD)
  ORGVERSION ?= $(subst release_,,$(shell git describe --abbrev=0 HEAD))
  GITSTATUS  ?= $(shell git status -uno --porcelain)
else
 -include mk/version.mk
  GITVERSION ?= N/A
  ORGVERSION ?= N/A
endif
DATE          = $(shell date +%Y-%m-%d)
ifneq ($(GITSTATUS),)
  GITVERSION := $(GITVERSION:.dirty=).dirty
endif

.PHONY:	all oldorg update update2 up0 up1 up2 single $(SUBDIRS) \
	check test install $(INSTSUB) \
	info html pdf card refcard doc docs \
	autoloads cleanall clean $(CLEANDIRS:%=clean%) \
	clean-install cleanelc cleandirs cleanaddcontrib \
	cleanlisp cleandoc cleandocs cleantest \
	compile compile-dirty uncompiled \
	config config-test config-exe config-all config-eol config-version

CONF_BASE = EMACS DESTDIR ORGCM ORG_MAKE_DOC
CONF_DEST = lispdir infodir datadir testdir
CONF_TEST = BTEST_PRE BTEST_POST BTEST_OB_LANGUAGES BTEST_EXTRA
CONF_EXEC = CP MKDIR RM RMR FIND SUDO PDFTEX TEXI2PDF TEXI2HTML MAKEINFO INSTALL_INFO
CONF_CALL = BATCH BATCHL ELC ELCDIR BTEST MAKE_LOCAL_MK MAKE_ORG_INSTALL MAKE_ORG_VERSION
config-eol:: EOL = \#
config-eol:: config-all
config config-all::
	$(info )
	$(info ========= Emacs executable and Installation paths)
	$(foreach var,$(CONF_BASE),$(info $(var)	= $($(var))$(EOL)))
	$(foreach var,$(CONF_DEST),$(info $(var)	= $(DESTDIR)$($(var))$(EOL)))
	$(info ========= Additional files from contrib/lisp)
	$(info $(notdir \
		$(wildcard \
		$(addsuffix .el, \
		$(addprefix contrib/lisp/, \
		$(basename \
		$(notdir $(ORG_ADD_CONTRIB))))))))
config-test config-all::
	$(info )
	$(info ========= Test configuration)
	$(foreach var,$(CONF_TEST),$(info $(var)	= $($(var))$(EOL)))
config-exe config-all::
	$(info )
	$(info ========= Executables used by make)
	$(foreach var,$(CONF_EXEC),$(info $(var)	= $($(var))$(EOL)))
config-cmd config-all::
	$(info )
	$(info ========= Commands used by make)
	$(foreach var,$(CONF_CALL),$(info $(var)	= $($(var))$(EOL)))
config config-test config-exe config-all config-version::
	$(info ========= Org version)
	$(info make:  Org-mode version $(ORGVERSION) ($(GITVERSION) => $(lispdir)))
	@echo ""

oldorg:	compile info	# what the old makefile did when no target was specified
uncompiled:	cleanlisp autoloads	# for developing
refcard:	card
update update2::	up0 all

single:	ORGCM=single
single:	compile

.PRECIOUS:	local.mk
local.mk:
	$(info ======================================================)
	$(info = Invoke "make help" for a synopsis of make targets. =)
	$(info = Created a default local.mk template.               =)
	$(info = Setting "oldorg" as the default target.            =)
	$(info = Please adapt local.mk to your local setup!         =)
	$(info ======================================================)
	-@$(MAKE_LOCAL_MK)

all compile::
	$(foreach dir, doc lisp, $(MAKE) -C $(dir) clean;)
compile compile-dirty::
	$(MAKE) -C lisp $@
all clean-install::
	$(foreach dir, $(SUBDIRS), $(MAKE) -C $(dir) $@;)

check test::	compile
check test test-dirty::
	-$(MKDIR) $(testdir)
	TMPDIR=$(testdir) $(BTEST)
ifeq ($(TEST_NO_AUTOCLEAN),) # define this variable to leave $(testdir) around for inspection
	$(MAKE) cleantest
endif

up0::	cleanaddcontrib
up0 up1 up2::
	git remote update
	git pull
up1 up2::	all
	$(MAKE) test-dirty
up2 update2::
	$(SUDO) $(MAKE) install

install:	$(INSTSUB)

install-info:	install-doc

doc docs:	$(ORG_MAKE_DOC)

info html pdf card:
	$(MAKE) -C doc $@

$(INSTSUB):
	$(MAKE) -C $(@:install-%=%) install

autoloads: lisp
	$(MAKE) -C $< $@

cleandirs:
	$(foreach dir, $(SUBDIRS), $(MAKE) -C $(dir) cleanall;)

clean:	cleanlisp cleandoc

cleanall: cleandirs cleantest cleanaddcontrib
	-$(FIND) . \( -name \*~ -o -name \*# -o -name .#\* \) -exec $(RM) {} \;
	-$(FIND) $(CLEANDIRS) \( -name \*~ -o -name \*.elc \) -exec $(RM) {} \;

$(CLEANDIRS:%=clean%):
	-$(FIND) $(@:clean%=%) \( -name \*~ -o -name \*.elc \) -exec $(RM) {} \;

cleanelc:
	$(MAKE) -C lisp $@

cleanaddcontrib:
	-$(RM) $(wildcard $(addprefix lisp/,$(notdir $(wildcard contrib/lisp/*.el))))

cleanlisp:	cleanaddcontrib
cleanlisp cleandoc:
	$(MAKE) -C $(@:clean%=%) clean

cleandocs:
	$(MAKE) -C doc clean
	-$(FIND) doc -name \*~ -exec $(RM) {} \;

cleantest:
	$(RMR) $(testdir)
