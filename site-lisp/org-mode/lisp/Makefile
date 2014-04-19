.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

ifneq ($(ORG_ADD_CONTRIB),)
  _ORG_ADD_EL_ := \
	$(notdir \
	$(wildcard \
	$(addsuffix .el, \
	$(addprefix ../contrib/lisp/, \
	$(basename \
	$(notdir $(ORG_ADD_CONTRIB)))))))
endif

LISPV 	:= org-version.el
LISPI 	:= org-loaddefs.el
LISPA 	:= $(LISPV) $(LISPI)
LISPB 	:= $(LISPA:%el=%elc) org-install.elc
LISPF 	:= $(filter-out $(LISPA),$(sort $(wildcard *.el) $(_ORG_ADD_EL_)))
LISPC 	:= $(filter-out $(LISPB) $(LISPN:%el=%elc),$(LISPF:%el=%elc))
_ORGCM_ := dirall single source slint1 slint2
-include local.mk

.PHONY:	all compile compile-dirty \
	$(_ORGCM_) $(_ORGCM_:%=compile-%) \
	autoloads addcontrib \
	install clean cleanauto cleanall cleanelc clean-install

# do not clean here, done in toplevel make
all compile compile-dirty::	 autoloads
ifeq ($(filter-out $(_ORGCM_),$(ORGCM)),)
	$(MAKE) compile-$(ORGCM)
else
	$(error ORGCM has illegal value $(ORGCM) (valid: $(_ORGCM_)))
endif

compile-dirall:	dirall
compile-single: single $(LISPC)
compile-source:	source dirall
compile-slint1:	dirall slint1
compile-slint2:	source dirall slint1

# internal
dirall:
	@$(info ==================== $@ ====================)
	@$(ELCDIR)
single:
	@$(info ==================== $@ ====================)
source: cleanelc
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(MAKE) $(elc) && $(RM) $(elc);)
slint1:
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(RM) $(elc); $(MAKE) $(elc);)

%.elc:	%.el
	@$(info Compiling single $(abspath $<)...)
	-@$(ELC) $<

addcontrib:
ifneq ($(ORG_ADD_CONTRIB),)
	$(CP) $(addprefix ../contrib/lisp/,$(_ORG_ADD_EL_)) .
endif

autoloads:	cleanauto addcontrib $(LISPI) $(LISPV)

$(LISPV):	$(LISPF)
	@echo "org-version: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_VERSION)

$(LISPI):	$(LISPV) $(LISPF)
	@echo "org-loaddefs: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_INSTALL)

install:	 compile $(LISPF)
	if [ ! -d $(DESTDIR)$(lispdir) ] ; then \
	  $(MKDIR) $(DESTDIR)$(lispdir) ; \
	fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPA) $(DESTDIR)$(lispdir)

cleanauto clean cleanall::
	$(RM) $(LISPA) $(LISPB)
clean cleanall cleanelc::
	$(RM) *.elc

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* $(DESTDIR)$(lispdir)/ox*.el* ; \
	fi ;
