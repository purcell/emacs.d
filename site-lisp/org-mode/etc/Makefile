ETCDIRS = styles schema
-include local.mk	# optional local customization

.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

.PHONY:		all install clean cleanall clean-install

all:

install:	$(ETCDIRS)
	for dir in $? ; do \
	  if [ ! -d $(DESTDIR)$(datadir)/$${dir} ] ; then \
	    $(MKDIR) $(DESTDIR)$(datadir)/$${dir} ; \
	  fi ; \
	  $(CP) $${dir}/* $(DESTDIR)$(datadir)/$${dir} ; \
	done ;

clean:

cleanall:

clean-install:	$(ETCDIRS)
	for dir in $? ; do \
	  if [ -d $(DESTDIR)$(datadir)/$${dir} ] ; then \
	    $(RMR) $(DESTDIR)$(datadir)/$${dir} ; \
	  fi ; \
	done ;
