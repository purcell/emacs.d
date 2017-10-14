### Makefile for contribs
#
# This file is in the public domain.

EMACS=emacs
LISP=sbcl

LOAD_PATH=-L . -L ..
CONTRIBS = $(patsubst slime-%.el,%,$(wildcard slime-*.el))
CONTRIB_TESTS = $(patsubst test/slime-%-tests.el,%,$(wildcard test/slime-*.el))
SLIME_VERSION=$(shell grep "Version:" ../slime.el | grep -E -o "[0-9.]+$$")

ELFILES := $(shell find . -type f -iname "*.el")
ELCFILES := $(patsubst %.el,%.elc,$(ELFILES))

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)
	$(EMACS) -Q --batch $(LOAD_PATH) \
	        --eval "(batch-byte-recompile-directory 0)" .

# ELPA builds for contribs
#
$(CONTRIBS:%=elpa-%): CONTRIB=$(@:elpa-%=%)
$(CONTRIBS:%=elpa-%): CONTRIB_EL=$(CONTRIB:%=slime-%.el)
$(CONTRIBS:%=elpa-%): CONTRIB_CL=$(CONTRIB:%=swank-%.lisp)
$(CONTRIBS:%=elpa-%): CONTRIB_VERSION=$(shell (                          \
	                                  grep "Version:" $(CONTRIB_EL)  \
	                                  || echo $(SLIME_VERSION)       \
	                                ) | grep -E -o "[0-9.]+$$" )
$(CONTRIBS:%=elpa-%): PACKAGE=$(CONTRIB:%=slime-%-$(CONTRIB_VERSION))
$(CONTRIBS:%=elpa-%): PACKAGE_EL=$(CONTRIB:%=slime-%-pkg.el)
$(CONTRIBS:%=elpa-%): ELPA_DIR=elpa/$(PACKAGE)
$(CONTRIBS:%=elpa-%): compile
	elpa_dir=$(ELPA_DIR)
	mkdir -p $$elpa_dir;                                             \
	emacs --batch $(CONTRIB_EL)                                      \
	--eval "(require 'cl-lib)"                                       \
	--eval "(search-forward \"define-slime-contrib\")"               \
	--eval "(up-list -1)"                                            \
	--eval "(pp                                                      \
	        (pcase (read (point-marker))                             \
	          (\`(define-slime-contrib ,name ,docstring . ,rest)     \
	           \`(define-package ,name \"$(CONTRIB_VERSION)\"        \
	        ,docstring                                               \
	        ,(cons '(slime \"$(SLIME_VERSION)\")                     \
	         (cl-loop for form in rest                               \
	             when (eq :slime-dependencies (car form))            \
	             append (cl-loop for contrib in (cdr form)           \
	                             if (atom contrib)                   \
	                             collect                             \
	                               \`(,contrib \"$(SLIME_VERSION)\") \
	                             else                                \
	                             collect contrib))))))))"   >        \
	$$elpa_dir/$(PACKAGE_EL);                                        \
	cp $(CONTRIB_EL) $$elpa_dir;                                     \
	[ -r $(CONTRIB_CL) ] && cp $(CONTRIB_CL) $$elpa_dir;             \
	ls $$elpa_dir
	cd elpa && tar cvf $(PACKAGE).tar $(PACKAGE)
	rm -rf $(ELPA_DIR)

elpa-all: $(CONTRIBS:%=elpa-%)

$(CONTRIB_TESTS:%=check-%): CONTRIB_NAME=$(patsubst check-%,slime-%,$@)
$(CONTRIB_TESTS:%=check-%): SELECTOR=(quote (tag contrib))
$(CONTRIB_TESTS:%=check-%): compile
	$(EMACS) -Q --batch $(LOAD_PATH) -L test			   \
	        --eval "(require (quote slime))"			   \
	        --eval "(slime-setup (quote ($(CONTRIB_NAME))))"	   \
	        --eval "(require					   \
                         (intern					   \
			  (format \"%s-tests\" (quote $(CONTRIB_NAME)))))" \
	        --eval '(setq inferior-lisp-program "$(LISP)")'		   \
	        --eval "(slime-batch-test $(SELECTOR))"

check-all: $(CONTRIB_TESTS:%=check-%)

check-fancy: compile
	$(EMACS) -Q --batch  $(LOAD_PATH) -L test			\
	        --eval "(setq debug-on-error t)"			\
	        --eval "(require (quote slime))"			\
	        --eval "(slime-setup (quote (slime-fancy)))"		\
	        --eval "(mapc (lambda (sym)				\
	                         (require				\
	                           (intern (format \"%s-tests\" sym))	\
	                           nil t))				\
	                      (slime-contrib-all-dependencies		\
	                        (quote slime-fancy)))"			\
	        --eval '(setq inferior-lisp-program "$(LISP)")'		\
	        --eval '(slime-batch-test (quote (tag contrib)))'

