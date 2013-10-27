SHELL = /bin/sh
EMACS = emacs
FILES = $(filter-out evil-tests.el,$(filter-out evil-pkg.el,$(wildcard evil*.el)))
VERSION := $(shell sed -ne '/define-package/,$$p' evil-pkg.el | sed -ne '/^\s*"[[:digit:]]\+\(\.[[:digit:]]\+\)*"\s*$$/ s/^.*"\(.*\)".*$$/\1/p')
ELPAPKG = evil-$(VERSION)
PROFILER =
DOC = doc
TAG =
LIBS = -L lib

ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile compile-batch info pdf clean tests test emacs term terminal profiler indent elpa version

# Byte-compile Evil.
all: compile
compile: $(ELCFILES)

.depend: $(FILES)
	@echo Compute dependencies
	@rm -f .depend
	@for f in $(FILES); do \
	    sed -n "s/(require '\(evil-.*\))/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

# Byte-compile all files in one batch. This is faster than
# compiling each file in isolation, but also less stringent.
compile-batch: clean
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile ${FILES}

# Documentation.
doc: info pdf

info: clean
	cd $(DOC) && makeinfo evil.texi

pdf: clean
	cd $(DOC) && texi2pdf evil.texi

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f .depend
	cd $(DOC) && rm -f *.aux *.cp *.fn *.fns *.info *.ky *.log *.pg *.toc *.tp *.vr *.vrs

# Run tests.
# The TAG variable may specify a test tag or a test name:
#       make test TAG=repeat
# This will only run tests pertaining to the repeat system.
test:
	$(EMACS) -nw -Q -L . $(LIBS) -l evil-tests.el \
--eval "(evil-tests-initialize '(${TAG}) '(${PROFILER}))"

# Byte-compile Evil and run all tests.
tests: compile
	$(EMACS) -nw -Q -L . $(LIBS) -l evil-tests.el \
--eval "(evil-tests-initialize '(${TAG}) '(${PROFILER}))"
	rm -f *.elc .depend

# Load Evil in a fresh instance of Emacs and run all tests.
emacs:
	$(EMACS) -Q -L . $(LIBS) -l goto-chg.el -l evil-tests.el \
--eval "(evil-mode 1)" \
--eval "(evil-tests-initialize '(${TAG}) '(${PROFILER}) t)" &

# Load Evil in a terminal Emacs and run all tests.
term: terminal
terminal:
	$(EMACS) -nw -Q -L . $(LIBS) -l goto-chg.el -l evil-tests.el \
--eval "(evil-mode 1)" \
--eval "(evil-tests-initialize '(${TAG}) '(${PROFILER}) t)"

# Run all tests with profiler.
profiler:
	$(EMACS) --batch -Q -L . $(LIBS) -l goto-chg.el -l evil-tests.el \
--eval "(evil-tests-initialize '(${TAG}) (or '(${PROFILER}) t))"

# Re-indent all Evil code.
# Loads Evil into memory in order to indent macros properly.
# Also removes trailing whitespace, tabs and extraneous blank lines.
indent: clean
	$(EMACS) --batch --eval '(setq vc-handled-backends nil)' ${FILES} evil-tests.el -Q -L . $(LIBS) -l evil-tests.el \
--eval "(dolist (buffer (reverse (buffer-list))) \
(when (buffer-file-name buffer) \
(set-buffer buffer) \
(message \"Indenting %s\" (current-buffer)) \
(setq-default indent-tabs-mode nil) \
(untabify (point-min) (point-max)) \
(indent-region (point-min) (point-max)) \
(delete-trailing-whitespace) \
(untabify (point-min) (point-max)) \
(goto-char (point-min)) \
(while (re-search-forward \"\\n\\\\{3,\\\\}\" nil t) \
(replace-match \"\\n\\n\")) \
(when (buffer-modified-p) (save-buffer 0))))"

# Create an ELPA package.
elpa:
	@echo "Creating ELPA package $(ELPAPKG).tar"
	@rm -rf ${ELPAPKG}
	@mkdir ${ELPAPKG}
	@cp $(FILES) COPYING evil-pkg.el ${ELPAPKG}
	@tar cf ${ELPAPKG}.tar ${ELPAPKG}
	@rm -rf ${ELPAPKG}

# Change the version using make VERSION=x.y.z
version:
	$(EMACS) --batch --eval '(setq vc-handled-backends nil)' ${FILES} evil-tests.el -Q \
--eval "\
(progn \
  (find-file \"evil-vars.el\") \
  (when (re-search-forward \"^(defconst evil-version \\\"\\\\([-_.[:word:]]*\\\\)\\\"\" nil t) \
    (replace-match \"${VERSION}\" t t nil 1)) \
  (find-file \"evil-pkg.el\") \
  (goto-line 3) \
  (when (and (string-match-p \"[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\" \"${VERSION}\") \
             (re-search-forward \"\\\"\\\\([-_.[:word:]]*\\\\)\\\"\" nil t)) \
    (replace-match \"${VERSION}\" t t nil 1)) \
  (dolist (buffer (reverse (buffer-list))) \
    (when (buffer-file-name buffer) \
      (set-buffer buffer) \
      (goto-char (point-min)) \
      (when (re-search-forward \"^;;[[:space:]]*Version:[[:space:]]*\\\\([-_.[:word:]]*\\\\)\" nil t) \
        (replace-match \"${VERSION}\" t t nil 1)) \
      (when (buffer-modified-p) (save-buffer 0))))) \
"


