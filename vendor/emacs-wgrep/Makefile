EMACS = emacs

check: compile
	$(EMACS) -q -batch -L . -l wgrep.el -l wgrep-test.el -f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L . -l wgrep.elc -l wgrep-test.el -f ert-run-tests-batch-and-exit

check2: check download-external
	$(EMACS) -q -batch \
		-L . -L ./ext -l wgrep.el -l wgrep-ag.el -l wgrep-helm.el -l wgrep-ack.el -l wgrep-pt.el \
		-l ext/ag.el \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"
	$(EMACS) -q -batch \
		-L . -L ./ext -l wgrep.elc -l wgrep-ag.elc -l wgrep-helm.elc -l wgrep-ack.elc -l wgrep-pt.elc \
		-l ext/ag.el \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"

download-external: clean-ext
	mkdir -p ext
	cd ext && wget "https://github.com/Wilfred/ag.el/raw/master/ag.el"
	cd ext && wget "https://github.com/magnars/dash.el/raw/master/dash.el"
	cd ext && wget "https://github.com/magnars/s.el/raw/master/s.el"

compile:
	$(EMACS) -q -batch -L . -f batch-byte-compile \
		wgrep.el wgrep-ag.el wgrep-helm.el wgrep-ack.el wgrep-pt.el

clean-ext:
	rm -rf ext

clean:
	rm *.elc
