EMACS ?= emacs

SYSTEMD_PATH := $(HOME)/.config/systemd/user
SYSTEMD_SCRIPT := deamonscripts/emacs.service
ifeq "$(shell which systemctl)" "/usr/bin/systemctl"
	USING_SYSTEMD = "true"
endif

DOTGNUS_SRC := .gnus
DOTGNUS_DEST := $(HOME)/.gnus
DOTGNUS_DEST_EXISTS := $(wildcard $(DOTGNUS_DEST))

install:
   ifdef USING_SYSTEMD
	mkdir -p "$(SYSTEMD_PATH)"
	cp "$(SYSTEMD_SCRIPT)" "$(SYSTEMD_PATH)/"
   endif
   ifneq "$(DOTGNUS_DEST_EXISTS)" "$(DOTGNUS_DEST)"
	cp "$(DOTGNUS_SRC)" "$(DOTGNUS_DEST)"
   endif

clean:
	rm $(SYSTEMD_PATH)/$(shell echo "$(SYSTEMD_SCRIPT)" | cut -d'/' -f2)
