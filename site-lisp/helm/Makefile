# makefile for helm.

# Author: Michael Markert.
# Copyright (C) 2011~2012, Michael Markert, all rights reserved.

## This file is NOT part of GNU Emacs
##
## License
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.

# emacs invocation
EMACS		:= emacs -Q -batch

# additional emacs loadpath
LOADPATH	:= -L .

# files to compile
EL			:= $(wildcard helm*.el)

# compiled files
ELC			:= $(EL:.el=.elc)

.PHONY: clean batch-compile

all: clean batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# compile needed files
compile: $(ELC)

# compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(EL)

# remove all generated files
clean:
	rm -f $(ELC)
