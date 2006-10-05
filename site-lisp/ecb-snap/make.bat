@echo off
REM This batchfile byte-compiles the ECB lisp files.

REM Copyright (C) 2000 - 2005 Jesper Nordenberg,
REM                           Klaus Berndl,
REM                           Kevin A. Burton,
REM                           Free Software Foundation, Inc.

REM Author: Jesper Nordenberg <mayhem@home.se>
REM         Klaus Berndl <klaus.berndl@sdm.de>
REM         Kevin A. Burton <burton@openprivacy.org>
REM Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
REM             Kevin A. Burton <burton@openprivacy.org>
REM Keywords: browser, code, programming, tools
REM Created: 2001

REM This program is free software; you can redistribute it and/or modify it under
REM the terms of the GNU General Public License as published by the Free Software
REM Foundation; either version 2, or (at your option) any later version.

REM This program is distributed in the hope that it will be useful, but WITHOUT
REM ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
REM FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
REM details.

REM You should have received a copy of the GNU General Public License along with
REM GNU Emacs; see the file COPYING.  If not, write to the Free Software
REM Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

REM $Id: make.bat,v 1.28 2005/02/28 11:31:51 berndl Exp $


REM Make sure you byte-compile ECB with the semantic, eieio and
REM speedbar-version you load into (X)Emacs (see below)!

REM =======================================================================
REM user configurable section

REM Define here the correct paths to your (X)Emacs-executable and the
REM required packages (use always FORWARD SLASHES in the paths!)

REM TODO: Supporting the new cedet 1.0 library (see Makefile)

set EMACS=C:/Programme/emacs-21/bin/emacs.exe
set SEMANTIC=../semantic
set EIEIO=../eieio
set SPEEDBAR=../speedbar

REM Call "make" to byte-compile the ECB.
REM If there are any warning messages during byte-compilation (normally
REM there aren't any) you can savely ignore them!

REM end of user configurable section
REM =======================================================================


REM Do not change anything below!

echo Byte-compiling ECB with LOADPATH= %SEMANTIC% %EIEIO% %SPEEDBAR%

if exist ecb-compile-script-init del ecb-compile-script-init
if exist ecb.elc del *.elc

echo (add-to-list 'load-path nil) > ecb-compile-script-init
echo (add-to-list 'load-path "%SEMANTIC%") >> ecb-compile-script-init
echo (add-to-list 'load-path "%EIEIO%") >> ecb-compile-script-init
echo (add-to-list 'load-path "%SPEEDBAR%") >> ecb-compile-script-init
echo (require 'ecb) >> ecb-compile-script-init
echo (setq debug-on-error t) >> ecb-compile-script-init

%EMACS% -batch -no-site-file -l ecb-compile-script-init --eval "(ecb-byte-compile t)"

del ecb-compile-script-init

REM End of make.bat
