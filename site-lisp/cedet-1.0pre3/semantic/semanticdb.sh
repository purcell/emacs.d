#!/bin/sh
# semanticdb.sh --- Build a semantic cache for input arguments
#
# Copyright (C) 2002, 2004 Eric M. Ludlam
#
# Author: Eric M. Ludlam <zappo@gnu.org>
# Keywords: tags
# X-RCS: $Id: semanticdb.sh,v 1.2 2004/08/03 01:35:59 zappo Exp $
#
# This file is not part of GNU Emacs.
#
# Semanticdb is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
# 
# Commentary:
#
# Emacs usually builds a semantic cache on the fly.  If you want to use
# a tool that accesses database files without having to visit all the files, 
# however, you should use this script BEFORE starting Emacs.
#

if [ -z "$SEMANTIC_PATH" ]; then
    # Use reverse sort to sneakilly get the most recent version number.
    shopt -s nullglob
    # The extra * allow the shell to strip out junk
    files=`ls -d ~/lisp/cedet/semantic*/ ~/lisp/cedet-*/semantic/ ~/cedet/semantic*/ ~/cedet-*/semantic/`
    for i in $files; do
        if [ -d $i ]; then
          loadpath=$i
        fi
    done
else
    loadpath="$SEMANTIC_PATH"
fi

if [ -z "$SEMANTIC_EMACS" ]; then
    emacs="emacs"
else
    emacs="$SEMANTIC_EMACS"
fi

files=$*

exec $emacs -batch -l "${loadpath}/semanticdb-mk.el" $files

#end