#!/bin/bash
pkg=elpa-mirror-1.1.2
mkdir $pkg
cp README.org *.el *.js index.html $pkg
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/
