#!/bin/sh

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

trap EXIT

for emacs in xemacs ; do # emacs-20.7 emacs-21.3.50 xemacs ; do
  for lisp in 'cmucl -noinit' sbcl lispworks-personal-4300 'clisp -K full' acl5; do
  echo testing: $emacs $lisp dribble.$emacs_$lisp result.$emacs_$lisp 
  test.sh $emacs "$lisp" "dribble.${emacs}_${lisp}" "result.${emacs}_${lisp}"
 done
done
    