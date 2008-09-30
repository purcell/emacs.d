dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.


# serial 1

AC_DEFUN(AM_PATH_LISPDIR,
 [# If set to t, that means we are running in a shell under Emacs.
  # If you have an Emacs named "t", then use the full path.
  test "$EMACS" = t && EMACS=
  AC_PATH_PROGS(EMACS, emacs xemacs, no)
  if test $EMACS != "no"; then
    AC_MSG_CHECKING([where .elc files should go])
    dnl Set default value
    lispdir="\$(datadir)/emacs/site-lisp"
    emacs_flavor=`echo "$EMACS" | sed -e 's,^.*/,,'`
    if test "x$prefix" = "xNONE"; then
      if test -d $ac_default_prefix/share/$emacs_flavor/site-lisp; then
	lispdir="\$(prefix)/share/$emacs_flavor/site-lisp"
      else
	if test -d $ac_default_prefix/lib/$emacs_flavor/site-lisp; then
	  lispdir="\$(prefix)/lib/$emacs_flavor/site-lisp"
	fi
      fi
    else
      if test -d $prefix/share/$emacs_flavor/site-lisp; then
	lispdir="\$(prefix)/share/$emacs_flavor/site-lisp"
      else
	if test -d $prefix/lib/$emacs_flavor/site-lisp; then
	  lispdir="\$(prefix)/lib/$emacs_flavor/site-lisp"
	fi
      fi
    fi
    AC_MSG_RESULT($lispdir)
  fi
  AC_SUBST(lispdir)])

