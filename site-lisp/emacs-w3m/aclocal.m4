AC_DEFUN(AC_SET_VANILLA_FLAG,
 [dnl Determine arguments to run Emacs as vanilla.
  retval=`echo ${EMACS}| ${EGREP} xemacs| ${EGREP} -v '^$'`
  if test -z "${retval}"; then
	VANILLA_FLAG="-q -no-site-file --no-unibyte"
  else
	VANILLA_FLAG="-vanilla"
  fi
  AC_SUBST(VANILLA_FLAG)])

AC_DEFUN(AC_SET_XEMACSDEBUG,
 [dnl Set the XEMACSDEBUG environment variable, which is eval'd when
  dnl XEmacs 21.5 starts, in order to suppress warnings for Lisp shadows
  dnl when XEmacs 21.5 starts.
  if test "${VANILLA_FLAG}" = "-vanilla"; then
	XEMACSDEBUG='XEMACSDEBUG='\''(setq log-warning-minimum-level (quote error))'\'' '
  else
	XEMACSDEBUG=
  fi
  AC_SUBST(XEMACSDEBUG)])

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${XEMACSDEBUG}${EMACS}' '${VANILLA_FLAG}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	eval ${XEMACSDEBUG}${EMACS}' '${VANILLA_FLAG}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	retval="`cat ${OUTPUT}`"
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1="${retval}"
])
$1="${EMACS_cv_SYS_$1}"
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_PATH_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test x${EMACS} = xt && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset EMACS_cv_SYS_flavor;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, xemacs...]],
   [if test "${withval}" = yes -o -z "${withval}"; then
      AC_PATH_PROGS(EMACS, emacs xemacs, emacs)
    else
      AC_PATH_PROG(EMACS, ${withval}, ${withval}, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [XEMACS=xemacs]],
   [if test x$withval = xyes -o x$withval = x; then
      AC_PATH_PROG(EMACS, xemacs, xemacs, xemacs)
    else
      AC_PATH_PROG(EMACS, $withval, $withval, xemacs)
    fi])
  test -z "${EMACS}" && AC_PATH_PROGS(EMACS, emacs xemacs, emacs)
  AC_SUBST(EMACS)
  AC_SET_VANILLA_FLAG
  AC_SET_XEMACSDEBUG

  AC_MSG_CHECKING([what a flavor does ${EMACS} have])
  AC_EMACS_LISP(flavor,
    (if (featurep (quote xemacs))\
	(if (and\
	     (condition-case nil\
		 (progn\
		   (unless (or itimer-process itimer-timer)\
		     (itimer-driver-start))\
		   (let* ((inhibit-quit t)\
			  (ctime (current-time))\
			  (itimer-timer-last-wakeup\
			   (prog1\
			       ctime\
			     (setcar ctime (1- (car ctime)))))\
			  (itimer-list nil)\
			  (itimer (start-itimer \"*testing*\"\
						(function ignore) 5)))\
		     (sleep-for 0.1)\
		     (prog1\
			 (> (itimer-value itimer) 0)\
		       (delete-itimer itimer))))\
	       (error nil))\
	     (string-match\
	      (concat (vector 94 92 40 63 58 32 43 92 41 42 92 91 92 93))\
	      (concat (vector 32 91 93)))\
	     (or (not (executable-find \"cat\"))\
		 (with-temp-buffer\
		   (insert \"foo\")\
		   (backward-char)\
		   (call-process-region (1- (point)) (point) \"cat\" t t)\
		   (goto-char (point-min))\
		   (looking-at \"foo\"))))\
	    \"XEmacs\"\
	  (let ((v (emacs-version)))\
	    (if (string-match (char-to-string 41) v)\
		(substring v 0 (match-end 0))\
	      \"Old XEmacs\")))\
      (concat \"Emacs \"\
	      (mapconcat (function identity)\
			 (nreverse\
			  (cdr (nreverse\
				(split-string emacs-version\
					      (concat (vector 92 46))))))\
			 \".\"))),
    noecho)
  case "${flavor}" in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  Emacs\ 2[[1234]]\.*)
    EMACS_FLAVOR=emacs;;
  *)
    EMACS_FLAVOR=unsupported;;
  esac
  AC_MSG_RESULT(${flavor})
  if test ${EMACS_FLAVOR} = unsupported; then
    AC_MSG_ERROR(${flavor} is not supported.)
    exit 1
  fi])

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine PACKAGEDIR.
  AC_EMACS_LISP(PACKAGEDIR,
    (let ((prefix \"${prefix}\")\
	  (dirs (append\
		 (cond ((boundp (quote early-package-hierarchies))\
			(append (if early-package-load-path\
				    early-package-hierarchies)\
				(if late-package-load-path\
				    late-package-hierarchies)\
				(if last-package-load-path\
				    last-package-hierarchies)))\
		       ((boundp (quote early-packages))\
			(append (if early-package-load-path\
				    early-packages)\
				(if late-package-load-path\
				    late-packages)\
				(if last-package-load-path\
				    last-packages))))\
		 (if (and (boundp (quote configure-package-path))\
			  (listp configure-package-path))\
		     (delete \"\" configure-package-path))))\
	  package-dir)\
      (while (and dirs (not package-dir))\
	(if (file-directory-p (car dirs))\
	    (setq package-dir (car dirs)\
		  dirs (cdr dirs))))\
      (if package-dir\
	  (progn\
	    (if (string-match \"/\$\" package-dir)\
		(setq package-dir (substring package-dir 0\
					     (match-beginning 0))))\
	    (if (and prefix\
		     (progn\
		       (setq prefix (file-name-as-directory prefix))\
		       (eq 0 (string-match (regexp-quote prefix)\
					   package-dir))))\
		(replace-match \"\$(prefix)/\" nil nil package-dir)\
	      package-dir))\
	\"NONE\")),
    noecho)])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for PACKAGEDIR.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	AC_EXAMINE_PACKAGEDIR
      else
	PACKAGEDIR="${withval}"
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test -z "${PACKAGEDIR}"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT(${PACKAGEDIR})
    fi
  else
    PACKAGEDIR=NONE
  fi
  AC_SUBST(PACKAGEDIR)])

AC_DEFUN(AC_PATH_LISPDIR, [
  if test ${EMACS_FLAVOR} = emacs; then
	tribe=emacs
  else
	tribe=${EMACS_FLAVOR}
  fi
  AC_MSG_CHECKING([prefix for ${EMACS}])
  if test "${prefix}" = NONE; then
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),noecho)
	prefix=${EMACS_cv_SYS_prefix}
  fi
  AC_MSG_RESULT(${prefix})
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      where lisp files should go
                          (use --with-packagedir for XEmacs package)],
    lispdir="${withval}")
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "${lispdir}"; then
    dnl Set the default value.
    theprefix="${prefix}"
    if test "${theprefix}" = NONE; then
	theprefix=${ac_default_prefix}
    fi
    lispdir="\$(datadir)/${tribe}/site-lisp/w3m"
    for thedir in share lib; do
	potential=
	dnl The directory name should be quoted because it might contain spaces.
	if test -d "${theprefix}/${thedir}/${tribe}/site-lisp"; then
	   lispdir="\$(prefix)/${thedir}/${tribe}/site-lisp/w3m"
	   break
	fi
    done
  fi
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_RESULT(${lispdir}/
         (it will be ignored when \"make install-package\" is done))
  else
    AC_MSG_RESULT(${lispdir}/)
  fi
  AC_SUBST(lispdir)])

AC_DEFUN(AC_PATH_ICONDIR,
 [dnl Examin icon directory.

  dnl Ignore cache.
  unset EMACS_cv_SYS_icondir;

  if test ${EMACS_FLAVOR} = xemacs -o ${EMACS_FLAVOR} = emacs; then
    AC_ARG_WITH(icondir,
     [  --with-icondir=ICONDIR  directory for icons [\$(data-directory)/images/w3m]],
      ICONDIR="${withval}")
    AC_MSG_CHECKING([where icon files should go])
    if test -z "${ICONDIR}"; then
      dnl Set the default value.
      AC_EMACS_LISP(icondir,
        (let ((prefix \"${prefix}\")\
	      (default (expand-file-name \"images/w3m\" data-directory)))\
	  (if (and prefix\
		   (progn\
		     (setq prefix (file-name-as-directory prefix))\
		     (eq 0 (string-match (regexp-quote prefix) default))))\
	      (replace-match \"\$(prefix)/\" nil nil default)\
	    default)),
	${prefix},noecho)
      ICONDIR=${EMACS_cv_SYS_icondir}
    fi
    if test ${EMACS_FLAVOR} = xemacs; then
      AC_MSG_RESULT(${ICONDIR}/
         (it will be ignored when \"make install-package\" is done))
    else
      AC_MSG_RESULT(${ICONDIR})
    fi
  else
    ICONDIR=NONE
  fi
  AC_SUBST(ICONDIR)])

AC_DEFUN(AC_ADD_LOAD_PATH,
 [dnl Check for additional load path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATHs    specify additional PATHs for load-path
                          use colons to separate directory names],
   [AC_MSG_CHECKING([where to find the additional elisp libraries])
      if test "x${withval}" != xyes -a "x${withval}" != x; then
	ADDITIONAL_LOAD_PATH="${withval}"
      else
	if test x"$USER" != xroot -a x"$HOME" != x -a -f "$HOME"/.emacs; then
          ADDITIONAL_LOAD_PATH=`${XEMACSDEBUG}${EMACS} -batch -l "$HOME"/.emacs -l w3mhack.el NONE -f w3mhack-load-path 2>/dev/null | $EGREP -v '^$'`
        else
          ADDITIONAL_LOAD_PATH=`${XEMACSDEBUG}${EMACS} -batch -l w3mhack.el NONE -f w3mhack-load-path 2>/dev/null | $EGREP -v '^$'`
        fi
      fi
      AC_MSG_RESULT(${ADDITIONAL_LOAD_PATH})],
    ADDITIONAL_LOAD_PATH=NONE)
  AC_ARG_WITH(attic,
   [  --with-attic            use attic libraries for compiling [default: no]
                          (it does not mean installing attic libraries)],
   [if test "x${withval}" = xyes; then
      if test x"$ADDITIONAL_LOAD_PATH" = xNONE; then
        ADDITIONAL_LOAD_PATH=`pwd`/attic
      else
        ADDITIONAL_LOAD_PATH=${ADDITIONAL_LOAD_PATH}:`pwd`/attic
      fi
    fi])
  retval=`eval ${XEMACSDEBUG}${EMACS}' '${VANILLA_FLAG}' -batch -l w3mhack.el '${ADDITIONAL_LOAD_PATH}' -f w3mhack-print-status 2>/dev/null | $EGREP -v '\''^$'\'`
  if test x"$retval" != xOK; then
    AC_MSG_ERROR([Process couldn't proceed.  See the above messages.])
  fi
  AC_SUBST(ADDITIONAL_LOAD_PATH)])
