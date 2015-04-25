Eproject is an extension that lets you group related files together
as projects.  It aims to be as unobtrusive as possible -- no new
files are created (or required to exist) on disk, and buffers that
aren't a member of a project are not affected in any way.

The main starting point for eproject is defining project types.
There is a macro for this, define-project-type, that accepts four
arguments, the type name (a symbol), a list of supertypes (for
inheriting properties), a form that is executed to determine
whether a file is a member of a project, and then a free-form
property list.  An example will clear things up.

Let's create a "perl" project type, for Perl projects that have a
Makefile.PL.

(define-project-type perl (generic)
  (look-for "Makefile.PL")
  :relevant-files ("\\.pm$" "\\.t$"))

Now when you open a file and somewhere above in the directory tree
there is a Makefile.PL, it will be a "perl project".

There are a few things you get with this.  A hook called
perl-project-file-visit-hook will be run, and the buffer will have
the "eproject-mode" minor-mode turned on.  You can also read and
set metadata via the eproject-attribute and
eproject-add-project-metadatum calls.

(This is mostly helpful to Lisp programmers rather than end-users;
if you want tools for visiting and managing projects (and ibuffer
integration), load `eproject-extras'.  These extras are great
examples of the eproject API in action, so please take a look even
if you don't want those exact features.)

Let's look at the mechanics of the define-project-type call.  The
first argument is the name of the project type -- it can be any
symbol.  The next argument is a list of other projects types that
this project will inherit from.  That means that if you call
eproject-get-project-metadatum and the current project doesn't
define a value, we'll look at the supertypes until we get something
non-nil.  Usually you will want to set this to (generic), which
will make your type work correctly even if you don't define any of
your own metadata.

The next argument is a form that will be executed with the filename
that was just opened bound to FILE.  It is expected to return the
project root, or nil if FILE is not in a project of this type.  The
look-for function will look up the directory tree for a file that
is named the same as its argument (see the docstring for
`eproject--look-for-impl' for all the details).  You can write any
Lisp here you like; we'll see some more examples later.  (You only
get one form, so if you need to execute more than one, just wrap it
in a progn.)

The final (&rest-style) argument is a property list of initial project
metadata.  You can put anything you want here, as long as it is in the
form of a property list (keyword, value, keyword, value, ...).

After this form runs, eproject will be able to recognize files in
the type of the project you defined.  It also creates a hook named
<type>-project-file-visit-hook.  You can do anything you want here,
including access (eproject-type) and (eproject-root).

As an example, in my perl-project-file-visit-hook, I do this:

(lambda ()
  (ignore-errors
    (stylish-repl-eval-perl
     (format "use lib '%s'" (car (perl-project-includes)))))))

This will add the library directory of this project to my current
stylish-repl session, so that I can use my project in the REPL
immediately.  (I do something similar for Lisp + SLIME projects)

That's basically all there is.  eproject is designed to be minimal and
extensible, so I hope it meets your needs.

Please e-mail me or find me on #emacs (jrockway) if you have
questions.  If you'd like to send a patch (always appreciated),
please diff against the latest git version, available by running:

$ git clone git://github.com/jrockway/eproject

Share and enjoy.
