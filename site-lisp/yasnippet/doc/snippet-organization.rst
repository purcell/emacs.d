===================
Organizing snippets
===================

.. _Organizing Snippets: snippet-organization.html
.. _Expanding Snippets: snippet-expansion.html
.. _Writing Snippets: snippet-development.html
.. _The YASnippet Menu: snippet-menu.html

.. contents::

Loading snippets
================

Snippet definitions are stored in files in the filesystem. Unless you
use the simpler `bundle version <index.html@installation>`_), these
are arranged so that YASnippet can load them into *snippet
tables*. The triggering mechanisms (see `Expanding snippets`_) will
look up these snippet tables and (hopefully) expand the snippet you
intended.

The non-bundle version of YASnippet, once unpacked, comes with a full
directory of snippets, which you can copy somewhere and use. You can
also create or download more directories.

Once these directories are in place reference them in the variable
``yas/root-directory`` and load them with ``yas/load-directory``:

.. sourcecode:: common-lisp

  ;; Develop and keep personal snippets under ~/emacs.d/mysnippets
  (setq yas/root-directory "~/emacs.d/mysnippets")

  ;; Load the snippets
  (yas/load-directory yas/root-directory)

The point in using ``yas/root-directory`` (as opposed to calling
``yas/load-directory`` directly) is considering "~/emacs.d/mysnippets"
for snippet development, so you can use commands like
``yas/new-snippet`` and others described in section `Writing
Snippets`_.

You can make this variable a list and store more items into it:

.. sourcecode:: common-lisp

  ;; Develop in ~/emacs.d/mysnippets, but also
  ;; try out snippets in ~/Downloads/interesting-snippets
  (setq yas/root-directory '("~/emacs.d/mysnippets"
                             "~/Downloads/interesting-snippets"))

  ;; Map `yas/load-directory' to every element
  (mapc 'yas/load-directory yas/root-directory)

In this last example, the all the directories are loaded and their
snippets considered for expansion. However development still happens
in the first element, "~/emacs.d/mysnippets".

Organizing snippets
===================

Once you've setup ``yas/root-directory`` , you can store snippets
inside sub-directories of these directories.

Snippet definitions are put in plain text files. They are arranged
by sub-directories, and the snippet tables are named after these
directories.

The name corresponds to the Emacs mode where you want expansion to
take place. For example, snippets for ``c-mode`` are put in the
``c-mode`` sub-directory.

The ``.yas.parents`` file
-------------------------

It's very useful to have certain modes share snippets between
themselves. To do this, choose a mode subdirectory and place a
``.yas-parents`` containing a whitespace-separated list of other
mode names. When you reload those modes become parents of the
original mode.

.. sourcecode:: text

  $ tree
  .
  |-- c-mode
  |   |-- .yas-parents    # contains "cc-mode text-mode" 
  |   `-- printf
  |-- cc-mode
  |   |-- for
  |   `-- while
  |-- java-mode
  |   |-- .yas-parents    # contains "cc-mode text-mode"
  |   `-- println
  `-- text-mode
      |-- email
      `-- time

The ``.yas-make-groups`` file
-----------------------------

.. image:: images/menu-groups.png
   :align: right

If you place an empty plain text file ``.yas-make-groups`` inside one
of the mode directories, the names of these sub-directories are
considered groups of snippets and `The YASnippet Menu`_ is organized
much more cleanly, as you can see in the image.

Another alternative way to achieve this is to place a ``# group:``
directive inside the snippet definition. See `Writing Snippets`_.

.. sourcecode:: text

  $ tree ruby-mode/
  ruby-mode/
  |-- .yas-make-groups
  |-- collections
  |   |-- each
  |   `-- ...
  |-- control structure
  |   |-- forin
  |   `-- ...
  |-- definitions
  |   `-- ...
  `-- general
      `-- ...


YASnippet bundle
================

The most convenient way to define snippets for YASnippet is to put
them in a directory arranged by the mode and use
``yas/load-directory`` to load them.

However, this might slow down the Emacs start-up speed if you have many
snippets. You can use ``yas/define-snippets`` to define a bunch of
snippets for a particular mode in an Emacs-lisp file.

Since this is hard to maintain, there's a better way: define your
snippets in directory and then call ``M-x yas/compile-bundle`` to
compile it into a bundle file when you modified your snippets.

The release bundle of YASnippet is produced by
``yas/compile-bundle``. The bundle uses ``yas/define-snippets`` to
define snippets. This avoids the IO and parsing overhead when loading
snippets.

Further more, the generated bundle is a stand-alone file not depending
on ``yasnippet.el``. The released bundles of YASnippet are all
generated this way.

See the internal documentation for these functions

* ``M-x describe-function RET yas/define-snippets RET`` 
* ``M-x describe-function RET yas/compile-bundle RET``.

Customizable variables
======================

``yas/root-directory``
----------------------

Root directory that stores the snippets for each major mode.

If you set this from your .emacs, can also be a list of strings,
for multiple root directories. If you make this a list, the first
element is always the user-created snippets directory. Other
directories are used for bulk reloading of all snippets using
``yas/reload-all``

``yas/ignore-filenames-as-triggers``
------------------------------------
  
If non-nil, don't derive tab triggers from filenames.

This means a snippet without a ``# key:`` directive wont have a tab
trigger.

..  LocalWords:  html YASnippet filesystem yas sourcecode setq mapc printf perl
..  LocalWords:  println cperl forin filenames filename ERb's yasnippet Avar el
..  LocalWords:  rjs RET
