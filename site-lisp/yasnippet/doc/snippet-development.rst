================
Writing snippets
================

.. _Organizing Snippets: snippet-organization.html
.. _Expanding Snippets: snippet-expansion.html
.. _Writing Snippets: snippet-development.html
.. _The YASnippet Menu: snippet-menu.html

.. contents::

Snippet development
===================

Quickly finding snippets
------------------------

There are some ways you can quickly find a snippet file:

* ``M-x yas/new-snippet``

  Prompts you for a snippet name, then tries to guess a suitable
  directory to store it, prompting you for creation if it does not
  exist. Finally, places you in a new buffer set to ``snippet-mode``
  so you can write your snippet.

* ``M-x yas/find-snippets``

  Lets you find the snippet file in the directory the snippet was
  loaded from (if it exists) like ``find-file-other-window``. The
  directory searching logic is similar to ``M-x yas/new-snippet``.

* ``M-x yas/visit-snippet-file``

  Prompts you for possible snippet expansions like
  ``yas/insert-snippet``, but instead of expanding it, takes you
  directly to the snippet definition's file, if it exists.

Once you find this file it will be set to ``snippet-mode`` (see ahead)
and you can start editing your snippet.


Using the ``snippet-mode`` major mode
-------------------------------------

There is a major mode ``snippet-mode`` to edit snippets. You can set
the buffer to this mode with ``M-x snippet-mode``. It provides
reasonably useful syntax highlighting.

Two commands are defined in this mode:

* ``M-x yas/load-snippet-buffer``

    When editing a snippet, this loads the snippet into the correct
    mode and menu. Bound to ``C-c C-c`` by default while in
    ``snippet-mode``.

* ``M-x yas/tryout-snippet``

    When editing a snippet, this opens a new empty buffer, sets it to
    the appropriate major mode and inserts the snippet there, so you
    can see what it looks like. This is bound to ``C-c C-t`` while in
    ``snippet-mode``.

There are also *snippets for writing snippets*: ``vars``, ``$f`` and
``$m`` :-).

File content
============

A file defining a snippet generally contains the template to be
expanded.

Optionally, if the file contains a line of ``# --``, the lines above
it count as comments, some of which can be *directives* (or meta
data). Snippet directives look like ``# property: value`` and tweak
certain snippets properties described below. If no ``# --`` is found,
the whole file is considered the snippet template.

Here's a typical example:

.. sourcecode:: text

  # contributor: pluskid <pluskid@gmail.com>
  # name: __...__
  # --
  __${init}__

Here's a list of currently supported directives:

``# key:`` snippet abbrev
--------------------------

This is the probably the most important directive, it's the abbreviation you
type to expand a snippet just before hitting ``yas/trigger-key``. If you don't
specify this the snippet will not be expandable through the key mechanism.

``# name:`` snippet name
------------------------

This is a one-line description of the snippet. It will be displayed in
the menu. It's a good idea to select a descriptive name for a
snippet -- especially distinguishable among similar snippets.

If you omit this name it will default to the file name the snippet was
loaded from.

``# condition:`` snippet condition
----------------------------------
This is a piece of Emacs-lisp code. If a snippet has a condition, then it
will only be expanded when the condition code evaluate to some non-nil
value.

See also ``yas/buffer-local-condition`` in `Expanding snippets`_


``# group:`` snippet menu grouping
----------------------------------

When expanding/visiting snippets from the menu-bar menu, snippets for a
given mode can be grouped into sub-menus . This is useful if one has
too many snippets for a mode which will make the menu too
long.

The ``# group:`` property only affect menu construction (See `the
YASnippet menu`_) and the same effect can be achieved by grouping
snippets into sub-directories and using the ``.yas-make-groups``
special file (for this see `Organizing Snippets`_


Refer to the bundled snippets for ``ruby-mode`` for examples on the
``# group:`` directive. Group can also be nested, e.g.  ``control
structure.loops`` tells that the snippet is under the ``loops`` group
which is under the ``control structure`` group.

``# expand-env:`` expand environment
------------------------------------

This is another piece of Emacs-lisp code in the form of a ``let``
*varlist form*, i.e. a list of lists assigning values to variables. It
can be used to override variable values while the snippet is being
expanded.

Interesting variables to override are ``yas/wrap-around-region`` and
``yas/indent-line`` (see `Expanding Snippets`_).

As an example, you might normally have ``yas/indent-line`` set to
``'auto`` and ``yas/wrap-around-region`` set to ``t``, but for this
particularly brilliant piece of ASCII art these values would mess up
your hard work. You can then use:

.. sourcecode:: text

  # name: ASCII home
  # expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
  # --
                  welcome to my
              X      humble
             / \      home,
            /   \      $0
           /     \
          /-------\
          |       |
          |  +-+  |
          |  | |  |
          +--+-+--+

``# binding:`` direct keybinding
---------------------------------

You can use this directive to expand a snippet directly from a normal
Emacs keybinding. The keybinding will be registered in the Emacs
keymap named after the major mode the snippet is active
for. 

Additionally a variable ``yas/prefix`` is set to to the prefix
argument you normally use for a command. This allows for small
variations on the same snippet, for example in this "html-mode"
snippet.

.. sourcecode:: text

  # name: <p>...</p>
  # binding: C-c C-c C-m
  # --
  <p>`(when yas/prefix "\n")`$0`(when yas/prefix "\n")`</p>

This binding will be recorded in the keymap
``html-mode-map``. To expand a paragraph tag newlines, just
press ``C-u C-c C-c C-m``. Omitting the ``C-u`` will expand the
paragraph tag without newlines.

``# contributor:`` snippet author
---------------------------------------------------

This is optional and has no effect whatsoever on snippet
functionality, but it looks nice.

Template syntax
===============

The syntax of the snippet template is simple but powerful, very
similar to TextMate's.

Plain Text
----------

Arbitrary text can be included as the content of a template. They are
usually interpreted as plain text, except ``$`` and `````. You need to
use ``\`` to escape them: ``\$`` and ``\```. The ``\`` itself may also
needed to be escaped as ``\\`` sometimes.

Embedded Emacs-lisp code
------------------------

Emacs-Lisp code can be embedded inside the template, written inside
back-quotes (`````). The lisp forms are evaluated when the snippet is
being expanded. The evaluation is done in the same buffer as the
snippet being expanded. 

Here's an example for ``c-mode`` to calculate the header file guard
dynamically:

.. sourcecode:: text

  #ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
  #define $1

  $0

  #endif /* $1 */

From version 0.6, snippets expansions are run with some special
Emacs-lisp variables bound. One of this is ``yas/selected-text``. You
can therefore define a snippet like:

.. sourcecode:: text

   for ($1;$2;$3) {
     `yas/selected-text`$0
   }

to "wrap" the selected region inside your recently inserted
snippet. Alternatively, you can also customize the variable
``yas/wrap-around-region`` to ``t`` which will do this automatically.

Tab stop fields
---------------

Tab stops are fields that you can navigate back and forth by ``TAB``
and ``S-TAB``. They are written by ``$`` followed with a
number. ``$0`` has the special meaning of the *exit point* of a
snippet. That is the last place to go when you've traveled all the
fields. Here's a typical example:

.. sourcecode:: text

  <div$1>
      $0
  </div>

Placeholder fields
------------------

Tab stops can have default values -- a.k.a placeholders. The syntax is
like this:

.. sourcecode:: text

  ${N:default value}

They acts as the default value for a tab stop. But when you firstly
type at a tab stop, the default value will be replaced by your
typing. The number can be omitted if you don't want to create
`mirrors`_ or `transformations`_ for this field.

.. _mirrors:

Mirrors
-------

We refer the tab stops with placeholders as a *field*. A field can have
mirrors. Its mirrors will get updated when you change the text of a
field. Here's an example:

.. sourcecode:: text

  \begin{${1:enumerate}}
      $0
  \end{$1}

When you type ``"document"`` at ``${1:enumerate}``, the word
``"document"`` will also be inserted at ``\end{$1}``. The best
explanation is to see the screencast(`YouTube
<http://www.youtube.com/watch?v=vOj7btx3ATg>`_ or `avi video
<http://yasnippet.googlecode.com/files/yasnippet.avi>`_).

The tab stops with the same number to the field act as its mirrors. If
none of the tab stops has an initial value, the first one is selected
as the field and others mirrors.

.. _transformations:

Mirrors with transformations
----------------------------

If the value of an ``${n:``-construct starts with and contains ``$(``,
then it is interpreted as a mirror for field ``n`` with a
transformation. The mirror's text content is calculated according to
this transformation, which is Emacs-lisp code that gets evaluated in
an environment where the variable ``text`` (or ``yas/text``) is bound
to the text content (string) contained in the field ``n``.Here's an
example for Objective-C:

.. sourcecode:: text

  - (${1:id})${2:foo}
  {
      return $2;
  }

  - (void)set${2:$(capitalize text)}:($1)aValue
  {
      [$2 autorelease];
      $2 = [aValue retain];
  }
  $0

Look at ``${2:$(capitalize text)}``, it is a mirror with
transformation instead of a field. The actual field is at the first
line: ``${2:foo}``. When you type text in ``${2:foo}``, the
transformation will be evaluated and the result will be placed there
as the transformed text. So in this example, if you type "baz" in the
field, the transformed text will be "Baz". This example is also
available in the screencast.

Another example is for ``rst-mode``. In reStructuredText, the document
title can be some text surrounded by "===" below and above. The "==="
should be at least as long as the text. So

.. sourcecode:: text

  =====
  Title
  =====

is a valid title but

.. sourcecode:: text

  ===
  Title
  ===

is not. Here's an snippet for rst title:

.. sourcecode:: text

  ${1:$(make-string (string-width text) ?\=)}
  ${1:Title}
  ${1:$(make-string (string-width text) ?\=)}

  $0

Fields with transformations
---------------------------

From version 0.6 on, you can also have lisp transformation inside
fields. These work mostly mirror transformations but are evaluated
when you first enter the field, after each change you make to the
field and also just before you exit the field.

The syntax is also a tiny bit different, so that the parser can
distinguish between fields and mirrors. In the following example

.. sourcecode:: text

  #define "${1:mydefine$(upcase yas/text)}"

``mydefine`` gets automatically upcased to ``MYDEFINE`` once you enter
the field. As you type text, it gets filtered through the
transformation every time.

Note that to tell this kind of expression from a mirror with a
transformation, YASnippet needs extra text between the ``:`` and the
transformation's ``$``. If you don't want this extra-text, you can use
two ``$``'s instead.

.. sourcecode:: text

  #define "${1:$$(upcase yas/text)}"

Please note that as soon as a transformation takes place, it changes
the value of the field and sets it its internal modification state to
``true``. As a consequence, the auto-deletion behaviour of normal
fields does not take place. This is by design.

Choosing fields value from a list and other tricks
--------------------------------------------------

As mentioned, the field transformation is invoked just after you enter
the field, and with some useful variables bound, notably
``yas/modified-p`` and ``yas/moving-away-p``. Because of this
feature you can place a transformation in the primary field that lets
you select default values for it.

The ``yas/choose-value`` does this work for you. For example:

.. sourcecode:: text

  <div align="${2:$$(yas/choose-value '("right" "center" "left"))}">
    $0
  </div>

See the definition of ``yas/choose-value`` to see how it was written
using the two variables.

Here's another use, for LaTeX-mode, which calls reftex-label just as
you enter snippet field 2. This one makes use of ``yas/modified-p``
directly.

.. sourcecode:: text 

  \section{${1:"Titel der Tour"}}%
  \index{$1}%
  \label{{2:"waiting for reftex-label call..."$(unless yas/modified-p (reftex-label nil 'dont-
  insert))}}%  

The function ``yas/verify-value`` has another neat trick, and makes
use of ``yas/moving-away-p``. Try it and see! Also, check out this
`thread
<http://groups.google.com/group/smart-snippet/browse_thread/thread/282a90a118e1b662>`_

Nested placeholder fields
-------------------------

From version 0.6 on, you can also have nested placeholders of the type:

.. sourcecode:: text

   <div${1: id="${2:some_id}"}>$0</div>

This allows you to choose if you want to give this ``div`` an ``id``
attribute. If you tab forward after expanding it will let you change
"some_id" to whatever you like. Alternatively, you can just press
``C-d`` (which executes ``yas/skip-and-clear-or-delete-char``) and go
straight to the exit marker.

By the way, ``C-d`` will only clear the field if you cursor is at the
beginning of the field *and* it hasn't been changed yet. Otherwise, it
performs the normal Emacs ``delete-char`` command.

Customizable variables
======================

``yas/trigger-key``
-------------------

The key bound to ``yas/expand`` when function ``yas/minor-mode`` is
active.

Value is a string that is converted to the internal Emacs key
representation using ``read-kbd-macro``.

Default value is ``"TAB"``.

``yas/next-field-key``
----------------------

The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using ``read-kbd-macro``.

Can also be a list of keys.

Default value is ``"TAB"``.

``yas/prev-field-key``
----------------------
  
The key to navigate to previous field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using ``read-kbd-macro``.

Can also be a list of keys.

Default value is ``("<backtab>" "<S-tab>)"``.

``yas/skip-and-clear-key``
--------------------------

The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using ``read-kbd-macro``.

Can also be a list of keys.

Default value is ``"C-d"``.

``yas/good-grace``
------------------

If non-nil, don't raise errors in inline Emacs-lisp evaluation inside
snippet definitions. An error string "[yas] error" is returned instead.

``yas/indent-line``
-------------------

The variable ``yas/indent-line`` controls the indenting. It is bound
to ``'auto`` by default, which causes your snippet to be indented
according to the mode of the buffer it was inserted in.

Another variable ``yas/also-auto-indent-first-line``, when non-nil
does exactly that :-).

To use the hard-coded indentation in your snippet template, set this
variable to ``fixed``.

To control indentation on a per-snippet basis, see also the directive
``# expand-env:`` in `Writing Snippets`_.

For backward compatibility with earlier versions of YASnippet, you can
also place a ``$>`` in your snippet, an ``(indent-according-to-mode)``
will be executed there to indent the line. This only takes effect when
``yas/indent-line`` is set to something other than ``'auto``.

.. sourcecode:: text

  for (${int i = 0}; ${i < 10}; ${++i})
  {$>
  $0$>
  }$>

``yas/wrap-around-region``
--------------------------

If non-nil, YASnippet will try to expand the snippet's exit marker
around the currently selected region. When this variable is set to t,
this has the same effect has using the ```yas/selected-text``` inline
evaluation.

Because on most systems starting to type deletes the currently
selected region, this works mostly for snippets with direct
keybindings or with the ``yas/insert-snippet`` command.

However, when the value is of this variable is ``cua`` YASnippet will
additionally look-up any recently selected that you deleted by starting
typing. This allows you select a region, type a snippet key (deleting
the region), then press ``yas/trigger-key`` to see the deleted region
spring back to life inside your new snippet.

``yas/triggers-in-field``
--------------------------

If non-nil, ``yas/next-field-key`` can trigger stacked expansions,
that is a snippet expansion inside another snippet
expansion. Otherwise, ``yas/next-field-key`` just tries to move on to
the next field.

``yas/snippet-revival``
-----------------------

Non-nil means re-activate snippet fields after undo/redo.

``yas/after-exit-snippet-hook`` and ``yas/before-expand-snippet-hook``
----------------------------------------------------------------------

These hooks are called, respectively, before the insertion of a
snippet and after exiting the snippet. If you find any strange but
functional use for them, that's probably a design flaw in YASnippet,
so let us know.

Importing TextMate snippets
===========================

There are a couple of tools that take TextMate's ".tmSnippet" xml
files and create YASnippet definitions:

 * `a python script by Jeff Wheeler
   <http://code.nokrev.com/?p=snippet-copier.git;a=blob_plain;f=snippet_copier.py>`_

 * a `ruby tool
   <http://yasnippet.googlecode.com/svn/trunk/extras/textmate_import.rb>`_
   , ``textmate_import.rb`` adapted from `Rob Christie's
   <http://www.neutronflux.net/2009/07/28/shoulda-snippets-for-emacs/>`_,
   which I have uploaded to the repository.

In this section, i'll shortly cover the **second** option. 

Download the ``textmate_import.rb`` tool and the TextMate
bundle you're interested in.

.. sourcecode:: text

  $ curl -O http://yasnippet.googlecode.com/svn/trunk/extras/textmate_import.rb
  $ svn export http://svn.textmate.org/trunk/Bundles/HTML.tmbundle/


Then invoke ``textmate_import.rb`` like this:

.. sourcecode:: text

  $ ./textmate_import.rb -d HTML.tmbundle/Snippets/ -o html-mode -g HTML.tmbundle/info.plist

You should end up with a ``html-mode`` subdir containing snippets
exported from textmate.

.. sourcecode:: text 

  $ tree html-mode # to view dir contents, if you have 'tree' installed

The ``-g`` is optional but helps the tool figure out the grouping.
According to `Organizing Snippets`_, don't forget to touch
``.yas-make-groups`` and ``.yas-ignore-filename-triggers`` inside the
``html-mode`` dir.

Also try ``textmate_import.rb --help`` for a list of options.

Please note that snippet importation is not yet perfect. You'll
probably have some adjustments to some/many snippets. Please
contribute these adjustments to the google group or, better yet, patch
the ``textmate_import.rb`` to automatically perform them and submit
that.
 
..  LocalWords:  html YASnippet yas sourcecode pluskid init filenames filename
..  LocalWords:  env varlist keybinding keymap rinari ifndef upcase endif
..  LocalWords:  nondirectory autorelease aValue inline
