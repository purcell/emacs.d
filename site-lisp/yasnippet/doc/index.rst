=============================
Yet Another Snippet extension
=============================

.. _Organizing Snippets: snippet-organization.html
.. _Expanding Snippets: snippet-expansion.html
.. _Writing Snippets: snippet-development.html
.. _The YASnippet Menu: snippet-menu.html 

.. contents::

**YASnippet** is a template system for Emacs. It allows you to type an
abbreviation and automatically expand it into function
templates. Bundled language templates includes: C, C++, C#, Perl,
Python, Ruby, SQL, LaTeX, HTML, CSS and more.

The snippet syntax is inspired from TextMate's syntax, you can even
`import <snippet-development.html#importing-textmate-snippets>`_
import most TextMate templates. YASnippet is a re-write of the
extension `smart-snippet`_. Both are original creations of `pluskid
<http://pluskid.lifegoo.org>`_.

.. _smart-snippet: http://code.google.com/p/smart-snippet/

Video Demo
==========

.. youtube:: 76Ygeg9miao
   :align: right

Watch the `demo at YouTube
<http://www.youtube.com/watch?v=76Ygeg9miao>`_ (download a higher
resolution version: `yasnippet.avi
<http://yasnippet.googlecode.com/files/yasnippet.avi>`_).

Installation
============

There are two archives you can download. To quickly tryout YASnippet,
download the simpler "bundle" version. If you plan to modify the
bundled templates and/or build your own, download the "normal"
package.

Install with ``yasnippet-bundle.el``
------------------------------------

1. Download the latest ``yasnippet-bundle-x.y.z.el.tgz`` and unpack it.
2. You'll get a file named ``yasnippet-bundle.el``, put it under
   ``~/.emacs.d/plugins/`` (create the directory if not exists).
3. Open the file in Emacs, and type ``Alt+x eval-buffer``.

That's it. Now open any one of your language file, you'll see a menu
YASnippet. you can pull the menu to insert a template. Or, you can
type the a *trigger key* then press ``TAB`` to expand it.

To have Emacs load YASnippet automatically when it starts, put the
following in your ``~/.emacs`` file:

   .. sourcecode:: common-lisp

     (add-to-list 'load-path
                   "~/.emacs.d/plugins")
     (require 'yasnippet-bundle)

The `youtube video <http://www.youtube.com/watch?v=76Ygeg9miao>`_
demonstrates this quick installation.

Normal Install
--------------

To install YASnippet as a normal emacs package, download and unpack
the latest ``yasnippet-x.y.z.tar.bz2``. You'll get a directory named
``yasnippet-x.y.z``, which you can put it in your
``~/.emacs.d/plugins`` and add the following in your ``.emacs`` file:

   .. sourcecode:: common-lisp

     (add-to-list 'load-path
                   "~/.emacs.d/plugins/yasnippet-x.y.z")
     (require 'yasnippet) ;; not yasnippet-bundle
     (yas/initialize)
     (yas/load-directory "~/.emacs.d/plugins/yasnippet-x.y.z/snippets")

Please refer to the documentation for full customization, or use the
customization group.

How to use YASnippet
====================

Since version 0.6, YASnippet contains more functionality. You don't
need to know all of it to use it successfully, but you it can improve
your snippeting experience.

Hence this section has been split into separate documents:

1. `Organizing Snippets`_

  Describes ways to organize your snippets in the hard disk (or not
  organize them at all and just use ``yasnippet-bundle.el``.

2. `Expanding Snippets`_

  Describes how YASnippet chooses snippets for expansion at point.

  Maybe, you'll want some snippets to be expanded in a particular
  mode, or only under certain conditions, or be prompted using
  ``ido``, etc...

3. `Writing Snippets`_

  Describes the YASnippet definition syntax, which is very close (but
  not equivalent) to Textmate's. Includes a section about converting
  TextMate snippets.

4. `The YASnippet menu`_

  Explains how to use the YASnippet menu to explore, learn and modify
  snippets.

Bugs, Contribution and Support
==============================

* If you find a bug, please report it at `Issue List
  <http://code.google.com/p/yasnippet/issues/list>`_.
* If you have problem using YASnippet, or have some new ideas,
  including snippets, please post to the `discussion group`_.

.. _discussion group: http://groups.google.com/group/smart-snippet
.. _wish list: http://code.google.com/p/yasnippet/wiki/WishList

Thank you very much for using YASnippet!

..  LocalWords:  YASnippet SQL LaTeX CSS yasnippet el eval html ido RET wiki
