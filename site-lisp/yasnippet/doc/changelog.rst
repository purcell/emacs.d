=========
ChangeLog
=========

.. _Organizing Snippets: snippet-organization.html
.. _Expanding Snippets: snippet-expansion.html
.. _Writing Snippets: snippet-development.html
.. _The YASnippet Menu: snippet-menu.html

0.7.0b / ????-??-??
===================

* Filenames can no longer be snippet triggers. Please upgrade your snippet
  collections.


0.6.1c / 2009-08-13
===================

* Fixed `issues <http://code.google.com/p/yasnippet/issues>`_ 99, 98, 93,
  90, 91, 88, 87. Thanks everybody.
* More compliant customization group `Issue94
  <http://code.google.com/p/yasnippet/issues/detail?id=94>`_, (thanks
  wyuenho).
* Added workaround for issue 97 in the FAQ
* Small updates to documentation.

0.6.1b / 2009-08-29
===================

* Much more powerful menu. See `The YASnippet menu`_.
* New ways to organize snippets. See `Organizing snippets`_.
* Added ``yas/also-auto-indent-first-line`` customization variable.
* Renamed directive ``# env:`` to ``# expand-env:``
* Rewrote much of the documentation.
* Added TextMate import tool ``textmate-import.rb`` to to svn
  repository (see "extras/")
* Added *experimental* bundle of textmate snippets
  ``yasnippet-textmate-bundle.el``
* Fixed `Issue 74
  <http://code.google.com/p/yasnippet/issues/detail?id=74>`_ (thanks
  rmartin.k...@gmail.com)
* Fixed `Issues 80 through 84
  <http://code.google.com/p/yasnippet/issues/detail?id=80>`_ (thanks
  Moritz Bunkus)
* Fixed many more issues...


0.6.0c / 2009-07-27
===================

* Now byte compiles correctly with no warnings.
* Fixed `Issue 68
  <http://code.google.com/p/yasnippet/issues/detail?id=68>`_ with
  mouse-clicking alternatives in ``ido-mode``.
* Added ``yas/also-auto-indent-first-line`` customization variable.


0.6.0b / 2009-07-25
===================
 
* Nested placeholders of the type ``<div${1: id="${2:someid}"}> $0``.

* More robust undo/redo support.

* Stacked snippet expansion (*snippet in snippet*).

* Transformation on a primary field with syntax ``${1:default$(transform)}``

* Validations on field exit through the ``yas/verify-value``
  primary field transformation.

* Wrapping the region in the exit marker ``$0`` of the snippet. Use
  ``yas/wrap-around-region``.

* Auto-indentation. Use ``yas/indent-line`` set to ``'auto`` 

* Easier definition of snippets. Use ``yas/find-snippets`` or
  ``yas/visit-snippet-file``. In the new ``snippet-mode`` use
  ``yas/load-snippet-buffer`` and ``yas/tryout-snippet``.

* Customization group ``yasnippet``.

* Overriding customization variables in snippets. Use the ``env:
  let-form`` template keyword.

* Fixed `Issue 60
  <http://code.google.com/p/yasnippet/issues/detail?id=60>`_
* Fixed `Issue 65
  <http://code.google.com/p/yasnippet/issues/detail?id=65>`_
* Fixed `Issue 56
  <http://code.google.com/p/yasnippet/issues/detail?id=56>`_

0.5.10 / 2009-02-11
===================

* Added *grouping* support so that the snippets in the menu can be
  groupped together.
* Make the bundle `ELPA <http://tromey.com/elpa/index.html>`_
  compatible.

0.5.9 / 2009-01-21
==================

* Fixed the bug of disabling the auto-indenting of ``cc-mode``.

0.5.8 / 2009-01-15
==================

* Added a ``key`` property in snippet definition for snippet names
  that are not valid path name.
* Fixed some bugs of indenting (`Issue 44
  <http://code.google.com/p/yasnippet/issues/detail?id=44>`_, `Issue
  46 <http://code.google.com/p/yasnippet/issues/detail?id=46>`_).
* Fixed `Issue 45
  <http://code.google.com/p/yasnippet/issues/detail?id=45>`_ by
  providing a proper default value for ``yas/buffer-local-condition``.
* Added helper function ``yas/substr`` for convenient mirror
  transformation.
* Make variable ``yas/registered-snippet`` properly initialized.
* Fixed the overlay error when overlay becomes empty (`Issue 49
  <http://code.google.com/p/yasnippet/issues/detail?id=49>`_ and
  `Issue 48
  <http://code.google.com/p/yasnippet/issues/detail?id=48>`_). This
  bug has occurred and been fixed earlier, and should not have
  happened if we have proper regression test.
* Added a workaround for ``c-electric-`` serial commands (`Issue 27
  <http://code.google.com/p/yasnippet/issues/detail?id=27>`_).
	
0.5.7 / 2008-12-03
==================

* Fixed `Issue 28
  <http://code.google.com/p/yasnippet/issues/detail?id=28>`_ of
  properly clean up snippet (by joaotavora).
* Added a new section "Field-level undo functionality" to correct
  `Issue 33 <http://code.google.com/p/yasnippet/issues/detail?id=33>`_
  (by joaotavora).
* Added some snippets from users for sql, erlang, scala, html, xml, latex, etc.
* Fixed `Issue 16
  <http://code.google.com/p/yasnippet/issues/detail?id=16>`_ by adding
  ``$>`` support. Here's the `doc for $> indenting
  <http://pluskid.lifegoo.com/upload/project/yasnippet/doc/define_snippet.html#indenting>`_.

0.5.6 / 2008-08-07
==================

* Added a buffer local variable ``yas/dont-activate`` to turn off
  ``yas/minor-mode`` in some major modes. See `Issue 29
  <http://code.google.com/p/yasnippet/issues/detail?id=29>`_.
* Make the environment of elisp evaluation more friendly to
  ``(current-column)``.
* Fixed the regular expression bug in python-mode snippets.
* Use filename or full key extension for snippet name if no ``name``
  property is defined.

0.5.5 / 2008-05-29
==================

* Tweak ``yas/extra-mode-hooks`` so that it can be more easily
  customized.
* Add an entry in FAQ about why ``TAB`` key doesn't work in some
  modes.

0.5.4 / 2008-05-15
==================

* Added ``ox-mode-hook`` and ``python-mode-hook`` to
  ``yas/extra-mode-hooks`` to fix the problem YASnippet is not enabled
  in those modes.

0.5.3 / 2008-05-07
==================

* Fix indent of python-mode snippets.
* Fix a bug of dropdown-list: conflicts with color-theme (`Issue 23
  <http://code.google.com/p/yasnippet/issues/detail?id=23>`_). Thanks
  Mike.
* Fix a bug of condition system.

0.5.2 / 2008-04-20
==================

* Fix a bug for comparing string to symbol using ``string=`` (which
  will fire an error).

0.5.1 / 2008-04-14
==================

* Use a beautiful css style in the document.

0.5.0 / 2008-04-10
==================

* Integrate with hippie-expand. Just add ``yas/hippie-try-expand`` to
  ``hippie-expand-try-functions-list``.
* If you set ``yas/fall-back-behavior`` to ``'return-nil``, YASnippet
  will return nil when it can't find a snippet to expand.
* Defect fix: the condition of a snippet was evaluated twice in
  earlier version.
* Deleting snippet (using ``C-w`` or ``C-k``) won't cause serious
  problem now.
* Several complex snippet for python-mode from Yasser included in the
  distribution.

0.4.5 / 2008-04-07
==================

* Merge the latest dropdown-list.el.
* Add snippets for f90-mode from Li Zhu.
* Bug fix: l-safe-expr-p: Lisp nesting exceeds ``max-lisp-eval-depth``
  error when several (more than two) snippets overlaps. Thanks
  sunwaybupt@newsmth for reporting this bug.

0.4.4 / 2008-03-24
==================

* Bug fix: dropdown-list.el doesn't recognize [return] properly.

0.4.3 / 2008-03-23
==================

* Bug fix: failed to recognize user customized yas/trigger-key.

0.4.2 / 2008-03-22
==================

* Make a separate document package for release. Also make document
  available online.

0.4.1 / 2008-03-21
==================

* Make sure ``yas/minor-mode``'s key bindings always take priority to
  other minor modes.

0.4.0 / 2008-03-20
==================

* Document refinement and released with YASnippet. Most of the Online
  wiki document will be deprecated soon.
* Powerful condition system added to yasnippet!
* Incorporate ``dropdown-list.el`` and make it default way for
  selecting multiple candidates. Thanks to `Jaeyoun Chung
  <http://groups.google.com/group/smart-snippet/browse_thread/thread/c869158b76addeb3/e7c6372ba457189e>`_.
* yas/before-expand-snippet-hook

0.3.2 / 2008-03-19
==================

* Enhancement: A better way to define minor-mode. Thanks to Kentaro
  Kuribayashi. See `this thread
  <https://groups.google.com/group/smart-snippet/browse_thread/thread/65cb3b5583eda887?hl=en>`_
  for more details.

0.3.1 / 2008-03-17
==================

* Bug fix: Emacs get confused when a field is deleted. See `issue 10
  <http://code.google.com/p/yasnippet/issues/detail?id=10>`_.

0.3.0 / 2008-03-16
==================

* Add a ``yas/after-exit-snippet-hook`` so that you can do something like
  ``indent-region`` or ``fill-region`` after finish the snippet.
* Use minor-mode instead of ``global-set-key`` to bind the trigger
  key. Now the trigger key and fall-back behavior can be more
  flexible. Not constrained to ``<tab>``. Thanks to Trey Jackson. See
  this `thread
  <https://groups.google.com/group/smart-snippet/browse_thread/thread/937f32a2a6dea4f2?hl=en>`_
  for more details.
* Now user can customize the popup function for selecting multiple
  candidate for the same snippet key.
* Support ``dropdown-list.el`` to be a better way to select multiple
  candidate when in text mode.

0.2.3 / 2008-03-15
==================

* Bug in non-window (-nw) mode when there's multiple candidate to
  expand. See `issue 7
  <http://code.google.com/p/yasnippet/issues/detail?id=7>`_.
* Allow expanding another snippet as long as not currently inside a
  field. 

0.2.2 / 2008-03-13
==================

* Added customized face for fields and mirrors. Better in dark
  background. And users can customize it.

0.2.1 / 2008-03-10
==================

* Fix the insert-behind problem under both Emacs 22 and Emacs 23. 

0.2.0 / 2008-03-10
==================

* Use big keymap overlay to detect ``insert-behind`` event manually to
  avoid sometimes missed hook calls. See `issue 3
  <http://code.google.com/p/yasnippet/issues/detail?id=3>`_ for more
  details.
* Support parent snippet table. Now you can set (for example)
  ``cc-mode`` as common mode for ``c++-mode``, ``c-mode`` and
  ``java-mode``. They'll share snippets defined for ``cc-mode``.

0.1.1 / 2008-03-08
==================

* Add a rake task to upload to google code.
* Use elisp compile-bundle function instead of python scrip

0.1.0 / 2008-03-07
==================

* Embedded elisp support.
* Fields navigation support.
* Mirror of fields support.
* Menu-bar support.
* Multiple snippets with same name support.
* Popup menu for multiple snippet with same name support.
* Transformation of fields support.
* Load directory support.
* Compile bundle support. 
