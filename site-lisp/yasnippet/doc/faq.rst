============================
 Frequently Asked Questions
============================

Why is there an extra newline?
==============================

If you have a newline at the end of the snippet definition file, then
YASnippet will add a newline when you expanding a snippet. Please
don't add a newline at the end if you don't want it when you saving
the snippet file.

Note some editors will automatically add a newline for you. In Emacs,
if you set ``require-final-newline`` to ``t``, it will add the final
newline for you automatically.

Why doesn't TAB expand a snippet?
=================================

First check the mode line to see if there's ``yas``. If not, then try
``M-x yas/minor-mode`` to manually turn on the minor mode and try to
expand the snippet again. If it works, then, you can add the following
code to your ``.emacs`` *before* loading YASnippet:

.. sourcecode:: lisp

  (add-hook 'the-major-mode-hook 'yas/minor-mode-on)

where ``the-major-mode`` is the major mode in which ``yas/minor-mode``
isn't enabled by default.

From YASnippet 0.6 you can also use the command ``M-x
yas/global-mode`` to turn on YASnippet automatically for *all* major
modes.

If ``yas/minor-mode`` is on but the snippet still not expanded. Then
try to see what command is bound to the ``TAB`` key: press ``C-h k``
and then press ``TAB``. Emacs will show you the result. 

You'll see a buffer prompted by Emacs saying that ``TAB runs the
command ...``. Alternatively, you might see ``<tab> runs the command
...``, note the difference between ``TAB`` and ``<tab>`` where the
latter has priority. If you see ``<tab>`` bound to a command other
than ``yas/expand``, (e.g. in ``org-mode``) you can try the following
code to work around:

.. sourcecode:: lisp

  (add-hook 'org-mode-hook
            (let ((original-command (lookup-key org-mode-map [tab])))
              `(lambda ()
                 (setq yas/fallback-behavior
                       '(apply ,original-command))
                 (local-set-key [tab] 'yas/expand))))

replace ``org-mode-hook`` and ``org-mode-map`` with the major mode
hook you are dealing with (Use ``C-h m`` to see what major mode you
are in).

As an alternative, you can also try

.. sourcecode:: lisp

  (defun yas/advise-indent-function (function-symbol)
    (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
             ,(format
               "Try to expand a snippet before point, then call `%s' as usual"
               function-symbol)
             (let ((yas/fallback-behavior nil))
               (unless (and (interactive-p)
                            (yas/expand))
                 ad-do-it)))))

  (yas/advise-indent-function 'ruby-indent-line)

To *advise* the modes indentation function bound to TAB, (in this case
``ruby-indent-line``) to first try to run ``yas/expand``.

If the output of ``C-h k RET <tab>`` tells you that ``<tab>`` is
indeed bound to ``yas/expand`` but YASnippet still doesn't work, check
your configuration and you may also ask for help on the `discussion
group <http://groups.google.com/group/smart-snippet>`_. See this
particular `thread
<http://code.google.com/p/yasnippet/issues/detail?id=93&can=1>`_ for
quite some solutions and alternatives.

Don't forget to attach the information on what command is bound to TAB
as well as the mode information (Can be obtained by ``C-h m``).

Why doesn't TAB navigation work with flyspell
=============================================

A workaround is to inhibit flyspell overlays while the snippet is active:

.. sourcecode:: lisp
  
  (add-hook 'flyspell-incorrect-hook
          #'(lambda (dummy1 dummy2 dymmy3)
              (and yas/active-field-overlay
                   (overlay-buffer yas/active-field-overlay))))

This is apparently related to overlay priorities. For some reason, the
``keymap`` property of flyspell's overlays always takes priority over
the same property in yasnippet's overlays, even if one sets the
latter's ``priority`` property to something big. If you know
emacs-lisp and can solve this problem, drop a line in the `discussion
group`_.

How do I turn off the minor mode where in some buffers
======================================================

The best way, since version 0.6.1c, is to set the default value of the
variable ``yas/dont-activate`` to a lambda function like so:

.. sourcecode:: lisp
  
  (set-default 'yas/dont-activate
             #'(lambda ()
                 (and yas/root-directory
                      (null (yas/get-snippet-tables)))))

This is also the default value starting for that version. It skips the
minor mode in buffers where it is not applicable (no snippet tables),
but only once you have setup your yas/root-directory.


How do I define an abbrev key containing characters not supported by the filesystem?
====================================================================================

**Note**: This question applies if you're still defining snippets
  whose key *is* the filename. This is behavior stil provided by
  version 0.6 for backward compatibilty, but is somewhat deprecated...

For example, you want to define a snippet by the key ``<`` which is
not a valid character for filename on Windows. This means you can't
use the filename as a trigger key in this case.

You should rather use the ``# key:`` directive to specify the key of
the defined snippet explicitly and name your snippet with an arbitrary
valid filename, ``lt.yasnippet`` for example, using ``<`` for the
``# key:`` directive:

.. sourcecode:: text

  # key: <
  # name: <...></...>
  # --
  <${1:div}>$0</$1>

.. _discussion group: http://groups.google.com/group/smart-snippet
