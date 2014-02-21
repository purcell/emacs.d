direx.el -- General purpose directory/tree explore
==================================================

Overview
--------

direx.el is a simple directory explorer.
It also works as a generic tree explore library.

Screenshots
-----------

![](http://cx4a.org/software/direx/direx-direx.png)

Usage
-----

Here is a minimal example usage:

    (require 'direx)
    (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

If you are using [popwin](https://github.com/m2ym/popwin-el), you can use
directory viewer as temporary "side-bar", like this:

    (push '(direx:direx-mode :position left :width 25 :dedicated t)
          popwin:special-display-config)
    (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

Configuration
-------------

`M-x customize-group RET direx RET`.

Libraries using direx.el
------------------------

- direx-project.el (bundled with direx.el)
  -- project tree explorer
- [jedi-direx](https://github.com/tkf/emacs-jedi-direx)
  -- Python source code viewer

Author
------

* Tomohiro Matsuyama <<tomo@cx4a.org>>

Contributors
------------

* Takafumi Arakaki <<aka.tkf@gmail.com>>

----

Copyright (C) 2011, 2012  Tomohiro Matsuyama <<tomo@cx4a.org>>
