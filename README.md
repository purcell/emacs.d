# A reasonable Emacs config

This is my emacs configuration tree, continually used and tweaked
since 2000, and it may be a good starting point for other Emacs
users, especially those who are web developers. These days it's
somewhat geared towards OS X, but it is known to also work on Linux
and Windows.

Emacs itself comes with support for many programming languages. This
config adds improved defaults and extended support for the following:

* Ruby / Ruby on Rails
* CSS / LESS / SASS / SCSS
* HAML / Markdown / Textile / ERB
* Clojure (via nrepl and slime)
* Javascript / Coffeescript
* Python
* PHP
* Haskell
* Erlang
* Common Lisp

In particular, there's a nice config for *tab autocompletion*, and
flymake is used to immediately highlight syntax errors in Ruby, HAML,
Python, Javascript, PHP and some other languages.

## Requirements

* Emacs 23 or greater (note that Emacs 24 is required for some functionality)

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`.
Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed.

## Similar configs

You might also want to check out `emacs-starter-kit` and `emacs-prelude`.

## Support / issues

If you hit any problems, please [file an issue on the github project](https://github.com/purcell/emacs.d)

-Steve Purcell

<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)
