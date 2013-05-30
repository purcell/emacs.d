# Warning
* I'm using [Vim](http://www.vim.org) key binding. Please see "Tips" section for restoring the Emacs key binding.
* People in Mainland China may need [goagent](http://code.google.com/p/goagent/) to download packages from ELPA. Run command "http_proxy=http://127.0.0.1:8087 emacs -nw" after starting goagent server.

# General

I base my emacs.d on [Steve Purcell's emacs.d](http://github.com/purcell/emacs.d) and synchronize from his once a month.

To install, clone this repo to ~/.emacs.d, i.e. ensure that the 'init.el' contained in this repo ends up at ~/.emacs.d/init.el and old ~/.emacs does NOT exist.

Thanks to Purcell, this emacs.d has
[fancy features](http://github.com/purcell/emacs.d) for most script
languages like Clojure, Python, Lisp, PHP, Javascript, Ruby
etc. Purcell is basically a web geek who use all the modern web
technologies.

I will support all the languages a desktop developer may use, like
C++, Java, Lua, Objective-C etc.

## Features

* git or subversion is NOT needed. I removed all the 'git submodule update' stuff.
* enhance major/minor modes for C/C++ developers
* optimized for cross-platform C++ development with CMake and wxWidgets
* emacs-w3m (console browser)
* eim (Chinese pinyin input method)
* org2blog (write wordpress blog with org-mode)
* make the configuration work under Linux and Cygwin
* The configuration will work with Emacs version >=24
* evil-mode and its plugins (Vim key binding)
* yasnippet and my customized snippets

## Third party CLI tools Emacs uses

Purcell won't list all the 3rd party tools this configuration dependent on. I will
try to list them HERE,

### w3m (web browser in console) 
- needed by `w3m` (w3m is emacs package name written in elisp)
- install by OS way
### jshint
- required by `flycheck``
- install node.js by OS way, then `sudo npm install -g jshint`
### aspell, and dictionary (aspell-en, for example)
- needed by flyspell
- install by OS way
### sbcl (lisp environment)
- needed by lisp `slime`
- install by OS way
### tidy (html tidy program)
- needed by nxml-mode
- install by OS way
### csslint
- install node.js by OS way, then `sudo npm install -g csslint`
### zip and unzip
- needed by org-mode to export org to odt
- install by OS way
### clang (http://clang.llvm.org)
- needed by cpputils-cmake, flymake, auto-complete-clang, company-clang
- install by OS way
### ctags (http://ctags.sourceforge.net)
- needed by many tags related plugin
- install by OS way
### GNU Global (http://www.gnu.org/software/global)
- needed by gtags
- You use this tool to navigate the C/C++/Java/Objective-C code.
- install by OS way
### pyflakes
- You need pyflakes for real time python syntax check (flymake-python)
- Install pip by OS way, then `pip install pyflakes`
- but on cygwin you need install setuptool in order to install pip.
### ditaa, grapviz and planetuml to convert ascii art to diagram and uml.

## How to install by OS way
* apt-cyg at Cygwin
* homebrew at OS X
* any package manager at Linux

Please note it's totally fine you don't install these CLI tools. Emacs won't crash. ;)
## Report bug
If you find any bug, please file an issue on the github project:
https://github.com/redguardtoo/emacs.d

## Tips
* by default EVIL (Vim emulation in Emacs) is used. You can comment out
 line containing "(require 'init-evil)" in init.el to unload it.
* Some package cannot be downloaded automatically because of network problem.
You need manually `M-x list-packages` and install it.
* I downgraded the yasnippet to an older version because latest yasnippet is
not compatible with auto-complete.
* You can speed up the start up by NOT loading some heavy weight
  components like evil or yasnippet. All you need to do is add below
  code into ~/.bashrc:
  ```sh
  alias e=emacs -q --no-splash --eval="(setq light-weight-emacs t)" -l "$HOME/.emacs.d/init.el"
  ```

## My personal custom.el
It's publicized at http://blog.binchen.org/?p=430 .

<hr>

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://www.linkedin.com/profile/view?id=31199295)
