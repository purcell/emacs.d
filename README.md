# Warning
* I'm using [Vim](http://www.vim.org) key binding. Please see "Tips" section for restoring the Emacs key binding.
* People in Mainland China may need [goagent](http://code.google.com/p/goagent/) to download packages from ELPA. Run command "http_proxy=http://127.0.0.1:8087 emacs -nw" after starting goagent server.
* C++/C developers, you need tell Emacs where to search headers to make auto-complete work. See section `clang` for details.


# General

I base my emacs.d on [Steve Purcell's emacs.d](http://github.com/purcell/emacs.d) and synchronize from his once a month.

To install, clone this repo to ~/.emacs.d, i.e. ensure that the 'init.el' contained in this repo ends up at ~/.emacs.d/init.el and old ~/.emacs does NOT exist.

Thanks to Purcell, this emacs.d has [fancy features](http://github.com/purcell/emacs.d) for most script languages like Clojure, Python, Lisp, PHP, Javascript, Ruby etc. Purcell is basically a web geek who use all the modern web technologies.

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

## Third party command line tools Emacs uses

You need to install some command line tools to use some features in Emacs. All those command line tools are *optional*. Your Emacs will not crash if some tools are not installed.

### w3m (web browser in console) 
* needed by `w3m` (w3m is emacs package name written in elisp)
* install by OS way

### lua
* required by `flymake-lua`
* install by OS way

### aspell, and corresponding dictionary (aspell-en, for example)
* needed by `flyspell`
* install by OS way
* I force the dictionary to "en_US" in init-spelling.el

### hunspel
* alternative of `aspell`
* install by OS way
* I force the dictionary to "en_US" in init-spelling.el

### sbcl (lisp environment)
* needed by lisp `slime`
* install by OS way

### tidy (html tidy program)
* needed by `nxml-mode`
* install by OS way

### csslint
* install `node.js` by OS way, then `sudo npm install -g csslint`

### zip and unzip
* needed by `org-mode` to export org to odt
* install by OS way

### xsel
* needed by my clipboard command `copy-to-x-clipboard` and `paste-from-x-clipboard` under Linux
* install by OS way

### clang (http://clang.llvm.org)
* needed by `cpputils-cmake`, `flymake`, `auto-complete-clang`, `company-clang`
* install by OS way
* If you use `company-clang` (default), add `(setq company-clang-arguments '("-I/example1/dir" "-I/example2/dir"))` into ~/.emacs.d/init.el
* If you prefer `auto-complete-clang` instead, add `(setq ac-clang-flags ("-I/example1/dir" "-I/example2/dir"))` into ~/.emacs.d/init.el
* If you use `cpputils-cmake` and `cmake`, `cpputils-cmake` will do all the set up for you.

### ctags (http://ctags.sourceforge.net)
* needed by many tags related plugin
* install by OS way

### GNU Global (http://www.gnu.org/software/global)
* needed by `gtags`
* You use this tool to navigate the C/C++/Java/Objective-C code.
* install by OS way

### pyflakes
* You need pyflakes for real time python syntax checker like `flymake-python`
* Install pip by OS way, then `pip install pyflakes`
* On cygwin you need install `setuptool` in order to install `pip`.

### libreoffice
* Its binary `soffice` is actually needed
* needed when converting odt file into doc (Microsoft Word 97)
* conversion will happen automatically when exporting org-mode to odt
* The conversion command is stored in variable `org-export-odt-convert-processes`
* Install by OS way

### ditaa, grapviz and planetuml to convert ascii art to diagram and uml.
I don't use them now.

## How to install by OS way
* `apt-cyg` at Cygwin
* `homebrew` at OS X
* any package manager at Linux

## Report bug
If you find any bug, please file an issue on the github project:
https://github.com/redguardtoo/emacs.d

## Tips
* By default EVIL (Vim emulation in Emacs) is used. You can comment out line containing "(require 'init-evil)" in init.el to unload it.

* Some package cannot be downloaded automatically because of network problem.
You need manually `M-x list-packages` and install it.

* If you use yasnippet and auto-complete, I suggest not using yasnippet as input source of auto-complete. 

* You can speed up the start up by NOT loading some heavy weight components like evil or yasnippet. All you need to do is add below code into ~/.bashrc:
  ```sh
  alias e=emacs -q --no-splash --eval="(setq light-weight-emacs t)" -l "$HOME/.emacs.d/init.el"
  ```
* If you use `gnus` for email thing (Gmail, for example). Most set up is already except privacy stuff. Please check `init-gnus.el` for my set up. There is also [a tutorial at my blog](http://blog.binchen.org/?p=403).

* To toggle Chinese input method (eim, for example), press `C-\` or run command `M-x toggle-input-method`.

## My personal custom.el
It's publicized at http://blog.binchen.org/?p=430 .

<hr>

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://www.linkedin.com/profile/view?id=31199295)
