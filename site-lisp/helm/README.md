```
 _____                            _   _      _
| ____|_ __ ___   __ _  ___ ___  | | | | ___| |_ __ ___
|  _| | '_ ` _ \ / _` |/ __/ __| | |_| |/ _ \ | '_ ` _ \
| |___| | | | | | (_| | (__\__ \ |  _  |  __/ | | | | | |
|_____|_| |_| |_|\__,_|\___|___/ |_| |_|\___|_|_| |_| |_|
```

## Abstract

`Helm` is incremental completion and selection narrowing framework for
Emacs. It will help steer you in the right direction when you're looking
for stuff in Emacs (like buffers, files, etc).

Helm is a fork of `anything.el` originaly written by Tamas Patrovic
and can be considered to be its successor. 
`Helm` sets out to clean up the legacy code in `anything.el`
and provide a cleaner, leaner and more modular tool, that's not tied in
the trap of backward compatibility. 

## Requirements

You need a recent Emacs to use latest helm, at least Emacs-24.3.

## Getting Started

### Quick install

  1. Clone the `helm` repository to some directory:
  
    ```elisp
    $ git clone https://github.com/emacs-helm/helm.git /path/to/helm/directory
    ```
  
  2. Run `make` from this directory.
  3. Add to `.emacs.el` (or equivalent):

    ```elisp
    (add-to-list 'load-path "/path/to/helm/directory")
    (require 'helm-config)
    ```

Alternatively, you can have a quick try to helm by launching from the helm directory:

`./emacs-helm.sh`

Note that this will not work on Windows systems.

### Install from Emacs packaging system

Helm is now available on Melpa at `http://melpa.milkbox.net/`
You will find there instructions to install.

_WARNING:_ For any upgrade after Version 1.5.6 using package.el, you will have to remove all your helm*.elc files
and recompile from a clean Emacs, expect failures if you don't do that.
Users that are installing from git will not have this problem
by recompiling their helm copy with "make".

**Note to Linux Distributions Maintainers**

`Only the extensions present in the github emacs-helm organisation are supported.`

### Alternate install warning

Some people are installing `helm` with their own config using diverses `require`, `autoload`
and other hacks, not using `helm-config`.
Expect failures and slowdown at startup unless you really know what you are doing when you do so.

### Emacs Prelude

If you're afraid to play with Emacs's configuration, but want to try
out Helm - have NO FEAR. Have a look at
[Emacs Prelude](https://github.com/bbatsov/prelude) - it has
Helm built-in and properly set-up.

### Basic usage

Just type `M-x helm-mini` and enjoy. You might want to bind that command to
a keyboard shortcut. Here's a suggestion:

```elisp
(global-set-key (kbd "C-c h") 'helm-mini)
```
You can also start with `M-x helm-mode` and enjoy helm completion in your favourites
Emacs commands (e.g `M-x`, `C-x C-f`, etc...).
You can enable this by adding in your init file:

```elisp
(helm-mode 1)
```

As a startup point you can also look at the helm section in Emacs menu to
discover some of the commands provided by helm.

### Advanced usage

Helm is capable of a lot.

You can find all the gory details on the [Helm Wiki](https://github.com/emacs-helm/helm/wiki).

## Known issues

Check out the project's
[issue list](https://github.com/emacs-helm/helm/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and sent us a pull request. :-)

## Contributors

Here's a [list](https://github.com/emacs-helm/helm/contributors) of all the people who have contributed to the
development of Helm.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

NOTE: When trying if something is working or not, be sure to start helm from `Emacs -Q` or even better
Start it from your helm directory with `./emacs-helm.sh`.

## Getting help

If [Helm Wiki](https://github.com/emacs-helm/helm/wiki) is not enough, you can ask for help
on [emacs-helm google group](https://groups.google.com/group/emacs-helm?hl=en).


Cheers,<br>
The Helm Team

