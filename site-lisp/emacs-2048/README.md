emacs-2048
==========

This is an implementation of [2048](http://gabrielecirulli.github.io/2048/) for GNU Emacs.

Installation
------------

To try it out, run Emacs and do the following:

`M-x load-file RET /path/to/2048.el RET`

`M-x 2048-play`

To install it, add the following to your .emacs or init.el:

```
(add-to-list 'load-path "/path/to/emacs-2048/")
(load-library '2048)
```

License
-------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.