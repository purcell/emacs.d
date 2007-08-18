Sharing your current color setup:

Use `color-theme-submit'.  If you have already invested time in
customizing Emacs faces, please consider sharing your current setup.
Make sure that color-theme.el is in your `load-path'.  Type M-x
load-library RET color-theme RET to load all the functions.  Type M-x
color-theme-submit RET and mail the result to the maintainer of this
package (see above for mail addres).

If you want to make sure that all your customization was exported,
type M-x list-faces-display RET to get a list of all faces currently
defined.  This is the list of faces that `color-theme-print' uses.

Installing a color theme:

Make sure that color-theme.el is in your `load-path'.  Type M-x
load-library RET color-theme RET to load all the functions.

The main function to call is color-theme-select.  Type M-x
color-theme-select RET.  That creates a Color Theme Selection
buffer.  Press RET or `i' on a color theme to install it for the
rest of your session.

If you want to install the color theme as soon as Emacs is started
up, read the description of the theme you like and remember the
name of the color theme function.  Press `d' on a color theme in
the Color Theme Selection buffer to read the description.  Assuming
you like the Gnome2 theme, you'll find that the function to use is
called `color-theme-gnome2'.  Add the following to the end of your
.emacs (removing the leading `;;').

(require 'color-theme)
(color-theme-gnome2)

Changing menu colors:

In Emacs 21 on X, you can set the menu colors and font using the
menu face.  Example for your .emacs file:

  (set-face-font 'menu "7x14")
  (set-face-foreground 'menu "white").

If are using X, you can set the menu foreground and background using
a resource file, usually .Xdefaults or .Xresources.  Usually
.Xdefaults is used when you start your session using a display
manager such as xdm or gdm.  .Xresources is usually used when you
start X directly via a shell script such as startx.  If you set
Emacs*Background and Emacs*Foreground in such a resource file, the
foreground and background of Emacs including the menu will be set.
If your .emacs then loads a color theme, the foreground and
background are changed -- with the exception of the menu.  There is
no way to manipulate the menu foreground and background color from
elisp.  You can also set more specific menu resources for Emacs in
the resource file.  Here is a sample entry for your resource file:

  Emacs*Background:		DarkSlateGray
  Emacs*Foreground:		wheat

Creating your own color theme:

Use M-x customize-face and customize the faces.  Make sure to "Set
for Current Session" -- you don't want to save these using custom!
When you are done, call M-x color-theme-print to produce the elisp
code required to recreate your theme.  Better yet, use M-x
color-theme-submit to mail it to the maintainer.  That way it will be
added to future versions of color-theme.el.

For more information on the elisp format of a color theme, start with
the documentation of `color-theme-install' using C-h f
color-theme-install.

When your color theme is just a variation of an existing color theme,
take a look at `color-theme-robin-hood' in order to see an example of
how to do it.  Essentially you want to call all the parent color
themes before installing your changes.  For all but the first parent
color theme, you need to make sure that `color-theme-is-cumulative'
is bound to t.  If you don't do that, users that set
`color-theme-is-cumulative' to nil will only install your changes
without the parent color themes.

Making a color theme work for both Emacs and XEmacs:

Once you have printed the color-theme, you can make sure it looks
similar in both Emacs and XEmacs by running
`color-theme-analyze-defun' on the printed theme.  This function
will check for missing faces for the other editor...
