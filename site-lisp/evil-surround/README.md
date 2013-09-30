This package emulates [surround.vim by Tim Pope](https://github.com/tpope/vim-surround).
The functionality is wrapped into a minor mode.
To enable it globally, add the following lines to ~/.emacs:

    (require 'surround)
    (global-surround-mode 1)

Alternatively, you can enable surround-mode along a major mode by adding
`turn-on-surround-mode' to the mode hook.

This package uses [Evil](surround.vim by Tim Pope) as its vi layer.

## Add surrounding ##
You can surround in visual-state with `s<textobject><trigger>`
or in normal-state with `ys<textobject><trigger>`.

## Change surrounding ##
You can change a surrounding with `cs<old-trigger><new-trigger>`.

## Delete surrounding ##
You can delete a surrounding with `cd<old-trigger><new-trigger>`.

## Add new surround pairs ##
A surround pair is this (trigger char with textual left and right strings):

    (?> . ("<" . ">"))

or this (trigger char and calling a function):

    (?< . surround-read-tag)

You can add new by adding them to `surround-pairs-alist`.
For more information do: `C-h v surround-pairs-alist`.

`surround-pairs-alist` is a buffer local variable, which means that you can have
different surround pairs in different modes.
By default `<` is used to insert a tag, in C++ this may not be useful - but
inserting angle brackets is, so you can add this:

    (add-hook 'c++-mode-hook (lambda ()
                               (push '(?< . ("< " . " >")) surround-pairs-alist)))

Don't worry about having two entries for `<` surround will take the first.

Or in Emacs Lisp modes using \` to enter \` ' is quite useful, but not adding a
pair of ` (the default behavior if no entry in `surround-pairs-alist` is
present), so you can do this:

    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                      (push '(?` . ("`" . "'")) surround-pairs-alist)))

without affecting your Markdown surround pairs, where the default is useful.

To change the default `surround-pairs-alist` you have to use `setq-default`, for
example to remove all default pairs:

    (setq-default surround-pairs-alist '())

or to add a pair that surrounds with two ` if you enter ~:

    (setq-default surround-pairs-alist (cons '(?~ ("``" . "``"))
                                             surround-pairs-alist))

## Add new supported operators ##
You can add support for new operators by adding them to `surround-operator-alist`.
For more information do: `C-h v surround-operator-alist`.

By default, surround works with `evil-change` and `evil-delete`.
To add support for the evil-paredit package, you need to add `evil-paredit-change`
and `evil-paredit-delete` to `surround-operator-alist`, like so:

    (add-to-list 'surround-operator-alist
                 '(evil-paredit-change . change))
    (add-to-list 'surround-operator-alist
                 '(evil-paredit-delete . delete))

## Usage examples ##

Here are some usage examples (taken from
[surround.vim](https://github.com/tpope/vim-surround/blob/master/README.markdown)):

Press `cs"'` inside

    "Hello world!"

to change it to

    'Hello world!'

Now press `cs'<q>` to change it to

    <q>Hello world!</q>

To go full circle, press `cst"` to get

    "Hello world!"

To remove the delimiters entirely, press `ds"`.

    Hello world!

Now with the cursor on "Hello", press `ysiw]` (`iw` is a text object).

    [Hello] world!

Let's make that braces and add some space (use `}` instead of `{` for no
space): `cs]{`

    { Hello } world!

Now wrap the entire line in parentheses with `yssb` or `yss)` (wrapping the line
is currently broken see [issue #5](https://github.com/timcharper/evil-surround/issues/5)).

    ({ Hello } world!)

Revert to the original text: `ds{ds)`

    Hello world!

Emphasize hello: `ysiw<em>`

    <em>Hello</em> world!

Finally, let's try out visual mode. Press a capital V (for linewise
visual mode) followed by `S<p class="important">`.

    <p class="important">
      <em>Hello</em> world!
    </p>

Suppose you want to call a function on your visual selection or a text
object. You can simply press `f` instead of the aforementioned keys
and are then prompted for a functionname in the minibuffer, like with
the tags. So with:

	"Hello world!"

... after selecting the string, then pressing `sf`, entering `print`
and pressing return you would get

    print("Hello world!")
