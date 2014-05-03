# visual-regexp

visual-regexp for Emacs is like `replace-regexp`, but with live  visual feedback directly in the buffer. Check out [visual-regexp-steroids](https://github.com/benma/visual-regexp-steroids.el/) if you want to use modern regular expressions instead of Emacs-style regular expressions.

While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches:

![entering regexp](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0A.png)

While constructing the replacement in the minibuffer, you get live visual feedback for the replacements:

![entering replacement](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0B.png)

It can be used to replace all matches in one go (like `replace-regexp`), or a decision can be made on each match (like `query-replace-regexp`). 

## Installation

If you are using Emacs 24, you can get visual-regexp from [melpa](http://melpa.milkbox.net/) with the package manager.

Add the following code to your init file. Of course you can select your own key bindings.
Note: `vr/mc-mark` is an interface to [multiple-cursors](https://github.com/magnars/multiple-cursors.el/).

```Lisp
(add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/") ;; if the files are not already in the load path
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
```
To customize, use `M-x customize-group [RET] visual-regexp`. 

An interesting application of this is regexp-renaming a bunch of files directly in a dired buffer with live feedback (using `wdired-change-to-wdired-mode`):

Construction of the regexp:
![entering regexp](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp1A.png)

Construction of the replacement string with inlined preview enabled:
![entering replacement](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp1B.png)

## Tip Jar
If you found this useful, please consider donating.

BTC: 1BxauiLGMQPb2pavkkQkuFe5CgrGMrUat2
