;;; vimpulse.el --- emulates Vim's most useful features, including Visual mode




;; ### For setup instructions, see "Installation" below. ###




;; Copyright (C) 2007 Alessandro Piras and Brad Beveridge
;; 
;; Version: 0.2.6.3
;; Keywords: emulations
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, Emacs pinky finger
;; Authors: Alessandro Piras <laynor@gmail.com>,
;;          Brad Beveridge <brad.beveridge@gmail.com>
;; Maintainer: Jason Spiro <jasonspiro3@gmail.com>
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Works well with GNU Emacs 21.4 and 22.0.
;;                Causes problems with undo, but has no other problems, 
;;                on XEmacs 21.4.19.
;;                Please send us compatibility info re. other Emacsen.
;; URL: http://emacswiki.org/elisp/vimpulse.el
;; 
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most useful features, including Visual
;; mode.  Vimpulse is a set of modifications to viper, the minor mode
;; that emulates Vi.  Vimpulse is not a minor mode; as soon as it is
;; loaded, viper will start working in a more Vim-like way.
;; 
;; Vimpulse is beta software.  It seems to work quite well already
;; though.  Patches and feature requests welcome.

;;; Installation:

;; 1. Copy vimpulse.el to somewhere in your load-path, e.g. your
;;    site-lisp directory.
;; 
;; 2. Add the following block of code to your ~/.emacs file.  (If the
;;    file does not exist, create it.  If you use Windows, see
;;    http://www.gnu.org/software/emacs/windows/faq3.html#TOC33 to
;;    learn where to save the file.  If you use a Mac, email the
;;    maintainer for help.)  Leave out the semicolons at the beginning
;;    of each line.
;; 
;(setq viper-mode t)                ; enable Viper at load time
;(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
;(require 'viper)                   ; load Viper
;(require 'vimpulse)                ; load Vimpulse
;(setq woman-use-own-frame nil)     ; don't create new frame for manpages
;(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)
;; 
;; 3. (Optional) If you will be using C-r (the redo key) and you use
;;    GNU Emacs, also install http://wonderworks.com/download/redo.el
;;    and add (require 'redo) to your .emacs file.  (XEmacs and
;;    Aquamacs come with redo.el included.)
;; 
;; 4. (Optional) If you want a nice-looking block visual mode (i.e.
;;    rectangle selection), download and install
;;    http://emacswiki.org/elisp/download/rect-mark.el 
;;    in your load-path and add the Lisp expression (require
;;    'rect-mark) to your .emacs file. Block mode will work
;;    without rect-mark but you won't see the nice rectangle.
;; 
;; We would love it if you sent an email to the maintainer saying
;; whether you liked or disliked vimpulse and why, and which
;; additional Vim features you would like implemented soonest.

;;; Usage:

;; The only feature of vimpulse with documentation available is visual
;; mode.
;; 
;; Visual mode:
;; 
;; To use visual mode, press v in normal mode.  Then use the motion
;; commands to select the region.  Then press d to delete, c to
;; change, r to replace, or y to copy.  You can use p to paste.  For
;; linewise visual mode, press V instead of v.  Then you can copy and
;; paste whole lines. If you want go to block visual mode, press C-v.
;; Here you can copy and paste the selected rectangle, but you have
;; to use C-p. C-p is used because it was a simple way to add visual block
;; mode in a way *close* to Vim without having to hack viper mode to
;; use the normal 'p' key.  In the future, it would be nice to see
;; vimpulse provide this the "right" way, but at this point I'm too
;; inexperienced with elisp to make that happen.
;; In block and linewise visual mode you may use I or A the insert or append
;; text before or after the selection in each selected line.
;;
;;
;; Other features:
;; 
;; This documentation is not written yet.  For now, see the definition
;; of viper-vi-global-user-map right near the beginning of the code.
;; You'll see a list of keys and what function each one calls.  The
;; documentation that comes with Vim -- which is also online at
;; http://vimdoc.sf.net -- may also be helpful.
;;
;; Tips:
;;
;; - Vimpulse makes C-r run "redo" in command mode but you can 
;;   still get reverse i-search by pressing C-s then C-r.

;;; Bugs:

;; (We would appreciate very much if you report bugs.)
;; 
;; Known bugs:
;; 
;; - (class of bugs) In visual or line visual mode, pressing things
;;   like C-g or C-SPC tends to confuse Vimpulse and do weird things.
;; 
;; - When a '(' and a ')' are on different lines, the cursor won't
;;   go left over the ')'.  This also affects unmatched ')'s like :-).
;;     - The problem lies with the show-paren-function advice near
;;       middle of file.
;;         - Hmm, the function "modifies paren matching under viper to
;;           work like in (almost) like in vim".  What does that mean?
;;           Is it for C-x C-e or something?  I don't think so; I bet
;;           it's something to do with blinking parens or something.  I
;;           should ask Alessandro.
;;     - Workaround: press 2h to skip backwards past the paren, or X
;;       to delete it.
;; 
;; - In visual mode, repeatedly pressing H, M, or L acts weirdly.  (I
;;   wonder if there are other keys that act weirdly in that mode too.)
;; 
;; - cW positions the cursor wrongly when used on the last word of a line
;; 
;; - One user who uses an ancient emacs-snapshot (from 2005) mentions
;;   that this mode causes all the keys on his keyboard to stop
;;   working unless he deletes the line that reads 'viper--key-maps
;;   from the macro my-get-emulation-keymap in this file.
;; 
;; - cw with a count doesn't work the same as Vim when the point
;;   is just before a space between words
;;     - Fix plan: try cw with a count, then try dwi with a count; or,
;;       ask on a relevant forum how the commands differ; or, check
;;       how it works in Vi / Vim then check the Vim manual for more
;;       info; then, decide how to best fix.
;; 
;; - Undo has problems in XEmacs.

;;; Development and documentation TODOs:

;; - make sure I have added all stuff in Brad's viper additions and
;;   from my collection, then start documenting already.  Once there
;;   are even the simplest of docs (a nice key map) people will have a
;;   far easier time using vimpulse and so I bet more will contribute.
;; 
;; - the / key should allow isearch that works like Vim's, or until
;;   that's implemented, it should at least remap / to isearch-forward
;;   or viper-isearch-forward.  This should be an option that should
;;   be disabled by default.  For now, have viper load .vimrc and
;;   check for vim-specific option strings like "set incsearch".  If
;;   anyone complains, rethink that plan.
;; 
;; - Folding.  This should be implemented as a separate lisp library
;;   usable for even non-viper users.  Which foldmethods to do first?
;;   I personally only use foldmethod=marker, and even that only rarely.
;; 
;; - i_C-(I forgot the letter) should do (copy-from-above-command 1)
;;   from misc.el
;; 
;; - add advanced C-w commands; they can can use windmove.el
;;   (directional window-selection routines)
;; 
;; - add :set spell / :set nospell that uses flyspell-mode
;; 
;; - add support for tabs.el, a tabs mode that works sensibly (get it
;;   from Emacs Lisp List)
;;     - minimum needed: :tabedit, :tabnext, :tabprevious 
;;     - since I'm emulating Vim, emulate its tab pages feature.  So a
;;       tab page should be able to hold one or more buffers.
;; 
;; - add Customize option to let users stop C-r from being redo?
;; 
;; - email and try to get redo.el included with GNU Emacs (since I
;;   won't include redo.el here since nobody else includes it in their
;;   Lisp files either)
;; 
;; - copy more features from Brad's work in darcs and from vimpact
;;   into vimpulse
;; 
;; - doc: look in google chat log, find description of one-char-off
;;   bug, see if it applies to this or to the not-yet-released
;;   viper-x, and if to this, mention under Bugs
;; 
;; - doc: fix ref to "your home directory": Windows users don't have
;;   one
;; 
;; - doc: list all new keys (and maybe all differences from viper) in
;;   Usage section
;; 
;; - doc: describe all new keys in Usage section; can look at Vim
;;   manual for ideas
;; 
;; - modify how tramp works so it also automatically handles URLs
;;   typed in the netrw syntax, i.e. http:// etc.  But first ask tramp
;;   upstream if they could please make those changes themselves.
;; 
;; - try to clean up namespace to use only vimpulse- prefix (but do I
;;   need to worry about the viper-visual- stuff, or is that a
;;   separate-enough prefix?)
;; 
;; - add CTRL-O for jumping back in the jumplist and CTRL-I for
;;   jumping forwards (for undoing one CTRL-O).  I wonder if emacs'
;;   tags functionality allows a jumplist.  I wonder if viper does
;;   tags like nvi does.
;; 
;; - on my PC (I run Ubuntu), if you start plain Vim then press CTRL-O
;;   many times, it starts opening recently opened files.  Is that
;;   useful?  Should vimpulse have persistent jump table functionality
;;   like that, and if so, should it use recentf or vim's .viminfo
;;   file or some tag functionality in emacs?  How will it interact
;;   with the fact that in emacs it's not traditional to suddenly
;;   close files without warning?
;; 
;; - make sentence movement work like in Vim.  I wonder if this can be
;;   done by setting viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - '.', '?', or '!'
;;         - then (optionally) one or more '"', ''', ')', and ']'
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros.  A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters '{' and '}' sometimes affect paragraph
;;               definitions.  See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where Tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;; 
;; - try to get vimpulse included with upstream viper; also, ideally,
;;   if you pressed "v" in viper, viper would offer to load vimpulse.
;;   (likely to happen?  Consider that Michael Kifer, the viper
;;   maintainer, told me he doesn't need vim keys.  Then again, maybe
;;   I could convince him that it's worth it to ship vim keys, for
;;   other people's benefit.)
;; 
;; - email ridip <rdp@inthefaith.net> and ask him for his vimpulse
;;   contribs and his dvorak stuff
;; 
;; - email to Tromey for upload into ELPA?  we'd have to redo this
;;   when a new major version comes out.  Or maybe we should just
;;   contribute some auto-ELPA-management code.  By the way, should we
;;   move vimpulse into CVS somewhere?
;; 
;; - maybe merge all feature requests that anyone has ever sent into a
;;   "Feature requests" section here

;;; Development plans:

;; The design plan for Vimpulse is for it to only emulate features
;; that are in Vim.  Unfortunately, other new features do not belong
;; in Vimpulse unless you can get the Vim people to implement those
;; features too.

;;; Undecided development questions:

;; - In vimpulse, like in real vim, C-r only does redo in command
;;   mode; in insert mode it does something else.  (In vimpulse that
;;   "something else" is reverse i-search.)  Should it do reverse
;;   i-search in insert mode too?
;; 
;; - When you press "v" for visual mode, Vimpulse modifies the mode
;;   section of the modeline, so it reads e.g. "(Emacs-Lisp visual)".
;;   Shouldn't it do something to the <V> indicator instead?
;; 
;; - In Vim, when a line starts with a "// " or ";; " comment and I
;;   press enter, Vim extends the comment onto the next line.  What
;;   Vim function is it that does this?  Is the function enabled in
;;   plain vanilla Vim 7 as shipped by vim.org?  (Check by seeing how
;;   it works on Vim for Windows running on either Windows or Wine.)
;;   Is it mostly useful or mostly annoying?  Is it worth implementing
;;   in Emacs considering there are other easy ways to create
;;   comments?
;; 
;; - In v / V mode, Vim makes sure there is always at least 1 char /
;;   line selected.  IMO it provides nice feedback as to whether
;;   visual mode is on or not.  Is this worth implementing?  This is
;;   especially important for the block mode because currently it's
;;   impossible to select the last character in a line.
;; 
;; - Sometimes when you use C (viper-change-to-eol) or other change
;;   commands, Jason's new viper-exec-change function shows a message
;;   like "Deleted 50 characters" as a side effect.  Is that annoying?
;;     - Update 1 month later:  I hardly notice the message.
;;     - Dear users:  Do you think I should disable the message?
;; 
;; - I want to allow buffer-switching without using the C-x key, since
;;   C-x b RET an extremely large amount of times per day is
;;   uncomfortable for my right pinky which presses RET.  There's
;;   already :b which seems to just invoke switch-to-buffer.  Is this
;;   right?  Is it bad if I make vimpulse emulate set autowrite=on
;;   then write new multi-buffer code?  What should the code's user
;;   interface be like?  I really should switch back to Vim for a day,
;;   learn more about how it deals with multiple buffers at once (and
;;   maybe also with tab pages) and emulate whatever of Vim's is most
;;   convenient.  What do you think of all the above?\
;;     - update: IIRC :set hidden lets you switch buffers w/o saving
;;     - update from Sebastien Rocca Serra: :set wildmenu plus 
;;       tab-completion makes :b very pleasant to use when you have 
;;       50+ buffers open.  Wildmenu is almost like iswitchb or ido.
;;     - I wonder how well that stuff works with just a few buffers open.
;; 
;; - simulate Vim's set virtualedit=onemore option to make C-x C-e
;;   possible w/o first advancing to next line?
;; 
;; - Would it be bad to edit users' .viminfo files without asking
;;   permission, or should some variable have to be customized on to do
;;   such a thing?
;; 
;; - should gj and gk do longlines-style movement like in Vim?  I
;;   really must resolve my Windows vs. Unix line-length hangups by
;;   Googling or asking before I even think about this.
;; 
;; - is there any need to implement Vim's new
;;   [count]dk-can-go-past-top-of-file-without-error functionality (to
;;   me, no need) or any related functionality?
;; 
;; - What to do about xemacs?  It doesn't ship with woman.  I wonder
;;   if woman is in some xemacs package?

;;; Change Log:

;; Version 0.2.6.3:
;;  [frank.fischer@s2001.tu-chemnitz.de:]
;;  - Support more visual-block-mode features: insert, append, delete, yank, change.
;;  - Change some vimpulse-functions and some viper-functions to handle
;;    block-mode properly.
;;  - Update documentation to reflect visual-block-mode.
;;  - The '=' command in visual-mode calls 'indent-region'.
;;
;; Version 0.2.6.2:
;;  [jasonspiro3@gmail.com:]
;;  - Improved XEmacs compatibility.
;;  - Small documentation improvements.
;; 
;; Version 0.2.6.1:
;;  [jasonspiro3@gmail.com:]
;;  - Removed duplicate definition of vimpulse-detect-mark-deactivate
;;    and duplicate add-hook call to add the hook.  I must have added
;;    the extra copies by accident when doing my last big merge; now
;;    they are gone.
;; 
;; Version 0.2.6.0:
;;  [jasonspiro3@gmail.com:]
;;  - Merged a patch for the function that powers * and #. Based on
;;    Ryoichi's patch and a cleaned-up version of Weihua's patch --
;;    thanks.  Now * and # will search for entire symbol at point,
;;    including underscores, not just word at point.
;;  - Todo addition.
;; 
;; Version 0.2.5.1:
;;  [jasonspiro3@gmail.com:]
;;  - Redefined viper-adjust-undo to do nothing.  This way, in
;;    insert mode, typing then moving the cursor then typing more
;;    counts as two separately undoable actions instead of one.
;;    Thanks to Weihua JIANG and to max_ from IRC #emacs for the idea.
;;  - Small extra TODO.
;; 
;; Version 0.2.5.0:
;;  [jasonspiro3@gmail.com:]
;;  - I've ignored my local changes for too long.  Here they are:
;;  - added keybindings from a Usenet post by Samuel Padgett
;;  - made change (cw, etc.) commands work more like Vim (my code)
;;  - I removed (setq ex-cycle-other-window nil); although it is very
;;    useful, it merely works around a problem with Viper.  I plan to
;;    discuss it with the Viper maintainer instead.
;;  - other changes and bugfixes from various people
;;  
;; Version 0.2.0.3:
;;  [jasonspiro3@gmail.com:]
;;  - Added Brad's viper-jump-to-tag-at-point
;; 
;; Version 0.2.0.2:
;;  [jasonspiro3@gmail.com:]
;;  - Small C-w keys and doc fixes.
;; 
;; Version 0.2.0.1:
;;  [cppjavaperl:]
;;  - Added support for block visual mode (i.e. rectangle selection).
;;  - Made C-p look for matches *prior* to the cursor, added C-n
;;    binding to look for matches *before* the cursor.  This works 
;;    more like Vim does.
;;  [jasonspiro3@gmail.com:]
;;  - Since vimpulse has no website, I added a prominent 
;;    pointer at the top to the installation instructions.
;; 
;; Version 0.2.0.0: Brad merged in several changes, including:
;;  - exit visual mode when the mark deactivates
;;  - changed the window manipulation to be global
;;  - added gf (goto file at point)
;;  - added \C-] and \C-t, tag jump & pop
;;  - added a helper function for defining keys
;;  - commented out show-paren-function, what is it meant to do?
;; 
;; Version 0.1.0.1: No code changes.  Small documentation changes,
;; including updates on moving-left bug.
;; 
;; Version 0.1: Initial release.

;;; Acknowledgements:

;; Thanks to <cppjavaperl@yahoo.com>, John <jn at ngedit.com>, Samuel
;; Padgett, Ryoichi Kanetaka <ryoichi.kanetaka at gmail.com>,
;; <sillyfox at yahoo.com>, Stian S., Toby Cubitt, Wang Xin, Weihua
;; JIANG <weihua.jiang at gmail.com>, Frank Fischer
;; <frank.fischer@s2001.tu-chemnitz.de> and all the other people who
;; have sent in bug reports or feedback.  Also, thanks to Michael
;; Kifer and all those who have contributed to viper-mode.
;; 
;; We love patches.  Would you like to see your name here?  Please
;; send code and/or documentation patches to the maintainer.  Ideas,
;; comments, and test results are appreciated too.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:



 



;; Begin main Vim emulation code {{{

;; Load advice.el.
(require 'advice)
;; Load redo.el if available.  Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21 doesn't
;; ship with APEL included.
(unless (featurep 'redo)
  (load "redo" 'noerror))

;;;;
;;;; Almost all of this code is taken from extended-viper 
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;;

(define-key viper-vi-global-user-map "K"    'woman)
(define-key viper-vi-global-user-map "gf"   'find-file-at-point)
(define-key viper-vi-global-user-map "gg"   'viper-goto-first-line) 
(define-key viper-vi-global-user-map "zb"   'viper-line-to-bottom)
(define-key viper-vi-global-user-map "zh"   'scroll-right)
(define-key viper-vi-global-user-map "zl"   'scroll-left)
(define-key viper-vi-global-user-map "zt"   'viper-line-to-top)
(define-key viper-vi-global-user-map "zz"   'viper-line-to-middle)
(define-key viper-vi-global-user-map "*"    'viper-search-forward-for-symbol-at-point) 
(define-key viper-vi-global-user-map "#"    'viper-search-backward-for-symbol-at-point) 
(define-key viper-vi-global-user-map " "    nil)
(define-key viper-vi-global-user-map "O"    'my-viper-open-new-line-above)
(define-key viper-vi-global-user-map "o"    'my-viper-open-new-line-below)
(define-key viper-vi-global-user-map "\C-]" 'viper-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)

; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Window manipulation
(define-key viper-vi-global-user-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-global-user-map "\C-w\C-w" 'viper-cycle-windows)
(define-key viper-vi-global-user-map "\C-ww" 'viper-cycle-windows)
(define-key viper-vi-global-user-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-wc" 'delete-window)
(define-key viper-vi-global-user-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-global-user-map "\C-wS" 'split-window-vertically)

; Block Visual Mode keys
(define-key viper-vi-global-user-map "\C-p" 'yank-rectangle)
(define-key viper-vi-global-user-map "\C-v" 'viper-visual-mode-block)

; Insert mode keys
; Vim-like completion keys
(define-key viper-insert-global-user-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-global-user-map "\C-n" 'my-viper-abbrev-expand-after)
(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)

(defvar viper-extra-ex-commands '(
      ("b" "buffer")
      ("bdelete" (viper-kill-current-buffer))
      ("bnext" "next")
      ("syntax" (global-font-lock-mode))
      ("split" (split-window))
      ; Emacs and Vim use inverted naming conventions for splits.
      ("vsplit" (split-window-horizontally))
))
 
;;; My code (Alessandro)
(defun my-viper-open-new-line-above (&optional arg)
  (interactive)
  (viper-Open-line arg)
  (indent-according-to-mode))
(defun my-viper-open-new-line-below (&optional arg)
  (interactive)
  (viper-open-line arg)
  (indent-according-to-mode))

;;; His code (Brad)
(defun viper-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1)) 

(defun viper-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)) 

(defun viper-cycle-windows ()
  "Cycle point to another window."
  (interactive) 
  (select-window (next-window)))

(defun viper-search-for-symbol-at-point (whether-forward)
  "Search forwards or backwards for the symbol under point."
  (let ((symbol (concat "\\<" (thing-at-point 'symbol) "\\>")))
    (setq viper-s-string symbol)
    (setq viper-s-forward whether-forward)
    (viper-search symbol whether-forward 1)))

(defun viper-search-forward-for-symbol-at-point ()
  (interactive)
  (viper-search-for-symbol-at-point t))

(defun viper-search-backward-for-symbol-at-point ()
  (interactive)
  (viper-search-for-symbol-at-point nil))

(defun viper-jump-to-tag-at-point ()
 (interactive)
 (let ((tag (thing-at-point 'word)))
   (find-tag tag)))

;;; Manipulation of Vipers functions by using the advice feature
;;; Many of the functions here rely as heavily on Viper's internals as Viper itself
;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defadvice viper-ex (around viper-extended-ex-commands (arg &optional string) activate)
  ad-do-it) 

(setq ex-token-alist (append viper-extra-ex-commands ex-token-alist))
;;End of Brad's code

;; This function replaces viper's original viper-exec-change function
;; which is invoked by key sequences starting with 'c'.  When the user
;; requests a command like 'cw', this function calls a sequence like
;; 'dwi' instead.  This stops viper from indicating the change
;; operation with distracting colored overlays and $ signs.  Instead,
;; it simply deletes the text then enters Insert mode, like Vim does.
;; 
;; The function works fine at eol and eob but TODO: understand the
;; original viper-exec-change command and see if mine does everything
;; it does.
(defun viper-exec-change (m-com com)
  (viper-exec-delete m-com com)
  (if (eq m-com 'viper-goto-eol)
      ; use viper-append here since vi's C (change to end of line)
      ; command works differently than c
      (viper-append nil) 
    (viper-insert nil)))

(defun viper-adjust-undo ()
  "This viper function has been redefined by vimpulse.el to 
do nothing.  This way, in insert mode, typing then moving 
the cursor then typing more counts as two separately undoable 
actions instead of one."
  )

;;; cppjavaperl's code
(defun my-viper-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Paren Matching workaround to work more like viper                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: What is this code _trying_ to do?  It some bugs with deleting ()'s 
;; FIXME: it advances the cursor (and let it there on unmatched paren!)
;; (defadvice show-paren-function (around viper-add-visual-maps activate)
;;   "modifies paren matching under viper to work like in (almost) like in vim"
;;   (if viper-vi-basic-minor-mode
;;       (cond
;;        ((= (char-after (point)) ?\)) ;; FIXME!!!!!
;; 	(forward-char)
;; 	ad-do-it
;; 	(backward-char))
;;        ((= (char-after (- (point) 1)) ?\)) nil)
;;        (t ad-do-it))
;;     ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; VISUAL MODE HACKS ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key viper-vi-basic-map "v" 'viper-visual-mode)
(define-key viper-vi-basic-map "V" 'viper-visual-mode-linewise)

;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
(defun vimpulse-set-movement-keys-for-map (map)
  (define-key map "\C-d" 'viper-scroll-up)
  (define-key map "\C-u" 'viper-scroll-down)
  (define-key map "j" 'viper-next-line)
  (define-key map "k" 'viper-previous-line)
  (define-key map "l" 'viper-forward-char)
  (define-key map "h" 'viper-backward-char))

;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;(setq vimpulse-movement-map (make-sparse-keymap))
;(vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;(viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map) 
;(viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

;; }}} End main Vim emulation code

(provide 'vimpulse)



 



;; Begin visual mode code {{{

(eval-when-compile (require 'easy-mmode))

;; local variables
(defgroup viper-visual nil
  "visual-mode for viper"
  :prefix "viper-visual-"
  :group 'emulations)

 (define-minor-mode viper-visual-mode
  "Toggles visual mode in viper"
  :lighter " visual"
  :initial-value nil
  :global nil
  :group 'viper-visual)   
(defvar viper-visual-mode-map (make-sparse-keymap)
  "Viper Visual mode keymap. This keymap is active when viper is in VISUAL mode")
(defvar viper-visual-mode-linewise nil
  "If non nil visual mode will operate linewise")
(defvar viper-visual-mode-block nil
  "If non nil visual mode will operate blockwise")
(defcustom viper-visual-load-hook nil
  "Hooks to run after loading viper-visual-mode."
  :type 'hook
  :group 'viper-visual)

(defadvice viper-move-marker-locally (around viper-move-marker-locally-wrap activate)
 (unless viper-visual-mode
   ad-do-it))

(defadvice viper-deactivate-mark (around viper-deactivate-mark-wrap activate)
 (unless viper-visual-mode
   ad-do-it))

;; this thing is just to silence the byte compiler
;; and stop it bugging about free variable
;; viper--key-maps in emacs 21 :)
(defmacro my-get-emulation-keymap ()
  (if (>= emacs-major-version 22)
      'viper--key-maps
    'minor-mode-map-alist))

(defadvice viper-normalize-minor-mode-map-alist (after viper-add-visual-maps activate)
  "This function modifies minor-mode-map-alists to include the visual mode keymap"
    (push (cons 'viper-visual-mode viper-visual-mode-map) (my-get-emulation-keymap)))

;; Keys that differ from normal mode
(defun viper-visual-mode-to-insert-mode () ;TODO: fix behavior to behave like vim
  (interactive)
  (viper-visual-mode 'toggle)
  (viper-change-state-to-insert))



;; The next definitions are for visual-block-mode

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region.
(defvar viper-visual-insert-coords nil
  "A list with (i-com ul-pos col nlines), where
`i-com' is the insert command (?i, ?a, ?I or ?A)
`ul-pos' is the position of the upper left corner of the region
`col' is the column of insertion
`nlines' is the number of lines in the region")

(defun viper-create-coords (i-com)
  "Updates the list of block insert coordinates with the current rectangle.
`i-com' should be ?c, ?i, ?a, ?I or ?A, the column for the insertion will be
chosen according to this command."
  
  (make-local-variable 'viper-visual-insert-coords)
  (setq viper-visual-insert-coords nil)
  
  (let ((nlines (count-lines (region-end) (region-beginning)))
	(col 0)) ; For ?I and ?A trivial: column is 0
    (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
      ;; for ?i and ?a chose the left (the right) column of the rectangle
      (let ((start-col (save-excursion 
			 (goto-char (region-beginning))
			 (current-column)))
	    (end-col (save-excursion
		       (goto-char (region-end))
		       (current-column))))
	;; decide if we use the left or the right column
	(setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
			     (min start-col end-col)
			     (1- (max start-col end-col)))))))
    
    ;; Ok we have all information, so go to the insert-point ...
    (goto-char (region-beginning)) 
    (move-to-column col)
    ;; ... and save the information
    (setq viper-visual-insert-coords 
	  (list i-com (point) col nlines))))


(defun viper-visual-insert (&optional arg)
  "Called when in visual mode to go insert mode at the beginning of the selection"
  (interactive "P")
  
  (when viper-visual-mode-linewise (viper-create-coords ?I))
  (when viper-visual-mode-block (viper-create-coords ?i))
  (viper-visual-mode nil)
  (if viper-visual-mode-linewise
      (viper-Insert arg)
      (viper-insert arg)))


(defun viper-visual-append (&optional arg)
  "Called when in visual mode to go to insert mode at the end of the selection"
  (interactive "P")
  
  (when viper-visual-mode-linewise (viper-create-coords ?A))
  (when viper-visual-mode-block (viper-create-coords ?a))
  (viper-visual-mode nil)
  (if viper-visual-mode-linewise
      (viper-Append arg)
      (viper-append arg)))


(defun connect-undos (n undo-list)
  "Connects the last n undo steps in undo-list to one step"
  (when (and (listp undo-list) 
	     (listp (cdr undo-list)) 
	     (> n 1))
    (if (null (cadr undo-list))
	(progn 
	  (setcdr undo-list (cddr undo-list))
	  (connect-undos (1- n) undo-list))
        (connect-undos n (cdr undo-list)))))


;; Redefinitions of viper functions to handle visual block-mode
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when viper-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((i-com (car viper-visual-insert-coords))
	  (pos (cadr viper-visual-insert-coords))
	  (col (caddr viper-visual-insert-coords))
	  (nlines (cadddr viper-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
	(dotimes (i (1- nlines))
	  (forward-line 1)
	  (let ((cur-col (move-to-column col)))
	    ;; If we are in block mode this line but do not hit the correct 
            ;; column, we check if we should convert tabs and/or append spaces
	    (if (and viper-visual-mode-block
		     (or (/= col cur-col) ;; wrong column or 
			 (eolp)))         ;; end of line 
		(cond ((< col cur-col) ;; we are inside a tab 
		       (move-to-column (1+ col) 'fill)   ;; -> convert to spaces
		       (move-to-column col 'fill) ;; this is needed for ?a
		       (viper-repeat nil))
		      ((and (>= col cur-col) ;; we are behind the end
			    (eq i-com ?a))           ;; and i-com is ?a
		       (move-to-column (1+ col) t)   ;; -> append spaces
		       (viper-repeat nil)))
		    
	        (viper-repeat nil)))))
      (setq viper-visual-insert-coords nil)
    
      ;; update the last two undos
      (if (> nlines 1)
	  (if (eq i-com ?c)
	      (connect-undos 3 buffer-undo-list)      ; delete, insert, repeat
	      (connect-undos 2 buffer-undo-list))     ; insert, repeat
	  (if (eq i-com ?c)
	      (connect-undos 2 buffer-undo-list)      ; delete, insert
	      (connect-undos 1 buffer-undo-list)))))) ; insert


(defun viper-visual-yank-command ()
  "Saves the visual selection in the kill-ring"
  (interactive)
  (viper-visual-mode nil)
  (if viper-visual-mode-block
      (progn 
	;(rm-kill-ring-save (region-beginning) (region-end))
	(kill-rectangle (region-beginning) (region-end))
	(goto-char (region-beginning))
	(yank-rectangle)
	(goto-char (region-beginning)))
      (viper-prefix-arg-com ?r 1 ?y)))


(defun viper-visual-delete-command ()
  "Deletes the visual selection"
  (interactive)
  (viper-visual-mode nil)
  (if viper-visual-mode-block
      (progn 
	;(rm-kill-region (region-beginning) (region-end))
	(kill-rectangle (region-beginning) (region-end))
	(goto-char (region-beginning)))
      (viper-prefix-arg-com ?r 1 ?d)))


(defun viper-visual-change-command ()
  "Called when in visual (block) mode to delete the selected region and go to insert mode"
  (interactive)
  
  (if viper-visual-mode-block
      (let ((beg (region-beginning))
	    (end (region-end)))
	(viper-create-coords ?c)
	(kill-rectangle beg end)
	(viper-visual-mode nil) 
	(viper-insert nil))
      (progn
	(viper-visual-mode nil)
	(viper-prefix-arg-com ?r 1 ?c))))

 
(defun viper-visual-replace-region (&optional arg)
  (interactive "P")
  (viper-visual-mode 'toggle)
  (cond
   ((= (mark) (point)) nil)
   (t 
    (if (< (mark) (point)) (exchange-point-and-mark))
    (viper-replace-char arg)		
    (let ((c (char-after (point))))
      (dotimes (i (- (mark) (point)))
	(cond
	 ((member (char-after (point)) '(?\r ?\n))
	  (forward-char))
	  (t (delete-char 1)
	     (insert c))))))))

(define-key viper-visual-mode-map "v" 'viper-visual-mode)
(define-key viper-visual-mode-map "V" 'viper-visual-mode)
(define-key viper-visual-mode-map "\C-v" 'viper-visual-mode)
(define-key viper-visual-mode-map "d" 'viper-visual-delete-command)
(define-key viper-visual-mode-map "x" 'viper-visual-delete-command)
(define-key viper-visual-mode-map "D" 'viper-visual-delete-command)
(define-key viper-visual-mode-map "d" 'viper-visual-delete-command)
(define-key viper-visual-mode-map "y" 'viper-visual-yank-command)
(define-key viper-visual-mode-map "i" 'viper-visual-mode-to-insert-mode)
(define-key viper-visual-mode-map "u" 'viper-visual-mode)
(define-key viper-visual-mode-map "c" 'viper-visual-change-command)
(define-key viper-visual-mode-map "F" 'viper-visual-change-command)
(define-key viper-visual-mode-map "c" 'viper-visual-change-command)
(define-key viper-visual-mode-map "C" 'viper-visual-change-command)
(define-key viper-visual-mode-map "s" 'viper-visual-change-command)
(define-key viper-visual-mode-map "S" 'viper-visual-change-command)
(define-key viper-visual-mode-map "r" 'viper-visual-replace-region)
(define-key viper-visual-mode-map "o" 'exchange-point-and-mark)
(define-key viper-visual-mode-map "O" 'exchange-point-and-mark)
(define-key viper-visual-mode-map "I" 'viper-visual-insert)
(define-key viper-visual-mode-map "A" 'viper-visual-append)
(define-key viper-visual-mode-map "=" 'indent-region)
;; Keys that have no effect in visual mode
(define-key viper-visual-mode-map "t" 'undefined)
(define-key viper-visual-mode-map "." 'undefined)
(define-key viper-visual-mode-map "T" 'undefined)
(add-hook 'post-command-hook '(lambda ()
				(if (and viper-visual-mode viper-visual-mode-linewise)
				    (beginning-of-line))))
   

(defun vimpulse-set-mark (pos)
  "Sets the region respecting the Emacsen-version, activates highlighting"
  (set-mark pos)
  (when (fboundp 'activate-region) 
    (activate-region))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Force transient-mark-mode to have visual selection ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (fboundp 'transient-mark-mode)
    (transient-mark-mode t)))

(defun vimpulse-deactivate-mark ()
  "Deactivates the region respecting the Emacsen-version and type"
  (interactive)
  (if (and viper-visual-mode-block (fboundp 'rm-deactivate-mark))
      (rm-deactivate-mark)
      (viper-deactivate-mark)))

;;;###auto-load
(defun viper-visual-mode-toggle (&optional arg)
  (interactive "P")
  (make-local-variable 'viper-visual-mode-linewise)
  (unless viper-visual-mode
    (vimpulse-deactivate-mark)
    (viper-change-state-to-vi))
  (when viper-visual-mode
    (setq viper-visual-mode-linewise nil)
    (setq viper-visual-mode-block nil)
    (vimpulse-set-mark (point))))


(defun viper-visual-mode-linewise (&optional arg)
  "Starts viper visual mode in `linewise' mode"
  (interactive "P")
  (beginning-of-line)
  (viper-visual-mode 'toggle)
  (setq viper-visual-mode-linewise t))
(add-hook 'viper-visual-mode-hook 'viper-visual-mode-toggle t)
(run-hooks 'viper-visual-load-hook)

(defun viper-visual-mode-block (&optional arg)
  "Starts viper visual mode in `block' mode"
  (interactive "P")
  (viper-visual-mode t)
  (setq viper-visual-mode-block t)
  ;; perhaps a bad hack -> rm-set-mark deactivates the normal mark
  (when (fboundp 'rm-set-mark) 
    (rm-set-mark nil)))

;; We need to detect when a command has deactivated the mark so that
;; Vimpulse is able to exit Visual mode
(defun vimpulse-detect-mark-deactivate ()
  (when (and viper-visual-mode (not mark-active))
    (viper-visual-mode 'toggle)))
(add-hook 'deactivate-mark-hook 'vimpulse-detect-mark-deactivate)

(provide 'viper-visual-mode)

;; }}} End visual mode code
