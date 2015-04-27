;;; tabbar-ruler.el --- Pretty tabbar, autohide, use both tabbar/ruler
;;
;; Filename: tabbar-ruler.el
;; Description: Changes tabbar setup to be similar to Aquaemacs.
;; Author: Matthew Fidler, Ta Quang Trung, Nathaniel Cunningham
;; Maintainer: Matthew L. Fidler
;; Created: Mon Oct 18 17:06:07 2010 (-0500)
;; Version: 0.43
;; Last-Updated: Sat Dec 15 15:44:34 2012 (+0800)
;;           By: Matthew L. Fidler
;;     Update #: 663
;; URL: http://github.com/mlf176f2/tabbar-ruler.el
;; Keywords: Tabbar, Ruler Mode, Menu, Tool Bar.
;; Compatibility: Windows Emacs 23.x
;; Package-Requires: ((tabbar "2.0.1"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; * Introduction
;; Tabbar ruler is an emacs package that allows both the tabbar and the
;; ruler to be used together.  In addition it allows auto-hiding of the
;; menu-bar and tool-bar.
;; 
;; 
;; Tabbar appearance based on reverse engineering Aquaemacs code and
;; changing to my preferences, and Emacs Wiki.
;; 
;; Tabbar/Ruler integration is new. Tabbar should be active on mouse
;; move.  Ruler should be active on self-insert commands.
;; 
;; Also allows auto-hiding of toolbar and menu.
;; 
;; To use this, put the library in your load path and use
;; 
;; 
;;   (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;   (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;;   (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;;   (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;;   (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
;;                                         ; scroll bar when your mouse is moving.
;;   (require 'tabbar-ruler)
;;   
;; 
;; 
;; 
;; * Changing how tabbar groups files/buffers
;; The default behavior for tabbar-ruler is to group the tabs by frame.
;; You can change this back to the old-behavior by:
;; 
;;   (tabbar-ruler-group-buffer-groups)
;; 
;; or by issuing the following code:
;; 
;; 
;;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
;; 
;; 
;; In addition, you can also group by projectile project easily by:
;; 
;; 
;;   (tabbar-ruler-group-by-projectile-project)
;; 
;; * Adding key-bindings to tabbar-ruler
;; You can add key-bindings to change the current tab.  The easiest way
;; to add the bindings is to add a key like:
;; 
;; 
;;   (global-set-key (kbd "C-c t") 'tabbar-ruler-move)
;; 
;; 
;; After that, all you would need to press is Control+c t and then the
;; arrow keys will allow you to change the buffer quite easily.  To exit
;; the buffer movement you can press enter or space.
;; 
;; * Known issues
;; the left arrow is text instead of an image.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 13-Sep-2014    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Version bump
;; 1-Jul-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Fix variable misspecification
;; 28-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Fixed strange org-readme issue
;; 28-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375) #663 (Matthew L. Fidler)
;;    Added popup scrollbarbar 
;; 27-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Added autoload for tabbar-install-faces.  That way ergoemacs and other
;;    packages can load the tabbar-ruler by just calling (tabbar-install-faces)
;; 6-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Added left-char and right char to tabbar-ruler-move-keymap so that
;;    keybindings in emacs 24.3 work correctly.
;; 6-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Changed movement commands.  The movement commands are simpler (in my opinion)
;; 4-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Change package description.  Fixed the documentation to actually
;;    change to the old tabbar method of grouping buffers.
;; 4-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Turn off ruler mode in the next buffer (if necessary)
;; 4-Jun-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Add movement keys.  Also add toggles for different groupings.
;; 1-May-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Try to address issue #4
;; 1-May-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Changed the modified font to italics.  Made the modified symbol
;;    customizable, but off by default.  Should address issue #5.
;; 5-Apr-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Trying to update upstream sources.
;; 5-Apr-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Fixed speed issues on windows.  It wasn't a redraw that was causing
;;    the speed issues, it was the constant recreation of the right-click
;;    menus... 
;; 27-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Fixed typo to fix issue #2.
;; 27-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Attempt to fix issue #2.  Whenever the color is not a string, assume
;;    that it should be transparent.  I'm unsure if the mac osx puts the
;;    translated color to a string.  However, it seems that the undefined
;;    should be the same as transparent.  Therefore, this fix *should* work...
;; 20-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Add inverse video option for unselected tabbar.  Made it the default.
;;    has better contrast between the selected and unselected tabs.
;; 20-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Changed emacs 24.3 to support the times character.  Also removed
;;    starred documentation strings.
;; 20-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Emacs 24.3 had an error when using ucs-insert.  Added fallbacks so
;;    that this works when ucs-insert does not work.
;; 20-Feb-2013    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Changed so that the separators do not need to be fancy images.  I
;;    found that when the separators were images, it slowed down emacs on
;;    windows.  Therefore, the fancy images are disabled by default.  This
;;    also includes the stylized close symbols.
;; 19-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Added back popup-menu
;; 19-Dec-2012    Matthew L. Fidler
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Upload to marmalade
;; 19-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Changed slope.  Made the background color the default background color
;;    if unspecified.  Made tabbar-hex-color return "None" if not defined
;; 15-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Made sure that the tabbr-ruler-separator-image is at least 17 pixels high
;; 15-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 15 15:44:34 2012 (+0800) #663 (Matthew L. Fidler)
;;    Attempt to fix another bug on load
;; 14-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Fixed tabbar ruler so that it loads cold.
;; 14-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Memoized the tabbar images to speed things up
;; 14-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Mat`'thew L. Fidler)
;;    Upload to Marmalade 
;; 14-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Fancy tabs
;; 13-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Added Bug fix for coloring.  Made the selected tab match the default
;;    color in the buffer.  Everything else is grayed out.
;; 10-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Took out a statement that may fix the left-scrolling bug?
;; 10-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Added package-menu-mode to the excluded tabbar-ruler fight modes.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Will no longer take over editing of org source blocks or info blocks.
;; 07-Dec-2012    Matthew L. Fidler
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Changed the order of checking so that helm will work when you move a mouse.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Now works with Helm.  Should fix issue #1
;; 06-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Mar  1 09:02:56 2012 (-0600) #659 (Matthew L. Fidler)
;;    Now colors are based on loaded theme (from minibar).  Also added
;;    bug-fix for setting tabbar colors every time a frame opens.  Also
;;    added a bug fix for right-clicking a frame that is not associated with
;;    a buffer.
;; 1-Mar-2012    Matthew L. Fidler
;;    Last-Updated: Thu Mar  1 08:38:09 2012 (-0600) #656 (Matthew L. Fidler)
;;    Will not change tool-bar-mode in Mac.  It causes some funny
;;    things to happen.
;; 9-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Feb  9 19:18:21 2012 (-0600) #651 (Matthew L. Fidler)
;;    Will not change the menu bar in a Mac.  Its always there.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Jan 14 21:58:51 2012 (-0600) #648 (Matthew L. Fidler)
;;    Added more commands that trigger the ruler.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Jan 14 21:44:32 2012 (-0600) #641 (Matthew L. Fidler)
;;    Added more ruler commands.   It works a bit better
;;    now. Additionally I have changed the ep- to tabbar-ruler-.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 15:01:27 2011 (-0600) #639 (Matthew L. Fidler)
;;    Changed EmacsPortable to tabbar-ruler
;; 08-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 14:59:57 2011 (-0600) #638 (Matthew L. Fidler)
;;    Added ELPA tags.  
;; 08-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 12:47:09 2011 (-0600) #604 (Matthew L. Fidler)
;;    Removed xpm dependencies.  Now no images are required, they are built by the library.
;; 04-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec  4 16:27:07 2010 (-0600) #551 (Matthew L. Fidler)
;;    Added context menu.
;; 01-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Dec  1 15:26:37 2010 (-0600) #341 (Matthew L. Fidler)
;;    Added scratch buffers to list.
;; 04-Nov-2010
;;    Last-Updated: Thu Nov  4 09:39:14 2010 (-0500) (us041375)
;;    Made tabbar mode default.
;; 02-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  2 10:14:12 2010 (-0500) (Matthew L. Fidler)
;;    Make post-command-hook handle errors gracefully.
;; 20-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;
;;    Changed behavior when outside the window to assume the last
;;    known mouse position. This fixes the two problems below.
;;
;; 20-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;
;;    As it turns out when the toolbar is hidden when the mouse is
;;    outside of the emacs window, it also hides when navigating the
;;    menu.  Switching behavior back.
;;
;; 20-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Made popup menu and toolbar be hidden when mouse is oustide of emacs window.
;; 20-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Changed to popup ruler-mode if tabbar and ruler are not displayed.
;; 19-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Changed tabbar, menu, toolbar and ruler variables to be buffer
;;    or frame local.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(eval-when-compile
  (require 'cl))
(require 'tabbar)
(require 'easymenu)


(defun tabbar-popup-menu ()
  "Keymap for pop-up menu.  Emacs only."
  `(,(format "%s" (nth 0 tabbar-last-tab))
    ["Close" tabbar-popup-close]
    ["Close all BUT this" tabbar-popup-close-but]
    "--"
    ["Save" tabbar-popup-save]
    ["Save As" tabbar-popup-save-as]
    "--"
    ["Rename File" tabbar-popup-rename
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
    ["Delete File" tabbar-popup-delete
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
    "--"
    ["Gzip File" tabbar-popup-gz
     :active (and (executable-find "gzip")
                  (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
                  (not (string-match "\\.gz\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
    ["Bzip File" tabbar-popup-bz2
     :active (and (executable-find "bzip2")
                  (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
                  (not (string-match "\\.bz2\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
    ["Decompress File" tabbar-popup-decompress
     :active (and
              (buffer-file-name (tabbar-tab-value tabbar-last-tab))
              (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
              (string-match "\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'"
                            (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
     ]
    ;;    "--"
    ;;    ["Print" tabbar-popup-print]
    ))

(defun tabbar-popup-print ()
  "Print Buffer"
  (interactive))

(defun tabbar-popup-close ()
  "Tab-bar pop up close"
  (interactive)
  (funcall tabbar-close-tab-function tabbar-last-tab))

(defun tabbar-popup-close-but ()
  "Tab-bar close all BUT this buffer"
  (interactive)
  (let ((cur (symbol-value (funcall tabbar-current-tabset-function))))
    (mapc (lambda(tab)
            (unless (eq tab tabbar-last-tab)
              (funcall tabbar-close-tab-function tab)))
          cur)))

(defun tabbar-popup-save-as ()
  "Tab-bar save as"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab)))
    (save-excursion
      (set-buffer buf)
      (call-interactively 'write-file))))

(defun tabbar-popup-rename ()
  "Tab-bar rename"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (save-excursion
      (set-buffer buf)
      (when (call-interactively 'write-file)
        (if (string= fn (buffer-file-name (current-buffer)))
            (error "Buffer has same name.  Just saved instead.")
          (delete-file fn))))))

(defun tabbar-popup-delete ()
  "Tab-bar delete file"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (when (yes-or-no-p (format "Are you sure you want to delete %s?" buf))
      (save-excursion
        (set-buffer buf)
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))
        (delete-file fn)))))

(defun tabbar-popup-remove-compression-ext (file-name &optional new-compression)
  "Removes compression extension, and possibly adds a new extension"
  (let ((ret file-name))
    (when (string-match "\\(\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)?\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'" ret)
      (setq ret (replace-match (concat (or new-compression "") (match-string 2 ret)) t t ret)))
    (symbol-value 'ret)))

(defun tabbar-popup-gz (&optional ext err)
  "Gzips the file"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf))
         (nfn (tabbar-popup-remove-compression-ext fn (or ext ".gz"))))
    (if (string= fn nfn)
        (error "Already has that compression!")
      (save-excursion
        (set-buffer buf)
        (write-file nfn)
        (if (not (file-exists-p nfn))
            (error "%s" (or err "Could not gzip file!"))
          (when (file-exists-p fn)
            (delete-file fn)))))))

(defun tabbar-popup-bz2 ()
  "Bzip file"
  (interactive)
  (tabbar-popup-gz ".bz2" "Could not bzip the file!"))

(defun tabbar-popup-decompress ()
  "Decompress file"
  (interactive)
  (tabbar-popup-gz "" "Could not decompress the file!"))

(defun tabbar-context-menu ()
  "Pop up a context menu."
  (interactive)
  (popup-menu (tabbar-popup-menu)))


(defun tabbar-hex-color (color)
  "Gets the hexadecimal value of a color"
  (let ((ret color))
    (cond
     ((not (eq (type-of color) 'string))
      (setq ret "None"))
     ((string= "#" (substring color 0 1))
      (setq ret (upcase ret)))
     ((color-defined-p color)
      (setq ret (concat "#"
                        (mapconcat
                         (lambda(val)
                           (format "%02X" (* val 255)))
                         (color-name-to-rgb color) ""))))
     (t (setq ret "None")))
    (symbol-value 'ret)))

(defcustom tabbar-ruler-swap-faces nil
  "Swap the selected / unselected tab colors"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-invert-deselected t
  "Invert deselected tabs"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-modified-symbol nil
  "Add modified symbol in addition to changing the face."
  :type 'boolean
  :type 'tabbar-ruler)

;;;###autoload
(defun tabbar-install-faces (&optional frame)
  "Installs faces for a frame."
  (interactive)
  (copy-face 'mode-line 'tabbar-default frame)
  (if tabbar-ruler-swap-faces
      (progn
        (copy-face 'default 'tabbar-selected frame)
        (copy-face 'shadow 'tabbar-unselected frame)
        (if tabbar-ruler-invert-deselected
            (progn
              (copy-face 'tabbar-selected 'tabbar-unselected)
              (set-face-attribute 'tabbar-selected frame
                                  :box nil)
              (invert-face 'tabbar-selected))
          (set-face-attribute 'tabbar-selected frame
                              :inherit 'mode-line-buffer-id
                              :background (face-attribute 'mode-line-inactive :background)
                              :box nil))
        (copy-face 'mode-line-buffer-id 'tabbar-unselected-highlight frame)
        (copy-face 'mode-line-inactive 'tabbar-selected-highlight frame))
  (copy-face 'default 'tabbar-selected frame)
  (copy-face 'shadow 'tabbar-unselected frame)
  
  (if tabbar-ruler-invert-deselected
      (progn
        (copy-face 'tabbar-selected 'tabbar-unselected)
        (set-face-attribute 'tabbar-unselected frame
                            :box nil)
        (invert-face 'tabbar-unselected))
    (set-face-attribute 'tabbar-unselected frame
                        :inherit 'mode-line-buffer-id
                        :background (face-attribute 'mode-line-inactive :background)
                        :box nil))
  
  
  (copy-face 'mode-line-buffer-id 'tabbar-selected-highlight frame)
  (copy-face 'mode-line-inactive 'tabbar-unselected-highlight frame))
  
  (set-face-attribute 'tabbar-separator frame
                      :inherit 'tabbar-default
                      :box nil)
  
  (set-face-attribute 'tabbar-button frame
                      :inherit 'tabbar-default
                      :box nil))

(add-hook 'after-make-frame-functions 'tabbar-install-faces)
(add-hook 'emacs-startup-hook 'tabbar-install-faces)
(tabbar-install-faces)


;; Taken from powerline

(defun tabbar-create-or-get-tabbar-cache ()
  "Return a frame-local hash table that acts as a memoization
cache for tabbar. Create one if the frame doesn't have one
yet."
  (or (frame-parameter nil 'tabbar-cache)
      (let ((table (make-hash-table :test 'equal)))
        ;; Store it as a frame-local variable
        (modify-frame-parameters nil `((tabbar-cache . ,table)))
        table)))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun tabbar-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the tabbar-memoized function over
the original function.  Use frame-local memoization."
  (typecase func
    (symbol (fset func (tabbar-memoize-wrap-frame-local (symbol-function func))) func)
    (function (tabbar-memoize-wrap-frame-local func))))

(defun tabbar-memoize-wrap-frame-local (func)
  "Return the tabbar-memoized version of FUNC.  The memoization cache is
frame-local."
  (let ((cache-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(tabbar-memoized function)")
       (let* ((,cache-sym (tabbar-create-or-get-tabbar-cache))
              (,val-sym (gethash ,args-sym ,cache-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,cache-sym))))))

(defun tabbar-ruler-tab-separator-image (face1 face2 &optional face3 next-on-top slope height)
  "Creates a Tabbar Ruler Separator Image.
FACE1 is the face to the left
FACE2 is the face to the right
FACE3 is the background face (optional)

When FACE1 is nil and FACE2 is present this function creates the
first tab image.

When FACE2 is nil and FACE1 is present this function creates the
last tab image.

When FACE1 = FACE2, this creates a non-selected separator

When FACE1 does not equal FACE2, this creates a selected separator
"
  (let* ((h (or height (max 20 (frame-char-height))))
         (m (or slope 2))
         (w (/ h m))
         (i h)
         x1 x2 e1 e2 e3 e4
         (color1 (if face1 (tabbar-hex-color (face-attribute face1 :background)) "None"))
         (color1-border (if face1 (tabbar-hex-color (face-attribute face1 :foreground)) "None"))
         (color2 (if face2 (tabbar-hex-color (face-attribute face2 :background)) "None"))
         (color2-border (if face2 (tabbar-hex-color (face-attribute face2 :foreground)) "None"))
         (color-background (if face3 (tabbar-hex-color (face-attribute face3 :background)) (tabbar-hex-color (face-attribute 'default :background))))
         (ret "/* XPM */\nstatic char * "))
    (cond
     ((string= color1 color2)
      (setq ret (concat ret "tabbar_ruler_default_separator")))
     ((not face2)
      (setq ret (concat ret "tabbar_ruler_separator_end")))
     (t
      (setq ret (concat ret "tabbar_ruler_separator_end_sel"))))
    (setq ret (concat ret "[] = {\n"))
    (setq ret (format "%s\"%s %s 5 1\",\n" ret (round w) (- h 1)))
    ;; Now do colors
    (setq ret (format "%s\"  c %s\",\n" ret color-background))
    (setq ret (format "%s\". c  %s\",\n" ret color1))
    (setq ret (format "%s\"> c %s\",\n" ret color1-border))
    (setq ret (format "%s\"= c %s\",\n" ret color2))
    (setq ret (format "%s\"+ c %s\"" ret color2-border))
    (while (>= i 1)
      (setq x1 (round (+ 1 (/ (- i 1) m))))
      (setq x2 (round (/ (- (+ h m) i) m)))
      (cond
       ((and face2 (>= x1 x2))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) (if (not face1)?  ?.))))
        (if (= x1 x2)
            (progn
              (if (or next-on-top (not face1))
                  (setq e2 "+")
                (setq e2 ">"))
              (setq e3 "")
              (setq e4 ""))
          (if face1
              (setq e2 ">")
            (setq e2 ""))
          (setq e3 (make-string (- x1 x2) ? ))
          (setq e4 "+"))
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=))))
       ((or (and face1 (not face2))
            (and (< x1 x2) (not (or next-on-top (not face1)))))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) ?.)))
        (setq e2 ">")
        (setq e3 "")
        (setq e4 "")
        (if (= x2 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x2) (if (not face2) ?  ?=)))))
       ((and (< x1 x2) (or next-on-top (not face1)))
        (if (= x1 1)
            (setq e1 "")
          (setq e1 (make-string (- x1 1) (if (not face1) ?  ?.))))
        (setq e2 "+")
        (setq e3 "")
        (setq e4 "")
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=)))))
      (setq ret (format "%s,\n\"%s%s%s%s%s\"" ret e1 e2 e3 e4 e5))
      (setq i (- i 1)))
    (setq ret (format "%s};" ret))
    (symbol-value 'ret)))


(defun* tabbar-ruler-image (&key type disabled color)
  "Returns the scroll-images"
  (let ((clr2 (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :background))
                (tabbar-hex-color (face-attribute 'mode-line :background))))
        (clr (or color (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :foreground))
                         (tabbar-hex-color (face-attribute 'mode-line :foreground))))))
    (if (eq type 'close)
        (format "/* XPM */
        static char * close_tab_xpm[] = {
        \"14 11 3 1\",
        \"       c None\",
        \".      c %s\",
        \"+      c %s\",
        \"     .....    \",
        \"    .......   \",
        \"   .........  \",
        \"  ... ... ... \",
        \"  .... . .... \",
        \"  ..... ..... \",
        \"  .... . .... \",
        \"  ... ... ... \",
        \"   .........  \",
        \"    .......   \",
        \"     .....    \"};" clr clr2)
      
      (format
       "/* XPM */
static char * scroll_%s_%s_xpm[] = {
\"17 17 2 1\",
\"       c None\",
\".      c %s\",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
%s
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \"};
" (symbol-name type)
(if disabled "disabled" "enabled")
clr
(cond
 ((eq 'right type)
  "\"                 \",
\"     ..          \",
\"     ....        \",
\"     ......      \",
\"     .....       \",
\"     ...         \",
"
  )
 ((eq 'left type)
  "\"                 \",
\"          ..     \",
\"        ....     \",
\"      ......     \",
\"       .....     \",
\"         ...     \","
  )
 ((eq 'up type)
  "\"        .        \",
\"       ..        \",
\"       ...       \",
\"      ....       \",
\"      .....      \",
\"      .....      \",")
 ((eq 'down type)
  "\"      .....      \",
\"      .....      \",
\"      ....       \",
\"       ...       \",
\"       ..        \",
\"        .        \","))))))


(defconst tabbar-home-button-enabled-image
  `((:type xpm :data ,(tabbar-ruler-image :type 'down)))
  "Default image for the enabled home button.")

(defconst tabbar-home-button-disabled-image
  `((:type xpm :data ,(tabbar-ruler-image :type 'up)))
  "Default image for the disabled home button")


(defconst tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
            (cons "[x]" tabbar-home-button-disabled-image)))

(setq tabbar-buffer-home-button
      (cons (cons "[+]" tabbar-home-button-enabled-image)
            (cons "[-]" tabbar-home-button-disabled-image)))

(setq tabbar-scroll-left-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'left))))

(setq tabbar-scroll-left-button-disabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'left :disabled t))))

(setq tabbar-scroll-left-button
      (cons (cons " <" tabbar-scroll-left-button-enabled-image)
            (cons " =" tabbar-scroll-left-button-disabled-image)))

(setq tabbar-scroll-right-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'right))))

(setq tabbar-scroll-right-button-disabled-image
      `((:type xpm :data  ,(tabbar-ruler-image :type 'right :disabled t))))

(setq tabbar-scroll-right-button
      (cons (cons " >" tabbar-scroll-right-button-enabled-image)
            (cons " =" tabbar-scroll-right-button-disabled-image)))

(defsubst tabbar-normalize-image (image &optional margin nomask)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image.  If optional NOMASK is non-nil, no mask
property is included."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (unless nomask
          (setq plist (plist-put plist :mask '(heuristic t)))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

(defvar tabbar-close-tab-function nil
  "Function to call to close a tabbar tab.  Passed a single argument, the tab
construct to be closed.")

(defvar tabbar-new-tab-function nil
  "Function to call to create a new buffer in tabbar-mode.  Optional single
argument is the MODE for the new buffer.")

;; for buffer tabs, use the usual command to close/kill a buffer
(defun tabbar-buffer-close-tab (tab)
  (let ((buffer (tabbar-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))))

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)

(defvar tabbar-last-tab nil)

(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
         (mouse-button (event-basic-type mouse-event)))
    (if  (eq mouse-button 'mouse-3)
        (progn
          (setq tabbar-last-tab tab)
          (tabbar-context-menu))
      (if (eq action 'close-tab)
          (when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
            (funcall tabbar-close-tab-function tab))
        (when tabbar-select-tab-function
          (funcall tabbar-select-tab-function
                   (tabbar-make-mouse-event type) tab)
          (tabbar-display-update))))))

(defun tabbar-reset ()
  "Reset memoized functions."
  (interactive)
  (tabbar-memoize 'tabbar-ruler-tab-separator-image)
  (tabbar-memoize 'tabbar-make-tab-keymap)
  (tabbar-memoize 'tabbar-ruler-image))
(tabbar-reset)

(defsubst tabbar-drag-p (event)
  "Return non-nil if EVENT is a mouse drag event."
  (memq 'drag (event-modifiers event)))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (cond 
    ((tabbar-click-p event)
      (let ((target (posn-string (event-start event))))
        (tabbar-click-on-tab
          (get-text-property (cdr target) 'tabbar-tab (car target))
          event
          (get-text-property (cdr target) 'tabbar-action (car target)))))
    ((tabbar-drag-p event)
      (let ((start-target (posn-string (event-start event)))
            (end-target (posn-string (event-end event))))
        (tabbar-drag-tab
          (get-text-property (cdr start-target) 'tabbar-tab (car start-target))
          (get-text-property (cdr end-target) 'tabbar-tab (car end-target))
          event)))
  ))

(defun tabbar-drag-tab (dragged-tab dropped-tab event)
  "Handle DRAGGED-TAB dragged-and-dropped onto DROPPED-TAB.
   Include full mouse EVENT from drag-and-drop action."
  (let ((start-tabset (tabbar-tab-tabset dragged-tab)))
    (when (and (eq start-tabset (tabbar-tab-tabset dropped-tab))
           (not (eq dragged-tab dropped-tab)))
      (let* ((tabs (tabbar-tabs start-tabset))
         (drop-tail-length (length (memq dropped-tab tabs)))
         (drag-tail-length (length (memq dragged-tab tabs)))
         (dragdrop-pair (list dragged-tab dropped-tab))
         new-tablist)
    (when (> drag-tail-length drop-tail-length)
      (setq dragdrop-pair (reverse dragdrop-pair)))
    (dolist (thistab (reverse tabs))
      ;; build list of tabs.  When we hit dragged-tab, don't append it.
      ;; When we hit dropped-tab, append dragdrop-pair
      (cond
        ((eq thistab dragged-tab))
        ((eq thistab dropped-tab)
         (setq new-tablist (append dragdrop-pair new-tablist)))
        (t (add-to-list 'new-tablist thistab))
      ))
    (set start-tabset new-tablist)
    ;; (setq tabbar-window-cache nil)  ;; didn't help
    (tabbar-set-template start-tabset nil)
    ;; open the dragged tab
    (funcall tabbar-select-tab-function
             (tabbar-make-mouse-event event) dragged-tab)
    (tabbar-display-update)
    ))))

(defsubst tabbar-line-tab (tab &optional not-last sel)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ( (selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
          (modified-p (buffer-modified-p (tabbar-tab-value tab)))
          (close-button-image (tabbar-find-image 
                               `((:type xpm :data ,(tabbar-ruler-image :type 'close :disabled (not modified-p)
                                                                       :color (if (eq tab sel)
                                                                                  (face-attribute 'default :foreground)
                                                                                "gray10"))))))
          (keymap (tabbar-make-tab-keymap tab))
          (separator-image (if tabbar-ruler-fancy-tab-separator
                               (tabbar-find-image
                                `((:type xpm :data
                                         ,(tabbar-ruler-tab-separator-image
                                           (if (eq tab sel)
                                               'tabbar-selected
                                             'tabbar-unselected)
                                           (if not-last
                                               (if (eq (car not-last) sel)
                                                   'tabbar-selected
                                                 'tabbar-unselected) nil)
                                           nil
                                           (if (and not-last
                                                    (eq (car not-last) sel))
                                               t nil)))))
                             nil))
          (face (if selected-p
                    (if modified-p
                        'tabbar-selected-modified
                      'tabbar-selected)
                  (if modified-p
                      'tabbar-unselected-modified
                    'tabbar-unselected))))
    (concat
     (propertize " " 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (propertize 
      (if tabbar-tab-label-function
          (funcall tabbar-tab-label-function tab)
        tab)
      'tabbar-tab tab
      'local-map keymap
      'help-echo 'tabbar-help-on-tab
      'mouse-face 'tabbar-highlight
      'face face
      'pointer 'hand)
     (propertize (if (and modified-p tabbar-ruler-modified-symbol)
                     (with-temp-buffer
                       (condition-case err
                           (ucs-insert "207A")
                         (error
                          (condition-case err
                              (insert-char 0x207A)
                            (error (insert "*")))))
                       (insert " ")
                       (buffer-substring (point-min) (point-max))) " ")
                 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (if tabbar-ruler-fancy-close-image
         (propertize (with-temp-buffer
                       (condition-case err
                           (ucs-insert "00D7")
                         (error
                          (condition-case err
                              (insert-char #x00D7)
                            (error (insert "x")))))
                       (buffer-string))
                     'display (tabbar-normalize-image close-button-image 0)
                     'face face
                     'pointer 'hand
                     'tabbar-tab tab
                     'local-map keymap
                     'tabbar-action 'close-tab)
       (propertize
        (with-temp-buffer
          (condition-case err
              (ucs-insert "00D7")
            (error (condition-case err
                       (insert-char #x00D7)
                     (error (insert "x")))))
          (insert " ")
          (buffer-string))
        'face face
        'pointer 'hand
        'tabbar-tab tab
        'local-map keymap
        'tabbar-action 'close-tab))
     (if tabbar-ruler-fancy-tab-separator
         (propertize "|"
                     'display (tabbar-normalize-image separator-image))
       tabbar-separator-value))))

(defsubst tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts
         (separator-image (if tabbar-ruler-fancy-tab-separator
                              (tabbar-find-image
                               `((:type xpm :data
                                        ,(tabbar-ruler-tab-separator-image
                                          nil
                                          (if (eq (car tabs) sel)
                                              'tabbar-selected
                                            'tabbar-unselected)))))
                            nil)))
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (if tabbar-ruler-fancy-tab-separator
            (setq elts  (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                  atsel (eq (car tabs) sel)
                  tabs  (cdr tabs))
          (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
                atsel (eq (car tabs) sel)
                tabs  (cdr tabs))))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (if tabbar-ruler-fancy-tab-separator
          (setq elts (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                tabs (cdr tabs))
        (setq elts (cons (tabbar-line-tab (car tabs)) elts)
              tabs (cdr tabs))))
    ;; Cache and return the new tab bar.
    (if tabbar-ruler-fancy-tab-separator
        (tabbar-set-template
         tabset
         (list (tabbar-line-buttons tabset)
               (propertize "|"
                           'display (tabbar-normalize-image separator-image))
               (nreverse elts)
               (propertize "%-"
                           'face (list :background padcolor
                                       :foreground padcolor)
                           'pointer 'arrow)))
      (tabbar-set-template
       tabset
       (list (tabbar-line-buttons tabset)
             (nreverse elts)
             (propertize "%-"
                         'face (list :background padcolor
                                     :foreground padcolor)
                         'pointer 'arrow))))))

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :foreground "DarkOrange3"
     :weight bold))
   "Face used for selected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :foreground "DarkOrange3"
     :weight bold))
   "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

;; Hooks based on yswzing's hooks, but modified for this function state.
;; called each time the modification state of the buffer changed
(defun tabbar-ruler-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; first-change-hook is called BEFORE the change is made
(defun tabbar-ruler-on-buffer-modification ()
  (set-buffer-modified-p t)
  (tabbar-ruler-modification-state-change))
(add-hook 'after-save-hook 'tabbar-ruler-modification-state-change)

(defcustom tabbar-ruler-global-tabbar 't
  "Should tabbar-ruler have a global tabbar?"
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-global-ruler nil
  "Should tabbar-ruler have a global ruler?"
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-popup-menu nil
  "Should tabbar-ruler have a popup menu.  As mouse moves toward top of window, the menu pops up."
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-popup-toolbar nil
  "Should tabbar-ruler have a popup toolbar.  As mouse moves toward top of window, the toolbar pops up."
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-popup-scrollbar nil
  "Should tabbas-ruler have popup scrollbar.  As mouse moves, the scroll-bar pops up.  Otherwise the sroll-bar is turned off."
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-popup-menu-min-y 5 ;
  "Minimum number of pixels from the top before a menu/toolbar pops up."
  :type 'integer
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-popup-menu-min-y-leave 50
  "Minimum number of pixels form the top before a menu/toolbar disappears."
  :type 'integer
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-do-not-switch-on-ruler-when-tabbar-is-on-y 75
  "Minimum number of pixels to switch on ruler when tabbar is on."
  :type 'integer          
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*")
  "Excluded buffers in tabbar."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fight-igore-modes '(info-mode helm-mode package-menu-mode)
  "Exclude these mode when changing between tabbar and ruler."
  :type '(repeat (symbol :tag "Major Mode"))
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fancy-tab-separator nil
  "Separate each tab with a fancy generated image"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fancy-close-image nil
  "Use an image for the close"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-movement-timer-delay 0.1
  "Timer Delay for `tabbar-ruler-movement-timer'"
  :type 'number
  :group 'tabbar-ruler)

(defvar tabbar-ruler-tabbar-off 't)
(defvar tabbar-ruler-ruler-off 't)
(set (make-variable-buffer-local 'tabbar-ruler-toolbar-off) nil)
(set (make-variable-buffer-local 'tabbar-ruler-ruler-off) nil)

(defvar tabbar-ruler-toolbar-off nil)
(defvar tabbar-ruler-menu-off nil)
(add-hook 'find-file-hook
          (lambda()
            (interactive)
            (tabbar-ruler-tabbar-ruler-fight 't)))

(defcustom tabbar-ruler-ruler-display-commands
  '(ac-trigger-commands
    esn-upcase-char-self-insert
    esn-magic-$
    right-char
    left-char
    previous-line
    next-line
    backward-paragraph
    forward-paragraph
    cua-scroll-down
    cua-scroll-up
    cua-paste
    cua-paste-pop
    scroll-up
    scroll-down
    autopair-newline
    autopair-insert-opening
    autopair-skip-close-maybe
    autopair-backspace
    backward-delete-char-untabify
    delete-backward-char
    self-insert-command)
  "Ruler display commands."
  :group 'tabbar-ruler
  :type '(repeat symbol))

(defun tabbar-ruler-tabbar-ruler-fight (&optional initialize)
  "Defines the fighting behavior of the tabbar-ruler ruler and tabbar."
  (condition-case error
      (progn
        (cond
         ((minibufferp)
          nil)
         (tabbar-ruler-keep-tabbar
          (setq tabbar-ruler-keep-tabbar nil)
          nil)
         ((and (save-match-data (string-match "^[*]Org Src " (buffer-name))))
          nil)
         ((member major-mode tabbar-ruler-fight-igore-modes)
          nil)
         ( (eq major-mode 'helm-mode)
           nil)
         ( (eq last-command 'mouse-drag-region)
           (tabbar-ruler-mouse-movement))
         ( (and tabbar-ruler-global-ruler tabbar-ruler-global-tabbar)
           (cond
            ( (memq last-command tabbar-ruler-ruler-display-commands)
              (when tabbar-ruler-popup-scrollbar
                (scroll-bar-mode -1))
              (when tabbar-ruler-ruler-off
                (ruler-mode 1)
                (setq tabbar-ruler-ruler-off nil))
              (unless tabbar-ruler-tabbar-off
                (tabbar-mode -1)
                (setq tabbar-ruler-tabbar-off 't))
              (when tabbar-ruler-popup-menu
                (unless tabbar-ruler-menu-off
                  (unless (eq system-type 'darwin)
		    (menu-bar-mode -1))
                  (setq tabbar-ruler-menu-off 't)))
              (when tabbar-ruler-popup-toolbar
                (unless (eq system-type 'darwin)
                  (unless tabbar-ruler-toolbar-off
                    (tool-bar-mode -1)
                    (setq tabbar-ruler-toolbar-off 't)))))
            ( (save-match-data (string-match "\\(mouse\\|ignore\\|window\\|frame\\)" (format "%s" last-command)))
              (when nil ;; Took this out;  Afterward it works much better...
                (unless tabbar-ruler-ruler-off
                  (ruler-mode -1)
                  (setq tabbar-ruler-ruler-off 't))
                (when tabbar-ruler-tabbar-off
                  (tabbar-mode 1)
                  (setq tabbar-ruler-tabbar-off nil))))
            ( 't
              (when (or initialize (and tabbar-ruler-ruler-off tabbar-ruler-tabbar-off))
                (when tabbar-ruler-popup-scrollbar
                  (scroll-bar-mode -1))
                (when tabbar-ruler-ruler-off
                  (ruler-mode 1)
                  (setq tabbar-ruler-ruler-off nil))
                (unless tabbar-ruler-tabbar-off
                  (tabbar-mode -1)
                  (setq tabbar-ruler-tabbar-off 't))))))
         ( tabbar-ruler-global-ruler
           (when tabbar-ruler-ruler-off
             (ruler-mode 1)
             (setq tabbar-ruler-ruler-off nil)))
         ( tabbar-ruler-global-tabbar
           (when tabbar-ruler-tabbar-off
             (tabbar-mode 1)
             (setq tabbar-ruler-tabbar-off nil)))))
    (error
     (message "Error in post-command-hook for Ruler/Tabbar: %s" (error-message-string error)))))

(add-hook 'post-command-hook 'tabbar-ruler-tabbar-ruler-fight)
(defvar tabbar-ruler-movement-timer nil)
(defvar tabbar-ruler-movement-x nil)
(defvar tabbar-ruler-movement-y nil)

(defun tabbar-ruler-mouse-movement ()
  "Mouse Movement function"
  (interactive)
  (when tabbar-ruler-movement-timer
    (cancel-timer tabbar-ruler-movement-timer))  
  (let* ((y-pos (cddr (mouse-pixel-position)))
         (x-pos (cadr (mouse-pixel-position))))
    (unless y-pos
      (setq y-pos tabbar-ruler-movement-y))
    (unless x-pos
      (setq x-pos tabbar-ruler-movement-x))
    (when (or (not tabbar-ruler-movement-x) (not tabbar-ruler-movement-y)
              (and tabbar-ruler-movement-x tabbar-ruler-movement-y
                   (not
                    (and
                     (= tabbar-ruler-movement-x x-pos)
                     (= tabbar-ruler-movement-y y-pos)))))
      (when (and x-pos y-pos)
        (when tabbar-ruler-popup-scrollbar
          (scroll-bar-mode 1))
        (setq tabbar-ruler-movement-x x-pos)
        (setq tabbar-ruler-movement-y y-pos)
        (unless tabbar-ruler-ruler-off
          (ruler-mode -1)
          (setq tabbar-ruler-ruler-off 't))
        (when tabbar-ruler-tabbar-off
          (tabbar-mode 1)
          (setq tabbar-ruler-tabbar-off nil))
        (if (>= (if (or tabbar-ruler-menu-off tabbar-ruler-toolbar-off)
                    tabbar-ruler-popup-menu-min-y
                  tabbar-ruler-popup-menu-min-y-leave) y-pos)
            (progn
              (when tabbar-ruler-popup-menu
                (when tabbar-ruler-menu-off
                  (unless (eq system-type 'darwin)
		    (menu-bar-mode 1))
                  (setq tabbar-ruler-menu-off nil)))
              (when tabbar-ruler-popup-toolbar
                (unless (eq system-type 'darwin)
                  (when tabbar-ruler-toolbar-off
                    (tool-bar-mode 1)
                    (setq tabbar-ruler-toolbar-off nil)))))
          (when tabbar-ruler-popup-menu
            (unless tabbar-ruler-menu-off
              (unless (eq system-type 'darwin)
		(menu-bar-mode -1))
              (setq tabbar-ruler-menu-off 't)))
          (when tabbar-ruler-popup-toolbar
            (unless (eq system-type 'darwin)
              (unless tabbar-ruler-toolbar-off
                (tool-bar-mode -1)
                (setq tabbar-ruler-toolbar-off 't)))))))
    (setq tabbar-ruler-movement-timer (run-with-idle-timer
                                       tabbar-ruler-movement-timer-delay
                                       nil
                                       'tabbar-ruler-mouse-movement))))
(tabbar-ruler-mouse-movement)


(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(defun last-tabbar-ruler-tabbar-buffer-groups nil)

(defun tabbar-ruler-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (setq last-tabbar-ruler-tabbar-buffer-groups
        (list
         (cond
          ;;          ((or (get-buffer-process (current-buffer))
          ;;               ;; Check if the major mode derives from `comint-mode' or
          ;;               ;; `compilation-mode'.
          ;;               (tabbar-buffer-mode-derived-p
          ;;                major-mode '(comint-mode compilation-mode)))
          ;;           "Process")
          ;;    ((string-match "^ *[*]" (buffer-name))
          ;;     "Common"
          ;;     )
          ((eq major-mode 'dired-mode)
           "Dired")
          ((memq major-mode
                 '(help-mode apropos-mode Info-mode Man-mode))
           "Help")
          ((memq major-mode
                 '(rmail-mode
                   rmail-edit-mode vm-summary-mode vm-mode mail-mode
                   mh-letter-mode mh-show-mode mh-folder-mode
                   gnus-summary-mode message-mode gnus-group-mode
                   gnus-article-mode score-mode gnus-browse-killed-mode))
           "Mail")
          (t
           "Files"
           ))))
  (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups))


(defun tabbar-ruler-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or *, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((member (buffer-name b) tabbar-ruler-excluded-buffers) nil)
                     ;; ((string= "*Messages*" (format "%s" (buffer-name b))))
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ;;((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defvar tabbar-ruler-projectile-tabbar-buffer-group-calc nil
  "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")
(defun tabbar-ruler-projectile-tabbar-buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
  
  (if tabbar-ruler-projectile-tabbar-buffer-group-calc
      (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
    (set (make-local-variable 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
         
         (cond
          ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
          ((condition-case err
               (projectile-project-root)
             (error nil)) (list (projectile-project-name)))
          ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
          ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
          ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
          ((memq major-mode '(dired-mode)) '("Dir"))
          (t '("Main"))))
    (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)))

(defun tabbar-ruler-group-by-projectile-project()
  "Group by projectile project."
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-ruler-projectile-tabbar-buffer-groups))

(defun tabbar-ruler-group-user-buffers-helper ()
   ;; customize tabbar to have only 2 groups: emacs's and user's buffers
   ;; all normal files will be shown in group user's buffers
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs's buffers")
               ((eq major-mode 'dired-mode) "emacs's buffers")
               (t "user's buffers"))))

(defun tabbar-ruler-group-user-buffers ()
   (interactive)
   (setq tabbar-buffer-groups-function 'tabbar-ruler-group-user-buffers-helper))

(defun tabbar-ruler-group-buffer-groups ()
  "Use tabbar's major-mode grouping of buffers."
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

;; default group mode
(tabbar-ruler-group-user-buffers)

;;; Adapted from auto-hide in EmacsWiki

(defvar tabbar-display-functions
  '(tabbar-press-home
    tabbar-backward
    tabbar-forward
    tabbar-backward-tab
    tabbar-forward-tab
    tabbar-backward-group
    tabbar-forward-group
    tabbar-press-scroll-left
    tabbar-press-scroll-right)
  "Tabbar movement functions")

(defvar tabbar-ruler-keep-tabbar nil)

(mapc
 (lambda(x)
   (eval `(defun ,(intern (concat "tabbar-ruler-" (symbol-name x))) (&optional arg)
            ,(concat "Turn on tabbar before running `" (symbol-name x) "'")
            (interactive "p")
            (setq tabbar-ruler-keep-tabbar t)
            (unless tabbar-ruler-ruler-off
              (ruler-mode -1)
              (setq tabbar-ruler-ruler-off 't))
            (when tabbar-ruler-tabbar-off
              (tabbar-mode 1)
              (setq tabbar-ruler-tabbar-off nil))
            (setq current-prefix-arg current-prefix-arg)
            (call-interactively ',x)
            (setq tabbar-ruler-keep-tabbar t)
            (unless tabbar-ruler-ruler-off
              (ruler-mode -1)
              (setq tabbar-ruler-ruler-off 't))
            (when tabbar-ruler-tabbar-off
              (tabbar-mode 1)
              (setq tabbar-ruler-tabbar-off nil)))))
 tabbar-display-functions)

;;;###autoload
(defun tabbar-ruler-up (&optional arg)
  "Tabbar press up key."
  (interactive "p")
  (setq current-prefix-arg current-prefix-arg)
  (call-interactively 'tabbar-ruler-tabbar-press-home))

;;;###autoload
(defun tabbar-ruler-forward (&optional arg)
  "Forward ruler. Takes into consideration if the home-key was pressed.
This is based on the variable `tabbar--buffer-show-groups'"
  (interactive "p")
  (cond
   (tabbar--buffer-show-groups
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-forward-group)
    (tabbar-ruler-tabbar-press-home))
   (t
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-forward-tab))))

;;;###autoload
(defun tabbar-ruler-backward (&optional arg)
  "Backward ruler.  Takes into consideration if the home-key was pressed."
  (interactive "p")
  (cond
   (tabbar--buffer-show-groups
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-backward-group)
    (tabbar-ruler-tabbar-press-home))
   (t
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-backward-tab))))

(when (not (fboundp 'set-temporary-overlay-map))
  ;; Backport this function from newer emacs versions
  (defun set-temporary-overlay-map (map &optional keep-pred)
    "Set a new keymap that will only exist for a short period of time.
The new keymap to use must be given in the MAP variable. When to
remove the keymap depends on user input and KEEP-PRED:

- if KEEP-PRED is nil (the default), the keymap disappears as
  soon as any key is pressed, whether or not the key is in MAP;

- if KEEP-PRED is t, the keymap disappears as soon as a key *not*
 i in MAP is pressed;

- otherwise, KEEP-PRED must be a 0-arguments predicate that will
  decide if the keymap should be removed (if predicate returns
  nil) or kept (otherwise). The predicate will be called after
  each key sequence."
    
    (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
           (overlaysym (make-symbol "t"))
           (alist (list (cons overlaysym map)))
           (clearfun
            `(lambda ()
               (unless ,(cond ((null keep-pred) nil)
                              ((eq t keep-pred)
                               `(eq this-command
                                    (lookup-key ',map
                                                (this-command-keys-vector))))
                              (t `(funcall ',keep-pred)))
                 (remove-hook 'pre-command-hook ',clearfunsym)
                 (setq emulation-mode-map-alists
                       (delq ',alist emulation-mode-map-alists))))))
      (set overlaysym overlaysym)
      (fset clearfunsym clearfun)
      (add-hook 'pre-command-hook clearfunsym)
      
      (push alist emulation-mode-map-alists))))

(defvar tabbar-ruler-move-keymap (make-sparse-keymap)
  "Keymap for tabbar-ruler movement")

(define-key tabbar-ruler-move-keymap [remap previous-line] 'tabbar-ruler-up)
(define-key tabbar-ruler-move-keymap [remap next-line] 'tabbar-ruler-up)
(define-key tabbar-ruler-move-keymap [remap backward-char] 'tabbar-ruler-backward)
(define-key tabbar-ruler-move-keymap [remap forward-char] 'tabbar-ruler-forward)
(define-key tabbar-ruler-move-keymap [remap left-char] 'tabbar-ruler-backward)
(define-key tabbar-ruler-move-keymap [remap right-char] 'tabbar-ruler-forward)
(define-key tabbar-ruler-move-keymap (kbd "SPC") (lambda() (interactive) (message "Exited tabbar-movement")))
(define-key tabbar-ruler-move-keymap (kbd "<return>") (lambda() (interactive) (message "Exited tabbar-movement")))

(defun tabbar-ruler-move-pred ()
  "Determines if emacs should keep the temporary keymap
  `tabbar-ruler-move-keymap' when running `tabbar-ruler-move'."
  (memq this-command '(tabbar-ruler-up tabbar-ruler-backward tabbar-ruler-forward)))

;;;###autoload
(defun tabbar-ruler-move ()
  "Start the movement for the tabbar"
  (interactive)
  (setq tabbar-ruler-keep-tabbar t)
  (unless tabbar-ruler-ruler-off
    (ruler-mode -1)
    (setq tabbar-ruler-ruler-off 't))
  (when tabbar-ruler-tabbar-off
    (tabbar-mode 1)
    (setq tabbar-ruler-tabbar-off nil))
  (message "Use arrow keys to change buffers (or line movement commands).  Exit with space/return or any other key.")
  (set-temporary-overlay-map tabbar-ruler-move-keymap 'tabbar-ruler-move-pred))

;; Hook save and change events to show modified buffers in tabbar
(defun on-saving-buffer ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(defun on-modifying-buffer ()
  (set-buffer-modified-p (buffer-modified-p))
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(defun after-modifying-buffer (begin end length)
  (set-buffer-modified-p (buffer-modified-p))
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(add-hook 'after-save-hook 'on-saving-buffer)
(add-hook 'first-change-hook 'on-modifying-buffer)
(add-hook 'after-change-functions 'after-modifying-buffer)

(defun tabbar-ruler-remove-caches
  "Remove caches for tabbar-ruler."
  ;; Courtesy of Ba Manzi
  ;; https://bitbucket.org/bamanzi/dotemacs-elite/issue/24/tabbar-ruler-not-work-in-emacs-244-keep
  (mapc #'(lambda (frame)
            (modify-frame-parameters frame '((tabbar-cache . nil))))
        (frame-list)))

(add-hook 'desktop-after-read-hook 'tabbar-ruler-remove-caches)

(provide 'tabbar-ruler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabbar-ruler.el ends here
