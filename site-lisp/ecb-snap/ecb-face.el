;;; ecb-face.el --- all face-options of ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2001

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-face.el,v 1.23 2005/02/28 11:31:58 berndl Exp $

;;; Commentary:

;; This file contains all options with type 'face and all face-definitions of
;; ECB.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun set-face-parent)
(silentcomp-defun make-face-bold)
(silentcomp-defun make-face)
(silentcomp-defun set-face-foreground)

(defgroup ecb-face-options nil
  "Settings for all faces used in ECB."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-faces nil
  "Definitions of all ECB-faces"
  :group 'ecb-face-options 
  :group 'faces
  :prefix "ecb-")

(defmacro ecb-face-default (&optional height bold-p italic-p
                                      inherit
                                      fg-light-col fg-dark-col
                                      bg-light-col bg-dark-col
                                      fg-rest bg-rest
                                      reverse-video-p)
  "Macro for setting default values for an ECB face.
The parameters are set for the following display-types:
- ((class color) (background light)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                      FG-LIGHT-COL, BG-LIGHT-COL
- ((class color) (background dark)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                     FG-DARK-COL, BG-DARK-COL
- t: HEIGHT, BOLD-P, ITALIC-P, INHERIT, FG-REST, BG-REST, REVERSE-VIDEO."
  `(list (list '((class color) (background light))
               (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not ecb-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not ecb-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-light-col (list :foreground ,fg-light-col))
                       (if ,bg-light-col (list :background ,bg-light-col))))
         (list '((class color) (background dark))
               (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not ecb-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not ecb-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-dark-col (list :foreground ,fg-dark-col))
                       (if ,bg-dark-col (list :background ,bg-dark-col))))
         (list 't (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                          (if ,bold-p (if (not ecb-running-xemacs)
                                          (list :weight 'bold)
                                        (list :bold t)))
                          (if ,italic-p (if (not ecb-running-xemacs)
                                            (list :slant 'italic)
                                          (list :italic t)))
                          (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                          (if ,fg-rest (list :foreground ,fg-rest))
                          (if ,bg-rest (list :foreground ,bg-rest))
                          (if ,reverse-video-p (list :reverse-video t))))))

(defface ecb-default-general-face (ecb-face-default 1.0)
  "*Basic face for all ECB tree-buffers.
It큦 recommended to define here the font-family, the font-size, the basic
color etc.

In GNU Emacs 21.X all faces \(even the face 'ecb-default-highlight-face') used
in the ECB tree-buffers inherit from this face. Therefore the default
attributes like font etc. of a face used in a tree-buffer can be very easily
changed with face 'ecb-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`ecb-directories-general-face', `ecb-sources-general-face',
`ecb-methods-general-face' and `ecb-history-general-face' offer the choice to
use the face 'ecb-default-general-face' so also with XEmacs and GNU Emacs 20.X
the basic face-settings can be easily changed just by customizing the face
'ecb-default-general-face'!"
  :group 'ecb-faces)

(defface ecb-tree-guide-line-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face
                                                    "grey" "gray")
  "*Face for the guide-lines in the tree-buffers."
  :group 'ecb-faces)

(defcustom ecb-tree-guide-line-face 'ecb-tree-guide-line-face
  "*Face for the guide-lines in the tree-buffers."
  :group 'ecb-face-options)

(defface ecb-directories-general-face (ecb-face-default 1.0 nil nil
                                                        'ecb-default-general-face)
  "*Basic face for the ECB directories buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-directories-general-face 'ecb-default-general-face
  "*Basic face for the ECB directories buffer.
This defines the basic face the whole directory buffer should displayed with.
If the face 'ecb-default-general-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-general-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-directories
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-directories-general-face)))

(defface ecb-sources-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB sources buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-sources-general-face 'ecb-default-general-face
  "*Basic face for the ECB sources buffer.
This defines the basic face the whole sources buffer should displayed with. If
the face 'ecb-default-general-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-general-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-sources
  :type 'face
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-sources-general-face)))

(defface ecb-methods-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB methods buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-methods-general-face 'ecb-default-general-face
  "*Basic face for the ECB methods buffer.
This defines the basic face the whole methods buffer should displayed with. If
the face 'ecb-default-general-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-general-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-methods
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-methods-general-face)))

(defface ecb-history-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB history buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-history-general-face 'ecb-default-general-face
  "*Basic face for the ECB history buffer.
This defines the basic face the whole history buffer should displayed with. If
the face 'ecb-default-general-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-general-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-history
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-history-general-face)))

(defface ecb-default-highlight-face (ecb-face-default nil nil nil
                                                      nil ;'ecb-default-general-face
                                                      "yellow" nil
                                                      "cornflower blue" "magenta"
                                                      nil nil t)
  "*Define basic face for highlighting the selected node in a tree-buffer.
In GNU Emacs 21.X all highlighting faces in the ECB tree-buffers inherit from
this face. Therefore the default attributes like font etc. of a face used in a
tree-buffer for highlighting the current tag can be very easily changed with
face 'ecb-default-highlight-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`ecb-directory-face', `ecb-source-face', `ecb-method-face' and
`ecb-history-face' offer the choice to use the face
'ecb-default-highlight-face' so also with XEmacs and GNU Emacs 20.X the basic
face-settings can be easily changed just by customizing the face
'ecb-default-highlight-face'!"
  :group 'ecb-faces)

(defface ecb-directory-face (ecb-face-default nil nil nil
                                              'ecb-default-highlight-face
                                              "yellow" nil
                                              "cornflower blue" "magenta"
                                              nil nil t)
  "*Define face used for highlighting current dir in directories buffer."
  :group 'ecb-faces)

(defcustom ecb-directory-face 'ecb-default-highlight-face
  "*Face used for highlighting current directory in the directories-buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-directories
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-directory-face)))

(defface ecb-source-face (ecb-face-default nil nil nil
                                           'ecb-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current source in the sources buffer."
  :group 'ecb-faces)

(defcustom ecb-source-face 'ecb-default-highlight-face
  "*Face used for highlighting current source in the sources buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-sources
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-sources-face)))

(defface ecb-method-face (ecb-face-default nil nil nil
                                           'ecb-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current tag in the methods buffer."
  :group 'ecb-faces)

(defcustom ecb-method-face 'ecb-default-highlight-face
  "*Face used for highlighting current tag in the methods buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-methods
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-method-face)))

(defface ecb-method-non-semantic-face (ecb-face-default nil nil nil
                                                        'ecb-methods-general-face
                                                        "brown" "brown")
  "*Define face used for displaying tags of non-semantic-sources."
  :group 'ecb-faces)

(defcustom ecb-method-non-semantic-face 'speedbar-tag-face
  "*Face used for for displaying tags of non-semantic-sources.
Default is the face used by speedbar for tags.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-methods
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-method-face)))

(defface ecb-history-face (ecb-face-default nil nil nil
                                            'ecb-default-highlight-face
                                            "yellow" nil
                                            "cornflower blue" "magenta"
                                            nil nil t)
  "*Define face used for highlighting current entry in the history buffer."
  :group 'ecb-faces)

(defcustom ecb-history-face 'ecb-default-highlight-face
  "*Face used for highlighting current entry in the history buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-history
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-history-face)))

(defface ecb-analyse-face (ecb-face-default nil nil nil
                                            'ecb-default-highlight-face
                                            "yellow" nil
                                            "cornflower blue" "magenta"
                                            nil nil t)
  "*Define face used for highlighting current entry in the analyse buffer."
  :group 'ecb-faces)

(defcustom ecb-analyse-face 'ecb-default-highlight-face
  "*Face used for highlighting current entry in the analyse buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-analyse
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-analyse-face)))

(defface ecb-analyse-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB analyse buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-analyse-general-face 'ecb-default-general-face
  "*Basic face for the ECB analyse buffer.
This defines the basic face the whole history buffer should displayed with. If
the face 'ecb-default-general-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-general-face'.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-analyse
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-analyse-general-face)))

(defface ecb-analyse-bucket-element-face (ecb-face-default nil nil nil
                                                           'ecb-default-general-face
                                                           "brown")
  "*Face used for displaying elements of buckets in the ECB-analyse-buffer.

In GNU Emacs 21.X this face inherits from the face 'ecb-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the ECB-buffers should be displayed with the same basic
attributes set by 'ecb-default-general-face' this set of basic attributes have
to be set in 'ecb-analyse-bucket-node-face' too!"
  :group 'ecb-faces)

(defcustom ecb-analyse-bucket-element-face 'ecb-analyse-bucket-element-face
  "*Basic face for displaying elements of buckets in the ECB-analyse-buffer.
This defines the basic face for the elements of category-buckets like Context,
Prefix, Completions etc. in the ECB-analyse-buffer.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-analyse
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-analyse-bucket-element-face)))

(defface ecb-analyse-bucket-node-face (ecb-face-default nil t nil
                                                           'ecb-default-general-face)
  "*Face used for displaying a bucket-node in the ECB-analyse-buffer.

In GNU Emacs 21.X this face inherits from the face 'ecb-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the ECB-buffers should be displayed with the same basic
attributes set by 'ecb-default-general-face' this set of basic attributes have
to be set in 'ecb-analyse-bucket-node-face' too!"
  :group 'ecb-faces)

(defcustom ecb-analyse-bucket-node-face 'ecb-analyse-bucket-node-face
  "*Basic face for displaying a bucket-node in the ECB-analyse-buffer.
This defines the basic face for the bucket-nodes like Context, Prefix,
Completions etc. in the ECB-analyse-buffer.

Changes take first effect after finishing and reactivating ECB!"
  :group 'ecb-face-options
  :group 'ecb-analyse
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-analyse-bucket-node-face)))

(defface ecb-tag-header-face (ecb-face-default nil nil nil nil nil nil
                                               "SeaGreen1" "SeaGreen1"
                                               nil nil t)
  "*Define face used for highlighting the tag header.
The tag header is the first line of the tag which is highlighted after
jumping to it by clicking onto a node in the methods buffer."
  :group 'ecb-faces)
  
(defcustom ecb-tag-header-face 'ecb-tag-header-face
  "*Face used for highlighting the tag header.
The tag header is the first line of the tag which is highlighted after
jumping to it by clicking onto a node in the methods buffer."
  :group 'ecb-face-options
  :group 'ecb-methods
  :type 'face)

(defface ecb-source-in-directories-buffer-face (ecb-face-default nil nil nil
                                                                 'ecb-default-general-face
                                                                 "medium blue"
                                                                 "LightBlue1"
                                                                 nil nil
                                                                 nil "gray")
  "*Define a face for displaying sources in the directories buffer."
  :group 'ecb-faces)
 
(defcustom ecb-source-in-directories-buffer-face
  'ecb-source-in-directories-buffer-face
  "*Face for source files in the directories buffer."
  :group 'ecb-directories
  :group 'ecb-face-options
  :type 'face)

(defface ecb-source-read-only-face (ecb-face-default nil nil t)
  "*Define a face for displaying read-only sources."
  :group 'ecb-faces)
 
(defcustom ecb-source-read-only-face
  'ecb-source-read-only-face
  "*Face for read-only sources."
  :group 'ecb-sources
  :group 'ecb-directories
  :group 'ecb-face-options
  :type 'face)

(defface ecb-directory-not-accessible-face (ecb-face-default nil nil nil
                                                             'ecb-default-general-face
                                                             "gray60"
                                                             "gray60"
                                                             nil nil
                                                             nil "gray60")
  "*Define a face for displaying not accessible dirs in the directories buffer."
  :group 'ecb-faces)
 
(defcustom ecb-directory-not-accessible-face
  'ecb-directory-not-accessible-face
  "*Face for not accessible dirs in the directories buffer."
  :group 'ecb-directories
  :group 'ecb-face-options
  :type 'face)

(defface ecb-type-tag-class-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-interface-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-struct-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-typedef-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-union-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-enum-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-type-tag-group-face (ecb-face-default nil t nil nil
                                                   (if ecb-running-xemacs
                                                       "dimgray"
                                                     "dim gray")
                                                   (if ecb-running-xemacs
                                                       "dimgray"
                                                     "dim gray"))
  "*Define face used with option `ecb-type-tag-display'."
  :group 'ecb-faces)

(defface ecb-bucket-node-face (ecb-face-default nil t nil
                                                'ecb-default-general-face)
  "*Face used for displaying bucket-nodes in the ECB-buffers.
See also `ecb-bucket-node-display'.

In GNU Emacs 21.X this face inherits from the face 'ecb-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the ECB-buffers should be displayed with the same basic
attributes set by 'ecb-default-general-face' this set of basic attributes have
to be set in 'ecb-bucket-node-face' too!"
  :group 'ecb-faces)

;; - mode-line faces-------------------------------------------

;; For XEmacs a face in the modeline should really inhertit from the face
;; 'modeline!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Currently with XEmacs 21.4.X
;; set-face-parent MUST be before defface - therefore we have to use make-face
;; first and then adding the values to this face we would have also added
;; by`defface. The defface is here only used to make this face customizable!
;; Maybe later XEmacs-versions support the parent-keyword with defface then we
;; can change back this ugly hack.
(when ecb-running-xemacs
  (make-face 'ecb-mode-line-win-nr-face)
  (set-face-parent 'ecb-mode-line-win-nr-face 'modeline nil '(default))
  (make-face-bold 'ecb-mode-line-win-nr-face))
(defface ecb-mode-line-win-nr-face (ecb-face-default nil t)
  "*Define face for the window-number in the mode-line.
See `ecb-mode-line-display-window-number'."
  :group 'ecb-faces)

(defcustom ecb-mode-line-win-nr-face 'ecb-mode-line-win-nr-face
  "*Face used for the window-number in the mode-line.
See `ecb-mode-line-display-window-number'. For XEmacs the face should inherit
from the face 'modeline \(see `set-face-parent')!"
  :group 'ecb-mode-line
  :group 'ecb-face-options
  :type 'face)

(when ecb-running-xemacs
  (make-face 'ecb-mode-line-prefix-face)
  (set-face-parent 'ecb-mode-line-prefix-face 'modeline nil '(default))
  (set-face-foreground 'ecb-mode-line-prefix-face "forestgreen"))
;;                        nil '(default color win)))
(defface ecb-mode-line-prefix-face (ecb-face-default nil nil nil nil
                                                     "forestgreen"
                                                     "forestgreen")
  "*Define face for the prefix in the mode-line.
See `ecb-mode-line-prefixes'."
  :group 'ecb-faces)

(defcustom ecb-mode-line-prefix-face 'ecb-mode-line-prefix-face
  "*Face used for the prefix in the mode-line.
See `ecb-mode-line-prefixes'. For XEmacs the face should inherit from the face
'modeline \(see `set-face-parent')!"
  :group 'ecb-mode-line
  :group 'ecb-face-options
  :type 'face)

(when ecb-running-xemacs
  (make-face 'ecb-mode-line-data-face)
  (set-face-parent 'ecb-mode-line-data-face 'modeline nil '(default)))
(defface ecb-mode-line-data-face (ecb-face-default)
  "*Define face for the data in the mode-line.
See `ecb-mode-line-data'."
  :group 'ecb-faces)

(defcustom ecb-mode-line-data-face 'ecb-mode-line-data-face
  "*Face used for the data in the mode-line.
See `ecb-mode-line-data'. For XEmacs the face should inherit from the face
'modeline \(see `set-face-parent')!"
  :group 'ecb-mode-line
  :group 'ecb-face-options
  :type 'face)



(silentcomp-provide 'ecb-face)

;;; ecb-face.el ends here
