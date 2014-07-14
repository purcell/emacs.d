;;; evil.el --- extensible vi layer

;; Authors:
;;      Alessandro Piras <laynor@gmail.com>
;;      Antono Vasiljev <antono.vasiljev at gmail.com>
;;      Barry O'Reilly <gundaetiapo at gmail.com>
;;      Christoph Lange <langec at web.de>
;;      Frank Fischer <frank-fischer at shadow-soft.de>
;;      Frank Terbeck <ft at bewatermyfriend.org>
;;      Gordon Gustafson <gordon3.14@gmail.com>
;;      Jonathan Claggett <jclaggett at lonocloud.com>
;;      José A. Romero L. <escherdragon@gmail.com>
;;      Lars Andersen <expez at expez.com>
;;      Lintaro Ina <tarao.gnn at gmail.com>
;;      Lukasz Wrzosek <wrzoski@gmail.com>
;;      Marian Schubert <maio@netsafe.cz>
;;      Michael Markert <markert.michael at googlemail.com>
;;      Nikolai Weibull <now at bitwi.se>
;;      phaebz <phaebz at gmail.com>
;;      Sanel Zukan <sanelz at gmail.com>
;;      Sarah Brofeldt <sarah at thinkmonster.(none)>
;;      Simon Hafner <hafnersimon at gmail.com>
;;      Stefan Wehr <mail at stefanwehr.de>
;;      Sune Simonsen <sune.simonsen at jayway.com>
;;      Thomas Hisch <thomas at opentech.at>
;;      Trevor Murphy <trevor.m.murphy at gmail.com>
;;      Ulrich Müller <ulm at gentoo.org>
;;      Vegard Øye <vegard_oye at hotmail.com>
;;      Winfred Lu <winfred.lu@gmail.com>
;;      Wolfgang Jenkner <wjenkner at inode.at>
;;      Xiao Hanyu <xiaohanyu1988 at gmail.com>
;;      York Zhao <yzhao at telecor.com>

;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;;      To get in touch, please use the bug tracker or the
;;      mailing list (see below).
;; Created: 2011-03-01
;; Version: 1.0.9
;; Keywords: emulation, vim
;; URL: http://gitorious.org/evil
;;      Repository: git://gitorious.org/evil/evil.git
;;      EmacsWiki: http://www.emacswiki.org/emacs/Evil
;; Bug tracker: https://bitbucket.org/lyro/evil/issues
;;      If you have bug reports, suggestions or patches, please
;;      create an issue at the bug tracker (open for everyone).
;;      Other discussions (tips, extensions) go to the mailing list.
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;      You don't have to subscribe to post; we usually reply
;;      within a few days and CC our replies back to you.
;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.
;;
;; Evil lives in a Git repository. To obtain Evil, do
;;
;;      git clone git://gitorious.org/evil/evil.git
;;
;; Move Evil to ~/.emacs.d/evil (or somewhere else in the `load-path').
;; Then add the following lines to ~/.emacs:
;;
;;      (add-to-list 'load-path "~/.emacs.d/evil")
;;      (require 'evil)
;;      (evil-mode 1)
;;
;; Evil requires undo-tree.el for linear undo and undo branches:
;;
;;      http://www.emacswiki.org/emacs/UndoTree
;;
;; Otherwise, Evil uses regular Emacs undo.
;;
;; Evil requires `goto-last-change' and `goto-last-change-reverse'
;; function for the corresponding motions g; g, as well as the
;; last-change-register `.'. One package providing these functions is
;; goto-chg.el:
;;
;;     http://www.emacswiki.org/emacs/GotoChg
;;
;; Without this package the corresponding motions will raise an error.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-core)
(require 'evil-states)
(require 'evil-repeat)
(require 'evil-macros)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-digraphs)
(require 'evil-types)
(require 'evil-commands)
(require 'evil-maps)
(require 'evil-integration)

(run-hooks 'evil-after-load-hook)

(provide 'evil)

;;; evil.el ends here
