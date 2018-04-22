;;; ob.el --- Working with Code Blocks in Org        -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'org-macs)
(require 'org-compat)
(require 'ob-eval)
(require 'ob-core)
(require 'ob-comint)
(require 'ob-exp)
(require 'ob-keys)
(require 'ob-table)
(require 'ob-lob)
(require 'ob-ref)
(require 'ob-tangle)

(provide 'ob)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob.el ends here
