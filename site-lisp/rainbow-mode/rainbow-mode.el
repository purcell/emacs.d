;;; rainbow-mode.el --- Colorize color names in buffers

;; Copyright (C) 2010-2012 Free Software Foundation, Inc

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: faces
;; Version: 0.9

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'regexp-opt)
(require 'faces)
(require 'color)

(unless (require 'xterm-color nil t)
  (require 'ansi-color))

(defgroup rainbow nil
  "Show color strings with a background color."
  :tag "Rainbow"
  :group 'help)

;; Hexadecimal colors
(defvar rainbow-hexadecimal-colors-font-lock-keywords
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (1 (rainbow-colorize-itself 1)))
    ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb]:[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb][Ii]:[0-9.]+/[0-9.]+/[0-9.]+"
     (0 (rainbow-colorize-itself)))
    ("\\(?:[Cc][Ii][Ee]\\(?:[Xx][Yy][Zz]\\|[Uu][Vv][Yy]\\|[Xx][Yy][Yy]\\|[Ll][Aa][Bb]\\|[Ll][Uu][Vv]\\)\\|[Tt][Ee][Kk][Hh][Vv][Cc]\\):[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?"
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for hexadecimal colors.")

;; rgb() colors
(defvar rainbow-html-rgb-colors-font-lock-keywords
  '(("rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
     (0 (rainbow-colorize-rgb)))
    ("rgba(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
     (0 (rainbow-colorize-rgb)))
    ("hsl(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*)"
     (0 (rainbow-colorize-hsl)))
    ("hsla(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
     (0 (rainbow-colorize-hsl))))
  "Font-lock keywords to add for RGB colors.")

;; HTML colors name
(defvar rainbow-html-colors-font-lock-keywords nil
  "Font-lock keywords to add for HTML colors.")
(make-variable-buffer-local 'rainbow-html-colors-font-lock-keywords)

(defcustom rainbow-html-colors-alist
  '(("AliceBlue" . "#F0F8FF")
    ("AntiqueWhite" . "#FAEBD7")
    ("Aqua" . "#00FFFF")
    ("Aquamarine" . "#7FFFD4")
    ("Azure" . "#F0FFFF")
    ("Beige" . "#F5F5DC")
    ("Bisque" . "#FFE4C4")
    ("Black" . "#000000")
    ("BlanchedAlmond" . "#FFEBCD")
    ("Blue" . "#0000FF")
    ("BlueViolet" . "#8A2BE2")
    ("Brown" . "#A52A2A")
    ("BurlyWood" . "#DEB887")
    ("CadetBlue" . "#5F9EA0")
    ("Chartreuse" . "#7FFF00")
    ("Chocolate" . "#D2691E")
    ("Coral" . "#FF7F50")
    ("CornflowerBlue" . "#6495ED")
    ("Cornsilk" . "#FFF8DC")
    ("Crimson" . "#DC143C")
    ("Cyan" . "#00FFFF")
    ("DarkBlue" . "#00008B")
    ("DarkCyan" . "#008B8B")
    ("DarkGoldenRod" . "#B8860B")
    ("DarkGray" . "#A9A9A9")
    ("DarkGrey" . "#A9A9A9")
    ("DarkGreen" . "#006400")
    ("DarkKhaki" . "#BDB76B")
    ("DarkMagenta" . "#8B008B")
    ("DarkOliveGreen" . "#556B2F")
    ("Darkorange" . "#FF8C00")
    ("DarkOrchid" . "#9932CC")
    ("DarkRed" . "#8B0000")
    ("DarkSalmon" . "#E9967A")
    ("DarkSeaGreen" . "#8FBC8F")
    ("DarkSlateBlue" . "#483D8B")
    ("DarkSlateGray" . "#2F4F4F")
    ("DarkSlateGrey" . "#2F4F4F")
    ("DarkTurquoise" . "#00CED1")
    ("DarkViolet" . "#9400D3")
    ("DeepPink" . "#FF1493")
    ("DeepSkyBlue" . "#00BFFF")
    ("DimGray" . "#696969")
    ("DimGrey" . "#696969")
    ("DodgerBlue" . "#1E90FF")
    ("FireBrick" . "#B22222")
    ("FloralWhite" . "#FFFAF0")
    ("ForestGreen" . "#228B22")
    ("Fuchsia" . "#FF00FF")
    ("Gainsboro" . "#DCDCDC")
    ("GhostWhite" . "#F8F8FF")
    ("Gold" . "#FFD700")
    ("GoldenRod" . "#DAA520")
    ("Gray" . "#808080")
    ("Grey" . "#808080")
    ("Green" . "#008000")
    ("GreenYellow" . "#ADFF2F")
    ("HoneyDew" . "#F0FFF0")
    ("HotPink" . "#FF69B4")
    ("IndianRed" . "#CD5C5C")
    ("Indigo" . "#4B0082")
    ("Ivory" . "#FFFFF0")
    ("Khaki" . "#F0E68C")
    ("Lavender" . "#E6E6FA")
    ("LavenderBlush" . "#FFF0F5")
    ("LawnGreen" . "#7CFC00")
    ("LemonChiffon" . "#FFFACD")
    ("LightBlue" . "#ADD8E6")
    ("LightCoral" . "#F08080")
    ("LightCyan" . "#E0FFFF")
    ("LightGoldenRodYellow" . "#FAFAD2")
    ("LightGray" . "#D3D3D3")
    ("LightGrey" . "#D3D3D3")
    ("LightGreen" . "#90EE90")
    ("LightPink" . "#FFB6C1")
    ("LightSalmon" . "#FFA07A")
    ("LightSeaGreen" . "#20B2AA")
    ("LightSkyBlue" . "#87CEFA")
    ("LightSlateGray" . "#778899")
    ("LightSlateGrey" . "#778899")
    ("LightSteelBlue" . "#B0C4DE")
    ("LightYellow" . "#FFFFE0")
    ("Lime" . "#00FF00")
    ("LimeGreen" . "#32CD32")
    ("Linen" . "#FAF0E6")
    ("Magenta" . "#FF00FF")
    ("Maroon" . "#800000")
    ("MediumAquaMarine" . "#66CDAA")
    ("MediumBlue" . "#0000CD")
    ("MediumOrchid" . "#BA55D3")
    ("MediumPurple" . "#9370D8")
    ("MediumSeaGreen" . "#3CB371")
    ("MediumSlateBlue" . "#7B68EE")
    ("MediumSpringGreen" . "#00FA9A")
    ("MediumTurquoise" . "#48D1CC")
    ("MediumVioletRed" . "#C71585")
    ("MidnightBlue" . "#191970")
    ("MintCream" . "#F5FFFA")
    ("MistyRose" . "#FFE4E1")
    ("Moccasin" . "#FFE4B5")
    ("NavajoWhite" . "#FFDEAD")
    ("Navy" . "#000080")
    ("OldLace" . "#FDF5E6")
    ("Olive" . "#808000")
    ("OliveDrab" . "#6B8E23")
    ("Orange" . "#FFA500")
    ("OrangeRed" . "#FF4500")
    ("Orchid" . "#DA70D6")
    ("PaleGoldenRod" . "#EEE8AA")
    ("PaleGreen" . "#98FB98")
    ("PaleTurquoise" . "#AFEEEE")
    ("PaleVioletRed" . "#D87093")
    ("PapayaWhip" . "#FFEFD5")
    ("PeachPuff" . "#FFDAB9")
    ("Peru" . "#CD853F")
    ("Pink" . "#FFC0CB")
    ("Plum" . "#DDA0DD")
    ("PowderBlue" . "#B0E0E6")
    ("Purple" . "#800080")
    ("Red" . "#FF0000")
    ("RosyBrown" . "#BC8F8F")
    ("RoyalBlue" . "#4169E1")
    ("SaddleBrown" . "#8B4513")
    ("Salmon" . "#FA8072")
    ("SandyBrown" . "#F4A460")
    ("SeaGreen" . "#2E8B57")
    ("SeaShell" . "#FFF5EE")
    ("Sienna" . "#A0522D")
    ("Silver" . "#C0C0C0")
    ("SkyBlue" . "#87CEEB")
    ("SlateBlue" . "#6A5ACD")
    ("SlateGray" . "#708090")
    ("SlateGrey" . "#708090")
    ("Snow" . "#FFFAFA")
    ("SpringGreen" . "#00FF7F")
    ("SteelBlue" . "#4682B4")
    ("Tan" . "#D2B48C")
    ("Teal" . "#008080")
    ("Thistle" . "#D8BFD8")
    ("Tomato" . "#FF6347")
    ("Turquoise" . "#40E0D0")
    ("Violet" . "#EE82EE")
    ("Wheat" . "#F5DEB3")
    ("White" . "#FFFFFF")
    ("WhiteSmoke" . "#F5F5F5")
    ("Yellow" . "#FFFF00")
    ("YellowGreen" . "#9ACD32"))
  "Alist of HTML colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :group 'rainbow)

(defcustom rainbow-html-colors-major-mode-list
  '(html-mode css-mode php-mode nxml-mode xml-mode)
  "List of major mode where HTML colors are enabled when
`rainbow-html-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-html-colors 'auto
  "When to enable HTML colors.
If set to t, the HTML colors will be enabled.  If set to nil, the
HTML colors will not be enabled.  If set to auto, the HTML colors
will be enabled if a major mode has been detected from the
`rainbow-html-colors-major-mode-list'."
  :group 'rainbow)

;; X colors
(defvar rainbow-x-colors-font-lock-keywords
  `((,(regexp-opt (x-defined-colors) 'words)
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for X colors.")

(defcustom rainbow-x-colors-major-mode-list
  '(emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode)
  "List of major mode where X colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-x-colors 'auto
  "When to enable X colors.
If set to t, the X colors will be enabled.  If set to nil, the
X colors will not be enabled.  If set to auto, the X colors
will be enabled if a major mode has been detected from the
`rainbow-x-colors-major-mode-list'."
  :group 'rainbow)

;; LaTeX colors
(defvar rainbow-latex-rgb-colors-font-lock-keywords
  '(("{rgb}{\\([0-9.]+\\),\s*\\([0-9.]+\\),\s*\\([0-9.]+\\)}"
     (0 (rainbow-colorize-rgb-float)))
    ("{RGB}{\\([0-9]\\{1,3\\}\\),\s*\\([0-9]\\{1,3\\}\\),\s*\\([0-9]\\{1,3\\}\\)}"
     (0 (rainbow-colorize-rgb)))
    ("{HTML}{\\([0-9A-Fa-f]\\{6\\}\\)}"
     (0 (rainbow-colorize-hexadecimal-without-sharp))))
  "Font-lock keywords to add for LaTeX colors.")

(defcustom rainbow-latex-colors-major-mode-list
  '(latex-mode)
  "List of major mode where LaTeX colors are enabled when
`rainbow-x-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-latex-colors 'auto
  "When to enable LaTeX colors.
If set to t, the LaTeX colors will be enabled. If set to nil, the
LaTeX colors will not be enabled.  If set to auto, the LaTeX colors
will be enabled if a major mode has been detected from the
`rainbow-latex-colors-major-mode-list'."
  :group 'rainbow)

;; Shell colors
(defvar rainbow-ansi-colors-font-lock-keywords
  '(("\\(\\\\[eE]\\|\\\\033\\|\\\\x1[bB]\\|\033\\)\\[\\([0-9;]*m\\)"
     (0 (rainbow-colorize-ansi))))
  "Font-lock keywords to add for ANSI colors.")

(defcustom rainbow-ansi-colors-major-mode-list
  '(sh-mode c-mode c++-mode)
  "List of major mode where ANSI colors are enabled when
`rainbow-ansi-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-ansi-colors 'auto
  "When to enable ANSI colors.
If set to t, the ANSI colors will be enabled. If set to nil, the
ANSI colors will not be enabled.  If set to auto, the ANSI colors
will be enabled if a major mode has been detected from the
`rainbow-ansi-colors-major-mode-list'."
  :group 'rainbow)

;; R colors

;; R colors name
(defvar rainbow-r-colors-font-lock-keywords nil
  "Font-lock keywords to add for R colors.")
(make-variable-buffer-local 'rainbow-r-colors-font-lock-keywords)

;; use the following code to generate the list in R
;; output_colors <- function(colors) {for(color in colors) {col <- col2rgb(color); cat(sprintf("(\"%s\" . \"#%02X%02X%02X\")\n",color,col[1],col[2],col[3]));}}
;; output_colors(colors())
(defcustom rainbow-r-colors-alist
  '(("white" . "#FFFFFF")
    ("aliceblue" . "#F0F8FF")
    ("antiquewhite" . "#FAEBD7")
    ("antiquewhite1" . "#FFEFDB")
    ("antiquewhite2" . "#EEDFCC")
    ("antiquewhite3" . "#CDC0B0")
    ("antiquewhite4" . "#8B8378")
    ("aquamarine" . "#7FFFD4")
    ("aquamarine1" . "#7FFFD4")
    ("aquamarine2" . "#76EEC6")
    ("aquamarine3" . "#66CDAA")
    ("aquamarine4" . "#458B74")
    ("azure" . "#F0FFFF")
    ("azure1" . "#F0FFFF")
    ("azure2" . "#E0EEEE")
    ("azure3" . "#C1CDCD")
    ("azure4" . "#838B8B")
    ("beige" . "#F5F5DC")
    ("bisque" . "#FFE4C4")
    ("bisque1" . "#FFE4C4")
    ("bisque2" . "#EED5B7")
    ("bisque3" . "#CDB79E")
    ("bisque4" . "#8B7D6B")
    ("black" . "#000000")
    ("blanchedalmond" . "#FFEBCD")
    ("blue" . "#0000FF")
    ("blue1" . "#0000FF")
    ("blue2" . "#0000EE")
    ("blue3" . "#0000CD")
    ("blue4" . "#00008B")
    ("blueviolet" . "#8A2BE2")
    ("brown" . "#A52A2A")
    ("brown1" . "#FF4040")
    ("brown2" . "#EE3B3B")
    ("brown3" . "#CD3333")
    ("brown4" . "#8B2323")
    ("burlywood" . "#DEB887")
    ("burlywood1" . "#FFD39B")
    ("burlywood2" . "#EEC591")
    ("burlywood3" . "#CDAA7D")
    ("burlywood4" . "#8B7355")
    ("cadetblue" . "#5F9EA0")
    ("cadetblue1" . "#98F5FF")
    ("cadetblue2" . "#8EE5EE")
    ("cadetblue3" . "#7AC5CD")
    ("cadetblue4" . "#53868B")
    ("chartreuse" . "#7FFF00")
    ("chartreuse1" . "#7FFF00")
    ("chartreuse2" . "#76EE00")
    ("chartreuse3" . "#66CD00")
    ("chartreuse4" . "#458B00")
    ("chocolate" . "#D2691E")
    ("chocolate1" . "#FF7F24")
    ("chocolate2" . "#EE7621")
    ("chocolate3" . "#CD661D")
    ("chocolate4" . "#8B4513")
    ("coral" . "#FF7F50")
    ("coral1" . "#FF7256")
    ("coral2" . "#EE6A50")
    ("coral3" . "#CD5B45")
    ("coral4" . "#8B3E2F")
    ("cornflowerblue" . "#6495ED")
    ("cornsilk" . "#FFF8DC")
    ("cornsilk1" . "#FFF8DC")
    ("cornsilk2" . "#EEE8CD")
    ("cornsilk3" . "#CDC8B1")
    ("cornsilk4" . "#8B8878")
    ("cyan" . "#00FFFF")
    ("cyan1" . "#00FFFF")
    ("cyan2" . "#00EEEE")
    ("cyan3" . "#00CDCD")
    ("cyan4" . "#008B8B")
    ("darkblue" . "#00008B")
    ("darkcyan" . "#008B8B")
    ("darkgoldenrod" . "#B8860B")
    ("darkgoldenrod1" . "#FFB90F")
    ("darkgoldenrod2" . "#EEAD0E")
    ("darkgoldenrod3" . "#CD950C")
    ("darkgoldenrod4" . "#8B6508")
    ("darkgray" . "#A9A9A9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#A9A9A9")
    ("darkkhaki" . "#BDB76B")
    ("darkmagenta" . "#8B008B")
    ("darkolivegreen" . "#556B2F")
    ("darkolivegreen1" . "#CAFF70")
    ("darkolivegreen2" . "#BCEE68")
    ("darkolivegreen3" . "#A2CD5A")
    ("darkolivegreen4" . "#6E8B3D")
    ("darkorange" . "#FF8C00")
    ("darkorange1" . "#FF7F00")
    ("darkorange2" . "#EE7600")
    ("darkorange3" . "#CD6600")
    ("darkorange4" . "#8B4500")
    ("darkorchid" . "#9932CC")
    ("darkorchid1" . "#BF3EFF")
    ("darkorchid2" . "#B23AEE")
    ("darkorchid3" . "#9A32CD")
    ("darkorchid4" . "#68228B")
    ("darkred" . "#8B0000")
    ("darksalmon" . "#E9967A")
    ("darkseagreen" . "#8FBC8F")
    ("darkseagreen1" . "#C1FFC1")
    ("darkseagreen2" . "#B4EEB4")
    ("darkseagreen3" . "#9BCD9B")
    ("darkseagreen4" . "#698B69")
    ("darkslateblue" . "#483D8B")
    ("darkslategray" . "#2F4F4F")
    ("darkslategray1" . "#97FFFF")
    ("darkslategray2" . "#8DEEEE")
    ("darkslategray3" . "#79CDCD")
    ("darkslategray4" . "#528B8B")
    ("darkslategrey" . "#2F4F4F")
    ("darkturquoise" . "#00CED1")
    ("darkviolet" . "#9400D3")
    ("deeppink" . "#FF1493")
    ("deeppink1" . "#FF1493")
    ("deeppink2" . "#EE1289")
    ("deeppink3" . "#CD1076")
    ("deeppink4" . "#8B0A50")
    ("deepskyblue" . "#00BFFF")
    ("deepskyblue1" . "#00BFFF")
    ("deepskyblue2" . "#00B2EE")
    ("deepskyblue3" . "#009ACD")
    ("deepskyblue4" . "#00688B")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1E90FF")
    ("dodgerblue1" . "#1E90FF")
    ("dodgerblue2" . "#1C86EE")
    ("dodgerblue3" . "#1874CD")
    ("dodgerblue4" . "#104E8B")
    ("firebrick" . "#B22222")
    ("firebrick1" . "#FF3030")
    ("firebrick2" . "#EE2C2C")
    ("firebrick3" . "#CD2626")
    ("firebrick4" . "#8B1A1A")
    ("floralwhite" . "#FFFAF0")
    ("forestgreen" . "#228B22")
    ("gainsboro" . "#DCDCDC")
    ("ghostwhite" . "#F8F8FF")
    ("gold" . "#FFD700")
    ("gold1" . "#FFD700")
    ("gold2" . "#EEC900")
    ("gold3" . "#CDAD00")
    ("gold4" . "#8B7500")
    ("goldenrod" . "#DAA520")
    ("goldenrod1" . "#FFC125")
    ("goldenrod2" . "#EEB422")
    ("goldenrod3" . "#CD9B1D")
    ("goldenrod4" . "#8B6914")
    ("gray" . "#BEBEBE")
    ("gray0" . "#000000")
    ("gray1" . "#030303")
    ("gray2" . "#050505")
    ("gray3" . "#080808")
    ("gray4" . "#0A0A0A")
    ("gray5" . "#0D0D0D")
    ("gray6" . "#0F0F0F")
    ("gray7" . "#121212")
    ("gray8" . "#141414")
    ("gray9" . "#171717")
    ("gray10" . "#1A1A1A")
    ("gray11" . "#1C1C1C")
    ("gray12" . "#1F1F1F")
    ("gray13" . "#212121")
    ("gray14" . "#242424")
    ("gray15" . "#262626")
    ("gray16" . "#292929")
    ("gray17" . "#2B2B2B")
    ("gray18" . "#2E2E2E")
    ("gray19" . "#303030")
    ("gray20" . "#333333")
    ("gray21" . "#363636")
    ("gray22" . "#383838")
    ("gray23" . "#3B3B3B")
    ("gray24" . "#3D3D3D")
    ("gray25" . "#404040")
    ("gray26" . "#424242")
    ("gray27" . "#454545")
    ("gray28" . "#474747")
    ("gray29" . "#4A4A4A")
    ("gray30" . "#4D4D4D")
    ("gray31" . "#4F4F4F")
    ("gray32" . "#525252")
    ("gray33" . "#545454")
    ("gray34" . "#575757")
    ("gray35" . "#595959")
    ("gray36" . "#5C5C5C")
    ("gray37" . "#5E5E5E")
    ("gray38" . "#616161")
    ("gray39" . "#636363")
    ("gray40" . "#666666")
    ("gray41" . "#696969")
    ("gray42" . "#6B6B6B")
    ("gray43" . "#6E6E6E")
    ("gray44" . "#707070")
    ("gray45" . "#737373")
    ("gray46" . "#757575")
    ("gray47" . "#787878")
    ("gray48" . "#7A7A7A")
    ("gray49" . "#7D7D7D")
    ("gray50" . "#7F7F7F")
    ("gray51" . "#828282")
    ("gray52" . "#858585")
    ("gray53" . "#878787")
    ("gray54" . "#8A8A8A")
    ("gray55" . "#8C8C8C")
    ("gray56" . "#8F8F8F")
    ("gray57" . "#919191")
    ("gray58" . "#949494")
    ("gray59" . "#969696")
    ("gray60" . "#999999")
    ("gray61" . "#9C9C9C")
    ("gray62" . "#9E9E9E")
    ("gray63" . "#A1A1A1")
    ("gray64" . "#A3A3A3")
    ("gray65" . "#A6A6A6")
    ("gray66" . "#A8A8A8")
    ("gray67" . "#ABABAB")
    ("gray68" . "#ADADAD")
    ("gray69" . "#B0B0B0")
    ("gray70" . "#B3B3B3")
    ("gray71" . "#B5B5B5")
    ("gray72" . "#B8B8B8")
    ("gray73" . "#BABABA")
    ("gray74" . "#BDBDBD")
    ("gray75" . "#BFBFBF")
    ("gray76" . "#C2C2C2")
    ("gray77" . "#C4C4C4")
    ("gray78" . "#C7C7C7")
    ("gray79" . "#C9C9C9")
    ("gray80" . "#CCCCCC")
    ("gray81" . "#CFCFCF")
    ("gray82" . "#D1D1D1")
    ("gray83" . "#D4D4D4")
    ("gray84" . "#D6D6D6")
    ("gray85" . "#D9D9D9")
    ("gray86" . "#DBDBDB")
    ("gray87" . "#DEDEDE")
    ("gray88" . "#E0E0E0")
    ("gray89" . "#E3E3E3")
    ("gray90" . "#E5E5E5")
    ("gray91" . "#E8E8E8")
    ("gray92" . "#EBEBEB")
    ("gray93" . "#EDEDED")
    ("gray94" . "#F0F0F0")
    ("gray95" . "#F2F2F2")
    ("gray96" . "#F5F5F5")
    ("gray97" . "#F7F7F7")
    ("gray98" . "#FAFAFA")
    ("gray99" . "#FCFCFC")
    ("gray100" . "#FFFFFF")
    ("green" . "#00FF00")
    ("green1" . "#00FF00")
    ("green2" . "#00EE00")
    ("green3" . "#00CD00")
    ("green4" . "#008B00")
    ("greenyellow" . "#ADFF2F")
    ("grey" . "#BEBEBE")
    ("grey0" . "#000000")
    ("grey1" . "#030303")
    ("grey2" . "#050505")
    ("grey3" . "#080808")
    ("grey4" . "#0A0A0A")
    ("grey5" . "#0D0D0D")
    ("grey6" . "#0F0F0F")
    ("grey7" . "#121212")
    ("grey8" . "#141414")
    ("grey9" . "#171717")
    ("grey10" . "#1A1A1A")
    ("grey11" . "#1C1C1C")
    ("grey12" . "#1F1F1F")
    ("grey13" . "#212121")
    ("grey14" . "#242424")
    ("grey15" . "#262626")
    ("grey16" . "#292929")
    ("grey17" . "#2B2B2B")
    ("grey18" . "#2E2E2E")
    ("grey19" . "#303030")
    ("grey20" . "#333333")
    ("grey21" . "#363636")
    ("grey22" . "#383838")
    ("grey23" . "#3B3B3B")
    ("grey24" . "#3D3D3D")
    ("grey25" . "#404040")
    ("grey26" . "#424242")
    ("grey27" . "#454545")
    ("grey28" . "#474747")
    ("grey29" . "#4A4A4A")
    ("grey30" . "#4D4D4D")
    ("grey31" . "#4F4F4F")
    ("grey32" . "#525252")
    ("grey33" . "#545454")
    ("grey34" . "#575757")
    ("grey35" . "#595959")
    ("grey36" . "#5C5C5C")
    ("grey37" . "#5E5E5E")
    ("grey38" . "#616161")
    ("grey39" . "#636363")
    ("grey40" . "#666666")
    ("grey41" . "#696969")
    ("grey42" . "#6B6B6B")
    ("grey43" . "#6E6E6E")
    ("grey44" . "#707070")
    ("grey45" . "#737373")
    ("grey46" . "#757575")
    ("grey47" . "#787878")
    ("grey48" . "#7A7A7A")
    ("grey49" . "#7D7D7D")
    ("grey50" . "#7F7F7F")
    ("grey51" . "#828282")
    ("grey52" . "#858585")
    ("grey53" . "#878787")
    ("grey54" . "#8A8A8A")
    ("grey55" . "#8C8C8C")
    ("grey56" . "#8F8F8F")
    ("grey57" . "#919191")
    ("grey58" . "#949494")
    ("grey59" . "#969696")
    ("grey60" . "#999999")
    ("grey61" . "#9C9C9C")
    ("grey62" . "#9E9E9E")
    ("grey63" . "#A1A1A1")
    ("grey64" . "#A3A3A3")
    ("grey65" . "#A6A6A6")
    ("grey66" . "#A8A8A8")
    ("grey67" . "#ABABAB")
    ("grey68" . "#ADADAD")
    ("grey69" . "#B0B0B0")
    ("grey70" . "#B3B3B3")
    ("grey71" . "#B5B5B5")
    ("grey72" . "#B8B8B8")
    ("grey73" . "#BABABA")
    ("grey74" . "#BDBDBD")
    ("grey75" . "#BFBFBF")
    ("grey76" . "#C2C2C2")
    ("grey77" . "#C4C4C4")
    ("grey78" . "#C7C7C7")
    ("grey79" . "#C9C9C9")
    ("grey80" . "#CCCCCC")
    ("grey81" . "#CFCFCF")
    ("grey82" . "#D1D1D1")
    ("grey83" . "#D4D4D4")
    ("grey84" . "#D6D6D6")
    ("grey85" . "#D9D9D9")
    ("grey86" . "#DBDBDB")
    ("grey87" . "#DEDEDE")
    ("grey88" . "#E0E0E0")
    ("grey89" . "#E3E3E3")
    ("grey90" . "#E5E5E5")
    ("grey91" . "#E8E8E8")
    ("grey92" . "#EBEBEB")
    ("grey93" . "#EDEDED")
    ("grey94" . "#F0F0F0")
    ("grey95" . "#F2F2F2")
    ("grey96" . "#F5F5F5")
    ("grey97" . "#F7F7F7")
    ("grey98" . "#FAFAFA")
    ("grey99" . "#FCFCFC")
    ("grey100" . "#FFFFFF")
    ("honeydew" . "#F0FFF0")
    ("honeydew1" . "#F0FFF0")
    ("honeydew2" . "#E0EEE0")
    ("honeydew3" . "#C1CDC1")
    ("honeydew4" . "#838B83")
    ("hotpink" . "#FF69B4")
    ("hotpink1" . "#FF6EB4")
    ("hotpink2" . "#EE6AA7")
    ("hotpink3" . "#CD6090")
    ("hotpink4" . "#8B3A62")
    ("indianred" . "#CD5C5C")
    ("indianred1" . "#FF6A6A")
    ("indianred2" . "#EE6363")
    ("indianred3" . "#CD5555")
    ("indianred4" . "#8B3A3A")
    ("ivory" . "#FFFFF0")
    ("ivory1" . "#FFFFF0")
    ("ivory2" . "#EEEEE0")
    ("ivory3" . "#CDCDC1")
    ("ivory4" . "#8B8B83")
    ("khaki" . "#F0E68C")
    ("khaki1" . "#FFF68F")
    ("khaki2" . "#EEE685")
    ("khaki3" . "#CDC673")
    ("khaki4" . "#8B864E")
    ("lavender" . "#E6E6FA")
    ("lavenderblush" . "#FFF0F5")
    ("lavenderblush1" . "#FFF0F5")
    ("lavenderblush2" . "#EEE0E5")
    ("lavenderblush3" . "#CDC1C5")
    ("lavenderblush4" . "#8B8386")
    ("lawngreen" . "#7CFC00")
    ("lemonchiffon" . "#FFFACD")
    ("lemonchiffon1" . "#FFFACD")
    ("lemonchiffon2" . "#EEE9BF")
    ("lemonchiffon3" . "#CDC9A5")
    ("lemonchiffon4" . "#8B8970")
    ("lightblue" . "#ADD8E6")
    ("lightblue1" . "#BFEFFF")
    ("lightblue2" . "#B2DFEE")
    ("lightblue3" . "#9AC0CD")
    ("lightblue4" . "#68838B")
    ("lightcoral" . "#F08080")
    ("lightcyan" . "#E0FFFF")
    ("lightcyan1" . "#E0FFFF")
    ("lightcyan2" . "#D1EEEE")
    ("lightcyan3" . "#B4CDCD")
    ("lightcyan4" . "#7A8B8B")
    ("lightgoldenrod" . "#EEDD82")
    ("lightgoldenrod1" . "#FFEC8B")
    ("lightgoldenrod2" . "#EEDC82")
    ("lightgoldenrod3" . "#CDBE70")
    ("lightgoldenrod4" . "#8B814C")
    ("lightgoldenrodyellow" . "#FAFAD2")
    ("lightgray" . "#D3D3D3")
    ("lightgreen" . "#90EE90")
    ("lightgrey" . "#D3D3D3")
    ("lightpink" . "#FFB6C1")
    ("lightpink1" . "#FFAEB9")
    ("lightpink2" . "#EEA2AD")
    ("lightpink3" . "#CD8C95")
    ("lightpink4" . "#8B5F65")
    ("lightsalmon" . "#FFA07A")
    ("lightsalmon1" . "#FFA07A")
    ("lightsalmon2" . "#EE9572")
    ("lightsalmon3" . "#CD8162")
    ("lightsalmon4" . "#8B5742")
    ("lightseagreen" . "#20B2AA")
    ("lightskyblue" . "#87CEFA")
    ("lightskyblue1" . "#B0E2FF")
    ("lightskyblue2" . "#A4D3EE")
    ("lightskyblue3" . "#8DB6CD")
    ("lightskyblue4" . "#607B8B")
    ("lightslateblue" . "#8470FF")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#B0C4DE")
    ("lightsteelblue1" . "#CAE1FF")
    ("lightsteelblue2" . "#BCD2EE")
    ("lightsteelblue3" . "#A2B5CD")
    ("lightsteelblue4" . "#6E7B8B")
    ("lightyellow" . "#FFFFE0")
    ("lightyellow1" . "#FFFFE0")
    ("lightyellow2" . "#EEEED1")
    ("lightyellow3" . "#CDCDB4")
    ("lightyellow4" . "#8B8B7A")
    ("limegreen" . "#32CD32")
    ("linen" . "#FAF0E6")
    ("magenta" . "#FF00FF")
    ("magenta1" . "#FF00FF")
    ("magenta2" . "#EE00EE")
    ("magenta3" . "#CD00CD")
    ("magenta4" . "#8B008B")
    ("maroon" . "#B03060")
    ("maroon1" . "#FF34B3")
    ("maroon2" . "#EE30A7")
    ("maroon3" . "#CD2990")
    ("maroon4" . "#8B1C62")
    ("mediumaquamarine" . "#66CDAA")
    ("mediumblue" . "#0000CD")
    ("mediumorchid" . "#BA55D3")
    ("mediumorchid1" . "#E066FF")
    ("mediumorchid2" . "#D15FEE")
    ("mediumorchid3" . "#B452CD")
    ("mediumorchid4" . "#7A378B")
    ("mediumpurple" . "#9370DB")
    ("mediumpurple1" . "#AB82FF")
    ("mediumpurple2" . "#9F79EE")
    ("mediumpurple3" . "#8968CD")
    ("mediumpurple4" . "#5D478B")
    ("mediumseagreen" . "#3CB371")
    ("mediumslateblue" . "#7B68EE")
    ("mediumspringgreen" . "#00FA9A")
    ("mediumturquoise" . "#48D1CC")
    ("mediumvioletred" . "#C71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#F5FFFA")
    ("mistyrose" . "#FFE4E1")
    ("mistyrose1" . "#FFE4E1")
    ("mistyrose2" . "#EED5D2")
    ("mistyrose3" . "#CDB7B5")
    ("mistyrose4" . "#8B7D7B")
    ("moccasin" . "#FFE4B5")
    ("navajowhite" . "#FFDEAD")
    ("navajowhite1" . "#FFDEAD")
    ("navajowhite2" . "#EECFA1")
    ("navajowhite3" . "#CDB38B")
    ("navajowhite4" . "#8B795E")
    ("navy" . "#000080")
    ("navyblue" . "#000080")
    ("oldlace" . "#FDF5E6")
    ("olivedrab" . "#6B8E23")
    ("olivedrab1" . "#C0FF3E")
    ("olivedrab2" . "#B3EE3A")
    ("olivedrab3" . "#9ACD32")
    ("olivedrab4" . "#698B22")
    ("orange" . "#FFA500")
    ("orange1" . "#FFA500")
    ("orange2" . "#EE9A00")
    ("orange3" . "#CD8500")
    ("orange4" . "#8B5A00")
    ("orangered" . "#FF4500")
    ("orangered1" . "#FF4500")
    ("orangered2" . "#EE4000")
    ("orangered3" . "#CD3700")
    ("orangered4" . "#8B2500")
    ("orchid" . "#DA70D6")
    ("orchid1" . "#FF83FA")
    ("orchid2" . "#EE7AE9")
    ("orchid3" . "#CD69C9")
    ("orchid4" . "#8B4789")
    ("palegoldenrod" . "#EEE8AA")
    ("palegreen" . "#98FB98")
    ("palegreen1" . "#9AFF9A")
    ("palegreen2" . "#90EE90")
    ("palegreen3" . "#7CCD7C")
    ("palegreen4" . "#548B54")
    ("paleturquoise" . "#AFEEEE")
    ("paleturquoise1" . "#BBFFFF")
    ("paleturquoise2" . "#AEEEEE")
    ("paleturquoise3" . "#96CDCD")
    ("paleturquoise4" . "#668B8B")
    ("palevioletred" . "#DB7093")
    ("palevioletred1" . "#FF82AB")
    ("palevioletred2" . "#EE799F")
    ("palevioletred3" . "#CD6889")
    ("palevioletred4" . "#8B475D")
    ("papayawhip" . "#FFEFD5")
    ("peachpuff" . "#FFDAB9")
    ("peachpuff1" . "#FFDAB9")
    ("peachpuff2" . "#EECBAD")
    ("peachpuff3" . "#CDAF95")
    ("peachpuff4" . "#8B7765")
    ("peru" . "#CD853F")
    ("pink" . "#FFC0CB")
    ("pink1" . "#FFB5C5")
    ("pink2" . "#EEA9B8")
    ("pink3" . "#CD919E")
    ("pink4" . "#8B636C")
    ("plum" . "#DDA0DD")
    ("plum1" . "#FFBBFF")
    ("plum2" . "#EEAEEE")
    ("plum3" . "#CD96CD")
    ("plum4" . "#8B668B")
    ("powderblue" . "#B0E0E6")
    ("purple" . "#A020F0")
    ("purple1" . "#9B30FF")
    ("purple2" . "#912CEE")
    ("purple3" . "#7D26CD")
    ("purple4" . "#551A8B")
    ("red" . "#FF0000")
    ("red1" . "#FF0000")
    ("red2" . "#EE0000")
    ("red3" . "#CD0000")
    ("red4" . "#8B0000")
    ("rosybrown" . "#BC8F8F")
    ("rosybrown1" . "#FFC1C1")
    ("rosybrown2" . "#EEB4B4")
    ("rosybrown3" . "#CD9B9B")
    ("rosybrown4" . "#8B6969")
    ("royalblue" . "#4169E1")
    ("royalblue1" . "#4876FF")
    ("royalblue2" . "#436EEE")
    ("royalblue3" . "#3A5FCD")
    ("royalblue4" . "#27408B")
    ("saddlebrown" . "#8B4513")
    ("salmon" . "#FA8072")
    ("salmon1" . "#FF8C69")
    ("salmon2" . "#EE8262")
    ("salmon3" . "#CD7054")
    ("salmon4" . "#8B4C39")
    ("sandybrown" . "#F4A460")
    ("seagreen" . "#2E8B57")
    ("seagreen1" . "#54FF9F")
    ("seagreen2" . "#4EEE94")
    ("seagreen3" . "#43CD80")
    ("seagreen4" . "#2E8B57")
    ("seashell" . "#FFF5EE")
    ("seashell1" . "#FFF5EE")
    ("seashell2" . "#EEE5DE")
    ("seashell3" . "#CDC5BF")
    ("seashell4" . "#8B8682")
    ("sienna" . "#A0522D")
    ("sienna1" . "#FF8247")
    ("sienna2" . "#EE7942")
    ("sienna3" . "#CD6839")
    ("sienna4" . "#8B4726")
    ("skyblue" . "#87CEEB")
    ("skyblue1" . "#87CEFF")
    ("skyblue2" . "#7EC0EE")
    ("skyblue3" . "#6CA6CD")
    ("skyblue4" . "#4A708B")
    ("slateblue" . "#6A5ACD")
    ("slateblue1" . "#836FFF")
    ("slateblue2" . "#7A67EE")
    ("slateblue3" . "#6959CD")
    ("slateblue4" . "#473C8B")
    ("slategray" . "#708090")
    ("slategray1" . "#C6E2FF")
    ("slategray2" . "#B9D3EE")
    ("slategray3" . "#9FB6CD")
    ("slategray4" . "#6C7B8B")
    ("slategrey" . "#708090")
    ("snow" . "#FFFAFA")
    ("snow1" . "#FFFAFA")
    ("snow2" . "#EEE9E9")
    ("snow3" . "#CDC9C9")
    ("snow4" . "#8B8989")
    ("springgreen" . "#00FF7F")
    ("springgreen1" . "#00FF7F")
    ("springgreen2" . "#00EE76")
    ("springgreen3" . "#00CD66")
    ("springgreen4" . "#008B45")
    ("steelblue" . "#4682B4")
    ("steelblue1" . "#63B8FF")
    ("steelblue2" . "#5CACEE")
    ("steelblue3" . "#4F94CD")
    ("steelblue4" . "#36648B")
    ("tan" . "#D2B48C")
    ("tan1" . "#FFA54F")
    ("tan2" . "#EE9A49")
    ("tan3" . "#CD853F")
    ("tan4" . "#8B5A2B")
    ("thistle" . "#D8BFD8")
    ("thistle1" . "#FFE1FF")
    ("thistle2" . "#EED2EE")
    ("thistle3" . "#CDB5CD")
    ("thistle4" . "#8B7B8B")
    ("tomato" . "#FF6347")
    ("tomato1" . "#FF6347")
    ("tomato2" . "#EE5C42")
    ("tomato3" . "#CD4F39")
    ("tomato4" . "#8B3626")
    ("turquoise" . "#40E0D0")
    ("turquoise1" . "#00F5FF")
    ("turquoise2" . "#00E5EE")
    ("turquoise3" . "#00C5CD")
    ("turquoise4" . "#00868B")
    ("violet" . "#EE82EE")
    ("violetred" . "#D02090")
    ("violetred1" . "#FF3E96")
    ("violetred2" . "#EE3A8C")
    ("violetred3" . "#CD3278")
    ("violetred4" . "#8B2252")
    ("wheat" . "#F5DEB3")
    ("wheat1" . "#FFE7BA")
    ("wheat2" . "#EED8AE")
    ("wheat3" . "#CDBA96")
    ("wheat4" . "#8B7E66")
    ("whitesmoke" . "#F5F5F5")
    ("yellow" . "#FFFF00")
    ("yellow1" . "#FFFF00")
    ("yellow2" . "#EEEE00")
    ("yellow3" . "#CDCD00")
    ("yellow4" . "#8B8B00")
    ("yellowgreen" . "#9ACD32"))
  "Alist of R colors.
Each entry should have the form (COLOR-NAME . HEXADECIMAL-COLOR)."
  :group 'rainbow)
(defcustom rainbow-r-colors-major-mode-list
  '(ess-mode)
  "List of major mode where R colors are enabled when
`rainbow-r-colors' is set to auto."
  :group 'rainbow)

(defcustom rainbow-r-colors 'auto
  "When to enable R colors.
If set to t, the R colors will be enabled.  If set to nil, the
R colors will not be enabled.  If set to auto, the R colors
will be enabled if a major mode has been detected from the
`rainbow-r-colors-major-mode-list'."
  :group 'rainbow)


;; Functions
(defun rainbow-colorize-match (color &optional match)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
  (let ((match (or match 0)))
    (put-text-property
     (match-beginning match) (match-end match)
     'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                               "white" "black"))
             (:background ,color)))))

(defun rainbow-colorize-itself (&optional match)
  "Colorize a match with itself."
  (rainbow-colorize-match (match-string-no-properties (or match 0)) match))

(defun rainbow-colorize-hexadecimal-without-sharp ()
  "Colorize an hexadecimal colors and prepend # to it."
  (rainbow-colorize-match (concat "#" (match-string-no-properties 1))))

(defun rainbow-colorize-by-assoc (assoc-list)
  "Colorize a match with its association from ASSOC-LIST."
  (rainbow-colorize-match (cdr (assoc-string (match-string-no-properties 0)
                                             assoc-list t))))

(defun rainbow-rgb-relative-to-absolute (number)
  "Convert a relative NUMBER to absolute. If NUMBER is absolute, return NUMBER.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\"."
  (let ((string-length (- (length number) 1)))
    ;; Is this a number with %?
    (if (eq (elt number string-length) ?%)
        (/ (* (string-to-number (substring number 0 string-length)) 255) 100)
      (string-to-number number))))

(defun rainbow-colorize-hsl ()
  "Colorize a match with itself."
  (let ((h (/ (string-to-number (match-string-no-properties 1)) 360.0))
        (s (/ (string-to-number (match-string-no-properties 2)) 100.0))
        (l (/ (string-to-number (match-string-no-properties 3)) 100.0)))
    (rainbow-colorize-match
     (multiple-value-bind (r g b)
	 (color-hsl-to-rgb h s l)
       (format "#%02X%02X%02X" (* r 255) (* g 255) (* b 255))))))

(defun rainbow-colorize-rgb ()
  "Colorize a match with itself."
  (let ((r (rainbow-rgb-relative-to-absolute (match-string-no-properties 1)))
        (g (rainbow-rgb-relative-to-absolute (match-string-no-properties 2)))
        (b (rainbow-rgb-relative-to-absolute (match-string-no-properties 3))))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-colorize-rgb-float ()
  "Colorize a match with itself, with relative value."
  (let ((r (* (string-to-number (match-string-no-properties 1)) 255.0))
        (g (* (string-to-number (match-string-no-properties 2)) 255.0))
        (b (* (string-to-number (match-string-no-properties 3)) 255.0)))
    (rainbow-colorize-match (format "#%02X%02X%02X" r g b))))

(defun rainbow-colorize-ansi ()
  "Return a matched string propertized with ansi color face."
  (let ((xterm-color? (featurep 'xterm-color))
        (string (match-string-no-properties 0))
        color)
    (save-match-data
      (let* ((replaced (concat
                        (replace-regexp-in-string
                         "^\\(\\\\[eE]\\|\\\\033\\|\\\\x1[bB]\\)"
                         "\033" string) "x"))
             xterm-color-current
             ansi-color-context
             (applied (funcall (if xterm-color?
                                   'xterm-color-filter
                                 'ansi-color-apply)
                               replaced))
             (face-property (get-text-property
                             0
                             (if xterm-color? 'face 'font-lock-face)
                             applied)))
        (unless (listp (car face-property))
          (setq face-property (list face-property)))
        (setq color (funcall (if xterm-color? 'cadr 'cdr)
                             (or (assq (if xterm-color?
                                           :foreground
                                         'foreground-color)
                                       face-property)
                                 (assq (if xterm-color?
                                           :background
                                         'background-color)
                                       face-property))))))
    (when color
      (rainbow-colorize-match color))))

(defun rainbow-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun rainbow-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
	 (r (/ (car values) 256.0))
         (g (/ (cadr values) 256.0))
	 (b (/ (caddr values) 256.0)))
    (rainbow-color-luminance r g b)))

(defun rainbow-turn-on ()
  "Turn on raibow-mode."
  (font-lock-add-keywords nil
                          rainbow-hexadecimal-colors-font-lock-keywords)
  ;; Activate X colors?
  (when (or (eq rainbow-x-colors t)
            (and (eq rainbow-x-colors 'auto)
                 (memq major-mode rainbow-x-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-x-colors-font-lock-keywords))
  ;; Activate LaTeX colors?
  (when (or (eq rainbow-latex-colors t)
            (and (eq rainbow-latex-colors 'auto)
                 (memq major-mode rainbow-latex-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-latex-rgb-colors-font-lock-keywords))
  ;; Activate ANSI colors?
  (when (or (eq rainbow-ansi-colors t)
            (and (eq rainbow-ansi-colors 'auto)
                 (memq major-mode rainbow-ansi-colors-major-mode-list)))
    (font-lock-add-keywords nil
                            rainbow-ansi-colors-font-lock-keywords))
  ;; Activate HTML colors?
  (when (or (eq rainbow-html-colors t)
            (and (eq rainbow-html-colors 'auto)
                 (memq major-mode rainbow-html-colors-major-mode-list)))
    (setq rainbow-html-colors-font-lock-keywords
          `((,(regexp-opt (mapcar 'car rainbow-html-colors-alist) 'words)
             (0 (rainbow-colorize-by-assoc rainbow-html-colors-alist)))))
    (font-lock-add-keywords nil
                            `(,@rainbow-html-colors-font-lock-keywords
                              ,@rainbow-html-rgb-colors-font-lock-keywords)))
  ;; Activate R colors?
  (when (or (eq rainbow-r-colors t)
            (and (eq rainbow-r-colors 'auto)
                 (memq major-mode rainbow-r-colors-major-mode-list)))
    (setq rainbow-r-colors-font-lock-keywords
          `((,(regexp-opt (mapcar 'car rainbow-r-colors-alist) 'words)
             (0 (rainbow-colorize-by-assoc rainbow-r-colors-alist)))))
    (font-lock-add-keywords nil
                            rainbow-r-colors-font-lock-keywords
                            )))

(defun rainbow-turn-off ()
  "Turn off rainbow-mode."
  (font-lock-remove-keywords
   nil
   `(,@rainbow-hexadecimal-colors-font-lock-keywords
     ,@rainbow-x-colors-font-lock-keywords
     ,@rainbow-latex-rgb-colors-font-lock-keywords
     ,@rainbow-r-colors-font-lock-keywords
     ,@rainbow-html-colors-font-lock-keywords
     ,@rainbow-html-rgb-colors-font-lock-keywords)))

;;;###autoload
(define-minor-mode rainbow-mode
  "Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\"."
  :lighter " Rbow"
  (progn
    (if rainbow-mode
        (rainbow-turn-on)
      (rainbow-turn-off))))

;;;; ChangeLog:

;; 2013-08-05  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: 0.9, allow spaces in LaTeX colors
;; 
;; 2013-05-03  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: add support for R, bump version to 0.8
;; 	
;; 	Signed-off-by: Julien Danjou <julien@danjou.info>
;; 
;; 2013-02-26  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: version 0.7
;; 	
;; 	* rainbow-mode.el: don't activate font-lock-mode
;; 
;; 2012-12-11  Julien Danjou  <julien@danjou.info>
;; 
;; 	* rainbow-mode: update to 0.6, add support for ANSI coloring
;; 
;; 2012-11-26  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: fix some LaTex docstrings
;; 
;; 2012-11-14  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: version 0.5
;; 	
;; 	* rainbow-mode.el: fix syntax error on
;; 	`rainbow-hexadecimal-colors-font-lock-keywords'.
;; 
;; 2012-11-09  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode: version 0.4
;; 	
;; 	* rainbow-mode.el: Use functions from color package to colorize HSL rather
;; 	than our own copy.
;; 
;; 2012-11-09  Julien Danjou  <julien@danjou.info>
;; 
;; 	rainbow-mode 0.3
;; 	
;; 	* rainbow-mode.el: avoid colorizing HTML entities
;; 
;; 2011-09-23  Julien Danjou  <julien@danjou.info>
;; 
;; 	Update rainbow-mode to version 0.2
;; 
;; 2011-07-01  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Reorganize repository layout, allowing site installation.
;; 	
;; 	A Makefile with "site", "archive" and "archive-full" rules can now be
;; 	used for site-installation, partial archive deployment, and full
;; 	archive deployment respectively.
;; 	
;; 	Rewrite the admin/archive-contents.el script to handle these changes.
;; 
;; 2011-07-01  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Give every package its own directory in packages/
;; 	including single-file packages.
;; 


(provide 'rainbow-mode)

;;; rainbow-mode.el ends here
