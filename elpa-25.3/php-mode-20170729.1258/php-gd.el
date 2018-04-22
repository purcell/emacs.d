;; Copyright (C) 2015  David Arroyo Menéndez

;; Author: David Arroyo Menéndez <davidam@gnu.org>
;; Maintainer: David Arroyo Menéndez <davidam@gnu.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
;; Boston, MA 02110-1301 USA,

;; GD functions
;; file:///usr/share/doc/php-doc/html/ref.image.html
;; http://php.net/manual/en/ref.image.php

(define-skeleton php-gd_info
  "Insert a gd_info statement. Retrieve information about the currently installed GD library"
  ""
  > "gd_info();" \n
)

(define-skeleton php-getimagesize
  "Insert a getimagesize statement."
  ""
  '(setq filename (skeleton-read "Filename? "))
  '(setq imageinfo (skeleton-read "Image info? "))
  > "getimagesize(" filename ", " imageinfo ");" \n
)

(define-skeleton php-getimagesizefromstring
  "Insert a getimagesizefromstring statement."
  ""
  '(setq imagedata (skeleton-read "Image data? "))
  '(setq imageinfo (skeleton-read "Image info? "))
  > "getimagesizefromstring(" imagedata ", " imageinfo ");" \n
)

(define-skeleton php-image_type_to_extension
  "Insert an image_type_to_extension statement. Get file extension for image type"
  ""
  '(setq imagetype (skeleton-read "Image type?"))
  '(setq include_dot (skeleton-read "Include dot to the extension (TRUE | FALSE)"))
  > "image_type_to_extension(" imagetype ", " include_dot ");" \n
)

(define-skeleton php-image_type_to_mime_type
  "Insert an image_type_to_mime_type statement"
  ""
  '(setq imagetype (skeleton-read "Image type?"))
  > "image_type_to_mime_type(" imagetype ");" \n
) 

(define-skeleton php-image2wbmp
  "Insert an image2wbmp statement. Output image to browser or file"
  ""
  '(setq filename (skeleton-read "Filename? "))
  '(setq image (skeleton-read "Image? "))
  '(setq treshold (skeleton-read "Treshold? "))
  > "image2wbmp(" filename ", " image ", " treshold ");" \n
)

(define-skeleton php-imageaffine
  "Insert an imageaffine statement."
  ""
  '(setq image (skeleton-read "Image? "))
  '(setq affine (skeleton-read "Affine array? "))
  '(setq clip (skeleton-read "Clip? "))
  > "imageaffine(" image ", " affine ", " clip ");" \n
)

(define-skeleton php-imageaffinematrixconcat
  "Insert an imageaffinematrixconcat statement"
  ""
  '(setq m1 (skeleton-read "Matrix 1? "))
  '(setq m2 (skeleton-read "Matrix 2? "))
  > "imageaffinematrixconcat(" m1 ", " m2 ");" \n
)

(define-skeleton php-imageaffinematrixget
  "Insert an imageaffinematrixget statement"
  ""
  '(setq type (skeleton-read "Type? "))
  '(setq options (skeleton-read "Options? "))
  > "imageaffinematrixget(" m1 ", " m2 ");" \n
)

(define-skeleton php-imagealphablending
  "Insert an imagealphablending statement"
  ""
  '(setq image (skeleton-read "Image? "))
  '(setq blendmode (skeleton-read "Blendmode? (TRUE | FALSE)"))
  > "imagealphablending(" image ", " blendmode ");" \n
)

(define-skeleton php-imageantialias
  "Insert an imageantialias statement. Should antialias functions be used or not"
  ""
  '(setq image (skeleton-read "Image? "))
  '(setq enabled (skeleton-read "Enabled? (true | false) "))
  > "imageantialias(" image ", " enabled ");" \n
)

(define-skeleton php-imagearc
  "Insert an imagearc statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq cx (skeleton-read "x-coordinate of the center: "))
  '(setq cy (skeleton-read "y-coordinate of the center: "))
  '(setq width (skeleton-read "width: "))
  '(setq height (skeleton-read "height: "))
  '(setq start (skeleton-read "start: "))
  '(setq end (skeleton-read "end: "))
  '(setq color (skeleton-read "color: "))
  > "imagearc(" image ", " cx ", " cy ", " width ", " height ", " start ", " end ", " color ");" \n
)

(define-skeleton php-imagechar
  "Insert an imagechar statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq font (skeleton-read "Font: ")) 
  '(setq x (skeleton-read "x: "))
  '(setq y (skeleton-read "y: "))
  '(setq c (skeleton-read "c: "))
  '(setq color (skeleton-read "color: "))
  > "imagechar(" image ", " font ", " x ", " y ", " c ", " color ");" \n
)

(define-skeleton php-imagecharup
  "Insert an imagecharup statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq font (skeleton-read "Font: ")) 
  '(setq x (skeleton-read "x: "))
  '(setq y (skeleton-read "y: "))
  '(setq c (skeleton-read "c: "))
  '(setq color (skeleton-read "color: "))
  > "imagecharup(" image ", " font ", " x ", " y ", " c ", " color ");" \n
)

(define-skeleton php-imagecolorallocate
  "Insert an imagecolorallocate statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  > "imagecolorallocate(" image ", " red ", " green ", " blue ");" \n
)

(define-skeleton php-imagecolorallocatealpha
  "Insert an imagecolorallocatealpha statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  '(setq alpha (skeleton-read "Alpha: "))
  > "imagecolorallocatealpha(" image ", " red ", " green ", " blue ", " alpha ");" \n
)

(define-skeleton php-imagecolorat
  "Insert an imagecolorat statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq x (skeleton-read "x: "))
  '(setq y (skeleton-read "y: "))
  > "imagecolorat(" image ", " x ", " y ");" \n
)

(define-skeleton php-imagecolorclosest 
  "Insert an imagecolorclosest statement. Get the index of the closest color to the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  > "imagecolorclosest(" image ", " red ", " green ", " blue ");" \n
)

(define-skeleton php-imagecolorclosestalpha 
  "Insert an imagecolorclosest statement. Get the index of the closest color to the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  '(setq alpha (skeleton-read "Alpha: "))
  > "imagecolorclosestalpha(" image ", " red ", " green ", " blue ", " alpha ");" \n
)

(define-skeleton php-imagecolorclosesthwb
  "Insert an imagecolorclosesthwb statement. Get the index of the closest color to the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  > "imagecolorclosestalpha(" image ", " red ", " green ", " blue ");" \n
)

(define-skeleton php-imagecolordeallocate
  "Insert an imagecolordeallocate statement. De-allocate a color for an image"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq color (skeleton-read "color: "))
  > "imagecolordeallocate(" image ", " color ");" \n
)

(define-skeleton php-imagecolorexact
  "Insert an imagecolorexact statement. Get the index of the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  > "imagecolorexact(" image ", " red ", " green ", " blue ");" \n
)

(define-skeleton php-imagecolorexactalpha
  "Insert an imagecolorexact statement. Get the index of the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  '(setq alpha (skeleton-read "Alpha: "))
  > "imagecolorexact(" image ", " red ", " green ", " blue ", " alpha ");" \n
)

(define-skeleton php-imagecolormatch
  "Insert an imagecolormatch. Makes the colors of the palette version of an image more closely match the true color version"
  '(setq image1 (skeleton-read "Image 1: "))
  '(setq image2 (skeleton-read "Image 2: "))
  > "imagecolormatch(" image1 ", " image2 ");" \n
)

(define-skeleton php-imagecolorresolve
  "Insert an imagecolorresolve statement. Get the index of the specified color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  > "imagecolorresolve(" image ", " red ", " green ", " blue ");" \n
)

(define-skeleton php-imagecolorresolvealpha
  "Insert an imagecolorresolvealpha statement. Get the index of the specified color + alpha or it closest alternative"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  '(setq alpha (skeleton-read "Alpha: "))
  > "imagecolorresolvealpha(" image ", " red ", " green ", " blue ", " alpha ");" \n
)

(define-skeleton php-imagecolorset
  "Insert an imagecolorset statement. Set the color for the specified palette index"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq index (skeleton-read "Index: "))
  '(setq red (skeleton-read "Red: "))
  '(setq green (skeleton-read "Green: "))
  '(setq blue (skeleton-read "Blue: "))
  '(setq alpha (skeleton-read "Alpha: "))
  > "imagecolorset(" image ", " index ", " red ", " green ", " blue ", " alpha ");" \n
)

(define-skeleton php-imagecolorsforindex
  "Insert an imagecolorsforindex statement. Get the colors for an index"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq index (skeleton-read "Index: "))
  > "imagecolorsforindex(" image ", " index ");" \n
)

(define-skeleton php-imagecolorstotal
  "Insert an imagecolorstotal statement. Get the colors for an index"
  ""
  '(setq image (skeleton-read "Image: "))
  > "imagecolorstotal(" image ");" \n
)

(define-skeleton php-imagecolortransparent
  "Insert an imagecolortransparent statement. Sets the transparent color in the given image."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq color (skeleton-read "Color: "))
  > "imagecolortransparent(" image ", " color ");" \n
)

(define-skeleton php-imageconvolution
  "Insert an imageconvolution statement."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq matrix (skeleton-read "Matrix: "))
  '(setq div (skeleton-read "Divisor result of the convolution: "))
  '(setq offset (skeleton-read "Offset: "))
  > "imageconvolution(" image ", " matrix ", " div ", " offset ");" \n
)

(define-skeleton php-imagecopy
  "Insert an imagecopy statement."
  ""
  '(setq dst_im (skeleton-read "Destination image: "))
  '(setq src_im (skeleton-read "Source image: "))
  '(setq dst_x (skeleton-read "X coordinate of destination point: "))
  '(setq dst_y (skeleton-read "Y coordinate of destination point: "))
  '(setq src_x (skeleton-read "X coordinate of source point: "))
  '(setq src_y (skeleton-read "Y coordinate of source point: "))
  > "imagecopy(" dst_im ", " src_im ", " dst_x ", " dst_y ", " src_x ", " src_y ");" \n
)

(define-skeleton php-imagecopymerge
  "Insert an imagecopymerge statement."
  ""
  '(setq dst_im (skeleton-read "Destination image: "))
  '(setq src_im (skeleton-read "Source image: "))
  '(setq dst_x (skeleton-read "X coordinate of destination point: "))
  '(setq dst_y (skeleton-read "Y coordinate of destination point: "))
  '(setq src_x (skeleton-read "X coordinate of source point: "))
  '(setq src_y (skeleton-read "Y coordinate of source point: "))
  '(setq src_w (skeleton-read "Source width: "))
  '(setq src_h (skeleton-read "Source height: "))
  '(setq pct (skeleton-read "Pct: "))
  > "imagecopymerge(" dst_im ", " src_im ", " dst_x ", " dst_y ", " src_x ", " src_y ", " src_w ", " src_h ", " pct ");" \n
)

(define-skeleton php-imagecopymergegray
  "Insert an imagecopymergegray statement."
  ""
  '(setq dst_im (skeleton-read "Destination image: "))
  '(setq src_im (skeleton-read "Source image: "))
  '(setq dst_x (skeleton-read "X coordinate of destination point: "))
  '(setq dst_y (skeleton-read "Y coordinate of destination point: "))
  '(setq src_x (skeleton-read "X coordinate of source point: "))
  '(setq src_y (skeleton-read "Y coordinate of source point: "))
  '(setq src_w (skeleton-read "Source width: "))
  '(setq src_h (skeleton-read "Source height: "))
  '(setq pct (skeleton-read "Pct: "))
  > "imagecopymergegray(" dst_im ", " src_im ", " dst_x ", " dst_y ", " src_x ", " src_y ", " src_w ", " src_h ", " pct ");" \n
)


(define-skeleton php-imagecopyresampled
  "Insert an imagecopyresampled statement."
  ""
  '(setq dst_im (skeleton-read "Destination image: "))
  '(setq src_im (skeleton-read "Source image: "))
  '(setq dst_x (skeleton-read "X coordinate of destination point: "))
  '(setq dst_y (skeleton-read "Y coordinate of destination point: "))
  '(setq src_x (skeleton-read "X coordinate of source point: "))
  '(setq src_y (skeleton-read "Y coordinate of source point: "))
  '(setq dst_w (skeleton-read "Destination width: "))
  '(setq dst_h (skeleton-read "Destination height: "))
  '(setq src_w (skeleton-read "Source width: "))
  '(setq src_h (skeleton-read "Source height: "))
  '(setq pct (skeleton-read "Pct: "))
  > "imagecopyresampled(" dst_im ", " src_im ", " dst_x ", " dst_y ", " src_x ", " src_y ", " dst_w ", " dst_h ", " src_w ", " src_h ", " pct ");" \n
)

(define-skeleton php-imagecopyresized
  "Insert an imagecopyresized statement."
  ""
  '(setq dst_im (skeleton-read "Destination image: "))
  '(setq src_im (skeleton-read "Source image: "))
  '(setq dst_x (skeleton-read "X coordinate of destination point: "))
  '(setq dst_y (skeleton-read "Y coordinate of destination point: "))
  '(setq src_x (skeleton-read "X coordinate of source point: "))
  '(setq src_y (skeleton-read "Y coordinate of source point: "))
  '(setq dst_w (skeleton-read "Destination width: "))
  '(setq dst_h (skeleton-read "Destination height: "))
  '(setq src_w (skeleton-read "Source width: "))
  '(setq src_h (skeleton-read "Source height: "))
  '(setq pct (skeleton-read "Pct: "))
  > "imagecopyresized(" dst_im ", " src_im ", " dst_x ", " dst_y ", " src_x ", " src_y ", " dst_w ", " dst_h ", " src_w ", " src_h ", " pct ");" \n
)

(define-skeleton php-imagecreate
  "Insert an imagecreate statement."
  ""
  '(setq width (skeleton-read "Width: "))
  '(setq height (skeleton-read "Height: "))
  > "imagecreate(" width ", " height ");" \n
)

(define-skeleton php-imagecreatefromgd2
  "Insert an imagecreatefromgd2."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromgd2(" filename ");" \n
)

(define-skeleton php-imagecreatefromgd2part
  "Insert an imagecreatefromgd2part"
  ""
  '(setq filename (skeleton-read "Filename: "))
  '(setq src_x (skeleton-read "X coordinate: "))
  '(setq src_y (skeleton-read "Y coordinate: "))
  '(setq width (skeleton-read "Width: "))
  '(setq height (skeleton-read "Height: "))
  > "imagecreatefromgd2part(" filename ", " src_x ", " src_y ", " width ", " height ");" \n
)

(define-skeleton php-imagecreatefromgd
  "Insert an imagecreatefromgd."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromgd(" filename ");" \n
)

(define-skeleton php-imagecreatefromgif
  "Insert an imagecreatefromgif."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromgif(" filename ");" \n
)

(define-skeleton php-imagecreatefromjpeg
  "Insert an imagecreatefromjpeg."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromjpeg(" filename ");" \n
)

(define-skeleton php-imagecreatefrompng
  "Insert an imagecreatefromjpeg."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromjpeg(" filename ");" \n
)

(define-skeleton php-imagecreatefromstring
  "Insert an imagecreatefromstring."
  ""
  '(setq string (skeleton-read "String: "))
  > "imagecreatefromstring(" string ");" \n
)

(define-skeleton php-imagecreatefromwbmp
  "Insert an imagecreatefromwbmp."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromwbmp(" filename ");" \n
)

(define-skeleton php-imagecreatefromwebp
  "Insert an imagecreatefromwebp."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromwebp(" filename ");" \n
)

(define-skeleton php-imagecreatefromxbm
  "Insert an imagecreatefromxbm."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromxbm(" filename ");" \n
)

(define-skeleton php-imagecreatefromxpm
  "Insert an imagecreatefromxpm."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagecreatefromxpm(" filename ");" \n
)

(define-skeleton php-imagecreatetruecolor
  "Insert an imagecreatetruecolor."
  ""
  '(setq width (skeleton-read "Width: "))
  '(setq heigth (skeleton-read "Heigth: "))
  > "imagecreatetruecolor(" width ", " heigth ");" \n
) 

(define-skeleton php-imagecrop
  "Insert an imagecrop."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq rect (skeleton-read "Array Rect: "))
  > "imagecrop(" image ", " rect " );" \n
)

(define-skeleton php-imagecropauto
  "Insert an imagecropauto statement."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq mode (skeleton-read "Mode: "))
  '(setq treshold (skeleton-read "Treshold: "))
  '(setq color (skeleton-read "Color: "))
  > "imagecropauto(" image ", " mode ", " treshold ", " color " );" \n
)

(define-skeleton php-imagedashedline
  "Insert a statement to draw a dahsed line"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq x1 (skeleton-read "Upper left X coordinate: "))
  '(setq y1 (skeleton-read "Upper left Y coordinate: "))
  '(setq x2 (skeleton-read "Bottom right X coordinate: "))
  '(setq y2 (skeleton-read "Bottom right Y coordinate: "))
  '(setq color (skeleton-read "Color: "))
  > "imagedashedline(" image ", " x1 ", " y1 ", " x2 ", " y2 ", " color ");" \n
)

(define-skeleton php-imagedestroy
  "Insert an imagedestroy statement."
  ""
  '(setq image (skeleton-read "Image: "))
  > "imagedestroy(" image " );" \n
)

(define-skeleton php-imageellipse
  "Insert an imageellipse statement."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq cx (skeleton-read "x-coordinate of the center: "))
  '(setq cy (skeleton-read "y-coordinate of the center: "))
  '(setq width (skeleton-read "the ellipse width: "))
  '(setq heigth (skeleton-read "the ellipse heigth: "))
  '(setq color (skeleton-read "the ellipse color: "))
  > "imageellipse(" image ", " cx ", " cy ", " width ", " heigth ", " color ");" \n
)

(define-skeleton php-imagefill
  "Insert an imagefill statement."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq x (skeleton-read "x-coordinate of start point: "))
  '(setq y (skeleton-read "y-coordinate of start point: "))
  '(setq color (skeleton-read "the fill color: "))
  > "imagefill(" image ", " x ", " y ", " color ");" \n
)

(define-skeleton php-imagefilledarc
  "Insert an imagefilledarc statement. Draw a partial arc and fill it"
  ""
  '(setq image (skeleton-read "image: "))
  '(setq cx (skeleton-read "x-coordinate of the center: "))
  '(setq cy (skeleton-read "y-coordinate of the center: "))
  '(setq width (skeleton-read "the arc width: "))
  '(setq heigth (skeleton-read "the arc heigth: "))
  '(setq start (skeleton-read "the arc start angle in degrees: "))
  '(setq end (skeleton-read "the arc end angle in degrees: "))
  '(setq color (skeleton-read "the ellipse color: "))
  '(setq style (skeleton-read "style: "))
  > "imagefilledarc(" image ", " cx ", " cy ", " width ", " heigth ", " start ", " end ", " color ", " style ");" \n
)

(define-skeleton php-imagefilledellipse
  "Insert an imagefilledarc statement. Draw a partial arc and fill it"
  ""
  '(setq image (skeleton-read "image: "))
  '(setq cx (skeleton-read "x-coordinate of the center: "))
  '(setq cy (skeleton-read "y-coordinate of the center: "))
  '(setq width (skeleton-read "the ellipse width: "))
  '(setq heigth (skeleton-read "the ellipse heigth: "))
  '(setq color (skeleton-read "the ellipse color: "))
  > "imagefilledellipse(" image ", " cx ", " cy ", " width ", " heigth ", " color ");" \n
)

(define-skeleton php-imagefilledpolygon
  "Insert an imagefilledpolygon statement. Creates a polygon filled with color."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq points (skeleton-read "Array of points determined by x and y: " ))
  '(setq num_points (skeleton-read "Total number of vertices: "))
  '(setq color (skeleton-read "Color to fill: "))
  > "imagefilledpolygon(" image ", " points ", " num_points ", " color ");" \n
)

(define-skeleton php-imagefilledrectangle
  "Insert an imagefilledrectangle statement. Creates a rectangle filled with color"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq x1 (skeleton-read "X coordinate for point 1: "))
  '(setq y1 (skeleton-read "Y coordinate for point 1: "))
  '(setq x2 (skeleton-read "X coordinate for point 2: "))
  '(setq y2 (skeleton-read "Y coordinate for point 2: "))
  '(setq color (skeleton-read "Color to fill: "))
  > "imagefilledrectangle(" image ", " x1 ", " y1 ", " x2 ", " y2 ", " color ");" \n
)

(define-skeleton php-imagefilltoborder
  "Insert an imagefilltoborder statement."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq x (skeleton-read "X coordinate of start: "))
  '(setq y (skeleton-read "Y coordinate of start: "))
  '(setq border (skeleton-read "The border color: "))
  '(setq color (skeleton-read "Color to fill: "))
  > "imagefilltoborder(" x ", " y ", " border ", " color ");" \n
)

(define-skeleton php-imagefilter
  "Insert an imagefilter statement. Applies a filter to an image"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq filtertype (skeleton-read "Filter type: "))
  '(setq arg1 (skeleton-read "Arg 1: "))
  '(setq arg2 (skeleton-read "Arg 2: "))
  '(setq arg3 (skeleton-read "Arg 3: "))
  '(setq arg4 (skeleton-read "Arg 4: "))
  > "imagefilter(" image ", " filtertype ", " arg1 ", " arg2 ", " arg3 ", " arg4 ");" \n
)

(define-skeleton php-imageflip
  "Insert an imageflip statement. Flips an image using a given mode"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq mode (skeleton-read "Mode (IMG_FLIP_HORIZONTAL | IMG_FLIP_VERTICAL | IMG_FLIP_BOTH): "))
  > "imageflip(" image ", " mode ");" \n
)

(define-skeleton php-imagefontheight
  "Insert a get font height statement"
  ""
  '(setq font (skeleton-read "Font: "))
  > "imagefontheight(" font ");" \n
)

(define-skeleton php-imagefontwidth
  "Insert a get font width statement"
  ""
  '(setq font (skeleton-read "Font: ")) 
  > "imagefontwidth(" font ");" \n
)

(define-skeleton php-imageftbbox
  "Insert an imageftbbox statement"
  ""
  '(setq size (skeleton-read "Font size: "))
  '(setq angle (skeleton-read "Angle in degrees: "))
  '(setq fontfile (skeleton-read "The name of the TrueType font file (can be an URL): "))
  '(setq text (skeleton-read "The string to be measured: "))
  '(setq extrainfo (skeleton-read "Extra information: "))
  > "imageftbbox(" size ", " angle ", " fontfile ", " text ", " extrainfo ");" \n
)

(define-skeleton php-imagefttext
  "Insert an imagefttext statement"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq size (skeleton-read "Font size: "))
  '(setq angle (skeleton-read "Angle in degrees: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq color (skeleton-read "color: "))
  '(setq fontfile (skeleton-read "The name of the TrueType font file (can be an URL): "))
  '(setq text (skeleton-read "The string to be measured: "))
  '(setq extrainfo (skeleton-read "Extra information: "))
  > "imagefttext(" image ", " size ", " angle ", " x ", " y ", " color ", " fontfile ", " text ", " extrainfo ");" \n
)

(define-skeleton php-imagegammacorrect
  "Insert an imagegammacorrect statement. Applies gamma correction."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq inputgamma (skeleton-read "Input gamma: "))
  '(setq outputgamma (skeleton-read "Output gamma: "))
  > "imagegammacorrect(" image ", " inputgamma ", " outputgamma ");" \n
)

(define-skeleton php-imagegd2
  "Insert an imagegd2 statement. Output gd2 image to browser or file."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq filename (skeleton-read "Filename: "))
  '(setq chunk_size (skeleton-read "Chunk Size: "))
  '(setq type (skeleton-read "Type: "))
  > "imagegd2(" image ", " filename ", " chunk_size ", " type ");" \n
)

(define-skeleton php-imagegd
  "Insert an imagegd statement. Output g2 image to the given filename."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq filename (skeleton-read "Filename: "))
  > "imagegd(" image ", " filename ");" \n
)

(define-skeleton php-imagegif
  "Insert an imagegif statement. Output image to browser or file."
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq filename (skeleton-read "Filename: "))
  > "imagegif(" image ", " filename ");" \n
)

(define-skeleton php-imagegrabscreen
  "Insert an imagegrabscreen statement. Captures the whole screen."
  ""
  > "imagegrabscreen();" \n
)

(define-skeleton php-imagegrabwindow
  "Insert an imagegrabwindow statement. Captures a window."
  ""
  '(setq window (skeleton-read "Window handle: "))
  '(setq client_area (skeleton-read "Client area: "))
  > "imagegrabwindow(" window ", " client_area ");" \n
)

(define-skeleton php-imageinterlace
  "Insert an imageinterlace statement. Enable or disable interlace"
  ""
  '(setq image (skeleton-read "Image: "))
  '(setq interlace (skeleton-read "Interlace: "))
  > "imageinterlace(" image ", " interlace ");" \n
)

(define-skeleton php-imageistruecolor
  "Insert an imageistruecolor statement. Finds whether an image is a truecolor image"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  > "imageistruecolor(" image ");" \n
)

(define-skeleton php-imagejpeg
  "Insert an imagejpeg statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq filename (skeleton-read "Filename: "))
  '(setq quality (skeleton-read "Quality: "))
  > "imagejpeg(" image ", " filename ", " quality ");" \n
)

(define-skeleton php-imagelayereffect 
  "Insert an imagelayereffect statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq effect (skeleton-read "Effect: "))
  > "imagelayereffect(" image ", " effect ");" \n
)

(define-skeleton php-imageline
  "Insert an imageline statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq x1 (skeleton-read "X1: "))
  '(setq y1 (skeleton-read "Y1: "))
  '(setq x2 (skeleton-read "X2: "))
  '(setq y2 (skeleton-read "Y2: "))
  '(setq color (skeleton-read "Color: "))
  > "imageline(" image ", " x1 ", " y1 ", " x2 ", " y2 ", " color ");" \n
)

(define-skeleton php-imageloadfont
  "Load a new font."
  ""
  '(setq file (skeleton-read "File: "))
  > "imageloadfont(" file ");" \n
)

(define-skeleton php-imagepalettecopy
  "Copy the palette from one image to another."
  ""
  '(setq destination (skeleton-read "Destination: "))
  '(setq source (skeleton-read "Source: "))
  > "imagepalettecopy(" destination ", " source ");" \n
)

(define-skeleton php-imagepalettetotruecolor
  "Converts a palette based image to true color"
  ""
  '(setq source (skeleton-read "Source: "))
  > "imagepalettetotruecolor(" source ");" \n
)

(define-skeleton php-imagepng 
  "Output a PNG image to either the browser or a file"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq filename (skeleton-read "Filename: "))
  '(setq quality (skeleton-read "Quality: "))
  '(setq filters (skeleton-read "Filters: "))
  > "imagepng(" image ", " filename ", " quality ", " filters ");" \n
)

(define-skeleton php-imagepolygon
  "Insert an imagepolygon statement. It draws a polygon."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq points (skeleton-read "Array of points: "))
  '(setq num_points (skeleton-read "Number of points: "))
  '(setq color (skeleton-read "Color: "))
  > "imagepolygon(" image ", " points ", " num_points ", " color ");" \n
)

(define-skeleton php-imagepsbbox
  "Insert an imagepsbbox statement. It gives the bounding box of a text rectangle using PostScript Type1 fonts"
  ""
  '(setq text (skeleton-read "Text: "))
  '(setq font (skeleton-read "Font: "))
  '(setq size (skeleton-read "Size: "))
  > "imagepsbbox(" text ", " font ", " size ");" \n
)

(define-skeleton php-imagepsencodefont
  "Insert an imagepsencodefont statement. It changes the character encoding vector of a font"
  ""
  '(setq font_index (skeleton-read "Font index: "))
  '(setq encodingfile (skeleton-read "Encoding file: "))
  > "imagepsencodefont(" font_index ", " encoding_file ");" \n
)

(define-skeleton php-imagepsextendfont
  "Insert an imagepsextendfont statement."
  ""
  '(setq font_index (skeleton-read "Font index: "))
  '(setq extend (skeleton-read "Extension value: "))
  > "imagepsextendfont(" font_index ", " extend ");" \n
)

(define-skeleton php-imagepsfreefont
  "Insert an imagepsfreefont statement."
  ""
  '(setq font_index (skeleton-read "Font index: "))
  > "imagepsfreefont(" font_index ");" \n
)

(define-skeleton php-imagepsloadfont
  "Insert an imagepsloadfont statement."
  ""
  '(setq filename (skeleton-read "Filename: "))
  > "imagepsloadfont(" filename ");" \n
)

(define-skeleton php-imagepsslantfont
  "Insert an imagepsslantfont statement."
  ""
  '(setq font_index (skeleton-read "Font index: "))
  '(setq slant (skeleton-read "Slant: "))
  > "imagepsslantfont(" font_index ", " slant ");" \n
)

(define-skeleton php-imagepstext
  "Insert an imagepstext statement. Draws a text over an image using PostScript Type1 fonts"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq text (skeleton-read "Text: "))
  '(setq font_index (skeleton-read "Font index: "))
  '(setq size (skeleton-read "Size: "))
  '(setq foreground (skeleton-read "Foreground: "))
  '(setq background (skeleton-read "Background: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq space (skeleton-read "Space: "))
  '(setq tightness (skeleton-read "Tightness: "))
  '(setq angle (skeleton-read "Angle: "))
  '(setq antialias_steps (skeleton-read "Antialias steps: "))
  > "imagepstext(" image ", " text ", " font_index ", " size ", " foreground ", " background ", " x ", " y ", " space ", " tightness ", " angle ", " antialias ");" \n
)

(define-skeleton php-imagerectangle
  "Insert an imagerectangle statement"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq x1 (skeleton-read "X1: "))
  '(setq y1 (skeleton-read "Y1: "))
  '(setq x2 (skeleton-read "X2: "))
  '(setq y2 (skeleton-read "Y2: "))
  '(setq color (skeleton-read "Color: "))
  > "imagerectangle(" image ", " x1 ", " y1 ", " x2 ", " y2 ", " color ");" \n
)

(define-skeleton php-imagerotate
  "Insert an imagerotate statement. Rotate an image with a given angle."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq angle (skeleton-read "Angle: "))
  '(setq bgd_color (skeleton-read "Color of the uncovered zone: "))
  '(setq ignore_transparent (skeleton-read "Ignore transparent? "))
  > "imagerotate(" image ", " angle ", " bgd_color ", " ignore_transparent ");" \n
)

(define-skeleton php-imagesavealpha
  "Insert an imagesavealpha statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq saveflag (skeleton-read "Save Flag: "))
  > "imagesavealpha(" image ", " saveflag ");" \n
)

(define-skeleton php-imagescale
  "Insert an imagescale statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq new_width (skeleton-read "New width: "))
  '(setq new_height (skeleton-read "New height: "))
  '(setq mode (skeleton-read "Mode: "))
  > "imagescale(" image ", " new_width ", " new_height ", " mode ");" \n
)

(define-skeleton php-imagesetbrush
  "Insert an imagesetbrush statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq brush (skeleton-read "Resource Brush: "))
  > "imagesetbrush(" image ", " brush ");" \n
)

(define-skeleton php-imagesetinterpolation
  "Insert an imagesetinterpolation statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq interpolation (skeleton-read "Interpolation Method: "))
  > "imagesetinterpolation(" image ", " interpolation ");" \n
)

(define-skeleton php-imagesetpixel
  "Insert an imagesetpixel statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq color (skeleton-read "Color: "))
  > "imagesetpixel(" image ", " x ", " y ", " color ");" \n
)
  
(define-skeleton php-imagesetstyle
  "Insert an imagesetstyle statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq style (skeleton-read "Array style: "))
  > "imagesetstyle(" image ", " style ");" \n
)
  
(define-skeleton php-imagesetthickness
  "Insert an imagesetthickness statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq thickness (skeleton-read "Array thickness: "))
  > "imagesetthickness(" image ", " thickness ");" \n
)
  
(define-skeleton php-imagesettile
  "Insert an imagesettile statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq tile (skeleton-read "Resource Tile: "))
  > "imagesettile(" image ", " tile ");" \n
)
  
(define-skeleton php-imagestring
  "Insert an imagestring statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq font (skeleton-read "Font: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq string (skeleton-read "String: "))
  '(setq color (skeleton-read "Color: "))
  > "imagestring(" image ", " font ", " x ", " y ", " string ", " color ");" \n
)

(define-skeleton php-imagestringup
  "Insert an imagestring statement. Draws a string vertically"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq font (skeleton-read "Font: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq string (skeleton-read "String: "))
  '(setq color (skeleton-read "Color: "))
  > "imagestringup(" image ", " font ", " x ", " y ", " string ", " color ");" \n
)

(define-skeleton php-imagesx
  "Insert an imagesx statement. Get image width."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  > "imagesx(" image ");" \n
)

(define-skeleton php-imagesy
  "Insert an imagesy statement. Get image heigth."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  > "imagesx(" image ");" \n
)

(define-skeleton php-imagetruecolortopalette
  "Insert an imagetruecolortopalette statement. Convert a true color image to a palette image."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq dither (skeleton-read "Dither: "))
  '(setq ncolors (skeleton-read "Maximum number of colors in the palette: "))
  > "imagetruecolortopalette(" image ", " dither ", " ncolors ");" \n
)

(define-skeleton php-imagettfbbox
  "Insert an imagettfbbox statement."
  ""
  '(setq size (skeleton-read "Size: "))
  '(setq angle (skeleton-read "Angle: "))
  '(setq fontfile (skeleton-read "The name of the TrueType font file: "))
  '(setq text (skeleton-read "String to be measured"))
  > "imagettfbbox(" size ", " angle ", " fontfile ", " text ");" \n
)

(define-skeleton php-imagettftext
  "Insert an imagettftext statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq size (skeleton-read "Size: "))
  '(setq angle (skeleton-read "Angle: "))
  '(setq x (skeleton-read "X: "))
  '(setq y (skeleton-read "Y: "))
  '(setq string (skeleton-read "String: "))
  '(setq color (skeleton-read "Color: "))
  '(setq fontfile (skeleton-read "Fontfile: "))  
  '(setq text (skeleton-read "The text in UTF-8: "))  
  > "imagettftext(" image ", " size ", " angle ", " x ", " y ", " string ", " color ", " fontfile ", " text ");" \n
)

(define-skeleton php-imagetypes
  "Insert an imagetypes statement."
  ""
  > "imagetypes();"
)

(define-skeleton php-imagewbmp
  "Insert an imagewbmp statement. Output image to browser or file"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq filename (skeleton-read "Filename: "))
  '(setq foreground (skeleton-read "Foreground: "))
  > "imagewbmp(" image ", " filename ", " foreground ");" \n
)

(define-skeleton php-imagewebp
  "Insert an imagewebp statement."
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq filename (skeleton-read "Filename: "))
  > "imagewebp(" image ", " filename ");" \n
)

(define-skeleton php-imagexbm
  "Insert an imagexbm statement. Output an xbm image to browser or file"
  ""
  '(setq image (skeleton-read "Resource Image: "))
  '(setq filename (skeleton-read "Filename: "))
  '(setq foreground (skeleton-read "Foreground: "))
  > "imagexbm(" image ", " filename ", " foreground ");" \n
)

(define-skeleton php-iptcembed
  "Insert an iptcembed statement. Embeds a binary iptc data into a jpeg image"
  ""
  '(setq iptcdata (skeleton-read "The iptc data: "))
  '(setq jpeg_filename (skeleton-read "Path to jpeg image: "))
  '(setq spool (skeleton-read "Spool flag: "))
  > "iptcembed(" iptcdata ", " jpeg_filename ", " spool ");" \n
)

(define-skeleton php-iptcparse
  "Insert an iptcparse statement. Parse a binary IPTC block into single tags."
  ""
  '(setq iptcblock (skeleton-read "The iptc block: "))
  > "iptcparse(" iptcblock ");" \n
)

(define-skeleton php-jpeg2wbmp
  "Insert a jpeg2wbmp. Convert JPEG image file to WBMP image file"
  ""
  '(setq jpegname (skeleton-read "Path to jpeg: "))
  '(setq wbmpname (skeleton-read "Path to destination wbmp: "))
  '(setq dest_height (skeleton-read "Destination image height: "))
  '(setq dest_width (skeleton-read "Destination image width: "))
  '(setq treshold (skeleton-read "Treshold: "))
  > "jpeg2wbmp(" jpegname ", " wbmpname ", " dest_height ", " dest_width ", " treshold ");" \n
)

(define-skeleton php-png2wbmp
  "Insert a png2wbmp. Convert PNG image file to WBMP image file"
  ""
  '(setq pngname (skeleton-read "Path to png: "))
  '(setq wbmpname (skeleton-read "Path to destination wbmp: "))
  '(setq dest_height (skeleton-read "Destination image height: "))
  '(setq dest_width (skeleton-read "Destination image width: "))
  '(setq treshold (skeleton-read "Treshold: "))
  > "png2wbmp(" pngname ", " wbmpname ", " dest_height ", " dest_width ", " treshold ");" \n
)

