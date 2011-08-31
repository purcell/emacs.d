;;; w3m-symbol.el --- Stuffs to replace symbols for emacs-w3m -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2009
;; ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: w3m, WWW, hypermedia, i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-when-compile
  (defvar w3m-output-coding-system)
  (defvar w3m-language)
  (defvar w3m-use-symbol)
  (autoload 'w3m-mule-unicode-p "w3m"))

(defgroup w3m-symbol nil
  "Symbols for w3m"
  :group 'w3m)

(defvar w3m-symbol-custom-type
  '(list
    :convert-widget w3m-widget-type-convert-widget
    (let* ((w `(sexp :match (lambda (widget value) (stringp value))
		     :size 4 :value ""
		     ,@(if (not (widget-get widget :copy))
			   ;; Emacs versions prior to 22.
			   '(:value-to-internal
			     (lambda (widget value)
			       (if (string-match "\\`\".*\"\\'" value)
				   value
				 (prin1-to-string value)))))))
	   (a `(,@w :format "%v "))
	   (b `(,@w :format "%v\n"))
	   (c (list a a a a a a a b))
	   (d (list a a a a a b)))
      `(:indent 4 :tag "Customize"
		,@c ,@c ,@c ,@c ,@d ,@d ,b ,b))))

(defcustom w3m-default-symbol
  '("-+" " |" "--" " +" "-|" " |" "-+" ""
    "--" " +" "--" ""   "-+" ""   ""   ""
    "-+" " |" "--" " +" "-|" " |" "-+" ""
    "--" " +" "--" ""   "-+" ""   ""   ""
    " *" " +" " o" " #" " @" " -"
    " =" " x" " %" " *" " o" " #"
    " #"
    "<=UpDn ")
  "List of symbol string, used by defaultly."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-BIG5-symbol
  '("$(0#3(B" "$(0#7(B" "$(0#5(B" "$(0#<(B" "$(0#6(B" "$(0#:(B" "$(0#=(B" ""
    "$(0#4(B" "$(0#>(B" "$(0#9(B" ""   "$(0#?(B" ""   ""   ""
    "$(0#3(B" "$(0#7(B" "$(0#5(B" "$(0#<(B" "$(0#6(B" "$(0#:(B" "$(0#=(B" ""
    "$(0#4(B" "$(0#>(B" "$(0#9(B" ""   "$(0#?(B" ""   ""   ""
    "$(0!&(B" "$(0!{(B" "$(0!w(B" "$(0!r(B" "$(0!|(B" "$(0!x(B"
    "$(0!v(B" "$(0!s(B" "$(0!t(B" "$(0!s(B" "$(0!r(B" "$(0!{(B"
    "$(0!s(B"
    "$(0!N"U"V(B")
  "List of symbol string, used in Chienese-BIG5 environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-CNS-symbol
  '("$(G#3(B" "$(G#7(B" "$(G#5(B" "$(G#<(B" "$(G#6(B" "$(G#:(B" "$(G#=(B" ""
    "$(G#4(B" "$(G#>(B" "$(G#9(B" ""   "$(G#?(B" ""   ""   ""
    "$(G#3(B" "$(G#7(B" "$(G#5(B" "$(G#<(B" "$(G#6(B" "$(G#:(B" "$(G#=(B" ""
    "$(G#4(B" "$(G#>(B" "$(G#9(B" ""   "$(G#?(B" ""   ""   ""
    "$(G!&(B" "$(G!{(B" "$(G!w(B" "$(G!r(B" "$(G!|(B" "$(G!x(B"
    "$(G!v(B" "$(G!s(B" "$(G!t(B" "$(G!s(B" "$(G!r(B" "$(G!{(B"
    "$(G!s(B"
    "$(G!N"U"V(B")
  "List of symbol string, used in Chienese-CNS environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-GB-symbol
  '("$A)`(B" "$A)@(B" "$A)P(B" "$A)0(B" "$A)H(B" "$A)&(B" "$A)4(B" ""
    "$A)X(B" "$A)8(B" "$A)$(B" ""   "$A)<(B" ""   ""   ""
    "$A)`(B" "$A)D(B" "$A)S(B" "$A)3(B" "$A)L(B" "$A)'(B" "$A)7(B" ""
    "$A)[(B" "$A);(B" "$A)%(B" ""   "$A)?(B" ""   ""   ""
    "$A!$(B" "$A!u(B" "$A!n(B" "$A!p(B" "$A!v(B" "$A!o(B"
    "$A!r(B" "$A!q(B" "$A!w(B" "$A!q(B" "$A!p(B" "$A!u(B"
    "$A!q(B"
    "$A!6!|!}(B")
  "List of symbol string, used in Chienese-GB environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Japanese-symbol
  '("$B(+(B" "$B('(B" "$B(((B" "$B(#(B" "$B()(B" "$B("(B" "$B($(B" ""
    "$B(*(B" "$B(&(B" "$B(!(B" ""   "$B(%(B" ""   ""   ""
    "$B(+(B" "$B(7(B" "$B(8(B" "$B(.(B" "$B(9(B" "$B(-(B" "$B(/(B" ""
    "$B(:(B" "$B(1(B" "$B(,(B" ""   "$B(0(B" ""   ""   ""
    "$B!&(B" "$B""(B" "$B!y(B" "$B!{(B" "$B"#(B" "$B!z(B"
    "$B!}(B" "$B!|(B" "$B"$(B" "$B!|(B" "$B!{(B" "$B""(B"
    "$B!|(B"
    "$B"c","-(B")
  "List of symbol string, used in Japanese environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Korean-symbol
  '("$(C&+(B" "$(C&'(B" "$(C&((B" "$(C&#(B" "$(C&)(B" "$(C&"(B" "$(C&$(B" ""
    "$(C&*(B" "$(C&&(B" "$(C&!(B" ""   "$(C&%(B" ""   ""   ""
    "$(C&+(B" "$(C&7(B" "$(C&8(B" "$(C&.(B" "$(C&9(B" "$(C&-(B" "$(C&/(B" ""
    "$(C&:(B" "$(C&1(B" "$(C&,(B" ""   "$(C&0(B" ""   ""   ""
    "$(C!$(B" "$(C!`(B" "$(C!Y(B" "$(C![(B" "$(C!a(B" "$(C!Z(B"
    "$(C!](B" "$(C!\(B" "$(C!b(B" "$(C!\(B" "$(C![(B" "$(C!`(B"
    "$(C!\(B"
    "$(C!l!h!i(B")
  "List of symbol string, used in Korean environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-mule-unicode-symbol
  (when (w3m-mule-unicode-p)
    (append
     (mapcar (lambda (p)
	       (if p
		   (char-to-string
		    (make-char (or (nth 2 p) 'mule-unicode-2500-33ff)
			       (car p) (cadr p)))
		 ""))
	     '((32 92) (32 60) (32 76) (32 44) (32 68) (32 34) (32 48) nil
	       (32 84) (32 52) (32 32) nil     (32 56) nil     nil     nil
	       (32 92) (32 64) (32 79) (32 47) (32 72) (32 35) (32 51) nil
	       (32 87) (32 55) (32 33) nil     (32 59) nil     nil     nil
	       (115 34 mule-unicode-0100-24ff) (33 97) (34 102) (34 43) (33 96) (34 101)
	       (34 46) (34 47) (33 115) (34 47) (34 43) (33 97)
	       (34 47)))
     (list (format "%c %c %c "
		   (make-char 'mule-unicode-0100-24ff 121 42)
		   (make-char 'mule-unicode-0100-24ff 118 113)
		   (make-char 'mule-unicode-0100-24ff 118 115)))))
  "List of symbol string, using mule-unicode characters."
  :group 'w3m-symbol
  :type (if (w3m-mule-unicode-p)
	    w3m-symbol-custom-type
	  '(const :format "%{%t%}: %v")))

(defcustom w3m-symbol nil
  "List of symbol string."
  :group 'w3m-symbol
  :type `(radio (const :format "Auto detect  " nil)
		(const :tag "Default" w3m-default-symbol)
		(const :format "Chinese BIG5 " w3m-Chinese-BIG5-symbol)
		(const :format "Chinese CNS " w3m-Chinese-CNS-symbol)
		(const :tag "Chinese GB" w3m-Chinese-GB-symbol)
		(const :format "Japanese     " w3m-Japanese-symbol)
		(const :format "Korean      " w3m-Korean-symbol)
		,@(when w3m-mule-unicode-symbol
		   '((const :tag "Mule-Unicode" w3m-mule-unicode-symbol)))
		(variable :format "%t symbol: %v\n" :size 0
			  :value w3m-default-symbol)
		,w3m-symbol-custom-type))

(defun w3m-use-symbol ()
  (cond ((functionp w3m-use-symbol)
	 (funcall w3m-use-symbol))
	(t w3m-use-symbol)))

(eval-when-compile (defvar current-language-environment))

(defun w3m-symbol ()
  (cond (w3m-symbol
	 (if (symbolp w3m-symbol)
	     (symbol-value w3m-symbol)
	   w3m-symbol))
	((and (eq w3m-output-coding-system 'utf-8)
	      w3m-mule-unicode-symbol))
	((let ((lang (or w3m-language
			 (and (boundp 'current-language-environment)
			      current-language-environment
			      ;; In XEmacs 21.5 it may be the one like
			      ;; "Japanese (UTF-8)".
			      (if (string-match "[\t ]+("
						current-language-environment)
				  (substring current-language-environment
					     0 (match-beginning 0))
				current-language-environment)))))
	   (when (boundp (intern (format "w3m-%s-symbol" lang)))
	     (symbol-value (intern (format "w3m-%s-symbol" lang))))))
	(t w3m-default-symbol)))

;;;###autoload
(defun w3m-replace-symbol ()
  (when (w3m-use-symbol)
    (let ((symbol-list (w3m-symbol)))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<_SYMBOL TYPE=\\([0-9]+\\)>" nil t)
	  (let ((symbol (nth (string-to-number (match-string 1)) symbol-list))
		(start (point))
		end symbol-cnt)
	    (search-forward "</_SYMBOL>" nil t)
	    (setq end (match-beginning 0)
		  symbol-cnt (/ (string-width (buffer-substring start end))
				(string-width symbol)))
	    (goto-char start)
	    (delete-region start end)
	    (insert (apply 'concat (make-list symbol-cnt symbol)))))))))

(provide 'w3m-symbol)

;;; w3m-symbol.el ends here
