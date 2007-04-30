;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-tuareg.el - Tuareg customization example: Append this file to .emacs.

;; Here is an example of Tuareg variables customization:

(add-hook 'tuareg-mode-hook
	  '(lambda ()
	     (setq tuareg-lazy-= t)
					; indent `=' like a standard keyword
	     (setq tuareg-lazy-paren t)
					; indent [({ like standard keywords
	     (setq tuareg-in-indent 0)
					; no indentation after `in' keywords
	     (auto-fill-mode 1)
					; turn on auto-fill minor mode
	     (if (featurep 'sym-lock)   ; Sym-Lock customization only
		 (setq sym-lock-mouse-face-enabled nil))
					; turn off special face under mouse
	     ))

;; If you use Sym-Lock, you could also add some customization code after the
;; `(require 'sym-lock)' in your `.emacs'

(if (featurep 'sym-lock)
    (setq tuareg-sym-lock-keywords
	  '(("<-" 0 1 172 nil) ("->" 0 1 174 nil)
	    ;; (":=" 0 1 220 nil)
	    ("<=" 0 1 163 nil) (">=" 0 1 179 nil)
	    ("<>" 0 1 185 nil) ("==" 0 1 186 nil)
	    ("||" 0 1 218 nil) ("&&" 0 1 217 nil)
	    ("[^*]\\(\\*\\ nil)\\." 1 8 180 nil)
	    ("\\(/\\ nil)\\." 1 3 184 nil)
	    ;; (":>" 0 1 202 nil)
	    ;; (";;" 0 1 191 nil)
	    ("\\<_\\>" 0 3 188 nil) ("\\<sqrt\\>" 0 3 214 nil)
	    ("\\<unit\\>" 0 3 198 nil) ("\\<fun\\>" 0 3 108 nil)
	    ("\\<or\\>" 0 3 218 nil) ("\\<not\\>" 0 3 216 nil))))
