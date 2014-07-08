;;; Package --- Configure GNU octave mode
;;; Commentary:
;;; Code:

;;; To begin using Octave mode for all .m files you visit
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;; Turn on the abbrevs, auto-fill and font-lock features automatically
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

(provide 'init-octave)
;;; init-octave.el ends here
