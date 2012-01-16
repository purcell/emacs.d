;@see /usr/share/emacs/site-lisp/emacspeak/lisp/emacspeak-setup.el
; add "export DTK_PROGRAM=espeak" in my ~/.bashrc
(defun emspk ()
  (interactive)
  (load-file "/usr/share/emacs/site-lisp/emacspeak/lisp/emacspeak-setup.el")
  )
(provide 'init-emacspeak)
