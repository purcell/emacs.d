(require 'todochiku) ;; growl notifications when compilation finishes
(setq todochiku-icons-directory (expand-file-name "~/.emacs.d/site-lisp/todochiku-icons"))


;;----------------------------------------------------------------------------
;; Use terminal-notifier in OS X Mountain Lion
;; https://github.com/alloy/terminal-notifier (Install in /Applications)
;;----------------------------------------------------------------------------
(setq terminal-notifier-path
      "/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier")

(defadvice todochiku-get-arguments (around todochiku-nc)
  (setq ad-return-value
        (list "-title" title "-message" message "-activate" "org.gnu.Emacs")))

(when (file-executable-p terminal-notifier-path)
  (setq todochiku-command terminal-notifier-path)
  (ad-activate 'todochiku-get-arguments))


(provide 'init-growl)
