;;; gnuplot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gnuplot" "gnuplot.el" (23009 25397 0 0))
;;; Generated autoloads from gnuplot.el

(autoload 'gnuplot-mode "gnuplot" "\
Major mode for editing and executing GNUPLOT scripts.
This was written with version 4.6 of gnuplot in mind, but should
work with newer and older versions.

Report bugs in `gnuplot-mode' using \\[gnuplot-bug-report].

			    ------O------

Gnuplot-mode includes two different systems for keyword
completion and documentation lookup: a newer one,
`gnuplot-context-sensitive-mode' (enabled by default), and a
older one which extracts keywords from gnuplot's Info file.  Both
systems allow looking up documentation in the Info file.  The
older system also depends having the info file properly installed
to make a list of keywords.

The info file should be installed by default with the Gnuplot
distribution, or is available at the `gnuplot-mode' web page:
http://github.com/bruceravel/gnuplot-mode/

With the new context-sensitive mode active, gnuplot-mode can also
provide `eldoc-mode' syntax hints as you type.  This requires a
separate file of strings, `gnuplot-eldoc.el', which is also
provided by recent Gnuplot distributions.

			    ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
\(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 3.  The GUI does not know how to read from continuation lines.
 4.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 5.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

			    ------O------

 Key bindings:
 \\{gnuplot-mode-map}

\(fn)" t nil)

(autoload 'gnuplot-make-buffer "gnuplot" "\
Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (autoload 'gnuplot-make-buffer \"gnuplot\"
               \"open a buffer in gnuplot mode\" t)
     (global-set-key [(f10)] 'gnuplot-make-buffer)

\(fn)" t nil)

(autoload 'run-gnuplot "gnuplot" "\
Run an inferior Gnuplot process.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("gnuplot-context.el" "gnuplot-gui.el"
;;;;;;  "gnuplot-pkg.el") (23009 25397 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gnuplot-autoloads.el ends here
