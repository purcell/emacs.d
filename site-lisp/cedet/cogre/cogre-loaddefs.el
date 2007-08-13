;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cogre-load-graph cogre) "cogre" "cogre.el" (17953
;;;;;;  30523))
;;; Generated autoloads from cogre.el

(autoload (quote cogre) "cogre" "\
Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create.

\(fn NAME &optional GRAPH-CLASS)" t nil)

(autoload (quote cogre-load-graph) "cogre" "\
Load a graph from FILE into a new graph buffer.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre-mode.el" (17881
;;;;;;  2107))
;;; Generated autoloads from cogre-mode.el

(autoload (quote cogre-mode) "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-uml-create cogre-uml-quick-class) "uml-create"
;;;;;;  "uml-create.el" (17954 15791))
;;; Generated autoloads from uml-create.el

(autoload (quote cogre-uml-quick-class) "uml-create" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown.

\(fn CLASS)" t nil)

(autoload (quote cogre-uml-create) "uml-create" "\
Create a new UML diagram, with CLASS as the root node.
CLASS must be a type in the current project.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "wisent-dot.el"
;;;;;;  (17213 39659))
;;; Generated autoloads from wisent-dot.el

(autoload (quote wisent-dot-setup-parser) "wisent-dot" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook (quote graphviz-dot-mode-hook) (quote wisent-dot-setup-parser))

;;;***

;;;### (autoloads nil nil ("cogre-load.el" "cogre-uml.el" "picture-hack.el"
;;;;;;  "wisent-dot-wy.el") (18110 13446 740638))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cogre-loaddefs.el ends here
