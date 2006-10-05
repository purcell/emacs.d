;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cogre-load-graph cogre) "cogre" "cogre.el" (17091
;;;;;;  25107))
;;; Generated autoloads from cogre.el

(autoload (quote cogre) "cogre" "\
Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create." t nil)

(autoload (quote cogre-load-graph) "cogre" "\
Load a graph from FILE into a new graph buffer." t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre-mode.el" (17091
;;;;;;  25107))
;;; Generated autoloads from cogre-mode.el

(autoload (quote cogre-mode) "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}" t nil)

;;;***

;;;### (autoloads (cogre-uml-create cogre-uml-quick-class) "uml-create"
;;;;;;  "uml-create.el" (17091 25107))
;;; Generated autoloads from uml-create.el

(autoload (quote cogre-uml-quick-class) "uml-create" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown." t nil)

(autoload (quote cogre-uml-create) "uml-create" "\
Create a new UML diagram, with CLASS as the root node.
CLASS must be a type in the current project." t nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "wisent-dot.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent-dot.el

(autoload (quote wisent-dot-setup-parser) "wisent-dot" "\
Setup buffer for parse." nil nil)

(add-hook (quote graphviz-dot-mode-hook) (quote wisent-dot-setup-parser))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cogre-loaddefs.el ends here
