;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cogre-load-graph cogre) "cogre" "cogre.el" (18857
;;;;;;  63845))
;;; Generated autoloads from cogre.el

(eieio-defclass-autoload 'cogre-graph '(eieio-persistent) "cogre" "A Connected Graph.\na connected graph contains a series of nodes and links which are\nrendered in a buffer, or serialized to disk.")

(eieio-defclass-autoload 'cogre-graph-element '(eieio-named) "cogre" "A Graph Element.\nGraph elements are anything that is drawn into a `cogre-graph'.\nGraph elements have a method for marking themselves dirty.")

(eieio-defclass-autoload 'cogre-node '(cogre-graph-element) "cogre" "Connected Graph node.\nNodes are regions with a fill color, and some amount of text representing\na status, or values.")

(eieio-defclass-autoload 'cogre-link '(cogre-graph-element) "cogre" "Connected Graph link.\nLinks are lines drawn between two nodes, or possibly loose in space\nas an intermediate step.  Some links have text describing what they\ndo, and most links have special markers on one end or another, such as\narrows or circles.")

(autoload 'cogre "cogre" "\
Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create.

\(fn NAME &optional GRAPH-CLASS)" t nil)

(autoload 'cogre-load-graph "cogre" "\
Load a graph from FILE into a new graph buffer.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre-mode.el" (18857
;;;;;;  63845))
;;; Generated autoloads from cogre-mode.el

(autoload 'cogre-mode "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cogre-uml" "cogre-uml.el" (18857 63845))
;;; Generated autoloads from cogre-uml.el

(eieio-defclass-autoload 'cogre-package '(cogre-node) "cogre-uml" "A Package node.\nPackages represent other class diagrams, and list the major nodes\nwithin them.  They can be linked by dependency links.")

(eieio-defclass-autoload 'cogre-class '(cogre-node) "cogre-uml" "A Class node.\nClass nodes represent a class, and can list the attributes and methods\nwithin them.  Classes can have attribute links, and class hierarchy links.")

(eieio-defclass-autoload 'cogre-inherit '(cogre-link) "cogre-uml" "This type of link indicates that the two nodes reference infer inheritance.\nThe `start' node is the child, and the `end' node is the parent.\nThis is supposed to infer that START inherits from END.")

(eieio-defclass-autoload 'cogre-aggrigate '(cogre-link) "cogre-uml" "This type of link indicates aggregation.\nThe `start' node is the owner of the aggregation, the `end' node is\nthe item being aggregated.\nThis is supposed to infer that START contains END.")

;;;***

;;;### (autoloads (cogre-uml-utest cogre-utest) "cogre-utest" "cogre-utest.el"
;;;;;;  (18857 63845))
;;; Generated autoloads from cogre-utest.el

(autoload 'cogre-utest "cogre-utest" "\
Unit test Various aspects of COGRE.

\(fn)" t nil)

(autoload 'cogre-uml-utest "cogre-utest" "\
Quick test for UML chart generation.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-uml-create cogre-uml-quick-class) "uml-create"
;;;;;;  "uml-create.el" (18857 63845))
;;; Generated autoloads from uml-create.el

(autoload 'cogre-uml-quick-class "uml-create" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown.

\(fn CLASS)" t nil)

(autoload 'cogre-uml-create "uml-create" "\
Create a new UML diagram, with CLASS as the root node.
CLASS must be a type in the current project.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "wisent-dot.el"
;;;;;;  (18857 63845))
;;; Generated autoloads from wisent-dot.el

(autoload 'wisent-dot-setup-parser "wisent-dot" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

;;;***

;;;### (autoloads nil nil ("cogre-load.el" "picture-hack.el" "wisent-dot-wy.el")
;;;;;;  (19926 52532 644918))

;;;***

(provide 'cogre-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cogre-loaddefs.el ends here
