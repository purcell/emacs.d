;;; cider-resolve.el --- Resolve clojure symbols according to current nREPL connection

;; Copyright © 2015-2017 Bozhidar Batsov, Artur Malabarba and CIDER contributors

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The ns cache is a dict of namespaces stored in the connection buffer.  This
;; file offers functions to easily get information about variables from this
;; cache, given the variable's name and the file's namespace.  This
;; functionality is similar to that offered by the `cider-var-info' function
;; (and others).  The difference is that all functions in this file operate
;; without contacting the server (they still rely on an active connection
;; buffer, but no messages are actually exchanged).

;; For this reason, the functions here are well suited for very
;; performance-sentitive operations, such as font-locking or
;; indentation.  Meanwhile, operations like code-jumping are better off
;; communicating with the middleware, just in the off chance that the cache is
;; outdated.

;; Below is a typical entry on this cache dict.  Note that clojure.core symbols
;; are excluded from the refers to save space.

;; "cider.nrepl.middleware.track-state"
;; (dict "aliases"
;;       (dict "cljs" "cider.nrepl.middleware.util.cljs"
;;             "misc" "cider.nrepl.middleware.util.misc"
;;             "set" "clojure.set")
;;       "interns" (dict a
;;                       "assoc-state"    (dict "arglists"
;;                                              (("response"
;;                                                (dict "as" "msg" "keys"
;;                                                      ("session")))))
;;                       "filter-core"    (dict "arglists"
;;                                              (("refers")))
;;                       "make-transport" (dict "arglists"
;;                                              (((dict "as" "msg" "keys"
;;                                                      ("transport")))))
;;                       "ns-as-map"      (dict "arglists"
;;                                              (("ns")))
;;                       "ns-cache"       (dict)
;;                       "relevant-meta"  (dict "arglists"
;;                                              (("var")))
;;                       "update-vals"    (dict "arglists"
;;                                              (("m" "f")))
;;                       "wrap-tracker"   (dict "arglists"
;;                                              (("handler"))))
;;       "refers" (dict "set-descriptor!" "#'clojure.tools.nrepl.middleware/set-descriptor!"))

;;; Code:

(require 'cider-client)
(require 'nrepl-dict)
(require 'cider-util)

(defvar cider-repl-ns-cache)

(defun cider-resolve--get-in (&rest keys)
  "Return (nrepl-dict-get-in cider-repl-ns-cache KEYS)."
  (when cider-connections
    (with-current-buffer (cider-current-connection)
      (nrepl-dict-get-in cider-repl-ns-cache keys))))

(defun cider-resolve-alias (ns alias)
  "Return the namespace that ALIAS refers to in namespace NS.
If it doesn't point anywhere, returns ALIAS."
  (or (cider-resolve--get-in ns "aliases" alias)
      alias))

(defconst cider-resolve--prefix-regexp "\\`\\(?:#'\\)?\\([^/]+\\)/")

(defun cider-resolve-var (ns var)
  "Return a dict of the metadata of a clojure var VAR in namespace NS.
VAR is a string.
Return nil only if VAR cannot be resolved."
  (let* ((var-ns (when (string-match cider-resolve--prefix-regexp var)
                   (cider-resolve-alias ns (match-string 1 var))))
         (name (replace-regexp-in-string cider-resolve--prefix-regexp "" var)))
    (or
     (cider-resolve--get-in (or var-ns ns) "interns" name)
     (unless var-ns
       ;; If the var had no prefix, it might be referred.
       (if-let ((referal (cider-resolve--get-in ns "refers" name)))
           (cider-resolve-var ns referal)
         ;; Or it might be from core.
         (unless (equal ns "clojure.core")
           (cider-resolve-var "clojure.core" name)))))))

(defun cider-resolve-core-ns ()
  "Return a dict of the core namespace for current connection.
This will be clojure.core or cljs.core depending on `cider-repl-type'."
  (when (cider-connected-p)
    (with-current-buffer (cider-current-connection)
      (cider-resolve--get-in (if (equal cider-repl-type "cljs")
                                 "cljs.core"
                               "clojure.core")))))

(defun cider-resolve-ns-symbols (ns)
  "Return a plist of all valid symbols in NS.
Each entry's value is the metadata of the var that the symbol refers to.
NS can be the namespace name, or a dict of the namespace itself."
  (when-let ((dict (if (stringp ns)
                       (cider-resolve--get-in ns)
                     ns)))
    (nrepl-dbind-response dict (interns refers aliases)
      (append (cdr interns)
              (nrepl-dict-flat-map (lambda (alias namespace)
                                     (nrepl-dict-flat-map (lambda (sym meta)
                                                            (list (concat alias "/" sym) meta))
                                                          (cider-resolve--get-in namespace "interns")))
                                   aliases)))))

(provide 'cider-resolve)
;;; cider-resolve.el ends here
