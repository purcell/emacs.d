;;; json-snatcher.el --- Grabs the path to JSON values in a JSON file -*- lexical-binding: t -*-

;; Copyright (C) 2013 Sterling Graham <sterlingrgraham@gmail.com>

;; Author: Sterling Graham <sterlingrgraham@gmail.com>
;; URL: http://github.com/sterlingg/json-snatcher
;; Package-Version: 20150511.2047
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Well this was my first excursion into ELisp programmming.  It didn't go too badly once
;; I fiddled around with a bunch of the functions.
;;
;; The process of getting the path to a JSON value at point starts with
;; a call to the jsons-print-path function.
;;
;; It works by parsing the current buffer into a list of parse tree nodes
;; if the buffer hasn't already been parsed in the current Emacs session.
;; While parsing, the region occupied by the node is recorded into the
;; jsons-parsed-regions hash table as a list.The list contains the location
;; of the first character occupied by the node, the location of the last
;; character occupied, and the path to the node.  The parse tree is also stored
;; in the jsons-parsed list for possible future use.
;;
;; Once the buffer has been parsed, the node at point is looked up in the
;; jsons-curr-region list, which is the list of regions described in the
;; previous paragraph for the current buffer.  If point is not in one of these
;; interval ranges nil is returned, otherwise the path to the value is returned
;; in the form [<key-string>] for objects, and [<loc-int>] for arrays.
;; eg: ['value1'][0]['value2'] gets the array at with name value1, then gets the
;; 0th element of the array (another object), then gets the value at 'value2'.
;;

;;; Installation:
;;
;; IMPORTANT: Works ONLY in Emacs 24 due to the use of the lexical-binding variable.
;;
;; To install add the json-snatcher.el file to your load-path, and
;; add the following lines to your .emacs file:
;;(require 'json-snatcher)
;; (defun js-mode-bindings ()
;;   "Sets a hotkey for using the json-snatcher plugin."
;;   (when (string-match  "\\.json$" (buffer-name))
;;       (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
;; (add-hook 'js-mode-hook 'js-mode-bindings)
;; (add-hook 'js2-mode-hook 'js-mode-bindings)
;;
;; This binds the key to snatch the path to the JSON value to C-c C-g only
;; when either JS mode, or JS2 mode is active on a buffer ending with
;; the .json extension.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defvar jsons-curr-token 0
  "The current character in the buffer being parsed.")
(defvar jsons-parsed (make-hash-table :test 'equal)
  "Hashes each open buffer to the parse tree for that buffer.")
(defvar jsons-parsed-regions (make-hash-table :test 'equal)
  "Hashes each open buffer to the ranges in the buffer for each of the parse trees nodes.")
(defvar jsons-curr-region () "The node ranges in the current buffer.")
(defvar jsons-path-printer 'jsons-print-path-python "Default jsons path printer")
(add-hook 'kill-buffer-hook 'jsons-remove-buffer)

(defun jsons-consume-token ()
  "Return the next token in the stream."
  (goto-char jsons-curr-token)
  (let* ((delim_regex "\\([\][\\{\\}:,]\\)")
         ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
         ;; used to validate escapes if needed later. The second half of the string regex is pretty
         ;; pointless at the moment. I did it this way, so that the code closely mirrors
         ;; the RFC.
         (string_regex "\\(\"\\(\\([^\"\\\\\r\s\t\n]\\)*\\([\r\s\t\n]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^\r\s\t\n]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
         (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
         (literal_regex "\\(true\\|false\\|null\\)")
         (full_regex (concat "\\(" delim_regex "\\|" literal_regex "\\|" string_regex "\\|" num_regex "\\)")))

    (if (re-search-forward full_regex (point-max) "Not nil")
        (progn
          (setq jsons-curr-token (match-end 0))
          (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
      (message "Reached EOF. Possibly invalid JSON."))))

(defun jsons-array (path)
  "Create a new json array object that contain the identifier \"json-array\".
a list of the elements contained in the array, and the PATH to the array."
  (let*(
        (token (jsons-consume-token))
        (array "json-array")
        (elements ())
        (i 0))
    (while (not (string= token "]"))
      (if (not (string= token ","))
          (let ((json-val (jsons-value token path i)))
            (setq i (+ i 1))
            (push json-val elements)
            (setq token (jsons-consume-token)))
        (setq token (jsons-consume-token))))
    (list array (reverse elements) path)))

(defun jsons-literal (token path)
  "Given a TOKEN and PATH, this function return the PATH to the literal."
  (let ((match_start (match-beginning 0))
        (match_end (match-end 0)))
    (progn
      (setq jsons-curr-region (append (list (list match_start match_end path)) jsons-curr-region))
      (list "json-literal" token path (list match_start match_end)))))

(defun jsons-member (token path)
  "This function is called when a member in a JSON object needs to be parsed.
Given the current TOKEN, and the PATH to this member."
  (let* ((member ())
         (value token)
         (range_start (match-beginning 0))
         (range_end (match-end 0))
         )
    (setq member (list "json-member" token))
    (if (not (string= (jsons-consume-token) ":"))
        (error "Encountered token other than : in jsons-member")
      nil)
    (let ((json-val (jsons-value (jsons-consume-token) (cons value path) nil)))
      (setq member (list member (append json-val
                                        (list range_start range_end))))
      (setq jsons-curr-region (append (list (list range_start range_end (elt json-val 2))) jsons-curr-region))
    member)))

(defun jsons-number (token path)
  "This function will return a json-number given by the current TOKEN.
PATH points to the path to this number.  A json-number is defined as per
the num_regex in the `jsons-get-tokens' function."
  (progn
    (setq jsons-curr-region (append (list (list (match-beginning 0) (match-end 0) path)) jsons-curr-region))
    (list "json-number" token path)))

(defun jsons-object (path)
  "This function is called when a { is encountered while parsing.
PATH is the path in the tree to this object."
  (let*(
        (token (jsons-consume-token))
        (members (make-hash-table :test 'equal))
        (object (list "json-object" members path)))
    (while (not (string= token "}"))
      (if (not (string= token ","))
          (let ((json-mem (jsons-member token path)))
            (puthash (elt (elt json-mem 0) 1) (elt json-mem 1) (elt object 1))
            (setq token (jsons-consume-token)))
        (setq token (jsons-consume-token))))
    object))

(defun jsons-string (token path)
  "This function is called when a string is encountered while parsing.
The TOKEN is the current token being examined.
The PATH is the path to this string."
(let ((match_start (match-beginning 0))
      (match_end (match-end 0)))
  (progn
    (setq jsons-curr-region (append (list (list match_start match_end path)) jsons-curr-region))
  (list "json-string" token path (list match_start match_end)))))

(defun jsons-value (token path array-index)
  "A value, which is either an object, array, string, number, or literal.
The is-array variable is nil if inside an array, or the index in
the array that it occupies.
TOKEN is the current token being parsed.
PATH is the path to this value.
ARRAY-INDEX is non-nil if the value is contained within an array, and
points to the index of this value in the containing array."
;;TODO: Refactor the if array-index statement.
  (if array-index
      (if (jsons-is-number token)
          (list "json-value" (jsons-number token (cons array-index path)) (list (match-beginning 0) (match-end 0)))
        (cond
         ((string= token "{") (jsons-object (cons array-index path)))
         ((string= token "[") (jsons-array (cons array-index path)))
         ((string= (substring token 0 1) "\"") (jsons-string token (cons array-index path)))
         (t (jsons-literal token (cons array-index path)))))
    (if (jsons-is-number token)
        (list "json-value" (jsons-number token path) path (list (match-beginning 0) (match-end 0)))
      (cond
       ((string= token "{") (jsons-object path))
       ((string= token "[") (jsons-array path))
       ((string= (substring token 0 1) "\"") (jsons-string token path))
       (t (jsons-literal token path))))))


(defun jsons-get-path ()
  "Function to check whether we can grab the json path from the cursor position in the json file."
  (let ((i 0)
        (node nil))
    (setq jsons-curr-region (gethash (current-buffer) jsons-parsed-regions))
    (when (not (gethash (current-buffer) jsons-parsed))
      (jsons-parse))
    (while (< i (length jsons-curr-region))
      (let*
          ((json_region (elt jsons-curr-region i))
           (min_token (elt json_region 0))
           (max_token (elt json_region 1)))
        (when (and (> (point) min_token) (< (point) max_token))
          (setq node (elt json_region 2))))
      (setq i (+ i 1)))
    node))

(defun jsons-is-number (str)
  "Test to see whether STR is a valid JSON number."
  (progn
    (match-end 0)
    (save-match-data
      (if (string-match "^\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)$" str)
          (progn
            (match-end 0)
            t)
        nil))))

(defun jsons-parse ()
  "Parse the file given in file, return a list of nodes representing the file."
  (save-excursion
    (setq jsons-curr-token 0)
    (setq jsons-curr-region ())
    (if (not (gethash (current-buffer) jsons-parsed))
        (let* ((token (jsons-consume-token))
               (return_val nil))
          (cond
           ((string= token "{") (setq return_val (jsons-object ())))
           ((string= token "[") (setq return_val (jsons-array ())))
           (t nil))
          (puthash (current-buffer) return_val jsons-parsed)
          (puthash (current-buffer) jsons-curr-region jsons-parsed-regions)
          return_val)
      (gethash (current-buffer) jsons-parsed))))

(defun jsons-print-to-buffer (node buffer)
  "Prints the given NODE to the BUFFER specified in buffer argument.
TODO: Remove extra comma printed after lists of object members, and lists of array members."
  (let ((id (elt node 0)))
    (cond
     ((string= id "json-array")
      (progn
        (jsons-put-string buffer "[")
        (mapc (lambda (x) (progn
                            (jsons-print-to-buffer buffer x)
                            (jsons-put-string buffer ",") )) (elt node 1))
        (jsons-put-string buffer "]")))
     ((string= id "json-literal")
      (jsons-put-string buffer (elt node 1)))
     ((string= id "json-member")
      (jsons-put-string buffer (elt node 1))
      (jsons-put-string buffer ": ")
      (jsons-print-to-buffer buffer (elt node 2)))
     ((string= id "json-number")
      (jsons-put-string buffer (elt node 1)))
     ((string= id "json-object")
      (progn
        (jsons-put-string buffer "{")
        (maphash (lambda (key value)
                   (progn
                     (jsons-put-string buffer key)
                     (jsons-put-string buffer ":")
                     (jsons-print-to-buffer buffer value)
                     (jsons-put-string buffer ","))) (elt node 1))
      (jsons-put-string buffer "}")))
     ((string= id "json-string")
      (jsons-put-string buffer (elt node 1)))
     ((string= id "json-value")
      (jsons-print-to-buffer buffer (elt node 1)))
     (t nil))))

(defun jsons-print-path-jq ()
  "Print the jq path to the JSON value under point, and save it in the kill ring."
  (let* ((path (jsons-get-path))
         (i 0)
         (jq_str ".")
         key)
    (setq path (reverse path))
    (while (< i (length path))
      (if (numberp (elt path i))
          (progn
            (setq jq_str (concat jq_str "[" (number-to-string (elt path i)) "]"))
            (setq i (+ i 1)))
        (progn
          (setq key (elt path i))
          (setq jq_str (concat jq_str (substring key 1 (- (length key) 1))))
          (setq i (+ i 1))))
      (when (elt path i)
        (unless (numberp (elt path i))
          (setq jq_str (concat jq_str ".")))))
    (progn (kill-new jq_str)
           (princ jq_str))))

(defun jsons-print-path-python ()
  "Print the python path to the JSON value under point, and save it in the kill ring."
  (let ((path (jsons-get-path))
        (i 0)
        (python_str ""))
    (setq path (reverse path))
    (while (< i (length path))
      (if (numberp (elt path i))
          (progn
            (setq python_str (concat python_str "[" (number-to-string (elt path i)) "]"))
            (setq i (+ i 1)))
        (progn
          (setq python_str (concat python_str "[" (elt path i) "]"))
          (setq i (+ i 1)))))
    (progn (kill-new python_str)
           (princ python_str))))

;;;###autoload
(defun jsons-print-path ()
  "Print the path to the JSON value under point, and save it in the kill ring."
  (interactive)
  (funcall jsons-path-printer))

(defun jsons-put-string (buffer str)
  "Append STR to the BUFFER specified in the argument."
    (save-current-buffer
      (set-buffer (get-buffer-create buffer))
      (insert (prin1-to-string str t))))

(defun jsons-remove-buffer ()
  "Used to clean up the token regions, and parse tree used by the parser."
  (progn
    (remhash (current-buffer) jsons-parsed)
    (remhash (current-buffer) jsons-parsed-regions)))

(provide 'json-snatcher)

;; Local-Variables:
;; indent-tabs-mode: nil
;; End:

;;; json-snatcher.el ends here
